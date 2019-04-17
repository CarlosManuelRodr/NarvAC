(* ::Package:: *)

(* ::Title:: *)
(*CPU*)


(* ::Chapter:: *)
(*Begin package*)


BeginPackage["CPU`"]


(* ::Chapter:: *)
(*Package description*)


(* ::Chapter:: *)
(*Begin package*)


Begin["`Private`"]


(* ::Subchapter:: *)
(*Global*)


Unprotect[BitAnd];
BitAnd[1,x_]:=x;
BitAnd[x_,1]:=x;
Protect[BitAnd];

Unprotect[BitNot];
BitNot[0] := 1;
BitNot[1] := 0;
Protect[BitAnd];


(* ::Subchapter:: *)
(*Multiplexers*)


Mux2to1[{select_},{input1_,input2_}]:=BitOr[BitAnd[input1,BitNot[select]],BitAnd[input2,select]];

Mux4to1[{select1_,select2_}, {input1_,input2_,input3_,input4_}] := Mux2to1[
    {select1},
    {
        Mux2to1[{select2},{input1,input2}],
        Mux2to1[{select2},{input3,input4}]
    }
];

Mux8to1[{select1_,select2_,select3_},{input1_,input2_,input3_,input4_,input5_,input6_,input7_,input8_}]:=
Mux2to1[
    {select1},
    {
        Mux4to1[{select2,select3}, {input1,input2,input3,input4}],
        Mux4to1[{select2,select3}, {input5,input6,input7,input8}]
    }
];

MultiplexerNTo1[2, {select_}, input__] := BitOr[BitAnd[First[input], BitNot[select]], BitAnd[Last[input],select]];
MultiplexerNTo1[n_, select_, input__] := Block[{partInput},
    partInput = HalfSplit[input];
    MultiplexerNTo1[
        2,
        {First[select]},
        {
            MultiplexerNTo1[n/2,Rest[select],First[partInput]],
            MultiplexerNTo1[n/2,Rest[select],Last[partInput]]
        }
    ]
];


(* ::Text:: *)
(*Auxiliary*)


MuxInputByOpcode[n_, opcodeAndOutput_] := Block[{addresses, missing},
    addresses = Tuples[{0,1},n]; (* Generate the list of all opcode inputs.  *)
    missing = Thread[{Complement[addresses, opcodeAndOutput[[All,1]]], 0}]; (* Select the ones with no inputs and map them to zero. *)
    Return[SortBy[Join[missing,opcodeAndOutput], First][[All, 2]]] (* Join missing opcodes with the given opcodes and sort. *)
];


(* ::Subchapter:: *)
(*ALU*)


(* ::Text:: *)
(*Mnemonics and opcodes*)


aluMnemonics = {"Add","Substract","Increment","Decrement","Pass"};
aluOpcodes = Thread[aluMnemonics->Take[Tuples[{0,1},3], Length[aluMnemonics]]];
revAluOpcodes = Map[Reverse,aluOpcodes];
ALUOpCode[mnemonic_] := ReplaceAll[mnemonic, aluOpcodes];


(* ::Text:: *)
(*Operations*)


HalfAdder[a_,b_] := {BitAnd[a,b], BitXor[a,b]};
FullAdder[a_,b_,c_] := Block[{halfAdd1,halfAdd2},
    halfAdd1 = HalfAdder[a,b];
    halfAdd2 = HalfAdder[Last[halfAdd1],c];
    {BitOr[First[halfAdd1], First[halfAdd2]], Last[halfAdd2]}
];

ALUAdder[A_,B_,carry_:0] := Block[{littleEndianA, littleEndianB, pairs, sum, bigEndianSum},
    littleEndianA = Reverse[A];
    littleEndianB = Reverse[B];

    (* Se crea conjunto de pares a sumar *)
    pairs = Transpose[{littleEndianA,littleEndianB}];

    (* Se a\[NTilde]aden dos pares extra para la \[UAcute]ltima adici\[OAcute]n *)
    pairs = Join[pairs,{{0,0}, {0,0}}];
    sum = Drop[FoldPairList[{Last[#1],Apply[FullAdder, Join[{First[#1]},#2]]}&, {carry,0}, pairs], 1];
    bigEndianSum = Reverse[sum];
    Return[{First[bigEndianSum], Rest[bigEndianSum]}];
];

ALUSubstractor[A_, B_, borrow_:1] := Block[{littleEndianA, littleEndianB, pairs, sum, bigEndianSum},
    littleEndianA = Reverse[A];
    littleEndianB = Reverse[BitNot[B]];

    (* Se crea conjunto de pares a sumar *)
    pairs = Transpose[{littleEndianA,littleEndianB}];

    (* Se a\[NTilde]aden dos pares extra para la \[UAcute]ltima adici\[OAcute]n *)
    pairs = Join[pairs, {{0,0},{0,0}}];
    sum = Drop[FoldPairList[{Last[#1], Apply[FullAdder,Join[{First[#1]},#2]]}&, {borrow,0}, pairs],1];
    bigEndianSum = Reverse[sum];
    Return[{BitNot[First[bigEndianSum]], Rest[bigEndianSum]}];
];


(* ::Text:: *)
(*Execution*)


ALUZero[a_] := BitNot[Apply[BitOr,a]];
ALUNotZero[a_] := Apply[BitOr,a];
ALUParity[a_] := Apply[BitXor,a];


ALUExecute[opcode_,a_,b_] := Block[
    {
        add,substract,
        increment,decrement,pass,output,positiveFlag,
        negativeFlag,parityFlag
    },

    add = ALUAdder[a,b];
    substract = ALUSubstractor[a,b];
    increment = ALUAdder[PadLeft[{1},Length[a]],a];
    decrement = ALUSubstractor[a,PadLeft[{1},Length[a]]];
    pass = a;

    output = Mux8to1[opcode,
        MuxInputByOpcode[3,
            {
                {ALUOpCode["Add"], Last[add]},
                {ALUOpCode["Substract"], Last[substract]},
                {ALUOpCode["Increment"], Last[increment]},
                {ALUOpCode["Decrement"], Last[decrement]},
                {ALUOpCode["Pass"], pass}
            }
        ]
    ];

    positiveFlag = Mux8to1[opcode,
        MuxInputByOpcode[3,
            {
                {ALUOpCode["Add"], 1},
                {ALUOpCode["Substract"], BitNot[First[substract]]},
                {ALUOpCode["Increment"], 1},
                {ALUOpCode["Decrement"], BitNot[First[decrement]]},
                {ALUOpCode["Pass"], 0}
            }
        ]
    ];

    negativeFlag = Mux8to1[opcode,
        MuxInputByOpcode[3,
            {
                {ALUOpCode["Add"], 0},
                {ALUOpCode["Substract"], First[substract]},
                {ALUOpCode["Increment"], 0},
                {ALUOpCode["Decrement"], First[decrement]},
                {ALUOpCode["Pass"], 0}
            }
        ]
    ];

    Return[{<|"Positive"->positiveFlag, "Negative"->negativeFlag, "Zero"->ALUZero[output], "NotZero"->ALUNotZero[output], "Parity"->ALUParity[output]|>, output}];
];

ALUClear[] := Block[{},
    WriteFlag[0,"ALUFlagPositive"];
    WriteFlag[0,"ALUFlagNegative"];
    WriteFlag[0,"ALUFlagZero"];
    WriteFlag[0,"ALUFlagNotZero"];
    WriteFlag[0,"ALUFlagParity"];
];


(* ::Subchapter:: *)
(*Memory*)


SetAttributes[MemoryEvaluate,HoldAll];
MemoryEvaluate[expr_,tag_]:=Block[{},
    If[Head[SpecialSymbol[tag]] == SpecialSymbol,
        SpecialSymbol[tag] = 0;
    ];
    SpecialSymbol[tag] = ReleaseHold[expr[SpecialSymbol[tag]]];
    Return[SpecialSymbol[tag]];
];

AndOrLatch[set_, reset_, tag_] := MemoryEvaluate[BitAnd[BitOr[#,set], BitNot[reset]]&, tag];

GatedLatch[dataInput_, writeEnable_, tag_] := AndOrLatch[BitAnd[dataInput,writeEnable], BitAnd[BitNot[dataInput],writeEnable], tag];

ReadFlag[tag_] := GatedLatch[0,0,tag];
WriteFlag[dataInput_, tag_] := GatedLatch[dataInput,1,tag];


(* ::Text:: *)
(*1 bit*)


AndOrLatch[set_, reset_, tag_] := MemoryEvaluate[BitAnd[BitOr[#,set], BitNot[reset]]&, tag];

GatedLatch[dataInput_, writeEnable_, tag_] := AndOrLatch[BitAnd[dataInput,writeEnable], BitAnd[BitNot[dataInput],writeEnable], tag];

ReadFlag[tag_] := GatedLatch[0,0,tag];
WriteFlag[dataInput_,tag_] := GatedLatch[dataInput,1,tag];


(* ::Text:: *)
(*8 bit*)


Bit8Register[dataInput__, writeEnable_, tag_] := Table[
	GatedLatch[
		Part[dataInput,index],
		writeEnable,
		StringJoin[tag, "_bit8register", ToString[index]]
	],
	{index, 1, 8}
];
ReadBit8Register[tag_] := Bit8Register[{0,0,0,0,0,0,0,0}, 0, tag];
WriteBit8Register[dataInput__, tag_] := Bit8Register[dataInput, 1, tag];


(* ::Subchapter:: *)
(*Control unit*)


(* ::Text:: *)
(*Opcodes*)


cpuMnemonics = {
    "LoadA","LoadB","SetA","SetB","Store","Add",
    "Substract","Increment","Decrement","Jump","JumpPos","JumpNeg","JumpZero","JumpNotZero","JumpOdd",
    "Call","Return","Print","Halt"
};
opcodes = Thread[cpuMnemonics->Take[Tuples[{0,1}, 8], Length[cpuMnemonics]]];
revOpcodes = Map[Reverse, opcodes];
OpCode[mnemonic_] := ReplaceAll[mnemonic, opcodes];


(* ::Text:: *)
(*Set up registers*)


CUClear[]:=Block[{},
    WriteBit4Register[{0,0,0,0},"InstructionAddress"];
    WriteBit8Register[{0,0,0,0,0,0,0,0},"InstructionRegister"];
    WriteBit8Register[{0,0,0,0,0,0,0,0},"RegisterA"];
    WriteBit8Register[{0,0,0,0,0,0,0,0},"RegisterB"];
    WriteFlag[0,"Ram Read-Enable"];
    WriteFlag[0,"Ram Write-Enable"];
    WriteFlag[0,"RegisterA Read-Enable"];
    WriteFlag[0,"RegisterB Read-Enable"];
    WriteFlag[0,"RegisterA Write-Enable"];
    WriteFlag[0,"RegisterB Write-Enable"];
    WriteFlag[0,"ALU Opcode"];
    WriteFlag[0,"Halt"];
];


(* ::Text:: *)
(*Fetch*)


CUFetch[]:=Block[{instructionAddress,instruction},
    instructionAddress = ReadBit4Register["InstructionAddress"];
    instruction = ReadBit4AddressBlock[instructionAddress,"MainMemory"];
    WriteBit8Register[instruction,"InstructionRegister"];
];


(* ::Text:: *)
(*Decode*)


CUDecode[]:=Block[
    {
        opcode,ramReadEnable,ramWriteEnable,registerAReadEnable,
        registerBReadEnable,registerAWriteEnable,registerBWriteEnable,
        aluOpcode,aluInstruction,halt, jumpEnable, jumpNegEnable,
        jumpZeroEnable,printEnable,
        return = {}
    },

    opcode = First[HalfSplit[ReadBit8Register["InstructionRegister"]]];
    ramReadEnable = MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeLoadA[],1},
                {OpcodeLoadB[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[ramReadEnable,"Ram Read-Enable"]];

    ramWriteEnable = MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeStoreA[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[ramWriteEnable,"Ram Write-Enable"]];


    registerAReadEnable = MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeStoreA[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[registerAReadEnable,"RegisterA Read-Enable"]];


    registerBReadEnable = MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeStoreB[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[registerBReadEnable,"RegisterB Read-Enable"]];

    registerAWriteEnable = MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeLoadA[],1},
                {OpcodeAdd[],1},
                {OpcodeSubstract[],1},
                {OpcodeIncrement[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[registerAWriteEnable,"RegisterA Write-Enable"]];

    registerBWriteEnable = MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeLoadB[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[registerBWriteEnable,"RegisterB Write-Enable"]];

    jumpEnable = MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeJump[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[jumpEnable,"Jump"]];

    jumpNegEnable = MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeJumpNeg[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[jumpNegEnable,"JumpNeg"]];

    jumpZeroEnable = MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeJumpZero[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[jumpZeroEnable,"JumpZero"]];

    aluInstruction =  MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeAdd[],1},
                {OpcodeSubstract[],1},
                {OpcodeIncrement[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[aluInstruction,"ALU Instruction"]];

    aluOpcode =  MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeAdd[],ALUOpcodeAdd[]},
                {OpcodeSubstract[],ALUOpcodeSubstract[]},
                {OpcodeIncrement[],ALUOpcodeIncrement[]}
            }
        ]
    ];
    AppendTo[return,WriteBit3Register[aluOpcode,"ALU Opcode"]];

    printEnable =  MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodePrint[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[printEnable,"Print Enable"]];

    halt =  MultiplexerNTo1[16,opcode,
        MuxInputByOpcode[4,
            {
                {OpcodeHalt[],1}
            }
        ]
    ];
    AppendTo[return,WriteFlag[halt,"Halt"]];
    Return[return];
];


(* ::Text:: *)
(*Execute*)


CUExecute[]:=Block[{param,readFromRam,aluResult,nextInstructionAddress, return = {}},
param = Last[HalfSplit[ReadBit8Register["InstructionRegister"]]];
readFromRam = ReadBit4AddressBlock[param,"MainMemory"];

(* Read operations *)
AppendTo[return,Bit8Register[readFromRam,BitAnd[ReadFlag["RegisterA Write-Enable"],BitNot[ReadFlag["ALU Instruction"]]],"RegisterA"]];
AppendTo[return,Bit8Register[readFromRam,BitAnd[ReadFlag["RegisterB Write-Enable"],BitNot[ReadFlag["ALU Instruction"]]],"RegisterB"]];

(* ALU operations *)
aluResult = ALUExecute[ReadBit3Register["ALU Opcode"],ReadBit8Register["RegisterA"],ReadBit8Register["RegisterB"]];
AppendTo[return,Bit8Register[Last[aluResult],BitAnd[ReadFlag["RegisterA Write-Enable"],ReadFlag["ALU Instruction"]],"RegisterA"]];
AppendTo[return,GatedLatch[First[aluResult]["Overflow"],ReadFlag["ALU Instruction"],"ALUFlagOverflow"]];
AppendTo[return,GatedLatch[First[aluResult]["Negative"],ReadFlag["ALU Instruction"],"ALUFlagNegative"]];
AppendTo[return,GatedLatch[First[aluResult]["Zero"],ReadFlag["ALU Instruction"],"ALUFlagZero"]];
AppendTo[return,GatedLatch[First[aluResult]["Parity"],ReadFlag["ALU Instruction"],"ALUFlagParity"]];

(* Write operations *)
AppendTo[return,Bit4AddressBlock[PadLeft[ReadBit8Register["RegisterA"],8],ReadFlag["Ram Write-Enable"],param,"MainMemory"]];

(* Instruction address operations *)
AppendTo[return,Bit4Register[param,ReadFlag["Jump"],"InstructionAddress"]];
AppendTo[return,Bit4Register[param,BitAnd[ReadFlag["JumpNeg"],ReadFlag["ALUFlagNegative"]],"InstructionAddress"]];

nextInstructionAddress = Last[ALUAdder[ReadBit4Register["InstructionAddress"],{0,0,0,1}]];
AppendTo[return,Bit4Register[nextInstructionAddress,BitAnd[BitNot[ReadFlag["Jump"]],BitNot[ReadFlag["JumpNeg"]]],"InstructionAddress"]];
AppendTo[return,Bit4Register[nextInstructionAddress,BitAnd[ReadFlag["JumpNeg"],BitNot[ReadFlag["ALUFlagNegative"]]],"InstructionAddress"]];

If[ReadFlag["JumpNeg"],Print[ReadBit8Register["RegisterA"]]];

Return[return];
];


(* ::Subchapter:: *)
(*Execution*)


CPUCycle[]:=Block[{},
CUFetch[];
CUDecode[];

If[ReadFlag["Halt"] != 1,CUExecute[]];
];
Execute[program_]:=Block[{steps},
ClearProgram[];
ALUClear[];
CUClear[];
LoadProgram[program];

steps = Reap[
CUFetch[];
CUDecode[];
Sow[CPUPlot[]];

While[ReadFlag["Halt"] != 1,
CUExecute[];
CUFetch[];
CUDecode[];
Sow[CPUPlot[]];
]
][[2,1]];
Return[steps];
];

Execute[program_,cycles_]:=Block[{steps,iter = 0},
ClearProgram[];
ALUClear[];
CUClear[];
LoadProgram[program];

steps = Reap[
CUFetch[];
CUDecode[];
Sow[CPUPlot[]];

While[ReadFlag["Halt"] != 1 && iter < cycles,
CUExecute[];
CUFetch[];
CUDecode[];
Sow[CPUPlot[]];
iter++;
]
][[2,1]];
Return[steps];
];


LoadProgram[program_]:=Block[{addresses},
addresses = Map[PadLeft[IntegerDigits[#,2],4]&,Range[0,15]];
MapThread[WriteBit4AddressBlock[#2,#1,"MainMemory"]&,{addresses,program}];
];
ClearProgram[]:=Block[{addresses},
addresses = Map[PadLeft[IntegerDigits[#,2],4]&,Range[0,15]];
Map[WriteBit4AddressBlock[{0,0,0,0,0,0,0,0},#1,"MainMemory"]&,addresses];
];


(* ::Chapter:: *)
(*End of package*)


End[ ]

EndPackage[ ]
