(* ::Package:: *)

(* ::Title:: *)
(*CPU Emulator*)


(* ::Chapter:: *)
(*Begin package*)


BeginPackage["CPUEmulator`"]


(* ::Chapter:: *)
(*Package description*)


OpCode::usage = "\!\(\*
StyleBox[\"OpCode\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"mnemonic\",\nFontSlant->\"Italic\"]\)] returns the opcode associated with \!\(\*
StyleBox[\"mnemonic\",\nFontSlant->\"Italic\"]\).";
UpdateParity::usage = "\!\(\*
StyleBox[\"UpdateParity\",\nFontWeight->\"Bold\"]\)[] update the parity flag. ";
UpdateALU::usage = "\!\(\*
StyleBox[\"UpdateALU\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"numericResult\",\nFontSlant->\"Italic\"]\)] updates the flags \!\(\*
StyleBox[\"ALUPositiveFlag\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"ALUNegativeFlag\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"ALUZeroFlag\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"and\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"ALUNotZeroFlag\",\nFontSlant->\"Italic\"]\) according to the base 10 number \!\(\*
StyleBox[\"numericResult\",\nFontSlant->\"Italic\"]\).";
LoadA::usage = "\!\(\*
StyleBox[\"LoadA\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\)] loads the content of RAM at \!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\) to the A register.";
LoadB::usage = "\!\(\*
StyleBox[\"LoadB\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\)] loads the content of RAM at \!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\) to the B register.";
SetA::usage = "\!\(\*
StyleBox[\"SetA\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"value\",\nFontSlant->\"Italic\"]\)] set register A with \!\(\*
StyleBox[\"value\",\nFontSlant->\"Italic\"]\).";
SetB::usage = "\!\(\*
StyleBox[\"SetB\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"value\",\nFontSlant->\"Italic\"]\)] set register B with \!\(\*
StyleBox[\"value\",\nFontSlant->\"Italic\"]\).";
Store::usage = "\!\(\*
StyleBox[\"Store\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\)] store the value of register A to the memory location \!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\).";
Add::usage = "\!\(\*
StyleBox[\"Add\",\nFontWeight->\"Bold\"]\)[] add the value of register A to the value of register B and store it in register A.";
Substract::usage = "\!\(\*
StyleBox[\"Substract\",\nFontWeight->\"Bold\"]\)[]  substract the value of register A to the value of register B and store it in register A.";
UIncrement::usage = "\!\(\*
StyleBox[\"UIncrement\",\nFontWeight->\"Bold\"]\)[] increment in one unit the value of register A.";
UDecrement::usage = "\!\(\*
StyleBox[\"UDecrement\",\nFontWeight->\"Bold\"]\)[] decrement in one unit the value of register A.";
Jump::usage = "\!\(\*
StyleBox[\"Jump\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\)] set \!\(\*
StyleBox[\"addressRegister\",\nFontSlant->\"Italic\"]\) to the value of \!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\).";
JumpConditional::usage = "\!\(\*
StyleBox[\"JumpConditional\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"condition\",\nFontSlant->\"Italic\"]\)] set \!\(\*
StyleBox[\"addressRegister\",\nFontSlant->\"Italic\"]\) to the value of \!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\) if the register at \!\(\*
StyleBox[\"condition\",\nFontSlant->\"Italic\"]\) has value of 1.";
Call::usage = "\!\(\*
StyleBox[\"Call\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\)] store current \!\(\*
StyleBox[\"addressRegister\",\nFontSlant->\"Italic\"]\) value on the stack and  jump to \!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\).";
ReturnC::usage = "\!\(\*
StyleBox[\"ReturnC\",\nFontWeight->\"Bold\"]\)[] set \!\(\*
StyleBox[\"addressRegister\",\nFontSlant->\"Italic\"]\) to the last value of the stack. If the stack is empty, halt the machine.";
PrintA::usage = "\!\(\*
StyleBox[\"PrintA\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\)] print the value of RAM at \!\(\*
StyleBox[\"address\",\nFontSlant->\"Italic\"]\) in the output buffer.";
ResetCPU::usage = "\!\(\*
StyleBox[\"ResetCPU\",\nFontWeight->\"Bold\"]\)[] reset registers, flags and RAM to zero.";
AddressAdvance::usage = "\!\(\*
StyleBox[\"AddressAdvance\",\nFontWeight->\"Bold\"]\)[] increase \!\(\*
StyleBox[\"addressRegister\",\nFontSlant->\"Italic\"]\) by one unit.";
CPU::usage = "\!\(\*
StyleBox[\"CPU\",\nFontWeight->\"Bold\"]\)[] perform one CPU cycle.";

RAMPlot::usage = "\!\(\*
StyleBox[\"RAMPlot\",\nFontWeight->\"Bold\"]\)[] plot the content of RAM.";
ALUStatusPlot::usage = "\!\(\*
StyleBox[\"ALUStatusPlot\",\nFontWeight->\"Bold\"]\)[] plot the ALU registers.";
CUStatusPlot::usage = "\!\(\*
StyleBox[\"CUStatusPlot\",\nFontWeight->\"Bold\"]\)[] plot the CU registers.";
StackPlot::usage = "\!\(\*
StyleBox[\"StackPlot\",\nFontWeight->\"Bold\"]\)[] plot the content of the stack.";
CPUPlot::usage = "\!\(\*
StyleBox[\"CPUPlot\",\nFontWeight->\"Bold\"]\)[] plot RAM, ALU, CU and stack.";

LoadProgram::usage = "\!\(\*
StyleBox[\"LoadProgram\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"program\",\nFontSlant->\"Italic\"]\)] load program to RAM.";
ClearProgram::usage = "\!\(\*
StyleBox[\"ClearProgram\",\nFontWeight->\"Bold\"]\)[] clear RAM and fill it with zero values.";

InstructionsPlot::usage = "\!\(\*
StyleBox[\"InstructionsPlot\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"code\",\nFontSlant->\"Italic\"]\)] plot the instructions in assembler.";
OutputPlot::usage = "\!\(\*
StyleBox[\"OutputPlot\",\nFontWeight->\"Bold\"]\)[] plot the output buffer.";
ExecutionPlot::usage = "\!\(\*
StyleBox[\"ExecutionPlot\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"code\",\nFontSlant->\"Italic\"]\)] plot RAM, ALU, CU, output, instructions and stack.";
ViewExecution::usage = "\!\(\*
StyleBox[\"ViewExecution\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"code\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"maxIterations\",\nFontSlant->\"Italic\"]\)] view every execution step in a dynamic panel.";


(* ::Chapter:: *)
(*Begin package*)


Begin["`Private`"]


(* ::Subchapter:: *)
(*Opcodes*)


(* ::Input::Initialization:: *)
cpuMnemonics = {
    "LoadA","LoadB","SetA","SetB","Store","Add",
    "Substract","Increment","Decrement","Jump","JumpPos","JumpNeg","JumpZero","JumpNotZero","JumpOdd",
    "Call","Return","Print","Halt"
};
opcodes = Thread[cpuMnemonics->Take[Tuples[{0,1}, 8], Length[cpuMnemonics]]];
revOpcodes = Map[Reverse, opcodes];
OpCode[mnemonic_] := ReplaceAll[mnemonic, opcodes];


(* ::Subchapter:: *)
(*CPU*)


(* ::Input::Initialization:: *)
DecimalToBinary[n_,size_ :8] := PadLeft[IntegerDigits[n,2], size];
BinaryNot[0] = 1;
BinaryNot[1] = 0;
BinaryToDecimal[l_] := FromDigits[l,2];
BinaryIncrement[l_,n_ :1] := DecimalToBinary[BinaryToDecimal[l]+n,Length[l]];
BinaryDecrement[l_, n_ :1] := DecimalToBinary[BinaryToDecimal[l]-n,Length[l]];

UpdateParity[] := If[EvenQ[BinaryToDecimal[ARegister]],ALUParityFlag = 1, ALUParityFlag = 0];
UpdateALU[] := Block[{n},
    n = BinaryToDecimal[ARegister];
    If[n > 0,ALUPositiveFlag = 1,ALUPositiveFlag = 0];
    If[n < 0,ALUNegativeFlag = 1,ALUNegativeFlag = 0];
    If[n == 0,ALUZeroFlag = 1,ALUZeroFlag = 0];
    If[n != 0,ALUNotZeroFlag = 1,ALUNotZeroFlag = 0];
];
UpdateALU[numericResult_] := Block[{},
    If[numericResult > 0,ALUPositiveFlag = 1,ALUPositiveFlag = 0];
    If[numericResult < 0,ALUNegativeFlag = 1,ALUNegativeFlag = 0];
    If[numericResult == 0,ALUZeroFlag = 1,ALUZeroFlag = 0];
    If[numericResult != 0,ALUNotZeroFlag = 1,ALUNotZeroFlag = 0];
];
LoadA[address_] := Block[{},
    ARegister = RAM[[BinaryToDecimal[address]+1]];
    UpdateParity[];
    UpdateALU[];
    AddressAdvance[];
];
LoadB[address_] := Block[{},
    BRegister = RAM[[BinaryToDecimal[address]+1]];
    UpdateParity[];
    AddressAdvance[];
];
SetA[value_] := Block[{},
    ARegister = value;
    UpdateALU[];
    AddressAdvance[];
];
SetB[value_] := Block[{},
    BRegister = value;
    AddressAdvance[];
];
Store[address_] := Block[{},
    RAM[[BinaryToDecimal[address]+1]] = ARegister;
    AddressAdvance[];
];
Add[] := Block[{numericResult},
    numericResult = BinaryToDecimal[ARegister]+BinaryToDecimal[BRegister];
    ARegister = DecimalToBinary[numericResult];
    UpdateALU[numericResult];
    UpdateParity[];
    AddressAdvance[];
];
Substract[] := Block[{numericResult},
    numericResult = BinaryToDecimal[ARegister]-BinaryToDecimal[BRegister];
    ARegister = DecimalToBinary[numericResult];
    UpdateALU[numericResult];
    UpdateParity[];
    AddressAdvance[];
];
UIncrement[] := Block[{numericResult},
    numericResult = BinaryToDecimal[ARegister]+1;
    ARegister = DecimalToBinary[numericResult];
    UpdateALU[numericResult];
    UpdateParity[];
    AddressAdvance[];
];
UDecrement[] := Block[{numericResult},
    numericResult = BinaryToDecimal[ARegister]-1;
    ARegister = DecimalToBinary[numericResult];
    UpdateALU[numericResult];
    UpdateParity[];
    AddressAdvance[];
];
Jump[address_] := (addressRegister = address);
JumpConditional[address_, condition_] := If[condition==1,
    addressRegister = address;
  ,
    AddressAdvance[];
];

Call[address_] := Block[{stackAddress},
    stackAddress = BinaryToDecimal[stackPointerRegister]+1;
    stack[[stackAddress]]= addressRegister;
    stackPointerRegister = BinaryIncrement[stackPointerRegister];
    addressRegister = address;
];
ReturnC[] := Block[{stackAddress},
    If[BinaryToDecimal[stackPointerRegister]!=0,
        stackPointerRegister = BinaryDecrement[stackPointerRegister];
        stackAddress = BinaryToDecimal[stackPointerRegister]+1;
        addressRegister = stack[[stackAddress]];
        stack[[stackAddress]] = {0,0,0,0,0,0,0,0};
        AddressAdvance[];
        ,
        halted = True;
    ];
];
PrintA[address_]:=Block[{},
    AppendTo[outputBuffer,BinaryToDecimal[Part[RAM,BinaryToDecimal[address]+1]]];
    AddressAdvance[];
];


(* ::Input::Initialization:: *)
ResetCPU[] := Block[{},
   halted = False;
   ARegister = {0, 0, 0, 0, 0, 0, 0, 0};
   BRegister = {0, 0, 0, 0, 0, 0, 0, 0};
   
   addressRegister = {0, 0, 0, 0, 0, 0, 0, 0};
   instructionRegister = {0, 0, 0, 0, 0, 0, 0, 0};
   parameterRegister = {0, 0, 0, 0, 0, 0, 0, 0};
   stackPointerRegister = {0, 0, 0, 0, 0, 0, 0, 0};
   
   ALUPositiveFlag = 0;
   ALUNegativeFlag = 0;
   ALUZeroFlag = 0;
   ALUNotZeroFlag = 0;
   ALUParityFlag = 0;
   
   RAM = ConstantArray[{0, 0, 0, 0, 0, 0, 0, 0}, 256];
   stack = ConstantArray[{0, 0, 0, 0, 0, 0, 0, 0}, 8];
   
   outputBuffer = {};
];
ResetCPU[];

AddressAdvance[] := (addressRegister = BinaryIncrement[addressRegister, 2]);
CPU[]:=Block[{},
    If[!halted,
        instructionRegister = Part[RAM, BinaryToDecimal[addressRegister]+1];
        parameterRegister = Part[RAM, BinaryToDecimal[addressRegister]+2];

        If[instructionRegister == OpCode["LoadA"], LoadA[parameterRegister]];
        If[instructionRegister == OpCode["LoadB"],LoadB[parameterRegister]];
        If[instructionRegister == OpCode["SetA"], SetA[parameterRegister]];
        If[instructionRegister == OpCode["SetB"], SetB[parameterRegister]];
        If[instructionRegister == OpCode["Store"], Store[parameterRegister]];
        If[instructionRegister == OpCode["Add"], Add[]];
        If[instructionRegister == OpCode["Substract"], Substract[]];
        If[instructionRegister == OpCode["Increment"], UIncrement[]];
        If[instructionRegister == OpCode["Decrement"], UDecrement[]];
        If[instructionRegister == OpCode["Print"], PrintA[parameterRegister]];

        If[instructionRegister == OpCode["Jump"], Jump[parameterRegister]];
        If[instructionRegister == OpCode["JumpPos"], JumpConditional[parameterRegister, ALUPositiveFlag]];
        If[instructionRegister == OpCode["JumpNeg"], JumpConditional[parameterRegister, ALUNegativeFlag]];
        If[instructionRegister == OpCode["JumpZero"], JumpConditional[parameterRegister, ALUZeroFlag]];
        If[instructionRegister == OpCode["JumpNotZero"], JumpConditional[parameterRegister, ALUNotZeroFlag]];
        If[instructionRegister == OpCode["JumpOdd"], JumpConditional[parameterRegister, BinaryNot[ALUParityFlag]]];
        If[instructionRegister == OpCode["Call"], Call[parameterRegister]];
        If[instructionRegister == OpCode["Return"], ReturnC[]];
    ]
];


(* ::Subchapter:: *)
(*Carga de programa a la memoria*)


(* ::Input::Initialization:: *)
LoadProgram[program_] := Block[{x}, RAM = PadRight[program, 256, x] /. x->{0,0,0,0,0,0,0,0}];
ClearProgram[] := (RAM = ConstantArray[{0,0,0,0,0,0,0,0}, 256]);


(* ::Subchapter:: *)
(*Plotting*)


(* ::Input::Initialization:: *)
HalfSplit[l_] := Block[{len},
    len = Length[l]/2;
    {Take[l,len], Drop[l,len]}
];
BinaryToHex[n_] := BaseForm[FromDigits[n,2], 16];

RAMPanel[RAM_, base_] := Block[{currentAddress,cursor,ramInDecimal},
    currentAddress = BinaryToDecimal[addressRegister];
    cursor = currentAddress-base+2;
    If[Negative[cursor], cursor = 0];

    ramInDecimal = Map[BinaryToDecimal, RAM];
    Grid[Join[{{"Address","Binary value","Decimal value"}},Thread[{Range[base, base+15], RAM, ramInDecimal}]], Background->{None, {1->LightGreen,cursor->Yellow}}, Frame->All]
];
RAMPlot[] := Block[{tabs, currentTab},
    tabs = MapThread[RAMPanel, {Partition[RAM, 16], Range[0, 255, 16]}];
    currentTab = Ceiling[(BinaryToDecimal[addressRegister]+1)/16];

    Panel[TabView[Thread[Range[0,255,16]->tabs], currentTab], Style["RAM",Bold]]
];

ALUStatusPlot[] := Panel[
    Grid[
        {
            {"Positive: ",ALUPositiveFlag},
            {"Negative: ",ALUNegativeFlag},
            {"Zero: ",ALUZeroFlag},
            {"NotZero: ",ALUNotZeroFlag},
            {"Parity: ",ALUParityFlag}
        },
        Frame->All,
        Background->{{LightRed}, None}
    ],
    Style["ALU Status", Bold]
];

CUStatusPlot[] := Block[{mainRegisters, instructionRegisters, flagsStatus},
    mainRegisters = Grid[
        {
            {"","Register A","Register B"},
            {"Binary",Row[ARegister],Row[BRegister]},
            {"Decimal",BinaryToDecimal[ARegister],BinaryToDecimal[BRegister]}
        },
        Frame->All,
        Background->{None, {LightGreen}}
    ];

    instructionRegisters = Grid[
    {
    {"Instruction Register","Address Register"},
    {
    Grid[
    {
    {"Opcode","Parameter"},
    {Row[instructionRegister],Row[parameterRegister]},
    {instructionRegister/.revOpcodes,BinaryToDecimal[parameterRegister]}
    },
    Frame->All,Background->{None,{LightCyan}}
    ],
    Grid[
    {
    {"Binary",Row[addressRegister]},
    {"Decimal",BinaryToDecimal[addressRegister]}
    },
    Frame->All,Background->{{LightCyan},None}
    ]
    }
    },
    Frame->All,
    Background->{None,{LightBlue}}
    ];
    Panel[Column[{mainRegisters,instructionRegisters}],Style["CU Status",Bold],ImageSize->300]
];
StackPlot[] := Block[{cursor, stackInDecimal, plt},
    cursor = BinaryToDecimal[stackPointerRegister]+2;
    stackInDecimal = Map[BinaryToDecimal,stack];
    plt = Join[{{"Stack level","Binary value","Decimal value"}},Thread[{Range[1,8],stack,stackInDecimal}]];
    Panel[Grid[plt, Background->{None, {1->LightGreen, cursor->Yellow}}, Frame->All], Style["Stack", Bold]]
];

CPUPlot[] := Panel[Grid[{{ALUStatusPlot[], Column[{CUStatusPlot[], StackPlot[]}], RAMPlot[]}}, Alignment->Top], Style["CPU", Bold]];


(* ::Subchapter:: *)
(*Visualizaci\[OAcute]n de ejecuci\[OAcute]n*)


(* ::Input::Initialization:: *)
GetInstructions[asmcode_] := Block[{separated, completed},
    separated = DeleteCases[Map[StringSplit, StringSplit[asmcode, "\n"]], {}];
    completed = ReplaceAll[separated, l_ /; (Length[l] == 1) :> Append[l, 0]];
    Return[completed];
];
GetPosition[instructions_, token_] := Block[{pos, labels, countingRules},
    pos = Position[instructions, token];
    If[Length[pos]>1, Return[$Failed]];
    labels = Take[instructions, pos[[1,1]]-1][[All,1]];
    countingRules = Join[Thread[cpuMnemonics->2], {"Label"->0, "Declare"->1}];

    Total[ReplaceAll[labels, countingRules]]
];
ProcessTags[instructions_] := Block[{labels,variablePos},
    labels = Cases[instructions,{"Label",tag_} :> (tag->GetPosition[instructions, {"Label", tag}])];
    variablePos = Cases[instructions, {"Declare", tag_, value_} :> (tag->GetPosition[instructions, {"Declare", tag, value}])];
    Join[labels, variablePos]
];
MachineInstructions[instructions_] := Block[{setNumeric, removeLabels, replaceTags},
    setNumeric = ReplaceAll[instructions, {{"Declare", _, value_} :> ToExpression[value], {"SetA", value_} :> {"SetA", ToExpression[value]}, {"SetB", value_} :> {"SetB", ToExpression[value]} }];
    removeLabels = DeleteCases[setNumeric, {"Label",__}];
    replaceTags = ReplaceAll[removeLabels, ProcessTags[instructions]];
    Return[replaceTags];
];
AssemblyCompile[asmcode_] := Block[{linearized, rules, machineCode},
    linearized = Flatten[MachineInstructions[GetInstructions[asmcode]]];
    rules = Append[opcodes, n_Integer :> DecimalToBinary[n]];
    machineCode = ReplaceAll[linearized, rules];
    Return[machineCode];
];
InstructionsPanel[machineInstructions_,from_] := Block[{cursor,instructions},
    cursor = BinaryToDecimal[addressRegister]-from+1;
    If[Negative[cursor], cursor = 0];
    instructions = Thread[{Range[from, from+Length[Flatten[machineInstructions]]-1], Flatten[machineInstructions]}];

    Grid[
        instructions,
        Background->{{LightGreen}, {cursor->Yellow}}
    ]
];
InstructionsPlot[code_] := Block[{mi,tabs,currentAddress,currentTab},
    mi = Flatten[MachineInstructions[GetInstructions[code]]];
    tabs = MapThread[InstructionsPanel, {Partition[mi, UpTo[16]], Range[0, Length[mi], 16]}];

    currentTab = Ceiling[(BinaryToDecimal[addressRegister]+1)/16];

    Panel[TabView[Thread[Range[0, Length[mi],16]->tabs], currentTab], Style["Program", Bold]]
];


(* ::Input::Initialization:: *)
ColumnJoin[l_] := StringJoin[Riffle[DeleteCases[l,""], "\n"]];
OutputPlot[] := Panel[
    InputField[
        ColumnJoin[Map[ToString, outputBuffer]], String,
        FieldSize->{10,{0,Infinity}},
        Enabled->False
    ],
    Style["Output", Bold]
];
ExecutionPlot[code_] := Block[{cpuPanel, memoryAndOutput},
    cpuPanel = Panel[Grid[{{ALUStatusPlot[], Column[{CUStatusPlot[], StackPlot[]}]}}, Alignment->Top], Style["CPU",Bold]];
    memoryAndOutput = Panel[Grid[{{InstructionsPlot[code], RAMPlot[], OutputPlot[]}}, Alignment->Top], Style["Memory and output",Bold]];

    Column[{cpuPanel, memoryAndOutput}]
];
ViewExecution[code_, maxIterations_ :100] := DynamicModule[{program, frames, i = 0},
    ResetCPU[];
    program = AssemblyCompile[code];
    LoadProgram[program];
    frames = {ExecutionPlot[code]};

    While[!halted && i<maxIterations,
        CPU[];
        AppendTo[frames, ExecutionPlot[code]];
        i++;
    ];

    Manipulate[frames[[iteration]], {{iteration,1,"Iteration"}, 1, Length[frames], 1}]
];


(* ::Chapter:: *)
(*End of package*)


End[ ]

EndPackage[ ]
