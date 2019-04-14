(* ::Package:: *)

(* ::Title:: *)
(*CPU Emulator*)


(* ::Chapter:: *)
(*Begin package*)


BeginPackage["CPUEmulator`"]


(* ::Chapter:: *)
(*Package description*)


DFA::usage ="\!\(\*
StyleBox[\"DFA\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"transitions\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"start\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"accept\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"stateExpr\",\nFontSlant->\"Italic\"]\)] create an DFA from a transition list.";


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
opcodes = Thread[cpuMnemonics->Take[Tuples[{0,1},8],Length[cpuMnemonics]]];
revOpcodes = Map[Reverse,opcodes];
OpCode[mnemonic_]:=ReplaceAll[mnemonic,opcodes];


(* ::Subchapter:: *)
(*CPU*)


(* ::Input::Initialization:: *)
DecimalToBinary[n_,size_ :8]:=PadLeft[IntegerDigits[n,2],size];
BinaryNot[0]=1;
BinaryNot[1]=0;
BinaryToDecimal[l_]:=FromDigits[l,2];
BinaryIncrement[l_,n_ :1]:=DecimalToBinary[BinaryToDecimal[l]+n,Length[l]];
BinaryDecrement[l_, n_ :1]:=DecimalToBinary[BinaryToDecimal[l]-n,Length[l]];

UpdateParity[]:=If[EvenQ[BinaryToDecimal[ARegister]],ALUParityFlag = 1, ALUParityFlag = 0];
UpdateALU[numericResult_]:=Block[{},
If[numericResult > 0,ALUPositiveFlag = 1,ALUPositiveFlag = 0];
If[numericResult < 0,ALUNegativeFlag = 1,ALUNegativeFlag = 0];
If[numericResult == 0,ALUZeroFlag = 1,ALUZeroFlag = 0];
If[numericResult != 0,ALUNotZeroFlag = 1,ALUNotZeroFlag = 0];
];
LoadA[address_]:=Block[{},
ARegister = RAM[[BinaryToDecimal[address]+1]];
UpdateParity[];
];
LoadB[address_]:=Block[{},
BRegister = RAM[[BinaryToDecimal[address]+1]];
UpdateParity[];
];
SetA[value_]:=(ARegister = value);
SetB[value_]:=(BRegister = value);
Store[address_]:=(RAM[[BinaryToDecimal[address]+1]] = ARegister);
Add[]:=Block[{numericResult},
numericResult = BinaryToDecimal[ARegister]+BinaryToDecimal[BRegister];
ARegister = DecimalToBinary[numericResult];
UpdateALU[numericResult];
UpdateParity[];
];
Substract[]:=Block[{numericResult},
numericResult = BinaryToDecimal[ARegister]-BinaryToDecimal[BRegister];
ARegister = DecimalToBinary[numericResult];
UpdateALU[numericResult];
UpdateParity[];
];
UIncrement[]:=Block[{numericResult},
numericResult = BinaryToDecimal[ARegister]+1;
ARegister = DecimalToBinary[numericResult];
UpdateALU[numericResult];
UpdateParity[];
];
UDecrement[]:=Block[{numericResult},
numericResult = BinaryToDecimal[ARegister]-1;
ARegister = DecimalToBinary[numericResult];
UpdateALU[numericResult];
UpdateParity[];
];
Jump[address_]:=(addressRegister = address);
JumpConditional[address_,condition_]:=If[condition==1,
addressRegister = address;
Return[True];
,
Return[False];
];

Call[address_]:=Block[{stackAddress},
stackAddress = BinaryToDecimal[stackPointerRegister]+1;
stack[[stackAddress]]= addressRegister;
stackPointerRegister = BinaryIncrement[stackPointerRegister];
addressRegister = address;
];
ReturnC[]:=Block[{stackAddress},
If[BinaryToDecimal[stackPointerRegister]!=0,
stackPointerRegister = BinaryDecrement[stackPointerRegister];
stackAddress = BinaryToDecimal[stackPointerRegister]+1;
addressRegister = stack[[stackAddress]];
stack[[stackAddress]] = {0,0,0,0,0,0,0,0};
Return[True];
,
halted = True;
Return[False];
];
];
PrintA[address_]:=AppendTo[outputBuffer,BinaryToDecimal[Part[RAM,BinaryToDecimal[address]+1]]];


ResetCPU[]:=(
halted = False;
ARegister = {0,0,0,0,0,0,0,0};
BRegister = {0,0,0,0,0,0,0,0};

addressRegister = {0,0,0,0,0,0,0,0};
instructionRegister = {0,0,0,0,0,0,0,0};
parameterRegister = {0,0,0,0,0,0,0,0};
stackPointerRegister = {0,0,0,0,0,0,0,0};

ALUPositiveFlag = 0;
ALUNegativeFlag = 0;
ALUZeroFlag = 0;
ALUNotZeroFlag = 0;
ALUParityFlag = 0;

RAM = ConstantArray[{0,0,0,0,0,0,0,0},256];
stack = ConstantArray[{0,0,0,0,0,0,0,0},8];

outputBuffer = {};
);
ResetCPU[];

AddressAdvance[]:=(addressRegister = BinaryIncrement[addressRegister,2]);
CPU[]:=Block[{},
If[!halted,
instructionRegister = Part[RAM,BinaryToDecimal[addressRegister]+1];
parameterRegister = Part[RAM,BinaryToDecimal[addressRegister]+2];

If[instructionRegister == OpCode["LoadA"], 
LoadA[parameterRegister];
AddressAdvance[];
];
If[instructionRegister == OpCode["LoadB"],
LoadB[parameterRegister];
AddressAdvance[];
];
If[instructionRegister == OpCode["SetA"], 
SetA[parameterRegister];
AddressAdvance[];
];
If[instructionRegister == OpCode["SetB"], 
SetB[parameterRegister];
AddressAdvance[];
];
If[instructionRegister == OpCode["Store"], 
Store[parameterRegister];
AddressAdvance[];
];
If[instructionRegister == OpCode["Add"], 
Add[];
AddressAdvance[];
];
If[instructionRegister == OpCode["Substract"], 
Substract[];
AddressAdvance[];
];
If[instructionRegister == OpCode["Increment"], 
UIncrement[];
AddressAdvance[];
];
If[instructionRegister == OpCode["Decrement"], 
UDecrement[];
AddressAdvance[];
];
If[instructionRegister == OpCode["Print"],
 PrintA[parameterRegister];
AddressAdvance[];
];

If[instructionRegister == OpCode["Jump"], Jump[parameterRegister]];
If[instructionRegister == OpCode["JumpPos"], 
If[!JumpConditional[parameterRegister,ALUPositiveFlag],AddressAdvance[]]
];
If[instructionRegister == OpCode["JumpNeg"], 
If[!JumpConditional[parameterRegister,ALUNegativeFlag],AddressAdvance[]]
];
If[instructionRegister == OpCode["JumpZero"], 
If[!JumpConditional[parameterRegister,ALUZeroFlag],AddressAdvance[]]
];
If[instructionRegister == OpCode["JumpNotZero"], 
If[!JumpConditional[parameterRegister,ALUNotZeroFlag],AddressAdvance[]]
];
If[instructionRegister == OpCode["JumpOdd"], 
If[!JumpConditional[parameterRegister,BinaryNot[ALUParityFlag]],AddressAdvance[]]
];
If[instructionRegister == OpCode["Call"],Call[parameterRegister]];
If[instructionRegister == OpCode["Return"],
If[ReturnC[],AddressAdvance[]];
];
]
];


(* ::Subchapter:: *)
(*Carga de programa a la memoria*)


(* ::Input::Initialization:: *)
LoadProgram[program_]:=Block[{x},RAM = PadRight[program,256,x]/.x->{0,0,0,0,0,0,0,0}];
ClearProgram[]:=(RAM = ConstantArray[{0,0,0,0,0,0,0,0},256]);


(* ::Subchapter:: *)
(*Plotting*)


(* ::Input::Initialization:: *)
HalfSplit[l_]:=Block[{len},
len = Length[l]/2;
{Take[l,len],Drop[l,len]}
];
BinaryToHex[n_]:=BaseForm[FromDigits[n,2],16];

RAMPanel[RAM_,base_]:=Block[{currentAddress,cursor,ramInDecimal},
currentAddress = BinaryToDecimal[addressRegister];
cursor = currentAddress-base+2;
If[Negative[cursor], cursor = 0];

ramInDecimal = Map[BinaryToDecimal,RAM];
Grid[Join[{{"Address","Binary value","Decimal value"}},Thread[{Range[base,base+15],RAM,ramInDecimal}]],Background->{None,{1->LightGreen,cursor->Yellow}},Frame->All]
];
RAMPlot[]:=Block[{tabs,currentTab},
tabs = MapThread[RAMPanel,{Partition[RAM,16],Range[0,255,16]}];
currentTab = Ceiling[(BinaryToDecimal[addressRegister]+1)/16];

Panel[TabView[Thread[Range[0,255,16]->tabs],currentTab],Style["RAM",Bold]]
];

ALUStatusPlot[]:=Panel[
Grid[{
{"Positive: ",ALUPositiveFlag},
{"Negative: ",ALUNegativeFlag},
{"Zero: ",ALUZeroFlag},
{"NotZero: ",ALUNotZeroFlag},
{"Parity: ",ALUParityFlag}
},
Frame->All,
Background->{{LightRed},None}
],
Style["ALU Status",Bold]
];

CUStatusPlot[]:=Block[{mainRegisters,instructionRegisters,flagsStatus},
mainRegisters = Grid[
{
{"","Register A","Register B"},
{"Binary",Row[ARegister],Row[BRegister]},
{"Decimal",BinaryToDecimal[ARegister],BinaryToDecimal[BRegister]}
},
Frame->All,
Background->{None,{LightGreen}}
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
StackPlot[]:=Block[{cursor,stackInDecimal,plt},
cursor = BinaryToDecimal[stackPointerRegister]+2;
stackInDecimal = Map[BinaryToDecimal,stack];
plt = Join[{{"Stack level","Binary value","Decimal value"}},Thread[{Range[1,8],stack,stackInDecimal}]];
Panel[Grid[plt,Background->{None,{1->LightGreen,cursor->Yellow}},Frame->All],Style["Stack",Bold]]
];

CPUPlot[]:=Panel[Grid[{{ALUStatusPlot[],Column[{CUStatusPlot[],StackPlot[]}],RAMPlot[]}},Alignment->Top],Style["CPU",Bold]];


(* ::Subchapter:: *)
(*Visualizaci\[OAcute]n de ejecuci\[OAcute]n*)


(* ::Input::Initialization:: *)
InstructionsPanel[machineInstructions_,from_]:=Block[{cursor,instructions},
cursor = BinaryToDecimal[addressRegister]-from+1;
If[Negative[cursor], cursor = 0];
instructions = Thread[{Range[from,from+Length[Flatten[machineInstructions]]-1],Flatten[machineInstructions]}];

Grid[
instructions,
Background->{{LightGreen},{cursor->Yellow}}
]
];
InstructionsPlot[code_]:=Block[{mi,tabs,currentAddress,currentTab},
mi =Flatten[ MachineInstructions[GetInstructions[code]]];
tabs = MapThread[InstructionsPanel,{Partition[mi,UpTo[16]],Range[0,Length[mi],16]}];

currentTab = Ceiling[(BinaryToDecimal[addressRegister]+1)/16];

Panel[TabView[Thread[Range[0,Length[mi],16]->tabs],currentTab],Style["Program",Bold]]
];


(* ::Input::Initialization:: *)
OutputPlot[]:=Panel[
InputField[
ColumnJoin[Map[ToString,outputBuffer]],String,
FieldSize->{10,{0,Infinity}},
Enabled->False
],
Style["Output",Bold]
];
ExecutionPlot[code_]:=Block[{cpuPanel,memoryAndOutput},
cpuPanel = Panel[Grid[{{ALUStatusPlot[],Column[{CUStatusPlot[],StackPlot[]}]}},Alignment->Top],Style["CPU",Bold]];
memoryAndOutput = Panel[Grid[{{InstructionsPlot[code],RAMPlot[],OutputPlot[]}},Alignment->Top],Style["Memory and output",Bold]];

Column[{cpuPanel,memoryAndOutput}]
];
ViewExecution[code_,maxIterations_ :100]:=DynamicModule[{program,frames, i = 0},
ResetCPU[];
program = AssemblyCompile[code];
LoadProgram[program];
frames = {ExecutionPlot[code]};

While[!halted && i<maxIterations,
CPU[];
AppendTo[frames,ExecutionPlot[code]];
i++;
];

Manipulate[frames[[iteration]],{iteration,1,Length[frames],1}]
];


(* ::Chapter:: *)
(*End of package*)


End[ ]

EndPackage[ ]
