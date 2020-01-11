(* ::Package:: *)

(* ::Title:: *)
(*CPU Emulator*)


(* ::Chapter:: *)
(*Begin package*)


BeginPackage["CPU`"]


(* ::Chapter:: *)
(*Package description*)


MemoryEvaluate::usage = "";
VisualizeGate::usage = "";
VisualizeAdder::usage = "";
VisualizeMultiplexer::usage = "";
PlotExpression::usage = "";
PlotSystem::usage = "";
PlotInteractiveSystem::usage = "";
PlotSystemEvaluations::usage = "";
CompactTreeForm::usage = "";
CompactEvaluatedTreeForm::usage = "";
PlotCompactInteractiveSystem::usage = "";
PlotCompactSystemEvaluations::usage = "";

Mux2to1::usage = "";
Mux4to1::usage = "";
Mux8to1::usage = "";
MultiplexerNTo1::usage = "";
MuxInputByOpcode::usage = "";
Multiplexer8Bit::usage = "";
Multiplexer3Bit::usage = "";

HalfAdder::usage = "";
FullAdder::usage = "";
ALUAdder::usage = "";
ALUSubstractor::usage = "";
ALUOpCode::usage = "";
ALUZero::usage = "";
ALUNotZero::usage = "";
ALUParity::usage = "";
ALUExecute::usage = "";
ALUClear::usage = "";

AndOrLatch::usage = "";
GatedLatch::usage = "";
Flag::usage = "";
ReadFlag::usage = "";
WriteFlag::usage = "";
Bit8AddressCell::usage = "";
Bit8AddressBlock::usage = "";
ReadBit8AddressBlock::usage = "";
WriteBit8AddressBlock::usage = "";
ResetBit8AddressBlock::usage = "";

Bit3Register::usage = "";
ReadBit3Register::usage = "";
WriteBit3Register::usage = "";
Bit8Register::usage = "";
ReadBit8Register::usage = "";
WriteBit8Register::usage = "";

CUClear::usage = "";
OpCode::usage = "";
CUFetch::usage = "";
CUDecode::usage = "";
CUExecute::usage = "";

LoadProgram::usage = "";
ClearProgram::usage = "";
CPUPlot::usage = "";
CPUAndProgramPlot::usage = "";
ExecuteCodeInteractive::usage = "";

ExecuteProgram::usage = "";
ExecuteCode::usage = "";


(* ::Chapter:: *)
(*Begin package*)


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Auxiliary*)


(* ::Input::Initialization:: *)
HalfSplit[l_] := TakeDrop[l, Floor[Length[l]/2]];

SetAttributes[MemoryEvaluate,HoldAll];
MemoryEvaluate[expr_,tag_] :=
    Block[ {},
        If[ Head[MemorySymbol[tag]]== MemorySymbol,
            MemorySymbol[tag] = 0;
        ];
        MemorySymbol[tag] = ReleaseHold[expr[MemorySymbol[tag]]];
        Return[MemorySymbol[tag]];
    ];

Unprotect[BitAnd];
BitAnd[1,x_] :=x;
BitAnd[x_,1] :=x;
Protect[BitAnd];

Unprotect[BitNot];
BitNot[0] := 1;
BitNot[1] := 0;
Protect[BitAnd];

DecimalToBinary[n_,size_ :8] :=
    PadLeft[IntegerDigits[n,2],size];
BinaryToDecimal[l_] :=
    FromDigits[l,2];


(* ::Section:: *)
(*Visualization*)


(* ::Subsection::Closed:: *)
(*Circuit result tables*)


(* ::Input::Initialization:: *)
VisualizeGate[module_] := Block[{logicTable,labels,gridList},
logicTable =  Map[Join[#,{Apply[module,#]}]&,Tuples[{0,1},2]];
labels = Join[Take[Map[ToUpperCase,Alphabet[]],2],{"Result"}];
gridList = Join[{labels},logicTable];
Grid[gridList,Frame->All,Background->{None,{LightGreen}}]
];
VisualizeAdder[module_,inputSize_,flagName_] :=
    Block[ {logicTable,labels,gridList},
        logicTable =  Map[Join[#,Apply[module,#]]&,Tuples[{0,1},inputSize]];
        labels = Join[Take[Map[ToUpperCase,Alphabet[]],inputSize],{flagName,"Result"}];
        gridList = Join[{labels},logicTable];
        Grid[gridList,Frame->All,Background->{None,{LightGreen}}]
    ];

VisualizeMultiplexer[mux_,inputSize_] :=
    Block[ {logicTable,labels,gridList},
        logicTable =  Map[Join[#,{mux[#,Take[Alphabet[],2^inputSize]]}]&,Tuples[{0,1},inputSize]];
        labels = Join[Take[Map[ToUpperCase,Alphabet[]],inputSize],{"Selection"}];
        gridList = Join[{labels},logicTable];
        Grid[gridList,Frame->All,Background->{None,{LightGreen}}]
    ];


(* ::Subsection::Closed:: *)
(*Expression graph*)


(* ::Input::Initialization:: *)
EvaluateHead[expr_,values_] :=
    ReplaceRepeated[expr,{f_Symbol[arg__]:>ReplaceAll[f[arg],values][arg]}];

StructuredEvaluateHead[expr_,values_] :=
    Block[ {logicOp},
        logicOp = {BitOr,BitAnd,BitXor,BitNot};
        ReplaceRepeated[expr,{f_Symbol[arg__]/;MemberQ[logicOp,f]:>{f,ReplaceAll[f[arg],values]}[arg]}]
    ];


(* ::Input::Initialization:: *)
NestedEvaluate[expr_,values_] :=
    If[ Head[expr]===List,
        Map[NestedEvaluate[#,values]&,Level[expr,1]],
        ReplaceAll[EvaluateHead[expr,values],values]
    ];

StructuredNestedEvaluate[expr_,values_] :=
    If[ Head[expr]===List,
        Map[StructuredNestedEvaluate[#,values]&,Level[expr,1]],
        ReplaceAll[StructuredEvaluateHead[expr,values],values]
    ];


(* ::Input::Initialization:: *)
SetOptions[$FrontEndSession,AutoStyleOptions->{"HighlightFormattingErrors"->False}];

SelectColor[coord_,edge_] :=
    If[ edge =!= List,
        If[ Head[First[edge]] =!=Symbol,
            If[ Last[edge]==1,
                {ColorData["HTML"]["LawnGreen"],Line[coord]},
                {GrayLevel[0.3],Line[coord]}
            ],
            If[ Head[Last[edge]] =!=Symbol,
                If[ Last[edge]==1,
                    {ColorData["HTML"]["LawnGreen"],Line[coord]},
                    {GrayLevel[0.3],Line[coord]}
                ],
                {ColorData["HTML"]["SteelBlue"],Line[coord]}
            ]
        ]
    ];

PlotExpression[expr_,values_,opts: OptionsPattern[]] :=
    TreeForm[NestedEvaluate[expr,values],
    VertexRenderingFunction->None,
    EdgeRenderingFunction->(
    ReleaseHold[SelectColor[#,#2]]&),
    AspectRatio->1,
    Background->Black,
    opts
    ];


(* ::Subsection::Closed:: *)
(*Interactive expression plot*)


(* ::Input::Initialization:: *)
GetSymbols[system_] :=
    Complement[
    DeleteDuplicates[Level[system,{-1},Heads->True]],
    {List,BitOr,BitAnd,BitXor,BitNot,0,1,Association}
    ];

NodeColor[value_] :=
    White;
NodeColor[{op_,value_}] :=
    ReplaceAll[{op,value},
    {
    {BitOr,0}->RGBColor[0., 0.16, 0.44] ,{BitOr,1}->RGBColor[0.26, 0.61, 1.],
    {BitAnd,0}->RGBColor[0.64, 0.52, 0.] ,{BitAnd,1}->RGBColor[0.9500000000000001, 1., 0.02],
    {BitXor,0}->RGBColor[0.03, 0.32, 0.] ,{BitXor,1}->RGBColor[0.54, 1., 0.1],
    {BitNot,0}->RGBColor[0.38, 0.03, 0.] ,{BitNot,1}->RGBColor[1., 0.23, 0.2],
    {_,_}->White
    }
    ];
SelectValue[value_] :=
    value;
SelectValue[{op_,value_}] :=
    value;

SetOptions[$FrontEndSession,DynamicEvaluationTimeout->20];

PlotSystem[system_,values_,opts: OptionsPattern[]]:=
TreeForm[
StructuredNestedEvaluate[system,values],
EdgeRenderingFunction->(
ReleaseHold[SelectColor[#,SelectValue[#2]]]&),
VertexRenderingFunction->({NodeColor[#2],EdgeForm[Black],Disk[#,.1],Black,Text[SelectValue[#2],#1]}&),
ImageSize->1000,
AspectRatio->1/2,
Background->Black,
FilterRules[{opts},Options[TreeForm]]
];

PlotInteractiveSystem[system_,plotVertex_:({NodeColor[#2],EdgeForm[Black],Disk[#,.1],Black,Text[SelectValue[#2],#1]}&)] :=
    DynamicModule[{symbols,controllerSymbols,boxes,controls,plot},
    symbols = GetSymbols[system];
    controllerSymbols = Map[Symbol[StringJoin[SymbolName[#],ToString[$ModuleNumber]]]&,symbols];
    boxes = Map[Checkbox[Dynamic[#],{0,1}]&,controllerSymbols];
    controls = Panel[Multicolumn[Map[Row[{SymbolName[#[[1]]]<>": ",#[[2]]}]&,Transpose[{symbols,boxes}]],5],Style["Control",Bold]];
    plot = Dynamic[
    TreeForm[
    StructuredNestedEvaluate[system,Thread[symbols->controllerSymbols]],
    EdgeRenderingFunction->(
    ReleaseHold[SelectColor[#,SelectValue[#2]]]&),
    VertexRenderingFunction->plotVertex,
    ImageSize->1000,
    AspectRatio->1/2,
    Background->Black
    ]
    ];
    Panel[Column[{controls,Framed[plot]}]]
    ,
    SaveDefinitions->True
];
PlotSystemEvaluations[system_,opts: OptionsPattern[]]:=Block[{symbols,replacements},
symbols = GetSymbols[system];
replacements = Map[symbols->#&,Tuples[{0,1},Length[symbols]]];
Map[
Panel[
Column[
{
Framed[Row[{Style["Input: ",Bold],Thread[#]}]],
Framed[Row[{Style["Result: ",Bold],ReplaceAll[system,Thread[#]]}]],
Framed[PlotSystem[system,Thread[#],opts]]
}
]
]&,
replacements
]
];


(* ::Subsection::Closed:: *)
(*Compact tree graph*)


(* ::Input::Initialization:: *)
GetLabelName[label_] :=
    Replace[label,Framed[Style[labelName_,___],___]:>labelName];
TreeFormToGraph[treeForm_] :=
    Module[ {graph,tree = ToExpression@ToBoxes@treeForm,order,pos,label},
        label = Cases[tree,Inset[name_,n_]:>Rule[n,GetLabelName[name]],Infinity];
        {order,pos} = Catenate/@Cases[tree,Line[order_]|GraphicsComplex[pos_,___]:>{order,pos},Infinity];
        graph = Graph[DirectedEdge@@@order,VertexLabels->label,VertexCoordinates->MapIndexed[Rule[First[#2],#]&,pos]];
        {graph,ReleaseHold[label]}
    ];

RuleListToMatrix[list_] :=
    Transpose[{list[[All,1]],list[[All,2]]}];
SeparateFirst[l_] :=
    {First[l],Drop[l,1]};
MakeReplaceRules[fixed_,toBeReplaced_] :=
    Map[(vertex_\[DirectedEdge]#):>(vertex\[DirectedEdge]fixed)&,toBeReplaced];

ReplaceRuleForLabel[labels_,labelName_] :=
    Block[ {fixed,toBeReplaced,replaceRules},
        {fixed,toBeReplaced} = SeparateFirst[Cases[RuleListToMatrix[labels],{vertex_,labelName}:>vertex]];
        replaceRules = MakeReplaceRules[fixed,toBeReplaced];
        MakeReplaceRules[fixed,toBeReplaced]
    ];
ReplaceRuleForAllLabels[labels_] :=
    Block[ {nameLabels},
        nameLabels = DeleteCases[
        DeleteDuplicates[labels[[All,2]]],
        x_ /;MemberQ[{List,BitOr,BitAnd,BitXor,BitNot,0,1,Association},x]
        ];
        Flatten[Map[ReplaceRuleForLabel[labels,#]&,nameLabels]]
    ];

CompactTreeForm[expr_] :=
    Block[ {graph,labels,newGraph,deleteVertex,newLabels},
        {graph,labels} = TreeFormToGraph[TreeForm[expr]];
        newGraph = ReplaceAll[EdgeList[graph],ReplaceRuleForAllLabels[labels]];
        deleteVertex = Complement[labels[[All,1]],VertexList[newGraph]];
        newLabels = DeleteCases[labels,x_/;MemberQ[deleteVertex,First[x]]];
        Graph[ReplaceAll[newGraph,x_\[DirectedEdge]y_:>y\[DirectedEdge]x],VertexLabels->newLabels,DirectedEdges->True]
    ];


(* ::Input::Initialization:: *)
DecorateVertexes[labels_] :=
    Block[ {and,xor,or,not,list},
        and = Map[Style[#,RGBColor[0.03, 0.8200000000000001, 0.]]&,Cases[labels,HoldPattern[x_->BitAnd]:>x]];
        xor = Map[Style[#,RGBColor[0.9, 0.14, 0.]]&,Cases[labels,HoldPattern[x_->BitXor]:>x]];
        or = Map[Style[#,RGBColor[0.26, 0.39, 1.]]&,Cases[labels,HoldPattern[x_->BitOr]:>x]];
        not = Map[Style[#,RGBColor[1., 1., 0.04]]&,Cases[labels,HoldPattern[x_->BitNot]:>x]];
        list = Map[Style[#,RGBColor[0.12, 0.1, 0.]]&,Cases[labels,HoldPattern[x_->List]:>x]];
        Join[and,xor,or,not,list]
    ];

Options[CompactEvaluatedTreeForm] = Join[{"ShowLabels"->Automatic},Options[Graph]];
CompactEvaluatedTreeForm[expr_,values_,opts: OptionsPattern[]] :=
    Block[ {
    graph,labels,newGraph,deleteVertex,newLabels,
    evaluatedGraph,evaluatedLabels,newEvaluatedGraph,deleteEvaluatedVertex,newEvaluatedLabels,
    highlightedVertexes,highlightedEdges
    },
        {graph,labels} = TreeFormToGraph[TreeForm[expr]];
        newGraph = ReplaceAll[EdgeList[graph],ReplaceRuleForAllLabels[labels]];
        deleteVertex = Complement[labels[[All,1]],VertexList[newGraph]];
        newLabels = DeleteCases[labels,x_/;MemberQ[deleteVertex,First[x]]];
        {evaluatedGraph,evaluatedLabels} = TreeFormToGraph[TreeForm[NestedEvaluate[expr,values]]];
        newEvaluatedGraph = ReplaceAll[EdgeList[evaluatedGraph],ReplaceRuleForAllLabels[evaluatedLabels]];
        deleteEvaluatedVertex = Complement[evaluatedLabels[[All,1]],VertexList[newEvaluatedGraph]];
        newEvaluatedLabels = DeleteCases[evaluatedLabels,x_/;MemberQ[deleteEvaluatedVertex,First[x]]];
        highlightedVertexes = Cases[newEvaluatedLabels,HoldPattern[x_->1]:>x];
        highlightedEdges = Select[EdgeList[newGraph],MemberQ[highlightedVertexes,Last[#]]&];
        graph = Graph[
        ReplaceAll[newGraph,x_\[DirectedEdge]y_:>y\[DirectedEdge]x],
        If[ OptionValue["ShowLabels"]===Automatic,
            VertexLabels->newLabels,
            VertexLabels->None
        ],
        DirectedEdges->True,
        ImageSize->500,
        FilterRules[{opts},Options[Graph]]
        ];
        HighlightGraph[
        ReplaceAll[graph,x_\[DirectedEdge]y_:>y\[DirectedEdge]x],
        Join[ReplaceAll[highlightedEdges,x_\[DirectedEdge]y_:>y\[DirectedEdge]x],DecorateVertexes[labels]],
        VertexSize->0.2,
        FilterRules[{opts},Options[HighlightGraph]]
        ]
    ];
CompactExprMatrixPlot[expr_] :=
    MatrixPlot[AdjacencyMatrix[CompactTreeForm[expr]]];

BinaryResultToDecimal[n_List /; Depth[n]>2] :=
    Map[BinaryResultToDecimal,n];
BinaryResultToDecimal[n_List /; Depth[n]==2] :=
    FromDigits[n,2];
BinaryResultToDecimal[n_] :=
    n;

PlotCompactInteractiveSystem[system_,opts: OptionsPattern[]] :=
    DynamicModule[{symbols,controllerSymbols,boxes,controls,gathered},
    symbols = GetSymbols[system];
    controllerSymbols = Map[Symbol[StringJoin[SymbolName[#],ToString[$ModuleNumber]]]&,symbols];
    boxes = Map[Checkbox[Dynamic[#],{0,1}]&,controllerSymbols];
    controls = Panel[Multicolumn[Map[Row[{SymbolName[#[[1]]]<>": ",#[[2]]}]&,Transpose[{symbols,boxes}]],5],Style["Input",Bold]];
    gathered = GatherBy[symbols,StringTake[SymbolName[#],1]&];
    Panel[
    Column[
    {
    controls,
    Framed[Row[{Style["Input in decimal: ",Bold],Dynamic[BinaryResultToDecimal[ReplaceAll[gathered,Thread[symbols->controllerSymbols]]]]}]],
    Framed[Row[{Style["Result: ",Bold],Dynamic[ReplaceAll[system,Thread[symbols->controllerSymbols]]]}]],
    Framed[Row[{Style["Result in decimal: ",Bold],Dynamic[BinaryResultToDecimal[ReplaceAll[system,Thread[symbols->controllerSymbols]]]]}]],
    Framed[Dynamic[CompactEvaluatedTreeForm[system,Thread[symbols->controllerSymbols],opts]]]
    }
    ]
    ]
    ,
    SaveDefinitions->True
];

PlotCompactSystemEvaluations[system_,opts: OptionsPattern[]]:=Block[{symbols,replacements},
symbols = GetSymbols[system];
replacements = Map[symbols->#&,Tuples[{0,1},Length[symbols]]];
Map[
Panel[
Column[
{
Framed[Row[{Style["Input: ",Bold],Thread[#]}]],
Framed[Row[{Style["Result: ",Bold],ReplaceAll[system,Thread[#]]}]],
Framed[CompactEvaluatedTreeForm[system,Thread[#],opts]]
}
]
]&,
replacements
]
];


(* ::Section:: *)
(*Multiplexer*)


(* ::Subsection::Closed:: *)
(*Multiplexer definition*)


(* ::Input::Initialization:: *)
Mux2to1[{select_},{input1_,input2_}] :=
    BitOr[BitAnd[input1,BitNot[select]],BitAnd[input2,select]];


(* ::Input::Initialization:: *)
Mux4to1[{select1_,select2_},{input1_,input2_,input3_,input4_}] :=
    Mux2to1[
    {select1},
    {
    Mux2to1[{select2},{input1,input2}],
    Mux2to1[{select2},{input3,input4}]
    }
    ];


(* ::Input::Initialization:: *)
Mux8to1[{select1_,select2_,select3_},{input1_,input2_,input3_,input4_,input5_,input6_,input7_,input8_}] :=
    Mux2to1[
    {select1},
    {
    Mux4to1[{select2,select3},{input1,input2,input3,input4}],
    Mux4to1[{select2,select3},{input5,input6,input7,input8}]
    }
    ];


(* ::Input::Initialization:: *)
MultiplexerNTo1[2,{select_},input__] :=
    BitOr[BitAnd[First[input],BitNot[select]],BitAnd[Last[input],select]];
MultiplexerNTo1[n_,select_,input__] :=
    Block[ {partInput},
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


(* ::Subsection::Closed:: *)
(*Multiplexer selection by opcode*)


(* ::Input::Initialization:: *)
MuxInputByOpcode[n_,opcodeAndOutput_] :=
    Block[ {addresses,missing},
        addresses = Tuples[{0,1},n]; (* Generate the list of all opcode inputs.  *)
        missing = Thread[{Complement[addresses,opcodeAndOutput[[All,1]]],0}]; (* Select the ones with no inputs and map them to zero. *)
        Return[SortBy[Join[missing,opcodeAndOutput],First][[All,2]]]
    (* Join missing opcodes with the given opcodes and sort. *)
        ];

Multiplexer8Bit[opcode_,opcodeAndOutput_] :=
    Block[ {opcodeLength = 8},
        MultiplexerNTo1[2^opcodeLength,opcode,MuxInputByOpcode[opcodeLength,opcodeAndOutput]]
    ];

Multiplexer3Bit[opcode_,opcodeAndOutput_] :=
    Mux8to1[opcode,MuxInputByOpcode[3,opcodeAndOutput]];


(* ::Section:: *)
(*ALU*)


(* ::Subsection::Closed:: *)
(*Base circuits*)


(* ::Input::Initialization:: *)
HalfAdder[a_,b_] :=
    {BitAnd[a,b],BitXor[a,b]};


(* ::Input::Initialization:: *)
FullAdder[a_,b_,c_] :=
    Block[ {halfAdd1,halfAdd2},
        halfAdd1 = HalfAdder[a,b];
        halfAdd2 = HalfAdder[Last[halfAdd1],c];
        {BitOr[First[halfAdd1],First[halfAdd2]],Last[halfAdd2]}
    ];


(* ::Input::Initialization:: *)
ALUAdder[A_,B_,carry_:0] :=
    Block[ {littleEndianA,littleEndianB,pairs,sum,bigEndianSum},
        littleEndianA = Reverse[A];
        littleEndianB = Reverse[B];

        (* Se crea conjunto de pares a sumar *)
        pairs = Transpose[{littleEndianA,littleEndianB}];

        (* Se a\[NTilde]aden dos pares extra para la \[UAcute]ltima adici\[OAcute]n *)
        pairs = Join[pairs,{{0,0},{0,0}}];
        sum = Drop[FoldPairList[{Last[#1],Apply[FullAdder,Join[{First[#1]},#2]]}&,{carry,0},pairs],1];
        bigEndianSum = Reverse[sum];
        Return[{First[bigEndianSum],Rest[bigEndianSum]}];
    ];


(* ::Input::Initialization:: *)
ALUSubstractor[A_,B_,borrow_:1] :=
    Block[ {littleEndianA,littleEndianB,pairs,sum,bigEndianSum},
        littleEndianA = Reverse[A];
        littleEndianB = Reverse[BitNot[B]];

        (* Se crea conjunto de pares a sumar *)
        pairs = Transpose[{littleEndianA,littleEndianB}];

        (* Se a\[NTilde]aden dos pares extra para la \[UAcute]ltima adici\[OAcute]n *)
        pairs = Join[pairs,{{0,0},{0,0}}];
        sum = Drop[FoldPairList[{Last[#1],Apply[FullAdder,Join[{First[#1]},#2]]}&,{borrow,0},pairs],1];
        bigEndianSum = Reverse[sum];
        Return[{BitNot[First[bigEndianSum]],Rest[bigEndianSum]}];
    ];


(* ::Subsection::Closed:: *)
(*Execution by opcode*)


(* ::Input::Initialization:: *)
aluMnemonics = {"Add","Substract","Increment","Decrement","Pass"};
aluOpcodes = Thread[aluMnemonics->Take[Tuples[{0,1},3],Length[aluMnemonics]]];
revAluOpcodes = Map[Reverse,aluOpcodes];
ALUOpCode[mnemonic_] :=
    ReplaceAll[mnemonic,aluOpcodes];


(* ::Input::Initialization:: *)
ALUZero[a_] :=
    BitNot[Apply[BitOr,a]];
ALUNotZero[a_] :=
    Apply[BitOr,a];
ALUParity[a_] :=
    Apply[BitXor,a];


(* ::Input::Initialization:: *)
ALUExecute[opcode_,a_,b_] :=
    Block[ {
    add,substract,
    increment,decrement,pass,output,positiveFlag,
    negativeFlag,parityFlag
    },
        add = ALUAdder[a,b];
        substract = ALUSubstractor[a,b];
        increment = ALUAdder[PadLeft[{1},Length[a]],a];
        decrement = ALUSubstractor[a,PadLeft[{1},Length[a]]];
        pass = a;
        output = Multiplexer3Bit[opcode,
        {
        {ALUOpCode["Add"],Last[add]},
        {ALUOpCode["Substract"],Last[substract]},
        {ALUOpCode["Increment"],Last[increment]},
        {ALUOpCode["Decrement"],Last[decrement]},
        {ALUOpCode["Pass"],pass}
        }
        ];
        positiveFlag = Multiplexer3Bit[opcode,
        {
        {ALUOpCode["Add"],1},
        {ALUOpCode["Substract"],BitNot[First[substract]]},
        {ALUOpCode["Increment"],1},
        {ALUOpCode["Decrement"],BitNot[First[decrement]]},
        {ALUOpCode["Pass"],0}
        }
        ];
        negativeFlag = Multiplexer3Bit[opcode,
        {
        {ALUOpCode["Add"],0},
        {ALUOpCode["Substract"],First[substract]},
        {ALUOpCode["Increment"],0},
        {ALUOpCode["Decrement"],First[decrement]},
        {ALUOpCode["Pass"],0}
        }
        ];
        Return[<|"Positive"->positiveFlag,"Negative"->negativeFlag,"Zero"->ALUZero[output],"NotZero"->ALUNotZero[output],"Parity"->ALUParity[output],"Result"->output|>];
    ];
ALUExecute[opcode_,a_] :=
    ALUExecute[opcode,a,ConstantArray[0,Length[a]]];

ALUClear[] :=
    Block[ {},
        WriteFlag[0,"ALUFlagPositive"];
        WriteFlag[0,"ALUFlagNegative"];
        WriteFlag[0,"ALUFlagZero"];
        WriteFlag[0,"ALUFlagNotZero"];
        WriteFlag[0,"ALUFlagParity"];
    ];


(* ::Section:: *)
(*Memory*)


(* ::Subsection::Closed:: *)
(*Base circuits*)


(* ::Input::Initialization:: *)
AndOrLatch[set_,reset_,tag_] :=
    MemoryEvaluate[BitAnd[BitOr[#,set],BitNot[reset]]&,tag];


(* ::Input::Initialization:: *)
GatedLatch[dataInput_,writeEnable_,tag_] :=
    AndOrLatch[BitAnd[dataInput,writeEnable],BitAnd[BitNot[dataInput],writeEnable],tag];


(* ::Input::Initialization:: *)
Flag[dataInput_,writeEnable_,tag_] :=
    GatedLatch[dataInput,writeEnable,tag];
ReadFlag[tag_] :=
    GatedLatch[0,0,tag];
WriteFlag[dataInput_,tag_] :=
    GatedLatch[dataInput,1,tag];


(* ::Subsection::Closed:: *)
(*Memory cell*)


(* ::Input::Initialization:: *)
Bit8AddressCell[dataInput_,writeEnable_,address__,tag_] :=
    GatedLatch[
    dataInput,
    writeEnable,
    StringJoin[
    tag,
    "bit_",
    ToString[
    MultiplexerNTo1[
    256,
    address,
    Range[256]
    ]
    ]
    ]
    ];


(* ::Input::Initialization:: *)
Bit8AddressBlock[dataInput__/;Length[dataInput]==8,writeEnable_,address__,tag_] :=
    Table[Bit8AddressCell[Part[dataInput,i],writeEnable,address,StringJoin[tag,"bit_",ToString[i]]],{i,8}];

ReadBit8AddressBlock[address__,tag_] :=
    Bit8AddressBlock[{0,0,0,0,0,0,0,0},0,address,tag];
WriteBit8AddressBlock[dataInput__/;Length[dataInput]==8,address__,tag_] :=
    Bit8AddressBlock[dataInput,1,address,tag];

ResetBit8AddressBlock[tag_,size_] :=
    Block[ {addresses},
        addresses = Map[PadLeft[IntegerDigits[#,2],8]&,Range[0,size]];
        Map[WriteBit8AddressBlock[{0,0,0,0,0,0,0,0},#1,tag]&,addresses];
    ];


(* ::Section:: *)
(*Registers*)


(* ::Subsection::Closed:: *)
(*3 and 8 bit registers*)


(* ::Input::Initialization:: *)
Bit3Register[dataInput__,writeEnable_,tag_] :=
    Table[
    GatedLatch[
    Part[dataInput,index],
    writeEnable,
    StringJoin[tag,"_bit3register",ToString[index]]
    ],
    {index,1,3}
    ];
ReadBit3Register[tag_] :=
    Bit3Register[{0,0,0},0,tag];
WriteBit3Register[dataInput__,tag_] :=
    Bit3Register[dataInput,1,tag];

Bit8Register[dataInput__,writeEnable_,tag_] :=
    Table[
    GatedLatch[
    Part[dataInput,index],
    writeEnable,
    StringJoin[tag,"_bit8register",ToString[index]]
    ],
    {index,1,8}
    ];
ReadBit8Register[tag_] :=
    Bit8Register[{0,0,0,0,0,0,0,0},0,tag];
WriteBit8Register[dataInput__,tag_] :=
    Bit8Register[dataInput,1,tag];


(* ::Section:: *)
(*CPU*)


(* ::Subsection::Closed:: *)
(*Control unit initialization*)


(* ::Input::Initialization:: *)
CUClear[] :=
    Block[ {},
        ResetBit8AddressBlock["Stack",8];
        WriteBit8Register[{0,0,0,0,0,0,0,0},"InstructionAddressRegister"];
        WriteBit8Register[{0,0,0,0,0,0,0,1},"ParameterAddressRegister"];
        WriteBit8Register[{0,0,0,0,0,0,0,0},"InstructionRegister"];
        WriteBit8Register[{0,0,0,0,0,0,0,0},"ParameterRegister"];
        WriteBit8Register[{0,0,0,0,0,0,0,0},"StackPointerRegister"];
        WriteBit8Register[{0,0,0,0,0,0,0,0},"RegisterA"];
        WriteBit8Register[{0,0,0,0,0,0,0,0},"RegisterB"];
        WriteFlag[0,"Ram Read-Enable"];
        WriteFlag[0,"Ram Write-Enable"];
        WriteFlag[0,"Stack Read-Enable"];
        WriteFlag[0,"Stack Write-Enable"];
        WriteFlag[0,"RegisterA Read-Enable"];
        WriteFlag[0,"RegisterB Read-Enable"];
        WriteFlag[0,"RegisterA Write-Enable"];
        WriteFlag[0,"RegisterB Write-Enable"];
        WriteFlag[0,"ALU Instruction"];
        WriteFlag[0,"ALU Opcode"];
        WriteFlag[0,"Print Enable"];
        WriteFlag[0,"Halt"];
        outputBuffer = {};
    ];


(* ::Subsection::Closed:: *)
(*Opcodes*)


(* ::Input::Initialization:: *)
cpuMnemonics = {
"LoadA","LoadB","SetA","SetB","Store","Add",
"Substract","Increment","Decrement","Jump","JumpPos","JumpNeg","JumpZero","JumpNotZero","JumpOdd",
"Call","Return","Print","Halt"
};
opcodes = Thread[cpuMnemonics->Take[Tuples[{0,1},8],Length[cpuMnemonics]]];
revOpcodes = Map[Reverse,opcodes];
OpCode[mnemonic_] :=
    ReplaceAll[mnemonic,opcodes];


(* ::Subsection::Closed:: *)
(*Fetch*)


(* ::Input::Initialization:: *)
CUFetch[] :=
    Block[ {instructionAddress,parameterAddress,instruction,parameter},
        instructionAddress = ReadBit8Register["InstructionAddressRegister"];
        parameterAddress = Last[ALUAdder[instructionAddress,{0,0,0,0,0,0,0,1}]];
        instruction = ReadBit8AddressBlock[instructionAddress,"RAM"];
        parameter = ReadBit8AddressBlock[parameterAddress,"RAM"];
        WriteBit8Register[instruction,"InstructionRegister"];
        WriteBit8Register[parameter,"ParameterRegister"];
        Return[<|"Instruction Address"->instructionAddress,"Parameter Address"->parameterAddress, "Instruction Register"->instruction, "Parameter Register"->parameter,"Instruction opcode"->ReplaceAll[instruction,revOpcodes]|>];
    ];


(* ::Subsection::Closed:: *)
(*Decode*)


(* ::Input::Initialization:: *)
CUDecode[] :=
    Block[ {
    opcode,ramReadEnable,ramWriteEnable,stackReadEnable,stackWriteEnable,registerAReadEnable,registerAWriteEnable,
    registerBWriteEnable,aluOpcode,aluInstruction, jumpEnable,jumpPosEnable, jumpNegEnable,
    jumpZeroEnable,jumpNotZeroEnable,printEnable,setFromParam
    },
        opcode = ReadBit8Register["InstructionRegister"];
        ramReadEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["LoadA"],1},
        {OpCode["LoadB"],1}
        }
        ];
        WriteFlag[ramReadEnable,"Ram Read-Enable"];
        ramWriteEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["Store"],1}
        }
        ];
        WriteFlag[ramWriteEnable,"Ram Write-Enable"];
        stackReadEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["Return"],1}
        }
        ];
        WriteFlag[stackReadEnable,"Stack Read-Enable"];
        stackWriteEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["Call"],1}
        }
        ];
        WriteFlag[stackWriteEnable,"Stack Write-Enable"];
        registerAReadEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["Store"],1},
        {OpCode["Print"],1}
        }
        ];
        WriteFlag[registerAReadEnable,"RegisterA Read-Enable"];
        registerAWriteEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["LoadA"],1},
        {OpCode["SetA"],1},
        {OpCode["Add"],1},
        {OpCode["Substract"],1},
        {OpCode["Increment"],1}
        }
        ];
        WriteFlag[registerAWriteEnable,"RegisterA Write-Enable"];
        registerBWriteEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["LoadB"],1},
        {OpCode["SetB"],1}
        }
        ];
        WriteFlag[registerBWriteEnable,"RegisterB Write-Enable"];
        jumpEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["Jump"],1}
        }
        ];
        WriteFlag[jumpEnable,"Jump"];
        jumpPosEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["JumpPos"],1}
        }
        ];
        WriteFlag[jumpPosEnable,"JumpPos"];
        jumpNegEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["JumpNeg"],1}
        }
        ];
        WriteFlag[jumpNegEnable,"JumpNeg"];
        jumpZeroEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["JumpZero"],1}
        }
        ];
        WriteFlag[jumpZeroEnable,"JumpZero"];
        jumpNotZeroEnable = Multiplexer8Bit[opcode,
        {
        {OpCode["JumpNotZero"],1}
        }
        ];
        WriteFlag[jumpNotZeroEnable,"JumpNotZero"];
        aluInstruction =  Multiplexer8Bit[opcode,
        {
        {OpCode["Add"],1},
        {OpCode["Substract"],1},
        {OpCode["Increment"],1},
        {OpCode["Decrement"],1}
        }
        ];
        WriteFlag[aluInstruction,"ALU Instruction"];
        aluOpcode =  Multiplexer8Bit[opcode,
        {
        {OpCode["Add"],ALUOpCode["Add"]},
        {OpCode["Substract"],ALUOpCode["Substract"]},
        {OpCode["Increment"],ALUOpCode["Increment"]},
        {OpCode["Decrement"],ALUOpCode["Decrement"]}
        }
        ];
        WriteBit3Register[aluOpcode,"ALU Opcode"];
        printEnable =  Multiplexer8Bit[opcode,
        {
        {OpCode["Print"],1}
        }
        ];
        WriteFlag[printEnable,"Print Enable"];
        setFromParam =  Multiplexer8Bit[opcode,
        {
        {OpCode["SetA"],1},
        {OpCode["SetB"],1}
        }
        ];
        WriteFlag[setFromParam,"Set From Parameter"];
        Return[
        <|
        "Ram Read-Enable"->ramReadEnable,"Ram Write-Enable"->ramWriteEnable,
        "Stack Read-Enable"->stackReadEnable,"Stack Write-Enable"->stackWriteEnable,
        "RegisterA Read-Enable"->registerAReadEnable,"RegisterA Write-Enable"->registerAWriteEnable,"RegisterB Write-Enable"->registerBWriteEnable,
        "Jump"->jumpEnable,"JumpPos"->jumpPosEnable,"JumpNeg"->jumpNegEnable,"JumpZero"->jumpZeroEnable,"JumpNotZero"->jumpNotZeroEnable,
        "ALU Instruction"->aluInstruction,"ALU Opcode"->aluOpcode,
        "Print Enable"->printEnable,"Set From Parameter"->setFromParam
        |>
        ];
    ];


(* ::Subsection::Closed:: *)
(*Execute*)


(* ::Input::Initialization:: *)
CUExecute[] :=
    Block[ {param,readFromRam,aluResult,nextInstructionAddress,nextStackPointer,stackLast,status = "Running"},
        param = ReadBit8Register["ParameterRegister"];
        readFromRam = ReadBit8AddressBlock[param,"RAM"];

        (* Print *)
        If[ ReadFlag["Print Enable"]==1,
            AppendTo[outputBuffer,BinaryToDecimal[readFromRam]]
        ];

        (* Set operations *)
        Bit8Register[param,BitAnd[ReadFlag["RegisterA Write-Enable"],ReadFlag["Set From Parameter"],BitNot[ReadFlag["ALU Instruction"]]],"RegisterA"];
        Bit8Register[param,BitAnd[ReadFlag["RegisterB Write-Enable"],ReadFlag["Set From Parameter"],BitNot[ReadFlag["ALU Instruction"]]],"RegisterB"];

        (* Load operations *)
        Bit8Register[readFromRam,BitAnd[ReadFlag["RegisterA Write-Enable"],BitNot[ReadFlag["Set From Parameter"]],BitNot[ReadFlag["ALU Instruction"]]],"RegisterA"];
        Bit8Register[readFromRam,BitAnd[ReadFlag["RegisterB Write-Enable"],BitNot[ReadFlag["Set From Parameter"]],BitNot[ReadFlag["ALU Instruction"]]],"RegisterB"];

        (* ALU operations *)
        aluResult = ALUExecute[ReadBit3Register["ALU Opcode"],ReadBit8Register["RegisterA"],ReadBit8Register["RegisterB"]];
        Bit8Register[aluResult["Result"],BitAnd[ReadFlag["RegisterA Write-Enable"],ReadFlag["ALU Instruction"]],"RegisterA"];
        Flag[aluResult["Positive"],ReadFlag["ALU Instruction"],"ALUFlagPositive"];
        Flag[aluResult["Negative"],ReadFlag["ALU Instruction"],"ALUFlagNegative"];
        Flag[aluResult["Zero"],ReadFlag["ALU Instruction"],"ALUFlagZero"];
        Flag[aluResult["NotZero"],ReadFlag["ALU Instruction"],"ALUFlagNotZero"];
        Flag[aluResult["Parity"],ReadFlag["ALU Instruction"],"ALUFlagParity"];

        (* Store operations *)
        Bit8AddressBlock[ReadBit8Register["RegisterA"],ReadFlag["Ram Write-Enable"],param,"RAM"];

        (* Instruction address operations *)
        Bit8Register[param,
        BitOr[
        ReadFlag["Jump"],
        BitAnd[ReadFlag["JumpPos"],ReadFlag["ALUFlagPositive"]],
        BitAnd[ReadFlag["JumpNeg"],ReadFlag["ALUFlagNegative"]],
        BitAnd[ReadFlag["JumpZero"],ReadFlag["ALUFlagZero"]],
        BitAnd[ReadFlag["JumpNotZero"],ReadFlag["ALUFlagNotZero"]],
        BitAnd[ReadFlag["JumpOdd"],ReadFlag["ALUFlagParity"]]
        ],
        "InstructionAddressRegister"
        ];
        nextInstructionAddress = Last[ALUAdder[ReadBit8Register["InstructionAddressRegister"],{0,0,0,0,0,0,1,0}]];
        Bit8Register[nextInstructionAddress,
        BitOr[
        BitAnd[
        BitNot[ReadFlag["Jump"]],
        BitNot[ReadFlag["JumpPos"]],
        BitNot[ReadFlag["JumpNeg"]],
        BitNot[ReadFlag["JumpZero"]],
        BitNot[ReadFlag["JumpNotZero"]],
        BitNot[ReadFlag["JumpOdd"]]
        ],
        BitAnd[ReadFlag["JumpPos"],BitNot[ReadFlag["ALUFlagPositive"]]],
        BitAnd[ReadFlag["JumpNeg"],BitNot[ReadFlag["ALUFlagNegative"]]],
        BitAnd[ReadFlag["JumpZero"],BitNot[ReadFlag["ALUFlagZero"]]],
        BitAnd[ReadFlag["JumpNotZero"],BitNot[ReadFlag["ALUFlagNotZero"]]],
        BitAnd[ReadFlag["JumpOdd"],BitNot[ReadFlag["ALUFlagParity"]]]
        ],
        "InstructionAddressRegister"
        ];

        (* Call operations on stack *)
        Bit8AddressBlock[param,ReadFlag["Stack Write-Enable"],ReadBit8Register["StackPointerRegister"],"Stack"];
        nextStackPointer = Last[ALUAdder[ReadBit8Register["StackPointerRegister"],{0,0,0,0,0,0,0,1}]];
        Bit8Register[nextStackPointer,ReadFlag["Stack Write-Enable"],"StackPointerRegister"];
        If[ BinaryToDecimal[ReadBit8Register["StackPointerRegister"]]>7,
            status = "Stack overflow";
            WriteFlag[1,"Halt"];
        ];
        Bit8Register[param,ReadFlag["Stack Write-Enable"],"InstructionAddressRegister"];

        (* Return operations on stack *)
        stackLast = ReadBit8AddressBlock[ReadBit8Register["StackPointerRegister"],"Stack"];
        Bit8Register[stackLast,ReadFlag["Stack Read-Enable"],"InstructionAddressRegister"];
        Bit8AddressBlock[{0,0,0,0,0,0,0,0},ReadFlag["Stack Read-Enable"],ReadBit8Register["StackPointerRegister"],"Stack"];
        nextStackPointer = ALUSubstractor[ReadBit8Register["StackPointerRegister"],{0,0,0,0,0,0,0,1}];
        Bit8Register[Last[nextStackPointer],BitAnd[BitNot[First[nextStackPointer]],ReadFlag["Stack Read-Enable"]],"StackPointerRegister"];
        If[ First[nextStackPointer] == 1 && ReadFlag["Stack Read-Enable"],
            status = "Halted";
            WriteFlag[1,"Halt"];
        ];
        <|
        "RegisterA"-> ReadBit8Register["RegisterA"],"RegisterB"->ReadBit8Register["RegisterB"],
        "InstructionAddressRegister"->ReadBit8Register["InstructionAddressRegister"],"Status"->status
        |>
    ];


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*Load program in memory*)


(* ::Input::Initialization:: *)
LoadProgram[program_] :=
    Module[ {x,addresses,programInRAM},
        addresses = Map[PadLeft[IntegerDigits[#,2],8]&,Range[0,255]];
        programInRAM = ReplaceAll[PadRight[program,256,x],x->{0,0,0,0,0,0,0,0}];
        MapThread[WriteBit8AddressBlock[#2,#1,"RAM"]&,{addresses,programInRAM}];
    ];
ClearProgram[] :=
    ResetBit8AddressBlock["RAM",255];


(* ::Subsection::Closed:: *)
(*CPU Visualizations*)


(* ::Input::Initialization:: *)
InstructionsPanel[machineInstructions_,from_] :=
    Block[ {cursor,instructions},
        cursor = BinaryToDecimal[ReadBit8Register["InstructionAddressRegister"]]-from+1;
        If[ Negative[cursor],
            cursor = 0
        ];
        instructions = Thread[{Range[from,from+Length[Flatten[machineInstructions]]-1],Flatten[machineInstructions]}];
        Grid[
        instructions,
        Background->{{LightGreen},{cursor->Yellow}}
        ]
    ];
InstructionsPlot[code_] :=
    Block[ {mi,tabs,currentAddress,currentTab},
        mi = Flatten[CPUMachineInstructions[CPUGetInstructions[code]]];
        tabs = MapThread[InstructionsPanel,{Partition[mi,UpTo[16]],Range[0,Length[mi]-1,16]}];
        currentTab = Ceiling[(BinaryToDecimal[ReadBit8Register["InstructionAddressRegister"]]+1)/16];
        Panel[TabView[Thread[Range[0,Length[mi]-1,16]->tabs],currentTab],Style["Program",Bold]]
    ];


(* ::Input::Initialization:: *)
ColumnJoin[l_] :=
    StringJoin[Riffle[DeleteCases[l,""],"\n"]];
OutputPlot[] :=
    Panel[
    InputField[
    ColumnJoin[Map[ToString,outputBuffer]],String,
    FieldSize->{10,{0,Infinity}},
    Enabled->False
    ],
    Style["Output",Bold]
    ];

StackPlot[] :=
    Block[ {addresses,stack,cursor,stackInDecimal,plt,stackPointerGrid,stackGrid},
        cursor = BinaryToDecimal[ReadBit8Register["StackPointerRegister"]]+2;
        addresses = Map[PadLeft[IntegerDigits[#,2],8]&,Range[0,7]];
        stack = Map[ReadBit8AddressBlock[#,"Stack"]&,addresses];
        stackInDecimal = Map[BinaryToDecimal,stack];
        plt = Join[{{"Stack level","Binary value","Decimal value"}},Thread[{Range[1,8],stack,stackInDecimal}]];
        stackPointerGrid = Grid[
        {
        {"Stack Pointer Register"},
        {Row[ReadBit8Register["StackPointerRegister"]]}
        },
        Frame->All,
        Background->{None,{LightBlue}}
        ];
        stackGrid = Grid[plt,Background->{None,{1->LightGreen,cursor->Yellow}},Frame->All];
        Panel[Column[{stackPointerGrid,stackGrid}],Style["Stack",Bold]]
    ];

RAMPanel[RAM_,base_] :=
    Block[ {currentAddress,cursor,ramInDecimal},
        currentAddress = BinaryToDecimal[ReadBit8Register["InstructionAddressRegister"]];
        cursor = currentAddress-base+2;
        If[ Negative[cursor],
            cursor = 0
        ];
        ramInDecimal = Map[BinaryToDecimal,RAM];
        Grid[Join[{{"Address","Binary value","Decimal value"}},Thread[{Range[base,base+15],RAM,ramInDecimal}]],Background->{None,{1->LightGreen,cursor->Yellow}},Frame->All]
    ];
RAMPlot[] :=
    Block[ {addresses,ram,tabs,currentTab},
        addresses = Map[PadLeft[IntegerDigits[#,2],8]&,Range[0,255]];
        ram = Map[ReadBit8AddressBlock[#,"RAM"]&,addresses];
        tabs = MapThread[RAMPanel,{Partition[ram,16],Range[0,255,16]}];
        currentTab = Ceiling[(BinaryToDecimal[ReadBit8Register["InstructionAddressRegister"]]+1)/16];
        Panel[TabView[Thread[Range[0,255,16]->tabs],currentTab],Style["RAM",Bold]]
    ];

ALUStatusPlot[] :=
    Panel[
    Grid[{
    {"Positive: ",ReadFlag["ALUFlagPositive"]},
    {"Negative: ",ReadFlag["ALUFlagNegative"]},
    {"Zero: ",ReadFlag["ALUFlagZero"]},
    {"NotZero: ",ReadFlag["ALUFlagNotZero"]},
    {"Parity: ",ReadFlag["ALUFlagParity"]}
    },
    Frame->All,
    Background->{{LightRed},None}
    ],
    Style["ALU Status",Bold]
    ];

CUStatusPlot[] :=
    Block[ {mainRegisters,instructionRegisters,flagsStatus},
        mainRegisters = Grid[
        {
        {"Register A","Register B"},
        {Row[ReadBit8Register["RegisterA"]],Row[ReadBit8Register["RegisterB"]]}
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
        {Row[ReadBit8Register["InstructionRegister"]],Row[ReadBit8Register["ParameterRegister"]]},
        {ReadBit8Register["InstructionRegister"]/.revOpcodes,BinaryToDecimal[ReadBit8Register["ParameterRegister"]]}
        },
        Frame->All,Background->{None,{LightCyan}}
        ],
        Row[ReadBit8Register["InstructionAddressRegister"]]
        }
        },
        Frame->All,
        Background->{None,{LightBlue}}
        ];
        flagsStatus = Grid[{
        {"Ram Read-Enable: ",ReadFlag["Ram Read-Enable"]},
        {"Ram Write-Enable: ",ReadFlag["Ram Write-Enable"]},
        {"Stack Read-Enable: ",ReadFlag["Stack Read-Enable"]},
        {"Stack Write-Enable: ",ReadFlag["Stack Write-Enable"]},
        {"RegisterA Read-Enable: ",ReadFlag["RegisterA Read-Enable"]},
        {"RegisterA Write-Enable: ",ReadFlag["RegisterA Write-Enable"]},
        {"RegisterB Write-Enable: ",ReadFlag["RegisterB Write-Enable"]},
        {"Jump: ",ReadFlag["Jump"]},
        {"JumpPos: ",ReadFlag["JumpPos"]},
        {"JumpNeg: ",ReadFlag["JumpNeg"]},
        {"JumpZero: ",ReadFlag["JumpZero"]},
        {"JumpNotZero: ",ReadFlag["JumpNotZero"]},
        {"ALU Instruction: ",ReadFlag["ALU Instruction"]},
        {"ALU Opcode: ",ReadBit3Register["ALU Opcode"]/.revAluOpcodes},
        {"Print Enable: ",ReadFlag["Print Enable"]},
        {"Halt: ",ReadFlag["Halt"]}
        },
        Frame->All,
        Background->{{LightRed},None}
        ];
        Panel[Column[{mainRegisters,instructionRegisters,flagsStatus}],Style["CU Status",Bold],ImageSize->250]
    ];

CPUPlot[] :=
    Block[ {cpuPanel,memoryAndOutput},
        cpuPanel = Panel[Grid[{{ALUStatusPlot[],Column[{CUStatusPlot[]}],StackPlot[]}},Alignment->Top],Style["CPU",Bold]];
        memoryAndOutput = Panel[Grid[{{RAMPlot[],OutputPlot[]}},Alignment->Top],Style["Memory and output",Bold]];
        Column[{cpuPanel,memoryAndOutput}]
    ];
CPUAndProgramPlot[code_] :=
    Block[ {cpuPanel,memoryAndOutput},
        cpuPanel = Panel[Grid[{{ALUStatusPlot[],Column[{CUStatusPlot[]}],StackPlot[]}},Alignment->Top],Style["CPU",Bold]];
        memoryAndOutput = Panel[Grid[{{InstructionsPlot[code],RAMPlot[],OutputPlot[]}},Alignment->Top],Style["Memory and output",Bold]];
        Column[{cpuPanel,memoryAndOutput}]
    ];
ExecuteCodeInteractive[code_,cycles_] :=
    DynamicModule[{result = ExecuteCode[code,cycles]},
    Manipulate[Part[result,i],{i,1,Length[result],1}]
    ];

ExecuteCodeInteractive[code_] :=
    DynamicModule[{result = ExecuteCode[code]},
    Manipulate[Part[result,i],{i,1,Length[result],1}]
    ];


(* ::Input::Initialization:: *)
ALUResultModel[] :=
    Block[ {param,readFromRam,aluResult,a1,a2,a3,a4,a5,a6,a7,a8,b1,b2,b3,b4,b5,b6,b7,b8,m1,m2,m3,m4,m5,m6,m7,m8},
        WriteBit8Register[{a1,a2,a3,a4,a5,a6,a7,a8},"InstructionRegister"];
        CUDecode[];
        WriteBit8Register[{b1,b2,b3,b4,b5,b6,b7,b8},"ParameterRegister"];
        WriteBit8AddressBlock[{m1,m2,m3,m4,m5,m6,m7,m8},{b1,b2,b3,b4,b5,b6,b7,b8},"RAM"];
        param = ReadBit8Register["ParameterRegister"];
        readFromRam = ReadBit8AddressBlock[param,"RAM"];
        Bit8Register[param,BitAnd[ReadFlag["RegisterA Write-Enable"],ReadFlag["Set From Parameter"],BitNot[ReadFlag["ALU Instruction"]]],"RegisterA"];
        Bit8Register[param,BitAnd[ReadFlag["RegisterB Write-Enable"],ReadFlag["Set From Parameter"],BitNot[ReadFlag["ALU Instruction"]]],"RegisterB"];
        Bit8Register[readFromRam,BitAnd[ReadFlag["RegisterA Write-Enable"],BitNot[ReadFlag["Set From Parameter"]],BitNot[ReadFlag["ALU Instruction"]]],"RegisterA"];
        Bit8Register[readFromRam,BitAnd[ReadFlag["RegisterB Write-Enable"],BitNot[ReadFlag["Set From Parameter"]],BitNot[ReadFlag["ALU Instruction"]]],"RegisterB"];
        aluResult = ALUExecute[ReadBit3Register["ALU Opcode"],ReadBit8Register["RegisterA"],ReadBit8Register["RegisterB"]];
        Return[aluResult["Result"]];
    ];

RegisterBModel[] :=
    Block[ {param,readFromRam,registerB,a1,a2,a3,a4,a5,a6,a7,a8,b1,b2,b3,b4,b5,b6,b7,b8,m1,m2,m3,m4,m5,m6,m7,m8},
        WriteBit8Register[{a1,a2,a3,a4,a5,a6,a7,a8},"InstructionRegister"];
        CUDecode[];
        WriteBit8Register[{b1,b2,b3,b4,b5,b6,b7,b8},"ParameterRegister"];
        WriteBit8AddressBlock[{m1,m2,m3,m4,m5,m6,m7,m8},{b1,b2,b3,b4,b5,b6,b7,b8},"RAM"];
        param = ReadBit8Register["ParameterRegister"];
        readFromRam = ReadBit8AddressBlock[param,"RAM"];
        Bit8Register[param,BitAnd[ReadFlag["RegisterB Write-Enable"],ReadFlag["Set From Parameter"],BitNot[ReadFlag["ALU Instruction"]]],"RegisterB"];
        registerB = Bit8Register[readFromRam,BitAnd[ReadFlag["RegisterB Write-Enable"],BitNot[ReadFlag["Set From Parameter"]],BitNot[ReadFlag["ALU Instruction"]]],"RegisterB"];
        Return[registerB];
    ];

CPUModelPlot[model_] :=
    Block[ {instruction,parameter,readFromRam,a1,a2,a3,a4,a5,a6,a7,a8,b1,b2,b3,b4,b5,b6,b7,b8,m1,m2,m3,m4,m5,m6,m7,m8},
        instruction = Thread[{a1,a2,a3,a4,a5,a6,a7,a8}->ReadBit8Register["InstructionRegister"]];
        parameter = Thread[{b1,b2,b3,b4,b5,b6,b7,b8}->ReadBit8Register["ParameterRegister"]];
        readFromRam = Thread[{m1,m2,m3,m4,m5,m6,m7,m8}->ReadBit8AddressBlock[parameter,"RAM"]];
        PlotExpression[model,Join[instruction,parameter,readFromRam]]
    ];


(* ::Input::Initialization:: *)
ExecuteModel[program_,cycles_] :=
    Block[ {steps,iter = 0},
        ClearProgram[];
        ALUClear[];
        CUClear[];
        LoadProgram[program];
        steps = Reap[
        CUFetch[];
        CUDecode[];
        Sow[CPUModelPlot[model]];
        While[ReadFlag["Halt"] != 1 && iter < cycles,
        CUExecute[];
        CUFetch[];
        CUDecode[];
        Sow[CPUModelPlot[model]];
        iter++;
        ]
        ][[2,1]];
        Return[steps];
    ];


(* ::Subsection::Closed:: *)
(*Program execution*)


(* ::Input::Initialization:: *)
CPUCycle[] :=
    Block[ {},
        CUFetch[];
        CUDecode[];
        If[ ReadFlag["Halt"] != 1,
            CUExecute[]
        ];
    ];
ExecuteProgram[program_] :=
    Block[ {steps},
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

ExecuteProgram[program_,cycles_] :=
    Block[ {steps,iter = 0},
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

ExecuteCode[code_,cycles_] :=
    Block[ {program,steps,iter = 0},
        program = CPUAssemblyCompile[code];
        ClearProgram[];
        ALUClear[];
        CUClear[];
        LoadProgram[program];
        steps = Reap[
        CUFetch[];
        CUDecode[];
        Sow[CPUAndProgramPlot[code]];
        While[ReadFlag["Halt"] != 1 && iter < cycles,
        CUExecute[];
        CUFetch[];
        CUDecode[];
        Sow[CPUAndProgramPlot[code]];
        iter++;
        ]
        ][[2,1]];
        Return[steps];
    ];


(* ::Chapter:: *)
(*End of package*)


End[ ]

EndPackage[ ]
