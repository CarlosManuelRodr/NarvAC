(* ::Package:: *)

(* ::Title:: *)
(*PL/0 Compiler*)


(* ::Text:: *)
(*This parser uses a finite automaton to recognize regular expressions which are also used by the lexical analyzer (lexer) to define the tokens to recognize. The input specified by the user is converted into a list of tokens and then is passed to the parser which builds a parse tree based on a given grammar. The grammar also has associated actions that are instructions that describe how to synthesize the tree into an intermediate code (IC) representation.*)


(* ::Chapter:: *)
(*Begin package*)


BeginPackage["PL0Compiler`"]


(* ::Chapter:: *)
(*Package description*)


Protect[EmptyString, Term, NonTerm];


DFA::usage ="\!\(\*
StyleBox[\"DFA\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"transitions\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"start\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"accept\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"stateExpr\",\nFontSlant->\"Italic\"]\)] create an DFA from a transition list.";
DFATransitions::usage ="\!\(\*
StyleBox[\"DFATransitions\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"transitions\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"state\",\nFontSlant->\"Italic\"]\)]";
DFAIterate::usage ="\!\(\*
StyleBox[\"DFAIterate\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"transitions\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"state\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"inputSymbol\",\nFontSlant->\"Italic\"]\)] get the next step in the computation of the DFA for a inputSymbol.";
DFACompute::usage ="\!\(\*
StyleBox[\"DFACompute\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"dfa\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"inputString\",\nFontSlant->\"Italic\"]\)] compute if input is recognized by \!\(\*
StyleBox[\"dfa\",\nFontSlant->\"Italic\"]\).";
DFAExecutionPlot::usage ="\!\(\*
StyleBox[\"DFAExecutionPlot\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"dfa\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"inputString\",\nFontSlant->\"Italic\"]\)] return a dynamic execution plot of the computation performed by \!\(\*
StyleBox[\"dfa\",\nFontSlant->\"Italic\"]\).";

FiniteAutomataPlot::usage ="\!\(\*
StyleBox[\"FiniteAutomataPlot\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"fa\",\nFontSlant->\"Italic\"]\)] return a plot of the finite automata represented by \!\(\*
StyleBox[\"fa\",\nFontSlant->\"Italic\"]\).";

NFA::usage ="\!\(\*
StyleBox[\"NFA\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"transitions\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"start\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"accept\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"stateExpr\",\nFontSlant->\"Italic\"]\)] create an NFA from a transition list.";
NFAIterate::usage ="\!\(\*
StyleBox[\"NFAIterate\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"transitions\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"state\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"inputSymbol\",\nFontSlant->\"Italic\"]\)] get the next step in the computation of the NFA for a \!\(\*
StyleBox[\"inputSymbol\",\nFontSlant->\"Italic\"]\).";
NFACompute::usage ="\!\(\*
StyleBox[\"NFACompute\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"inputString\",\nFontSlant->\"Italic\"]\)] compute if \!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\) is recognized by n\!\(\*
StyleBox[\"fa\",\nFontSlant->\"Italic\"]\).";
NFASimplify::usage ="\!\(\*
StyleBox[\"NFASimplify\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\)] tries to simplify the structure of \!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\) by removing unnecesary nodes.";
NFAUnion::usage ="\!\(\*
StyleBox[\"NFAUnion\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"nfa1\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"nfa2\",\nFontSlant->\"Italic\"]\)] perform the regular operation union.";
NFAConcatention::usage ="\!\(\*
StyleBox[\"NFAConcatention\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"nfa1\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"nfa2\",\nFontSlant->\"Italic\"]\)] perform the regular operation concatenation.";
NFAStar::usage ="\!\(\*
StyleBox[\"NFAStar\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"fa\",\nFontSlant->\"Italic\"]\)] perform the regular operation kleene star.";
NFAToDFA::usage ="\!\(\*
StyleBox[\"NFAToDFA\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\)] convert a NFA to a DFA by traversing every posible computation and transforming group of NFA states into a single DFA state.";
NFAExecutionTreeComputeTrace::usage ="\!\(\*
StyleBox[\"NFAExecutionTreeComputeTrace\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"inputString\",\nFontSlant->\"Italic\"]\)] return the trace of the computation performed by \!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\).";
NFAExecutionTree::usage ="\!\(\*
StyleBox[\"NFAExecutionTree\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"inputString\",\nFontSlant->\"Italic\"]\)] return the execution tree of the computation performed by \!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\).";
NFAExecutionPlot::usage ="\!\(\*
StyleBox[\"NFAExecutionPlot\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"inputString\",\nFontSlant->\"Italic\"]\)] return a dynamic execution plot of the computation performed by \!\(\*
StyleBox[\"nfa\",\nFontSlant->\"Italic\"]\).";

Regex::usage ="\!\(\*
StyleBox[\"Regex\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"c\",\nFontSlant->\"Italic\"]\)] creates a recognizer for the character \!\(\*
StyleBox[\"c\",\nFontSlant->\"Italic\"]\).";
RegexUnion::usage ="\!\(\*
StyleBox[\"RegexUnion\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"args\",\nFontSlant->\"Italic\"]\)] Perform the regular operation union. Can be applied to an arbitrary number of finite automate type arguments \!\(\*
StyleBox[\"args\",\nFontSlant->\"Italic\"]\).";
RegexConcatenation::usage ="\!\(\*
StyleBox[\"RegexConcatenation\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"args\",\nFontSlant->\"Italic\"]\)] Perform the regular operation concatenation. Can be applied to an arbitrary number of finite automate type arguments \!\(\*
StyleBox[\"args\",\nFontSlant->\"Italic\"]\).";
RegexStar::usage ="\!\(\*
StyleBox[\"RegexStar\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"fa\",\nFontSlant->\"Italic\"]\)] perform the regular operation kleene star.";
RegexDagger::usage ="\!\(\*
StyleBox[\"RegexDagger\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"str\",\nFontSlant->\"Italic\"]\)]perform the regular operation dagger.";
RegexAlphabet::usage ="\!\(\*
StyleBox[\"RegexAlphabet\",\nFontWeight->\"Bold\"]\)[]  A...Za...z recognizer.";
RegexAlphabetLowercase::usage ="\!\(\*
StyleBox[\"RegexAlphabetLowercase\",\nFontWeight->\"Bold\"]\)[] lowercase alphabet wrapper.";
RegexAlphabetUppercase::usage ="\!\(\*
StyleBox[\"RegexAlphabetUppercase\",\nFontWeight->\"Bold\"]\)[] uppercase alphabet wrapper.";
RegexDigits::usage ="\!\(\*
StyleBox[\"RegexDigits\",\nFontWeight->\"Bold\"]\)[] 0...9 recognizer.";
RegexASCIIChars::usage ="\!\(\*
StyleBox[\"RegexASCIIChars\",\nFontWeight->\"Bold\"]\)[] recognizes every char in the ASCII standard.";
RegexCompute::usage ="\!\(\*
StyleBox[\"RegexCompute\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"fa\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\)] compute if \!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\) is recognized by \!\(\*
StyleBox[\"fa\",\nFontSlant->\"Italic\"]\), and if applicable, return the recognized token.";

Token::usage ="\!\(\*
StyleBox[\"Token\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"symbol\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\)] is a token constructor.";
CreateTokens::usage ="\!\(\*
StyleBox[\"CreateTokens\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] creates a token list from a list that contains {symbol, name} pairs.";
CreateTokenRecognizer::usage ="\!\(\*
StyleBox[\"CreateTokenRecognizer\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"tokenList\",\nFontSlant->\"Italic\"]\)] creates a recognizer (implemented as  a regular expression and using a finite automata as a backend) of every token in \!\(\*
StyleBox[\"tokenList\",\nFontSlant->\"Italic\"]\). ";
GetToken::usage ="\!\(\*
StyleBox[\"GetToken\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"symbolTokens\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"keywordTokens\",\nFontSlant->\"Italic\"]\)] for a \!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\) string tries to match a symbol token or a keyword token. If no match is 
found it returns $Failed. Keyword tokens have precedence over symbol tokens.";
GetNextToken::usage ="\!\(\*
StyleBox[\"GetNextToken\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"program\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"symbolTokens\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"keywordTokens\",\nFontSlant->\"Italic\"]\)] for a input string \!\(\*
StyleBox[\"program\",\nFontSlant->\"Italic\"]\), tries to match the biggest token from the left of the string. ";
Tokenize::usage ="\!\(\*
StyleBox[\"Tokenize\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"program\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"symbolTokens\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"keywordTokens\",\nFontSlant->\"Italic\"]\)] transform the input string \!\(\*
StyleBox[\"program\",\nFontSlant->\"Italic\"]\) into a list of tokens by using the recognizers \!\(\*
StyleBox[\"symbolTokens\",\nFontSlant->\"Italic\"]\) and \!\(\*
StyleBox[\"keywordTokens\",\nFontSlant->\"Italic\"]\).";
Lint::usage ="\!\(\*
StyleBox[\"Lint\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"program\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"symbolTokens\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"keywordTokens\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"colorRules\",\nFontSlant->\"Italic\"]\)] applies styling transfomations for each token.";

GrammarPrettify::usage ="\!\(\*
StyleBox[\"GrammarPrettify\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\)] creates a textual representation for \!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\).";
GrammarGraph::usage ="\!\(\*
StyleBox[\"GrammarGraph\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\)] creates a graph for \!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\).";
MatchTerminalsWithRule::usage ="\!\(\*
StyleBox[\"MatchTerminalsWithRule\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"stateId\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"lastId\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"rule\",\nFontSlant->\"Italic\"]\)] try to match the \!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\) with a production \!\(\*
StyleBox[\"rule\",\nFontSlant->\"Italic\"]\) from the grammar. 
\!\(\*
StyleBox[\"stateId\",\nFontSlant->\"Italic\"]\) and \!\(\*
StyleBox[\"lastId\",\nFontSlant->\"Italic\"]\) are labels to avoid ambiguity in the parse tree.";
MatchTerminals::usage ="\!\(\*
StyleBox[\"MatchTerminals\",\nFontWeight->\"Bold\"]\)[{\!\(\*
StyleBox[\"state\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"stateId\",\nFontSlant->\"Italic\"]\)}, \!\(\*
StyleBox[\"lastId\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\)] tries to match the \!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\) with every production rule in \!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\). This 
is used for the parse process.";
VerifyGrammar::usage ="\!\(\*
StyleBox[\"VerifyGrammar\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\)] verifies the correctness of \!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\).";
VerifyInput::usage ="\!\(\*
StyleBox[\"VerifyInput\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"tokenList\",\nFontSlant->\"Italic\"]\)] verifies the correctness of an input to be parsed in the form of a \!\(\*
StyleBox[\"tokenList\",\nFontSlant->\"Italic\"]\).";
Term::usage = "Label for terminal symbols.";
NonTerm::usage "Label for non-terminal symbols.";
Parse::usage ="\!\(\*
StyleBox[\"Parse\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\)] parses input in the form of a token list and returns a parse tree and semantic actions associated with every NonTerminal of the parse tree.";
ParseTree::usage ="\!\(\*
StyleBox[\"ParseTree\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"input\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"grammar\",\nFontSlant->\"Italic\"]\)] returns a graphical representation of the parse tree generated by \!\(\*
StyleBox[\"Parse\",\nFontWeight->\"Bold\"]\).";

GetAction::usage ="\!\(\*
StyleBox[\"GetAction\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"treeSymbol\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"parseTree\",\nFontSlant->\"Italic\"]\)] returns the semantic action corresponding to the parseTree.";
SynthesizeNonTerm::usage ="\!\(\*
StyleBox[\"SynthesizeNonTerm\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"treeSymbol\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"parseTree\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"synthesizations\",\nFontSlant->\"Italic\"]\)] creates the synthesization for the nonterminal \!\(\*
StyleBox[\"treeSymbol\",\nFontSlant->\"Italic\"]\).";
SynthesizeTerm::usage ="\!\(\*
StyleBox[\"SynthesizeTerm\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"treeSymbol\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"synthesizations\",\nFontSlant->\"Italic\"]\)] creates the synthesization for the terminal \!\(\*
StyleBox[\"treeSymbol\",\nFontSlant->\"Italic\"]\).";
SynthesizeTree::usage ="\!\(\*
StyleBox[\"SynthesizeTree\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"parseTree\",\nFontSlant->\"Italic\"]\)] synthesizes the \!\(\*
StyleBox[\"parseTree\",\nFontSlant->\"Italic\"]\) by using the semantic actions in the grammar.";
Synthesis::usage = "";
ViewSynthesisProcess::usage = "";

ProcedureProcessLabels::usage ="\!\(\*
StyleBox[\"ProcedureProcessLabels\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"ic\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"from\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"to\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"globalVar\",\nFontSlant->\"Italic\"]\)] processes the procedure enclosed in [\!\(\*
StyleBox[\"from\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\) \!\(\*
StyleBox[\"to\",\nFontSlant->\"Italic\"]\)] 
changing the labels to reflect that symbols in the interval belong to the corresponding procedure. Labels in \!\(\*
StyleBox[\"globalVars\",\nFontSlant->\"Italic\"]\) are left unprocessed.";
ICProcessLabels::usage ="\!\(\*
StyleBox[\"ICProcessLabels\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"ic\",\nFontSlant->\"Italic\"]\)] processes all the labels in \!\(\*
StyleBox[\"ic\",\nFontSlant->\"Italic\"]\).";
PL0ParseAndSynthesize::usage ="\!\(\*
StyleBox[\"PL0ParseAndSynthesize\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"code\",\nFontSlant->\"Italic\"]\)] returns ic without processing the labels.";
PL0CompileToIC::usage ="\!\(\*
StyleBox[\"PL0CompileToIC\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"code\",\nFontSlant->\"Italic\"]\)] returns ic with processed labels.";
PlotIC::usage ="\!\(\*
StyleBox[\"PlotIC\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"ic\",\nFontSlant->\"Italic\"]\)] graphical representation of IC.";

CompileICToASM::usage ="\!\(\*
StyleBox[\"CompileICToASM\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"ic\",\nFontSlant->\"Italic\"]\)] compiles intermediate code into assembler.";
CompilePL0ToASM::usage ="\!\(\*
StyleBox[\"CompilePL0ToASM\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"code\",\nFontSlant->\"Italic\"]\)] compiles PL/0 code into assembler.";

GetInstructions::usage ="\!\(\*
StyleBox[\"GetInstructions\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"asmcode\",\nFontSlant->\"Italic\"]\)] formats assembly code into a nested list.";
GetPosition::usage ="\!\(\*
StyleBox[\"GetPosition\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"instructions\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"token\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"]\",\nFontSlant->\"Italic\"]\) calculates the final position of a given token considering that Labels occupy 0 bits in the 
machine code, Declare 1 byte, and any other instruction 2 bytes.";
ProcessTags::usage ="\!\(\*
StyleBox[\"ProcessTags\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"instructions\",\nFontSlant->\"Italic\"]\)] generates the substitution rule for converting labels to memory positions.";
MachineInstructions::usage ="\!\(\*
StyleBox[\"MachineInstructions\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"instructions\",\nFontSlant->\"Italic\"]\)] returns the exact instructions that will be assembled.";
AssemblyCompile::usage ="\!\(\*
StyleBox[\"AssemblyCompile\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"asmcode\",\nFontSlant->\"Italic\"]\)] compiles ASM to machine code.";
PL0Compile::usage ="\!\(\*
StyleBox[\"PL0Compile\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"code\",\nFontSlant->\"Italic\"]\)] compiles PL/0 code to machine code.";


(* ::Chapter:: *)
(*Error messages*)


GetNextToken::badInput = "There are input symbols that cannot be matched with any token.";
Parse::incompleteGrammar = "Actions in this grammar are not correctly specified";
Parse::badGrammar = "Incorrect grammar";
Parse::badInput = "Incorrect input";


(* ::Chapter:: *)
(*Begin package*)


Begin["`Private`"]


(* ::Subchapter::Closed:: *)
(*Finite state machine*)


(* ::Subsection::Closed:: *)
(*Deterministic finite automaton (DFA)*)


(* ::Text:: *)
(*In the theory of computation, a branch of theoretical computer science, a deterministic finite automaton (DFA)\[LongDash]also known as deterministic finite acceptor (DFA), deterministic finite state machine (DFSM), or deterministic finite state automaton (DFSA)\[LongDash]is a finite-state machine that accepts or rejects strings of symbols and only produces a unique computation (or run) of the automaton for each input string.*)
(**)
(*DFAs recognize exactly the set of regular languages, which are, among other things, useful for doing lexical analysis and pattern matching. DFAs can be built from nondeterministic finite automata (NFAs) using the powerset construction method. This is useful because usually NFA are slower to compute.*)


(* ::Input::Initialization:: *)
Protect[\[Epsilon]]; (* \[Epsilon] will be used as a symbol to denote the empty string *)
Transition[parent_, child_, inputSymbol_] := <|"Parent"->parent,"Node"->child,"InputSymbol"->inputSymbol|>;
EmptyTransition[parent_, child_] := Transition[parent,child,\[Epsilon]];
FATransitions[transitions_, state_] := Cases[transitions,KeyValuePattern[{"Parent"->state}]];
Concatenate[l_] := Apply[Join,l];


(* ::Input::Initialization:: *)
NameTag[machine_Association, state_] := Subscript[machine["Name"], ToString[state]];

Options[FiniteAutomataPlot]={"Legended"->True,"Labeled"->False};
FiniteAutomataPlot[fa_Association,OptionsPattern[]] := Block[{graphData,startTag,acceptTags,legend,graph},
    Switch[OptionValue["Labeled"], 
        True,
        graphData = Cases[fa["Transitions"], KeyValuePattern[{"Parent"->p_, "Node"->n_, "InputSymbol"->i_}] :> Labeled[p->n,i]];
        startTag = fa["StartState"]->Red;
        acceptTags = Map[If[# != fa["StartState"], #->Green,#->Purple]&,fa["AcceptStates"]];
        ,
        False,
        graphData = Cases[fa["Transitions"], KeyValuePattern[{"Parent"->p_, "Node"->n_, "InputSymbol"->i_}] :> p->n];
        startTag = fa["StartState"]->Red;
        acceptTags = Map[If[# != fa["StartState"], #->Green,#->Purple]&,fa["AcceptStates"]];
    ];

    legend = PointLegend[{LightBlue,Red,Green,Purple},{"State","Start state","Accept state","Start/Accept state"},LegendMarkers->Graphics[Disk[]]];

    graph = Graph[
        graphData,
        ImageSize->400,
        VertexLabels->"Name",
        VertexStyle->Join[{startTag},acceptTags], 
        VertexSize->0.1
    ];

    If[OptionValue["Legended"], 
        Return[Legended[graph,legend]], 
        Return[graph]
    ]
];


(* ::Input::Initialization:: *)
(* DFA object constructor *)
DFA[name_, transitions_, start_, accept_, stateExpr_: {}] := 
<|
    "Name"->name,(* Descriptive name to keep in track the regular operations applied to it *)
    "Type"->"DFA",
    "Transitions"->transitions,
    "StartState"->start,
    "AcceptStates"->accept,
    "StateExpressions"->stateExpr (* Each state may have an associated expression *)
|>;

(* Return every deterministic transition leading to state *)
DFATransitions[transitions_, state_] := Cases[transitions, KeyValuePattern[{"Parent"->state}]];

(* Get the next state *)
Options[DFAIterate]={"Trace"->False};
DFAIterate[transitions_, state_, inputSymbol_, OptionsPattern[]] := Block[{next},
    next = Cases[
        DFATransitions[transitions,state], 
        KeyValuePattern[{"InputSymbol"->i_ /; MemberQ[i, inputSymbol]}]
    ];

    (* A DFA must have only one available transition *)
    If[Length[next]==1,
        If[OptionValue["Trace"], 
            Return[First[next]]
            ,
            Return[First[next]["Node"]];
        ]
        ,
        (* TODO: Crear mecanismo de alerta *)
        Return[$Failed];
    ]
];

(* Returns the trace of the computation and the result of whether the machine accepts the inputString *)
DFACompute[dfa_Association,inputString_] := Block[{computation,result},
    computation = FoldList[
        DFAIterate[dfa["Transitions"], #1["Node"], #2, "Trace"->True]&,
        <|"Node"->dfa["StartState"]|>,
        inputString
    ];
    result = MemberQ[dfa["AcceptStates"], Last[computation]["Node"]];
    Return[{computation, result}]
];


(* ::Text:: *)
(*Plotting functions*)


(* ::Input::Initialization:: *)
Options[DFAExecutionPlot] = {"SimpleNodes"->False};
DFAExecutionPlot[dfa_Association, inputString_, OptionsPattern[]] := DynamicModule[{trace, executionSteps, ruleIndexes, graphData, startTag, acceptTags},
    trace = DFACompute[dfa, inputString];
    executionSteps = Drop[First[trace], 1];
    ruleIndexes = Map[Position[dfa["Transitions"], #]&, executionSteps];

    If[OptionValue["SimpleNodes"], 
        graphData = Cases[dfa["Transitions"], KeyValuePattern[{"Parent"->p_, "Node"->n_, "InputSymbol"->i_}] :> Labeled[p->n,i]];
        startTag = dfa["StartState"]->Red;
        acceptTags = Map[#->Green&,dfa["AcceptStates"]];
        ,
        graphData = Cases[dfa["Transitions"], KeyValuePattern[{"Parent"->p_, "Node"->n_, "InputSymbol"->i_}] :> Labeled[NameTag[dfa,p] -> NameTag[dfa,n], i]];
        startTag = NameTag[dfa, dfa["StartState"]] -> Red;
        acceptTags = Map[NameTag[dfa, #]->Green&, dfa["AcceptStates"]];
    ];

    Manipulate[
        Column[
            {
                Row[{"Input string: ",Grid[{inputString}, Frame->All, Background->{i->Green}]}], 
                Row[{"Result: ",If[Last[trace], "Accepted", "Not accepted"]}], 
                Graph[
                MapAt[Style[#, Red]&, graphData, ruleIndexes[[i]]], 
                ImageSize->400,
                VertexLabels->"Name",
                VertexStyle->Join[{startTag}, acceptTags], 
                VertexSize->0.1
                ]
            }
        ]
        ,
        {i,1,Length[ruleIndexes], 1}
    ]
];


(* ::Subsection::Closed:: *)
(*Non-deterministic Finite Automata (NFA)*)


(* ::Text:: *)
(*In automata theory, a finite state machine is called a deterministic finite automaton (DFA), if*)
(**)
(*each of its transitions is uniquely determined by its source state and input symbol, and*)
(*reading an input symbol is required for each state transition.*)
(*A nondeterministic finite automaton (NFA), or nondeterministic finite state machine, does not need to obey these restrictions. In particular, every DFA is also an NFA. Sometimes the term NFA is used in a narrower sense, referring to a NFA that is not a DFA, but not in this article.*)
(**)
(*Using the subset construction algorithm, each NFA can be translated to an equivalent DFA, i.e. a DFA recognizing the same formal language. Like DFAs, NFAs only recognize regular languages.*)


(* ::Input::Initialization:: *)
NFA[name_, transitions_, start_, accept_, stateExpr_: {}] := 
<|
    "Name"->name,
    "Type"->"NFA",
    "Transitions"->transitions,
    "StartState"->start,
    "AcceptStates"->accept ,
    "StateExpressions"->stateExpr
|>;

ContainsQ[list_List,form_] := MemberQ[list, form];
ContainsQ[form1_, form2_] := (form1===form2);

PureNondetStateQ[transitions_, state_] := (DeleteDuplicates[FATransitions[transitions, state][[All, "InputSymbol"]]] === {\[Epsilon]});

(* Return the list of nodes accesible via empty transitions *)
NFANondetNodes[transitions_, state_Association] := Cases[FATransitions[transitions, state["Node"]], KeyValuePattern[{"InputSymbol"->i_ /; ContainsQ[i,\[Epsilon]]}]];
NFANondetNodes[transitions_, state_Integer] := Cases[FATransitions[transitions,state], KeyValuePattern[{"InputSymbol"->i_/;ContainsQ[i,\[Epsilon]]}]];
NFANondetNodes[transitions_, state_List] := DeleteDuplicates[Flatten[Map[NFANondetNodes[transitions,#]&,state]]];
NFANondetNodesRecursive[transitions_, states_]  :=  FixedPoint[DeleteDuplicates[Join[#, NFANondetNodes[transitions, #[[All, "Node"]]]]]&, states];

Options[NFAIterate]={"Trace"->False};
NFAIterate[transitions_, state_?AtomQ, inputSymbol_, OptionsPattern[]] := Block[{next, deterministicTransitions, forkTransitions, deterministicNodes, emptyTransition},
    (* Get all the transitions corresponding to a DFA *)
    deterministicTransitions = Cases[FATransitions[transitions,state], KeyValuePattern[{"InputSymbol"->i_ /; MemberQ[i, inputSymbol]}]];
    deterministicNodes = Sort[DeleteDuplicates[deterministicTransitions[[All, "Node"]]]];

    (* Run through the empty transitions of the DFA nodes *)
    forkTransitions = NFANondetNodes[transitions, deterministicNodes]; (* Get past the current deterministic nodes *)
    forkTransitions = NFANondetNodesRecursive[transitions, forkTransitions]; (* Append every valid nondet transition *)

    (* The next nodes will be a union between the deterministic transitions and the nodes reachable from nondet transitions *)
    next = DeleteDuplicates[Join[deterministicTransitions, forkTransitions]];

    If[Length[next]>0,
        If[OptionValue["Trace"], 
            Return[next], 
            Return[next[[All, "Node"]]]
        ]
        ,
        Return[{}]
    ];
];
NFAIterate[transitions_, state_List,inputSymbol_, opt:OptionsPattern[]] := DeleteDuplicates[Flatten[Map[NFAIterate[transitions, #, inputSymbol, opt]&, state]]];

NFACompute[nfa_Association,inputString_] := Block[{computation, result, start},
    start = NFANondetNodesRecursive[
        nfa["Transitions"], 
        {<|"Node"->nfa["StartState"]|>}
    ];
    computation = FoldList[
        NFAIterate[nfa["Transitions"], #1[[All,"Node"]], #2,"Trace"->True]&,
        start,
        inputString
    ];
    result = Apply[Or, Map[MemberQ[nfa["AcceptStates"], #]&, Last[computation][[All, "Node"]]]];
    Return[{computation, result}];
];


(* ::Input::Initialization:: *)
TransitionApplyThreshold[transition_, threshold_] := MapAt[#+threshold&, transition, {{Key["Parent"]}, {Key["Node"]}}];
FAApplyThreshold[fa_Association, threshold_] := <|
    "Name" -> fa["Name"], 
    "Transitions" -> Map[TransitionApplyThreshold[#, threshold]&, fa["Transitions"]], 
    "StartState" -> fa["StartState"] + threshold,
    "AcceptStates" -> fa["AcceptStates"] + threshold,
    "StateExpressions" -> If[Length[fa["StateExpressions"]] != 0, MapAt[# + threshold&, fa["StateExpressions"], {All, 1}], {}]
|>;


(* ::Input::Initialization:: *)
GetDirectSuccesors[fa_, state_] := Select[
Nest[NFANondetNodes[fa["Transitions"], #]&, {state}, 2], 
    (!ContainsQ[fa["AcceptStates"], #["Parent"]] && PureNondetStateQ[fa["Transitions"], #["Parent"]] && Length[FAParents[fa["Transitions"], #["Parent"]]] == 1)
    &
];

ReplaceKey[assoc_, key_, replaceTo_] := MapAt[replaceTo&, assoc, Key[key]];

DeleteIntermediateTransition[transitions_, startState_, endState_] := Block[{deleteState, replacement, newTransitions, cleanedUp},
    deleteState = endState["Parent"];
    replacement = ReplaceKey[endState, "Parent", startState["Node"]];
    cleanedUp = DeleteCases[transitions, KeyValuePattern["Parent" -> deleteState]];
    cleanedUp = DeleteCases[cleanedUp, KeyValuePattern["Node" -> deleteState]];

    newTransitions = Append[cleanedUp, replacement];
    Return[newTransitions];
];

SimplifyStateIteration[nfa_, state_] := Block[{newFA,newTransitions},
    newFA = nfa;
    newTransitions = Fold[
        DeleteIntermediateTransition[#1, <|"Node"->state|>, #2]&,
        nfa["Transitions"], 
        GetDirectSuccesors[nfa, state]
    ];
    newFA["Transitions"] = newTransitions;
    Return[newFA];
];
SimplifyState[nfa_, state_] := FixedPoint[SimplifyStateIteration[#, state]&, nfa];

NFASimplify[nfa_] := Block[{simplified, oldStates, newStates, stateRelabelRule = {}},
    simplified = Fold[SimplifyState, nfa, GetStates[nfa["Transitions"]]];

    oldStates = GetStates[simplified["Transitions"]];
    newStates = Range[0, Length[oldStates]-1];
    stateRelabelRule = Thread[oldStates -> newStates];

    simplified["Transitions"] = DeleteDuplicates[MapAt[Replace[#, stateRelabelRule]&, simplified["Transitions"], {{All, Key["Parent"]}, {All, Key["Node"]}}]];
    simplified["StartState"] = Replace[simplified["StartState"], stateRelabelRule];
    simplified["AcceptStates"] = ReplaceAll[simplified["AcceptStates"], stateRelabelRule];
    If[Length[simplified["StateExpressions"]] != 0,
        simplified["StateExpressions"] = MapAt[Replace[#, stateRelabelRule]&, simplified["StateExpressions"], {All, 1}];
    ];
    Return[simplified];
];


(* ::Input::Initialization:: *)
NFAUnion[nfa1_Association,nfa2_Association] := Block[{minIndex, newIndexThreshold, newM1, newM2, newAccept, newTransitions, newFA, newStateExpressions},
    minIndex = Min[nfa1["Transitions"][[All, "Parent"]]];
    newIndexThreshold = Max[nfa1["Transitions"][[All, "Node"]]] + 1;
    newM1 = FAApplyThreshold[nfa1, 1];
    newM2 = FAApplyThreshold[nfa2, newIndexThreshold + 1];
    newAccept = Sort[Join[newM1["AcceptStates"], newM2["AcceptStates"]]];
    newTransitions = {EmptyTransition[minIndex, newM1["StartState"]],  EmptyTransition[minIndex, newM2["StartState"]]};
    newStateExpressions = SortBy[Join[newM1["StateExpressions"], newM2["StateExpressions"]], First];

    newFA = <|
        "Name"->StringJoin[nfa1["Name"], "\[Union]",nfa2["Name"]], 
        "Type"->"NFA",
        "Transitions"->Join[newM1["Transitions"], newM2["Transitions"], newTransitions], 
        "StartState"->minIndex,
        "AcceptStates"->newAccept,
        "StateExpressions"->newStateExpressions
    |>;
    Return[newFA];
];
NFAUnion[nfas__ /; Length[List[nfas]] > 2] := Fold[NFAUnion, First[List[nfas]], Rest[List[nfas]]];


(* ::Input::Initialization:: *)
NFAConcatention[nfa1_Association,nfa2_Association] := Block[{newIndexThreshold, newM2, newTransitions, newFA, newStateExpressions},
    newIndexThreshold = Max[nfa1["Transitions"][[All, "Node"]]] + 1;
    newM2 = FAApplyThreshold[nfa2, newIndexThreshold];

    newTransitions = Map[EmptyTransition[#, newM2["StartState"]]&, nfa1["AcceptStates"]];

    newStateExpressions = Join[nfa1["StateExpressions"], newM2["StateExpressions"]];
    newStateExpressions = If[Length[newStateExpressions] != 0, SortBy[Join[nfa1["StateExpressions"], newM2["StateExpressions"]], First], {}];

    newFA = <|
        "Name"->StringJoin[nfa1["Name"], nfa2["Name"]], 
        "Type"->"NFA",
        "Transitions"->Join[nfa1["Transitions"], newM2["Transitions"], newTransitions], 
        "StartState"->nfa1["StartState"], 
        "AcceptStates"->newM2["AcceptStates"], 
        "StateExpressions"->newStateExpressions
    |>;
    Return[newFA];
];
NFAConcatention[fas__/;Length[List[fas]]>2] := Fold[NFAConcatention,First[List[fas]], Rest[List[fas]]];


(* ::Input::Initialization:: *)
NFAStar[nfa_Association] := Block[{minIndex,newM,newTransitions,newAccept,newFA},
    minIndex = Min[nfa["Transitions"][[All, "Parent"]]];
    newM = FAApplyThreshold[nfa, 1];
    newTransitions = Join[{EmptyTransition[minIndex, newM["StartState"]]}, Map[EmptyTransition[#, newM["StartState"]]&, newM["AcceptStates"]]];
    newAccept = Append[newM["AcceptStates"], minIndex];

    newFA = <|
        "Name"->StringJoin["(",nfa["Name"], "\!\(\*SuperscriptBox[\()\), \(*\)]\)"], 
        "Type"->"NFA",
        "Transitions"->Join[newM["Transitions"], newTransitions], 
        "StartState"->minIndex,
        "AcceptStates"->newAccept,
        "StateExpressions"->newM["StateExpressions"]
    |>;
    Return[newFA];
];


(* ::Input::Initialization:: *)
(* Check if there is at least one of the accepted states in searchState *)
ContainsStateQ[stateList_, searchState_?AtomQ] := MemberQ[stateList, searchState];
ContainsStateQ[stateList_, searchState_List] := Apply[Or,Map[MemberQ[stateList, #]&, searchState]];

(* Get parents of the current state *)
FAParents[transitions_, state_] := DeleteDuplicates[Cases[transitions, KeyValuePattern[{"Node" -> state}]][[All, "Parent"]]];

(* Check if the state is inaccesible *)
JunkStateQ[transitions_, start_, state_] := Block[{stateParents, nonSelfTransitions},
    If[ContainsStateQ[start, state], Return[False]];
    stateParents = FAParents[transitions, state];
    nonSelfTransitions = Complement[stateParents, {state}];

    If[Length[nonSelfTransitions]==0,
        Return[True], 
        Return[False]
    ]
];

SafeSort[l_List] := Sort[l];
SafeSort[l_?AtomQ] := l;

(* Infer the alphabet from the transition list *)
GetAlphabet[transitions_] := DeleteCases[DeleteDuplicates[Flatten[transitions[[All, "InputSymbol"]]]], \[Epsilon]];

(* Infer the states from the transition list *)
GetStates[transitions_] := Sort[DeleteDuplicates[Flatten[Cases[transitions, KeyValuePattern[{"Parent"->p_, "Node"->n_}] :> {p, n}]]]];

(* Get the states reachable from the current states list *)
Explore[transitions_, states_] := Map[
    Transition[
    SafeSort[states], 
    SafeSort[DeleteDuplicates[NFAIterate[transitions,states,#, "Trace"->False]]], 
    {#}]&,
    GetAlphabet[transitions]
];

(* Explore one step down each branch of the computation and append it to the explored branches list *)
StepDown[transitions_, branches_] := DeleteDuplicates[
    Flatten[
        Join[
            branches,
            Map[
                Explore[
                    transitions,
                    #["Node"]
                ]&,
                branches
            ]
        ], 
        1
    ]
];

NewStateNode[node_, newStateRules_] := 
<|
    "Parent"->Replace[node["Parent"], newStateRules], 
    "Node"->Replace[node["Node"], newStateRules], 
    "InputSymbol"->node["InputSymbol"]
|>;

NFAToDFA[nfa_Association] := Block[{protoDFA, start, protoDFAStates, newStateRules, newStates, containsAccept, newFA, newAccept, newStart, newTransitions, newTransitionsCleanedUp,protoDFAExpressionNodes, newStateExpressionsNodes},
    If[nfa["Type"] =!= "NFA",Return[$Failed]];

    (* Explore all paths until there is no unexplored transition *)
    start = FixedPoint[
        DeleteDuplicates[
            Join[
            #, 
            NFANondetNodes[nfa["Transitions"], #[[All,"Node"]]]]
        ]&,
        {<|"Node"->nfa["StartState"]|>}
    ];

    (* Make a DFA by exploring all possible paths in the NFA *)
    protoDFA = FixedPoint[StepDown[nfa["Transitions"], #]&, {<|"Node"->start[[All, "Node"]]|>}];
    protoDFAStates = DeleteDuplicates[Join[Drop[protoDFA, 1][[All, "Parent"]], Drop[protoDFA, 1][[All, "Node"]]]];

    (* Match old NFA parameters to the new DFA *)
    newStateRules = Thread[protoDFAStates -> Range[Length[protoDFAStates]]];
    newStates =newStateRules[[All, 2]];
    containsAccept = Position[protoDFAStates, s_ /; ContainsStateQ[s, nfa["AcceptStates"]]];
    newAccept = Extract[newStates, containsAccept];
    newStart = Replace[First[protoDFAStates], newStateRules];
    newTransitions = Map[NewStateNode[#, newStateRules]&, Drop[protoDFA, 1]];
    newTransitionsCleanedUp = DeleteDuplicates[DeleteCases[newTransitions, t_ /; JunkStateQ[newTransitions, newStart, t["Node"]]]];

    protoDFAExpressionNodes = Concatenate[
        Map[
            Thread[Cases[protoDFAStates, s_ /; ContainsStateQ[s, First[#]]] -> Last[#]]&,
            nfa["StateExpressions"]
        ]
    ];
    If[Length[protoDFAExpressionNodes] != 0,
        newStateExpressionsNodes = MapAt[Replace[#, newStateRules]&, protoDFAExpressionNodes, {All, 1}]
        ,
        newStateExpressionsNodes = {}
    ];

    newFA = <|
        "Name"->nfa["Name"], 
        "Type"->"DFA",
        "Transitions"->newTransitionsCleanedUp,
        "StartState"->newStart,
        "AcceptStates"->newAccept,
        "StateExpressions"->newStateExpressionsNodes
    |>;
    Return[newFA];
];


(* ::Text:: *)
(*Plotting functions*)


(* ::Input::Initialization:: *)
(* Pretty output for NFAExecutionTree *)
GenerationTransform[node_Association, generation_] := {
    StringJoin["G: ",ToString[generation-1], " S: ",ToString[node["Parent"]]]->StringJoin["G: ",ToString[generation], " S: ",ToString[node["Node"]]]
    ,
    ToString[node["InputSymbol"]]
};
GenerationTransform[nodes_List, generation_] := Map[GenerationTransform[#, generation]&, nodes];

NFAReplaceParents[transitions_, newParent_] := Map[MapAt[newParent&, #, Key["Parent"]]&, transitions];

NFAExecutionTreeIterate[transitions_, state_?AtomQ, inputSymbol_] := Block[{next, deterministicTransitions, forkTransitions, deterministicNodes, emptyTransition},
    deterministicTransitions = Cases[FATransitions[transitions, state], KeyValuePattern[{"InputSymbol"->i_ /; MemberQ[i, inputSymbol]}]];
    deterministicNodes = Sort[DeleteDuplicates[deterministicTransitions[[All, "Node"]]]];

    forkTransitions = NFANondetNodes[transitions, deterministicNodes];
    forkTransitions = NFANondetNodesRecursive[transitions, forkTransitions];
    forkTransitions = NFAReplaceParents[forkTransitions, state]; (* This is the only difference from NFAIterate *)

    next = DeleteDuplicates[Join[deterministicTransitions, forkTransitions]];

    If[Length[next]>0, Return[next], Return[{}]];
];
NFAExecutionTreeIterate[transitions_, state_List, inputSymbol_] := DeleteDuplicates[Flatten[Map[NFAExecutionTreeIterate[transitions, #, inputSymbol]&, state]]];

NFAExecutionTreeComputeTrace[nfa_Association,inputString_] := Block[{computation, result, start},
    start = NFANondetNodesRecursive[nfa["Transitions"], {<|"Node"->nfa["StartState"]|>}]; (* This is the only difference from NFACompute *)
    computation = FoldList[NFAExecutionTreeIterate[nfa["Transitions"], #1[[All,"Node"]], #2]&, start, inputString];
    result = Apply[Or, Map[MemberQ[nfa["AcceptStates"], #]&, Last[computation][[All, "Node"]]]];
    Return[{computation, result}];
];

(* Plot the execution tree showing the active states at every input *)
NFAExecutionTree[nfa_Association, inputString_] := Block[{trace, root, traceTree},
    trace = Drop[First[NFAExecutionTreeComputeTrace[nfa, inputString]], 1];
    root = First[GenerationTransform[First[trace], 1]];
    traceTree = Flatten[MapIndexed[GenerationTransform[#1, First[#2]]&, trace], 1];

    TreePlot[
        DeleteDuplicates[traceTree], 
        Automatic,StringJoin["G: 0 S: ", ToString[nfa["StartState"]]], 

        VertexLabeling->True,
        DirectedEdges->True
    ]
];

Options[NFAExecutionPlot] = {"SimpleNodes"->False};
NFAExecutionPlot[nfa_Association, inputString_, OptionsPattern[]] := DynamicModule[{trace, executionSteps, ruleIndexes, graphData, startTag, acceptTags, currentStates, start, parentStates},
    trace = NFACompute[nfa, inputString];
    executionSteps = First[trace];
    ruleIndexes = Map[Position[nfa["Transitions"], Apply[Alternatives,#]]&, executionSteps];
    currentStates = Cases[nfa["Transitions"], KeyValuePattern[{"Node"->n_}] :> n];
    parentStates = Cases[nfa["Transitions"], KeyValuePattern[{"Parent"->p_}] :> p];
    start = DeleteDuplicates[Sort[Join[{<|"Node"->nfa["StartState"]|>}, NFANondetNodes[nfa["Transitions"], nfa["StartState"]]]]];

    If[OptionValue["SimpleNodes"], 
        graphData = Cases[nfa["Transitions"], KeyValuePattern[{"Parent"->p_, "Node"->n_, "InputSymbol"->i_}] :> Labeled[p->n, i]];
        acceptTags = Map[#->Green&, nfa["AcceptStates"]];
        startTag = nfa["StartState"]->Red;
        ,
        graphData = Cases[nfa["Transitions"], KeyValuePattern[{"Parent"->p_, "Node"->n_, "InputSymbol"->i_}] :> Labeled[NameTag[nfa, p]->NameTag[nfa, n], i]];
        acceptTags = Map[NameTag[nfa,#]->Green&, nfa["AcceptStates"]];
        startTag = NameTag[nfa, nfa["StartState"]]->Red;
    ];

    Manipulate[
        Column[
            {
                Row[{"Input string: ",Grid[{inputString}, Frame->All, Background->{(i-1)->Green}]}], 
                Row[{"Start states: ",Sort[start[[All,"Node"]]]}], 
                Row[{"Parent states: ",Sort[Extract[parentStates, ruleIndexes[[i]]]]}], 
                Row[{"Current states: ",Sort[Extract[currentStates, ruleIndexes[[i]]]]}], 
                Row[{"Accept states: ",nfa["AcceptStates"]}], 
                Row[{"Result: ",If[Last[trace], "Accepted","Not accepted"]}], 
                Graph[
                MapAt[Style[#, Red]&,graphData, ruleIndexes[[i]]], 
                ImageSize->400,
                VertexLabels->"Name",
                VertexStyle->Join[{startTag}, acceptTags], 
                VertexSize->0.1
                ]
            }
        ]
        ,
        {i, 1, Length[ruleIndexes], 1}
    ]
];


(* ::Subchapter::Closed:: *)
(*Regular expressions*)


(* ::Text:: *)
(*A regular expression, regex or regexp  is a sequence of characters that define a search pattern. Usually this pattern is used by string searching algorithms for "find" or "find and replace" operations on strings, or for input validation. It is a technique that developed in theoretical computer science and formal language theory.*)
(**)
(*This regex implementation is just a front-end to the Finite Automata operations.*)


(* ::Input::Initialization:: *)
RegexUnion[args__] := NFAUnion[args];
RegexConcatenation[args__] := NFAConcatention[args];
RegexStar[fa_Association] := NFAStar[fa];
RegexStar[str_String] := NFAStar[Regex[str]];
RegexDagger[fa_Association] := NFAConcatention[fa,NFAStar[fa]];
RegexDagger[str_String] := NFAConcatention[Regex[str], NFAStar[Regex[str]]];


(* ::Input::Initialization:: *)
Regex[c_String/;StringLength[c]==1] := NFA[c,{Transition[0,1,{c}]},0,{1}];
Regex[c_String/;StringLength[c]==1,stateExpr_] := NFA[c,{Transition[0,1,{c}]},0,{1},{1->stateExpr}];
Regex[str_String/;StringLength[str]>1, tokenRecognize_: ""] := Block[{m},
    m = Apply[NFAConcatention, Map[Regex, Characters[str]]];
    If[tokenRecognize != "",
        m["StateExpressions"] = Thread[m["AcceptStates"]->tokenRecognize];
    ];

    Return[m];
];
Regex[r_Association, tokenRecognize_] := Block[{m = r},
    m["StateExpressions"] = Thread[m["AcceptStates"]->tokenRecognize];
    Return[m];
];


(* ::Input::Initialization:: *)
RegexAlphabet[alphabet_] := Block[{input, name, transitions, acceptStates, newFA},
    input = Characters[alphabet];
    newFA = <|
        "Name"->StringJoin[Riffle[input, "\[Union]"]], 
        "Type"->"NFA",
        "Transitions"->Map[Transition[0, #, {input[[#]]}]&, Range[Length[input]]], 
        "StartState"->0,
        "AcceptStates"->Range[Length[input]], 
        "StateExpressions"->{}
    |>;
    Return[newFA];
];

RegexAlphabetLowercase[] := RegexAlphabet[StringJoin@Alphabet[]];
RegexAlphabetUppercase[] := RegexAlphabet[StringJoin@ToUpperCase[Alphabet[]]];
RegexAlphabet[] := RegexAlphabet[StringJoin@Join[Alphabet[], ToUpperCase[Alphabet[]]]];
RegexDigits[] := RegexAlphabet[StringJoin@Map[ToString, Range[0, 9]]];

asciiChars = {"\t","\n","\f","\r"," ","!","#","$","%","&","'","(",")","*","+",",","-",".","/","0","1","2","3","4","5","6","7","8","9",":",";","<","=",">","?","@","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","[","\\","]","^","_","`","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","{","|","}","~","\.7f"};

RegexASCIIChars[] := RegexAlphabet[StringJoin@asciiChars];


(* ::Input::Initialization:: *)
RegexCompute[fa_, input_] := Block[{trace, result, recognizedTokens},
    If[fa["Type"]== "NFA",
        {trace,result} = NFACompute[fa, Characters[input]];
        recognizedTokens = 
        Cases[
            Last[trace][[All,"Node"]], 
            (node_ /; ContainsQ[fa["StateExpressions"][[All,1]], node]) :>  Replace[node, fa["StateExpressions"]]
        ];

        ,
        {trace,result} = DFACompute[fa, Characters[input]];
        recognizedTokens = 
        Cases[
            {Last[trace]["Node"]},
            (node_ /; ContainsQ[fa["StateExpressions"][[All,1]], node]) :>  Replace[node, fa["StateExpressions"]]
        ];
    ];

    Return[{trace, result, recognizedTokens}];
];


(* ::Subchapter::Closed:: *)
(*Lexer*)


(* ::Text:: *)
(*In computer science, lexical analysis, lexing or tokenization is the process of converting a sequence of characters (such as in a computer program or web page) into a sequence of tokens (strings with an assigned and thus identified meaning). A program that performs lexical analysis may be termed a lexer, tokenizer, or scanner, though scanner is also a term for the first stage of a lexer.*)


(* ::Input::Initialization:: *)
(* 
   There are various types of tokens. Symbol tokens can be any simple expression like
  {"/", "Slash"},
   {"+", "Plus"},
   {"-", "Minus"},
   ...
  That cannot be confused with an identifier. It is necessary to make this distintion since keyword tokens like
  {"const", "Const"},
   {"var", "Var"},
   {"procedure", "Procedure"},
   {"call", "Call"},
   ...
  have the same form of identifiers, so the lexer has to give precedence to the keyword token. 
 *)
Token[symbol_String, name_String] := <|"Symbol"->symbol, "Name"->name|>;
CreateTokens[list_] := Map[Token[#[[1]], #[[2]]]&, list];
CreateTokenRecognizer[tokenList_] := NFAToDFA[Apply[RegexUnion, Map[Regex[#["Symbol"], #["Name"]]&, tokenList]]];


(* ::Input::Initialization:: *)
GetToken[input_, symbolTokens_, keywordTokens_] := Block[{symbolAccept, symbolResult, keywordAccept, keywordResult},
    {symbolAccept,symbolResult} = Rest[RegexCompute[symbolTokens, input]];
    {keywordAccept,keywordResult} = Quiet[Rest[RegexCompute[keywordTokens, input]]];
    If[keywordAccept, Return[First[keywordResult]]]; (* Keywords have precedence over symbols *)
    If[symbolAccept, Return[First[symbolResult]]];
    Return[$Failed];
];

SafeStringTake[string_, n_] := StringTake[string, Min[n, StringLength[string]]];
SafeStringTake[string_, {m_, n_}] := StringTake[string, {m, Min[n, StringLength[string]]}];

GetNextToken[program_, symbolTokens_, keywordTokens_] := Block[{tokenizerResult, nextToken},
    (* Try to get the first biggest token from the input *)
    tokenizerResult = NestWhileList[
        {
            First[#]+1,
            GetToken[SafeStringTake[program,First[#]+1], symbolTokens,keywordTokens]
        }&,
        {
            0,
            None
        },
        ((Last[#] =!= $Failed) && (First[#] <= StringLength[program]))&
    ];

    (* If no tokens were matched, abort the compilation *)
    If[tokenizerResult == {{0,None},{1,$Failed}},
        Message[GetNextToken::badInput];
        Abort[]
    ];

    (* Select the last before GetToken failed *)
    nextToken = If[Length[tokenizerResult]>2, tokenizerResult[[-2]], Last[tokenizerResult]];
    Return[nextToken];
];

Options[Tokenize] = {"IncludeBlank"->False};
Tokenize[program_, symbolTokens_, keywordTokens_, OptionsPattern[]] := Block[
    {
        length,type,
        cursor = 0,
        tokenList = {},
        omitTokens ,
        formattedTokens
    },

    If[OptionValue["IncludeBlank"], 
        omitTokens = {},
        omitTokens = {"Whitespace","LineBreak"}
    ];

    (* Iterate over all the input getting the tokens one by one *)
    While[cursor < StringLength[program], 
        {length, type} = GetNextToken[StringDrop[program, cursor], symbolTokens, keywordTokens];
        If[!MemberQ[omitTokens, type], AppendTo[tokenList, <|"Type"->type, "Value"->SafeStringTake[program, {cursor, cursor+length-1}+1]|>]];
        cursor+=length;
    ];

    (* Tokens without value only carry type information *)
    formattedTokens = Map[
        If[#["Type"] =!= #["Value"], 
            <|
            "Type"->#["Type"], 
            "Value"->#["Value"]
            |>,
            <|
            "Type"->#["Type"]
            |>
        ]
        &,
        tokenList
    ];

    Return[formattedTokens];
];

Lint[program_, symbolTokens_, keywordTokens_, colorRules_] := Block[{tokens, highlighted, linted},
    tokens = Tokenize[program, symbolTokens, keywordTokens, "IncludeBlank"->True];
    highlighted = Map[Style[#["Value"], #["Type"] /. colorRules]&, tokens];
    Return[Row[highlighted]];
];


(* ::Subchapter::Closed:: *)
(*Parser*)


(* ::Text:: *)
(*Parsing, syntax analysis, or syntactic analysis is the process of analysing a string of symbols, either in natural language, computer languages or data structures, conforming to the rules of a formal grammar. A parser is a software component that takes input data (frequently text) and builds a data structure \[Dash] often some kind of parse tree, abstract syntax tree or other hierarchical structure, giving a structural representation of the input while checking for correct syntax.*)
(**)
(*In this project a recursive descent parser is used.  A recursive descent parser is a kind of top-down parser built from a set of mutually recursive procedures (or a non-recursive equivalent) where each such procedure implements one of the nonterminals of the grammar. Thus the structure of the resulting program closely mirrors that of the grammar it recognizes.*)


(* ::Input::Initialization:: *)
GrammarPrettifyRule[rule_] := Style[rule["From"], Bold]->ReplaceAll[rule["To"], {NonTerm[arg_] :> Style[arg,Bold], Term[arg_] :> Style[arg,Italic]}];
GrammarPrettify[grammar_] := Column[Map[GrammarPrettifyRule,grammar]];

GrammarGraph[grammar_] := Block[{graphData, allSymbols, colorStyle},
    graphData = DeleteDuplicates[Flatten[Map[Thread, Map[(NonTerm[#["From"]] -> #["To"])&, grammar]]]];
    allSymbols = DeleteDuplicates[Flatten[Map[{NonTerm[#["From"]], #["To"]}&, grammar]]];
    colorStyle = Join[Thread[Cases[allSymbols, _NonTerm] -> Blue], Thread[Cases[allSymbols, _Term] -> Red]];

    Graph[graphData, VertexLabels->"Name", VertexStyle->colorStyle]
];


(* ::Input::Initialization:: *)
GetArgument[_[arg_]] := arg;
Substitute[l1_, l2_] := Join[l1,Drop[l2,Length[l1]]];

TermMatchQ[input_, {}] := True;
TermMatchQ[input_, terms_] := (Take[input, Length[terms]][[All, "Type"]] == Map[GetArgument, terms]);
GenerateTransitions[rule_, stateId_, lastId_] := MapIndexed[{rule["From"], stateId} -> {#1, lastId + First[#2]}&, rule["To"]];

MatchTerminalsWithRule[stateId_, lastId_, input_, rule_] := Block[{terms, transition, rest, cuttedInput, currentState, lastIndex, matchTransitions},
    currentState = rule["From"];
    If[rule["To"]===EmptyString[], Return[{{{currentState,stateId}->{EmptyString[], lastId+1}},{},input,lastId+1}]];

    (* 
    GenerateTransitions creates a list of every transition that can be obtained from a single rule. For example:
    GenerateTransition[<|"From"\[Rule]a,"To"\[Rule]{b,c,d}|>, 1, 4] will return
    {{a,1}\[Rule]{b,5},{a,1}\[Rule]{c,6},{a,1}\[Rule]{d,7}}
    *)
    transition = GenerateTransitions[rule, stateId, lastId];
    lastIndex = Last@Last@Last@transition;
    terms = TakeWhile[rule["To"], Head[#] =!= NonTerm&]; (* Consume every symbol until it encounters a NonTerminal *)
    rest = Drop[transition[[All,2]], Length[terms]];

    If[Length[terms] > Length[input], Return[$Failed]];

    If[TermMatchQ[input, terms],  (* If consumed terms coincide with input *)
        matchTransitions = MapIndexed[
            If[KeyExistsQ[#1, "Value"], 
                {rule["From"], stateId}->{Term[#1["Type"]], lastId+First[#2], #1["Value"]},
                {rule["From"], stateId}->{Term[#1["Type"]], lastId+First[#2]}
            ]&,
            Take[input, Length[terms]]
        ];
        transition = Substitute[matchTransitions, transition];
        cuttedInput = Drop[input, Length[terms]];
        Return[{transition, rest, cuttedInput, lastIndex}];
        ,
        Return[$Failed];
    ];
];

MapUntil[f_, expr_, test_] := Drop[
    NestWhileList[
        {f[#[[2, 1]]], Drop[#[[2]], 1], (!test[f[#[[2, 1]]]] && Length[#[[2]]] != 1)}&,
        {0, expr, True},
        #[[3]]&
    ][[All, 1]]
    , 
    1
];

(* Try to match the input with every rule in the grammar *)
MatchTerminals[{state_, stateId_}, lastId_, input_, grammar_] := Block[{possibleTransitions, matched},
    possibleTransitions = Cases[grammar, KeyValuePattern[{"From" -> state}]];
    matched = Last[MapUntil[{#, MatchTerminalsWithRule[stateId, lastId, input, #]}&, possibleTransitions, Last[#] =!= $Failed&]];
    Return[matched];
];


(* ::Input::Initialization:: *)
Protect[$IncompleteGrammar];
VerifyGrammar[grammar_] := Block[{grammarKeys},
    grammarKeys = DeleteDuplicates[Map[Keys, grammar]];
    If[grammarKeys == {{"From", "To", "Action"}}, Return[True]];
    If[MemberQ[grammarKeys, {"From", "To"}],  Return[$IncompleteGrammar]];
    Return[False];
];
VerifyInput[tokenList_] := Block[{inputKeys},
    (* Input must be a list of tokens with Type and Value entries. *)
    inputKeys = DeleteDuplicates[Map[Keys, tokenList]];
    If[inputKeys == {{"Type","Value"}}, 
        Return[True], 
        Return[False]
    ];
];

Options[Parse]={"Trace"->False};
Parse::incompleteGrammar = "Actions in this grammar are not correctly specified";
Parse::badGrammar = "Incorrect grammar";
Parse::badInput = "Incorrect input";
Parse[input_, grammar_, OptionsPattern[]] := Block[{grammarCheck, pGrammar, trace, symbolRules, rule, stack, transitions, next, inputBuffer, lastId, parseTree, parseSymbol, toStack},
    (* Error check *)
    grammarCheck = VerifyGrammar[grammar];
    If[grammarCheck == False,
        Message[Parse::badGrammar];
        Abort[];
    ];
    If[grammarCheck === $IncompleteGrammar, Message[Parse::incompleteGrammar]];
    If[VerifyInput[input] == False,
        Message[Parse::badInput];
        Abort[];
    ];

    (* Initialization *)
    pGrammar = MapAt[NonTerm, grammar, {All, Key["From"]}];
    stack = {};
    parseTree = {};
    lastId = 1;
    inputBuffer = input;
    parseSymbol = {First[pGrammar]["From"], 1};
    symbolRules = {};
    trace = {<|"ParseSymbol"->parseSymbol, "Stack"->stack, "LastID"->lastId, "InputBuffer"->inputBuffer|>};

    (* Loop until there are no input symbols left and no symbol left to parse *)
    While[{inputBuffer,parseSymbol} =!= {{}, None},
        If[Head[First[parseSymbol]] === NonTerm,
            (* Case where the parse symbol is a non terminal *)
            {rule, {transitions, next, inputBuffer, lastId} } = MatchTerminals[parseSymbol, lastId, inputBuffer, pGrammar];
            AppendTo[parseTree, transitions];
            AppendTo[symbolRules, {parseSymbol, rule["Action"]}];
            ,
            (* Case where the parse symbol is a terminal *)
            If[GetArgument[First[parseSymbol]] == First[inputBuffer]["Type"], 
            inputBuffer = Drop[inputBuffer,1];
            next = {};
            ];
        ];

        If[Length[next] > 0,
            (* If there are symbols left to process, process the first and push the rest to the stack *)
            {parseSymbol, toStack} = {First[next], Reverse[Rest[next]]};
            stack = Join[stack, toStack];
            ,
            (* If there are no symbols left, pop one from the stack *)
            If[Length[stack] > 0,
            {parseSymbol, stack} = {Last[stack], Drop[stack, -1]};
            ,
            (* Flag to stop the loop when the stack is empty *)
            parseSymbol = None;
            ];
        ];

        AppendTo[trace, <|"ParseSymbol"->parseSymbol, "Stack"->stack, "LastID"->lastId, "InputBuffer"->inputBuffer|>]
    ];

    parseTree = Flatten[parseTree, 1];

    If[OptionValue["Trace"], 
        Return[trace], 

        If[grammarCheck === $IncompleteGrammar,
            Return[{parseTree, {}}]
            ,
            Return[{parseTree, symbolRules}]
        ]
    ];
];

ParseTree[input_,grammar_]:=Block[{parseTree,startProduction,styledParseTree,styleRules,nodes,formattedNodes,plainNodes,nodeColorStyle},
parseTree = First@Parse[input,grammar];
styleRules = {NonTerm[x_]:>Style[ToString[x],Blue],Term[x_]:>Style[ToString[x],Red]};
styledParseTree = ReplaceAll[parseTree,styleRules];
startProduction = First[First[styledParseTree]];

nodes = Flatten[Map[{First[#],Last[#]}&,styledParseTree],1];
formattedNodes = Map[#->#[[1]]&,nodes];

plainNodes = Flatten[Map[{First[#],Last[#]}&,parseTree],1];
nodeColorStyle = ReplaceAll[plainNodes,
{
{NonTerm[x_],y_}:>({Style[ToString[x],Blue],y}->Blue),
{Term[x_],y_}:>({Style[ToString[x],Red],y}->Red),
{Term[x_],y_,z_}:>({Style[ToString[x],Red],y,z}->Red),
{EmptyString[],_}:> Nothing
}
];

TreePlot[styledParseTree,Automatic,startProduction,VertexLabels->formattedNodes,VertexStyle->nodeColorStyle]
];


(* ::Chapter:: *)
(*PL/0 Implementation*)


(* ::Subchapter::Closed:: *)
(*Lexer tokens definition*)


(* ::Text:: *)
(*Here we define the tokens that will be recognized by the lexer.*)


(* ::Input::Initialization:: *)
tokenSymbols = CreateTokens[
    {
    {" ","Whitespace"},
    {"\n", "LineBreak"},
    {":", "IncompleteAssign"},
    {":=", "Assign"},
    {"*", "Times"},
    {"/", "Slash"},
    {"+", "Plus"},
    {"-", "Minus"},
    {"=", "Equal"},
    {"#", "NotEqual"},
    {"(", "LeftParenthesis"},
    {")", "RightParenthesis"},
    {";", "Semicolon"},
    {",", "Comma"},
    {".", "Dot"},
    {">", "Greater"},
    {">=", "GreaterOrEqual"},
    {"<", "Lower"},
    {"<=", "LowerOrEqual"}
    }
];
tokenKeywords = CreateTokens[
{
    {"const", "Const"},
    {"var", "Var"},
    {"procedure", "Procedure"},
    {"call", "Call"},
    {"print", "Print"},
    {"begin", "Begin"},
    {"end", "End"},
    {"if", "If"},
    {"then", "Then"},
    {"while", "While"},
    {"do", "Do"},
    {"odd", "Odd"}
    }
];

tokenSymbolsRecognizer = CreateTokenRecognizer[tokenSymbols];
keywordRecognizer = CreateTokenRecognizer[tokenKeywords];

identifierRegex =RegexConcatenation[RegexAlphabet[], RegexStar[RegexUnion[RegexAlphabet[], RegexDigits[]]]];
numberLiteralRegex = RegexConcatenation[RegexDigits[], RegexStar[RegexDigits[]]];

identifierRecognizer = Regex[identifierRegex, "Identifier"];
numberLiteralRecognizer = Regex[numberLiteralRegex, "NumberLiteral"];
symbolRecognizer = RegexUnion[identifierRecognizer, numberLiteralRecognizer, tokenSymbolsRecognizer];


(* ::Input::Initialization:: *)
(* Used for linting *)
colorRules = {
    "Var"->RGBColor[0.33, 0.6, 0.], 
    "Const"->RGBColor[0.33, 0.6, 0.], 
    "NumberLiteral"->RGBColor[0.76, 0.37, 0.], 
    "Procedure"->RGBColor[0., 0.32, 1.], 
    "Call"->RGBColor[0.55, 0., 0.8200000000000001], 
    "Print"->RGBColor[0.55, 0., 0.8200000000000001], 
    "Begin"->RGBColor[0.55, 0., 0.8200000000000001], 
    "End"->RGBColor[0.55, 0., 0.8200000000000001], 
    "If"->RGBColor[0.55, 0., 0.8200000000000001], 
    "Then"->RGBColor[0.55, 0., 0.8200000000000001], 
    "While"->RGBColor[0.55, 0., 0.8200000000000001], 
    "Do"->RGBColor[0.55, 0., 0.8200000000000001], 
    "Odd"->RGBColor[0.55, 0., 0.8200000000000001], 
    "Assign"->RGBColor[0.5, 0.01, 0.], 
    "Equal"->RGBColor[0.5, 0.01, 0.], 
    "NotEqual"->RGBColor[0.5, 0.01, 0.], 
    "Greater"->RGBColor[0.5, 0.01, 0.], 
    "Lower"->RGBColor[0.5, 0.01, 0.], 
    "GreaterOrEqual"->RGBColor[0.5, 0.01, 0.], 
    "LowerOrEqual"->RGBColor[0.5, 0.01, 0.], 
    _->Black
};


Tokenize[program_] := Tokenize[program, symbolRecognizer, keywordRecognizer];
Lint[program_] := Lint[program, symbolRecognizer, keywordRecognizer, colorRules];


(* ::Subchapter:: *)
(*Grammar definition*)


(* ::Input::Initialization:: *)
(* Variable name generator *)
currentVar = None;
index = 0;

ResetVarGenerator[] := (index = 0);
NewVar[] := (currentVar = StringJoin["$", ToString[index++]]);
CurrentVar[] := currentVar;
CurrentVar[s_] := StringJoin[s,currentVar];

(* Label name generator *)
currentLabel = None;
labelIndex = 0;

ResetLabelGenerator[] := (labelIndex = 0);
NewLabel[] := (currentLabel = ToString[labelIndex++]);
CurrentLabel[s_] := StringJoin[currentLabel, "::", s];

LineJoin[l_] := StringJoin[Riffle[DeleteCases[l,""], " "]];
ColumnJoin[l_] := StringJoin[Riffle[DeleteCases[l,""], "\n"]];
LabelIdentifier[s_] := StringJoin["<", s, ">"];


(* ::Input::Initialization:: *)
grammar = {
(* Program *)
<|"From"->"Program","To"->{NonTerm["Block"],Term["Dot"]},
"Action"->{"TACode":>NonTerm["Block"]["TACode"]}|>,
<|"From"->"Block","To"->{NonTerm["ConstOpt"],NonTerm["VarOpt"],NonTerm["ProcRep"],NonTerm["Statement"]},
"Action"->{
"Value":>"",
"TACode":>ColumnJoin[
{
NonTerm["Statement"]["TACode"],
"return",
NonTerm["ProcRep"]["TACode"],
NonTerm["ConstOpt"]["TACode"],
NonTerm["VarOpt"]["TACode"]
}
]
}|>,

<|"From"->"ConstOpt","To"->{Term["Const"],Term["Identifier"],Term["Equal"],Term["NumberLiteral"],NonTerm["ConstOptRep"],Term["Semicolon"]},
"Action"->{
"Value":>"",
"TACode":>LineJoin[{"declare_const",LabelIdentifier[Term["Identifier"]["Value"]],Term["NumberLiteral"]["Value"]}]
}
|>,
<|"From"->"ConstOpt","To"->EmptyString[],
"Action"->{"Value"->"","TACode"->""}|>,

<|"From"->"ConstOptRep","To"->{Term["Comma"], Term["Identifier"],Term["Equal"],Term["NumberLiteral"],NonTerm["ConstOptRep"]},
"Action"->{
"TACode":>ColumnJoin[
{
LineJoin[{"declare_const",LabelIdentifier[Term["Identifier"]["Value"]],Term["NumberLiteral"]["Value"]}],
NonTerm["ConstOptRep"]["TACode"]
}
]
}
|>,
<|"From"->"ConstOptRep","To"->EmptyString[],
"Action"->{"Value"->"","TACode"->""}|>,

<|"From"->"VarOpt","To"->{Term["Var"],Term["Identifier"],NonTerm["VarOptRep"],Term["Semicolon"]},
"Action"->{
"Value":>"",
"TACode":>ColumnJoin[
{
LineJoin[{"declare_var",LabelIdentifier[Term["Identifier"]["Value"]]}],
NonTerm["VarOptRep"]["TACode"]
}
]
}|>,
<|"From"->"VarOpt","To"->EmptyString[],
"Action"->{"Value"->"","TACode"->""}|>,

<|"From"->"VarOptRep","To"->{Term["Comma"], Term["Identifier"],NonTerm["VarOptRep"]},
"Action"->{
"Value":>"",
"TACode":>ColumnJoin[
{
LineJoin[{"declare_var",LabelIdentifier[Term["Identifier"]["Value"]]}],
NonTerm["VarOptRep"]["TACode"]
}
]
}
|>,
<|"From"->"VarOptRep","To"->EmptyString[],
"Action"->{"Value"->"","TACode"->""}|>,

<|"From"->"ProcRep","To"->{Term["Procedure"],Term["Identifier"],Term["Semicolon"],NonTerm["Block"],Term["Semicolon"],NonTerm["ProcRep"]},
"Action"->{
"Value":>Term["Identifier"]["Value"],
"TACode":>
ColumnJoin[
{
LineJoin[{"begin_proc",LabelIdentifier[Term["Identifier"]["Value"]]}],
NonTerm["Block"]["TACode"],
LineJoin[{"end_proc",LabelIdentifier[Term["Identifier"]["Value"]]}],
NonTerm["ProcRep"]["TACode"]
}
]
}|>,
<|"From"->"ProcRep","To"->EmptyString[],
"Action"->{"Value"->"","TACode"->""}|>,

(* Statements *)
<|"From"->"Statement","To"->{Term["Identifier"],Term["Assign"],NonTerm["Expression"]},
"Action"->{
"Value":>"",
"TACode":>
ColumnJoin[
{
NonTerm["Expression"]["TACode"],
LineJoin[{"set",LabelIdentifier[Term["Identifier"]["Value"]],NonTerm["Expression"]["Value"]}]
}
]
}|>,
<|"From"->"Statement","To"->{Term["Call"],Term["Identifier"]},
"Action"->{
"Value":>"",
"TACode":>LineJoin[{"call", LabelIdentifier[Term["Identifier"]["Value"]]}]
}|>,
<|"From"->"Statement","To"->{Term["Print"],Term["Identifier"]},
"Action"->{
"Value":>"",
"TACode":>LineJoin[{"print", LabelIdentifier[Term["Identifier"]["Value"]]}]
}|>,
<|"From"->"Statement","To"->{Term["Begin"],NonTerm["Statement"],NonTerm["StatementRep"],Term["End"]},
"Action"->{
"Value":>"",
"TACode":>
ColumnJoin[
{
NonTerm["Statement"]["TACode"],
NonTerm["StatementRep"]["TACode"]
}
]
}|>,
<|"From"->"Statement","To"->{Term["If"],NonTerm["Condition"],Term["Then"],NonTerm["Statement"]},
"Action"->{
"Value":>NewLabel[],
"TACode":>
ColumnJoin[
{
NonTerm["Condition"]["TACode"],
LineJoin[{NonTerm["Condition"]["Conditional"],LabelIdentifier[CurrentLabel["L"]]}],
NonTerm["Statement"]["TACode"],
LineJoin[{"label",LabelIdentifier[CurrentLabel["L"]]}]
}
]
}|>,
<|"From"->"Statement","To"->{Term["While"],NonTerm["Condition"],Term["Do"],NonTerm["Statement"]},
"Action"->{
"Value":>NewLabel[],
"TACode":>
ColumnJoin[
{
LineJoin[{"label",LabelIdentifier[CurrentLabel["L1"]]}],
NonTerm["Condition"]["TACode"],
LineJoin[{NonTerm["Condition"]["Conditional"],LabelIdentifier[CurrentLabel["L2"]]}],
NonTerm["Statement"]["TACode"],
LineJoin[{"goto",LabelIdentifier[CurrentLabel["L1"]]}],
LineJoin[{"label",LabelIdentifier[CurrentLabel["L2"]]}]
}
]
}|>,
<|"From"->"Statement","To"->EmptyString[],
"Action"->{"Value"->"","TACode"->""}|>,

<|"From"->"StatementRep","To"->{Term["Semicolon"],NonTerm["Statement"],NonTerm["StatementRep"]},
"Action"->{
"Value":>"",
"TACode":>
ColumnJoin[
{
NonTerm["Statement"]["TACode"],
NonTerm["StatementRep"]["TACode"]
}
]
}|>,
<|"From"->"StatementRep","To"->EmptyString[],
"Action"->{"Value"->"","TACode"->""}|>,

(* Conditionals *)
<|"From"->"Condition","To"->{Term["Odd"],NonTerm["Expression"]},
"Action"->{
"TACode":>
ColumnJoin[
{
NonTerm["Expression"]["TACode"],
LineJoin[{"odd",NonTerm["Expression"]["Value"]}]
}
],
"Conditional"->"if_odd"
}
|>,
<|"From"->"Condition","To"->{NonTerm["Expression1"],NonTerm["Op"],NonTerm["Expression2"]},"Action"->{
"TACode":>
ColumnJoin[
{
NonTerm["Expression1"]["TACode"],
NonTerm["Expression2"]["TACode"],
LineJoin[{"compare",NonTerm["Expression2"]["Value"],NonTerm["Expression1"]["Value"]}]
}
],
"Conditional"->NonTerm["Op"]["Conditional"]
}|>,
<|"From"->"Expression1","To"->{NonTerm["Expression"]},"Action"->{"Value":>NonTerm["Expression"]["Value"],"TACode":>NonTerm["Expression"]["TACode"]}|>,
<|"From"->"Expression2","To"->{NonTerm["Expression"]},"Action"->{"Value":>NonTerm["Expression"]["Value"],"TACode":>NonTerm["Expression"]["TACode"]}|>,
<|"From"->"Op","To"->{Term["Equal"]},"Action"->{"Conditional"->"if_equal"}|>,
<|"From"->"Op","To"->{Term["NotEqual"]},"Action"->{"Conditional"->"if_not_equal"}|>,
<|"From"->"Op","To"->{Term["Lower"]},"Action"->{"Conditional"->"if_less"}|>,
<|"From"->"Op","To"->{Term["LowerOrEqual"]},"Action"->{"Conditional"->"if_less_or_equal"}|>,
<|"From"->"Op","To"->{Term["Greater"]},"Action"->{"Conditional"->"if_greater"}|>,
<|"From"->"Op","To"->{Term["GreaterOrEqual"]},"Action"->{"Conditional"->"if_greater_or_equal"}|>,

(* Numeric expressions *)
<|"From"->"Expression","To"->{NonTerm["SignOpt"],NonTerm["Term"],NonTerm["AddRep"]},
"Action":>If[NonTerm["AddRep"]["Value"] != "",
(* Case when there is a nonempty AddRep *)
{
"Value":>NewVar[],
"TACode":>
ColumnJoin[
{
NonTerm["Term"]["TACode"],
NonTerm["AddRep"]["TACode"],
LineJoin[{NonTerm["AddRep"]["Op"],CurrentVar[],NonTerm["SignOpt"]["Value"], NonTerm["Term"]["Value"],NonTerm["AddRep"]["Value"]}]
}
]
}
,
(* Case when AddRep is an empty string *)
{
"Value":>NonTerm["Term"]["Value"],
"TACode":>NonTerm["Term"]["TACode"]
}
]
|>,

<|"From"->"SignOpt","To"->{Term["Plus"]},
"Action"->{"Value"->"+"}|>,
<|"From"->"SignOpt","To"->{Term["Minus"]},
"Action"->{"Value"->"-"}|>,
<|"From"->"SignOpt","To"->EmptyString[],
"Action"->{"Value"->""}|>,

<|"From"->"AddRep","To"->{Term["Plus"],NonTerm["Term"],NonTerm["AddRep"]},
"Action":>If[NonTerm["AddRep"]["Value"] != "",
(* Case when there is a nonempty AddRep *)
{
"Value":>NewVar[],
"TACode":>
ColumnJoin[
{
NonTerm["Term"]["TACode"],
NonTerm["AddRep"]["TACode"],
LineJoin[{NonTerm["AddRep"]["Op"],CurrentVar[],NonTerm["Term"]["Value"],NonTerm["AddRep"]["Value"]}]
}
],
"Op"->"add"
}
,
(* Case when AddRep is an empty string *)
{
"Value":>NonTerm["Term"]["Value"],
"TACode":>NonTerm["Term"]["TACode"],
"Op"->"add"
}
]
|>,

<|"From"->"AddRep","To"->{Term["Minus"],NonTerm["Term"],NonTerm["AddRep"]},
"Action":> If[NonTerm["AddRep"]["Value"] != "",
(* Case when there is a nonempty AddRep *)
{
"Value":>NewVar[],
"TACode":>ColumnJoin[
{
NonTerm["Term"]["TACode"],
NonTerm["AddRep"]["TACode"],
LineJoin[{NonTerm["AddRep"]["Op"],CurrentVar[],NonTerm["Term"]["Value"],NonTerm["AddRep"]["Value"]}]
}
],
"Op"->"substract"
}
,
(* Case when AddRep is an empty string *)
{
"Value":>NonTerm["Term"]["Value"],
"TACode":>NonTerm["Term"]["TACode"],
"Op"->"substract"
}
]
|>,

<|"From"->"AddRep","To"->EmptyString[],
"Action"->{"Value"->"","TACode"->"","Op"->""}|>,

<|"From"->"Term","To"->{NonTerm["Factor"],NonTerm["MultRep"]},
"Action":> If[NonTerm["MultRep"]["Value"] !="" ,
(* Case when there is a nonempty MultRep *)
{
"Value":>NewVar[],
"TACode":>
ColumnJoin[
{
NonTerm["Factor"]["TACode"],
NonTerm["MultRep"]["TACode"],
LineJoin[{NonTerm["MultRep"]["Op"],CurrentVar[],NonTerm["Factor"]["Value"],NonTerm["MultRep"]["Value"]}]
}
]
}
,
(* Case when MultRep is an empty string *)
{
"Value":>NonTerm["Factor"]["Value"],
"TACode":>NonTerm["Factor"]["TACode"]
}
]
|>,

<|"From"->"MultRep","To"->{Term["Times"],NonTerm["Factor"],NonTerm["MultRep"]},
"Action":>If[NonTerm["MultRep"]["Value"] !="" ,
(* Case when there is a nonempty MultRep *)
{
"Value":>NewVar[],
"TACode":>
ColumnJoin[
{
NonTerm["Factor"]["TACode"],
NonTerm["MultRep"]["TACode"],
LineJoin[{NonTerm["MultRep"]["Op"], CurrentVar[],NonTerm["Factor"]["Value"],NonTerm["MultRep"]["Value"]}]
}
],
"Op"->"multiply"
}
,
(* Case when MultRep is an empty string *)
{
"Value":>NonTerm["Factor"]["Value"],
"TACode":>NonTerm["Factor"]["TACode"],
"Op"->"multiply"
}
]
|>,

<|"From"->"MultRep","To"->{Term["Slash"],NonTerm["Factor"],NonTerm["MultRep"]},
"Action":>If[NonTerm["MultRep"]["Value"] !="" ,
{
"Value":>NewVar[],
"TACode":>
ColumnJoin[
{
NonTerm["Factor"]["TACode"],
NonTerm["MultRep"]["TACode"],
LineJoin[{NonTerm["MultRep"]["Op"],CurrentVar[], NonTerm["Factor"]["Value"],NonTerm["MultRep"]["Value"]}]
}
],
"Op"->"divide"
}
,
(* Case when MultRep is an empty string *)
{
"Value":>NonTerm["Factor"]["Value"],
"TACode":>NonTerm["Factor"]["TACode"],
"Op"->"divide"
}
]
|>,

<|"From"->"MultRep","To"->EmptyString[],
"Action"->{"Value"->"","TACode"->"","Op"->""}|>,

<|"From"->"Factor","To"->{Term["Identifier"]},
"Action"->{"Value":>LabelIdentifier[Term["Identifier"]["Value"]],"TACode"->""}|>,

<|"From"->"Factor","To"->{Term["NumberLiteral"]},
"Action"->{"Value"->Term["NumberLiteral"]["Value"],"TACode"->""}|>,

<|"From"->"Factor","To"->{Term["LeftParenthesis"],NonTerm["Expression"],Term["RightParenthesis"]},
"Action"->{"Value"->NonTerm["Expression"]["Value"],"TACode"->NonTerm["Expression"]["TACode"]}|>
};


Parse[input_] := Parse[input, grammar];
ParseTree[input_] := ParseTree[input, grammar];


(* ::Subchapter::Closed:: *)
(*Parse tree synthesization*)


(* ::Text:: *)
(*After the construction of the parse tree, the intermediate code can be constructed by using associated semantic rules that are evaluated from children nodes to parents.*)


(* ::Input::Initialization:: *)
GetAction[treeSymbol_, parseTree_] := Block[{children, replaceTerms, action},
    (* 
    Get the grammar action formatted for a given symbol. For example, for a parse tree where the symbol
    {NonTerm["Statement"], 19} has the associated action
    {"Value"\[RuleDelayed]"","TACode"\[RuleDelayed]ColumnJoin[{NonTerm["Statement"]["TACode"], NonTerm["StatementRep"]["TACode"]}]},
    executing
    GetAction[{NonTerm["Statement"], 19},parseTree] will return
    {"Value"\[RuleDelayed]"","TACode"\[RuleDelayed]ColumnJoin[{{NonTerm["Statement"], 28}["TACode"], {NonTerm["StatementRep"], 29}["TACode"]}]}
    *)
    children = Cases[First[parseTree], HoldPattern[treeSymbol->c_] :> c];
    replaceTerms = Map[First[#]->Take[#, 2]&, children];
    action = Last[FirstCase[Last[parseTree], {treeSymbol, _}]];

    ReplaceAll[action, replaceTerms]
];


(* ::Input::Initialization:: *)
SynthesizeNonTerm[treeSymbol_, parseTree_, synthesizations_] := Block[{eval, new},
    (* Synthesize from values obtained in past synthesizations *)
    eval = ReplaceAll[GetAction[treeSymbol, parseTree], synthesizations];
    If[eval === Missing["KeyAbsent", "Action"], Return[synthesizations]];

    new = Map[treeSymbol[First[#]]->Last[#]&, eval];
    Return[Join[new, synthesizations]]
];

SynthesizeTerm[treeSymbol_, synthesizations_] := Block[{eval, new},
    (* Label the value of a Terminal *)
    If[Length[treeSymbol] > 2,
        Prepend[synthesizations, Take[treeSymbol, 2]["Value"] -> Last[treeSymbol]], 
        synthesizations
    ]
];


(* ::Input::Initialization:: *)
SynthesizeTree[parseTree_] := Block[{startSymbol, depthFirstScan, symbolType, synthesized},
    ResetVarGenerator[];
    ResetLabelGenerator[];

    (* Walk the parse tree in post order *)
    startSymbol = parseTree[[1,1,1]];
    depthFirstScan = First[
        Last[
            Reap[
                DepthFirstScan[Graph[First[parseTree]], startSymbol,{"PostvisitVertex"->(Sow[#]&)}]
            ]
        ]
    ];

    (* Synthesize in the order of the depthfirstscan *)
    synthesized = {};
    Do[
        symbolType = Head[First[s]];
        If[symbolType === Term, synthesized = SynthesizeTerm[s, synthesized]];
        If[symbolType === NonTerm, synthesized = SynthesizeNonTerm[s, parseTree, synthesized]];
        ,
        {s, depthFirstScan}
    ];

    Return[synthesized];
];


(* ::Input::Initialization:: *)
FormatSynthValues[groupedSynthetization_] := MapAt[First[Level[#, 2]]&, groupedSynthetization, {All, 1}];
Synthesis[parseTree_]:=ReplaceAll[{NonTerm["Program"],1}["TACode"],SynthesizeTree[parseTree]];
ViewSynthesis[parseTree_] := Block[{grouped},
    grouped = GroupBy[SynthesizeTree[parseTree], Head[First[#]]&];
    Dataset@Map[FormatSynthValues, grouped]
];


(* ::Subchapter::Closed:: *)
(*Compilation to intermediate code*)


(* ::Text:: *)
(*Since the tree synthesization is L-Attributed, meaning that every node can only synthesize from its children, variables inside procedures "cannot see" if there is already another variable in the global space with the same name. This is not a problem since we can label variables inside procedures to differenciate them from their global counterparts. This is done by ProcedureProcessTags for each procedure. ICProcessTags does this but for the whole synthesized tree. The resulting code is a form of intermediate code that can be translated to any target machine.*)


(* ::Input::Initialization:: *)
IsLabelQ[s_]:=And[StringStartsQ[s, "<"], StringEndsQ[s, ">"]];
IsTempQ[s_]:=StringStartsQ[s, "$"];
InsertSequence[l1_,l2_,n_]:=FlattenAt[Insert[l1,l2,n],n];

ProcedureProcessLabelsAndTemps[ic_,from_,to_,globalVar_]:=Block[{name,procInstructions,labels,tempSymbols,replaceLabelsRules,replaceTempSymbolsRules,tempDeclarations,processed},
name = StringReplace[Last[Extract[ic,{from}]],"<"~~x__~~">":>x];
procInstructions = Flatten[Take[ic,{from+1,to-1}]];
labels = DeleteDuplicates[Select[procInstructions,IsLabelQ[#]&&!MemberQ[globalVar,#]&]];
tempSymbols = DeleteDuplicates[Select[procInstructions,IsTempQ]];
replaceLabelsRules = Map[#->StringReplace[#,"<"~~x__~~">":>StringJoin["<",name,"::",x,">"]]&,labels];
replaceTempSymbolsRules = Map[#->StringJoin["<",name,"::",#,">"]&,tempSymbols];
tempDeclarations = Thread[{"declare_var",replaceTempSymbolsRules[[All,2]]}];

processed = ReplaceAll[Take[ic,{from,to}],  Join[replaceLabelsRules,replaceTempSymbolsRules]];
processed = InsertSequence[processed,tempDeclarations,-2];
Return[processed];
];
GlobalProcessLabelsAndTemps[ic_]:=Block[{procInstructions,tempSymbols,replaceTempSymbolsRules,tempDeclarations,processed},
procInstructions = Flatten[ic];
tempSymbols = DeleteDuplicates[Select[procInstructions,IsTempQ]];
replaceTempSymbolsRules = Map[#->StringJoin["<",#,">"]&,tempSymbols];
tempDeclarations = Thread[{"declare_var",replaceTempSymbolsRules[[All,2]]}];
processed = ReplaceAll[ic,  replaceTempSymbolsRules];
processed = InsertSequence[processed,tempDeclarations,-2];
Return[processed];
];
GlobalProcessLabelsAndTemps[{}] = {};
ICProcessLabelsAndTemps[ic_]:=Block[{splitted,preProc,pos,parts,processed,postProc,globalVar,allProcessed,procICCode},
splitted = Map[StringSplit,StringSplit[ic,"\n"]];
pos = Flatten[Position[splitted,{"begin_proc",_}|{"end_proc",_}]];
If[pos != {},
preProc = GlobalProcessLabelsAndTemps[Take[splitted,{1,First[pos]-1}]];
postProc = GlobalProcessLabelsAndTemps[Take[splitted,{Last[pos]+1,Length[splitted]}]];
globalVar = Cases[Join[preProc,postProc],{"declare_var",label_}:>label];

parts = Partition[pos,2];
processed = Map[ProcedureProcessLabelsAndTemps[splitted,First[#],Last[#],globalVar]&,parts];

allProcessed = Join[preProc,Concatenate[processed],postProc];
,
allProcessed = GlobalProcessLabelsAndTemps[splitted];
];
procICCode = StringJoin[Riffle[Map[LineJoin,allProcessed],"\n"]];
Return[procICCode];
];


(* ::Input::Initialization:: *)
PL0ParseAndSynthesize[code_] := Block[{tokens,parseTree,synthesized},
    (* Return tree synthesis without label processing (may present namespace conflicts) *)
    tokens = Tokenize[code, symbolRecognizer, keywordRecognizer];
    parseTree = Parse[tokens, grammar] ;
    synthesized = ReplaceAll[{NonTerm["Program"], 1}["TACode"], SynthesizeTree[parseTree]];
    Return[synthesized];
];
PL0CompileToIC[code_] := ICProcessLabelsAndTemps[PL0ParseAndSynthesize[code]];


(* ::Input::Initialization:: *)
PlotIC[ic_] := Block[{splitted,completed},
    splitted = Map[StringSplit, StringSplit[ic, "\n"]];
    completed = Map[PadRight[#, 4,""]&, splitted];

    Grid[
        Prepend[completed, {"Instruction", "P1", "P2", "P3"}], 
        Background->{{LightGreen},{LightRed}},
        Frame->True
    ]
];


(* ::Chapter:: *)
(*Assembly code generation*)


(* ::Text:: *)
(*Intermediate code is converted directly to assembler by simple substitution rules. Also, some standard routines like multiplication or division have to be appended at the end of the ASM code.*)


(* ::Subchapter:: *)
(*Common routines*)


(* ::Text:: *)
(*Since the target machine doesn't have native instructions for implementing multiplication or division, we can add them as procedures.*)


(* ::Input::Initialization:: *)
divisionRoutine = "Label <divide>
LoadA <divide::zero>
Store <divide::result>
Label <divide::begin_loop>
LoadA <divide::result>
Increment
Store <divide::result>
LoadA <divide::n1>
LoadB <divide::n2>
Substract
Store <divide::n1>
JumpNeg <divide::end_loop>
Jump <divide::begin_loop>
Label <divide::end_loop>
LoadA <divide::result>
Decrement
Return
Declare <divide::n1> 0
Declare <divide::n2> 0
Declare <divide::result> 0
Declare <divide::zero> 0";
multiplicationRoutine = "Label <multiply>
LoadA <multiply::zero>
Store <multiply::result>
Label <multiply::begin_loop>
LoadA <multiply::result>
LoadB <multiply::n2>
Add
Store <multiply::result>
LoadA <multiply::n1>
Decrement
Store <multiply::n1>
JumpZero <multiply::end_loop>
Jump <multiply::begin_loop>
Label <multiply::end_loop>
Return
Declare <multiply::n1> 0
Declare <multiply::n2> 0
Declare <multiply::result> 0
Declare <multiply::zero> 0";


(* ::Subchapter:: *)
(*Intermediate code to assembly conversion rules*)


(* ::Input::Initialization:: *)
(* Intermediate code can be mapped directly to assembler with a substitution rule *)
icToAsmRules = {
    {"call",label_} :> {{"Call", label}},
    {"print",label_} :> {{"Print", label}},
    {"return"}->{{"Return"}},
    {"begin_proc",label_} :> {{"Label",label}},{"end_proc",_} :> Nothing,
    {"label",label_} :> {{"Label", label}},
    {"goto",label_} :> {{"Jump",label}},
    {"declare_var",label_} :> {{"Declare",label,"0"}},
    {"declare_const",label_, n_} :> {{"Declare",label,n}},

    {"set",out_/;IsLabelQ[out], n_/;!IsLabelQ[n]} :>  {{"SetA",n},{"Store",out}},
    {"set",out_/;IsLabelQ[out], l_/;IsLabelQ[l]} :>  {{"LoadA",l},{"Store",out}},

    {"compare",l1_/;IsLabelQ[l1], l2_/;IsLabelQ[l2]} :> {{"LoadA",l1},{"LoadB",l2},{"Substract"}},
    {"compare",l_/;IsLabelQ[l], n_/;!IsLabelQ[n]} :> {{"LoadA",l},{"SetB",n},{"Substract"}},
    {"compare",n_/;!IsLabelQ[n], l_/;IsLabelQ[l]} :> {{"SetA",n},{"LoadB",l},{"Substract"}},

    {"if_less",label_} :> {{"JumpNeg",label}},
    {"if_greater",label_} :> {{"JumpPos",label}},
    {"if_equal",label_} :> {{"JumpNotZero",label}},
    {"if_not_equal",label_} :> {{"JumpZero",label}},
    {"if_less_or_equal",label_} :> {{"JumpNeg",label},{"JumpNotZero",label}},
    {"if_greater_or_equal",label_} :> {{"JumpPos",label},{"JumpNotZero",label}},
    {"if_odd",label_} :> {{"JumpOdd",label}},

    {"odd",l_/;IsLabelQ[l]} :> {{"LoadA",l}},
    {"odd",n_/;!IsLabelQ[n]} :> {{"SetA",n}},

    {"add",out_, l1_/;IsLabelQ[l1], l2_/;IsLabelQ[l2]} :> {{"LoadA",l1},{"LoadB",l2},{"Add"},{"Store",out}},
    {"add",out_, n1_/;!IsLabelQ[n1], n2_/;!IsLabelQ[n2]} :> {{"SetA",n1},{"SetB",n2},{"Add"},{"Store",out}},
    {"add",out_, l_/;IsLabelQ[l], n_/;!IsLabelQ[n]} :> {{"LoadA",l},{"SetB",n},{"Add"},{"Store",out}},
    {"add",out_, n_/;!IsLabelQ[n], l_/;IsLabelQ[l]} :> {{"SetA",n},{"LoadB",l},{"Add"},{"Store",out}},

    {"substract",out_, l1_/;IsLabelQ[l1], l2_/;IsLabelQ[l2]} :> {{"LoadA",l1},{"LoadB",l2},{"Substract"},{"Store",out}},
    {"substract",out_, n1_/;!IsLabelQ[n1], n2_/;!IsLabelQ[n2]} :> {{"SetA",n1},{"SetB",n2},{"Substract"},{"Store",out}},
    {"substract",out_, l_/;IsLabelQ[l], n_/;!IsLabelQ[n]} :> {{"LoadA",l},{"SetB",n},{"Substract"},{"Store",out}},
    {"substract",out_, n_/;!IsLabelQ[n], l_/;IsLabelQ[l]} :> {{"SetA",n},{"LoadB",l},{"Substract"},{"Store",out}},

    {"multiply",out_, l1_/;IsLabelQ[l1], l2_/;IsLabelQ[l2]} :> {{"LoadA",l1},{"Store","<multiply::n1>"},{"LoadA",l2},{"Store","<multiply::n2>"},{"Call","<multiply>"},{"Store",out}},
    {"multiply",out_, n1_/;!IsLabelQ[n1], n2_/;!IsLabelQ[n2]} :> {{"SetA",n1},{"Store","<multiply::n1>"},{"SetA",n2},{"Store","<multiply::n2>"},{"Call","<multiply>"},{"Store",out}},
    {"multiply",out_, l_/;IsLabelQ[l], n_/;!IsLabelQ[n]} :> {{"LoadA",l},{"Store","<multiply::n1>"},{"SetA",n},{"Store","<multiply::n2>"},{"Call","<multiply>"},{"Store",out}},
    {"multiply",out_, n_/;!IsLabelQ[n], l_/;IsLabelQ[l]} :> {{"SetA",n},{"Store","<multiply::n1>"},{"LoadA",l},{"Store","<multiply::n2>"},{"Call","<multiply>"},{"Store",out}},

    {"divide",out_, l1_/;IsLabelQ[l1], l2_/;IsLabelQ[l2]} :> {{"LoadA",l1},{"Store","<divide::n1>"},{"LoadA",l2},{"Store","<divide::n2>"},{"Call","<divide>"},{"Store",out}},
    {"divide",out_, n1_/;!IsLabelQ[n1], n2_/;!IsLabelQ[n2]} :> {{"SetA",n1},{"Store","<divide::n1>"},{"SetA",n2},{"Store","<divide::n2>"},{"Call","<divide>"},{"Store",out}},
    {"divide",out_, l_/;IsLabelQ[l], n_/;!IsLabelQ[n]} :> {{"LoadA",l},{"Store","<divide::n1>"},{"SetA",n},{"Store","<divide::n2>"},{"Call","<divide>"},{"Store",out}},
    {"divide",out_, n_/;!IsLabelQ[n], l_/;IsLabelQ[l]} :> {{"SetA",n},{"Store","<divide::n1>"},{"LoadA",l},{"Store","<divide::n2>"},{"Call","<divide>"},{"Store",out}}
};
CompileICToASM[ic_] := Block[{splitted, replaced, generatedCode},
    splitted = Map[StringSplit, StringSplit[ic, "\n"]];
    replaced = ReplaceAll[splitted, icToAsmRules];
    generatedCode = ColumnJoin[Map[LineJoin, Concatenate[replaced]]];

    If[StringContainsQ[generatedCode, "multiply"], 
        generatedCode = ColumnJoin[{generatedCode, multiplicationRoutine}]
    ];
    If[StringContainsQ[generatedCode, "divide"], 
        generatedCode = ColumnJoin[{generatedCode, divisionRoutine}]
    ];

    Return[generatedCode];
];

CompilePL0ToASM[code_] := CompileICToASM[PL0CompileToIC[code]];


(* ::Subchapter:: *)
(*Machine code generation*)


(* ::Text:: *)
(*For the conversion of ASM code to machine code all the labels (IC arguments enclosed in  <> ) need to be converted to memory positions. *)


(* ::Input::Initialization:: *)
cpuMnemonics = {
    "LoadA","LoadB","SetA","SetB","Store","Add",
    "Substract","Increment","Decrement","Jump","JumpPos","JumpNeg","JumpZero","JumpNotZero","JumpOdd",
    "Call","Return","Print","Halt"
};
opcodes = Thread[cpuMnemonics->Take[Tuples[{0,1}, 8], Length[cpuMnemonics]]];
revOpcodes = Map[Reverse, opcodes];


(* ::Input::Initialization:: *)
DecimalToBinary[n_, size_ :8] := PadLeft[IntegerDigits[n, 2], size];
BinaryToDecimal[l_] := FromDigits[l, 2];

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
PL0Compile[code_] := AssemblyCompile[CompilePL0ToASM[code]];


(* ::Chapter:: *)
(*End of package*)


End[ ]

EndPackage[ ]
