(* ::Package:: *)

BeginPackage["LorentzObjects`"];


DeclaredQ::usage = "DeclaredQ[x] returns True if x is declared as an LObject.";
DeclaredObjects::usage = "DeclaredObjects[] returns a list of all declared LObjects.";
ClearAllLObjects::usage = "ClearAllLObjects[] clears all LObjects.";
DeclaredFlatMetricQ::usage = "DeclaredFlatMetricQ returns True if a metric without coordinate dependence has been declared.";
DeclaredCurvedMetricQ::usage = "DeclaredCurvedMetricQ returns True if a metric with coordinate dependence has been declared.";
LObjectType::usage = "LObjectType[x] returns the type of LObject that x has been declared as.";
DeclareLVector::usage = "DeclareLVector[v] declares v as an LVector without coordinate dependence.";
DeclareLTensor::usage = "DeclareLTensor[T] declares T as an LTensor without coordinate dependence and without determined symmetry properties.";
DeclareLMetric::usage = "DeclareLMetric[M] declares M as a flat-space LMetric.";
STDimension::usage = "STDimension is the dimension of spacetime. Default value is 4.";
LI::usage = "LI[x] represents a raised Lorentz index x. LI[-x] represents a lowered Lorentz index x";
LContract::usage = "LContract[expr] contracts repeated Lorentz indices.";
Eps::usage = "Eps[a,b,c,d] is the totally antisymmetric Levi-Civita symbol in four dimensions.";
EpsSort::usage = "EpsSort[expr] arranges the arguments of all Levi-Civita symbols in expr in canonical order.";
ReduceEps::usage = "ReduceEps controls whether LContract reduces products of Levi-Civita symbols. Default is False.";


Begin["`Private`"]


(* ::Section::Initialization::Closed:: *)
(*Declare vectors and tensors*)


(* ::Input::Initialization:: *)
DeclaredFlatMetricQ=False;
DeclaredCurvedMetricQ=False;


(* ::Input::Initialization:: *)
DeclaredQ[LObject_]:=MemberQ[ToExpression[(Sequence@@#)&/@Options[LObjectName[LObject]]],LObjectType];
DeclaredObjects[]:=Select[Names["Global`*"],DeclaredQ[ToExpression[#]]&];
ClearAllLObjects[]:=Module[{},
ClearAll/@DeclaredObjects[];
DeclaredCurvedMetricQ=False;
DeclaredFlatMetricQ=False;];


(* ::Input::Initialization:: *)
LObjectType[x_]:=OptionValue[LObjectName[x],"LObjectType"];
LVectorQ[x_]:=SameQ[OptionValue[LObjectName[x],"LObjectType"],LVector];
LTensorQ[x_]:=SameQ[OptionValue[LObjectName[x],"LObjectType"],LTensor];
LMetricQ[x_]:=SameQ[OptionValue[LObjectName[x],"LObjectType"],LMetric];
VariationQ[x_]:=SameQ[OptionValue[LObjectName[x],"LObjectType"],Variation];
SortProperties[x_]:=Switch[OptionValue[LObjectName[x],"Symmetric"],True,1,False,-1,Indeterminate,Indeterminate];


(* ::Input::Initialization:: *)
Options[DeclareLVector]={"Coordinates"->False};
DeclareLVector[v_,OptionsPattern[]]:=Module[{},
ClearAll[v];
Options[v]={"LObjectType"->LVector,"Symmetric"->True};
LObjectName[v]:=v;
LObjectName[v[__]]:=v;
If[OptionValue["Coordinates"],
LObjectName[v[__][__]]:=v;
v[c__][LI[x_]]:=TS[LI[x],v[c]];,
v[LI[x_]]:=TS[LI[x],v];
]
];
DeclareLVector[v_?ListQ]:=Do[DeclareLVector[v[[j]]],{j,1,Length[v]}];
DeclareLVector[v_?ListQ,"Coordinates"->True]:=Do[DeclareLVector[v[[j]],"Coordinates"->True],{j,1,Length[v]}];


(* ::Input::Initialization:: *)
Options[DeclareLTensor]={"Symmetric"->Indeterminate,"Coordinates"->False};
DeclareLTensor[T_,OptionsPattern[]]:=Module[{},
ClearAll[T];
Options[T]={"LObjectType"->LTensor,"Symmetric"->OptionValue["Symmetric"]};
LObjectName[T]:=T;
LObjectName[T[__]]:=T;
If[OptionValue["Coordinates"],
LObjectName[T[__][__]]:=T;
T[c__][LI[x_],LI[y_]]:=TS[LI[x],T[c],LI[y]];,
T[LI[x_],LI[y_]]:=TS[LI[x],T,LI[y]];
];
];


(* ::Input::Initialization:: *)
Options[DeclareLMetric]={"Coordinates"->False};
DeclareLMetric[g_,OptionsPattern[]]:=Module[{DiffMetricName},
ClearAll[g];
Options[g]={"LObjectType"->LMetric,"Symmetric"->True};
SetAttributes[g,Orderless];
LObjectName[g]:=g;
LObjectName[g[__]]:=g;
If[OptionValue["Coordinates"],
LObjectName[g[__][__]]:=g;
LCurvedMetricName=g;
DeclaredCurvedMetricQ=True;
g[c__][LI[x_],LI[y_]]:=TS[LI[x],g[c],LI[y]];
DiffMetricName=ToExpression["del"<>ToString[g]];
DiffMetric[g]=DiffMetricName;
DeclareLTensor[DiffMetricName,"Coordinates"->True,"Symmetric"->True];,
LFlatMetricName=g;
DeclaredFlatMetricQ=True;
g[LI[x_],LI[y_]]:=TS[LI[x],LI[y]];
MetricDet[g]=1;
];
];


(* ::Section::Initialization::Closed:: *)
(*Lorentz index contractions*)


(* ::Subsection::Initialization::Closed:: *)
(*Lorentz indices*)


(* ::Input::Initialization:: *)
Options[LI]={"LObjectType"->LorentzIndex};
LObjectName[LI[_]]:=LI;


(* ::Input::Initialization:: *)
LI/:-LI[x_]:=LI[-x];
FSI[-x_]:=-FSI[x];


(* ::Input::Initialization:: *)
ContractedQ[LI[x_],LI[y_]]:=SameQ[x/y,-1];


(* ::Input::Initialization:: *)
CleanDummyIndices[expr_]/;FreeQ[expr,Alternatives[LI[_],FSI[_]]]:=expr;


(* ::Input::Initialization:: *)
CleanDummyIndices[expr_]:=Module[{ListOfIndices,RepeatedIndices,OpenIndices,DummyIndices,NumberDummyIndices,FreeLabels},
ListOfIndices=Cases[expr,LI[_],\[Infinity]];
RepeatedIndices=Intersection[#,#/.LI[x_]:>LI[-x]]&@ListOfIndices;
OpenIndices=Complement[ListOfIndices,RepeatedIndices];
DummyIndices=DeleteDuplicates[RepeatedIndices/.LI[-x_]:>LI[x]/.LI[x_]:>x];
NumberDummyIndices=Length[DummyIndices];
FreeLabels=Take[Complement[DeleteCases[ToExpression[Alphabet["Greek"]],_?NumericQ],OpenIndices/.LI[-x_]:>x/.LI[x_]:>x],NumberDummyIndices];
expr/.MapThread[#1->#2&,{DummyIndices,FreeLabels}]
];


(* ::Subsection::Initialization::Closed:: *)
(*Tensor string and trace properties & spacetime dimension*)


(* ::Input::Initialization:: *)
STDimension=4;


(* ::Input::Initialization:: *)
Options[TS]={"LObjectType"->TensorString};
LObjectName[TS[__]]:=TS;
TS[a___,b_+c_,d___]:=TS[a,b,d]+TS[a,c,d];
TS[a___,Times[b_ ,c__],d___]/;Not[DeclaredQ[b]]:=b TS[a,c,d];
TS[___,0,___]:=0;
TS[a__,\[Eta]_,b__]/;SameQ[\[Eta],LFlatMetricName]:=TS[a,b];
TS[LI[\[Mu]_],LI[\[Nu]_]]/;ContractedQ[LI[\[Mu]],LI[\[Nu]]]:=STDimension;


(* ::Input::Initialization:: *)
Options[TT]={"LObjectType"->TensorTrace};
LObjectName[TT[__]]:=TT;
TT[a___,b_+c_,d___]:=TT[a,b,d]+TT[a,c,d];
TT[___,0,___]:=0
TT[a___,Times[b_ ,c__],d___]/;Not[DeclaredQ[b]]:=b TT[a,c,d];


(* ::Input::Initialization:: *)
TS[LI[x_],a__,LI[y_]]/;ContractedQ[LI[x],LI[y]]:=TT[a];
TT[a_]/;SortProperties[a]==-1:=0;


(* ::Subsubsection::Initialization::Closed:: *)
(*Sort TS (incomplete)*)


(* ::Input::Initialization:: *)
TSSimpleSort={(*TS[X1_[LI[\[Mu]1_]],X2_[LI[\[Mu]2_]]]/;And[NumericQ[SortProperties[X1]],NumericQ[SortProperties[X2]]]:>If[Signature[{X1[LI[\[Mu]1]],X2[LI[\[Mu]2]]}]==1,1,SortProperties[X1]SortProperties[X2]]TS[Sequence@@Sort[{X1[LI[\[Mu]1]],X2[LI[\[Mu]2]]}]],*)TS[a_,b_]:>TS[Sequence@@Sort[{a,b}]],TS[x_,a__,y_]/;And@@(NumericQ[SortProperties[#]]&/@{a}):>If[Signature[{x,y}]==1,TS[x,a,y],Times@@(SortProperties[#]&/@{a})TS[y,Sequence@@Reverse[{a}],x]]};


(* ::Input::Initialization:: *)
(*TensorSortable[LSequenceType_[TensorSequence__]]/;Or[LSequenceType===TS,LSequenceType===TT]:=And@@(NumericQ[SortProperties[#]]&/@(Select[{TensorSequence},!FreeQ[#,LTensor]&]/.{LTensor[Name_[___]][__]:>Name,LTensor[Name_[___]]:>Name}));*)


(* ::Input::Initialization:: *)
(*TSSort[LSequenceType_[a_,b_,c_]]/;And[TensorSortable[LSequenceType[a,b,c]],Signature[{a,c}]==1]:=LSequenceType[a,b,c];
TSSort[LSequenceType_[a_,b_,c_]]/;And[TensorSortable[LSequenceType[a,b,c]],Signature[{a,c}]==-1]:=(Times@@(SortProperties[#]&/@(Select[{a,b,c},!FreeQ[#,LTensor]&]/.{LTensor[Name_[___]][__]:>Name,LTensor[Name_[___]]:>Name})))LSequenceType[c,b,a];*)


(* ::Input::Initialization:: *)
(*TSSort[LSequenceType_[TensorSequence__]]/;And[TensorSortable[LSequenceType[TensorSequence]],Not[SameQ[Reverse[{TensorSequence}],{TensorSequence}]]]:=(Times@@(SortProperties[#]&/@(Select[{TensorSequence},!FreeQ[#,LTensor]&]/.{LTensor[Name_[___]][__]:>Name,LTensor[Name_[___]]:>Name})))LSequenceType[Sequence@@Reverse[{TensorSequence}]];*)


(* ::Input::Initialization:: *)
(*{#1,b,#2}&@@Sort[{a,c}]*)


(* ::Input::Initialization:: *)
(*TS[LTensor[S[]][LI[\[Beta][1]]],LTensor[S[]],LVector[k[1]]]//TensorSortable
TS[LTensor[S[]][LI[\[Beta][1]]],LTensor[S[]],LVector[k[1]]]//TSSort
TS[LVector[k[1]],LTensor[S[]],LTensor[S[]][LI[\[Beta][1]]]]//TensorSortable
TS[LVector[k[1]],LTensor[S[]],LTensor[S[]][LI[\[Beta][1]]]]//TSSort*)


(* ::Input::Initialization:: *)
(*TS[LVector[k[2]],LTensor[S[]],LVector[k[1]]]//TensorSortable
TS[LVector[k[2]],LTensor[S[]],LVector[k[1]]]//TSSort
TS[LVector[k[1]],LTensor[S[]],LVector[k[2]]]//TensorSortable
TS[LVector[k[1]],LTensor[S[]],LVector[k[2]]]//TSSort*)


(* ::Input::Initialization:: *)
(*TS[LTensor[h[]][LI[\[Mu]]],LTensor[S[]],LVector[k[1]]]//TensorSortable
TS[LTensor[h[]][LI[\[Mu]]],LTensor[S[]],LVector[k[1]]]//TSSort
TS[LVector[k[1]],LTensor[S[]],LTensor[h[]][LI[\[Mu]]]]//TensorSortable
TS[LVector[k[1]],LTensor[S[]],LTensor[h[]][LI[\[Mu]]]]//TSSort*)


(* ::Subsection::Initialization::Closed:: *)
(*Levi-Civita tensor*)


(* ::Input::Initialization:: *)
ReduceEps=False;


(* ::Input::Initialization:: *)
Eps::ContractionWithTensor="Contraction with tensor `1` is ambiguous. Assuming contraction with first tensor index.";


(* ::Input::Initialization:: *)
Options[Eps]={"LObjectType"->LCTensor};
LObjectName[Eps[__]]:=Eps;
Eps[a___,x_,b___,x_,c___]:=0;
Eps[a___,LI[x_],b___,LI[y_],c___]/;ContractedQ[LI[x],LI[y]]:=0;
Eps[a___,b_Plus,d___]:=Plus@@(Eps[a,#,d]&/@(List@@b));
Eps[a___,Times[b_ ,c__],d___]/;Not[DeclaredQ[b]]:=b Eps[a,c,d];
Eps[___,0,___]:=0;


(* ::Input::Initialization:: *)
EpsSort[a_+b_]:=EpsSort[a]+EpsSort[b];
EpsSort[a_ b_]:=EpsSort[a]EpsSort[b];
EpsSort[a_]:=If[Head[a]===Eps,Signature[a]Eps[Sequence@@Sort[List@@a]],a];


(* ::Subsection::Initialization::Closed:: *)
(*Contract*)


(* ::Input::Initialization:: *)
LContract[a_Plus]:=Plus@@(LContract/@(List@@a));
LContract[Power[a_,p_]]:=Power[LContract[a],p];
LContract[expr_Times]/;Not[FreeQ[expr,Power]]:=Module[{nonpower,powers,collectPowers},
nonpower=expr/.Power[_,_]:>1;
powers=DeleteDuplicates[Cases[expr,Power[_,_](*,\[Infinity]*)]/.Power[_,x_]:>x];
collectPowers=Table[Times@@(Cases[expr,Power[_,powers[[j]]]]/.Power[x_,_]:>x),{j,1,Length[powers]}];
LContract[nonpower]Times@@MapThread[Power[#1,#2]&,{LContract/@collectPowers,powers}]
];
LContract[a___ TS[LI[x_],Tensor_,b__]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[LI[z],Tensor,b]];
LContract[a___ TS[LI[x_],Tensor_,b__]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[z]]:=LContract[a TS[LI[y],Tensor,b]];
LContract[a___ TS[b__,Tensor_,LI[x_]]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[b,Tensor,LI[z]]];
LContract[a___ TS[b__,Tensor_,LI[x_]]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[z]]:=LContract[a TS[b,Tensor,LI[y]]];
LContract[a___ TS[b___,Tensor_[coord__,LI[x_],rest___],c___]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[b,Tensor[coord,LI[z],rest],c]];
LContract[a___ TS[b___,Tensor_[coord__,LI[x_],rest___],c___]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[z]]:=LContract[a TS[b,Tensor[coord,LI[y],rest],c]];
LContract[a___ Eps[b___,LI[x_],c___]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a Eps[b,LI[z],c]];
LContract[a___ Eps[b___,LI[x_],c___]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[z]]:=LContract[a Eps[b,LI[y],c]];
LContract[a___ TS[LI[x_],Vector1_]TS[LI[y_],Vector2_]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[Vector1,Vector2]];
LContract[a___ TS[Vector1_,LI[x_]]TS[LI[y_],Vector2_]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[Vector1,Vector2]];
LContract[a___ TS[Vector1_,LI[x_]]TS[Vector2_,LI[y_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[Vector1,Vector2]];
LContract[a___ TS[LI[c_],Tensor_,LI[x_]]TS[LI[y_],Vector_]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[LI[c],Tensor,Vector]];
LContract[a___ TS[LI[x_],Tensor_,LI[c_]]TS[LI[y_],Vector_]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[Vector,Tensor,LI[c]]];
LContract[a___ TS[c_,Tensor_,LI[x_]]TS[Vector_,LI[y_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[c,Tensor,Vector]];
LContract[a___ TS[LI[x_],Tensor_,c_]TS[Vector_,LI[y_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[Vector,Tensor,c]];
LContract[a___ TS[LI[c1_],Tensor1_,LI[x_]]TS[LI[y_],Tensor2_,LI[c2_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[LI[c1],Tensor1,Tensor2,LI[c2]]];
LContract[a___ TS[LI[c1_],Tensor1_,LI[x_]]TS[LI[c2_],Tensor2_,LI[y_]]]/;And[ContractedQ[LI[x],LI[y]],NumericQ[SortProperties[LObjectName[Tensor2]]]]:=SortProperties[LObjectName[Tensor2]]LContract[a TS[LI[c1],Tensor1,Tensor2,LI[c2]]];
LContract[a___ TS[LI[x_],Tensor1_,LI[c1_]]TS[LI[y_],Tensor2_,LI[c2_]]]/;And[ContractedQ[LI[x],LI[y]],NumericQ[SortProperties[LObjectName[Tensor1]]]]:=SortProperties[LObjectName[Tensor1]]LContract[a TS[LI[c1],Tensor1,Tensor2,LI[c2]]];
LContract[a___ TS[LI[y_],Vector_]TS[LI[x_],Tensor_,b__]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[Vector,Tensor,b]];
LContract[a___ TS[LI[y_],Vector_]TS[b__,Tensor_,LI[x_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[b,Tensor,Vector]];
LContract[a___ TS[LI[y_],Tensor2_,c__]TS[b__,Tensor1_,LI[x_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[b,Tensor1,Tensor2,c]];
LContract[a___ TS[LI[c_],Tensor2_,LI[y_]]TS[b__,Tensor1_,LI[x_]]]/;And[ContractedQ[LI[x],LI[y]],NumericQ[SortProperties[LObjectName[Tensor2]]]]:=SortProperties[LObjectName[Tensor2]]LContract[a TS[b,Tensor1,Tensor2,LI[c]]];
LContract[a___ TS[LI[c_],Tensor2_,LI[y_]]TS[LI[x_],Tensor1_,b__]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[LI[c],Tensor2,Tensor1,b]];
LContract[a___ TS[LI[y_],Tensor2_,LI[c_]]TS[LI[x_],Tensor1_,b__]]/;And[ContractedQ[LI[x],LI[y]],NumericQ[SortProperties[LObjectName[Tensor2]]]]:=SortProperties[LObjectName[Tensor2]]LContract[a TS[LI[c],Tensor2,Tensor1,b]];
LContract[a___ TS[c__,Tensor1_,LI[x_]]TS[LI[y_],Tensor2_,b__]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TS[c,Tensor1,Tensor2,b]];
LContract[a___ TS[LI[x_],Tensor1_,c_]TS[LI[y_],Tensor2_,b__]]/;And[ContractedQ[LI[x],LI[y]],LVectorQ[c],NumericQ[SortProperties[LObjectName[Tensor1]]]]:=SortProperties[LObjectName[Tensor1]]LContract[a TS[c,Tensor1,Tensor2,b]];
LContract[a___ TS[c__,Tensor1_,LI[x_]]TS[b_,Tensor2_,LI[y_]]]/;And[ContractedQ[LI[x],LI[y]],LVectorQ[b],NumericQ[SortProperties[LObjectName[Tensor2]]]]:=SortProperties[LObjectName[Tensor2]]LContract[a TS[c,Tensor1,Tensor2,b]];
LContract[a___ TS[LI[x_],Tensors1__,x2_]TS[LI[y_],Tensors2__,y2_]]/;And[ContractedQ[LI[x],LI[y]],And@@(NumericQ[SortProperties[#]]&/@{Tensors1})]:=(Times@@(SortProperties[#]&/@{Tensors1}))LContract[a TS[x2,Sequence@@Reverse[{Tensors1}],Tensors2,y2]];
LContract[a___ TS[x2_,Tensors1__,LI[x_]]TS[y2_,Tensors2__,LI[y_]]]/;And[ContractedQ[LI[x],LI[y]],And@@(NumericQ[SortProperties[#]]&/@{Tensors2})]:=(Times@@(SortProperties[#]&/@{Tensors2}))LContract[a TS[x2,Tensors1,Sequence@@Reverse[{Tensors2}],y2]];
LContract[x___ Eps[a___,LI[\[Mu]_],b___]TS[LI[\[Nu]_],Vector_]]/;ContractedQ[LI[\[Mu]],LI[\[Nu]]]:=LContract[x Eps[a,Vector,b]];
LContract[x___ Eps[a___,LI[\[Mu]_],b___]TS[Vector_,LI[\[Nu]_]]]/;ContractedQ[LI[\[Mu]],LI[\[Nu]]]:=LContract[x Eps[a,Vector,b]];
LContract[a___ Eps[\[Mu]1_,\[Nu]1_,\[Alpha]1_,\[Beta]1_]Eps[\[Mu]2_,\[Nu]2_,\[Alpha]2_,\[Beta]2_]]/;ReduceEps:=LContract[Expand[a(-Signature[{\[Mu]2,\[Nu]2,\[Alpha]2,\[Beta]2}]Plus@@(Signature[#]TS[\[Mu]1,#[[1]]]TS[\[Nu]1,#[[2]]]TS[\[Alpha]1,#[[3]]]TS[\[Beta]1,#[[4]]]&)/@Permutations[{\[Mu]2,\[Nu]2,\[Alpha]2,\[Beta]2}])]];
LContract[a___ TT[a1___,Tensor_[Coord__,LI[x_],Rest___],b___]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[y]]:=LContract[a TT[a1,Tensor[Coord,LI[z],Rest],b]];LContract[a___ TT[a1___,Tensor_[Coord__,LI[x_],Rest___],b___]TS[LI[y_],LI[z_]]]/;ContractedQ[LI[x],LI[z]]:=LContract[a TT[a1,Tensor[Coord,LI[y],Rest],b]];
LContract[expr_]:=expr;


(* ::Section::Closed:: *)
(*End package*)


End[];


EndPackage[];
