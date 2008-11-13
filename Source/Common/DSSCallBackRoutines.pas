unit DSSCallBackRoutines;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{$INCLUDE DSSCallBackStructDef.pas}


VAR
   CallBackRoutines :TDSSCallBacks;

PROCEDURE DoSimpleMsgCallback(S:pchar; maxlen:Cardinal); StdCall; // Call back for user-written models

implementation

Uses  ParserDel, DSSGlobals, Executive, SysUtils, CktElement;

Var
   CallBackParser  :TParser;
   CB_ParamName,
   CB_Param        :String;


{====================================================================================================================}

PROCEDURE DoSimpleMsgCallback(S:pchar; maxlen:Cardinal); StdCall; // Call back for user-written models

Begin
     DoSimpleMsg(s, 9000);
End;

   {These routines should work well with Fortran as well as C and VB}

{====================================================================================================================}

Procedure ParserLoad(S:Pchar; Maxlen:Cardinal); StdCall;

Begin
    CallBackParser.CmdString := S;
End;

{====================================================================================================================}

Procedure ParserIntValue(var i:Integer); StdCall;

Begin
    With CallBackParser Do Begin
      i := IntValue ;
    End;
End;


{====================================================================================================================}

Procedure ParserDblValue(var x:Double); StdCall;

Begin
    With CallBackParser Do Begin
      x := DblValue ;
    End;
End;

{====================================================================================================================}

Procedure ParserStrValue(s:pChar; Maxlen:Cardinal); StdCall;

{Copies null-terminated string into location pointed to by S up to the max chars specified}

Begin
    With CallBackParser Do Begin
      StrlCopy(s, pchar(CB_Param), Maxlen) ;
    End;
End;


{====================================================================================================================}

Function ParserNextParam(ParamName:pchar; Maxlen:Cardinal):Integer;Stdcall;
Begin
   With CallBackParser Do Begin
     CB_ParamName  := NextParam ;
     CB_Param      := StrValue;
   End;
   StrlCopy(ParamName, pchar(CB_ParamName), Maxlen) ;
   Result := Length(CB_Param);
End;

{====================================================================================================================}

Procedure DoDSSCommandCallBack(S:pchar; Maxlen:Cardinal); StdCall;
Begin
  SolutionAbort        := FALSE;
  DSSExecutive.Command := S;
End;

{====================================================================================================================}

Procedure GetActiveElementBusNamesCallBack(Name1:pchar; Len1:Cardinal; Name2:pchar; Len2:Cardinal); StdCall;
  {Get first two bus names of active Circuit Element for labeling graphs, etc.}
  {Coordinate must be defined else returns null string}
Var
   CktElement :TDSSCktElement;
   BusIdx     :Integer;
Begin
   StrlCopy(Name1, pchar(''), Len1) ;  // Initialize to null
   StrlCopy(Name2, pchar(''), Len2) ;
   If ActiveCircuit <> Nil Then Begin
     CktElement :=  ActiveCircuit.Activecktelement ;
     If CktElement <> Nil Then Begin
     {First bus}
       BusIdx := CktElement.Terminals^[1].busref;
       If BusIdx > 0 Then With  ActiveCircuit.Buses^[BusIdx]  Do
         If CoordDefined Then StrlCopy(Name1, pchar(ActiveCircuit.BusList.Get(Busidx)), Len1) ;
      {Second bus}
       BusIdx := CktElement.Terminals^[2].busref;
       If BusIdx > 0 Then With  ActiveCircuit.Buses^[BusIdx] do
         If CoordDefined Then StrlCopy(Name2, pchar(ActiveCircuit.BusList.Get(Busidx)), Len2) ;
      End; {If CktElement}
   End;  {If ActiveCircuit}
End;

{====================================================================================================================}

Function GetActiveElementIndexCallBack: Integer;  StdCall;
    {Usually just checking to see if this result >0}
Begin
   Result := 0;
   If Assigned(ActiveCircuit) Then
    If Assigned(ActiveCircuit.ActiveCktElement) Then
     Result := ActiveCircuit.ActiveCktElement.ClassIndex;
End;

{====================================================================================================================}

Function IsActiveElementEnabledCallBack: Boolean; StdCall;

Begin
    Result := False;
   If Assigned(ActiveCircuit) Then
    If Assigned(ActiveCircuit.ActiveCktElement) Then
     Result := ActiveCircuit.ActiveCktElement.Enabled;
End;

{====================================================================================================================}

Initialization

{Initialize Function Interface variables for user-Written Callbacks}

   With CallBackRoutines Do begin
      MsgCallBack := DoSimpleMsgCallback; // for user-written callbacks
      GetIntValue := ParserIntValue;
      GetDblValue := ParserDblValue;
      GetStrValue := ParserStrValue;
      LoadParser  := ParserLoad;
      NextParam   := ParserNextParam;
      DoDSSCommand := DoDSSCommandCallBack;
      GetActiveElementBusNames := GetActiveElementBusNamesCallBack;
      GetActiveElementIndex    := GetActiveElementIndexCallBack;
      IsActiveElementEnabled   := IsActiveElementEnabledCallBack;
  End;

  CallBackParser  := TParser.Create;

{====================================================================================================================}

Finalization

  CallBackParser.Free;

end.
