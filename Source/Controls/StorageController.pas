unit StorageController;
{
  ----------------------------------------------------------
  Copyright (c) 2009, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  A StorageController is a control element that is connected to a terminal of another
  circuit element and sends dispatch  signals to a set of energy storage it controls

  A StorageController is defined by a New command:

  New StorageController.Name=myname Element=devclass.name terminal=[ 1|2|...] Elementlist = (elem1  elem2 ...)

 
}

INTERFACE

USES
     Command, ControlClass, ControlElem, CktElement, DSSClass, Arraydef, ucomplex,
     utilities, PointerList, Classes;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageController = class(TControlClass)
     private

     protected
        PROCEDURE DefineProperties;
        FUNCTION MakeLike(const StorageControllerName:String):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       FUNCTION Edit:Integer; override;     // uses global parser
       FUNCTION NewObject(const ObjName:String):Integer; override;

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TStorageControllerObj = class(TControlElem)
     private

            FkWLimit,
            FkWBand,
            HalfkWBand,
            FkvarLimit,
            TotalWeight   :Double;
            FListSize:Integer;
            FStorageNameList:TStringList;
            FStorePointerList:PointerList.TPointerList;
            FWeights:pDoubleArray;

            MonitoredElement :TDSSCktElement;

     public

       constructor Create(ParClass:TDSSClass; const StorageControllerName:String);
       destructor Destroy; override;

       PROCEDURE RecalcElementData; Override;
       PROCEDURE CalcYPrim; Override;    // Always Zero for a StorageController

       PROCEDURE Sample;  Override;    // Sample control quantities and set action times in Control Queue
       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state

       PROCEDURE GetCurrents(Curr: pComplexArray); Override; // Get present value of terminal Curr
       PROCEDURE GetInjCurrents(Curr: pComplexArray); Override;   // Returns Injextion currents

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

       FUNCTION MakeStorageList:Boolean;
   end;


VAR
    ActiveStorageControllerObj:TStorageControllerObj;

{--------------------------------------------------------------------------}
IMPLEMENTATION

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit,  Storage, Sysutils, uCmatrix, MathUtil, Math;

CONST

    NumPropsThisClass = 6;


{--------------------------------------------------------------------------}
constructor TStorageController.Create;  // Creates superstructure for all StorageController objects
Begin
     Inherited Create;

     Class_name   := 'StorageController';
     DSSClassType := DSSClassType + STORAGE_CONTROL;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TStorageController.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PROCEDURE TStorageController.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names

     PropertyName[1] := 'Element';
     PropertyName[2] := 'Terminal';
     PropertyName[3] := 'kWLimit';
     PropertyName[4] := 'kWBand';
     PropertyName[5] := 'kvarlimit';
     PropertyName[6] := 'ElementList';
     PropertyName[7] := 'Weights';

     PropertyHelp[1] := 'Full object name of the circuit element, typically a line or transformer, '+
                        'which the control is monitoring. There is no default; must be specified.';
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the StorageController control is connected. '+
                        '1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.';
     PropertyHelp[3] := 'kW Limit (directional) for the monitored element. The storage elements are dispatched to try to hold the power in band '+
                        'at least until the storage is depleted.';
     PropertyHelp[4] := 'Bandwidth (kW) of the dead band around the target limit.' +
                        'No dispatch changes are attempted if the power in the monitored terminal stays within this band.';
     PropertyHelp[5] := 'Max kvar to be delivered through the element.  Uses same dead band as kW.';
     PropertyHelp[6] := 'Array list of Storage elements to be controlled.  If not specified, all storage elements in the circuit are assumed dispatched by this controller.';
     PropertyHelp[7] := 'Array of proportional weights corresponding to each storage element in the ElementList.' +
                        ' The needed kW to get back to center band is dispatched to each storage element according to these weights. ' +
                        'Default is to set all weights to 1.0.';

     ActiveProperty  := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
FUNCTION TStorageController.NewObject(const ObjName:String):Integer;
Begin
    // Make a new StorageController and add it to StorageController class list
    WITH ActiveCircuit Do
    Begin
      ActiveCktElement := TStorageControllerObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

{--------------------------------------------------------------------------}
FUNCTION TStorageController.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   i:Integer;

Begin

  // continue parsing with contents of Parser
  ActiveStorageControllerObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveStorageControllerObj;

  Result := 0;

  WITH ActiveStorageControllerObj Do Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 Do Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 364);
            1: ElementName     := lowercase(param);
            2: ElementTerminal := Parser.IntValue;
            3: FkWLimit        := Parser.DblValue;
            4: FkWBand         := Parser.DblValue;
            5: FkvarLimit      := Parser.DblValue;
            6: InterpretTStringListArray(Param, FStorageNameList);
            7: Begin
                 FListSize := FStorageNameList.count;
                 IF FListSize>0 Then Begin
                 Reallocmem(FWeights, Sizeof(FWeights^[1])*FListSize);
                 InterpretDblArray(Param, FListSize, FWeights);
                 End;
               End;

         ELSE
           // Inherited parameters
           ClassEdit( ActiveStorageControllerObj, ParamPointer - NumPropsthisClass)
         End;

         CASE ParamPointer OF
            4: HalfkWBand := FkWBand / 2.0;
            6: Begin   // levelize the list
                 FStorePointerList.Clear;  // clear this for resetting on first sample
                 FListSize := FStorageNameList.count;
                 Reallocmem(FWeights, Sizeof(FWeights^[1])*FListSize);
                 For i := 1 to FListSize Do FWeights^[i] := 1.0;
               End;
         ELSE

         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     RecalcElementData;
  End;

End;



{--------------------------------------------------------------------------}
FUNCTION TStorageController.MakeLike(const StorageControllerName:String):Integer;
VAR
   OtherStorageController:TStorageControllerObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this StorageController name in the present collection}
   OtherStorageController := Find(StorageControllerName);
   IF OtherStorageController<>Nil THEN
   WITH ActiveStorageControllerObj Do Begin

        NPhases := OtherStorageController.Fnphases;
        NConds  := OtherStorageController.Fnconds; // Force Reallocation of terminal stuff

        ElementName       := OtherStorageController.ElementName;
        ControlledElement := OtherStorageController.ControlledElement;  // Pointer to target circuit element
        MonitoredElement  := OtherStorageController.MonitoredElement;  // Pointer to target circuit element

        ElementTerminal   := OtherStorageController.ElementTerminal;


        For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherStorageController.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in StorageController MakeLike: "' + StorageControllerName + '" Not Found.', 370);

End;




{==========================================================================}
{                    TStorageControllerObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TStorageControllerObj.Create(ParClass:TDSSClass; const StorageControllerName:String);

Begin
     Inherited Create(ParClass);
     Name := LowerCase(StorageControllerName);
     DSSObjType := ParClass.DSSClassType;

     NPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class



     ElementName   := '';
     ControlledElement := nil;  // not used in this control
     ElementTerminal  := 1;
     MonitoredElement := Nil;

     FStorageNameList := TSTringList.Create;
     FWeights   := Nil;
     FStorePointerList := PointerList.TPointerList.Create(20);  // Default size and increment
     FListSize   := 0;
     FkWLimit    := 8000.0;
     FkWBand     := 100.0;
     TotalWeight := 1.0;
     HalfkWBand  := FkWBand/2.0;
     InitPropertyValues(0);
     FkvarLimit  := FkWLimit/2.0;


   //  RecalcElementData;

End;

destructor TStorageControllerObj.Destroy;
Begin
     ElementName := '';
     Inherited Destroy;
End;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.RecalcElementData;

VAR
   DevIndex :Integer;

Begin


{Check for existence of monitored element}

         Devindex := GetCktElementIndex(ElementName); // Global function
         IF   DevIndex>0  THEN Begin
             MonitoredElement := ActiveCircuit.CktElements.Get(DevIndex);
             IF ElementTerminal > MonitoredElement.Nterms
             THEN Begin
                 DoErrorMsg('StorageController: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Re-specify terminal no.', 371);
             End
             ELSE Begin
               // Sets name of i-th terminal's connected bus in StorageController's buslist
                 Setbus(1, MonitoredElement.GetBus(ElementTerminal));
             End;
         End
         ELSE DoSimpleMsg('Monitored Element in StorageController.'+Name+ ' does not exist:"'+ElementName+'"', 372);


End;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.CalcYPrim;
Begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
End;






{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.GetCurrents(Curr: pComplexArray);
VAR
   i:Integer;
Begin

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

PROCEDURE TStorageControllerObj.GetInjCurrents(Curr: pComplexArray);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds Do Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    WITH ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    If Complete THEN
    Begin
      Writeln(F);
    End;

End;


{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.DoPendingAction;
begin

        {Do Nothing}
end;

{--------------------------------------------------------------------------}
PROCEDURE TStorageControllerObj.Sample;

VAR
   i           :Integer;
   PDiff,
   QDiff       :Double;
   S           :Complex ;
   StorageObj         :TSTorageObj;
   StorekWChanged, StorekvarChanged: Boolean;
   GenkW, Genkvar :Double;

begin
     // If list is not define, go make one from all generators in circuit
     IF FStorePointerList.ListSize=0 Then  MakeStorageList;

     If FListSize>0 Then Begin

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
       S := MonitoredElement.Power[ElementTerminal];  // Power in active terminal

       PDiff := S.re * 0.001 - FkWLimit;

       QDiff := S.im * 0.001 - FkvarLimit;

       // Redispatch the vars.

       StorekWChanged := FALSE;
       StorekvarChanged := FALSE;

       If Abs(PDiff) > HalfkWBand Then Begin // Redispatch Generators
          // PDiff is kW needed to get back into band
          For i := 1 to FListSize Do Begin
              StorageObj := FStorePointerList.Get(i);
              // compute new dispatch value for this generator ...
              GenkW := Max(1.0, (StorageObj.PresentkW + PDiff *(FWeights^[i]/TotalWeight)));
              If GenkW <> StorageObj.PresentkW Then Begin
                  StorageObj.PresentkW := GenkW;  //****  Will this work?  Maybe
                  StorekWChanged := TRUE;
              End;
          End;
       End;

       If Abs(QDiff) > HalfkWBand Then Begin // Redispatch Storage elents
          // QDiff is kvar needed to get back into band
          For i := 1 to FListSize Do Begin
              StorageObj := FStorePointerList.Get(i);
              // compute new dispatch value for this generator ...
              Genkvar := Max(0.0, (StorageObj.Presentkvar + QDiff *(FWeights^[i]/TotalWeight)));
              If Genkvar <> StorageObj.Presentkvar Then Begin
                  StorageObj.kvarBase := Genkvar;
                  StorekvarChanged := TRUE;
              End;
          End;
       End;

       If StorekWChanged or StorekvarChanged Then  // Only push onto controlqueue if there has been a change
          With ActiveCircuit, ActiveCircuit.Solution Do Begin
            LoadsNeedUpdating := TRUE; // Force recalc of power parms
            // Push present time onto control queue to force re solve at new dispatch value
            ControlQueue.Push(intHour, DynaVars.t, 0, 0, Self);
          End;
       

       {Else just continue}
    End;


end;


procedure TStorageControllerObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1]  := '';   //'element';
     PropertyValue[2]  := '1';   //'terminal';
     PropertyValue[3]  := '8000';
     PropertyValue[4]  := '100';
     PropertyValue[5]  := '0';
     PropertyValue[6]  := '';
     PropertyValue[7]  := '';



  inherited  InitPropertyValues(NumPropsThisClass);

end;

Function TStorageControllerObj.MakeStorageList:Boolean;

VAR
   StorageObj:TStorageObj;
   i:Integer;

begin

   Result := FALSE;

   If FListSize>0 Then Begin    // Name list is defined - Use it

     For i := 1 to FListSize Do Begin
         StorageObj := StorageClass.Find(FStorageNameList.Strings[i-1]);
         If Assigned(StorageObj) and StorageObj.Enabled Then FStorePointerList.New := StorageObj;
     End;

   End
   Else Begin
     {Search through the entire circuit for enabled generators and add them to the list}
     
     For i := 1 to GenClass.ElementCount Do Begin
        StorageObj :=  GenClass.ElementList.Get(i);
        If StorageObj.Enabled Then FStorePointerList.New := StorageObj;
     End;

     {Allocate uniform weights}
     FListSize := FStorePointerList.ListSize;
     Reallocmem(FWeights, Sizeof(FWeights^[1])*FListSize);
     For i := 1 to FListSize Do FWeights^[i] := 1.0;

   End;

   // Add up total weights
   TotalWeight := 0.0;
   For i := 1 to FlistSize Do  TotalWeight := TotalWeight + FWeights^[i];

   If FStorePointerList.ListSize>0 Then Result := TRUE;
end;



procedure TStorageControllerObj.Reset;
begin
  // inherited;

end;



INITIALIZATION




end.
