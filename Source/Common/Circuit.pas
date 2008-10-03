unit Circuit;

{
 Change Log
   10-12-99 Added DuplicatesAllowed and ZonesLocked
   10-24-99 Added Losses Property
   12-15-99 Added Default load shapes and generator dispatch reference
   4-17=00  Add Loads List
   5-30-00  Added Positive Sequence Flag
   8-24-00  Added PriceCurve stuff
   8-1-01  Modified Compute Capacity to report up to loadmult=1
}

{$WARN UNIT_PLATFORM OFF}

interface

USES
     Solution, SysUtils, ArrayDef, HashList, PointerList, CktElement,
     DSSClass, DSSObject, Bus, LoadShape, ControlQueue, uComplex, AutoAdd, EnergyMeter;


TYPE
    TReductionStrategy = (rsDefault, rsStubs, rsTapEnds, rsMergeParallel, rsBreakLoop, rsDangling, rsSwitches);

    CktElementDef = RECORD
        CktElementClass:Integer;                      
        devHandle:Integer;
    END;

    pCktElementDefArray = ^CktElementDefArray;
    CktElementDefArray = Array[1..1] of CktElementDef;

    TCircuit = CLASS(Tobject)

      Private
          FName:String;
          NodeBuffer:pIntegerArray;
          NodeBufferMax:Integer;
          FBusNameRedefined:Boolean;
          FActiveCktElement :TCktElement;

          // Temp arrays for when the bus swap takes place
          SavedBuses: pTBusArray;
          SavedBusNames :pStringArray;
          SavedNumBuses :Integer;

          FLoadMultiplier:Double;  // global multiplier for every load

          AbortBusProcess:Boolean;

          Procedure AddDeviceHandle(Handle:Integer);
          Procedure AddABus;
          Procedure AddANodeBus;
          Function  AddBus(const BusName:String; NNodes:Integer):Integer;
          Procedure Set_ActiveCktElement(Value:TcktElement);
          Procedure Set_BusNameRedefined(Value:Boolean);
          Procedure DoResetMeterZones;
          Function Get_Losses:Complex; //Total Circuit losses
          Procedure Set_LoadMultiplier(Value :Double);
          Procedure SaveBusInfo;
          Procedure RestoreBusInfo;

          Function SaveMasterFile:Boolean;
          Function SaveDSSObjects:Boolean;
          Function SaveFeeders:Boolean;
          Function SaveBusCoords:Boolean;

          Procedure ReallocDeviceList;
          procedure Set_CaseName(const Value: String);


      Public
          FCaseName:String;

          ActiveBusIndex   :Integer;
          Fundamental   :Double;    // fundamental and default base frequency

          Control_BusNameRedefined  :Boolean;  // Flag for use by control elements to detect redefinition of buses

          BusList,
          AutoAddBusList,
          DeviceList       :THashList;
          DeviceRef  :pCktElementDefArray;  //Type and handle of device

          // lists of pointers to different elements by class
          Faults,
          CktElements,
          PDElements,
          PCElements,
          DSSControls,
          Sources,
          MeterElements,
          Monitors,
          EnergyMeters,
          Generators,
          Substations,  // not doing anything with this yet
          Transformers,
          CapControls,
          RegControls,
          Lines,
          Loads,
          ShuntCapacitors,
          Feeders        :TPointerList;

          ControlQueue:TControlQueue;

          Solution   :TSolutionObj;
          AutoAddObj :TAutoAdd;

          // For AutoAdd stuff
          UEWeight,
          LossWeight   :Double;

          NumUEregs,
          NumLossRegs  :Integer;
          Ueregs,
          LossRegs      :pIntegerArray;

          CapacityStart,
          CapacityIncrement:  Double;
                                   
          TrapezoidalIntegration,   // flag for trapezoidal integratio
          LogEvents  :Boolean;

          LoadDurCurve:String;
          LoadDurCurveObj:TLoadShapeObj;
          PriceCurve:String;
          PriceCurveObj:TLoadShapeObj;

          NumDevices, NumBuses, NumNodes:Integer;
          MaxDevices, MaxBuses, MaxNodes:Integer;
          IncDevices, IncBuses, IncNodes:Integer;
          
          // Bus and Node stuff
          Buses:    pTBusArray;
          MapNodeToBus:pTNodeBusArray;

          // Flags
          Issolved          :Boolean;
          DuplicatesAllowed :Boolean;
          ZonesLocked       :Boolean;
          MeterZonesComputed:Boolean;
          PositiveSequence  :Boolean;  // Model is to be interpreted as Pos seq

          // Voltage limits
          NormalMinVolts,
          NormalMaxVolts,
          EmergMaxVolts,
          EmergMinVolts     :Double;  //per unit voltage restraints for this circuit
          LegalVoltageBases :pDoubleArray;

          // Global circuit multipliers
          GeneratorDispatchReference,
          DefaultGrowthFactor,
          DefaultGrowthRate,
          GenMultiplier,   // global multiplier for every generator
          HarmMult   :Double;
          DefaultHourMult: Complex;

          PriceSignal:Double; // price signal for entire circuit

          // EnergyMeter Totals
          RegisterTotals:TRegisterArray;

          DefaultDailyShapeObj,
          DefaultYearlyShapeObj :TLoadShapeObj;

          CurrentDirectory   :String;

          ReductionStrategy:TReductionStrategy;
          ReductionMaxAngle, ReductionZmag:double;
          ReductionStrategyString:String;

          PctNormalFactor:Double;

          {Plot Markers}
          NodeMarkerCode,
          NodeMarkerWidth,
          SwitchMarkerCode:Integer;

          Constructor Create(const Name:String);
          Destructor Destroy; Override;

          Procedure AddCktElement(Handle:Integer);  // Adds last DSS object created to circuit

          Procedure TotalizeMeters;
          Function ComputeCapacity:Boolean;

          Function Save(Dir:String):Boolean;

          Procedure ProcessBusDefs;
          Procedure ReProcessBusDefs;
          Function  SetElementActive(Const FullObjectName:String):Integer;
          Procedure InvalidateAllPCElements;

          Procedure DebugDump(Var F:TextFile);

          property Name             :String   Read FName;
          Property CaseName         :String   Read FCaseName Write Set_CaseName;
          Property ActiveCktElement :TCktElement Read FActiveCktElement Write Set_ActiveCktElement;
          Property Losses           :Complex Read Get_Losses;  // Total Circuit PD Element losses
          Property BusNameRedefined :Boolean  Read FBusNameRedefined Write Set_BusNameRedefined;
          Property LoadMultiplier   :Double  Read FLoadMultiplier write Set_LoadMultiplier;

      End;

implementation

USES
     PDElement, CktElementClass,
     ParserDel,  DSSGlobals, Dynamics,
     Line, Transformer,  Vsource,
     Utilities, FileCtrl, DSSForms;

//----------------------------------------------------------------------------
Constructor TCircuit.Create(const Name:String);

// Var Retval:Integer;

BEGIN
     inherited Create;

     IsSolved := False;
     {*Retval   := *} SolutionClass.NewObject(Name);
     Solution := ActiveSolutionObj;

     FName       := LowerCase(Name);
     
     CaseName    := FName;  // Default case name to circuitname
                            // Sets CircuitName_

     Fundamental  := 60.0;
     ActiveCktElement := nil;
     ActiveBusIndex := 1;    // Always a bus

     // initial allocations increased from 100 to 1000 to speed things up

     MaxBuses   := 1000;  // good sized allocation to start
     MaxDevices := 1000;
     MaxNodes   := 3*MaxBuses;
     IncDevices := 1000;
     IncBuses   := 1000;
     IncNodes   := 3000;

     // Allocate some nominal sizes
     BusList    := THashList.Create(900);  // Bus name list Nominal size to start; gets reallocated
     DeviceList := THashList.Create(900);
     AutoAddBusList := THashList.Create(100);

     NumBuses   := 0;  // Eventually allocate a single source
     NumDevices := 0;
     NumNodes   := 0;

     Faults       := TPointerList.Create(2);
     CktElements  := TPointerList.Create(1000);
     PDElements   := TPointerList.Create(1000);
     PCElements   := TPointerList.Create(1000);
     DSSControls  := TPointerList.Create(10);
     Sources      := TPointerList.Create(10);
     MeterElements:= TPointerList.Create(20);
     Monitors     := TPointerList.Create(20);
     EnergyMeters := TPointerList.Create(5);
     Generators   := TPointerList.Create(5);
     Feeders   := TPointerList.Create(10);
     Substations  := TPointerList.Create(5);
     Transformers := TPointerList.Create(10);
     CapControls  := TPointerList.Create(10);
     RegControls  := TPointerList.Create(5);
     Lines        := TPointerList.Create(1000);
     Loads        := TPointerList.Create(1000);
     ShuntCapacitors :=  TPointerList.Create(20);

     Buses     := Allocmem(Sizeof(Buses^[1])     * Maxbuses);
     MapNodeToBus := Allocmem(Sizeof(MapNodeToBus^[1]) * MaxNodes);
     DeviceRef := AllocMem(SizeOf(DeviceRef^[1]) * MaxDevices);

     ControlQueue := TControlQueue.Create;

     LegalVoltageBases := AllocMem(SizeOf(LegalVoltageBases^[1]) * 8);
     // Default Voltage Bases
       LegalVoltageBases^[1] := 0.208;
       LegalVoltageBases^[2] := 0.480;
       LegalVoltageBases^[3] := 12.47;
       LegalVoltageBases^[4] := 24.9;
       LegalVoltageBases^[5] := 34.5;
       LegalVoltageBases^[6] := 115.0;
       LegalVoltageBases^[7] := 230.0;
       LegalVoltageBases^[8] := 0.0;  // terminates array

     NodeBufferMax := 20;
     NodeBuffer    := AllocMem(SizeOf(NodeBuffer^[1]) * NodeBufferMax); // A place to hold the nodes

     // Init global circuit load and harmonic source multipliers
     FLoadMultiplier := 1.0;
     GenMultiplier := 1.0;
     HarmMult := 1.0;

     PriceSignal := 25.0;   // $25/MWH

     // Factors for Autoadd stuff
     UEWeight   := 1.0;  // Default to weighting UE same as losses
     LossWeight := 1.0;
     NumUEregs  := 1;
     NumLossRegs := 1;
     UEregs   := nil;  // set to something so it wont break reallocmem
     LossRegs := nil;
     Reallocmem(UEregs, sizeof(UEregs^[1])*NumUEregs);
     Reallocmem(Lossregs, sizeof(Lossregs^[1])*NumLossregs);
     UEregs^[1]      := 10;   // Overload UE
     LossRegs^[1]    := 13;   // Zone Losses

     CapacityStart := 0.9;     // for Capacity search
     CapacityIncrement := 0.005;

     LoadDurCurve    := '';
     LoadDurCurveObj := nil;
     PriceCurve    := '';
     PriceCurveObj := nil;

     // Flags
     DuplicatesAllowed   := False;
     ZonesLocked         := False;   // Meter zones recomputed after each change
     MeterZonesComputed  := False;
     PositiveSequence    := False;

     NormalMinVolts := 0.95;
     NormalMaxVolts := 1.05;
     EmergMaxVolts  := 1.08;
     EmergMinVolts  := 0.90;

     NodeMarkerCode := 16;
     NodeMarkerWidth:= 1;
     SwitchMarkerCode := 5;

     
     TrapezoidalIntegration := FALSE;  // Default to Euler method
     LogEvents := FALSE;

     GeneratorDispatchReference := 0.0;
     DefaultGrowthRate := 1.025;
     DefaultGrowthFactor := 1.0;

     DefaultDailyShapeObj  := LoadShapeClass.Find('default');
     DefaultYearlyShapeObj := LoadShapeClass.Find('default');

     CurrentDirectory := '';

     BusNameRedefined := True;  // set to force rebuild of buslists, nodelists

     SavedBuses:= nil;
     SavedBusNames := nil;

     ReductionStrategy := rsDefault;
     ReductionMaxAngle := 15.0;
     ReductionZmag := 0.02;

   {Misc objects}
   AutoAddObj := TAutoAdd.Create;


END;

//----------------------------------------------------------------------------
Destructor  Tcircuit.Destroy;
VAR
    i:Integer;

BEGIN
     TRY
       For i := 1 to NumDevices Do TCktElement(CktElements.Get(i)).Free;

     EXCEPT
       ON E: Exception Do
         DoSimpleMsg('Exception Freeing Circuit Element:'  + CRLF + E.Message, 423);
     END;

     FOR i := 1 to NumBuses Do Buses^[i].Free;  // added 10-29-00

     Reallocmem(DeviceRef, 0);
     Reallocmem(Buses,     0);
     Reallocmem(MapNodeToBus, 0);
     Reallocmem(NodeBuffer, 0);
     Reallocmem(UEregs, 0);
     Reallocmem(Lossregs, 0);
     Reallocmem(LegalVoltageBases, 0);

     DeviceList.Free;
     BusList.Free;
     AutoAddBusList.Free;
     Solution.Free;
     PDElements.Free;
     PCElements.Free;
     DSSControls.Free;
     Sources.Free;
     Faults.Free;
     CktElements.Free;
     MeterElements.Free;
     Monitors.Free;
     EnergyMeters.Free;
     Generators.Free;
     Feeders.Free;
     Substations.Free;
     Transformers.Free;
     CapControls.Free;
     RegControls.Free;
     Loads.Free;
     Lines.Free;
     ShuntCapacitors.Free;

     ControlQueue.Free;

     AutoAddObj.Free;

     Inherited Destroy;
END;

//----------------------------------------------------------------------------
Procedure TCircuit.ProcessBusDefs;
VAR
   BusName:String;
   NNodes, NP,  Ncond, i, j, iTerm, RetVal:Integer;
   NodesOK:Boolean;

BEGIN
   WITH ActiveCktElement DO
     BEGIN
      np    := NPhases;
      Ncond := NConds;

      Parser.Token := FirstBus;     // use parser functions to decode
      FOR iTerm := 1 to Nterms DO
        BEGIN
           NodesOK := TRUE;
           // Assume normal phase rotation  for default
           FOR i := 1 to np DO NodeBuffer^[i] := i; // set up buffer with defaults

           // Default all other conductors to a ground connection
           // If user wants them ungrounded, must be specified explicitly!
           For i := np + 1 to NCond DO NodeBuffer^[i] := 0;

           // Parser will override bus connection if any specified
           BusName :=  Parser.ParseAsBusName(NNodes, NodeBuffer);

           // Check for error in node specification
           For j := 1 to NNodes Do
           Begin
               If NodeBuffer^[j] < 0 Then
               Begin
                   retval := DSSMessageDlg('Error in Node specification for Element: "'
                     +ParentClass.Name+'.'+Name+'"'+CRLF+
                     'Bus Spec: "'+Parser.Token+'"',FALSE);
                   NodesOK := FALSE;
                   If  retval=-1 Then Begin
                       AbortBusProcess := TRUE;
                       AppendGlobalresult('Aborted bus process.');
                       Exit
                   End;
                   Break;
               End;
           End;


           // Node -Terminal Connnections
           // Caution: Magic -- AddBus replaces values in nodeBuffer to correspond
           // with global node reference number.
           If NodesOK Then
           Begin
             ActiveTerminalIdx := iTerm;
             ActiveTerminal.BusRef := AddBus(BusName,   Ncond);
             SetNodeRef(iTerm, NodeBuffer);  // for active circuit
           End;
           Parser.Token := NextBus;
         END;
     END;
END;


//----------------------------------------------------------------------------
Procedure TCircuit.AddABus;
BEGIN
    If NumBuses > MaxBuses THEN BEGIN
        Inc(MaxBuses, IncBuses);
        ReallocMem(Buses, SizeOf(Buses^[1]) * MaxBuses);
    END;
END;

//----------------------------------------------------------------------------
Procedure TCircuit.AddANodeBus;
BEGIN
    If NumNodes > MaxNodes THEN BEGIN
        Inc(MaxNodes, IncNodes);
        ReallocMem(MapNodeToBus, SizeOf(MapNodeToBus^[1]) * MaxNodes);
    END;
END;

//----------------------------------------------------------------------------
Function TCircuit.AddBus(const BusName:String; NNodes:Integer):Integer;

VAR
   NodeRef, i :Integer;
BEGIN

// Trap error in bus name
    IF Length(BusName) = 0 THEN BEGIN  // Error in busname
       DoErrorMsg('TCircuit.AddBus', 'BusName for Object "' + ActiveCktElement.Name + '" is null.',
                  'Error in definition of object.', 424);
       For i := 1 to ActiveCktElement.NConds DO NodeBuffer^[i] := 0;
       Result := 0;
       Exit;
    END;

    Result := BusList.Find(BusName);
    If Result=0 THEN BEGIN
         Result := BusList.Add(BusName);    // Result is index of bus
         Inc(NumBuses);
         AddABus;   // Allocates more memory if necessary
         Buses^[NumBuses] := TBus.Create;
    END;

    {Define nodes belonging to the bus}
    {Replace Nodebuffer values with global reference number}
    WITH Buses^[Result] DO BEGIN
      FOR i := 1 to NNodes DO BEGIN
         NodeRef := Add(NodeBuffer^[i]);
         If NodeRef=NumNodes THEN BEGIN  // This was a new node so Add a NodeToBus element ????
             AddANodeBus;   // Allocates more memory if necessary
             MapNodeToBus^[NumNodes].BusRef  := Result;
             MapNodeToBus^[NumNodes].NodeNum := NodeBuffer^[i]
         END;
         NodeBuffer^[i] := NodeRef;  //  Swap out in preparation to setnoderef call
      END;
    END;
END;

//----------------------------------------------------------------------------
Procedure TCircuit.AddDeviceHandle(Handle:Integer);
BEGIN
    If NumDevices>MaxDevices THEN BEGIN
        MaxDevices := MaxDevices + IncDevices;
        ReallocMem(DeviceRef, Sizeof(DeviceRef^[1]) * MaxDevices);
    END;
    DeviceRef^[NumDevices].devHandle := Handle;    // Index into CktElements
    DeviceRef^[NumDevices].CktElementClass := LastClassReferenced;
END;


//----------------------------------------------------------------------------
Function TCircuit.SetElementActive(Const FullObjectName:String):Integer;

// Fast way to set a cktelement active
VAR
   Devindex      :Integer;
   DevClassIndex:Integer;
   DevType,
   DevName :String;

BEGIN

     Result := 0;

     ParseObjectClassandName(FullObjectName, DevType, DevName);
     DevClassIndex := ClassNames.Find(DevType);
     If DevClassIndex = 0 Then DevClassIndex := LastClassReferenced;
     Devindex := DeviceList.Find(DevName);
     WHILE DevIndex>0 DO BEGIN
         IF DeviceRef^[Devindex].CktElementClass=DevClassIndex THEN   // we got a match
          BEGIN
            ActiveDSSClass := DSSClassList.Get(DevClassIndex);
            Result := DeviceRef^[Devindex].devHandle;
           // ActiveDSSClass.Active := Result;
          //  ActiveCktElement := ActiveDSSClass.GetActiveObj;
            ActiveCktElement := CktElements.Get(Result);
            Break;
          END;
         Devindex := Devicelist.FindNext;   // Could be duplicates
     END;

     CmdResult := Result;

END;

//----------------------------------------------------------------------------
Procedure TCircuit.Set_ActiveCktElement(Value:TcktElement);
BEGIN
    FActiveCktElement := Value;
    ActiveDSSObject := Value;
END;

//----------------------------------------------------------------------------
Procedure TCircuit.AddCktElement(Handle:Integer);


BEGIN

   // Update lists that keep track of individual circuit elements
   Inc(NumDevices);

   // Resize DeviceList if no. of devices greatly exceeds allocation
   If NumDevices> 2*DeviceList.InitialAllocation Then ReAllocDeviceList;
   DeviceList.Add(ActiveCktElement.Name);
   CktElements.Add(ActiveCktElement);

   {Build Lists of PC and PD elements}
   CASE (ActiveCktElement.DSSObjType and BaseClassMask) OF
       PD_ELEMENT:   PDElements.Add(ActiveCktElement);
       PC_ELEMENT:   PCElements.Add(ActiveCktElement);
       CTRL_ELEMENT: DSSControls.Add(ActiveCktElement);
       METER_ELEMENT :MeterElements.Add(ActiveCktElement);
   Else
       {Nothing}
   End;

   {Build  lists of Special elements and generic types}
   CASE (ActiveCktElement.DSSObjType and CLASSMASK) OF
       MON_ELEMENT :Monitors.Add(ActiveCktElement);
       ENERGY_METER:EnergyMeters.Add(ActiveCktElement);
       GEN_ELEMENT :Generators.Add(ActiveCktElement);
       SOURCE      :Sources.Add(ActiveCktElement);
       CAP_CONTROL :CapControls.Add(ActiveCktElement);
       REG_CONTROL :RegControls.Add(ActiveCktElement);
       LOAD_ELEMENT  :Loads.Add(ActiveCktElement);
       CAP_ELEMENT: ShuntCapacitors.Add(ActiveCktElement);

       { Keep Lines, Transformer, and Lines and Faults in PDElements and separate lists
         so we can find them quickly.}
       XFMR_ELEMENT        :Transformers.Add(ActiveCktElement);
       LINE_ELEMENT:Lines.Add(ActiveCktElement);
       FAULTOBJECT :Faults.Add(ActiveCktElement);
       FEEDER_ELEMENT :Feeders.Add(ActiveCktElement);
   END;

  // AddDeviceHandle(Handle); // Keep Track of this device result is handle
  AddDeviceHandle(CktElements.ListSize); // Handle is global index into CktElements

END;

//----------------------------------------------------------------------------
Procedure TCircuit.DoResetMeterZones;

BEGIN

 { Do this only if meterzones unlocked .  Normally, Zones will remain unlocked
   so that all changes to the circuit will result in rebuilding the lists}
  If Not MeterZonesComputed or Not ZonesLocked Then
  Begin
     If LogEvents Then LogThisEvent('Resetting Meter Zones');
     EnergyMeterClass.ResetMeterZonesAll;
     MeterZonesComputed := True;
     If LogEvents Then LogThisEvent('Done Resetting Meter Zones');
  End;
  

END;

//----------------------------------------------------------------------------
Procedure TCircuit.SaveBusInfo;
Var
   i  :Integer;

Begin

{Save existing bus definitions and names for info that needs to be restored}
     SavedBuses    := Allocmem(Sizeof(SavedBuses^[1]) * NumBuses);
     SavedBusNames := Allocmem(Sizeof(SavedBusNames^[1]) * NumBuses);

     For i := 1 to NumBuses Do Begin
         SavedBuses^[i] := Buses^[i];
         SavedBusNames^[i] := BusList.get(i);
     End;
     SavedNumBuses := NumBuses;

End;

//----------------------------------------------------------------------------
Procedure TCircuit.RestoreBusInfo;

Var
   i,j,idx, jdx:Integer;
   pBus:TBus;

Begin

// Restore  kV bases, other values to buses still in the list
     For i := 1 to SavedNumBuses Do
       Begin
           idx := BusList.Find(SavedBusNames^[i]);
           If   idx <> 0 Then
             With Buses^[idx] Do Begin
               pBus    := SavedBuses^[i];
               kvBase  := pBus.kVBase;
               x       := pBus.x;
               Y       := pBus.y;
               CoordDefined := pBus.CoordDefined;
               Keep    := pBus.Keep;
               {Restore Voltages in new bus def that existed in old bus def}
               If assigned(pBus.VBus) Then Begin
                   For j := 1 to pBus.NumNodesThisBus Do Begin
                       jdx := FindIdx(pBus.GetNum(j));  // Find index in new bus for j-th node  in old bus
                       If jdx > 0 Then Vbus^[jdx] := pBus.VBus^[j];
                   End;
               End;
             End;
           SavedBusNames^[i] := ''; // De-allocate string
       End;

     If Assigned(SavedBuses) Then For i := 1 to SavedNumBuses Do SavedBuses^[i].Free;  // gets rid of old bus voltages, too

     ReallocMem(SavedBuses, 0);
     ReallocMem(SavedBusNames, 0);

End;

//----------------------------------------------------------------------------
Procedure TCircuit.ReProcessBusDefs;

// Redo all Buslists, nodelists

VAR
    CktElementSave :TCktElement;
    i:integer;

BEGIN
     If LogEvents Then LogThisEvent('Reprocessing Bus Definitions');

     AbortBusProcess := FALSE;
     SaveBusInfo;  // So we don't have to keep re-doing this
     // Keeps present definitions of bus objects until new ones created

     // get rid of old bus lists
     BusList.Free;  // Clears hash list of Bus names for adding more
     BusList := THashList.Create(NumDevices);  // won't have many more buses than this

     NumBuses := 0;  // Leave allocations same, but start count over
     NumNodes := 0;

     // Now redo all enabled circuit elements
     CktElementSave := ActiveCktElement;
     ActiveCktElement := CktElements.First;
     WHILE ActiveCktElement <> nil DO  BEGIN
       IF ActiveCktElement.Enabled THEN ProcessBusDefs;
       IF AbortBusProcess then Exit;
       ActiveCktElement := CktElements.Next;
     END;

     ActiveCktElement := CktElementSave;  // restore active circuit element

     FOR i := 1 to NumBuses Do Buses^[i].AllocateBusVoltages;
     FOR i := 1 to NumBuses Do Buses^[i].AllocateBusCurrents;

     RestoreBusInfo;     // frees old bus info, too
     DoResetMeterZones;  // Fix up meter zones to correspond

     BusNameRedefined := False;  // Get ready for next time
END;

//----------------------------------------------------------------------------
Procedure TCircuit.Set_BusNameRedefined(Value:Boolean);
BEGIN
    FBusNameRedefined := Value;

    IF Value THEN Begin
      Solution.SystemYChanged := True;  // Force Rebuilding of SystemY if bus def has changed
      Control_BusNameRedefined := True;  // So controls will know buses redefined
    End;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TCircuit.Get_Losses:Complex;

Var
   pdelem :TPDElement;
Begin

{Return total losses in all PD Elements}

        pdelem := PDElements.First;
        Result := cZERO;
        While pdelem <> nil Do
        Begin
            IF pdelem.enabled Then Begin
              {Ignore Shunt Elements}
              If Not pdElem.IsShunt Then Caccum(Result, pdelem.losses);
            End;
            pdelem := PDElements.Next;
        End;

End;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TCircuit.DebugDump(Var F:TextFile);

VAR
   i,j:Integer;

BEGIN

     Writeln(F, 'NumBuses= ', NumBuses:0);
     Writeln(F, 'NumNodes= ', NumNodes:0);
     Writeln(F, 'NumDevices= ', NumDevices:0);
     Writeln(F,'BusList:');
     For i := 1 to NumBuses Do BEGIN
       Write(F,'  ',Pad(BusList.Get(i),12));
       Write(F,' (', Buses^[i].NumNodesThisBus:0,' Nodes)');
       FOR j := 1 to Buses^[i].NumNodesThisBus Do Write(F,' ',Buses^[i].Getnum(j):0);
       Writeln(F);
     END;
     Writeln(F,'DeviceList:');
     For i := 1 to NumDevices Do BEGIN
        Write(F,'  ',Pad(DeviceList.Get(i),12));
        ActiveCktElement := CktElements.Get(i);
        If Not ActiveCktElement.Enabled THEN Write(F, '  DISABLED');
        Writeln(F);
     END ;
     Writeln(F,'NodeToBus Array:');
     For i := 1 to NumNodes DO BEGIN
       j :=  MapNodeToBus^[i].BusRef;
       Write(F,'  ',i:2,' ',j:2,' (=',BusList.Get(j),'.',MapNodeToBus^[i].NodeNum:0,')');
       Writeln(F);
     END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TCircuit.InvalidateAllPCElements;

VAR
   p:TCktElement;

BEGIN

   p := PCElements.First;
   WHILE (p <> nil)
   DO BEGIN
        p.YprimInvalid := True;
        p := PCElements.Next;
   END;

   Solution.SystemYChanged := True;  // Force rebuild of matrix on next solution

END;


// - - ------------------------------------------------------
Procedure TCircuit.Set_LoadMultiplier(Value :Double);

Begin

     If (Value <> FLoadMultiplier)
     Then   // We may have to change the Y matrix if the load multiplier  has changed
         Case Solution.LoadModel Of
              ADMITTANCE:  InvalidateAllPCElements
         Else
            {nada}
         End;

     FLoadMultiplier := Value;

End;

procedure TCircuit.TotalizeMeters;

{ Totalize all energymeters in the problem}

Var
    pEM:TEnergyMeterObj;
    i: Integer;

begin
      For i := 1 to NumEMRegisters Do RegisterTotals[i] := 0.;

      pEM := EnergyMeters.First;
      While pEM <> Nil Do With PEM Do Begin

          For i := 1 to NumEMRegisters Do RegisterTotals[i] := RegisterTotals[i] + Registers[i] * TotalsMask[i];

         pEM := EnergyMeters.Next;
      End;
end;

FUNCTION TCircuit.ComputeCapacity: Boolean;
Var
   CapacityFound :Boolean;

    FUNCTION SumSelectedRegisters(Const mtrRegisters:TRegisterArray; Regs: pIntegerArray;  count: Integer): Double;
    VAR
       i  :Integer;
    begin
         Result := 0.0;
         FOR i := 1 to count Do Begin
              Result := Result + mtrRegisters[regs^[i]];
         End;
    end;

begin
     Result := FALSE;
     If (EnergyMeters.ListSize = 0) Then Begin
       DoSimpleMsg('Cannot compute system capacity with EnergyMeter objects!', 430);
       Exit;
     End;

     If (NumUeRegs = 0) Then Begin
       DoSimpleMsg('Cannot compute system capacity with no UE resisters defined.  Use SET UEREGS=(...) command.', 431);
       Exit;
     End;

     Solution.Mode := SNAPSHOT;
     LoadMultiplier  := CapacityStart;
     CapacityFound := False;

     Repeat
          EnergyMeterClass.ResetAll;
          Solution.Solve;
          EnergyMeterClass.SampleAll;
          TotalizeMeters;

           // Check for non-zero in UEregs
           IF SumSelectedRegisters(RegisterTotals, UEregs, NumUEregs) <> 0.0 Then CapacityFound := True;
           // LoadMultiplier is a property ...
           IF Not CapacityFound Then LoadMultiplier := LoadMultiplier + CapacityIncrement;
     Until (LoadMultiplier > 1.0) or CapacityFound;
     If LoadMultiplier>1.0 Then LoadMultiplier := 1.0;
     Result := TRUE;
end;

Function TCircuit.Save(Dir:String):Boolean;
{Save the present circuit - Enabled devices only}

var
   i:Integer;
   Success:Boolean;
   CurrDir,SaveDir :String;

begin
   Result := FALSE;

// Make a new subfolder in the present folder based on the circuit name and
// a unique sequence number
   SaveDir :=  GetCurrentDir;  // remember where to come back to
   Success := FALSE;
   If Length(Dir)=0 Then Begin
     dir := Name;

     CurrDir := Dir;
     For i := 0 to 999 Do  // Find a unique dir name
      Begin
         If Not DirectoryExists(CurrDir) Then
          Begin
              If CreateDir(CurrDir) Then
               Begin
                  SetCurrentDir(CurrDir);
                  Success := TRUE;
                  Break;
               End;
          End;
         CurrDir :=  dir + Format('%.3d',[i]);
      End;
    End Else Begin
       If Not DirectoryExists(Dir) Then Begin
          CurrDir :=  dir;
          If CreateDir(CurrDir) Then
           Begin
              SetCurrentDir(CurrDir);
              Success := TRUE;
           End;
       End Else Begin  // Exists - overwrite
          CurrDir := Dir;
          SetCurrentDir(CurrDir);
          Success := TRUE;
       End;
    End;

    If Not Success Then
     Begin
       DoSimpleMsg('Could not create a folder "'+Dir+'" for saving the circuit.', 432);
       Exit;
     End;

    SavedFileList.Clear;  {This list keeps track of all files saved}

    // Initialize so we will know when we have saved the circuit elements
    For i := 1 to CktElements.ListSize Do TCktElement(CktElements.Get(i)).HasBeenSaved := False;

    // Initialize so we don't save a class twice
    For i := 1 to DSSClassList.ListSize Do TDssClass(DSSClassList.Get(i)).Saved := FALSE;

    {Ignore Feeder Class -- gets saved with Energymeters}
    FeederClass.Saved := TRUE;  // will think this class is already saved

    {Define voltage sources first}
    Success :=  WriteVsourceClassFile(GetDSSClassPtr('vsource'), TRUE);
    {Write library files so that they will be available to lines, loads, etc}
    {Use default filename=classname}
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('wiredata'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('linegeometry'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('linecode'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('loadshape'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('growthshape'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('TCC_Curve'),'', FALSE);
    If Success Then Success :=  WriteClassFile(GetDssClassPtr('Spectrum'),'', FALSE);
    If Success Then Success := SaveFeeders; // Save feeders first
    If Success Then Success := SaveDSSObjects;  // Save rest ot the objects
    If Success Then Success := SaveBusCoords;
    If Success Then Success := SaveMasterFile;



    If Success Then DoSimpleMsg('Circuit saved in directory: ' + GetCurrentDir, 433)
               Else DoSimpleMsg('Error attempting to save circuit in ' + GetCurrentDir, 434);
    // Return to Original directory
    SetCurrentDir(SaveDir);

    Result := TRUE;

end;

function TCircuit.SaveDSSObjects: Boolean;
Var

   Dss_Class:TDSSClass;
   i:integer;

begin
  Result := FALSE;

  // Write Files for all populated DSS Classes  Except Solution Class
  For i := 1 to DSSClassList.ListSize Do
   Begin
      Dss_Class := DSSClassList.Get(i);
      If (DSS_Class = SolutionClass) or Dss_Class.Saved Then Continue;   // Cycle to next
            {use default filename=classname}
      IF Not WriteClassFile(Dss_Class,'', (DSS_Class is TCktElementClass) ) Then Exit;  // bail on error
      DSS_Class.Saved := TRUE;
   End;

  Result := TRUE;

end;

function TCircuit.SaveMasterFile: Boolean;

Var
   F:TextFile;
   i:integer;

begin
  Result := FALSE;
  Try
      AssignFile(F, 'Master.DSS');
      Rewrite(F);

      Writeln(F, 'Clear');
      Writeln(F,'New Circuit.' + Name);
      Writeln(F);
      If PositiveSequence Then Writeln(F, 'Set Cktmodel=Positive');
      If DuplicatesAllowed Then Writeln(F, 'set allowdup=yes');
      Writeln(F);

      // Write Redirect for all populated DSS Classes  Except Solution Class
      For i := 1 to SavedFileList.Count  Do
       Begin
          Writeln(F, 'Redirect ', SavedFileList.Strings[i-1]);
       End;

      If FileExists('buscoords.dss') Then
      Begin
         Writeln(F,'MakeBusList');
         Writeln(F, 'Buscoords buscoords.dss');
      End;

      CloseFile(F);
      Result := TRUE;
  Except
      On E:Exception Do DoSimpleMsg('Error Saving Master File: '+E.Message, 435);
  End;

end;

function TCircuit.SaveFeeders: Boolean;
Var
   i:Integer;
   SaveDir, CurrDir:String;
   Meter:TEnergyMeterObj;
begin

   Result := TRUE;
{Write out all energy meter  zones to separate subdirectories}
   SaveDir := GetCurrentDir;
   For i := 1 to EnergyMeters.ListSize Do
    Begin
        Meter := EnergyMeters.Get(i); // Recast pointer
        CurrDir :=  Meter.Name;
        If CreateDir(CurrDir) Then
         Begin
            SetCurrentDir(CurrDir);
            Meter.SaveZone(CurrDir);
            SetCurrentDir(SaveDir);
         End
         Else Begin
            DoSimpleMsg('Cannot create directory: '+CurrDir, 436);
            Result := FALSE;
            SetCurrentDir(SaveDir);  // back to whence we came
            Break;
         End;
    End;  {For}
    
end;

function TCircuit.SaveBusCoords: Boolean;
Var
        F:TextFile;
        i:Integer;
begin

   Result := FALSE;

   Try
       AssignFile(F, 'BusCoords.dss');
       Rewrite(F);



       For i := 1 to NumBuses Do
       Begin
           If Buses^[i].CoordDefined then Writeln(F, CheckForBlanks(BusList.Get(i)), Format(', %-g, %-g', [Buses^[i].X, Buses^[i].Y]));
       End;

       Closefile(F);

       Result := TRUE;

   Except
       On E:Exception Do DoSimpleMsg('Error creating Buscoords.dss.', 437);
   End;

end;

procedure TCircuit.ReallocDeviceList;

Var
    TempList:THashList;
    i:Integer;

begin
{Reallocate the device list to improve the performance of searches}
    If LogEvents Then LogThisEvent('Reallocating Device List');
    TempList := THashList.Create(2*NumDevices);

    For i := 1 to DeviceList.ListSize Do
    Begin
        Templist.Add(DeviceList.Get(i));
    End;

    DeviceList.Free; // Throw away the old one.
    Devicelist := TempList;

end;

procedure TCircuit.Set_CaseName(const Value: String);
begin
  FCaseName := Value;
  CircuitName_ := Value + '_';
end;


end.
