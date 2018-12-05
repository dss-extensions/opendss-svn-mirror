unit Circuit;
{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 Change Log
   10-12-99 Added DuplicatesAllowed and ZonesLocked
   10-24-99 Added Losses Property
   12-15-99 Added Default load shapes and generator dispatch reference
   4-17=00  Add Loads List
   5-30-00  Added Positive Sequence Flag
   8-24-00  Added PriceCurve stuff   Updated 3-6-11
   8-1-01  Modified Compute Capacity to report up to loadmult=1
   9-25-15  Fixed broken repository
}

{$WARN UNIT_PLATFORM OFF}

interface

uses
    Classes,
    Solution,
    SysUtils,
    ArrayDef,
    HashList,
    PointerList,
    CktElement,
    DSSClass, {DSSObject,} Bus,
    LoadShape,
    PriceShape,
    ControlQueue,
    uComplex,
    AutoAdd,
    EnergyMeter,
    NamedObject,
    CktTree,
    {$IFDEF MSWINDOWS}
    Graphics,
    vcl.dialogs,
    {$ENDIF}
    {$IFNDEF FPC}
    MeTIS_Exec,
    {$ENDIF}
    math,
    Sparse_Math;

type
    TReductionStrategy = (rsDefault, rsStubs, {rsTapEnds,} rsMergeParallel, rsBreakLoop, rsDangling, rsSwitches, rsLaterals);

    CktElementDef = record
        CktElementClass: Integer;
        devHandle: Integer;
    end;

    pCktElementDefArray = ^CktElementDefArray;
    CktElementDefArray = array[1..1] of CktElementDef;


     // for adding markers to Plot
    TBusMarker = class(TObject)
    // Must be defined before calling circuit plot
    PRIVATE

    PUBLIC
        BusName: String;
        {$IFDEF MSWINDOWS}
        AddMarkerColor: Tcolor;
        {$ELSE}
      AddMarkerColor,
        {$ENDIF}
        AddMarkerCode,
        AddMarkerSize: Integer;

        constructor Create;
        destructor Destroy; OVERRIDE;
    end;

    TDSSCircuit = class(TNamedObject)

    PRIVATE
        NodeBuffer: pIntegerArray;
        NodeBufferMax: Integer;
        FBusNameRedefined: Boolean;
        FActiveCktElement: TDSSCktElement;
        FCaseName: String;

          // Temp arrays for when the bus swap takes place
        SavedBuses: pTBusArray;
        SavedBusNames: pStringArray;
        SavedNumBuses: Integer;
        FLoadMultiplier: Double;  // global multiplier for every load

        AbortBusProcess: Boolean;

        Branch_List: TCktTree; // topology from the first source, lazy evaluation
        BusAdjPC, BusAdjPD: TAdjArray; // bus adjacency lists of PD and PC elements

        procedure AddDeviceHandle(Handle: Integer);
        procedure AddABus;
        procedure AddANodeBus;
        function AddBus(const BusName: String; NNodes: Integer; ActorID: Integer): Integer;
        procedure Set_ActiveCktElement(Value: TDSSCktElement);
        procedure Set_BusNameRedefined(Value: Boolean);
        function Get_Losses(ActorID: Integer): Complex; //Total Circuit losses
        procedure Set_LoadMultiplier(Value: Double);
        procedure SaveBusInfo;
        procedure RestoreBusInfo;

        function SaveMasterFile: Boolean;
        function SaveDSSObjects: Boolean;
        function SaveFeeders: Boolean;
        function SaveBusCoords: Boolean;
        function SaveVoltageBases: Boolean;

        procedure ReallocDeviceList(ActorID: Integer);
        procedure Set_CaseName(const Value: String);

        function Get_Name: String;


    PUBLIC

        ActiveBusIndex: Integer;
        Fundamental: Double;    // fundamental and default base frequency

        Control_BusNameRedefined: Boolean;  // Flag for use by control elements to detect redefinition of buses

        BusList,
        AutoAddBusList,
        DeviceList: THashList;
        DeviceRef: pCktElementDefArray;  //Type and handle of device

          // lists of pointers to different elements by class
        Faults,
        PDElements,
        PCElements,
        DSSControls,
        Sources,
        MeterElements,
        Sensors,
        Monitors,
        EnergyMeters,
        Generators,
        StorageElements,
        PVSystems,
        Substations,
        Transformers,
        CapControls,
        RegControls,
        Lines,
        Loads,
        ShuntCapacitors,
        Feeders,
        Reactors,
        Relays,
        Fuses,
        Reclosers,
        SwtControls: PointerList.TPointerList;
        CktElements: PointerList.TPointerList;

        ControlQueue: TControlQueue;

        Solution: TSolutionObj;
        AutoAddObj: TAutoAdd;

          // For AutoAdd stuff
        UEWeight,
        LossWeight: Double;

        NumUEregs,
        NumLossRegs: Integer;
        Ueregs,
        LossRegs: pIntegerArray;

        CapacityStart,
        CapacityIncrement: Double;

        TrapezoidalIntegration,   // flag for trapezoidal integratio
        LogEvents: Boolean;

        LoadDurCurve: String;
        LoadDurCurveObj: TLoadShapeObj;
        PriceCurve: String;
        PriceCurveObj: TPriceShapeObj;

        NumDevices, NumBuses, NumNodes: Integer;
        MaxDevices, MaxBuses, MaxNodes: Integer;
        IncDevices, IncBuses, IncNodes: Integer;

          // Variables for the tearing Algorithm

        Coverage,                     // Used for the user to stablish the coverage for the algorithm
        Actual_Coverage: Double;   // Indicates the actual coverage of the circuit after running the tearing algorithm
        Longest_paths: array of Integer;   //Stores the coordinates of the longest paths in the circuit
        Path_Idx: array of Integer;   //Stores the indexes from where the areas where formed on the linearized graph
        Buses_Covered: array of Integer;   //Stores the number of buses (estimated - 1 quadrant) per path
        Path_Size: array of Integer;   //Stores the estimated size of each path
        New_Graph: array of Integer;   //Stores the latest weighted graph
        Num_SubCkts: Integer;            // Stores the number of subcircuits for tearing the circuit when executing the "tear_Circuit" command
        Link_Branches: array of String;    // Stores the names of the Link branches for Diakoptics
        PConn_Names: array of String;    // Stores the names of the buses (bus1) of the link branches
        PConn_Voltages: array of Double;    // Stores the voltages at the point of connection of the subcircuits
        Locations: array of Integer;   // Stores the indexes of the locations

          // Variables for Diakoptics
        Contours: TSparse_Complex;
        ZLL: TSparse_Complex;
        ZCT: TSparse_Complex;
        ZCC: TSparse_Complex;

          // Bus and Node stuff
        Buses: pTBusArray;
        MapNodeToBus: pTNodeBusArray;

          // Flags
        Issolved: Boolean;
        DuplicatesAllowed: Boolean;
        ZonesLocked: Boolean;
        MeterZonesComputed: Boolean;
        PositiveSequence: Boolean;  // Model is to be interpreted as Pos seq
        NeglectLoadY: Boolean;

          // Voltage limits
        NormalMinVolts,
        NormalMaxVolts,
        EmergMaxVolts,
        EmergMinVolts: Double;  //per unit voltage restraints for this circuit
        LegalVoltageBases: pDoubleArray;

          // Global circuit multipliers
        GeneratorDispatchReference,
        DefaultGrowthFactor,
        DefaultGrowthRate,
        GenMultiplier,   // global multiplier for every generator
        HarmMult: Double;
        DefaultHourMult: Complex;

        PriceSignal: Double; // price signal for entire circuit

          // EnergyMeter Totals
        RegisterTotals: TRegisterArray;

        DefaultDailyShapeObj,
        DefaultYearlyShapeObj: TLoadShapeObj;

        CurrentDirectory: String;

        ReductionStrategy: TReductionStrategy;
          {ReductionMaxAngle,} ReductionZmag: Double;
        ReduceLateralsKeepLoad: Boolean;
        ReductionStrategyString: String;

        PctNormalFactor: Double;

          {------Plot Marker Circuit Globals---------}
        NodeMarkerCode: Integer;
        NodeMarkerWidth: Integer;
        SwitchMarkerCode: Integer;

        TransMarkerSize: Integer;
        CapMarkerSize: Integer;
        RegMarkerSize: Integer;
        PVMarkerSize: Integer;
        StoreMarkerSize: Integer;
        FuseMarkerSize: Integer;
        RecloserMarkerSize: Integer;
        RelayMarkerSize: Integer;

        TransMarkerCode: Integer;
        CapMarkerCode: Integer;
        RegMarkerCode: Integer;
        PVMarkerCode: Integer;
        StoreMarkerCode: Integer;
        FuseMarkerCode: Integer;
        RecloserMarkerCode: Integer;
        RelayMarkerCode: Integer;

        MarkSwitches: Boolean;
        MarkTransformers: Boolean;
        MarkCapacitors: Boolean;
        MarkRegulators: Boolean;
        MarkPVSystems: Boolean;
        MarkStorage: Boolean;
        MarkFuses: Boolean;
        MarkReclosers: Boolean;
        MarkRelays: Boolean;
        NumCircuits: Integer;

        BusMarkerList: TList;  // list of buses to mark

          {---------------------------------}

        ActiveLoadShapeClass: Integer;


        constructor Create(const aName: String);
        destructor Destroy; OVERRIDE;

        procedure AddCktElement(Handle: Integer);  // Adds last DSS object created to circuit
        procedure ClearBusMarkers;

        procedure TotalizeMeters;
        function ComputeCapacity(ActorID: Integer): Boolean;

        function Save(Dir: String): Boolean;

        procedure ProcessBusDefs(ActorID: Integer);
        procedure ReProcessBusDefs(ActorID: Integer);
        procedure DoResetMeterZones(ActorID: Integer);
        function SetElementActive(const FullObjectName: String): Integer;
        procedure InvalidateAllPCElements;

        procedure DebugDump(var F: TextFile);

          // Access to topology from the first source
        function GetTopology: TCktTree;
        procedure FreeTopology;
        function GetBusAdjacentPDLists(ActorID: Integer): TAdjArray;
        function GetBusAdjacentPCLists(ActorID: Integer): TAdjArray;
        function Tear_Circuit(): Integer;                            // Tears the circuit considering the number of Buses of the original Circuit
        procedure Save_SubCircuits();
        procedure get_longest_path();
        function Append2PathsArray(New_Path: array of Integer): Integer;//  appends a new path to the array and returns the index(1D)
        procedure Normalize_graph();
        procedure Get_paths_4_Coverage();                             // Calculates the paths inside the graph
                                                                        // To guarantee the desired coverage when tearing the system
        procedure Format_SubCircuits(Path: String; NumCkts: Integer); // Arrange the files of the subcircuits to make them independent

        property Name: String READ Get_Name;
        property CaseName: String READ FCaseName WRITE Set_CaseName;
        property ActiveCktElement: TDSSCktElement READ FActiveCktElement WRITE Set_ActiveCktElement;
        property Losses[ActorID: Integer]: Complex READ Get_Losses;  // Total Circuit PD Element losses
        property BusNameRedefined: Boolean READ FBusNameRedefined WRITE Set_BusNameRedefined;
        property LoadMultiplier: Double READ FLoadMultiplier WRITE Set_LoadMultiplier;

    end;

implementation

uses
    PDElement,
    CktElementClass,
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Dynamics,
    Line,
    Transformer,
    Vsource,
    Utilities,
    Executive,
    StrUtils,
    {$IFNDEF FPC}
    DSSForms,
    SHELLAPI,
    windows;
{$ELSE}
     CmdForms;
    {$ENDIF}


//----------------------------------------------------------------------------
constructor TDSSCircuit.Create(const aName: String);

// Var Retval:Integer;

begin
    inherited Create('Circuit');

    IsSolved := false;
     {*Retval   := *} SolutionClass[ActiveActor].NewObject(Name);
    Solution := ActiveSolutionObj;

    LocalName := LowerCase(aName);

    CaseName := aName;  // Default case name to circuitname
                            // Sets CircuitName_

    Fundamental := DefaultBaseFreq;
    ActiveCktElement := nil;
    ActiveBusIndex := 1;    // Always a bus

     // initial allocations increased from 100 to 1000 to speed things up

    MaxBuses := 1000;  // good sized allocation to start
    MaxDevices := 1000;
    MaxNodes := 3 * MaxBuses;
    IncDevices := 1000;
    IncBuses := 1000;
    IncNodes := 3000;

     // Allocate some nominal sizes
    BusList := THashList.Create(900);  // Bus name list Nominal size to start; gets reallocated
    DeviceList := THashList.Create(900);
    AutoAddBusList := THashList.Create(100);

    NumBuses := 0;  // Eventually allocate a single source
    NumDevices := 0;
    NumNodes := 0;

    Faults := TPointerList.Create(2);
    CktElements := TPointerList.Create(1000);
    PDElements := TPointerList.Create(1000);
    PCElements := TPointerList.Create(1000);
    DSSControls := TPointerList.Create(10);
    Sources := TPointerList.Create(10);
    MeterElements := TPointerList.Create(20);
    Monitors := TPointerList.Create(20);
    EnergyMeters := TPointerList.Create(5);
    Sensors := TPointerList.Create(5);
    Generators := TPointerList.Create(5);
    StorageElements := TPointerList.Create(5);
    PVSystems := TPointerList.Create(5);
    Feeders := TPointerList.Create(10);
    Substations := TPointerList.Create(5);
    Transformers := TPointerList.Create(10);
    CapControls := TPointerList.Create(10);
    SwtControls := TPointerList.Create(50);
    RegControls := TPointerList.Create(5);
    Lines := TPointerList.Create(1000);
    Loads := TPointerList.Create(1000);
    ShuntCapacitors := TPointerList.Create(20);
    Reactors := TPointerList.Create(5);
    Reclosers := TPointerList.Create(10);
    Relays := TPointerList.Create(10);
    Fuses := TPointerList.Create(50);

    Buses := Allocmem(Sizeof(Buses^[1]) * Maxbuses);
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

    ActiveLoadShapeClass := USENONE; // Signify not set

    NodeBufferMax := 50;
    NodeBuffer := AllocMem(SizeOf(NodeBuffer^[1]) * NodeBufferMax); // A place to hold the nodes

     // Init global circuit load and harmonic source multipliers
    FLoadMultiplier := 1.0;
    GenMultiplier := 1.0;
    HarmMult := 1.0;

    PriceSignal := 25.0;   // $25/MWH

     // Factors for Autoadd stuff
    UEWeight := 1.0;  // Default to weighting UE same as losses
    LossWeight := 1.0;
    NumUEregs := 1;
    NumLossRegs := 1;
    UEregs := nil;  // set to something so it wont break reallocmem
    LossRegs := nil;
    Reallocmem(UEregs, sizeof(UEregs^[1]) * NumUEregs);
    Reallocmem(Lossregs, sizeof(Lossregs^[1]) * NumLossregs);
    UEregs^[1] := 10;   // Overload UE
    LossRegs^[1] := 13;   // Zone Losses

    CapacityStart := 0.9;     // for Capacity search
    CapacityIncrement := 0.005;

    LoadDurCurve := '';
    LoadDurCurveObj := nil;
    PriceCurve := '';
    PriceCurveObj := nil;

     // Flags
    DuplicatesAllowed := false;
    ZonesLocked := false;   // Meter zones recomputed after each change
    MeterZonesComputed := false;
    PositiveSequence := false;
    NeglectLoadY := false;

    NormalMinVolts := 0.95;
    NormalMaxVolts := 1.05;
    EmergMaxVolts := 1.08;
    EmergMinVolts := 0.90;

    NodeMarkerCode := 16;
    NodeMarkerWidth := 1;
    MarkSwitches := false;
    MarkTransformers := false;
    MarkCapacitors := false;
    MarkRegulators := false;
    MarkPVSystems := false;
    MarkStorage := false;
    MarkFuses := false;
    MarkReclosers := false;

    SwitchMarkerCode := 5;
    TransMarkerCode := 35;
    CapMarkerCode := 38;
    RegMarkerCode := 17; //47;
    PVMarkerCode := 15;
    StoreMarkerCode := 9;
    FuseMarkerCode := 25;
    RecloserMarkerCode := 17;
    RelayMarkerCode := 17;

    TransMarkerSize := 1;
    CapMarkerSize := 3;
    RegMarkerSize := 5; //1;
    PVMarkerSize := 1;
    StoreMarkerSize := 1;
    FuseMarkerSize := 1;
    RecloserMarkerSize := 5;
    RelayMarkerSize := 5;

    BusMarkerList := TList.Create;
    BusMarkerList.Clear;

    TrapezoidalIntegration := false;  // Default to Euler method
    LogEvents := false;

    GeneratorDispatchReference := 0.0;
    DefaultGrowthRate := 1.025;
    DefaultGrowthFactor := 1.0;

    DefaultDailyShapeObj := LoadShapeClass[ActiveActor].Find('default');
    DefaultYearlyShapeObj := LoadShapeClass[ActiveActor].Find('default');

    CurrentDirectory := '';

    BusNameRedefined := true;  // set to force rebuild of buslists, nodelists

    SavedBuses := nil;
    SavedBusNames := nil;

    ReductionStrategy := rsDefault;
//     ReductionMaxAngle := 15.0;
    ReductionZmag := 0.02;
    NumCircuits := 0;
    ReduceLateralsKeepLoad := true;

   {Misc objects}
    AutoAddObj := TAutoAdd.Create;

    Branch_List := nil;
    BusAdjPC := nil;
    BusAdjPD := nil;

   // tearing algorithm vars initialization


    Coverage := 0.9;      // 90% coverage expected by default
    Actual_coverage := -1;       //No coverage
    Num_SubCkts := CPU_Cores - 1;

    setlength(Longest_paths, 0);
    setlength(Path_Idx, 0);
    setlength(Buses_Covered, 0);
    setlength(Path_size, 0);

  // Diakoptics variables
    Contours := TSparse_Complex.Create;
    ZLL := TSparse_Complex.Create;
    ZCC := TSparse_Complex.Create;
    ZCT := TSparse_Complex.Create;

end;

//----------------------------------------------------------------------------
destructor TDSSCircuit.Destroy;
var
    i: Integer;
    pCktElem: TDSSCktElement;
    ElemName: String;

begin
    for i := 1 to NumDevices do
    begin
        try
            pCktElem := TDSSCktElement(CktElements.Get(i));
            ElemName := pCktElem.ParentClass.name + '.' + pCktElem.Name;
            pCktElem.Free;

        except
            ON E: Exception do
                DoSimpleMsg('Exception Freeing Circuit Element:' + ElemName + CRLF + E.Message, 423);
        end;
    end;

    for i := 1 to NumBuses do
        Buses^[i].Free;  // added 10-29-00

    Reallocmem(DeviceRef, 0);
    Reallocmem(Buses, 0);
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
    Sensors.Free;
    Generators.Free;
    StorageElements.Free;
    PVSystems.Free;
    Feeders.Free;
    Substations.Free;
    Transformers.Free;
    CapControls.Free;
    SwtControls.Free;
    RegControls.Free;
    Loads.Free;
    Lines.Free;
    ShuntCapacitors.Free;
    Reactors.Free;
    Reclosers.Free;
    Relays.Free;
    Fuses.Free;

    ControlQueue.Free;

    ClearBusMarkers;
    BusMarkerList.Free;

    AutoAddObj.Free;

    FreeTopology;

    inherited Destroy;
end;

{*******************************************************************************
*           Routine created to empty a recently created folder                 *
********************************************************************************}
procedure DelFilesFromDir(Directory, FileMask: String; DelSubDirs: Boolean);
{$IFDEF MSWINDOWS}
var
    SourceLst: String;
    FOS: TSHFileOpStruct;
    {$ENDIF}
begin
    {$IFDEF MSWINDOWS}
    FillChar(FOS, SizeOf(FOS), 0);
    FOS.wFunc := FO_DELETE;
    SourceLst := Directory + '\' + FileMask + #0;
    FOS.pFrom := Pchar(SourceLst);
    if not DelSubDirs then
        FOS.fFlags := FOS.fFlags or FOF_FILESONLY;
  // Remove the next line if you want a confirmation dialog box
    FOS.fFlags := FOS.fFlags or FOF_NOCONFIRMATION;
  // Add the next line for a "silent operation" (no progress box)
    FOS.fFlags := FOS.fFlags or FOF_SILENT;
    SHFileOperation(FOS);
    {$ENDIF}
end;
{*******************************************************************************
*         This routine retuns the index of the element within the array        *
********************************************************************************}
function get_element_Idx(graph_in: array of Integer; element: Integer): Integer;
var
    Found,                  // To indicate that the element was found
    End_Flag: Boolean;
    Graph_size,
    Local_idx: Integer;
begin
    Result := -1;     // In case the element is not in the array
    End_Flag := true;   //  To control the algorithm execution (while based)
    Local_idx := 0;
    Found := false;  //  Not found yet
    Graph_size := length(graph_in);
    while (End_Flag) and (Local_idx < Graph_Size) do
    begin
        if graph_in[Local_idx] = element then
        begin
            End_Flag := false;
            Found := true;
        end
        else
        begin
            inc(Local_idx);
        end;
    end;
    if Found then
        Result := Local_Idx;
end;
{*******************************************************************************
*         This routine calculates the longest path within a linearized         *
*         graph considering the zero level buses as the beginning of           *
*         new path                                                             *
********************************************************************************}
procedure TDSSCircuit.get_longest_path();
var
    End_flag: Boolean;    //  Terminates the process
    Current_Idx,                  //  Stores the Index value of the current level   
    Current_level: Integer;    //  Stores the current level traced
begin
    with solution do
    begin
        Current_level := maxintvalue(Inc_Mat_Levels);                    //  Init level
        Current_idx := get_element_idx(Inc_Mat_Levels, Current_level);  //  Init Index
        End_flag := true;
        setlength(New_graph, 0);
        while End_flag do
        begin
      //Checks the termination cirteria
            if (Current_level > Inc_Mat_Levels[Current_idx]) or (Inc_Mat_Levels[Current_idx] = 0) then
                End_Flag := false;
      // Is the current bus part of the new backbone?
            if Inc_Mat_Levels[Current_idx] = Current_level then
            begin
                dec(Current_level);
                setlength(New_graph, (length(New_graph) + 1));
                New_graph[High(New_graph)] := Current_idx;
            end;
            dec(Current_idx);
        end;
    end;
end;
{*******************************************************************************
*   This routine appends an array to the paths array and returns its index     *
********************************************************************************}
function TDSSCircuit.Append2PathsArray(New_Path: array of Integer): Integer;
var
    local_idx: Integer;
begin
    Result := High(Longest_paths) + 1;
    for local_idx := 0 to High(New_path) do
    begin
        setlength(Longest_paths, (length(Longest_paths) + 1));
        Longest_paths[High(Longest_paths)] := New_Path[Local_idx];
    end;
end;
{*******************************************************************************
*     This routine normalizes the Inc_matrix levels                            *
********************************************************************************}
procedure TDSSCircuit.Normalize_graph();
var
    Curr_level,                                   // To set the active level
    idx: Integer;                      //
    Ref_detected: Boolean;                      // To detect if there is a zero
begin
    Curr_level := -1;                          // Initializing values
    Ref_detected := false;
    with Solution do
    begin
        for idx := 0 to High(Inc_Mat_Levels) do     // Sweeps the whole graph
        begin
            if Inc_Mat_Levels[idx] = 0 then
                Ref_detected := true
            else
            begin
                if (Curr_level >= Inc_Mat_Levels[idx]) or Ref_detected then
                begin
                    Ref_detected := false;
                    Curr_level := Inc_Mat_Levels[idx] - 1;
                    Inc_Mat_Levels[idx] := 1;
                end
                else
                    Inc_Mat_Levels[idx] := Inc_Mat_Levels[idx] - Curr_level;
            end;
        end;
    end;

end;
{*******************************************************************************
*  Traces the paths (0) in the graph to guarantee the desired coverage         *
********************************************************************************}
procedure TDSSCircuit.Get_paths_4_Coverage();
var
    DBLTemp,                                          //  For storing temoprary doubles
    Sys_Size: Double;                           //  Stores the number of buses contained in the system
    SMEnd: Boolean;                          //  Terminates the state machine
    i,
    State: Integer;                          // The current state of the state machine
    Candidates: array of Integer;                // Array for 0 level buses idx
begin
    with solution do
    begin
        SMEnd := true;
        State := 0;
        Sys_Size := Double(length(Inc_Mat_Cols));
        setlength(Buses_Covered, 1);
        setlength(Path_Idx, 1);
        Actual_Coverage := -1;
        while SMEnd do                                            // The state machine starts
        begin
            case State of
                0:
                begin                                             // Processes the first path
                    setlength(Candidates, 0);
                    for i := 0 to (length(Inc_Mat_Levels) - 1) do     //Extracts the 0 Level Buses
                    begin
                        if solution.Inc_Mat_Levels[i] = 0 then
                        begin
                            setlength(Candidates, length(Candidates) + 1);
                            Candidates[High(Candidates)] := i;
                        end;
                    end;
                    setlength(Longest_paths, 0);
                    Buses_covered[0] := MaxIntValue(Candidates);       //  Extracts the maximum level covered
                    Path_Idx[0] := Append2PathsArray(Candidates); //  No shifting in the graph
                    State := 1;                             //  Go to the next state
                end;
                1:
                begin                                                  // Extracts a new path from the longest branch to
                    get_longest_path();                                    // the backbone (Zeros)
                    setlength(Path_Idx, (length(Path_Idx) + 1));
                    Path_Idx[High(Path_Idx)] := Append2PathsArray(New_Graph); //  Adds the new candidates
            //  Estimates the amount of buses covered in this path
                    setlength(Buses_covered, (length(Buses_covered) + 1));
                    Buses_covered[High(Buses_covered)] := New_Graph[0] - New_Graph[High(New_Graph)];
            // Replaces the latest path with 0 in the Bus levels array
                    for i := Path_Idx[High(Path_Idx)] to High(Longest_paths) do
                        Inc_Mat_Levels[Longest_paths[i]] := 0;
                    Normalize_graph;
            // remains in the same state
                end;
            end;
      //  Checks the coverage index to stablish if is necessary to keep tracing paths to increase the coverage
            DBLTemp := 0.0;
            for i := Low(Buses_covered) to High(Buses_covered) do
                DBLtemp := DBLTemp + (0.0 + Buses_Covered[i]);
            DBLtemp := DBLTemp / Sys_Size;
{      If the New coverage is different from the previous one and is below the expected coverage keep going
       The first criteria is to avoid keep working on a path that will not contribute to improve the coverage}
            if (DBLTemp <> Actual_Coverage) and (DBLTemp >= Coverage) then
                SMEnd := false;
            Actual_Coverage := DBLTemp;
        end;
    end;
end;
{*******************************************************************************
* This routine reads the master file of the torn circuit and creates the       *
*  header definitions for declaring separate subcircuits in OpenDSS            *
********************************************************************************}
procedure TDSSCircuit.Format_SubCircuits(Path: String; NumCkts: Integer);
var
    myFile: TextFile;
    Temp_txt,
    Temp_txt2,
    text: String;
    Xtra,
    File_Struc: array of String;
    Str_Found: Boolean;
    Local_Temp,
    FS_Idx,
    FS_Idx1,
    FS_Idx2: Integer;
const
    Reference: array[0..5] of String =                   // To filter the source file
        ('Redirect EnergyM', 'Redirect Monitor', 'MakeBu', 'Redirect BusVolta', 'Buscoords busco', 'Redirect zone');

begin
    // Reads the master file
    AssignFile(myFile, Path + '\master.dss');
    Reset(myFile);                                        // Prepares for reading
    setlength(File_Struc, 0);
    FS_Idx := 0;
    while not Eof(myFile) do                              // Extracts the file content as an array of strings
    begin
        setlength(File_Struc, (length(File_Struc) + 1));
        ReadLn(myFile, text);
        File_Struc[FS_Idx] := text;
        inc(FS_Idx);
    end;
    CloseFile(myFile);
    //  Creates the copy for the interconnected system
    setlength(Xtra, 0);
    AssignFile(myFile, Path + '\Master_Interconnected.dss');
    ReWrite(myFile);                                      // Prepares for writing
    for FS_Idx := 0 to High(File_Struc) do
    begin
        Str_Found := false;
        for FS_Idx1 := 0 to 5 do
        begin
            Local_Temp := ansipos(Reference[FS_Idx1], File_Struc[FS_Idx]);
            Str_Found := (Local_Temp <> 0) or Str_Found;
        end;
        if Str_found then
        begin
            setlength(Xtra, (length(Xtra) + 1));
            Xtra[High(Xtra)] := File_Struc[FS_Idx];
        end
        else
            WriteLn(myFile, File_Struc[FS_Idx]);
    end;
    // Adds the zones and the rest to the file
    for FS_Idx := 0 to High(Xtra) do
    begin
        WriteLn(myFile, Xtra[FS_Idx])
    end;

    CloseFile(myFile);

    // removes the unnecessary information from the master file (deletes the other zones)
    AssignFile(myFile, Path + '\master.dss');
    ReWrite(myFile);                                      // Prepares for writing
    for FS_Idx := 0 to High(File_Struc) do
    begin
        Local_Temp := ansipos('Redirect zone', File_Struc[FS_Idx]);
        if Local_Temp = 0 then
        begin
            Local_Temp := ansipos('Redirect EnergyM', File_Struc[FS_Idx]);
            if Local_Temp = 0 then
            begin
                Local_Temp := ansipos('Redirect Monitor', File_Struc[FS_Idx]);
                if Local_Temp = 0 then
                    WriteLn(myFile, File_Struc[FS_Idx]);
            end;
        end;
    end;
    CloseFile(myFile);
    // Copies the support files to the zones directories
    FS_Idx := 0;
    while FS_Idx <> -1 do
    begin
        Local_Temp := ansipos('Redirect zone', File_Struc[FS_Idx]);
        if Local_Temp = 0 then
        begin
            Local_Temp := ansipos('Redirect ', File_Struc[FS_Idx]);
            if Local_temp <> 0 then
            begin
                text := stringreplace(File_Struc[FS_Idx], 'Redirect ', '', [rfReplaceAll, rfIgnoreCase]);
                {$IFDEF MSWINDOWS}
                for FS_Idx1 := 2 to NumCkts do
                    CopyFile(Pchar(Path + '\' + text), Pchar(Path + '\zone_' + inttostr(FS_Idx1) + '\' + text), true);
                {$ENDIF}
            end;
            inc(FS_Idx);
        end
        else
            FS_Idx := -1;                             // Ends the routine
    end;
    // Creates the master file for each subcircuit
    for FS_Idx := 2 to NumCkts do
    begin
        AssignFile(myFile, Path + '\zone_' + inttostr(FS_Idx) + '\master.dss');
        ReWrite(myFile);
        WriteLn(myFile, 'Clear');
        WriteLn(myFile, 'New Circuit.Zone_' + inttostr(FS_Idx));
        FS_Idx1 := 2;
        while FS_Idx1 <> -1 do                      // Writes the global files
        begin
            Local_Temp := ansipos('Redirect zone', File_Struc[FS_Idx1]);
            if Local_Temp = 0 then
            begin
                WriteLn(myFile, File_Struc[FS_Idx1]);
                inc(FS_Idx1);
            end
            else
                FS_Idx1 := -1;
        end;
        for FS_Idx1 := 0 to High(File_Struc) do   // Writes the zone files
        begin
            Local_Temp := ansipos('Redirect zone_' + inttostr(FS_Idx), File_Struc[FS_Idx1]);
            if Local_Temp <> 0 then
            begin
                text := stringreplace(File_Struc[FS_Idx1], 'zone_' + inttostr(FS_Idx) + '\', '', [rfReplaceAll, rfIgnoreCase]);
                WriteLn(myFile, text);
            end;
        end;
        CloseFile(myFile);
    end;
    // Sets the properties of the VSource on each subcricuit based on the latest voltage measured
    FS_Idx1 := 0;
    for FS_Idx := 2 to NumCkts do
    begin
        AssignFile(myFile, Path + '\zone_' + inttostr(FS_Idx) + '\VSource.dss');
        ReWrite(myFile);
        for FS_Idx2 := 1 to 3 do
        begin
            if FS_Idx2 = 1 then
            begin
                Temp_txt := 'source';
                Temp_txt2 := 'Edit '
            end
            else
            begin
                Temp_txt := 'Vph_' + inttostr(FS_Idx2);
                Temp_txt2 := 'New '
            end;

            text := Temp_txt2 + 'Vsource.' + Temp_txt +
                ' bus1=' + PConn_Names[FS_Idx - 1] + '.' + inttostr(FS_Idx2) +
                ' phases=1 pu=1.0' +
                ' basekv=' + floattostrF(PConn_Voltages[FS_Idx1], ffGeneral, 8, 3) +
                ' angle=' + floattostrF(PConn_Voltages[FS_Idx1 + 1], ffGeneral, 8, 3) +
                ' R1=0 X1=0.001 R0=0 X0=0.001';
            WriteLn(myFile, text);
            FS_Idx1 := FS_Idx1 + 2;
        end;
        CloseFile(myFile);
    end;
end;

{*******************************************************************************
*        Saves the subcircuits created in memory into the hard drive           *
********************************************************************************
}
procedure TDSSCircuit.Save_SubCircuits();
var
    Fileroot: String;
begin
    // Prepares everything to save the base of the torn circuit on a separate folder
    Fileroot := GetCurrentDir;
    Fileroot := Fileroot + '\Torn_Circuit';
    CreateDir(Fileroot);                        // Creates the folder for storing the modified circuit
    DelFilesFromDir(Fileroot, '*', true);         // Removes all the files inside the new directory (if exists)
    DssExecutive.Command := 'save circuit Dir="' + Fileroot + '"';
    // This routine extracts and modifies the file content to separate the subsystems as OpenDSS projects indepedently
    Format_SubCircuits(FileRoot, length(Locations));
end;

{*******************************************************************************
*         This routine tears the circuit into many pieces as CPUs are          *
*         available in the local computer (in the best case)                   *
********************************************************************************}
function TDSSCircuit.Tear_Circuit(): Integer;
    {$IFDEF FPC}
begin
  DoErrorMsg('Tear_Circuit','MeTIS cannot start.',
             'The MeTIS program is not supported in FPC; TFileSearchReplace is unavailable.', 7006)
end;
    {$ELSE}
var
    FileCreated: Boolean;
    Ftree,
    F: TextFile;
    TreeNm,                                           // For debugging
    MeTISCmd,
    BusName,
    Terminal,
    TextCmd,
    PDElement,
    FileName: String;
    {$IFDEF MSWINDOWS}
    SEInfo: TShellExecuteInfo;                // Shell Info handle
    {$ENDIF}
    NodeIdx,
    Num_Pieces,
    Location_idx,                                     // Active Location
    j, jj, dbg, dbg2,
    i: Integer;                          // Generic counter variables
    Candidates: array of Integer;                 // Array for 0 level buses idx

    EMeter: TEnergyMeterObj;
    pBus: TDSSBus;
    Volts: Polar;
    Term_volts: array of Double;                  // To verify the connection of the branch
    MeTISZones: TStringList;                      // The list for assigning a zone to a bus after tearing
    BusZones: array of String;
    Replacer: TFileSearchReplace;

begin
    Num_pieces := Num_SubCkts;
    with solution do
    begin

    // Calculates the incidence matrix and laplacian to generate the graph file to be
    // send to MeTiS
        Calc_Inc_Matrix_Org(ActiveActor);                       //Calculates the ordered incidence matrix
        Laplacian := IncMat.Transpose();                        // Transposes the Incidence Matrix
        Laplacian := Laplacian.multiply(IncMat);                // Laplacian Matrix calculated
    // Generates the graph file
    {******************************************************************************************}
        FileName := GetOutputDirectory + CircuitName_[ActiveActor] + '.graph';
        Assignfile(F, FileName);
        ReWrite(F);
        Writeln(F, inttostr(length(Inc_Mat_Cols)) + ' ' + inttostr(length(Inc_Mat_Cols) - 1)); // it should be the rank of the incidence matrix
        jj := 0;
        for i := 1 to Laplacian.NZero do
        begin
            if (Laplacian.data[i - 1][0] = jj) and (Laplacian.data[i - 1][0] <> Laplacian.data[i - 1][1]) then
                Write(F, inttostr(Laplacian.data[i - 1][1] + 1) + ' ')
            else
            begin
                if (Laplacian.data[i - 1][0] <> jj) then
                begin
                    Writeln(F, '');
                    Write(F, inttostr(Laplacian.data[i - 1][1] + 1) + ' ');
                    inc(jj);
                end;
            end;
        end;
        CloseFile(F);
    {******************************************************************************************}
        if Num_pieces <= 8 then
            MeTISCmd := 'pmetis.exe'  // For less than 8 zones use pMeTIS
        else
            MeTISCmd := 'kmetis.exe';                    // For more than 8 zonez use k-Way (kMeTIS)
    {******************************************************************************************}
        if fileexists(Pchar(FileName + '.part.' + inttostr(Num_pieces))) then    // Checks if the file exists before
            deletefile(Pchar(FileName + '.part.' + inttostr(Num_pieces)));
        repeat
            {$IFDEF MSWINDOWS}
            TextCmd := RunMeTIS(DSSDirectory + MeTISCmd + ' ' + FileName + ' ' + inttostr(Num_pieces));  // Executes MeTIS
            {$ENDIF}
            Flag := ContainsText(TextCmd, 'I detected an error');
            if Flag then       // The # of edges was wrong, use the one proposed by MeTIS
            begin
                TextCmd := GetNumEdges(TextCmd);                     // Gest the # of edges proposed by MeTIS
                jj := length(inttostr(length(Inc_Mat_Cols))) + 2;// Caculates the index for replacing the number in the Graph File
        // Replaces the old data with the new at the file header
                Replacer := TFileSearchReplace.Create(FileName);
                try
                    Replacer.Replace(inttostr(length(Inc_Mat_Cols)) + ' ' + inttostr(length(Inc_Mat_Cols) - 1),
                        inttostr(length(Inc_Mat_Cols)) + ' ' + TextCmd, [rfIgnoreCase]);
                finally
                    Replacer.Free;
                end;
            end;
        until not flag;
    {******************************************************************************************}
    // Verifies if there was no error executing MeTIS and the zones file was created
        if (TextCmd <> '**Error**') and fileexists(Pchar(FileName + '.part.' + inttostr(Num_pieces))) then
        begin
            MeTISZones := TStringList.Create;                     // Opens the file containing the tearing results
            MeTISZones.LoadFromFile(FileName + '.part.' + inttostr(Num_pieces));
            setlength(Locations, 1);
            setlength(BusZones, 1);
            for i := 0 to (MeTISZones.Count - 1) do
            begin
                if i = 0 then
                begin
                    Locations[i] := 0;
                    BusZones[i] := MeTISZones[i];
                end
                else
                begin
                    if MeTISZones[i] <> BusZones[high(BusZones)] then   // Moving to another zone in the file
                    begin
                        j := 0;
                        if i < (MeTISZones.Count - 1) then                // If not lower means the zone is only 1 bus
                            j := Integer(MeTISZones[i] = MeTISZones[i + 1]);
                        if j = 1 then                                     // Varifies that the zone is big enough
                        begin
                            j := 0;                                      // Verifies that this zone hasn't been counted before
                            for jj := 0 to High(BusZones) do
                            begin
                                if MeTISZones[i] = BusZones[jj] then
                                begin
                                    inc(j);
                                    Break;
                                end;
                            end;
                            if j = 0 then                                   // Is not in the list, add the new location
                            begin
                                setlength(Locations, Length(Locations) + 1);
                                setlength(BusZones, Length(BusZones) + 1);
                                Locations[High(Locations)] := i;
                                BusZones[High(BusZones)] := MeTISZones[i];
                            end;
                        end;
                    end;
                end;
            end;

    //***********The directory is ready for storing the new circuit****************
            EMeter := EnergyMeters.First;
            while EMeter <> nil do
            begin
                EMeter.Enabled := false;
                EMeter := EnergyMeters.Next;
            end;
    //************ Creates the meters at the tearing locations  ********************
            Result := 1;                                  // Resets the result variable (Return)
            setlength(PConn_Voltages, length(Locations) * 6);        //  Sets the memory space for storing the voltage at the point of conn
            setlength(Link_branches, length(Locations));           //  Sets the memory space for storing the link branches names
            setlength(PConn_Names, length(Locations));             //  Sets the memory space for storing the Bus names
            SolutionAbort := false;
            j := 0;
            for i := 1 to High(Locations) do
            begin
                if Locations[i] >= 0 then
                begin
                    inc(Result);
          // Gets the name of the PDE for placing the EnergyMeter
                    with solution do
                    begin
                        PDElement := Inc_Mat_Rows[get_IncMatrix_Row(Locations[i])];
                        Link_Branches[i] := PDElement;
                        dbg := get_IncMatrix_Col(Locations[i]);      // Temporary stores the given location
      // Checks the branch orientation across the feeder by substracting the voltages around the branch
      // Start with Bus 1
                        setlength(Term_volts, 2);
                        for dbg := 0 to 1 do
                        begin
                            BusName := Inc_Mat_Cols[Active_Cols[dbg]];
                            SetActiveBus(BusName);           // Activates the Bus
                            pBus := Buses^[ActiveBusIndex];
                            jj := 1;
         // this code so nodes come out in order from smallest to larges
                            repeat
                                NodeIdx := pBus.FindIdx(jj);   // Get the index of the Node that matches jj
                                inc(jj)
                            until NodeIdx > 0;
                            Volts := ctopolardeg(Solution.NodeV^[pBus.GetRef(NodeIdx)]);  // referenced to pBus
                            Term_volts[dbg] := Volts.mag;
                        end;

        // Determines the best place to connect the EnergyMeter
                        Term_volts[0] := Term_volts[0] - Term_volts[1];
                        if Term_volts[0] >= 0 then
                            jj := 0
                        else
                            jj := 1;
                        BusName := Inc_Mat_Cols[Active_Cols[jj]];
                        Terminal := 'terminal=' + inttostr(jj + 1);

                        PConn_Names[i] := BusName;
                        SetActiveBus(BusName);           // Activates the Bus
                        pBus := Buses^[ActiveBusIndex];

                        for jj := 1 to 3 do
                        begin
               // this code so nodes come out in order from smallest to larges
                            NodeIdx := pBus.FindIdx(jj);   // Get the index of the Node that matches jj

                            Volts := ctopolardeg(Solution.NodeV^[pBus.GetRef(NodeIdx)]);  // referenced to pBus
                            PConn_Voltages[j] := (Volts.mag / 1000);
                            inc(j);
                            PConn_Voltages[j] := Volts.ang;
                            inc(j);
                        end;

                    end;
          // Generates the OpenDSS Command;
                    DssExecutive.Command := 'New EnergyMeter.Zone_' + inttostr(i + 1) + ' element=' + PDElement + ' ' + Terminal + ' option=R action=C';
                end;
            end;
        end
        else
        begin
            if (TextCmd = '**Error**') then
                DoErrorMsg('Tear_Circuit', 'MeTIS cannot start.',
                    'The MeTIS program (pmetis.exe/kmetis.exe) cannot be executed/found.', 7006)
            else
                DoErrorMsg('Tear_Circuit', 'The graph file is incorrect.',
                    'MeTIS cannot process the graph file because is incorrect' +
                    '(The number of edges is incorrect).', 7007);
        end;
    end;
end;
{$ENDIF}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
procedure TDSSCircuit.ProcessBusDefs(ActorID: Integer);
var
    BusName: String;
    NNodes, NP, Ncond, i, j, iTerm, RetVal: Integer;
    NodesOK: Boolean;

begin
    with ActiveCktElement do
    begin
        np := NPhases;
        Ncond := NConds;

        Parser[ActorID].Token := FirstBus;     // use parser functions to decode
        for iTerm := 1 to Nterms do
        begin
            NodesOK := true;
           // Assume normal phase rotation  for default
            for i := 1 to np do
                NodeBuffer^[i] := i; // set up buffer with defaults

           // Default all other conductors to a ground connection
           // If user wants them ungrounded, must be specified explicitly!
            for i := np + 1 to NCond do
                NodeBuffer^[i] := 0;

           // Parser will override bus connection if any specified
            BusName := Parser[ActorID].ParseAsBusName(NNodes, NodeBuffer, ActorID);

           // Check for error in node specification
            for j := 1 to NNodes do
            begin
                if NodeBuffer^[j] < 0 then
                begin
                    retval := DSSMessageDlg('Error in Node specification for Element: "' + ParentClass.Name + '.' + Name + '"' + CRLF +
                        'Bus Spec: "' + Parser[ActorID].Token + '"', false);
                    NodesOK := false;
                    if retval = -1 then
                    begin
                        AbortBusProcess := true;
                        AppendGlobalresult('Aborted bus process.');
                        Exit
                    end;
                    Break;
                end;
            end;


           // Node -Terminal Connnections
           // Caution: Magic -- AddBus replaces values in nodeBuffer to correspond
           // with global node reference number.
            if NodesOK then
            begin
                ActiveTerminalIdx := iTerm;
                ActiveTerminal.BusRef := AddBus(BusName, Ncond, ActorID);
                SetNodeRef(iTerm, NodeBuffer);  // for active circuit
            end;
            Parser[ActorID].Token := NextBus;
        end;
    end;
end;


//----------------------------------------------------------------------------
procedure TDSSCircuit.AddABus;
begin
    if NumBuses > MaxBuses then
    begin
        Inc(MaxBuses, IncBuses);
        ReallocMem(Buses, SizeOf(Buses^[1]) * MaxBuses);
    end;
end;

//----------------------------------------------------------------------------
procedure TDSSCircuit.AddANodeBus;
begin
    if NumNodes > MaxNodes then
    begin
        Inc(MaxNodes, IncNodes);
        ReallocMem(MapNodeToBus, SizeOf(MapNodeToBus^[1]) * MaxNodes);
    end;
end;

//----------------------------------------------------------------------------
function TDSSCircuit.AddBus(const BusName: String; NNodes: Integer; ActorID: Integer): Integer;

var
    NodeRef, i: Integer;
begin

// Trap error in bus name
    if Length(BusName) = 0 then
    begin  // Error in busname
        DoErrorMsg('TDSSCircuit.AddBus', 'BusName for Object "' + ActiveCktElement.Name + '" is null.',
            'Error in definition of object.', 424);
        for i := 1 to ActiveCktElement.NConds do
            NodeBuffer^[i] := 0;
        Result := 0;
        Exit;
    end;

    Result := BusList.Find(BusName);
    if Result = 0 then
    begin
        Result := BusList.Add(BusName);    // Result is index of bus
        Inc(NumBuses);
        AddABus;   // Allocates more memory if necessary
        Buses^[NumBuses] := TDSSBus.Create;
    end;

    {Define nodes belonging to the bus}
    {Replace Nodebuffer values with global reference number}
    with Buses^[Result] do
    begin
        for i := 1 to NNodes do
        begin
            NodeRef := Add(NodeBuffer^[i], ActorID);
            if NodeRef = NumNodes then
            begin  // This was a new node so Add a NodeToBus element ????
                AddANodeBus;   // Allocates more memory if necessary
                MapNodeToBus^[NumNodes].BusRef := Result;
                MapNodeToBus^[NumNodes].NodeNum := NodeBuffer^[i]
            end;
            NodeBuffer^[i] := NodeRef;  //  Swap out in preparation to setnoderef call
        end;
    end;
end;

//----------------------------------------------------------------------------
procedure TDSSCircuit.AddDeviceHandle(Handle: Integer);
begin
    if NumDevices > MaxDevices then
    begin
        MaxDevices := MaxDevices + IncDevices;
        ReallocMem(DeviceRef, Sizeof(DeviceRef^[1]) * MaxDevices);
    end;
    DeviceRef^[NumDevices].devHandle := Handle;    // Index into CktElements
    DeviceRef^[NumDevices].CktElementClass := LastClassReferenced[ActiveActor];
end;


//----------------------------------------------------------------------------
function TDSSCircuit.SetElementActive(const FullObjectName: String): Integer;

// Fast way to set a cktelement active
var
    Devindex: Integer;
    DevClassIndex: Integer;
    DevType,
    DevName: String;

begin

    Result := 0;

    ParseObjectClassandName(FullObjectName, DevType, DevName);
    DevClassIndex := ClassNames[ActiveActor].Find(DevType);
    if DevClassIndex = 0 then
        DevClassIndex := LastClassReferenced[ActiveActor];
    if DevName <> '' then
    begin
        Devindex := DeviceList.Find(DevName);
        while DevIndex > 0 do
        begin
            if DeviceRef^[Devindex].CktElementClass = DevClassIndex then   // we got a match
            begin
                ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(DevClassIndex);
                LastClassReferenced[ActiveActor] := DevClassIndex;
                Result := DeviceRef^[Devindex].devHandle;
             // ActiveDSSClass[ActiveActor].Active := Result;
            //  ActiveCktElement := ActiveDSSClass.GetActiveObj;
                ActiveCktElement := CktElements.Get(Result);
                Break;
            end;
            Devindex := Devicelist.FindNext;   // Could be duplicates
        end;
    end;

    CmdResult := Result;

end;

//----------------------------------------------------------------------------
procedure TDSSCircuit.Set_ActiveCktElement(Value: TDSSCktElement);
begin
    FActiveCktElement := Value;
    ActiveDSSObject[ActiveActor] := Value;
end;

//----------------------------------------------------------------------------
procedure TDSSCircuit.AddCktElement(Handle: Integer);


begin

   // Update lists that keep track of individual circuit elements
    Inc(NumDevices);

   // Resize DeviceList if no. of devices greatly exceeds allocation
    if Cardinal(NumDevices) > 2 * DeviceList.InitialAllocation then
        ReAllocDeviceList(ActiveActor);
    DeviceList.Add(ActiveCktElement.Name);
    CktElements.Add(ActiveCktElement);

   {Build Lists of PC and PD elements}
    case (ActiveCktElement.DSSObjType and BaseClassMask) of
        PD_ELEMENT:
            PDElements.Add(ActiveCktElement);
        PC_ELEMENT:
            PCElements.Add(ActiveCktElement);
        CTRL_ELEMENT:
            DSSControls.Add(ActiveCktElement);
        METER_ELEMENT:
            MeterElements.Add(ActiveCktElement);
    else
       {Nothing}
    end;

   {Build  lists of Special elements and generic types}
    case (ActiveCktElement.DSSObjType and CLASSMASK) of
        MON_ELEMENT:
            Monitors.Add(ActiveCktElement);
        ENERGY_METER:
            EnergyMeters.Add(ActiveCktElement);
        SENSOR_ELEMENT:
            Sensors.Add(ActiveCktElement);
        GEN_ELEMENT:
            Generators.Add(ActiveCktElement);
        SOURCE:
            Sources.Add(ActiveCktElement);
        CAP_CONTROL:
            CapControls.Add(ActiveCktElement);
        SWT_CONTROL:
            SwtControls.Add(ActiveCktElement);
        REG_CONTROL:
            RegControls.Add(ActiveCktElement);
        LOAD_ELEMENT:
            Loads.Add(ActiveCktElement);
        CAP_ELEMENT:
            ShuntCapacitors.Add(ActiveCktElement);
        REACTOR_ELEMENT:
            Reactors.Add(ActiveCktElement);
        RELAY_CONTROL:
            Relays.Add(ActiveCktElement);
        FUSE_CONTROL:
            Fuses.Add(ActiveCktElement);
        RECLOSER_CONTROL:
            Reclosers.Add(ActiveCktElement);

       { Keep Lines, Transformer, and Lines and Faults in PDElements and separate lists
         so we can find them quickly.}
        XFMR_ELEMENT:
            Transformers.Add(ActiveCktElement);
        LINE_ELEMENT:
            Lines.Add(ActiveCktElement);
        FAULTOBJECT:
            Faults.Add(ActiveCktElement);
        FEEDER_ELEMENT:
            Feeders.Add(ActiveCktElement);

        STORAGE_ELEMENT:
            StorageElements.Add(ActiveCktElement);
        PVSYSTEM_ELEMENT:
            PVSystems.Add(ActiveCktElement);
    end;

  // AddDeviceHandle(Handle); // Keep Track of this device result is handle
    AddDeviceHandle(CktElements.ListSize); // Handle is global index into CktElements
    ActiveCktElement.Handle := CktElements.ListSize;

end;

//----------------------------------------------------------------------------
procedure TDSSCircuit.DoResetMeterZones(ActorID: Integer);

begin

 { Do this only if meterzones unlocked .  Normally, Zones will remain unlocked
   so that all changes to the circuit will result in rebuilding the lists}
    if not MeterZonesComputed or not ZonesLocked then
    begin
        if LogEvents then
            LogThisEvent('Resetting Meter Zones', ActorID);
        EnergyMeterClass[ActorID].ResetMeterZonesAll(ActorID);
        MeterZonesComputed := true;
        if LogEvents then
            LogThisEvent('Done Resetting Meter Zones', ActorID);
    end;

    FreeTopology;

end;

//----------------------------------------------------------------------------
procedure TDSSCircuit.SaveBusInfo;
var
    i: Integer;

begin

{Save existing bus definitions and names for info that needs to be restored}
    SavedBuses := Allocmem(Sizeof(SavedBuses^[1]) * NumBuses);
    SavedBusNames := Allocmem(Sizeof(SavedBusNames^[1]) * NumBuses);

    for i := 1 to NumBuses do
    begin
        SavedBuses^[i] := Buses^[i];
        SavedBusNames^[i] := BusList.get(i);
    end;
    SavedNumBuses := NumBuses;

end;

//----------------------------------------------------------------------------
procedure TDSSCircuit.RestoreBusInfo;

var
    i, j, idx, jdx: Integer;
    pBus: TDSSBus;

begin

// Restore  kV bases, other values to buses still in the list
    for i := 1 to SavedNumBuses do
    begin
        idx := BusList.Find(SavedBusNames^[i]);
        if idx <> 0 then
            with Buses^[idx] do
            begin
                pBus := SavedBuses^[i];
                kvBase := pBus.kVBase;
                x := pBus.x;
                Y := pBus.y;
                CoordDefined := pBus.CoordDefined;
                Keep := pBus.Keep;
               {Restore Voltages in new bus def that existed in old bus def}
                if assigned(pBus.VBus) then
                begin
                    for j := 1 to pBus.NumNodesThisBus do
                    begin
                        jdx := FindIdx(pBus.GetNum(j));  // Find index in new bus for j-th node  in old bus
                        if jdx > 0 then
                            Vbus^[jdx] := pBus.VBus^[j];
                    end;
                end;
            end;
        SavedBusNames^[i] := ''; // De-allocate string
    end;

    if Assigned(SavedBuses) then
        for i := 1 to SavedNumBuses do
            SavedBuses^[i].Free;  // gets rid of old bus voltages, too

    ReallocMem(SavedBuses, 0);
    ReallocMem(SavedBusNames, 0);

end;

//----------------------------------------------------------------------------
procedure TDSSCircuit.ReProcessBusDefs(ActorID: Integer);

// Redo all Buslists, nodelists

var
    CktElementSave: TDSSCktElement;
    i: Integer;

begin
    if LogEvents then
        LogThisEvent('Reprocessing Bus Definitions', ActorID);

    AbortBusProcess := false;
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
    while ActiveCktElement <> nil do
    begin
        if ActiveCktElement.Enabled then
            ProcessBusDefs(ActorID);
        if AbortBusProcess then
            Exit;
        ActiveCktElement := CktElements.Next;
    end;

    ActiveCktElement := CktElementSave;  // restore active circuit element

    for i := 1 to NumBuses do
        Buses^[i].AllocateBusVoltages;
    for i := 1 to NumBuses do
        Buses^[i].AllocateBusCurrents;

    RestoreBusInfo;     // frees old bus info, too
    DoResetMeterZones(ActorID);  // Fix up meter zones to correspond

    BusNameRedefined := false;  // Get ready for next time
end;

//----------------------------------------------------------------------------
procedure TDSSCircuit.Set_BusNameRedefined(Value: Boolean);
begin
    FBusNameRedefined := Value;

    if Value then
    begin
        Solution.SystemYChanged := true;  // Force Rebuilding of SystemY if bus def has changed
        Control_BusNameRedefined := true;  // So controls will know buses redefined
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TDSSCircuit.Get_Losses(ActorID: Integer): Complex;

var
    pdelem: TPDElement;
begin

{Return total losses in all PD Elements}

    pdelem := PDElements.First;
    Result := cZERO;
    while pdelem <> nil do
    begin
        if pdelem.enabled then
        begin
              {Ignore Shunt Elements}
            if not pdElem.IsShunt then
                Caccum(Result, pdelem.losses[ActorID]);
        end;
        pdelem := PDElements.Next;
    end;

end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCircuit.DebugDump(var F: TextFile);

var
    i, j: Integer;

begin

    Writeln(F, 'NumBuses= ', NumBuses: 0);
    Writeln(F, 'NumNodes= ', NumNodes: 0);
    Writeln(F, 'NumDevices= ', NumDevices: 0);
    Writeln(F, 'BusList:');
    for i := 1 to NumBuses do
    begin
        Write(F, '  ', Pad(BusList.Get(i), 12));
        Write(F, ' (', Buses^[i].NumNodesThisBus: 0, ' Nodes)');
        for j := 1 to Buses^[i].NumNodesThisBus do
            Write(F, ' ', Buses^[i].Getnum(j): 0);
        Writeln(F);
    end;
    Writeln(F, 'DeviceList:');
    for i := 1 to NumDevices do
    begin
        Write(F, '  ', Pad(DeviceList.Get(i), 12));
        ActiveCktElement := CktElements.Get(i);
        if not ActiveCktElement.Enabled then
            Write(F, '  DISABLED');
        Writeln(F);
    end;
    Writeln(F, 'NodeToBus Array:');
    for i := 1 to NumNodes do
    begin
        j := MapNodeToBus^[i].BusRef;
        Write(F, '  ', i: 2, ' ', j: 2, ' (=', BusList.Get(j), '.', MapNodeToBus^[i].NodeNum: 0, ')');
        Writeln(F);
    end;


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TDSSCircuit.InvalidateAllPCElements;

var
    p: TDSSCktElement;

begin

    p := PCElements.First;
    while (p <> nil) do
    begin
        p.YprimInvalid[ActiveActor] := true;
        p := PCElements.Next;
    end;

    Solution.SystemYChanged := true;  // Force rebuild of matrix on next solution

end;


// - - ------------------------------------------------------
procedure TDSSCircuit.Set_LoadMultiplier(Value: Double);

begin

    if (Value <> FLoadMultiplier) then   // We may have to change the Y matrix if the load multiplier  has changed
        case Solution.LoadModel of
            ADMITTANCE:
                InvalidateAllPCElements
        else
            {nada}
        end;

    FLoadMultiplier := Value;

end;

procedure TDSSCircuit.TotalizeMeters;

{ Totalize all energymeters in the problem}

var
    pEM: TEnergyMeterObj;
    i: Integer;

begin
    for i := 1 to NumEMRegisters do
        RegisterTotals[i] := 0.;

    pEM := EnergyMeters.First;
    while pEM <> nil do
        with PEM do
        begin

            for i := 1 to NumEMRegisters do
                RegisterTotals[i] := RegisterTotals[i] + Registers[i] * TotalsMask[i];

            pEM := EnergyMeters.Next;
        end;
end;

function TDSSCircuit.ComputeCapacity(ActorID: Integer): Boolean;
var
    CapacityFound: Boolean;

    function SumSelectedRegisters(const mtrRegisters: TRegisterArray; Regs: pIntegerArray; count: Integer): Double;
    var
        i: Integer;
    begin
        Result := 0.0;
        for i := 1 to count do
        begin
            Result := Result + mtrRegisters[regs^[i]];
        end;
    end;

begin
    Result := false;
    if (EnergyMeters.ListSize = 0) then
    begin
        DoSimpleMsg('Cannot compute system capacity with EnergyMeter objects!', 430);
        Exit;
    end;

    if (NumUeRegs = 0) then
    begin
        DoSimpleMsg('Cannot compute system capacity with no UE resisters defined.  Use SET UEREGS=(...) command.', 431);
        Exit;
    end;

    Solution.Mode := SNAPSHOT;
    LoadMultiplier := CapacityStart;
    CapacityFound := false;

    repeat
        EnergyMeterClass[ActorID].ResetAll(ActorID);
        Solution.Solve(ActorID);
        EnergyMeterClass[ActorID].SampleAll(ActorID);
        TotalizeMeters;

           // Check for non-zero in UEregs
        if SumSelectedRegisters(RegisterTotals, UEregs, NumUEregs) <> 0.0 then
            CapacityFound := true;
           // LoadMultiplier is a property ...
        if not CapacityFound then
            LoadMultiplier := LoadMultiplier + CapacityIncrement;
    until (LoadMultiplier > 1.0) or CapacityFound;
    if LoadMultiplier > 1.0 then
        LoadMultiplier := 1.0;
    Result := true;
end;

function TDSSCircuit.Save(Dir: String): Boolean;
{Save the present circuit - Enabled devices only}

var
    i: Integer;
    Success: Boolean;
    CurrDir, SaveDir: String;

begin
    Result := false;

// Make a new subfolder in the present folder based on the circuit name and
// a unique sequence number
    SaveDir := GetCurrentDir;  // remember where to come back to
    Success := false;
    if Length(Dir) = 0 then
    begin
        dir := Name;

        CurrDir := Dir;
        for i := 0 to 999 do  // Find a unique dir name
        begin
            if not DirectoryExists(CurrDir) then
            begin
                if CreateDir(CurrDir) then
                begin
                    SetCurrentDir(CurrDir);
                    Success := true;
                    Break;
                end;
            end;
            CurrDir := dir + Format('%.3d', [i]);
        end;
    end
    else
    begin
        if not DirectoryExists(Dir) then
        begin
            CurrDir := dir;
            if CreateDir(CurrDir) then
            begin
                SetCurrentDir(CurrDir);
                Success := true;
            end;
        end
        else
        begin  // Exists - overwrite
            CurrDir := Dir;
            SetCurrentDir(CurrDir);
            Success := true;
        end;
    end;

    if not Success then
    begin
        DoSimpleMsg('Could not create a folder "' + Dir + '" for saving the circuit.', 432);
        Exit;
    end;

    SavedFileList[ActiveActor].Clear;  {This list keeps track of all files saved}

    // Initialize so we will know when we have saved the circuit elements
    for i := 1 to CktElements.ListSize do
        TDSSCktElement(CktElements.Get(i)).HasBeenSaved := false;

    // Initialize so we don't save a class twice
    for i := 1 to DSSClassList[ActiveActor].ListSize do
        TDssClass(DSSClassList[ActiveActor].Get(i)).Saved := false;

    {Ignore Feeder Class -- gets saved with Energymeters}
   // FeederClass.Saved := TRUE;  // will think this class is already saved

    {Define voltage sources first}
    Success := WriteVsourceClassFile(GetDSSClassPtr('vsource'), true);
    {Write library files so that they will be available to lines, loads, etc}
    {Use default filename=classname}
    if Success then
        Success := WriteClassFile(GetDssClassPtr('wiredata'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('cndata'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('tsdata'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('linegeometry'), '', false);
    // If Success Then Success :=  WriteClassFile(GetDssClassPtr('linecode'),'', FALSE);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('linespacing'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('linecode'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('xfmrcode'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('loadshape'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('TShape'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('priceshape'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('growthshape'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('XYcurve'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('TCC_Curve'), '', false);
    if Success then
        Success := WriteClassFile(GetDssClassPtr('Spectrum'), '', false);
    if Success then
        Success := SaveFeeders; // Save feeders first
    if Success then
        Success := SaveDSSObjects;  // Save rest ot the objects
    if Success then
        Success := SaveVoltageBases;
    if Success then
        Success := SaveBusCoords;
    if Success then
        Success := SaveMasterFile;


    if Success then
        DoSimpleMsg('Circuit saved in directory: ' + GetCurrentDir, 433)
    else
        DoSimpleMsg('Error attempting to save circuit in ' + GetCurrentDir, 434);
    // Return to Original directory
    SetCurrentDir(SaveDir);

    Result := true;

end;

function TDSSCircuit.SaveDSSObjects: Boolean;
var

    Dss_Class: TDSSClass;
    i: Integer;

begin
    Result := false;

  // Write Files for all populated DSS Classes  Except Solution Class
    for i := 1 to DSSClassList[ActiveActor].ListSize do
    begin
        Dss_Class := DSSClassList[ActiveActor].Get(i);
        if (DSS_Class = SolutionClass[ActiveActor]) or Dss_Class.Saved then
            Continue;   // Cycle to next
            {use default filename=classname}
        if not WriteClassFile(Dss_Class, '', (DSS_Class is TCktElementClass)) then
            Exit;  // bail on error
        DSS_Class.Saved := true;
    end;

    Result := true;

end;

function TDSSCircuit.SaveVoltageBases: Boolean;
var
    F: TextFile;
    i: Integer;
    VBases: String;
begin

    Result := false;
    try
        AssignFile(F, 'BusVoltageBases.DSS');
        Rewrite(F);

//        For i := 1 to NumBuses do
//          If Buses^[i].kVBase > 0.0 Then
//            Writeln(F, Format('SetkVBase Bus=%s  kvln=%.7g ', [BusList.Get(i), Buses^[i].kVBase]));
        DSSExecutive.Command := 'get voltagebases';
        VBases := GlobalResult;
        Writeln(F, 'Set Voltagebases=' + VBases);
        Writeln(F, 'CalcVoltagebases');
        CloseFile(F);
        Result := true;
    except
        On E: Exception do
            DoSimpleMsg('Error Saving BusVoltageBases File: ' + E.Message, 43501);
    end;

end;

function TDSSCircuit.SaveMasterFile: Boolean;

var
    F: TextFile;
    i: Integer;

begin
    Result := false;
    try
        AssignFile(F, 'Master.DSS');
        Rewrite(F);

        Writeln(F, 'Clear');
        Writeln(F, 'New Circuit.' + Name);
        Writeln(F);
        if PositiveSequence then
            Writeln(F, 'Set Cktmodel=Positive');
        if DuplicatesAllowed then
            Writeln(F, 'set allowdup=yes');
        Writeln(F);

      // Write Redirect for all populated DSS Classes  Except Solution Class
        for i := 1 to SavedFileList[ActiveActor].Count do
        begin
            Writeln(F, 'Redirect ', SavedFileList[ActiveActor].Strings[i - 1]);
        end;

        Writeln(F, 'MakeBusList');
        Writeln(F, 'Redirect BusVoltageBases.dss  ! set voltage bases');

        if FileExists('buscoords.dss') then
        begin
            Writeln(F, 'Buscoords buscoords.dss');
        end;

        CloseFile(F);
        Result := true;
    except
        On E: Exception do
            DoSimpleMsg('Error Saving Master File: ' + E.Message, 435);
    end;

end;

function TDSSCircuit.SaveFeeders: Boolean;
var
    i: Integer;
    SaveDir, CurrDir: String;
    Meter: TEnergyMeterObj;
begin

    Result := true;
{Write out all energy meter  zones to separate subdirectories}
    SaveDir := GetCurrentDir;
    for i := 1 to EnergyMeters.ListSize do
    begin
        Meter := EnergyMeters.Get(i); // Recast pointer
        CurrDir := Meter.Name;
        if DirectoryExists(CurrDir) then
        begin
            SetCurrentDir(CurrDir);
            Meter.SaveZone(CurrDir);
            SetCurrentDir(SaveDir);
        end
        else
        begin
            if CreateDir(CurrDir) then
            begin
                SetCurrentDir(CurrDir);
                Meter.SaveZone(CurrDir);
                SetCurrentDir(SaveDir);
            end
            else
            begin
                DoSimpleMsg('Cannot create directory: ' + CurrDir, 436);
                Result := false;
                SetCurrentDir(SaveDir);  // back to whence we came
                Break;
            end;
        end;
    end;  {For}

end;

function TDSSCircuit.SaveBusCoords: Boolean;
var
    F: TextFile;
    i: Integer;
begin

    Result := false;

    try
        AssignFile(F, 'BusCoords.dss');
        Rewrite(F);


        for i := 1 to NumBuses do
        begin
            if Buses^[i].CoordDefined then
                Writeln(F, CheckForBlanks(BusList.Get(i)), Format(', %-g, %-g', [Buses^[i].X, Buses^[i].Y]));
        end;

        Closefile(F);

        Result := true;

    except
        On E: Exception do
            DoSimpleMsg('Error creating Buscoords.dss.', 437);
    end;

end;

procedure TDSSCircuit.ReallocDeviceList(ActorID: Integer);

var
    TempList: THashList;
    i: Integer;

begin
{Reallocate the device list to improve the performance of searches}
    if LogEvents then
        LogThisEvent('Reallocating Device List', ActorID);
    TempList := THashList.Create(2 * NumDevices);

    for i := 1 to DeviceList.ListSize do
    begin
        Templist.Add(DeviceList.Get(i));
    end;

    DeviceList.Free; // Throw away the old one.
    Devicelist := TempList;

end;

procedure TDSSCircuit.Set_CaseName(const Value: String);
begin
    FCaseName := Value;
    CircuitName_[ActiveActor] := Value + '_';
end;

function TDSSCircuit.Get_Name: String;
begin
    Result := LocalName;
end;

function TDSSCircuit.GetBusAdjacentPDLists(ActorID: Integer): TAdjArray;
begin
    if not Assigned(BusAdjPD) then
        BuildActiveBusAdjacencyLists(BusAdjPD, BusAdjPC, ActorID);
    Result := BusAdjPD;
end;

function TDSSCircuit.GetBusAdjacentPCLists(ActorID: Integer): TAdjArray;
begin
    if not Assigned(BusAdjPC) then
        BuildActiveBusAdjacencyLists(BusAdjPD, BusAdjPC, ActorID);
    Result := BusAdjPC;
end;

function TDSSCircuit.GetTopology: TCktTree;
var
    i: Integer;
    elem: TDSSCktElement;
begin
    if not assigned(Branch_List) then
    begin
    {Initialize all Circuit Elements and Buses to not checked, then build a new tree}
        elem := CktElements.First;
        while assigned(elem) do
        begin
            elem.Checked := false;
            for i := 1 to elem.Nterms do
                elem.Terminals^[i].Checked := false;
            elem.IsIsolated := true; // till proven otherwise
            elem := CktElements.Next;
        end;
        for i := 1 to NumBuses do
            Buses^[i].BusChecked := false;
        Branch_List := GetIsolatedSubArea(Sources.First, true);  // calls back to build adjacency lists
    end;
    Result := Branch_List;
end;

procedure TDSSCircuit.FreeTopology;
begin
    if Assigned(Branch_List) then
        Branch_List.Free;
    Branch_List := nil;
    if Assigned(BusAdjPC) then
        FreeAndNilBusAdjacencyLists(BusAdjPD, BusAdjPC);
end;

procedure TDSSCircuit.ClearBusMarkers;
var
    i: Integer;
begin
    for i := 0 to BusMarkerList.count - 1 do
        TBusMarker(BusMarkerList.Items[i]).Free;
    BusMarkerList.Clear;
end;

{====================================================================}
{ TBusMarker }
{====================================================================}

constructor TBusMarker.Create;
begin
    inherited;
    BusName := '';
    AddMarkerColor :=
        {$IFDEF MSWINDOWS}
        clBlack
    {$ELSE}
0
    {$ENDIF}
    ;
    AddMarkerCode := 4;
    AddMarkerSize := 1;
end;

destructor TBusMarker.Destroy;
begin
    BusName := '';
    inherited;
end;


end.
