unit ReduceAlgs;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Reduction Algorithms}

{Primarily called from EnergyMeter}

interface

uses
    CktTree,
    PDELement;

procedure DoReduceDefault(var BranchList: TCktTree);
procedure DoReduceShortlines(var BranchList: TCktTree);
procedure DoReduceDangling(var BranchList: TCktTree);
{  procedure DoReduceTapEnds(var BranchList:TCktTree);}
procedure DoBreakLoops(var BranchList: TCktTree);
procedure DoMergeParallelLines(var BranchList: TCktTree);
procedure DoReduceSwitches(var Branchlist: TCktTree);
procedure DoReduceLaterals(var Branchlist: TCktTree);

procedure DoRemoveBranches(var BranchList: TCktTree; FirstPDElement: TPDElement; KeepLoad: Boolean; const EditStr: String);


implementation

uses
    Line,
    Utilities,
    DSSGlobals,
    Load,
    uComplex,
    ParserDel,
    CktElement,
    sysutils,
    ExecHelper,
    Bus;

const
    SERIESMERGE: Boolean = true;
    PARALLELMERGE: Boolean = false;

procedure DoMergeParallelLines(var BranchList: TCktTree);
{Merge all lines in this zone that are marked in parallel}

var
    LineElement: TLineObj;

begin
    if BranchList <> nil then
    begin
        BranchList.First;
        LineElement := BranchList.GoForward; // Always keep the first element
        while LineElement <> nil do
        begin
            if BranchList.PresentBranch.IsParallel then
            begin
               {There will always be two lines in parallel.  The first operation will disable the second}
                if LineElement.Enabled then
                    LineElement.MergeWith(TLineObj(BranchList.PresentBranch.LoopLineObj), false);  // Guaranteed to be a line
            end;
            LineElement := BranchList.GoForward;
        end;
    end;
end;

procedure DoBreakLoops(var BranchList: TCktTree);

{Break loops}
var
    LineElement: TLineObj;

begin
    if BranchList <> nil then
    begin
        BranchList.First;
        LineElement := BranchList.GoForward; // Always keep the first element
        while LineElement <> nil do
        begin
            if BranchList.PresentBranch.IsLoopedHere then
            begin
               {There will always be two lines in the loop.  The first operation will disable the second}
                if LineElement.Enabled then
                    TLineObj(BranchList.PresentBranch.LoopLineObj).Enabled := false; // Disable the other
            end;
            LineElement := BranchList.GoForward;
        end;
    end;
end;


procedure DoReduceTapEnds(var BranchList: TCktTree);
(*Var
   pLineElem1, pLineElem2:TLineObj;
   ToBusRef:Integer;
   AngleTest:Double;
   ParentNode:TCktTreeNode;
*)
begin

end;


procedure DoReduceDangling(var BranchList: TCktTree);
var
    pLineElem1: TDSSCktElement;
    ToBusRef: Integer;
begin
    if BranchList <> nil then
    begin
     {Let's throw away all dangling end branches}
        BranchList.First;
        pLineElem1 := BranchList.GoForward; // Always keep the first element

        while pLineElem1 <> nil do
        begin

            if IsLineElement(pLineElem1) then
                with BranchList.PresentBranch do
                begin

             {If it is at the end of a section and has no load,cap, reactor,
             or coordinate, just throw it away}
                    if IsDangling then
                    begin
                        ToBusRef := ToBusReference;  // only access this property once!
                        if ToBusRef > 0 then
                            with ActiveCircuit[ActiveActor].Buses^[ToBusRef] do
                                if not (Keep) then
                                    pLineElem1.Enabled := false;
                    end; {IF}
                end;  {If-With}
            pLineElem1 := BranchList.GoForward;
        end;
    end;

end;

function IsShortLine(const Elem: TDSSCktElement): Boolean;
var
    Ztest: Double;
    LineElement: TLineObj;

begin
    LineElement := TLineObj(Elem);
     {Get Positive Sequence or equivalent from matrix}
    if LineElement.SymComponentsModel then
        with LineElement do
            Ztest := Cabs(Cmplx(R1, X1)) * Len
    else {Get impedance from Z matrix}  {Zs - Zm}
        with LineElement do
        begin
            if NPhases > 1 then
                Ztest := Cabs(Csub(Z.Getelement(1, 1), Z.GetElement(1, 2))) * Len
            else
                Ztest := Cabs(Z.Getelement(1, 1)) * Len;
        end;

    if Ztest <= ActiveCircuit[ActiveActor].ReductionZmag then
        Result := true
    else
        Result := false;

end;

procedure DoReduceShortlines(var BranchList: TCktTree);
{Eliminate short lines and merge with lines on either side}
var
    LineElement1, LineElement2: TLineObj;
    ShuntElement: TDSSCktElement;
    ParentNode: TCktTreeNode;

begin
    if BranchList <> nil then
    begin  {eliminate really short lines}
      {First, flag all elements that need to be merged}
        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> nil do
        begin
            if IsLineElement(LineElement1) then
            begin
                if IsShortLine(LineElement1) then
                    LineElement1.Flag := true   {Too small: Mark for merge with something}
                else
                    LineElement1.Flag := false;
            end; {IF}
            LineElement1 := BranchList.GoForward;
        end; {WHILE}

        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> nil do
        begin
            if LineElement1.enabled then    // else skip

                if LineElement1.Flag then  // too short; Merge this element out
                begin
                    with BranchList do
                    begin
                        if (PresentBranch.NumChildBranches = 0) and (PresentBranch.NumShuntObjects = 0) then
                            LineElement1.Enabled := false     // just discard it
                        else
                        if (PresentBranch.NumChildBranches = 0) {****OR (PresentBranch.NumChildBranches>1)**} then                    {Merge with Parent and move shunt elements to TO node on parent branch}
                        begin
                            ParentNode := PresentBranch.ParentBranch;
                            if ParentNode <> nil then
                            begin
                                if ParentNode.NumChildBranches = 1 then   // only works for in-line
                                    if not ActiveCircuit[ActiveActor].Buses^[PresentBranch.ToBusReference].Keep then
                                    begin     // Check Keeplist
                                 {Let's consider merging}
                                        LineElement2 := ParentNode.CktObject;
                                        if LineElement2.enabled then  // Check to make sure it hasn't been merged out
                                            if IsLineElement(LineElement2) then
                                                if LineElement2.MergeWith(LineElement1, SERIESMERGE) then
                                                begin {Move any loads to ToBus Reference of parent branch}
                                                    if ParentNode.NumShuntObjects > 0 then
                                                    begin
                                         {Redefine bus connection for PC elements hanging on the bus that is eliminated}
                                                        ShuntElement := ParentNode.FirstShuntObject;
                                                        while ShuntElement <> nil do
                                                        begin
                                                            Parser[ActiveActor].CmdString := 'bus1="' + ActiveCircuit[ActiveActor].BusList.Get(PresentBranch.ToBusReference) + GetNodeString(ShuntElement.GetBus(1)) + '"';
                                                            ShuntElement.Edit(ActiveActor);
                                                            ShuntElement := ParentNode.NextShuntObject;
                                                        end;  {While}
                                                    end; {IF}
                                   //+++ LineElement1 := BranchList.GoForward; // skip to next branch since we eliminated a bus
                                                end;
                                    end; {IF}
                            end; {IF ParentNode}
                        end
                        else
                        if (PresentBranch.NumChildBranches = 1) then {Merge with child}
                        begin
                            if not ActiveCircuit[ActiveActor].Buses^[PresentBranch.ToBusReference].Keep then    // check keeplist
                            begin
                         {Let's consider merging}
                                LineElement2 := PresentBranch.FirstChildBranch.CktObject; // child of PresentBranch
                                if LineElement2.enabled then  // Check to make sure it hasn't been merged out
                                    if IsLineElement(LineElement2) then
                                        if LineElement2.MergeWith(LineElement1, SERIESMERGE) then
                                        begin
                                            if PresentBranch.NumShuntObjects > 0 then
                                            begin
                                 {Redefine bus connection to upline bus}
                                                ShuntElement := PresentBranch.FirstShuntObject;
                                                while ShuntElement <> nil do
                                                begin
                                                    Parser[ActiveActor].CmdString := 'bus1="' + ActiveCircuit[ActiveActor].BusList.Get(PresentBranch.FromBusReference) + GetNodeString(ShuntElement.GetBus(1)) + '"';
                                                    ShuntElement.Edit(ActiveActor);
                                                    ShuntElement := PresentBranch.NextShuntObject;
                                                end;  {While}
                                            end; {IF}
                                            LineElement1 := BranchList.GoForward; // skip to next branch since we eliminated a bus
                                        end;
                            end; {IF not}
                        end; {ELSE}
                    end;
                end;
            LineElement1 := BranchList.GoForward;
        end;

    end;
end;

procedure DoReduceSwitches(var Branchlist: TCktTree);

{Merge switches in with lines or delete if dangling}

var
    LineElement1, LineElement2: TLineObj;
begin

    if BranchList <> nil then
    begin

        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> nil do
        begin

            if LineElement1.Enabled then   // maybe we threw it away already
                if IsLineElement(LineElement1) then
                    if LineElement1.IsSwitch then
                        with BranchList.PresentBranch do
             {see if eligble for merging}
                            case NumChildBranches of
                                0: {Throw away if dangling}
                                    if NumShuntObjects = 0 then
                                        LineElement1.Enabled := false;

                                1:
                                    if NumShuntObjects = 0 then
                                        if not ActiveCircuit[ActiveActor].Buses^[ToBusReference].Keep then
                                        begin
                     {Let's consider merging}
                                            LineElement2 := FirstChildBranch.CktObject;
                                            if IsLineElement(LineElement2) then
                                                if not LineElement2.IsSwitch then
                                                    LineElement2.MergeWith(LineElement1, true){Series Merge}
                                        end;
                            else {Nada}
                            end;

            LineElement1 := BranchList.GoForward;
        end;
    end;

end;

procedure DoReduceDefault(var BranchList: TCktTree);

var
    LineElement1, LineElement2: TLineObj;
begin
    if BranchList <> nil then
    begin

     {Now merge remaining lines}

        LineElement1 := BranchList.First;
        LineElement1 := BranchList.GoForward; // Always keep the first element
        while LineElement1 <> nil do
        begin

            if IsLineElement(LineElement1) then
                if not LineElement1.IsSwitch then
                    if LineElement1.Enabled then   // maybe we threw it away already
                        with BranchList.PresentBranch do
                        begin
                 {see if eligble for merging}
                            if NumChildBranches = 1 then
                                if NumShuntObjects = 0 then
                                    if not ActiveCircuit[ActiveActor].Buses^[ToBusReference].Keep then
                                    begin
                     {Let's consider merging}
                                        LineElement2 := FirstChildBranch.CktObject;

                                        if IsLineElement(LineElement2) then
                                            if not LineElement2.IsSwitch then
                                                LineElement2.MergeWith(LineElement1, true){Series Merge}
                                    end;

                        end;

            LineElement1 := BranchList.GoForward;
        end;
    end;

end;

procedure DoRemoveBranches(var BranchList: TCktTree; FirstPDElement: TPDElement; KeepLoad: Boolean; const EditStr: String);
var
    PDElem: TPDElement;
    BusName: String;
    TotalkVA: Complex;
    pLoad: TLoadObj;
    NewLoadName: String;
    pShunt: TDSSCktElement;
    LoadBus: TDSSBus;
    LoadBasekV: Double;
    StartLevel: Integer;

begin

// Position BranchList at "FirstPDElement"
    PDElem := BranchList.First;
    while (PDElem <> FirstPDElement) and (PDElem <> nil) do
        PDElem := BranchList.GoForward;

    StartLevel := BranchList.level;

    if PDElem = nil then
    begin
        DoSimpleMsg(Format('%s.%s Not Found (Remove Command).', [FirstPDElement.ParentClass.Name, FirstPDElement.Name]), 5432100);
    end
    else
    begin

     { If KeepLoad, create a new Load object at upstream bus (from bus).}
        if KeepLoad then
            with BranchList.PresentBranch do
            begin
                BusName := FirstPDElement.GetBus(FromTerminal);
                TotalkVA := CDivreal(PDelem.Power[FromTerminal, ActiveActor], 1000.0);
                NewLoadName := Format('Eq_%s_%s', [FirstPDElement.Name, StripExtension(BusName)]);
       {Pick up the kV Base for the From bus}
                LoadBus := ActiveCircuit[ActiveActor].Buses^[FromBusReference];
                if Loadbus.kVBase > 0.0 then
                    LoadBasekV := LoadBus.kVBase
                else
                begin    // Try to guess from the present voltage at the first node on the bus
                    ActiveCircuit[ActiveActor].Solution.UpdateVBus(ActiveActor);
                    LoadBasekV := Cabs(Loadbus.Vbus^[1]) * 0.001;
                end;
                if FirstPDElement.NPhases > 1 then
                    LoadBasekV := LoadBasekV * Sqrt3;
       {Load up parser with definition of equivalent load}
                Parser[ActiveActor].CmdString := Format(' phases=%d Bus1=%s kW=%g kvar=%g kV=%g %s', [FirstPDElement.NPhases, Busname, TotalkVA.re, TotalkVA.im, LoadBasekV, EditStr]);
                AddObject('load', NewLoadName); // Add new load to circuit
            end;

     {Disable all elements in the tree downline from the start element}

   //  {****} WriteDLLDebugFile(Format('StartLevel = %d ',[ StartLevel]));

        while PDElem <> nil do
        begin
   // {****} WriteDLLDebugFile(Format('Disabling cktelement %d %s.%s',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name ]));
            with BranchList.PresentBranch do
            begin

                pShunt := BranchList.PresentBranch.FirstShuntObject;
                while pShunt <> nil do
                begin
                    pShunt.Enabled := false;
   //  {****} WriteDLLDebugFile(Format('Disabling shunt element %s.%s',[ pShunt.ParentClass.Name, pShunt.Name ]));
                    pShunt := BranchList.PresentBranch.NextShuntObject;
                end;

            end;

            PDElem.Enabled := false;
            PDElem := BranchList.GoForward;

         // Check to see if we are back where we started. If so, stop.
            if BranchList.Level <= StartLevel then
                PDElem := nil;

        end;

    end;

    with ActiveCircuit[ActiveActor] do
    begin
        ReprocessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
        DoResetMeterZones(ActiveActor);  // without eliminated devices
        Solution.SystemYChanged := true; // force rebuild of Y
    end;
end;

procedure DoReduceLaterals(var Branchlist: TCktTree);
{Remove all 1-phase laterals in Branchlist and lump load back to main feeder}

var
    PDelem: TPDElement;
    BusName: String;
    TotalkVA: Complex;
    pLoad: TLoadObj;
    NewLoadName: String;
    pShunt: TDSSCktElement;
    LoadBus: TDSSBus;
    LoadBasekV: Double;
    StartLevel: Integer;
    pBus: TDSSBus;

begin
 {
  Just march down the feeder until we encounter a 1-phase PD element
 }
   // Position BranchList at "beginning"
    PDElem := BranchList.First;

    while PDElem <> nil do
    begin

        if PDElem.nphases = 1 then   // ELIMINATE THIS LATERAL
        begin
        {Check to see if this is a 1-phase switch or other branch in the middle of a 3-phase branch and go on}
        {If the To bus has more than 1 phase, keep this branch else lump the load at the From node}
            pBus := ActiveCircuit[ActiveActor].Buses^[Branchlist.PresentBranch.ToBusReference];  //To Bus

            if pBus.NumNodesThisBus = 1 then // Eliminate the lateral starting with this branch
            begin

             { If KeepLoad (ReduceLateralsKeepLoad), create a new Load object at upstream bus (from bus).}

                if ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad then
                    with BranchList.PresentBranch do
                    begin
                        BusName := PDElem.GetBus(FromTerminal);
                        TotalkVA := CDivreal(PDelem.Power[FromTerminal, ActiveActor], 1000.0);
                        NewLoadName := Format('Lateral_%s', [PDElem.Name]);
                 {Pick up the kV Base for the From bus}
                        LoadBus := ActiveCircuit[ActiveActor].Buses^[FromBusReference];
                        if (Loadbus.kVBase > 0.0) then
                            LoadBasekV := LoadBus.kVBase  // Use defined voltage base
                        else
                        begin    // Try to guess voltage base from the present voltage at the first node on the bus
                            ActiveCircuit[ActiveActor].Solution.UpdateVBus(ActiveActor);
                            LoadBasekV := Cabs(Loadbus.Vbus^[1]) * 0.001;
                        end;
                 {Load up parser with definition of equivalent load and create a new Load object on the main feeder
                  with the same bus definition as the From Bus of the 1-phase line}
                        Parser[ActiveActor].CmdString := Format(' phases=%d Bus1=%s kW=%g kvar=%g kV=%g', [PDElem.NPhases, Busname, TotalkVA.re, TotalkVA.im, LoadBasekV]);
                        AddObject('load', NewLoadName); // Add new load to circuit
                    end;

             {

               Disable all elements in the tree downline from the beginning of the 1-phase lateral

             }

                StartLevel := BranchList.level;   // record level of first 1-phase branch in this lateral
                while PDElem <> nil do
                begin
           // {****} WriteDLLDebugFile(Format('Disabling cktelement %d %s.%s',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name ]));
                 { Get rid of loads and other shunt elements connected to this branch }
                    with BranchList.PresentBranch do
                    begin

                        pShunt := BranchList.PresentBranch.FirstShuntObject;
                        while pShunt <> nil do
                        begin
                            pShunt.Enabled := false;
           //  {****} WriteDLLDebugFile(Format('Disabling shunt element %s.%s',[ pShunt.ParentClass.Name, pShunt.Name ]));
                            pShunt := BranchList.PresentBranch.NextShuntObject;
                        end;

                    end;

                    PDElem.Enabled := false;
                    PDElem := BranchList.GoForward;

                 // Check to see if we are back where we started. If so, stop with this lateral and get on to the next.
                    if PDElem <> nil then
                        if (BranchList.Level <= StartLevel) then
                            Break;
                end;

            end
            else
                PDElem := BranchList.GoForward;


        end
        else
            PDElem := BranchList.GoForward;


   // {****} If PDElem<>Nil then WriteDLLDebugFile(Format('Going on.. cktelement %d %s.%s phases=%d',[ BranchList.Level, PDelem.ParentClass.Name, PDElem.Name, PDelem.NPhases  ]));

    end;

    with ActiveCircuit[ActiveActor] do
    begin
        ReprocessBusDefs(ActiveActor);  // to get new load added and account for disabled devices
        DoResetMeterZones(ActiveActor);  // without eliminated devices
        Solution.SystemYChanged := true; // force rebuild of Y
    end;


end;

end.
