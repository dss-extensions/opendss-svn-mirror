unit ExportCIMXML;

{
  ----------------------------------------------------------
  Copyright (c) 2009, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Write a CIM XML file using RDF Schema for the Common Distribution
  Power System Model, IEC 61968-13.}

interface

Procedure ExportCDPSM(FileNm:String);

implementation

Uses sysutils, Circuit, DSSGlobals, CktElement,
     PDElement, PCElement, Generator, Load, RegControl,
     Equivalent, Vsource, Isource, Line, Transformer,
     Fuse, Capacitor, CapControl, Reactor;

procedure DoubleNode (var F: TextFile; Node: String; val: Double);
begin
  Writeln (F, Format ('  <cim:%s>%g</cim:%s>', [Node, val, Node]));
end;

procedure IntegerNode (var F: TextFile; Node: String; val: Integer);
begin
  Writeln (F, Format ('  <cim:%s>%d</cim:%s>', [Node, val, Node]));
end;

procedure PrefixVbaseNode (var F: TextFile; Node: String; Prefix: String; val: Double);
begin
  Writeln (F, Format ('  <cim:%s>%s_%.3f</cim:%s>', [Node, Prefix, val, Node]));
end;

procedure VoltageLevelNode (var F: TextFile; Prefix: String; val: Double);
begin
  Writeln(F, Format('  <cim:%s.MemberOf_EquipmentContainer rdf:resource="#VoltageLevel_%.3f"/>',
    [Prefix, val]));
end;

procedure PhasesNode (var F: TextFile; Node: String; pElem:TDSSCktElement);
var
  val, phs: String;
  dot: Integer;
begin
  phs := pElem.FirstBus;
  dot := pos('.',phs);
  if dot < 1 then begin
    val := 'ABC';
  end else begin
    phs := Copy (phs, dot+1, Length(phs));
    val := '';
    if Pos ('1', phs) > 0 then val := val + 'A';
    if Pos ('2', phs) > 0 then val := val + 'B';
    if Pos ('3', phs) > 0 then val := val + 'C';
    if Pos ('4', phs) > 0 then val := val + 'N';
  end;
  Writeln (F, Format ('  <cim:%s>%s</cim:%s>', [Node, val, Node]));
end;

procedure StringNode (var F: TextFile; Node: String; val: String);
begin
  Writeln (F, Format ('  <cim:%s>%s</cim:%s>', [Node, val, Node]));
end;

procedure StartInstance (var F: TextFile; Root: String; Abbrev: String; Name: String);
begin
  Writeln(F, Format('<cim:%s ref:ID="%s_%s">', [Root, Abbrev, Name]));
  StringNode (F, 'IdentifiedObject.name', Name);
end;

procedure EndInstance (var F: TextFile; Root: String);
begin
  Writeln (F, Format ('</cim:%s>', [Root]));
end;

procedure WriteTerminals(var F:TextFile; pElem:TDSSCktElement; Abbrev: String; Name: String);
var
  Nterm, j : Integer;
  BusName, Ref : String;
begin
  Ref := Abbrev + '_' + Name;
  Nterm := pElem.Nterms;
  BusName := Pad(StripExtension(pElem.FirstBus), MaxBusNameLength);
  Write(F, Pad(FullName(PElem), MaxDeviceNameLength+2),' ');
  for j := 1 to NTerm do begin
      Write(F, Busname,' ');
      BusName := Pad(StripExtension(pElem.Nextbus),MaxBusNameLength);
  end;
  Writeln(F);
end;

Procedure ExportCDPSM(FileNm:String);
Var
  F   : TextFile;
  i   : Integer;
  val : double;

  pLoad  : TLoadObj;
  pVsrc  : TVsource;
  pIsrc  : TIsource;
  pEquiv : TEquivalent;
  pGen   : TGenerator;

  pCap  : TCapacitorObj;
  pCapC : TCapControl;
  pXf   : TTransf;
  pReg  : TRegControl;
  pLine : TLine;

Begin
  Try
    Assignfile(F, FileNm);
    ReWrite(F);

    Writeln(F,'<?xml version="1.0" encoding="utf-8"?>');
    Writeln(F,'<!-- un-comment this line to enable validation');
    Writeln(F,'<rdf:RDF xmlns:cim="http://iec.ch/TC57/2008/CIM-schema-cim13#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">');
    Writeln(F,'-->');
    with ActiveCircuit do begin
      i:=1;
      val:=LegalVoltageBases^[i];
      while val > 0.0 do begin
        Writeln(F, Format('<cim:BaseVoltage ref:ID="BaseVoltage_%.3f">', [val]));
        PrefixVbaseNode (F, 'IdentifiedObject.name', 'BaseVoltage', val);
        DoubleNode (F, 'BaseVoltage.nominalVoltage', val);
        Writeln(F,'</cim:BaseVoltage>');

        Writeln(F, Format('<cim:VoltageLevel ref:ID="VoltageLevel_%.3f">', [val]));
        PrefixVbaseNode (F, 'IdentifiedObject.name', 'VoltageLevel', val);
        Writeln(F, Format('  <cim:VoltageLevel.BaseVoltage rdf:resource="#BaseVoltage_%.3f"/>', [val]));
        DoubleNode (F, 'VoltageLevel.lowVoltageLimit', val * NormalMinVolts);
        DoubleNode (F, 'VoltageLevel.highVoltageLimit', val * NormalMaxVolts);
        Writeln(F,'</cim:VoltageLevel>');

        Inc(i);
        val:=LegalVoltageBases^[i];
      end;

      for i := 1 to NumBuses do begin
        Writeln(F, Format('<cim:ConnectivityNode ref:ID="%s">', [BusList.Get(i)]));
        StringNode (F, 'IdentifiedObject.name', BusList.Get(i));
        VoltageLevelNode (F, 'ConnectivityNode', Buses^[i].kVBase);
        DoubleNode (F, 'PositionPoint.xPosition', Buses^[i].x);
        DoubleNode (F, 'PositionPoint.yPosition', Buses^[i].y);
        Writeln(F,'</cim:ConnectivityNode>');
      end;
    end;

    pGen := ActiveCircuit.Generators.First;
    while pGen <> nil do begin
      with pGen do begin
        StartInstance (F, 'EquivalentGenerator', 'Gen', Name);
        EndInstance (F, 'EquivalentGenerator');
      end;
      pGen := ActiveCircuit.Generators.Next;
    end;

    pVsrc := ActiveCircuit.Sources.First; // pIsrc in the same list?
    while pVsrc <> nil do begin
      with pVsrc do begin
        StartInstance (F, 'Substation', 'Src', Name);
        EndInstance (F, 'Substation');
      end;
      pVsrc := ActiveCircuit.Sources.Next;
    end;

    pCap := ActiveCircuit.ShuntCapacitors.First;
    while pCap <> nil do begin
      with pCap do begin
        StartInstance (F, 'ShuntCompensator', 'Cap', Name);
        DoubleNode (F, 'ShuntCompensator.reactivePerSection', TotalKvar);
        IntegerNode (F, 'ShuntCompensator.normalSections', NumSteps);
        IntegerNode (F, 'ShuntCompensator.maximumSections', NumSteps);
        EndInstance (F, 'ShuntCompensator');
        WriteTerminals (F, pCap, 'Cap', Name);
      end;
      pCap := ActiveCircuit.ShuntCapacitors.Next;
    end;

    pCapC := ActiveCircuit.CapControls.First;
    while pCapC <> nil do begin
      with pCapC do begin
        StartInstance (F, 'CapControl', 'CapC', Name);
        EndInstance (F, 'CapControl');
      end;
      pCapC := ActiveCircuit.CapControls.Next;
    end;

    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      with pXf do begin
        StartInstance (F, 'PowerTransformer', 'Xf', Name);
        EndInstance (F, 'PowerTransformer');
      end;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    pReg := ActiveCircuit.RegControls.First;
    while pReg <> nil do begin
      with pReg do begin
        StartInstance (F, 'TapChanger', 'Reg', Name);
        EndInstance (F, 'TapChanger');
      end;
      pReg := ActiveCircuit.RegControls.Next;
    end;

    pLine := ActiveCircuit.Lines.First;
    while pLine <> nil do begin
      with pLine do begin
        StartInstance (F, 'ACLineSegment', 'Line', Name);
        EndInstance (F, 'ACLineSegment');
      end;
      pLine := ActiveCircuit.Lines.Next;
    end;

    pLoad := ActiveCircuit.Loads.First;
    while pLoad <> nil do begin
      if pLoad.Enabled then
        with pLoad do begin
          StartInstance (F, 'EquivalentLoad', 'Load', Name);
          VoltageLevelNode (F, 'EquivalentLoad', kvLoadBase);
          PhasesNode (F, 'ConductingEquipment.phases', pLoad);
          DoubleNode (F, 'EnergyConsumer.qfixed', kvarBase);
          DoubleNode (F, 'EnergyConsumer.pfixed', kWBase);
          IntegerNode (F, 'EnergyConsumer.customerCount', NumCustomers);
          EndInstance (F, 'EquivalentLoad');
          WriteTerminals (F, pLoad, 'Load', Name);
        end;
        pLoad := ActiveCircuit.Loads.Next;
    end;

    GlobalResult := FileNm;
  Finally
    CloseFile(F);
  End;
End;

end.
