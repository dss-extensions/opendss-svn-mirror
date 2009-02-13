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

Uses sysutils, Utilities, Circuit, DSSGlobals, CktElement,
     PDElement, PCElement, Generator, Load, RegControl,
     Equivalent, Vsource, Isource, Line, Transformer,
     Fuse, Capacitor, CapControl, Reactor, Feeder;

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

procedure LineRefNode (var F: TextFile; Name: String);
begin
  Writeln(F, Format('  <cim:ACLineSegment.MemberOf_Line rdf:resource="#Line_%s"/>',
    [Name]));
end;

procedure SubRefNode (var F: TextFile; Prefix: String; Name: String);
begin
  Writeln(F, Format('  <cim:%s.MemberOf_Substation rdf:resource="#Sub_%s"/>',
    [Prefix, Name]));
end;

procedure XfRefNode (var F: TextFile; Name: String);
begin
  Writeln(F, Format('  <cim:Transformer.MemberOf_PowerTransformer rdf:resource="#Xf_%s"/>',
    [Name]));
end;

procedure CapControlRefNodes (var F: TextFile; Cap: String; Term: String);
begin
  Writeln(F, Format('  <cim:RegulatingControl.regulatingCondEq rdf:resource="#Cap_%s"/>',
    [Cap]));
  Writeln(F, Format('  <cim:RegulatingControl.terminal rdf:resource="#Cap_%s"/>',
    [Term]));
end;

procedure BaseVoltageNode (var F: TextFile; Prefix: String; val: Double);
begin
  Writeln(F, Format('  <cim:%s.BaseVoltage rdf:resource="#BaseVoltage_%.3f"/>',
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

function IsGroundBus (const S: String) : Boolean;
var
  i : Integer;
begin
  Result := True;
  i := pos ('.1', S);
  if i > 0 then Result := False;
  i := pos ('.2', S);
  if i > 0 then Result := False;
  i := pos ('.3', S);
  if i > 0 then Result := False;
  i := pos ('.', S);
  if i = 0 then Result := False;
end;

procedure WriteTerminals(var F:TextFile; pElem:TDSSCktElement; Abbrev: String; Name: String);
var
  Nterm, j : Integer;
  BusName, Ref, TermName : String;
begin
  Ref := Abbrev + '_' + Name;
  Nterm := pElem.Nterms;
  BusName := pElem.FirstBus;
  for j := 1 to NTerm do begin
    if IsGroundBus (BusName) = False then begin
      BusName := StripExtension (BusName);

      Str (j, TermName);
      TermName := Ref + '_T' + TermName;

      StartInstance (F, 'Terminal', 'Trm', TermName);
      StringNode (F, 'Naming.name', TermName);
      Writeln (F, Format('  <cim:Terminal.ConductingEquipment rdf:resource="#%s"/>',
        [Ref]));
      Writeln (F, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#CN_%s"/>',
        [BusName]));
      EndInstance (F, 'Terminal');
    end;

    BusName := pElem.Nextbus;
  end;
end;

Procedure ExportCDPSM(FileNm:String);
Var
  F   : TextFile;
  i   : Integer;
  val : double;
  v1, v2 : double;
  WdgName : String;

  pLoad  : TLoadObj;
  pVsrc  : TVsourceObj;
  pIsrc  : TIsourceObj;
  pEquiv : TEquivalentObj;
  pGen   : TGeneratorObj;

  pCap  : TCapacitorObj;
  pCapC : TCapControlObj;
  pXf   : TTransfObj;
  pReg  : TRegControlObj;
  pLine : TLineObj;

  pFdr  : TFeederObj;
  kvFdr : double;

Begin
  Try
    Assignfile(F, FileNm);
    ReWrite(F);
    kvFdr := 0.0;

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
        Writeln(F, Format('<cim:ConnectivityNode ref:ID="CN_%s">', [BusList.Get(i)]));
        StringNode (F, 'IdentifiedObject.name', BusList.Get(i));
        VoltageLevelNode (F, 'ConnectivityNode', Buses^[i].kVBase);
        DoubleNode (F, 'PositionPoint.xPosition', Buses^[i].x);
        DoubleNode (F, 'PositionPoint.yPosition', Buses^[i].y);
        Writeln(F,'</cim:ConnectivityNode>');
      end;
    end;

    pFdr := ActiveCircuit.Feeders.First;
    while pFdr <> nil do begin
      with pFdr do begin
        StartInstance (F, 'Feeder', 'Fdr', Name);
        EndInstance (F, 'Feeder');
      end;
      pFdr := ActiveCircuit.Feeders.Next;
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
        VoltageLevelNode (F, 'Equipment', kvFdr);
        PhasesNode (F, 'ConductingEquipment.phases', pCap);
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
        StartInstance (F, 'RegulatingControl', 'CapC', Name);
        CapControlRefNodes (F, This_Capacitor.Name, ElementName);
        if CapControlType = 5 then begin
          v1 := OnValue;
          v2 := OffValue
        end else begin
          v1 := PfOnValue;
          v2 := PfOffValue;
        end;
        case CapControlType of
          1: StringNode (F, 'RegulatingControl.mode', 'currentFlow');
          2: StringNode (F, 'RegulatingControl.mode', 'voltage');
          3: StringNode (F, 'RegulatingControl.mode', 'reactivePower');
          4: StringNode (F, 'RegulatingControl.mode', '***time');
          5: StringNode (F, 'RegulatingControl.mode', '***powerFactor');
        end;
        DoubleNode (F, 'RegulatingControl.targetValue', v1);
        DoubleNode (F, 'RegulatingControl.targetRange', v2);
        EndInstance (F, 'RegulatingControl');
      end;
      pCapC := ActiveCircuit.CapControls.Next;
    end;

    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      with pXf do begin
        StartInstance (F, 'PowerTransformer', 'Xf', Name);
        if IsSubstation then SubRefNode (F, 'PowerTransformer', SubstationName);
        PhasesNode (F, 'PowerTransformer.phases', pXf);
        EndInstance (F, 'PowerTransformer');
        for i := 1 to NumberOfWindings do begin
          Str (i, WdgName);
          WdgName := Name + '_' + WdgName;
          StartInstance (F, 'TransformerWinding', 'Wdg', WdgName);
          XfRefNode (F, Name);
          BaseVoltageNode (F, 'ConductingEquipment.BaseVoltage', 0.001 * BaseVoltage[i]);
          DoubleNode (F, 'TransformerWinding.r', WdgResistance[i]);
          DoubleNode (F, 'TransformerWinding.x', XscVal[i]);
          DoubleNode (F, 'TransformerWinding.ratedKV', 0.001 * BaseVoltage[i]);
          DoubleNode (F, 'TransformerWinding.ratedMVA', 0.001 * WdgKVA[i]);
          case i of
            1 : StringNode (F, 'TransformerWinding.windingType', 'primary');
            2 : StringNode (F, 'TransformerWinding.windingType', 'secondary');
            3 : StringNode (F, 'TransformerWinding.windingType', 'tertiary');
            4 : StringNode (F, 'TransformerWinding.windingType', 'quaternary');
          end;
          if WdgConnection[i] > 0 then
            StringNode (F, 'TransformerWinding.connectionType', 'D')
          else
            StringNode (F, 'TransformerWinding.connectionType', 'Y');
          EndInstance (F, 'TransformerWinding');
        end;
      end;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    pReg := ActiveCircuit.RegControls.First;
    while pReg <> nil do begin
      with pReg do begin
        StartInstance (F, 'RatioTapChanger', 'Reg', Name);
        EndInstance (F, 'RatioTapChanger');
      end;
      pReg := ActiveCircuit.RegControls.Next;
    end;

    pLine := ActiveCircuit.Lines.First;
    while pLine <> nil do begin
      with pLine do begin
        StartInstance (F, 'Line', 'Line', Name);
        EndInstance (F, 'Line');
        WriteTerminals (F, pLine, 'Line', Name);

        StartInstance (F, 'ACLineSegment', 'Seg', Name);
        LineRefNode (F, Name);
        BaseVoltageNode (F, 'ConductingEquipment', kvFdr);
        DoubleNode (F, 'Conductor.length', Len);
        PhasesNode (F, 'ConductingEquipment.phases', pLine);
        DoubleNode (F, 'Conductor.r', R1);
        DoubleNode (F, 'Conductor.x', X1);
        DoubleNode (F, 'Conductor.bch', C1);
        DoubleNode (F, 'Conductor.r0', R0);
        DoubleNode (F, 'Conductor.x0', X0);
        DoubleNode (F, 'Conductor.bch0', C0);

        StringNode (F, 'CondCode', CondCode);
        if GeometrySpecified then StringNode (F, 'Geometry', GeometryCode);

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
