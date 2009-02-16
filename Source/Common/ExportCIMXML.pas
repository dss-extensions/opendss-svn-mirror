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
     Vsource, Line, Transformer, Ucomplex, UcMatrix, LineCode,
     Fuse, Capacitor, CapControl, Reactor, Feeder, WireData,
     LineGeometry;

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

procedure TapRefNode (var F: TextFile; pXf: TTransfObj; Wdg: Integer);
var
  WdgName: String;
begin
  Str (Wdg, WdgName);
  WdgName := pXf.Name + '_' + WdgName;
  Writeln(F,
    Format('  <cim:RatioTapChanger.transformerWinding rdf:resource="#Wdg_%s"/>',
    [WdgName]));
end;

procedure RegRefNode (var F: TextFile; Tap: String);
begin
  Writeln(F,
    Format('  <cim:RegulatingControl.ratioTapChanger rdf:resource="#Tap_%s"/>',
    [Tap]));
end;

procedure SchedRefNode (var F: TextFile; Reg: String);
begin
  Writeln(F,
    Format('  <cim:RegulationSchedule.regulatingControl rdf:resource="#RegCtrl_%s"/>',
    [Reg]));
end;

procedure ArrCondRefNode (var F: TextFile; Cond: String);
begin
  Writeln(F,
    Format('  <cim:WireArrangement.MemberOf_ConductorType rdf:resource="#Cond_%s"/>',
    [Cond]));
end;

procedure ArrWireRefNode (var F: TextFile; Wire: String);
begin
  Writeln(F,
    Format('  <cim:WireArrangement.WireType rdf:resource="#Wire_%s"/>',
    [Wire]));
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

procedure MatrixNode (var F: TextFile; Node: String; m: TCMatrix; n: Integer; imag: Boolean; scale: Double);
var
  i, j: Integer;
  v : Double;
begin
  Write (F, Format ('  <cim:%s>', [Node]));
  for i := 1 to n do begin
    for j := 1 to i do begin
      if imag then v := m.GetElement(i, j).im else v := m.GetElement(i,j).re;
      v := scale * v;
      Write (F, v:0:8);
      if j < i then Write (F, ',');
    end;
    if i < n then Write (F, '|');
  end;
  Writeln (F, Format ('</cim:%s>', [Node]));
end;

procedure StartInstance (var F: TextFile; Root: String; Abbrev: String; Name: String);
begin
  Writeln(F, Format('<cim:%s rdf:ID="%s_%s">', [Root, Abbrev, Name]));
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
  i1, i2, i3 : Integer;
  Zs, Zm : complex;
  Rs, Rm, Xs, Xm, R1, R0, X1, X0: double;
  WdgName : String;
  ArrName : String;

  pLoad  : TLoadObj;
  pVsrc  : TVsourceObj;
  pGen   : TGeneratorObj;

  pCap  : TCapacitorObj;
  pCapC : TCapControlObj;
  pXf   : TTransfObj;
  pReg  : TRegControlObj;
  pLine : TLineObj;

  pFdr  : TFeederObj;
  kvFdr : double;

  clsCode : TLineCode;
  clsGeom : TLineGeometry;
  clsWire : TWireData;

  pCode : TLineCodeObj;
  pGeom : TLineGeometryObj;
  pWire : TWireDataObj;

Begin
  Try
    Assignfile(F, FileNm);
    ReWrite(F);
    kvFdr := 0.0;

    Writeln(F,'<?xml version="1.0" encoding="utf-8"?>');
    Writeln(F,'<!-- un-comment this line to enable validation');
    Writeln(F,'-->');
    Writeln(F,'<rdf:RDF xmlns:cim="http://iec.ch/TC57/2008/CIM-schema-cim13#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">');
    Writeln(F,'<!--');
    Writeln(F,'-->');
    with ActiveCircuit do begin
      i:=1;
      val:=LegalVoltageBases^[i];
      while val > 0.0 do begin
        Writeln(F, Format('<cim:BaseVoltage rdf:ID="BaseVoltage_%.3f">', [val]));
        PrefixVbaseNode (F, 'IdentifiedObject.name', 'BaseVoltage', val);
        DoubleNode (F, 'BaseVoltage.nominalVoltage', val);
        Writeln(F,'</cim:BaseVoltage>');

        Writeln(F, Format('<cim:VoltageLevel rdf:ID="VoltageLevel_%.3f">', [val]));
        PrefixVbaseNode (F, 'IdentifiedObject.name', 'VoltageLevel', val);
        Writeln(F, Format('  <cim:VoltageLevel.BaseVoltage rdf:resource="#BaseVoltage_%.3f"/>', [val]));
        DoubleNode (F, 'VoltageLevel.lowVoltageLimit', val * NormalMinVolts);
        DoubleNode (F, 'VoltageLevel.highVoltageLimit', val * NormalMaxVolts);
        Writeln(F,'</cim:VoltageLevel>');

        Inc(i);
        val:=LegalVoltageBases^[i];
      end;

      for i := 1 to NumBuses do begin
        Writeln(F, Format('<cim:ConnectivityNode rdf:ID="CN_%s">', [BusList.Get(i)]));
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

    pVsrc := ActiveCircuit.Sources.First; // pIsrc are in the same list
    while pVsrc <> nil do begin
      if pVsrc.ClassNameIs('TVSourceObj') then
        with pVsrc do begin
          StartInstance (F, 'Substation', 'Sub', Name);
          EndInstance (F, 'Substation');

          Zs := Z.AvgDiagonal;
          Zm := Z.AvgOffDiagonal;
          Rs := Zs.re;
          Rm := Zm.re;
          Xs := Zs.im;
          Xm := Zm.im;
          v1 := pVsrc.NPhases;
          R1 := (Rs - Rm) / v1;
          X1 := (Xs - Xm) / v1;
          R0 := (Rs + (v1 - 1.0) * Rm) / v1;
          X0 := (Xs + (v1 - 1.0) * Xm) / v1;

          StartInstance (F, 'EnergySource', 'Eq', Name);
          VoltageLevelNode (F, 'Equipment', kVbase);
          PhasesNode (F, 'ConductingEquipment.phases', pVsrc);
          DoubleNode (F, 'EnergySource.nominalVoltage', kVbase);
          DoubleNode (F, 'EnergySource.voltageMagnitude', kVbase * PerUnit);
          DoubleNode (F, 'EnergySource.voltageAngle', TwoPi * Angle / 180.0);
          DoubleNode (F, 'EnergySource.r', R1);
          DoubleNode (F, 'EnergySource.x', X1);
          DoubleNode (F, 'EnergySource.r0', R0);
          DoubleNode (F, 'EnergySource.x0', X0);
          EndInstance (F, 'EnergySource');
          WriteTerminals (F, pVsrc, 'Eq', Name);
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
        IntegerNode (F, 'RegulatingControl.discrete', 1);
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
          // Xsc is an upper triangle array, X12, X13, X23
          // but the star equivalent doesn't even work for more than 3 windings
          i1:=1; i2:=1; i3:=1; // for 2 windings, placeholder for nwdg > 3
          if NumberOfWindings = 3 then
            case i of
              1 : begin i1:=1; i2:=2; i3:=3 end;
              2 : begin i1:=1; i2:=3; i3:=2 end;
              3 : begin i1:=2; i2:=3; i3:=1 end;
            end;
          v1 := 0.5 * (XscVal[i1] + XscVal[i2] - XscVal[i3]);
          DoubleNode (F, 'TransformerWinding.x', v1);
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
        StartInstance (F, 'RatioTapChanger', 'Tap', Name);
        TapRefNode (F, Transformer, TrWinding);
        i := NumTaps;
        IntegerNode (F, 'TapChanger.highStep', i);
        IntegerNode (F, 'TapChanger.lowStep', 0);
        IntegerNode (F, 'TapChanger.neutralStep', i div 2);
        IntegerNode (F, 'TapChanger.normalStep', i div 2);
        DoubleNode (F, 'TapChanger.neutralU', 120.0 * PT);
        DoubleNode (F, 'TapChanger.stepVoltageIncrement', 100.0 * TapIncrement);
        DoubleNode (F, 'TapChanger.initialDelay', InitialDelay);
        DoubleNode (F, 'TapChanger.subsequentDelay', SubsequentDelay);
        StringNode (F, 'TapChanger.tculControlMode', 'volt'); // what is 'local'?
        StringNode (F, 'TapChanger.type', 'voltageControl');
        EndInstance (F, 'RatioTapChanger');

        StartInstance (F, 'RegulatingControl', 'RegCtrl', Name);
        RegRefNode (F, Name);
        IntegerNode (F, 'RegulatingControl.discrete', 1);
        StringNode (F, 'RegulatingControl.mode', 'voltage');
        DoubleNode (F, 'RegulatingControl.targetValue', PT * TargetVoltage);
        DoubleNode (F, 'RegulatingControl.targetRange', PT * BandVoltage);
        EndInstance (F, 'RegulatingControl');

        StartInstance (F, 'RegulationSchedule', 'RegSched', Name);
        SchedRefNode (F, Name);
        if UseLineDrop then i:=1 else i:=0;
        IntegerNode (F, 'RegulationSchedule.lineDropCompensation', i);
        DoubleNode (F, 'RegulationSchedule.lineDropR', LineDropR);
        DoubleNode (F, 'RegulationSchedule.lineDropX', LineDropX);
        EndInstance (F, 'RegulationSchedule');
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

    clsCode := DSSClassList.Get(ClassNames.Find('linecode'));
    pCode := clsCode.ElementList.First;
    while pCode <> nil do begin
      StartInstance (F, 'LineCode', 'Code', pCode.Name);
      IntegerNode (F, 'numPhases', pCode.FNPhases);
      DoubleNode (F, 'baseFreq', pCode.BaseFrequency);
      DoubleNode (F, 'r', pCode.R1);
      DoubleNode (F, 'x', pCode.X1);
      DoubleNode (F, 'c', 1.0e9 * pCode.C1);
      DoubleNode (F, 'r0', pCode.R0);
      DoubleNode (F, 'x0', pCode.X0);
      DoubleNode (F, 'c0', 1.0e9 * pCode.C0);
      MatrixNode (F, 'rMatrix', pCode.Z, pCode.FNPhases,
        False, 1.0);
      MatrixNode (F, 'xMatrix', pCode.Z, pCode.FNPhases,
        True, 1.0);
      MatrixNode (F, 'cMatrix', pCode.YC, pCode.FNPhases,
        True, 1.0e9 / TwoPi / pCode.BaseFrequency);
      EndInstance (F, 'LineCode');
      pCode := clsCode.ElementList.Next;
    end;

    clsWire := DSSClassList.Get(ClassNames.Find('wiredata'));
    pWire := clsWire.ElementList.First;
    while pWire <> nil do begin
      StartInstance (F, 'WireType', 'Wire', pWire.Name);
      DoubleNode (F, 'WireType.gMR', pWire.GMR);
      DoubleNode (F, 'WireType.radius', pWire.Radius);
      DoubleNode (F, 'WireType.resistance', pWire.Rac);
      DoubleNode (F, 'WireType.ratedCurrent', pWire.NormAmps);
      IntegerNode (F, 'WireType.phaseConductorCount', 1);
      EndInstance (F, 'WireType');
      pWire := clsWire.ElementList.Next;
    end;

    clsGeom := DSSClassList.Get(ClassNames.Find('linegeometry'));
    pGeom := clsGeom.ElementList.First;
    while pGeom <> nil do begin
      StartInstance (F, 'ConductorType', 'Cond', pGeom.Name);
      EndInstance (F, 'ConductorType');

      for i := 1 to pGeom.Nconds do begin
        Str (i, ArrName);
        ArrName := pGeom.Name + '_' + ArrName;
        StartInstance (F, 'WireArrangement', 'Arr', ArrName);
        IntegerNode (F, 'WireArrangement.sequence', i);
        ArrCondRefNode (F, pGeom.Name);
        ArrWireRefNode (F, pGeom.WireName[i]);
        DoubleNode (F, 'WireArrangement.mountingPointX', pGeom.Xcoord[i]);
        DoubleNode (F, 'WireArrangement.mountingPointY', pGeom.Ycoord[i]);
        EndInstance (F, 'WireArrangement');
      end;

      pGeom := clsGeom.ElementList.Next;
    end;

    Writeln (F, '</rdf:RDF>');
    GlobalResult := FileNm;
  Finally
    CloseFile(F);
  End;
End;

end.
