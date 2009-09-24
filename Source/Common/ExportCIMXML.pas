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

Uses SysUtils, Utilities, Circuit, DSSClassDefs, DSSGlobals, CktElement,
     PDElement, PCElement, Generator, Load, RegControl,
     Vsource, Line, Transformer, Ucomplex, UcMatrix, LineCode,
     Fuse, Capacitor, CapControl, Reactor, Feeder, WireData,
     LineGeometry, NamedObject, StrUtils, Math;

procedure DoubleNode (var F: TextFile; Node: String; val: Double);
begin
  Writeln (F, Format ('  <cim:%s>%g</cim:%s>', [Node, val, Node]));
end;

procedure IntegerNode (var F: TextFile; Node: String; val: Integer);
begin
  Writeln (F, Format ('  <cim:%s>%d</cim:%s>', [Node, val, Node]));
end;

procedure BooleanNode (var F: TextFile; Node: String; val: Boolean);
var
  i: String;
begin
  if val then i := 'true' else i := 'false';
  Writeln (F, Format ('  <cim:%s>%s</cim:%s>', [Node, i, Node]));
end;

procedure RefNode (var F: TextFile; Node: String; Obj: TNamedObject);
begin
  Writeln (F, Format ('  <cim:%s rdf:resource="#%s"/>', [Node, Obj.ID]));
end;

procedure GuidNode (var F: TextFile; Node: String; ID: TGuid);
begin
  Writeln (F, Format ('  <cim:%s rdf:resource="#%s"/>', [Node, GuidToString (ID)]));
end;

procedure LineCodeRefNode (var F: TextFile; List: TLineCode; Name: String);
var
  Obj : TLineCodeObj;
begin
  if List.SetActive (Name) then begin
    Obj := List.GetActiveObj;
    if Obj.SymComponentsModel then
      Writeln (F, Format ('  <cim:SequenceImpedance rdf:resource="#%s"/>', [Obj.ID]))
    else
      Writeln (F, Format ('  <cim:PhaseImpedance rdf:resource="#%s"/>', [Obj.ID]));
  end;
end;

procedure GeometryRefNode (var F: TextFile; List: TLineGeometry; Name: String);
var
  Obj : TLineGeometryObj;
begin
  if List.SetActive (Name) then begin
    Obj := List.GetActiveObj;
    Writeln (F, Format ('  <cim:OverheadConductorInfo rdf:resource="#%s"/>', [Obj.ID]))
  end;
end;

procedure CircuitNode (var F: TextFile; Obj: TNamedObject);
begin
  Writeln(F, Format('  <cim:Equipment.MemberOf_Line rdf:resource="#%s"/>', [Obj.ID]));
end;

procedure XfRefNode (var F: TextFile; Name: String);
begin
  Writeln(F, Format('  <cim:Transformer.MemberOf_PowerTransformer rdf:resource="#Xf_%s"/>',
    [Name]));
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

procedure StartInstance (var F: TextFile; Root: String; Obj: TNamedObject);
begin
  Writeln(F, Format('<cim:%s rdf:ID="%s">', [Root, Obj.ID]));
  StringNode (F, 'IdentifiedObject.name', Obj.LocalName);
end;

procedure StartFreeInstance (var F: TextFile; Root: String);
var
  temp: TGUID;
begin
  CreateGUID (temp);
  Writeln(F, Format('<cim:%s rdf:ID="%s">', [Root, GUIDToString (temp)]));
end;

procedure EndInstance (var F: TextFile; Root: String);
begin
  Writeln (F, Format ('</cim:%s>', [Root]));
end;

procedure WriteLoadModel (var F: TextFile; Name: String; ID: TGuid;
  zP: Double; iP: Double; pP: Double; zQ: Double; iQ: Double; pQ: Double;
  eP: Double; eQ: Double);
begin
  Writeln(F, Format('<cim:LoadResponseCharacteristic rdf:ID="%s">', [GuidToString(ID)]));
  StringNode (F, 'IdentifiedObject.name', Name);
  if (eP > 0.0) or (eQ > 0.0) then
    BooleanNode (F, 'LoadResponseCharacteristic.exponentModel', true)
  else
    BooleanNode (F, 'LoadResponseCharacteristic.exponentModel', false);

  DoubleNode (F, 'LoadResponseCharacteristic.pConstantImpedance', zP);
  DoubleNode (F, 'LoadResponseCharacteristic.pConstantCurrent', iP);
  DoubleNode (F, 'LoadResponseCharacteristic.pConstantPower', pP);

  DoubleNode (F, 'LoadResponseCharacteristic.qConstantImpedance', zQ);
  DoubleNode (F, 'LoadResponseCharacteristic.qConstantCurrent', iQ);
  DoubleNode (F, 'LoadResponseCharacteristic.qConstantPower', pQ);

  DoubleNode (F, 'LoadResponseCharacteristic.pVoltageExponent', eP);
  DoubleNode (F, 'LoadResponseCharacteristic.qVoltageExponent', eQ);
  DoubleNode (F, 'LoadResponseCharacteristic.pFrequencyExponent', 0.0);
  DoubleNode (F, 'LoadResponseCharacteristic.qFrequencyExponent', 0.0);
  Writeln (F, '</cim:LoadResponseCharacteristic>');
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

procedure WritePositions(var F:TextFile; pElem:TDSSCktElement);
var
  Nterm, j, ref : Integer;
  BusName : String;
  temp: TGUID;
begin
  Nterm := pElem.Nterms;
  BusName := pElem.FirstBus;
  CreateGUID (temp);
  Writeln(F, Format('<cim:GeoLocation rdf:ID="%s">', [GUIDToString(temp)]));
  RefNode (F, 'PowerSystemResource', pElem);
  EndInstance (F, 'GeoLocation');

  for j := 1 to NTerm do begin
    if IsGroundBus (BusName) = False then begin
      ref := pElem.Terminals^[j].BusRef;
      StartFreeInstance (F, 'PositionPoint');
      GuidNode (F, 'PositionPoint.Location', temp);
      IntegerNode (F, 'PositionPoint.sequenceNumber', j);
      StringNode (F, 'PositionPoint.xPosition', FloatToStr (ActiveCircuit.Buses^[ref].x));
      StringNode (F, 'PositionPoint.yPosition', FloatToStr (ActiveCircuit.Buses^[ref].y));
      EndInstance (F, 'PositionPoint');
    end;
    BusName := pElem.Nextbus;
  end;
end;

procedure WriteTerminals(var F:TextFile; pElem:TDSSCktElement);
var
  Nterm, j, ref : Integer;
  BusName, TermName : String;
  temp: TGUID;
begin
  Nterm := pElem.Nterms;
  BusName := pElem.FirstBus;
  for j := 1 to NTerm do begin
    if IsGroundBus (BusName) = False then begin
      ref := pElem.Terminals^[j].BusRef;
      Str (j, TermName);
      TermName := pElem.Name + '_T' + TermName;
      CreateGUID (temp);

      Writeln(F, Format('<cim:Terminal rdf:ID="%s">', [GUIDToString(temp)]));
      StringNode (F, 'IdentifiedObject.name', TermName);
      IntegerNode (F, 'Terminal.sequenceNumber', j);
      Writeln (F, Format('  <cim:Terminal.ConductingEquipment rdf:resource="#%s"/>',
        [pElem.ID]));
      Writeln (F, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
        [ActiveCircuit.Buses[ref].ID]));
      EndInstance (F, 'Terminal');
    end;

    BusName := pElem.Nextbus;
  end;
  WritePositions (F, pElem);
end;

procedure WriteWdgTerminals(var F:TextFile; pXf:TTransfObj);
var
  i : Integer;
  BusName, WdgName, TermName : String;
begin
  BusName := pXf.FirstBus;
  for i := 1 to pXf.NumberOfWindings do begin
    BusName := StripExtension (BusName);
    Str (i, WdgName);
    WdgName := 'Wdg_' + pXf.Name + '_' + WdgName;
    TermName := WdgName + '_T1';

//    StartInstance (F, 'Terminal', 'Trm', TermName);
    StringNode (F, 'IdentifiedObject.name', TermName);
    Writeln (F, Format('  <cim:Terminal.ConductingEquipment rdf:resource="#%s"/>',
      [WdgName]));
    Writeln (F, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#CN_%s"/>',
      [BusName]));
    EndInstance (F, 'Terminal');

    BusName := pXf.Nextbus;
  end;
end;

Procedure ExportCDPSM(FileNm:String);
Var
  F      : TextFile;
  i, j   : Integer;
  seq    : Integer;
  val    : double;
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

  clsCode : TLineCode;
  clsGeom : TLineGeometry;
  clsWire : TWireData;

  pCode : TLineCodeObj;
  pGeom : TLineGeometryObj;
  pWire : TWireDataObj;

  // DSS-like load models
  id1_ConstkVA:     TGuid;
  id2_ConstZ:       TGuid;
  id3_ConstPQuadQ:  TGuid;
  id4_LinPQuadQ:    TGuid;
  id5_ConstI:       TGuid;
  id6_ConstPConstQ: TGuid;  // P can vary, Q not
  id7_ConstPConstX: TGuid;
Begin
  Try
    clsCode := DSSClassList.Get(ClassNames.Find('linecode'));
    clsWire := DSSClassList.Get(ClassNames.Find('wiredata'));
    clsGeom := DSSClassList.Get(ClassNames.Find('linegeometry'));

    Assignfile(F, FileNm);
    ReWrite(F);

    Writeln(F,'<?xml version="1.0" encoding="utf-8"?>');
    Writeln(F,'<!-- un-comment this line to enable validation');
    Writeln(F,'-->');
    Writeln(F,'<rdf:RDF xmlns:cim="http://iec.ch/TC57/2008/CIM-schema-cim14#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">');
    Writeln(F,'<!--');
    Writeln(F,'-->');

    StartInstance (F, 'Line', ActiveCircuit);
    EndInstance (F, 'Line');

    with ActiveCircuit do begin
      for i := 1 to NumBuses do begin
        Buses^[i].LocalName:= BusList.Get(i);
      end;

      for i := 1 to NumBuses do begin
        Writeln(F, Format('<cim:ConnectivityNode rdf:ID="%s">',
          [GUIDToString (Buses^[i].GUID)]));
        StringNode (F, 'IdentifiedObject.name', Buses^[i].LocalName);
        Writeln(F,'</cim:ConnectivityNode>');
      end;
    end;

    pGen := ActiveCircuit.Generators.First;
    while pGen <> nil do begin
      with pGen do begin
        StartInstance (F, 'EquivalentGenerator', pGen);
        CircuitNode (F, ActiveCircuit);
        EndInstance (F, 'EquivalentGenerator');
      end;
      pGen := ActiveCircuit.Generators.Next;
    end;

    pVsrc := ActiveCircuit.Sources.First; // pIsrc are in the same list
    while pVsrc <> nil do begin
      if pVsrc.ClassNameIs('TVSourceObj') then
        with pVsrc do begin
          Zs := Z.AvgDiagonal;
          Zm := Z.AvgOffDiagonal;
          Rs := Zs.re;
          Rm := Zm.re;
          Xs := Zs.im;
          Xm := Zm.im;
          v1 := pVsrc.NPhases;
          if v1 > 1.0 then begin
            R1 := Rs - Rm;
            X1 := Xs - Xm;
            R0 := Rs + (v1 - 1.0) * Rm;
            X0 := Xs + (v1 - 1.0) * Xm;
          end else begin
            R1 := Rs;
            X1 := Xs;
            R0 := Rs;
            X0 := Xs;
          end;

          StartInstance (F, 'EnergySource', pVsrc);
          CircuitNode (F, ActiveCircuit);
          PhasesNode (F, 'ConductingEquipment.phases', pVsrc);
          DoubleNode (F, 'EnergySource.nominalVoltage', kVbase);
          DoubleNode (F, 'EnergySource.voltageMagnitude', kVbase * PerUnit);
          DoubleNode (F, 'EnergySource.voltageAngle', TwoPi * Angle / 360.0);
          DoubleNode (F, 'EnergySource.r', R1);
          DoubleNode (F, 'EnergySource.x', X1);
          DoubleNode (F, 'EnergySource.r0', R0);
          DoubleNode (F, 'EnergySource.x0', X0);
          EndInstance (F, 'EnergySource');
          WriteTerminals (F, pVsrc);
        end;
      pVsrc := ActiveCircuit.Sources.Next;
    end;

    pCap := ActiveCircuit.ShuntCapacitors.First;
    while pCap <> nil do begin
      with pCap do begin
        StartInstance (F, 'ShuntCompensator', pCap);
        CircuitNode (F, ActiveCircuit);
        PhasesNode (F, 'ConductingEquipment.phases', pCap);
        DoubleNode (F, 'ShuntCompensator.nomU', NomKV);
        DoubleNode (F, 'ShuntCompensator.nomQ', TotalKvar);
        DoubleNode (F, 'ShuntCompensator.reactivePerSection', TotalKvar / NumSteps);
        IntegerNode (F, 'ShuntCompensator.normalSections', NumSteps);
        IntegerNode (F, 'ShuntCompensator.maximumSections', NumSteps);
        EndInstance (F, 'ShuntCompensator');
        WriteTerminals (F, pCap);
      end;
      pCap := ActiveCircuit.ShuntCapacitors.Next;
    end;

    pCapC := ActiveCircuit.CapControls.First;
    while pCapC <> nil do begin
      with pCapC do begin
        StartInstance (F, 'RegulatingControl', pCapC);
        RefNode (F, 'RegulatingCondEq', This_Capacitor);
        if CapControlType = 5 then begin
          v1 := PfOnValue;
          v2 := PfOffValue
        end else begin
          v1 := OnValue;
          v2 := OffValue
        end;
        case CapControlType of
          1: StringNode (F, 'RegulatingControl.mode', 'currentFlow');
          2: StringNode (F, 'RegulatingControl.mode', 'voltage');
          3: StringNode (F, 'RegulatingControl.mode', 'reactivePower');
          4: StringNode (F, 'RegulatingControl.mode', 'timeScheduled');
          5: StringNode (F, 'RegulatingControl.mode', 'powerFactor');
        end;
        BooleanNode (F, 'RegulatingControl.discrete', true);
        DoubleNode (F, 'RegulatingControl.targetValue', v1);
        DoubleNode (F, 'RegulatingControl.targetRange', v2);
        EndInstance (F, 'RegulatingControl');
      end;
      pCapC := ActiveCircuit.CapControls.Next;
    end;

    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      with pXf do begin
        StartInstance (F, 'DistributionTransformer', pXf);
        PhasesNode (F, 'PowerTransformer.phases', pXf);
        EndInstance (F, 'PowerTransformer');
        for i := 1 to NumberOfWindings do begin
          Str (i, WdgName);
          WdgName := Name + '_' + WdgName;
          StartInstance (F, 'TransformerWinding', pXf); // wdg
          XfRefNode (F, Name);
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
        WriteWdgTerminals (F, pXf);
      end;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    pReg := ActiveCircuit.RegControls.First;
    while pReg <> nil do begin
      with pReg do begin
        StartInstance (F, 'DistributionTapChanger', pReg);
        TapRefNode (F, Transformer, TrWinding);
        i := NumTaps;
        IntegerNode (F, 'TapChanger.highStep', i);
        IntegerNode (F, 'TapChanger.lowStep', 0);
        IntegerNode (F, 'TapChanger.neutralStep', NumTaps div 2);
        IntegerNode (F, 'TapChanger.normalStep', NumTaps div 2);
        DoubleNode (F, 'TapChanger.neutralU', 120.0 * PT);
        DoubleNode (F, 'TapChanger.stepVoltageIncrement', 100.0 * TapIncrement);
        DoubleNode (F, 'TapChanger.initialDelay', InitialDelay);
        DoubleNode (F, 'TapChanger.subsequentDelay', SubsequentDelay);
        BooleanNode (F, 'TapChanger.ltcFlag', True);
        BooleanNode (F, 'TapChanger.regulationStatus', True);

        StringNode (F, 'RatioTapChanger.tculControlMode', 'volt');

        DoubleNode (F, 'DistributionTapChanger.ptRatio', PT);
        DoubleNode (F, 'DistributionTapChanger.ctRatio', CT);
        DoubleNode (F, 'DistributionTapChanger.targetVoltage', PT * TargetVoltage);
        DoubleNode (F, 'DistributionTapChanger.bandVoltage', PT * BandVoltage);
        BooleanNode (F, 'DistributionTapChanger.lineDropCompensation', UseLineDrop);
        DoubleNode (F, 'DistributionTapChanger.lineDropR', LineDropR);
        DoubleNode (F, 'DistributionTapChanger.lineDropX', LineDropX);
        if UseReverseDrop then begin
          DoubleNode (F, 'DistributionTapChanger.reverseLineDropR', RevLineDropR);
          DoubleNode (F, 'DistributionTapChanger.reverseLineDropX', RevLineDropX)
        end else begin
          DoubleNode (F, 'DistributionTapChanger.reverseLineDropR', 0.0);
          DoubleNode (F, 'DistributionTapChanger.reverseLineDropX', 0.0)
        end;
        if UseLimit then
          DoubleNode (F, 'DistributionTapChanger.limitVoltage', VoltageLimit)
        else
          DoubleNode (F, 'DistributionTapChanger.limitVoltage', 0.0);
        StringNode (F, 'DistributionTapChanger.monitoredPhase', 'A'); // TBD
        EndInstance (F, 'DistributionTapChanger');

        StartFreeInstance (F, 'SvTapStep');
        RefNode (F, 'TapChanger', pReg);
        IntegerNode (F, 'position', NumTaps div 2);
        DoubleNode (F, 'continuousPosition', 1.0);
        EndInstance (F, 'SvTapStep');
      end;
      pReg := ActiveCircuit.RegControls.Next;
    end;

    pLine := ActiveCircuit.Lines.First;
    while pLine <> nil do begin
      with pLine do begin
        if IsSwitch then begin
          StartInstance (F, 'LoadBreakSwitch', pLine);
          CircuitNode (F, ActiveCircuit);
          PhasesNode (F, 'ConductingEquipment.phases', pLine);
          if pLine.Closed[0] then
            StringNode (F, 'Switch.normalOpen', 'false')
          else
            StringNode (F, 'Switch.normalOpen', 'true');
          EndInstance (F, 'LoadBreakSwitch');
        end else begin
          if not LineCodeSpecified and not GeometrySpecified then begin
            val := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
            StartInstance (F, 'ACLineSegment', pLine);
            CircuitNode (F, ActiveCircuit);
            DoubleNode (F, 'Conductor.length', Len);
            PhasesNode (F, 'ConductingEquipment.phases', pLine);
            DoubleNode (F, 'ACLineSegment.r', R1);
            DoubleNode (F, 'ACLineSegment.x', X1);
            DoubleNode (F, 'ACLineSegment.bch', C1 * val);
            DoubleNode (F, 'ACLineSegment.r0', R0);
            DoubleNode (F, 'ACLineSegment.x0', X0);
            DoubleNode (F, 'ACLineSegment.b0ch', C0 * val);
            EndInstance (F, 'ACLineSegment')
          end else begin
            StartInstance (F, 'DistributionLineSegment', pLine);
            CircuitNode (F, ActiveCircuit);
            DoubleNode (F, 'Conductor.length', Len);
            PhasesNode (F, 'ConductingEquipment.phases', pLine);
            if GeometrySpecified then GeometryRefNode (F, clsGeom, GeometryCode);
            if LineCodeSpecified then LineCodeRefNode (F, clsCode, CondCode);
            EndInstance (F, 'DistributionLineSegment')
          end;
        end;
        WriteTerminals (F, pLine);
      end;
      pLine := ActiveCircuit.Lines.Next;
    end;

    // create the DSS-like load models
    CreateGuid (id1_ConstkVA);
    CreateGuid (id2_ConstZ);
    CreateGuid (id3_ConstPQuadQ);
    CreateGuid (id4_LinPQuadQ);
    CreateGuid (id5_ConstI);
    CreateGuid (id6_ConstPConstQ);  // P can vary, Q not
    CreateGuid (id7_ConstPConstX);

    WriteLoadModel (F, 'Constant kVA', id1_ConstkVA,
      0, 0, 100,
      0, 0, 100,
      0, 0);
    WriteLoadModel (F, 'Constant Z', id2_ConstZ,
      100, 0, 0,
      100, 0, 0,
      0, 0);
    WriteLoadModel (F, 'Motor', id3_ConstPQuadQ,
      0, 0, 100,
      100, 0, 0,
      0, 0);
    WriteLoadModel (F, 'Mix Motor/Res', id4_LinPQuadQ,
      0, 0, 0,
      0, 0, 0,
      1, 2);
    WriteLoadModel (F, 'Constant I', id5_ConstI,
      0, 100, 0,
      0, 100, 0,
      0, 0);
    WriteLoadModel (F, 'Variable P, Fixed Q', id6_ConstPConstQ,
      0, 0, 100,
      0, 0, 100,
      0, 0);
    WriteLoadModel (F, 'Variable P, Fixed X', id7_ConstPConstX,
      0, 0, 100,
      100, 0, 0,
      0, 0);

    pLoad := ActiveCircuit.Loads.First;
    while pLoad <> nil do begin
      if pLoad.Enabled then
        with pLoad do begin
          StartInstance (F, 'EnergyConsumer', pLoad);
          CircuitNode (F, ActiveCircuit);
          PhasesNode (F, 'ConductingEquipment.phases', pLoad);
          case FLoadModel of
            1: GuidNode (F, 'LoadResponse', id1_ConstkVA);
            2: GuidNode (F, 'LoadResponse', id2_ConstZ);
            3: GuidNode (F, 'LoadResponse', id3_ConstPQuadQ);
            4: GuidNode (F, 'LoadResponse', id4_LinPQuadQ);
            5: GuidNode (F, 'LoadResponse', id5_ConstI);
            6: GuidNode (F, 'LoadResponse', id6_ConstPConstQ);
            7: GuidNode (F, 'LoadResponse', id7_ConstPConstX);
          end;
          DoubleNode (F, 'EnergyConsumer.qfixed', kvarBase);
          DoubleNode (F, 'EnergyConsumer.pfixed', kWBase);
          IntegerNode (F, 'EnergyConsumer.customerCount', NumCustomers);
          EndInstance (F, 'EnergyConsumer');
          WriteTerminals (F, pLoad);
        end;
        pLoad := ActiveCircuit.Loads.Next;
    end;

    pCode := clsCode.ElementList.First;
    while pCode <> nil do begin
      with pCode do begin
        if SymComponentsModel then begin
          val := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
          StartInstance (F, 'PerLengthSequenceImpedance', pCode);
          DoubleNode (F, 'PerLengthSequenceImpedance.r', R1);
          DoubleNode (F, 'PerLengthSequenceImpedance.x', X1);
          DoubleNode (F, 'PerLengthSequenceImpedance.bch', C1 * val);
          DoubleNode (F, 'PerLengthSequenceImpedance.r0', R0);
          DoubleNode (F, 'PerLengthSequenceImpedance.x0', X0);
          DoubleNode (F, 'PerLengthSequenceImpedance.b0ch', C0 * val);
          EndInstance (F, 'PerLengthSequenceImpedance')
        end else begin
          StartInstance (F, 'PerLengthPhaseImpedance', pCode);
          IntegerNode (F, 'PerLengthPhaseImpedance.conductorCount', FNPhases);
          EndInstance (F, 'PerLengthPhaseImpedance');
          seq := 0;
          for j:= 1 to FNPhases do begin
            for i:= j to FNPhases do begin
              Inc (seq);
              StartFreeInstance (F, 'PhaseImpedanceData');
              RefNode (F, 'PhaseImpedance', pCode);
              IntegerNode (F, 'PhaseImpedanceData.sequenceNumber', seq);
              DoubleNode (F, 'PhaseImpedanceData.r', Z.GetElement(i,j).re);
              DoubleNode (F, 'PhaseImpedanceData.x', Z.GetElement(i,j).im);
              DoubleNode (F, 'PhaseImpedanceData.b', YC.GetElement(i,j).im);
              EndInstance (F, 'PhaseImpedanceData')
            end;
          end;
        end;
      end;
      pCode := clsCode.ElementList.Next;
    end;

    pWire := clsWire.ElementList.First;
    while pWire <> nil do begin
      StartInstance (F, 'WireType', pWire);
      with pWire do begin
        StringNode (F, 'WireType.sizeDescription', DisplayName);
        if CompareText (LeftStr (LocalName, 2), 'AA') = 0 then
          StringNode (F, 'WireType.material', 'aluminum')
        else if CompareText (LeftStr (LocalName, 4), 'ACSR') = 0 then
          StringNode (F, 'WireType.material', 'acsr')
        else if CompareText (LeftStr (LocalName, 2), 'CU') = 0 then
          StringNode (F, 'WireType.material', 'copper')
        else if CompareText (LeftStr (LocalName, 3), 'EHS') = 0 then
          StringNode (F, 'WireType.material', 'steel')
        else
          StringNode (F, 'WireType.material', 'other');
        DoubleNode (F, 'WireType.gmr', GMR);
        DoubleNode (F, 'WireType.radius', Radius);
        DoubleNode (F, 'WireType.rDC20', Rdc);
        DoubleNode (F, 'WireType.rAC25', Rac);
        DoubleNode (F, 'WireType.rAC50', Rac);
        DoubleNode (F, 'WireType.rAC75', Rac);
        DoubleNode (F, 'WireType.ratedCurrent', MaxValue ([NormAmps, 0.0]));
        IntegerNode (F, 'WireType.strandCount', 0);
        IntegerNode (F, 'WireType.coreStrandCount', 0);
        DoubleNode (F, 'WireType.coreRadius', 0.0);
      end;
      EndInstance (F, 'WireType');
      pWire := clsWire.ElementList.Next;
    end;

    pGeom := clsGeom.ElementList.First;
    while pGeom <> nil do begin
      with pGeom do begin
        StartInstance (F, 'OverheadConductorInfo', pGeom);
        StringNode (F, 'ConductorInfo.usage', 'distribution');
        IntegerNode (F, 'ConductorInfo.phaseCount', Nphases);
        BooleanNode (F, 'ConductorInfo.insulated', false);
        IntegerNode (F, 'OverheadConductorInfo.phaseConductorCount', 1);
        DoubleNode (F, 'OverheadConductorInfo.neutralInsulationThickness', 0.0);
        EndInstance (F, 'OverheadConductorInfo');
        for i := 1 to NWires do begin
          StartFreeInstance (F, 'WireArrangement');
          RefNode (F, 'ConductorInfo', pGeom);
          RefNode (F, 'WireType', WireData[i]);
          IntegerNode (F, 'WireArrangement.position', i);
          DoubleNode (F, 'WireArrangement.mountingPointX', Xcoord[i]);
          DoubleNode (F, 'WireArrangement.mountingPointY', Ycoord[i]);
          EndInstance (F, 'WireArrangement')
        end;
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
