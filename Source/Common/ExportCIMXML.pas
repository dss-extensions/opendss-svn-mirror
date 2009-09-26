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
     LineGeometry, NamedObject, StrUtils, Math, XfmrCode, HashList;

Type
  GuidChoice = (Bank, Wdg, XfInf, WdgInf, ScTest, OcTest);
  TBankObject = class(TNamedObject)
  public
    vectorGroup: String;
    maxWindings: Integer;
    nWindings: Integer;
    connections: array of Integer;
    angles: array of Integer;
    phaseA: array of Integer;
    phaseB: array of Integer;
    phaseC: array of Integer;
    ground: array of Integer;

    constructor Create(MaxWdg: Integer);
    destructor Destroy; override;

    procedure AddTransformer (pXf: TTransfObj);
    procedure BuildVectorGroup;
  end;

Var
  GuidHash: THashList;       // index is 1-based
  GuidList: array of TGuid;  // index is 0-based
  BankHash: THashList;
  BankList: array of TBankObject;

constructor TBankObject.Create(MaxWdg: Integer);
begin
  maxWindings:=MaxWdg;
  nWindings:=0;
  SetLength (connections, MaxWdg);
  SetLength (angles, MaxWdg);
  SetLength (phaseA, MaxWdg);
  SetLength (phaseB, MaxWdg);
  SetLength (phaseC, MaxWdg);
  SetLength (ground, MaxWdg);
  Inherited Create('Bank');
end;

destructor TBankObject.Destroy;
begin
  connections := nil;
  angles := nil;
  phaseA := nil;
  phaseB := nil;
  phaseC := nil;
  ground := nil;
  Inherited Destroy;
end;

procedure TBankObject.BuildVectorGroup;
var
  i: Integer;
begin
  vectorGroup := '';
  i := 0; // dynamic arrays are zero-based
  while i < nWindings do begin
    if (phaseA[i] > 0) and (phaseB[i] > 0 ) and (phaseC[i] > 0) then begin
      if connections[i] > 0 then
        vectorGroup := vectorGroup + 'd'
      else
        vectorGroup := vectorGroup + 'y';
      if ground[i] > 0 then vectorGroup := vectorGroup + 'n';
      if angles[i] > 0 then vectorGroup := vectorGroup + IntToStr(angles[i])
    end else
      vectorGroup := vectorGroup + 'i';
    Inc (i)
  end;
  if Length(vectorGroup) > 0 then
    vectorGroup := UpperCase(LeftStr(vectorGroup, 1)) + RightStr (vectorGroup, Length(vectorGroup) - 1);
end;

function PhaseString (pElem:TDSSCktElement; bus: Integer):String;
var
  val, phs: String;
  dot: Integer;
begin
  phs := pElem.FirstBus;
  for dot:= 2 to bus do phs := pElem.NextBus;

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
  Result := val;
end;

procedure TBankObject.AddTransformer(pXf: TTransfObj);
var
  i: Integer;
  phs: String;
begin
  if pXf.NumberOfWindings > nWindings then nWindings := pXf.NumberOfWindings;

  for i:=1 to pXf.NumberOfWindings do begin
    phs := PhaseString (pXf, i);
    if Pos('A', phs) > 0 then phaseA[i-1] := 1;
    if Pos('B', phs) > 0 then phaseB[i-1] := 1;
    if Pos('C', phs) > 0 then phaseC[i-1] := 1;
    connections[i-1] := pXf.WdgConnection[i];
    if connections[i-1] <> connections[0] then angles [i-1] := 1;
    if (pXf.WdgRneutral[i] >= 0.0) or (pXf.WdgXneutral[i] > 0.0) then
      if connections[i-1] < 1 then
        ground[i-1] := 1;
  end;
end;

// the CIM transformer model requires some identified objects that don't have
// a counterpart in the DSS named objects.  These include banks, windings, and
// winding info.  So we create temporary GUIDs on the fly, and use a hash list when we
// need the GUIDs for later reference
procedure StartGuidList (size:Integer);
begin
  GuidHash := THashList.Create(size);
  SetLength (GuidList, size);
end;

procedure StartBankList (size: Integer);
begin
  BankHash := THashList.Create(size);
  SetLength (BankList, size);
end;

procedure AddBank (pBank: TBankObject);
var
  ref, size: Integer;
begin
  ref := BankHash.Add(pBank.LocalName);
  size := High(BankList) + 1;
  if ref > size then SetLength (BankList, 2 * size);
  BankList[ref-1] := pBank;
end;

function GetBank (sBank: String): TBankObject;
var
  ref : Integer;
begin
  Result := nil;
  ref := BankHash.Find (sBank);
  if ref > 0 then Result:=BankList[ref-1];
end;

// any temporary object (not managed by DSS) should have '=' prepended to the Name
function GetDevGuid (which: GuidChoice; Name: String; Seq: Integer): TGuid;
var
  key: String;
  ref: Integer;
  size: Integer;
begin
  case which of
    Bank: key := 'Bank=';
    Wdg: key := 'Wdg=';
    XfInf: key := 'XfInf=';
    WdgInf: key := 'WdgInf=';
    ScTest: key := 'ScTest=';
    OcTest: key := 'OcTest=';
  end;
  key:=key + Name + '=' + IntToStr (Seq);
  ref:=GuidHash.Find(key);
  if ref = 0 then begin
    ref := GuidHash.Add(key);
    CreateGuid (Result);
    size := High(GuidList) + 1;
    if ref > size then SetLength (GuidList, 2 * size);
    GuidList[ref-1] := Result
  end else begin
    Result := GuidList[ref-1]
  end;
end;

procedure FreeGuidList;
begin
  GuidHash.Free;
  GuidList := nil;
end;

procedure FreeBankList;
begin
  BankHash.Free;
  BankList := nil;
end;

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

function FirstPhaseString (pElem:TDSSCktElement; bus: Integer): String;
var
  val: String;
begin
  val := PhaseString (pElem, bus);
  if val <> '' then
    Result := LeftStr (val, 1)
  else
    Result := 'A';
end;

procedure PhasesNode (var F: TextFile; Node: String; pElem:TDSSCktElement; bus: Integer);
begin
  Writeln (F, Format ('  <cim:%s>%s</cim:%s>', [Node, PhaseString(pElem, bus), Node]));
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
  i, ref : Integer;
  TermName : String;
  temp: TGUID;
begin
  for i := 1 to pXf.NumberOfWindings do begin
    ref := pXf.Terminals^[i].BusRef;
    TermName := pXf.Name + '_T' + IntToStr (i);
    CreateGUID (temp);

    Writeln(F, Format('<cim:Terminal rdf:ID="%s">', [GUIDToString(temp)]));
    StringNode (F, 'IdentifiedObject.name', TermName);
    IntegerNode (F, 'Terminal.sequenceNumber', 1);  // sequence number always 1 for a winding
    Writeln (F, Format('  <cim:Terminal.ConductingEquipment rdf:resource="#%s"/>',
      [GuidToString (GetDevGuid (Wdg, pXf.Name, i))]));
    Writeln (F, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
      [ActiveCircuit.Buses[ref].ID]));
    EndInstance (F, 'Terminal');
  end;
  WritePositions (F, pXf);
end;

Procedure WriteXfmrCode (var F: TextFile; pXfmr: TXfmrCodeObj);
var
  pName: TNamedObject;
  ratShort, ratEmerg, val, Zbase: double;
  i, j, seq: Integer;
begin
  pName := TNamedObject.Create('dummy');
  with pXfmr do begin
    StartInstance (F, 'TransformerInfo', pXfmr);
    EndInstance (F, 'TransformerInfo');
    ratShort := NormMaxHKVA / Winding^[1].kva;
    ratEmerg := EmergMaxHKVA / Winding^[1].kva;
    Zbase := Winding^[1].kvll;
    Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;
    for i := 1 to NumWindings do begin
      pName.LocalName := pXfmr.Name + '_' + IntToStr (i);
      pName.GUID := GetDevGuid (WdgInf, pXfmr.Name, i);
      StartInstance (F, 'WindingInfo', pName);
      RefNode (F, 'TransformerInfo', pXfmr);
      IntegerNode (F, 'WindingInfo.sequenceNumber', i);
      if pXfmr.FNPhases < 3 then begin
        StringNode (F, 'WindingInfo.connectionKind', 'I');
        IntegerNode (F, 'WindingInfo.phaseAngle', 0)
      end else begin
        if Winding^[i].Connection = 1 then
          StringNode (F, 'WindingInfo.connectionKind', 'D')
        else
          if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
            StringNode (F, 'WindingInfo.connectionKind', 'Yn')
          else
            StringNode (F, 'WindingInfo.connectionKind', 'Y');
        if Winding^[i].Connection <> Winding^[1].Connection then
          IntegerNode (F, 'WindingInfo.phaseAngle', 1)
        else
          IntegerNode (F, 'WindingInfo.phaseAngle', 0);
      end;
      DoubleNode (F, 'WindingInfo.ratedU', Winding^[i].kvll);
      DoubleNode (F, 'WindingInfo.ratedS', Winding^[i].kva);
      DoubleNode (F, 'WindingInfo.shortTermS', Winding^[i].kva * ratShort);
      DoubleNode (F, 'WindingInfo.emergencyS', Winding^[i].kva * ratEmerg);
      DoubleNode (F, 'WindingInfo.r', Winding^[i].Rpu * Zbase);
      DoubleNode (F, 'WindingInfo.insulationU', 0.0);
      EndInstance (F, 'WindingInfo');
    end;
    StartFreeInstance (F, 'OpenCircuitTest');
    GuidNode (F, 'DistributionWindingTest.FromWinding', GetDevGuid (WdgInf, pXfmr.Name, 1));
    IntegerNode (F, 'DistributionWindingTest.fromTapStep', Winding^[1].NumTaps div 2);
    DoubleNode (F, 'OpenCircuitTest.excitingCurrent', pctImag);
    DoubleNode (F, 'OpenCircuitTest.excitingCurrentZero', pctImag);
    val := 0.01 * pctNoLoadLoss * Winding^[1].kva;
    DoubleNode (F, 'OpenCircuitTest.noLoadLoss', val);
    DoubleNode (F, 'OpenCircuitTest.noLoadLossZero', val);
    EndInstance (F, 'OpenCircuitTest');
    seq := 0;
    for i:= 1 to NumWindings do
      for j:= (i+1) to NumWindings do begin
        Inc (seq);
        pName.LocalName:= pXfmr.Name + '_' + IntToStr(seq);
        pName.GUID := GetDevGuid (ScTest, pXfmr.Name, seq);
        StartInstance (F, 'ShortCircuitTest', pName);
        GuidNode (F, 'DistributionWindingTest.FromWinding', GetDevGuid (WdgInf, pXfmr.Name, i));
        IntegerNode (F, 'DistributionWindingTest.fromTapStep', Winding^[i].NumTaps div 2);
        Zbase := Winding^[i].kvll;
        Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;  // all DSS impedances are on winding 1 base
        val := Xsc^[seq] * Zbase;
        DoubleNode (F, 'ShortCircuitTest.leakageImpedance', val);
        DoubleNode (F, 'ShortCircuitTest.leakageImpedanceZero', val);
        if seq = 1 then begin
          val := 0.01 * pctLoadLoss * Winding^[1].kva;
          DoubleNode (F, 'ShortCircuitTest.loadLoss', val);
          DoubleNode (F, 'ShortCircuitTest.loadLossZero', val);
        end;
        EndInstance (F, 'ShortCircuitTest');
        StartFreeInstance (F, 'ToWindingSpec');
        GuidNode (F, 'ShortCircuitTest', GetDevGuid (ScTest, pXfmr.Name, seq));
        GuidNode (F, 'ToWinding', GetDevGuid (WdgInf, pXfmr.Name, j));
        IntegerNode (F, 'ToWindingSpec.toTapStep', Winding^[j].NumTaps div 2);
        EndInstance (F, 'ToWindingSpec');
      end;
  end;
  pName.Free;
end;

Procedure WriteCableInfoTest (var F:TextFile);
var
  cab: TNamedObject;
  id: TGuid;
begin
  cab := TNamedObject.Create('CableTest');

  CreateGuid (id);
  cab.GUID := id;
  cab.LocalName := '606';
  StartInstance (F, 'ConcentricNeutralCableInfo', cab);
  StringNode (F, 'ConductorInfo.usage', 'distribution');
  IntegerNode (F, 'ConductorInfo.phaseCount', 3);
  BooleanNode (F, 'ConductorInfo.insulated', True);
  StringNode (F, 'ConductorInfo.insulationMaterial', 'crosslinkedPolyethylene');
  DoubleNode (F, 'ConductorInfo.insulationThickness', 220);
  DoubleNode (F, 'CableInfo.diameterOverCore', 0.575);
  DoubleNode (F, 'CableInfo.diameterOverInsulation', 1.06);
  DoubleNode (F, 'CableInfo.diameterOverJacket', 1.16);
  DoubleNode (F, 'CableInfo.diameterOverScreen', 1.29);
  DoubleNode (F, 'ConcentricNeutralCableInfo.diameterOverNeutral', 1.29);
  IntegerNode (F, 'ConcentricNeutralCableInfo.neutralStrandCount', 13);
  WireDataClass.code := 'CU_#14';
  If Assigned(ActiveWireDataObj) Then RefNode (F, 'WireType', ActiveWireDataObj);
  WireDataClass.code := 'AA_250';
  If Assigned(ActiveWireDataObj) Then Begin
    StartFreeInstance (F, 'WireArrangement');
    RefNode (F, 'ConductorInfo', cab);
    RefNode (F, 'WireType', ActiveWireDataObj);
    IntegerNode (F, 'WireArrangement.position', 1);
    DoubleNode (F, 'WireArrangement.mountingPointX', 0.0);
    DoubleNode (F, 'WireArrangement.mountingPointY', -4.0);
    EndInstance (F, 'WireArrangement');

    StartFreeInstance (F, 'WireArrangement');
    RefNode (F, 'ConductorInfo', cab);
    RefNode (F, 'WireType', ActiveWireDataObj);
    IntegerNode (F, 'WireArrangement.position', 2);
    DoubleNode (F, 'WireArrangement.mountingPointX', 0.5);
    DoubleNode (F, 'WireArrangement.mountingPointY', -4.0);
    EndInstance (F, 'WireArrangement');

    StartFreeInstance (F, 'WireArrangement');
    RefNode (F, 'ConductorInfo', cab);
    RefNode (F, 'WireType', ActiveWireDataObj);
    IntegerNode (F, 'WireArrangement.position', 3);
    DoubleNode (F, 'WireArrangement.mountingPointX', 1.0);
    DoubleNode (F, 'WireArrangement.mountingPointY', -4.0);
    EndInstance (F, 'WireArrangement')
  End;

  CreateGuid (id);
  cab.GUID := id;
  cab.LocalName := '607';
  StartInstance (F, 'TapeShieldCableInfo', cab);
  StringNode (F, 'ConductorInfo.usage', 'distribution');
  IntegerNode (F, 'ConductorInfo.phaseCount', 1);
  BooleanNode (F, 'ConductorInfo.insulated', True);
  StringNode (F, 'ConductorInfo.insulationMaterial', 'crosslinkedPolyethylene');
  DoubleNode (F, 'ConductorInfo.insulationThickness', 220);
  DoubleNode (F, 'CableInfo.diameterOverCore', 0.373);
  DoubleNode (F, 'CableInfo.diameterOverInsulation', 0.82);
  DoubleNode (F, 'CableInfo.diameterOverJacket', 0.88);
  DoubleNode (F, 'CableInfo.diameterOverScreen', 1.06);
  DoubleNode (F, 'TapeShieldCableInfo.tapeLap', 20.0);
  DoubleNode (F, 'TapeShieldCableInfo.tapeThickness', 5.0);
  EndInstance (F, 'TapeShieldCableInfo');
  WireDataClass.code := 'AA_1/0';
  If Assigned(ActiveWireDataObj) Then Begin
    StartFreeInstance (F, 'WireArrangement');
    RefNode (F, 'ConductorInfo', cab);
    RefNode (F, 'WireType', ActiveWireDataObj);
    IntegerNode (F, 'WireArrangement.position', 1);
    DoubleNode (F, 'WireArrangement.mountingPointX', 0.0);
    DoubleNode (F, 'WireArrangement.mountingPointY', -4.0);
    EndInstance (F, 'WireArrangement');
  End;
  WireDataClass.code := 'CU_1/0';
  If Assigned(ActiveWireDataObj) Then Begin
    StartFreeInstance (F, 'WireArrangement');
    RefNode (F, 'ConductorInfo', cab);
    RefNode (F, 'WireType', ActiveWireDataObj);
    IntegerNode (F, 'WireArrangement.position', 2);
    DoubleNode (F, 'WireArrangement.mountingPointX', 1.0 / 12.0);
    DoubleNode (F, 'WireArrangement.mountingPointY', -4.0);
    EndInstance (F, 'WireArrangement');
  End;
  cab.Free;
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
  pTemp  : TNamedObject;

  pBank  : TBankObject;
  maxWdg : Integer;
  sBank  : String;

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
  clsXfmr : TXfmrCode;

  pCode : TLineCodeObj;
  pGeom : TLineGeometryObj;
  pWire : TWireDataObj;
  pXfmr : TXfmrCodeObj;
  pXfTemp: TXfmrCodeObj;

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
    clsXfmr := DSSClassList.Get(ClassNames.Find('xfmrcode'));

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
          PhasesNode (F, 'ConductingEquipment.phases', pVsrc, 1);
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
        PhasesNode (F, 'ConductingEquipment.phases', pCap, 1);
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

    // begin the transformers.  write all the XfmrCodes first, and create a temporary
    //  XfmrCode for any that don't have one

    i1 := clsXfmr.ElementCount * 6; // 3 wdg info, 3 sctest
    i2 := ActiveCircuit.Transformers.ListSize * 11; // bank, info, 3 wdg, 3 wdg info, 3sctest
    StartGuidList (i1 + i2);
    StartBankList (ActiveCircuit.Transformers.ListSize);
    pTemp := TNamedObject.Create('Temp');

    pXfmr := clsXfmr.ElementList.First;
    while pXfmr <> nil do begin
      WriteXfmrCode (F, pXfmr);
      pXfmr := clsXfmr.ElementList.Next;
    end;

    // create all the banks and temporary XfmrCodes
    maxWdg := 0;
    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.NumberOfWindings > maxWdg then maxWdg := pXf.NumberofWindings;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      if pXf.XfmrBank = '' then
        sBank := '=' + pXf.Name
      else
        sBank := pXf.XfmrBank;

      pBank := GetBank (sBank);
      if pBank = nil then begin
        pBank := TBankObject.Create(maxWdg);
        pBank.LocalName := sBank;
        pBank.GUID := GetDevGuid (Bank, sBank, 0);
        AddBank (pBank);
      end;

      if pXf.XfmrCode = '' then begin
        pXfTemp := TXfmrCodeObj.Create(clsXfmr, '=' + pXf.LocalName);
        pXfTemp.GUID := GetDevGuid (XfInf, pXfTemp.LocalName, 0);
        pXfTemp.PullFromTransformer (pXf);
        WriteXfmrCode (F, pXfTemp);
        pXfTemp.Free;
      end;

      pXf := ActiveCircuit.Transformers.Next;
    end;

    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      with pXf do begin
        StartInstance (F, 'DistributionTransformer', pXf);
        if pXf.XfmrBank = '' then
          sBank := '=' + pXf.Name
        else
          sBank := pXf.XfmrBank;
        pBank := GetBank (sBank);
        RefNode (F, 'TransformerBank', pBank);
        pBank.AddTransformer (pXf);
        if pXf.XfmrCode = '' then
          GuidNode (F, 'TransformerInfo', GetDevGuid (XfInf, '=' + pXf.Name, 0))
        else begin
          clsXfmr.SetActive(pXf.XfmrCode);
          pXfmr := clsXfmr.GetActiveObj;
          RefNode (F, 'TransformerInfo', pXfmr);
        end;
        EndInstance (F, 'DistributionTransformer');

        for i:=1 to NumberOfWindings do begin
          pTemp.LocalName := pXf.Name;
          pTemp.GUID := GetDevGuid (Wdg, pTemp.LocalName, i);
          StartInstance (F, 'DistributionTransformerWinding', pTemp);
          PhasesNode (F, 'ConductingEquipment.phases', pXf, i);
          RefNode (F, 'Transformer', pXf);
          if XfmrCode = '' then
            GuidNode (F, 'WindingInfo', GetDevGuid (WdgInf, '=' + pXf.Name, i))
          else
            GuidNode (F, 'WindingInfo', GetDevGuid (WdgInf, XfmrCode, i));
          if (Winding^[i].Rneut < 0.0) or (Winding^[i].Connection = 1) then
            BooleanNode (F, 'DistributionTransformerWinding.grounded', false)
          else begin
            BooleanNode (F, 'DistributionTransformerWinding.grounded', true);
            DoubleNode (F, 'DistributionTransformerWinding.rground', Winding^[i].Rneut);
            DoubleNode (F, 'DistributionTransformerWinding.xground', Winding^[i].Xneut)
          end;
          EndInstance (F, 'DistributionTransformerWinding')
        end;
      end;
      WriteWdgTerminals (F, pXf);
      pXf := ActiveCircuit.Transformers.Next;
    end;
    pTemp.Free;

    // write all the transformer banks
    for i:=Low(BankList) to High(BankList) do begin
      pBank := BankList[i];
      if pBank = nil then break;
      pBank.BuildVectorGroup;
      StartInstance (F, 'TransformerBank', pBank);
      StringNode (F, 'TransformerBank.vectorGroup', pBank.vectorGroup);
      EndInstance (F, 'TransformerBank');
    end;

    pReg := ActiveCircuit.RegControls.First;
    while pReg <> nil do begin
      with pReg do begin
        StartInstance (F, 'DistributionTapChanger', pReg);
        GuidNode (F, 'RatioTapChanger.Winding', GetDevGuid (Wdg, Transformer.Name, TrWinding));
        IntegerNode (F, 'TapChanger.highStep', NumTaps);
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
        StringNode (F, 'DistributionTapChanger.monitoredPhase',
          FirstPhaseString (Transformer, TrWinding));
        EndInstance (F, 'DistributionTapChanger');

        StartFreeInstance (F, 'SvTapStep');
        RefNode (F, 'TapChanger', pReg);
        val := Transformer.PresentTap[TrWinding];
        i1 := Round((val - Transformer.Mintap[TrWinding]) / Transformer.TapIncrement[TrWinding]);
        IntegerNode (F, 'position', i1);
        DoubleNode (F, 'continuousPosition', val);
        EndInstance (F, 'SvTapStep');
      end;
      pReg := ActiveCircuit.RegControls.Next;
    end;

    FreeGuidList;
    FreeBankList;

    // done with the transformers

    pLine := ActiveCircuit.Lines.First;
    while pLine <> nil do begin
      with pLine do begin
        if IsSwitch then begin
          StartInstance (F, 'LoadBreakSwitch', pLine);
          CircuitNode (F, ActiveCircuit);
          PhasesNode (F, 'ConductingEquipment.phases', pLine, 1);
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
            PhasesNode (F, 'ConductingEquipment.phases', pLine, 1);
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
            PhasesNode (F, 'ConductingEquipment.phases', pLine, 1);
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
          PhasesNode (F, 'ConductingEquipment.phases', pLoad, 1);
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

    // temporary test harness until DSS has a cable constants module
    if CompareText ('ieee13', LeftStr(ActiveCircuit.Name, 6)) = 0 then
      WriteCableInfoTest (F);

    Writeln (F, '</rdf:RDF>');

    GlobalResult := FileNm;
  Finally
    CloseFile(F);
  End;
End;

end.
