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

Type
  CIMProfileChoice = (Combined, Functional, ElectricalProperties,
    Asset, Geographical, Topology, StateVariables);

Procedure ExportCDPSM (FileNm:String; prf:CIMProfileChoice = Combined);

implementation

Uses SysUtils, Utilities, Circuit, DSSClassDefs, DSSGlobals, CktElement,
     PDElement, PCElement, Generator, Load, RegControl,
     Vsource, Line, Transformer, Ucomplex, UcMatrix, LineCode,
     Fuse, Capacitor, CapControl, Reactor, Feeder, ConductorData, LineUnits,
     LineGeometry, NamedObject, StrUtils, Math, XfmrCode, HashList, WireData,
     LineSpacing, CNData, TSData;

Type
  GuidChoice = (Bank, Wdg, XfInf, WdgInf, ScTest, OcTest,
    BaseV, LinePhase, LoadPhase, CapPhase);
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
    a_unit: TTransfObj;  // save this for writing the bank coordinates

    constructor Create(MaxWdg: Integer);
    destructor Destroy; override;

    procedure AddTransformer (pXf: TTransfObj);
    procedure BuildVectorGroup;
  end;

  TBusLoad = class(TNamedObject)
  public
    pa, pb, pc, qa, qb, qc: double;
    na, nb, nc: Integer;
    pLastLoad: TLoadObj;
    constructor Create;
  end;

  TBusCap = class(TNamedObject)
  public
    qa, qb, qc: double;
    pLastCap: TCapacitorObj;
    constructor Create;
  end;

Var
  GuidHash: THashList;       // index is 1-based
  GuidList: array of TGuid;  // index is 0-based
  BankHash: THashList;
  BankList: array of TBankObject;
  BusLoadHash: THashList;
  BusLoadList: array of TBusLoad;
  BusCapHash: THashList;
  BusCapList: array of TBusCap;

Const
  CIM_NS = 'http://iec.ch/TC57/2010/CIM-schema-cim15';
  CIM_LEN_UNITS = UNITS_FT;

constructor TBusLoad.Create;
begin
  pa := 0.0;
  pb := 0.0;
  pc := 0.0;
  qa := 0.0;
  qb := 0.0;
  qc := 0.0;
  na := 0;
  nb := 0;
  nc := 0;
  pLastLoad := nil;
end;

constructor TBusCap.Create;
begin
  qa := 0.0;
  qb := 0.0;
  qc := 0.0;
  pLastCap := nil;
end;

procedure StartBusLoadList (size: Integer);
begin
  BusLoadHash := THashList.Create(size);
  SetLength (BusLoadList, size);
end;

procedure StartBusCapList (size: Integer);
begin
  BusCapHash := THashList.Create(size);
  SetLength (BusCapList, size);
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

function GetBusLoad (sBus: String): TBusLoad;
var
  ref : Integer;
begin
  Result := nil;
  ref := BusLoadHash.Find (sBus);
  if ref > 0 then Result:=BusLoadList[ref-1];
end;

procedure AccumulateBusLoad (pLoad: TLoadObj);
var
  pBus: TBusLoad;
  sBus, phs: String;
  ref, size, n: Integer;
  xph, p, q: double;
begin
  sBus := StripExtension (pLoad.FirstBus);
  ref := BusLoadHash.Find(sBus);
  if ref < 1 then begin
    ref := BusLoadHash.Add(sBus);
    size := High(BusLoadList) + 1;
    if ref > size then SetLength (BusLoadList, 2 * size);
    BusLoadList[ref-1] := TBusLoad.Create;
  end;
  pBus := BusLoadList[ref-1];
  pBus.LocalName := sBus;
  phs := PhaseString (pLoad, 1);
  xph := length (phs);
  n := pLoad.NumCustomers div length (phs);
  p := pLoad.kWBase / xph;
  q := pLoad.kvarBase / xph;
  if Pos('A', phs) > 0 then begin
    pBus.pa := pBus.pa + p;
    pBus.qa := pBus.qa + q;
    pBus.na := pBus.na + n;
  end;
  if Pos('B', phs) > 0 then begin
    pBus.pb := pBus.pb + p;
    pBus.qb := pBus.qb + q;
    pBus.nb := pBus.nb + n;
  end;
  if Pos('C', phs) > 0 then begin
    pBus.pc := pBus.pc + p;
    pBus.qc := pBus.qc + q;
    pBus.nc := pBus.nc + n;
  end;
  pBus.pLastLoad := pLoad;
end;

function GetBusCap (sBus: String): TBusCap;
var
  ref : Integer;
begin
  Result := nil;
  ref := BusCapHash.Find (sBus);
  if ref > 0 then Result:=BusCapList[ref-1];
end;

procedure AccumulateBusCap (pCap: TCapacitorObj);
var
  pBus: TBusCap;
  sBus, phs: String;
  ref, size: Integer;
  xph, q: double;
begin
  sBus := StripExtension (pCap.FirstBus);
  ref := BusCapHash.Find(sBus);
  if ref < 1 then begin
    ref := BusCapHash.Add(sBus);
    size := High(BusCapList) + 1;
    if ref > size then SetLength (BusCapList, 2 * size);
    BusCapList[ref-1] := TBusCap.Create;
  end;
  pBus := BusCapList[ref-1];
  pBus.LocalName := sBus;
  phs := PhaseString (pCap, 1);
  xph := length (phs);
  q := pCap.Totalkvar / xph;
  if Pos('A', phs) > 0 then pBus.qa := pBus.qc + q;
  if Pos('B', phs) > 0 then pBus.qb := pBus.qc + q;
  if Pos('C', phs) > 0 then pBus.qc := pBus.qc + q;
  pBus.pLastCap := pCap;
end;

procedure FreeBusLoadList;
begin
  BusLoadHash.Free;
  BusLoadList := nil;
end;

procedure FreeBusCapList;
begin
  BusCapHash.Free;
  BusCapList := nil;
end;

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
  a_unit := nil;
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

procedure TBankObject.AddTransformer(pXf: TTransfObj);
var
  i: Integer;
  phs: String;
begin
  if pXf.NumberOfWindings > nWindings then nWindings := pXf.NumberOfWindings;

  a_unit := pXf;
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
  ref := BankHash.Add(pBank.localName);
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
    BaseV: key := 'BaseV=';
    LinePhase: key := 'LinePhase=';
    LoadPhase: key := 'LoadPhase=';
    CapPhase: key := 'CapPhase=';
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

function GetBaseVName (val: double): String;
begin
//  Result := Format('BaseV_%.3f', [val]);
  Result := 'BaseV_' + FloatToStr (val);
end;

function GetBaseVGuid (val: double): TGuid;
begin
  Result := GetDevGuid (BaseV, GetBaseVName (val), 1);
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
  Writeln (F, Format ('  <cim:%s rdf:resource="#%s"/>', [Node, Obj.CIM_ID]));
end;

procedure GuidNode (var F: TextFile; Node: String; ID: TGuid);
begin
  Writeln (F, Format ('  <cim:%s rdf:resource="#%s"/>', [Node, GUIDToCIMString (ID)]));
end;

procedure LineCodeRefNode (var F: TextFile; List: TLineCode; Name: String);
var
  Obj : TLineCodeObj;
begin
  if List.SetActive (Name) then begin
    Obj := List.GetActiveObj;
    if Obj.SymComponentsModel then
      Writeln (F, Format ('  <cim:ACLineSegment.PerLengthImpedance rdf:resource="#%s"/>', [Obj.CIM_ID]))
    else
      Writeln (F, Format ('  <cim:ACLineSegment.PerLengthImpedance rdf:resource="#%s"/>', [Obj.CIM_ID]));
  end;
end;

procedure CircuitNode (var F: TextFile; Obj: TNamedObject);
begin
  Writeln(F, Format('  <cim:Equipment.EquipmentContainer rdf:resource="#%s"/>', [Obj.CIM_ID]));
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

procedure GeneratorControlEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:GeneratingUnit.genControlSource rdf:resource="%s#GeneratorControlSource.%s"/>',
    [CIM_NS, val]));
end;

procedure SynchMachTypeEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:SynchronousMachine.type rdf:resource="%s#SynchronousMachineType.%s"/>',
    [CIM_NS, val]));
end;

procedure SynchMachModeEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:SynchronousMachine.operatingMode rdf:resource="%s#SynchronousMachineOperatingMode.%s"/>',
    [CIM_NS, val]));
end;

procedure RegulatingControlEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:RegulatingControl.mode rdf:resource="%s#RegulatingControlModeKind.%s"/>',
    [CIM_NS, val]));
end;

procedure WindingConnectionEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:TransformerEndInfo.connectionKind rdf:resource="%s#WindingConnection.%s"/>',
    [CIM_NS, val]));
end;

procedure ConductorInsulationEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:ConductorInfo.insulationMaterial rdf:resource="%s#WireInsulationKind.%s"/>',
    [CIM_NS, val]));
end;

procedure ConductorUsageEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:WireSpacingInfo.usage rdf:resource="%s#WireUsageKind.%s"/>',
    [CIM_NS, val]));
end;

procedure CableShieldMaterialEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:CableInfo.shieldMaterial rdf:resource="%s#CableShieldMaterialKind.%s"/>',
    [CIM_NS, val]));
end;

procedure ConductorMaterialEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:WireInfo.material rdf:resource="%s#WireMaterialKind.%s"/>',
    [CIM_NS, val]));
end;

procedure CableOuterJacketEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:CableInfo.outerJacketKind rdf:resource="%s#CableOuterJacketKind.%s"/>',
    [CIM_NS, val]));
end;

procedure CableConstructionEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:CableInfo.constructionKind rdf:resource="%s#CableConstructionKind.%s"/>',
    [CIM_NS, val]));
end;

procedure TransformerControlEnum (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:RatioTapChanger.tculControlMode rdf:resource="%s#TransformerControlMode.%s"/>',
    [CIM_NS, val]));
end;

procedure MonitoredPhaseNode (var F: TextFile; val: String);
begin
  Writeln (F, Format ('  <cim:RegulatingControl.monitoredPhase rdf:resource="%s#PhaseCode.%s"/>',
    [CIM_NS, val]));
end;

procedure StringNode (var F: TextFile; Node: String; val: String);
begin
  Writeln (F, Format ('  <cim:%s>%s</cim:%s>', [Node, val, Node]));
end;

procedure StartInstance (var F: TextFile; Root: String; Obj: TNamedObject);
begin
  Writeln(F, Format('<cim:%s rdf:ID="%s">', [Root, Obj.CIM_ID]));
  StringNode (F, 'IdentifiedObject.name', Obj.localName);
end;

procedure StartFreeInstance (var F: TextFile; Root: String);
var
  temp: TGUID;
begin
  CreateGUID (temp);
  Writeln(F, Format('<cim:%s rdf:ID="%s">', [Root, GUIDToCIMString (temp)]));
end;

procedure EndInstance (var F: TextFile; Root: String);
begin
  Writeln (F, Format ('</cim:%s>', [Root]));
end;

procedure XfmrPhasesEnum (var F: TextFile; pElem:TDSSCktElement; bus: Integer);
begin
  Writeln (F, Format ('  <cim:TransformerTankEnd.phases rdf:resource="%s#PhaseCode.%s"/>',
    [CIM_NS, PhaseString(pElem, bus)]));
end;

procedure PhaseNode (var F: TextFile; Root: String; val: String);
begin
  Writeln (F, Format ('  <cim:%s.phase rdf:resource="%s#PhaseCode.%s"/>',
    [Root, CIM_NS, val]));
end;

procedure AttachLinePhases (var F: TextFile; pLine:TLineObj; Root: String);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
begin
  if pLine.NPhases = 3 then if pLine.NumConductorsAvailable = 0 then exit;
  pPhase := TNamedObject.Create('dummy');
  s := PhaseString(pLine, 1);
  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pLine.Name + '_' + phs;
    pPhase.GUID := GetDevGuid (LinePhase, pPhase.LocalName, 1);
    StartInstance (F, 'ACLineSegmentPhase', pPhase);
    PhaseNode (F, 'ACLineSegmentPhase', phs);
    RefNode (F, 'ACLineSegmentPhase.ACLineSegment', pLine);
    EndInstance (F, 'ACLineSegmentPhase');
  end;
end;

procedure AttachCapPhases (var F: TextFile; pCap:TCapacitorObj; pBus:TBusCap);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
begin
//  if pCap.NPhases = 3 then exit;
  pPhase := TNamedObject.Create('dummy');
  s := PhaseString(pCap, 1);
  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pCap.Name + '_' + phs;
    pPhase.GUID := GetDevGuid (CapPhase, pPhase.LocalName, 1);
    StartInstance (F, 'ShuntCompensatorPhase', pPhase);
    PhaseNode (F, 'ShuntCompensatorPhase', phs);
    DoubleNode (F, 'ShuntCompensator.bPerSection',
      pCap.NomKV / pCap.Totalkvar / pCap.NumSteps);
    DoubleNode (F, 'ShuntCompensator.gPerSection', 0.0);
    RefNode (F, 'ShuntCompensatorPhase.ShuntCompensator', pCap);
    EndInstance (F, 'ShuntCompensatorPhase');
  end;
end;

procedure AttachLoadPhases (var F: TextFile; pLoad:TLoadObj; pBus:TBusLoad);
var
  s, phs: String;
  i: Integer;
  pPhase: TNamedObject;
begin
//  if pLoad.NPhases = 3 then exit;
  pPhase := TNamedObject.Create('dummy');
  s := PhaseString(pLoad, 1);
  for i := 1 to length(s) do begin
    phs := s[i];
    pPhase.LocalName := pLoad.Name + '_' + phs;
    pPhase.GUID := GetDevGuid (LoadPhase, pPhase.LocalName, 1);
    StartInstance (F, 'EnergyConsumerPhase', pPhase);
    PhaseNode (F, 'EnergyConsumerPhase', phs);
    DoubleNode (F, 'EnergyConsumerPhase.pfixed', pLoad.kWBase);
    DoubleNode (F, 'EnergyConsumerPhase.qfixed', pLoad.kvarBase);
    RefNode (F, 'EnergyConsumerPhase.EnergyConsumer', pBus);
    EndInstance (F, 'EnergyConsumerPhase');
  end;
end;

procedure VersionInstance (var F: TextFile);
begin
  StartFreeInstance (F, 'IEC61970CIMVersion');
  StringNode (F, 'IEC61970CIMVersion.version', 'IEC61970CIM15v20');
  StringNode (F, 'IEC61970CIMVersion.date', '2011-03-03');
  EndInstance (F, 'IEC61970CIMVersion');
end;

procedure WriteLoadModel (var F: TextFile; Name: String; ID: TGuid;
  zP: Double; iP: Double; pP: Double; zQ: Double; iQ: Double; pQ: Double;
  eP: Double; eQ: Double);
begin
  Writeln(F, Format('<cim:LoadResponseCharacteristic rdf:ID="%s">', [GUIDToCIMString(ID)]));
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

procedure WritePositions(var F:TextFile; pElem:TDSSCktElement; geoGUID: TGuid; crsGUID: TGuid);
var
  Nterm, j, ref : Integer;
  BusName : String;
begin
  Nterm := pElem.Nterms;
  BusName := pElem.FirstBus;
  Writeln(F, Format('<cim:Location rdf:ID="%s">', [GUIDToCIMString(geoGUID)]));
  StringNode(F, 'IdentifiedObject.name', pElem.LocalName + '_Loc');
  GuidNode (F, 'Location.CoordinateSystem', crsGUID);
  EndInstance (F, 'Location');

  for j := 1 to NTerm do begin
    if IsGroundBus (BusName) = False then begin
      ref := pElem.Terminals^[j].BusRef;
      StartFreeInstance (F, 'PositionPoint');
      GuidNode (F, 'PositionPoint.Location', geoGUID);
      IntegerNode (F, 'PositionPoint.sequenceNumber', j);
      StringNode (F, 'PositionPoint.xPosition', FloatToStr (ActiveCircuit.Buses^[ref].x));
      StringNode (F, 'PositionPoint.yPosition', FloatToStr (ActiveCircuit.Buses^[ref].y));
      EndInstance (F, 'PositionPoint');
    end;
    BusName := pElem.Nextbus;
  end;
end;

procedure WriteTerminals(var F:TextFile; pElem:TDSSCktElement; geoGUID: TGuid; crsGUID: TGuid);
var
  Nterm, j, ref : Integer;
  BusName, TermName : String;
  temp: TGuid;
begin
  Nterm := pElem.Nterms;
  BusName := pElem.FirstBus;
  for j := 1 to NTerm do begin
    if IsGroundBus (BusName) = False then begin
      ref := pElem.Terminals^[j].BusRef;
//      Str (j, TermName);
      TermName := IntToStr (j);
      TermName := pElem.Name + '_T' + TermName;
      CreateGUID (temp);

      Writeln(F, Format('<cim:Terminal rdf:ID="%s">', [GUIDToCIMString(temp)]));
      StringNode (F, 'IdentifiedObject.name', TermName);
      IntegerNode (F, 'Terminal.sequenceNumber', j);
      Writeln (F, Format('  <cim:Terminal.ConductingEquipment rdf:resource="#%s"/>',
        [pElem.CIM_ID]));
      Writeln (F, Format('  <cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
        [ActiveCircuit.Buses[ref].CIM_ID]));
      EndInstance (F, 'Terminal');
    end;

    BusName := pElem.Nextbus;
  end;
  WritePositions (F, pElem, geoGUID, crsGUID);
end;

Procedure WriteXfmrCode (var F: TextFile; pXfmr: TXfmrCodeObj);
var
  pName, pBank: TNamedObject;
  ratShort, ratEmerg, val, Zbase: double;
  i, j, seq: Integer;
  temp: TGUID;
begin
  pName := TNamedObject.Create('dummy');
  pBank := TNamedObject.Create('dummy');
  with pXfmr do begin
    pBank.LocalName := pXfmr.Name + '_PowerXfInfo';
    CreateGUID (temp);
    pBank.GUID := temp;
    StartInstance (F, 'PowerTransformerInfo', pBank);
    EndInstance (F, 'PowerTransformerInfo');
    StartInstance (F, 'TransformerTankInfo', pXfmr);
    RefNode (F, 'TransformerTankInfo.PowerTransformerInfo', pBank);
    EndInstance (F, 'TransformerTankInfo');
    ratShort := NormMaxHKVA / Winding^[1].kva;
    ratEmerg := EmergMaxHKVA / Winding^[1].kva;
    for i := 1 to NumWindings do begin
      Zbase := Winding^[i].kvll;
      Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;
      pName.localName := pXfmr.Name + '_' + IntToStr (i);
      pName.GUID := GetDevGuid (WdgInf, pXfmr.Name, i);
      StartInstance (F, 'TransformerEndInfo', pName);
      RefNode (F, 'TransformerEndInfo.TransformerTankInfo', pXfmr);
      IntegerNode (F, 'TransformerEndInfo.endNumber', i);
      if pXfmr.FNPhases < 3 then begin
        WindingConnectionEnum (F, 'I');
        IntegerNode (F, 'TransformerEndInfo.phaseAngleClock', 0)
      end else begin
        if Winding^[i].Connection = 1 then
          WindingConnectionEnum (F, 'D')
        else
          if (Winding^[i].Rneut > 0.0) or (Winding^[i].Xneut > 0.0) then
            WindingConnectionEnum (F, 'Yn')
          else
            WindingConnectionEnum (F, 'Y');
        if Winding^[i].Connection <> Winding^[1].Connection then
          IntegerNode (F, 'TransformerEndInfo.phaseAngleClock', 1)
        else
          IntegerNode (F, 'TransformerEndInfo.phaseAngleClock', 0);
      end;
      DoubleNode (F, 'TransformerEndInfo.ratedU', Winding^[i].kvll);
      DoubleNode (F, 'TransformerEndInfo.ratedS', Winding^[i].kva);
      DoubleNode (F, 'TransformerEndInfo.shortTermS', Winding^[i].kva * ratShort);
      DoubleNode (F, 'TransformerEndInfo.emergencyS', Winding^[i].kva * ratEmerg);
      DoubleNode (F, 'TransformerEndInfo.r', Winding^[i].Rpu * Zbase);
      DoubleNode (F, 'TransformerEndInfo.insulationU', 0.0);
      EndInstance (F, 'TransformerEndInfo');
    end;
    seq := 0;
    Inc (seq);
    pName.localName:= pXfmr.Name + '_' + IntToStr(seq);
    pName.GUID := GetDevGuid (OcTest, pXfmr.Name, seq);
    StartInstance (F, 'NoLoadTest', pName);
    GuidNode (F, 'NoLoadTest.EnergisedEnd', GetDevGuid (WdgInf, pXfmr.Name, 1));
    DoubleNode (F, 'NoLoadTest.energisedEndVoltage', Winding^[1].kvll);
    DoubleNode (F, 'NoLoadTest.excitingCurrent', pctImag);
    DoubleNode (F, 'NoLoadTest.excitingCurrentZero', pctImag);
    val := 0.01 * pctNoLoadLoss * Winding^[1].kva;
    DoubleNode (F, 'NoLoadTest.loss', val);
    DoubleNode (F, 'NoLoadTest.lossZero', val);
    DoubleNode (F, 'TransformerTest.basePower', Winding^[1].kva);
    DoubleNode (F, 'TransformerTest.temperature', 50.0);
    EndInstance (F, 'NoLoadTest');
    for i:= 1 to NumWindings do
      for j:= (i+1) to NumWindings do begin
        Inc (seq);
        pName.localName:= pXfmr.Name + '_' + IntToStr(seq);
        pName.GUID := GetDevGuid (ScTest, pXfmr.Name, seq);
        StartInstance (F, 'ShortCircuitTest', pName);
        GuidNode (F, 'ShortCircuitTest.EnergisedEnd', GetDevGuid (WdgInf, pXfmr.Name, i));
         // NOTE: can insert more than one GroundedEnds for three-winding short-circuit tests
        GuidNode (F, 'ShortCircuitTest.GroundedEnds', GetDevGuid (WdgInf, pXfmr.Name, j));
        IntegerNode (F, 'ShortCircuitTest.energisedEndStep', Winding^[i].NumTaps div 2);
        IntegerNode (F, 'ShortCircuitTest.groundedEndStep', Winding^[j].NumTaps div 2);
        Zbase := Winding^[i].kvll;
        Zbase := 1000.0 * Zbase * Zbase / Winding^[1].kva;  // all DSS impedances are on winding 1 base
        val := Xsc^[seq] * Zbase;
        DoubleNode (F, 'ShortCircuitTest.leakageImpedance', val);
        DoubleNode (F, 'ShortCircuitTest.leakageImpedanceZero', val);
        if seq = 2 then begin  // TODO - profile requires a value for each test
          val := 0.01 * pctLoadLoss * Winding^[1].kva;
          DoubleNode (F, 'ShortCircuitTest.loss', val);
          DoubleNode (F, 'ShortCircuitTest.lossZero', val);
        end;
        DoubleNode (F, 'TransformerTest.basePower', Winding^[i].kva);
        DoubleNode (F, 'TransformerTest.temperature', 50.0);
        EndInstance (F, 'ShortCircuitTest');
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
  cab.localName := '606';
  StartInstance (F, 'ConcentricNeutralCableInfo', cab);
  ConductorUsageEnum (F, 'distribution');
  IntegerNode (F, 'ConductorInfo.phaseCount', 3);
  BooleanNode (F, 'ConductorInfo.insulated', True);
  ConductorInsulationEnum (F, 'crosslinkedPolyethylene');
  DoubleNode (F, 'ConductorInfo.insulationThickness', 220);
  DoubleNode (F, 'CableInfo.diameterOverCore', 0.575);
  DoubleNode (F, 'CableInfo.diameterOverInsulation', 1.06);
  DoubleNode (F, 'CableInfo.diameterOverJacket', 1.16);
  DoubleNode (F, 'CableInfo.diameterOverScreen', 1.29);
  CableShieldMaterialEnum (F, 'copper');
  DoubleNode (F, 'ConcentricNeutralCableInfo.diameterOverNeutral', 1.29);
  IntegerNode (F, 'ConcentricNeutralCableInfo.neutralStrandCount', 13);
  WireDataClass.code := 'CU_#14';
  If Assigned(ActiveConductorDataObj) Then RefNode (F, 'ConcentricNeutralCableInfo.WireType', ActiveConductorDataObj);
  EndInstance (F, 'ConcentricNeutralCableInfo');

  CreateGuid (id);
  cab.GUID := id;
  cab.localName := '607';
  StartInstance (F, 'TapeShieldCableInfo', cab);
  ConductorUsageEnum (F, 'distribution');
  IntegerNode (F, 'ConductorInfo.phaseCount', 1);
  BooleanNode (F, 'ConductorInfo.insulated', True);
  ConductorInsulationEnum (F, 'crosslinkedPolyethylene');
  DoubleNode (F, 'ConductorInfo.insulationThickness', 220);
  DoubleNode (F, 'CableInfo.diameterOverCore', 0.373);
  DoubleNode (F, 'CableInfo.diameterOverInsulation', 0.82);
  DoubleNode (F, 'CableInfo.diameterOverJacket', 0.88);
  DoubleNode (F, 'CableInfo.diameterOverScreen', 1.06);
  CableShieldMaterialEnum (F, 'copper');
  DoubleNode (F, 'TapeShieldCableInfo.tapeLap', 20.0);
  DoubleNode (F, 'TapeShieldCableInfo.tapeThickness', 5.0);
  EndInstance (F, 'TapeShieldCableInfo');
  cab.Free;
end;

Procedure ExportCDPSM(FileNm:String; prf:CIMProfileChoice);
Var
  F      : TextFile;
  i, j   : Integer;
  seq    : Integer;
  val    : double;
  v1, v2 : double;
  i1, i2, i3 : Integer;
  Zs, Zm : complex;
  Rs, Rm, Xs, Xm, R1, R0, X1, X0: double;
  pName1, pName2  : TNamedObject;

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
  clsSpac : TLineSpacing;
  clsTape : TTSData;
  clsConc : TCNData;

  pCode : TLineCodeObj;
  pGeom : TLineGeometryObj;
  pWire : TWireDataObj;
  pXfmr : TXfmrCodeObj;
  pSpac : TLineSpacingObj;
  pTape : TTSDataObj;
  pConc : TCNDataObj;
  pXfTemp: TXfmrCodeObj;

  // DSS-like load models
  id1_ConstkVA:     TGuid;
  id2_ConstZ:       TGuid;
  id3_ConstPQuadQ:  TGuid;
  id4_LinPQuadQ:    TGuid;
  id5_ConstI:       TGuid;
  id6_ConstPConstQ: TGuid;  // P can vary, Q not
  id7_ConstPConstX: TGuid;

  // for CIM Locations
  geoGUID: TGuid;
  crsGUID: TGuid;

  // for phase modeling
  phs: String;
  pBusLoad: TBusLoad;
  pBusCap: TBusCap;
Begin
  Try
    clsCode := DSSClassList.Get(ClassNames.Find('linecode'));
    clsWire := DSSClassList.Get(ClassNames.Find('wiredata'));
    clsGeom := DSSClassList.Get(ClassNames.Find('linegeometry'));
    clsXfmr := DSSClassList.Get(ClassNames.Find('xfmrcode'));
    clsSpac := DSSClassList.Get(ClassNames.Find('linespacing'));
    clsTape := DSSClassList.Get(ClassNames.Find('TSData'));
    clsConc := DSSClassList.Get(ClassNames.Find('CNData'));
    pName1 := TNamedObject.Create('Temp1');
    pName2 := TNamedObject.Create('Temp2');
    i1 := clsXfmr.ElementCount * 6; // 3 wdg info, 3 sctest
    i2 := ActiveCircuit.Transformers.ListSize * 11; // bank, info, 3 wdg, 3 wdg info, 3sctest
    StartGuidList (i1 + i2);
    StartBankList (ActiveCircuit.Transformers.ListSize);
    StartBusLoadList (ActiveCircuit.Loads.ListSize);
    StartBusCapList (ActiveCircuit.ShuntCapacitors.ListSize);

    Assignfile(F, FileNm);
    ReWrite(F);

    Writeln(F,'<?xml version="1.0" encoding="utf-8"?>');
    Writeln(F,'<!-- un-comment this line to enable validation');
    Writeln(F,'-->');
//    Writeln(F,'<rdf:RDF xmlns:cim="http://iec.ch/TC57/2009/CIM-schema-cim14#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">');
    Writeln(F,'<rdf:RDF xmlns:cim="' + CIM_NS + '#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">');
    Writeln(F,'<!--');
    Writeln(F,'-->');

    VersionInstance (F);

    pName1.LocalName := ActiveCircuit.Name + '_CrsUrn';
    CreateGUID (crsGUID);
    pName1.GUID := crsGUID;
    StartInstance (F, 'CoordinateSystem', pName1);
    StringNode (F, 'CoordinateSystem.crsUrn', 'OpenDSSLocalBusCoordinates');
    EndInstance (F, 'CoordinateSystem');

    pName1.localName := ActiveCircuit.Name + '_Region';
    CreateGUID (geoGUID);
    pName1.GUID := geoGUID;
    StartInstance (F, 'GeographicalRegion', pName1);
    EndInstance (F, 'GeographicalRegion');

    pName2.localName := ActiveCircuit.Name + '_SubRegion';
    CreateGUID (geoGUID);
    pName2.GUID := geoGUID;
    StartInstance (F, 'SubGeographicalRegion', pName2);
    RefNode (F, 'SubGeographicalRegion.Region', pName1);
    EndInstance (F, 'SubGeographicalRegion');

    StartInstance (F, 'Line', ActiveCircuit);
    RefNode (F, 'Line.Region', pName2);
    EndInstance (F, 'Line');

    with ActiveCircuit do begin
      for i := 1 to NumBuses do begin
        Buses^[i].localName:= BusList.Get(i);
      end;

      for i := 1 to NumBuses do begin
        Writeln(F, Format('<cim:ConnectivityNode rdf:ID="%s">',
          [GUIDToCIMString (Buses^[i].GUID)]));
        StringNode (F, 'IdentifiedObject.name', Buses^[i].localName);
        Writeln (F, Format('  <cim:ConnectivityNode.ConnectivityNodeContainer rdf:resource="#%s"/>',
          [ActiveCircuit.CIM_ID]));
        Writeln (F,'</cim:ConnectivityNode>');
      end;

      i := 1;
      while LegalVoltageBases[i] > 0.0 do begin
        pName1.LocalName := GetBaseVName (LegalVoltageBases[i]);
        pName1.GUID := GetBaseVGuid (LegalVoltageBases[i]);
        StartInstance (F, 'BaseVoltage', pName1);
        DoubleNode (F, 'BaseVoltage.nominalVoltage', 1000.0 * LegalVoltageBases[i]);
        EndInstance (F, 'BaseVoltage');
        inc(i);
      end;
    end;

    pGen := ActiveCircuit.Generators.First;
    while pGen <> nil do begin
      pName1.localName := pGen.Name + '_GenUnit';
      CreateGUID (geoGUID);
      pName1.GUID := geoGUID;
      StartInstance (F, 'GeneratingUnit', pName1);
      DoubleNode (F, 'GeneratingUnit.ratedNetMaxP', pGen.GenVars.kVArating / 1000.0);
      DoubleNode (F, 'GeneratingUnit.initialP', pGen.PresentkW / 1000.0);
      GeneratorControlEnum (F, 'plantControl');
      EndInstance (F, 'GeneratingUnit');

      StartInstance (F, 'SynchronousMachine', pGen);
      CircuitNode (F, ActiveCircuit);
      DoubleNode (F, 'SynchronousMachine.minQ', pGen.kvarMin / 1000.0);
      DoubleNode (F, 'SynchronousMachine.maxQ', pGen.kvarMax / 1000.0);
      DoubleNode (F, 'SynchronousMachine.baseQ', pGen.Presentkvar / 1000.0);
      RefNode (F, 'SynchronousMachine.GeneratingUnit', pName1);
      SynchMachTypeEnum (F, 'generator');
      SynchMachModeEnum (F, 'generator');
      EndInstance (F, 'SynchronousMachine');
 //     AttachPhases (F, pGen, 1, 'SynchronousMachine');
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
          DoubleNode (F, 'EnergySource.nominalVoltage', kVbase);
          DoubleNode (F, 'EnergySource.voltageMagnitude', kVbase * PerUnit);
          DoubleNode (F, 'EnergySource.voltageAngle', TwoPi * Angle / 360.0);
          DoubleNode (F, 'EnergySource.r', R1);
          DoubleNode (F, 'EnergySource.x', X1);
          DoubleNode (F, 'EnergySource.r0', R0);
          DoubleNode (F, 'EnergySource.x0', X0);
          CreateGuid (geoGUID);
          GuidNode (F, 'PowerSystemResource.Location', geoGUID);
          EndInstance (F, 'EnergySource');
//          AttachPhases (F, pVsrc, 1, 'EnergySource');
          WriteTerminals (F, pVsrc, geoGUID, crsGUID);
        end;
      pVsrc := ActiveCircuit.Sources.Next;
    end;

    // catering to the transmission folks, we export the total capacitance at each bus,
    //  and then a breakdown by phase
    pCap := ActiveCircuit.ShuntCapacitors.First;
    while pCap <> nil do begin
      if pCap.Enabled then AccumulateBusCap (pCap);
      pCap := ActiveCircuit.ShuntCapacitors.Next;
    end;

    for i := Low (BusCapList) to High (BusCapList) do begin
      pBusCap := BusCapList[i];
      if pBusCap = nil then begin
        break;
      end;
      CreateGUID (geoGUID);
      pBusCap.GUID := geoGUID;
      StartInstance (F, 'ShuntCompensator', pBusCap);
      CircuitNode (F, ActiveCircuit);
      val := pBusCap.qa + pBusCap.qb + pBusCap.qc;
      with pBusCap.pLastCap do begin
        DoubleNode (F, 'ShuntCompensator.nomU', NomKV);
//        DoubleNode (F, 'ShuntCompensator.nomQ', TotalKvar);
        DoubleNode (F, 'ShuntCompensator.bPerSection', NomKV / val / NumSteps);
        DoubleNode (F, 'ShuntCompensator.gPerSection', 0.0);
        IntegerNode (F, 'ShuntCompensator.normalSections', NumSteps);
        IntegerNode (F, 'ShuntCompensator.maximumSections', NumSteps);
      end;
      CreateGuid (geoGUID);
      GuidNode (F, 'PowerSystemResource.Location', geoGUID);
      EndInstance (F, 'ShuntCompensator');
      WriteTerminals (F, pBusCap.pLastCap, geoGUID, crsGUID);
    end;

    pCap := ActiveCircuit.ShuntCapacitors.First;
    while pCap <> nil do begin
      if pCap.Enabled then begin
        pBusCap := GetBusCap (StripExtension (pCap.FirstBus));
        AttachCapPhases (F, pCap, pBusCap);
      end;
      pCap := ActiveCircuit.ShuntCapacitors.Next;
    end;

{*
    pCap := ActiveCircuit.ShuntCapacitors.First;
    while pCap <> nil do begin
      with pCap do begin
        StartInstance (F, 'ShuntCompensator', pCap);
        CircuitNode (F, ActiveCircuit);
        DoubleNode (F, 'ShuntCompensator.nomU', NomKV);
//        DoubleNode (F, 'ShuntCompensator.nomQ', TotalKvar);
        DoubleNode (F, 'ShuntCompensator.bPerSection', NomKV / TotalKvar / NumSteps);
        DoubleNode (F, 'ShuntCompensator.gPerSection', 0.0);
        IntegerNode (F, 'ShuntCompensator.normalSections', NumSteps);
        IntegerNode (F, 'ShuntCompensator.maximumSections', NumSteps);
        CreateGuid (geoGUID);
        GuidNode (F, 'PowerSystemResource.Location', geoGUID);
        EndInstance (F, 'ShuntCompensator');
        AttachCapPhases (F, pCap);
        WriteTerminals (F, pCap, geoGUID, crsGUID);
      end;
      pCap := ActiveCircuit.ShuntCapacitors.Next;
    end;
 *}
    pCapC := ActiveCircuit.CapControls.First;
    while (pCapC <> nil) do begin
      with pCapC do begin
        StartInstance (F, 'RegulatingControl', pCapC);
        RefNode (F, 'RegulatingControl.RegulatingCondEq', This_Capacitor);
        if CapControlType = PFCONTROL then begin
          v1 := PfOnValue;
          v2 := PfOffValue
        end else begin
          v1 := OnValue;
          v2 := OffValue
        end;
        case CapControlType of
          CURRENTCONTROL: RegulatingControlEnum (F, 'currentFlow');
          VOLTAGECONTROL: RegulatingControlEnum (F, 'voltage');
          KVARCONTROL: RegulatingControlEnum (F, 'reactivePower');
          TIMECONTROL: RegulatingControlEnum (F, 'timeScheduled');
          PFCONTROL, SRPCONTROL: RegulatingControlEnum (F, 'powerFactor');
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
        pBank.localName := sBank;
        pBank.GUID := GetDevGuid (Bank, sBank, 0);
        AddBank (pBank);
      end;

      if pXf.XfmrCode = '' then begin
        pXfTemp := TXfmrCodeObj.Create(clsXfmr, '=' + pXf.name);
        pXfTemp.GUID := GetDevGuid (XfInf, pXfTemp.name, 0);
        pXfTemp.PullFromTransformer (pXf);
        WriteXfmrCode (F, pXfTemp);
        pXfTemp.Free;
      end;

      pXf := ActiveCircuit.Transformers.Next;
    end;

    pXf := ActiveCircuit.Transformers.First;
    while pXf <> nil do begin
      with pXf do begin
        StartInstance (F, 'TransformerTank', pXf);
        if pXf.XfmrBank = '' then
          sBank := '=' + pXf.Name
        else
          sBank := pXf.XfmrBank;
        pBank := GetBank (sBank);
        RefNode (F, 'TransformerTank.PowerTransformer', pBank);
        pBank.AddTransformer (pXf);
        if pXf.XfmrCode = '' then
          GuidNode (F, 'TransformerTank.TransformerTankInfo', GetDevGuid (XfInf, '=' + pXf.Name, 0))
        else begin
          clsXfmr.SetActive(pXf.XfmrCode);
          pXfmr := clsXfmr.GetActiveObj;
          RefNode (F, 'TransformerTank.TransformerTankInfo', pXfmr); // TODO - new classes
        end;
        CreateGUID (geoGUID);
        GuidNode (F, 'PowerSystemResource.Location', geoGUID);
        EndInstance (F, 'TransformerTank');
        WritePositions (F, pXf, geoGUID, crsGUID);

        for i:=1 to NumberOfWindings do begin
          pName1.localName := pXf.Name;
          pName1.GUID := GetDevGuid (Wdg, pName1.localName, i);
          StartInstance (F, 'TransformerTankEnd', pName1);
          XfmrPhasesEnum (F, pXf, i);  // TODO
          RefNode (F, 'TransformerTankEnd.TransformerTank', pXf);
//          if XfmrCode = '' then  // TODO - write the impedances directly
//            GuidNode (F, 'TransformerEnd.TransformerEndInfo', GetDevGuid (WdgInf, '=' + pXf.Name, i))
//          else
//            GuidNode (F, 'TransformerEnd.TransformerEndInfo', GetDevGuid (WdgInf, XfmrCode, i));
          if (Winding^[i].Rneut < 0.0) or (Winding^[i].Connection = 1) then
            BooleanNode (F, 'TransformerEnd.grounded', false)
          else begin
            BooleanNode (F, 'TransformerEnd.grounded', true);
            DoubleNode (F, 'TransformerEnd.rground', Winding^[i].Rneut);
            DoubleNode (F, 'TransformerEnd.xground', Winding^[i].Xneut)
          end;
          j := pXf.Terminals^[i].BusRef;
          pName2.LocalName := pXf.Name + '_T' + IntToStr (i);
          CreateGUID (geoGUID);
          pName2.GUID := geoGUID;
          RefNode (F, 'TransformerEnd.Terminal', pName2);
          GuidNode (F, 'TransformerEnd.BaseVoltage', GetBaseVGuid (sqrt(3.0) * ActiveCircuit.Buses^[j].kVBase));
          EndInstance (F, 'TransformerTankEnd');

          StartInstance (F, 'Terminal', pName2);
          IntegerNode (F, 'Terminal.sequenceNumber', i);
          RefNode (F, 'Terminal.ConductingEquipment', pBank);
          Writeln (F, Format('<cim:Terminal.ConnectivityNode rdf:resource="#%s"/>',
            [ActiveCircuit.Buses[j].CIM_ID]));
          EndInstance (F, 'Terminal');
        end;
      end;
      pXf := ActiveCircuit.Transformers.Next;
    end;

    // write all the transformer banks
    for i:=Low(BankList) to High(BankList) do begin
      pBank := BankList[i];
      if pBank = nil then break;
      pBank.BuildVectorGroup;
      StartInstance (F, 'PowerTransformer', pBank);
      CircuitNode (F, ActiveCircuit);
      StringNode (F, 'PowerTransformer.vectorGroup', pBank.vectorGroup);
      EndInstance (F, 'PowerTransformer');
    end;

    pReg := ActiveCircuit.RegControls.First;
    while (pReg <> nil) do begin
      with pReg do begin
        pName1.LocalName := pReg.LocalName + '_Info';
        CreateGUID (geoGUID);
        pName1.GUID := geoGUID;
        StartInstance (F, 'TapChangerInfo', pName1);
        DoubleNode (F, 'TapChangerInfo.ptRatio', PT);
        DoubleNode (F, 'TapChangerInfo.ctRating', CT);
        DoubleNode (F, 'TapChangerInfo.ctRatio', CT / 0.2);
        EndInstance (F, 'TapChangerInfo');

        pName2.LocalName := pReg.LocalName + '_Ctrl';
        CreateGUID (geoGUID);
        pName2.GUID := geoGUID;
        StartInstance (F, 'TapChangerControl', pName2);
        MonitoredPhaseNode (F, FirstPhaseString (Transformer, TrWinding));
        DoubleNode (F, 'RegulatingControl.targetVoltage', PT * TargetVoltage);
        DoubleNode (F, 'RegulatingControl.targetRange', PT * BandVoltage);
        BooleanNode (F, 'TapChangerControl.lineDropCompensation', UseLineDrop);
        DoubleNode (F, 'TapChangerControl.lineDropR', LineDropR);
        DoubleNode (F, 'TapChangerControl.lineDropX', LineDropX);
        if UseReverseDrop then begin
          DoubleNode (F, 'TapChangerControl.reverseLineDropR', RevLineDropR);
          DoubleNode (F, 'TapChangerControl.reverseLineDropX', RevLineDropX)
        end else begin
          DoubleNode (F, 'TapChangerControl.reverseLineDropR', 0.0);
          DoubleNode (F, 'TapChangerControl.reverseLineDropX', 0.0)
        end;
        if UseLimit then
          DoubleNode (F, 'TapChangerControl.limitVoltage', VoltageLimit)
        else
          DoubleNode (F, 'TapChangerControl.limitVoltage', 0.0);
        EndInstance (F, 'TapChangerControl');

        StartInstance (F, 'RatioTapChanger', pReg);
        GuidNode (F, 'RatioTapChanger.TransformerEnd', GetDevGuid (Wdg, Transformer.Name, TrWinding));
        GuidNode (F, 'TapChanger.TapChangerInfo', pName1.GUID);
        GuidNode (F, 'TapChanger.TapChangerControl', pName2.GUID);
        IntegerNode (F, 'TapChanger.highStep', NumTaps);
        IntegerNode (F, 'TapChanger.lowStep', 0);
        IntegerNode (F, 'TapChanger.neutralStep', NumTaps div 2);
        IntegerNode (F, 'TapChanger.normalStep', NumTaps div 2);
        DoubleNode (F, 'TapChanger.neutralU', 120.0 * PT);
        DoubleNode (F, 'RatioTapChanger.stepVoltageIncrement', 100.0 * TapIncrement);
        DoubleNode (F, 'TapChanger.initialDelay', InitialDelay);
        DoubleNode (F, 'TapChanger.subsequentDelay', SubsequentDelay);
        BooleanNode (F, 'TapChanger.ltcFlag', True);
        BooleanNode (F, 'TapChanger.regulationStatus', True);

        TransformerControlEnum (F, 'volt');

        EndInstance (F, 'RatioTapChanger');

        StartFreeInstance (F, 'SvTapStep');
        RefNode (F, 'SvTapStep.TapChanger', pReg);
        val := Transformer.PresentTap[TrWinding];
        i1 := Round((val - Transformer.Mintap[TrWinding]) / Transformer.TapIncrement[TrWinding]); // tap step
        DoubleNode (F, 'SvTapStep.position', val);
        EndInstance (F, 'SvTapStep');
      end;
      pReg := ActiveCircuit.RegControls.Next;
    end;

    // done with the transformers

    pLine := ActiveCircuit.Lines.First;
    while pLine <> nil do begin
      with pLine do begin
        v1 := ConvertLineUnits (CIM_LEN_UNITS, pLine.LengthUnits);
        CreateGuid (geoGUID);
        if IsSwitch then begin
          StartInstance (F, 'LoadBreakSwitch', pLine);
          CircuitNode (F, ActiveCircuit);
          DoubleNode (F, 'ProtectedSwitch.breakingCapacity', pLine.NormAmps);
          if pLine.Closed[0] then
            StringNode (F, 'Switch.normalOpen', 'false')
          else
            StringNode (F, 'Switch.normalOpen', 'true');
          GuidNode (F, 'PowerSystemResource.Location', geoGUID);
          EndInstance (F, 'LoadBreakSwitch');
          AttachLinePhases (F, pLine, 'Switch');
        end else begin
          StartInstance (F, 'ACLineSegment', pLine);
          CircuitNode (F, ActiveCircuit);
          if LineCodeSpecified then begin
            DoubleNode (F, 'Conductor.length', Len * v1);
            LineCodeRefNode (F, clsCode, pLine.CondCode);
          end else if GeometrySpecified then begin
            DoubleNode (F, 'Conductor.length', Len * v1);
          end else if SpacingSpecified then begin
            DoubleNode (F, 'Conductor.length', Len * v1);
          end else begin // TODO - from 2009, would now have to create PerLengthPhaseImpedance on the fly
            val := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
            DoubleNode (F, 'Conductor.length', Len * v1);
            DoubleNode (F, 'ACLineSegment.r', R1);
            DoubleNode (F, 'ACLineSegment.x', X1);
            DoubleNode (F, 'ACLineSegment.bch', C1 * val);
            DoubleNode (F, 'ACLineSegment.r0', R0);
            DoubleNode (F, 'ACLineSegment.x0', X0);
            DoubleNode (F, 'ACLineSegment.b0ch', C0 * val);
          end;
          GuidNode (F, 'PowerSystemResource.Location', geoGUID);
          EndInstance (F, 'ACLineSegment');
          AttachLinePhases (F, pLine, 'ACLineSegment');
        end;
        WriteTerminals (F, pLine, geoGUID, crsGUID);
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

    // catering to the transmission folks, we export the total load at each bus,
    //  and then a breakdown by phase
    pLoad := ActiveCircuit.Loads.First;
    while pLoad <> nil do begin
      if pLoad.Enabled then AccumulateBusLoad (pLoad);
      pLoad := ActiveCircuit.Loads.Next;
    end;

    for i := Low (BusLoadList) to High (BusLoadList) do begin
      pBusLoad := BusLoadList[i];
      if pBusLoad = nil then begin
        break;
      end;
      CreateGUID (geoGUID);
      pBusLoad.GUID := geoGUID;
      StartInstance (F, 'EnergyConsumer', pBusLoad);
      CircuitNode (F, ActiveCircuit);
      case pBusLoad.pLastLoad.FLoadModel of
        1: GuidNode (F, 'EnergyConsumer.LoadResponse', id1_ConstkVA);
        2: GuidNode (F, 'EnergyConsumer.LoadResponse', id2_ConstZ);
        3: GuidNode (F, 'EnergyConsumer.LoadResponse', id3_ConstPQuadQ);
        4: GuidNode (F, 'EnergyConsumer.LoadResponse', id4_LinPQuadQ);
        5: GuidNode (F, 'EnergyConsumer.LoadResponse', id5_ConstI);
        6: GuidNode (F, 'EnergyConsumer.LoadResponse', id6_ConstPConstQ);
        7: GuidNode (F, 'EnergyConsumer.LoadResponse', id7_ConstPConstX);
      end;
      val := pBusLoad.pa + pBusLoad.pb + pBusLoad.pc;
      DoubleNode (F, 'EnergyConsumer.pfixed', val);
      val := pBusLoad.qa + pBusLoad.qb + pBusLoad.qc;
      DoubleNode (F, 'EnergyConsumer.qfixed', val);
      seq := pBusLoad.na + pBusLoad.nb + pBusLoad.nc;
      IntegerNode (F, 'EnergyConsumer.customerCount', seq);
      CreateGuid (geoGUID);
      GuidNode (F, 'PowerSystemResource.Location', geoGUID);
      EndInstance (F, 'EnergyConsumer');
      WriteTerminals (F, pBusLoad.pLastLoad, geoGUID, crsGUID);
    end;

    pLoad := ActiveCircuit.Loads.First;
    while pLoad <> nil do begin
      if pLoad.Enabled then begin
        pBusLoad := GetBusLoad (StripExtension (pLoad.FirstBus));
        AttachLoadPhases (F, pLoad, pBusLoad);
      end;
      pLoad := ActiveCircuit.Loads.Next;
    end;

{*
    pLoad := ActiveCircuit.Loads.First;
    while pLoad <> nil do begin
      if pLoad.Enabled then
        with pLoad do begin
          StartInstance (F, 'EnergyConsumer', pLoad);
          CircuitNode (F, ActiveCircuit);
          case FLoadModel of
            1: GuidNode (F, 'EnergyConsumer.LoadResponse', id1_ConstkVA);
            2: GuidNode (F, 'EnergyConsumer.LoadResponse', id2_ConstZ);
            3: GuidNode (F, 'EnergyConsumer.LoadResponse', id3_ConstPQuadQ);
            4: GuidNode (F, 'EnergyConsumer.LoadResponse', id4_LinPQuadQ);
            5: GuidNode (F, 'EnergyConsumer.LoadResponse', id5_ConstI);
            6: GuidNode (F, 'EnergyConsumer.LoadResponse', id6_ConstPConstQ);
            7: GuidNode (F, 'EnergyConsumer.LoadResponse', id7_ConstPConstX);
          end;
          DoubleNode (F, 'EnergyConsumer.qfixed', kvarBase);
          DoubleNode (F, 'EnergyConsumer.pfixed', kWBase);
          IntegerNode (F, 'EnergyConsumer.customerCount', NumCustomers);
          CreateGuid (geoGUID);
          GuidNode (F, 'PowerSystemResource.Location', geoGUID);
          EndInstance (F, 'EnergyConsumer');
          AttachLoadPhases (F, pLoad);
          WriteTerminals (F, pLoad, geoGUID, crsGUID);
        end;
        pLoad := ActiveCircuit.Loads.Next;
    end;
*}
    pCode := clsCode.ElementList.First;
    while pCode <> nil do begin
      with pCode do begin
        v1 := ConvertLineUnits (CIM_LEN_UNITS, pCode.Units); // output per foot
        if SymComponentsModel then begin
          v2 := 1.0e-9 * TwoPi * BaseFrequency; // convert nF to mhos
          StartInstance (F, 'PerLengthSequenceImpedance', pCode);
          DoubleNode (F, 'PerLengthSequenceImpedance.r', R1 * v1);
          DoubleNode (F, 'PerLengthSequenceImpedance.x', X1 * v1);
          DoubleNode (F, 'PerLengthSequenceImpedance.bch', C1 * v1 * v2);
          DoubleNode (F, 'PerLengthSequenceImpedance.r0', R0 * v1);
          DoubleNode (F, 'PerLengthSequenceImpedance.x0', X0 * v1);
          DoubleNode (F, 'PerLengthSequenceImpedance.b0ch', C0 * v1 * v2);
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
              RefNode (F, 'PhaseImpedanceData.PhaseImpedance', pCode);
              IntegerNode (F, 'PhaseImpedanceData.sequenceNumber', seq);
              DoubleNode (F, 'PhaseImpedanceData.r', Z.GetElement(i,j).re * v1);
              DoubleNode (F, 'PhaseImpedanceData.x', Z.GetElement(i,j).im * v1);
              DoubleNode (F, 'PhaseImpedanceData.b', YC.GetElement(i,j).im * v1);
              EndInstance (F, 'PhaseImpedanceData')
            end;
          end;
        end;
      end;
      pCode := clsCode.ElementList.Next;
    end;

    pWire := clsWire.ElementList.First;
    while (pWire <> nil) do begin
      StartInstance (F, 'OverheadWireInfo', pWire);
      with pWire do begin
        StringNode (F, 'WireInfo.sizeDescription', DisplayName);
        if CompareText (LeftStr (name, 2), 'AA') = 0 then
          ConductorMaterialEnum (F, 'aluminum')
        else if CompareText (LeftStr (name, 4), 'ACSR') = 0 then
          ConductorMaterialEnum (F, 'acsr')
        else if CompareText (LeftStr (name, 2), 'CU') = 0 then
          ConductorMaterialEnum (F, 'copper')
        else if CompareText (LeftStr (name, 3), 'EHS') = 0 then
          ConductorMaterialEnum (F, 'steel')
        else
          ConductorMaterialEnum (F, 'other');
        DoubleNode (F, 'WireInfo.gmr', GMR);
        DoubleNode (F, 'WireInfo.radius', Radius);
        DoubleNode (F, 'WireInfo.rDC20', Rdc);
        DoubleNode (F, 'WireInfo.rAC25', Rac);
        DoubleNode (F, 'WireInfo.rAC50', Rac);
        DoubleNode (F, 'WireInfo.rAC75', Rac);
        DoubleNode (F, 'WireInfo.ratedCurrent', MaxValue ([NormAmps, 0.0]));
        IntegerNode (F, 'WireInfo.strandCount', 0);
        IntegerNode (F, 'WireInfo.coreStrandCount', 0);
        DoubleNode (F, 'WireInfo.coreRadius', 0.0);
      end;
      BooleanNode (F, 'WireInfo.insulated', false);
      DoubleNode (F, 'WireInfo.neutralInsulationThickness', 0.0);
      EndInstance (F, 'OverheadWireInfo');
      // TODO - link to all ACLineSegments and phases using this
      pName1.LocalName := 'WireAsset_' + pWire.Name;
      CreateGUID (geoGUID);
      pName1.GUID := GeoGUID;
      StartInstance (F, 'Asset', pName1);
      RefNode (F, 'Asset.AssetInfo', pWire);
      pLine := ActiveCircuit.Lines.First;
      while pLine <> nil do begin
        with pLine do begin
          phs := PhaseString (pLine, 1);
          for i:=1 to NumConductorsAvailable do
            if pWire = ConductorData[i] then begin
              if i = 1 then RefNode (F, 'Asset.PowerSystemResources', pLine);
              if i >= length(phs) then
                geoGUID := GetDevGuid (LinePhase, pLine.Name + '_N' , 1)
              else
                geoGUID := GetDevGuid (LinePhase, pLine.Name + '_' + phs[i], 1);
              GuidNode (F, 'Asset.PowerSystemResources', geoGUID);
            end;
        end;
        pLine := ActiveCircuit.Lines.Next;
      end;
      EndInstance (F, 'Asset');
      pWire := clsWire.ElementList.Next;
    end;

    pGeom := clsGeom.ElementList.First;
    while pGeom <> nil do begin
      with pGeom do begin
        StartInstance (F, 'WireSpacingInfo', pGeom);
        ConductorUsageEnum (F, 'distribution');
        IntegerNode (F, 'WireSpacingInfo.phaseWireCount', 1);
        DoubleNode (F, 'WireSpacingInfo.phaseWireSpacing', 0.0);
        if PhaseChoice = Overhead then
          BooleanNode (F, 'WireSpacingInfo.isCable', False)
        else
          BooleanNode (F, 'WireSpacingInfo.isCable', True);
        EndInstance (F, 'WireSpacingInfo');

        for i := 1 to NWires do begin
          StartFreeInstance (F, 'WirePosition');
          RefNode (F, 'WirePosition.WireSpacingInfo', pGeom);
          if i <= NPhases then // TODO - how to assign them? For now, using phs as a sequence # proxy
            PhaseNode (F, 'WirePosition', String (Chr(Ord('A')+i-1)))
          else
            PhaseNode (F, 'WirePosition', 'N');
          DoubleNode (F, 'WirePosition.xCoord', Xcoord[i]);
          DoubleNode (F, 'WirePosition.yCoord', Ycoord[i]);
          EndInstance (F, 'WirePosition')
        end;
        // TODO - link to all ACLineSegments and phases using this
        pName1.LocalName := 'WireSpacingAsset_' + pGeom.Name;
        CreateGUID (geoGUID);
        pName1.GUID := GeoGUID;
        StartInstance (F, 'Asset', pName1);
        RefNode (F, 'Asset.AssetInfo', pGeom);
        pLine := ActiveCircuit.Lines.First;
        while pLine <> nil do begin
          if pLine.GeometrySpecified then
            if pLine.GeometryCode = pGeom.Name then
                RefNode (F, 'Asset.PowerSystemResources', pLine);
          pLine := ActiveCircuit.Lines.Next;
        end;
        EndInstance (F, 'Asset');
      end;
      pGeom := clsGeom.ElementList.Next;
    end;

    pSpac := clsSpac.ElementList.First;
    while pSpac <> nil do begin
      with pSpac do begin
        StartInstance (F, 'WireSpacingInfo', pSpac);
        ConductorUsageEnum (F, 'distribution');
        IntegerNode (F, 'WireSpacingInfo.phaseWireCount', 1);
        DoubleNode (F, 'WireSpacingInfo.phaseWireSpacing', 0.0);
        if pSpac.Ycoord[1] > 0.0 then
          BooleanNode (F, 'WireSpacingInfo.isCable', False)
        else
          BooleanNode (F, 'WireSpacingInfo.isCable', True);
        EndInstance (F, 'WireSpacingInfo');

        for i := 1 to NWires do begin
          StartFreeInstance (F, 'WirePosition');
          RefNode (F, 'WirePosition.WireSpacingInfo', pSpac);
          if i <= NPhases then // TODO - how to assign them? For now, using phs as a sequence # proxy
            PhaseNode (F, 'WirePosition', String (Chr(Ord('A')+i-1)))
          else
            PhaseNode (F, 'WirePosition', 'N');
          DoubleNode (F, 'WirePosition.xCoord', Xcoord[i]);
          DoubleNode (F, 'WirePosition.yCoord', Ycoord[i]);
          EndInstance (F, 'WirePosition')
        end;
        // TODO - link to all ACLineSegments and phases using this
        pName1.LocalName := 'WireSpacingAsset_' + pSpac.Name;
        CreateGUID (geoGUID);
        pName1.GUID := GeoGUID;
        StartInstance (F, 'Asset', pName1);
        RefNode (F, 'Asset.AssetInfo', pSpac);
        pLine := ActiveCircuit.Lines.First;
        while pLine <> nil do begin
          if pLine.SpacingSpecified then
            if pLine.SpacingCode = pSpac.Name then
                RefNode (F, 'Asset.PowerSystemResources', pLine);
          pLine := ActiveCircuit.Lines.Next;
        end;
        EndInstance (F, 'Asset');
      end;
      pSpac := clsSpac.ElementList.Next;
    end;

    pName1.Free;
    pName2.Free;

    FreeGuidList;
    FreeBankList;
    FreeBusLoadList;
    FreeBusCapList;

    Writeln (F, '</rdf:RDF>');

    GlobalResult := FileNm;
  Finally
    CloseFile(F);
  End;
End;

end.
