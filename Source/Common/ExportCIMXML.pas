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

Uses uComplex,  Arraydef, sysutils,   Circuit, DSSGlobals,
     uCMatrix,  solution, CktElement, Utilities, Bus, MathUtil,
     PDElement, PCElement, Generator, EnergyMeter, Sensor, Load, RegControl,
     ParserDel, Math, Ymatrix;

procedure DoubleNode (var F: TextFile; Node: String; val: Double);
begin
  Writeln (F, Format ('  <cim:%s>%g</cim:%s>', [Node, val, Node]));
end;

procedure PrefixVbaseNode (var F: TextFile; Node: String; Prefix: String; val: Double);
begin
  Writeln (F, Format ('  <cim:%s>%s_%.3g</cim:%s>', [Node, Prefix, val, Node]));
end;

procedure StringNode (var F: TextFile; Node: String; val: String);
begin
  Writeln (F, Format ('  <cim:%s>%s</cim:%s>', [Node, val, Node]));
end;

Procedure ExportCDPSM(FileNm:String);
Var
   F: TextFile;
   i: Integer;
   val: double;
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
        Writeln(F, Format('<cim:BaseVoltage ref:ID="BaseVoltage_%.3g">', [val]));
        PrefixVbaseNode (F, 'IdentifiedObject.name', 'BaseVoltage', val);
        DoubleNode (F, 'BaseVoltage.nominalVoltage', val);
        Writeln(F,'</cim:BaseVoltage>');

        Writeln(F, Format('<cim:VoltageLevel ref:ID="VoltageLevel_%.3g">', [val]));
        PrefixVbaseNode (F, 'IdentifiedObject.name', 'VoltageLevel', val);
        Writeln(F, Format('  <cim:VoltageLevel.BaseVoltage rdf:resource="#BaseVoltage_%.3g"/>', [val]));
        DoubleNode (F, 'VoltageLevel.lowVoltageLimit', val * NormalMinVolts);
        DoubleNode (F, 'VoltageLevel.highVoltageLimit', val * NormalMaxVolts);
        Writeln(F,'</cim:VoltageLevel>');

        Inc(i);
        val:=LegalVoltageBases^[i];
      end;

      for i := 1 to NumBuses do begin
        Writeln(F, Format('<cim:ConnectivityNode ref:ID="%s">', [BusList.Get(i)]));
        StringNode (F, 'IdentifiedObject.name', BusList.Get(i));
        Writeln(F, Format('  <cim:ConnectivityNode.MemberOf_EquipmentContainer rdf:resource="#VoltageLevel_%.3g"/>', [Buses^[i].kVBase]));
        DoubleNode (F, 'PositionPoint.xPosition', Buses^[i].x);
        DoubleNode (F, 'PositionPoint.yPosition', Buses^[i].y);
        Writeln(F,'</cim:ConnectivityNode>');
      end;
    end;
    GlobalResult := FileNm;
  Finally
    CloseFile(F);
  End;
End;

end.
