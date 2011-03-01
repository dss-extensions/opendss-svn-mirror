unit ExportOptions;

interface

Uses Command;

CONST
        NumExportOptions = 27;

FUNCTION DoExportCmd:Integer;


VAR
         ExportOption,
         ExportHelp :Array[1..NumExportOptions] of String;
         ExportCommands:TCommandList;

implementation

Uses ExportResults, Monitor, ParserDel, sysutils, DSSGlobals, ExportCIMXML, Utilities;


Procedure DefineOptions;

Begin

      ExportOption[ 1] := 'Voltages';
      ExportOption[ 2] := 'SeqVoltages';
      ExportOption[ 3] := 'Currents';
      ExportOption[ 4] := 'SeqCurrents';
      ExportOption[ 5] := 'Estimation';
      ExportOption[ 6] := 'Capacity';
      ExportOption[ 7] := 'Overloads';
      ExportOption[ 8] := 'Unserved';
      ExportOption[ 9] := 'Powers';
      ExportOption[10] := 'SeqPowers';
      ExportOption[11] := 'Faultstudy';
      ExportOption[12] := 'Generators';
      ExportOption[13] := 'Loads';
      ExportOption[14] := 'Meters';
      ExportOption[15] := 'Monitors';
      ExportOption[16] := 'Yprims';
      ExportOption[17] := 'Y';
      ExportOption[18] := 'seqz';
      ExportOption[19] := 'P_byphase';
      ExportOption[20] := 'CDPSM';
      ExportOption[21] := 'CDPSMConnect';
      ExportOption[22] := 'CDPSMBalanced';
      ExportOption[23] := 'Buscoords';
      ExportOption[24] := 'Losses';
      ExportOption[25] := 'Guids';
      ExportOption[26] := 'Counts';
      ExportOption[27] := 'Summary';

      ExportHelp[ 1] := '(Default file = EXP_VOLTAGES.CSV) Voltages to ground by bus/node.';
      ExportHelp[ 2] := '(Default file = EXP_SEQVOLTAGES.CSV) Sequence voltages.';
      ExportHelp[ 3] := '(Default file = EXP_CURRENTS.CSV) Currents in each conductor of each element.';
      ExportHelp[ 4] := '(Default file = EXP_SEQCURRENTS.CSV) Sequence currents in each terminal of 3-phase elements.';
      ExportHelp[ 5] := '(Default file = EXP_ESTIMATION.CSV) Results of last estimation.';
      ExportHelp[ 6] := '(Default file = EXP_CAPACITY.CSV) Capacity report.';
      ExportHelp[ 7] := '(Default file = EXP_OVERLOADS.CSV) Overloaded elements report.';
      ExportHelp[ 8] := '(Default file = EXP_UNSERVED.CSV) Report on elements that are served in violation of ratings.';
      ExportHelp[ 9] := '(Default file = EXP_POWERS.CSV) Powers into each terminal of each element.';
      ExportHelp[10] := '(Default file = EXP_SEQPOWERS.CSV) Sequence powers into each terminal of 3-phase elements.';
      ExportHelp[11] := '(Default file = EXP_FAULTS.CSV) results of a fault study.';
      ExportHelp[12] := '(Default file = EXP_GENMETERS.CSV) Present values of generator meters. Adding the switch "/multiple" or "/m" will ' +
                        ' cause a separate file to be written for each generator.';
      ExportHelp[13] := '(Default file = EXP_LOADS.CSV) Report on loads from most recent solution.';
      ExportHelp[14] := '(Default file = EXP_METERS.CSV) Energy meter exports. Adding the switch "/multiple" or "/m" will ' +
                        ' cause a separate file to be written for each meter.';
      ExportHelp[15] := '(file name is assigned by Monitor export) Monitor values.';
      ExportHelp[16] := '(Default file = EXP_YPRIMS.CSV) All primitive Y matrices.';
      ExportHelp[17] := '(Default file = EXP_Y.CSV) System Y matrix.';
      ExportHelp[18] := '(Default file = EXP_SEQZ.CSV) Equivalent sequence Z1, Z0 to each bus.';
      ExportHelp[19] := '(Default file = EXP_P_BYPHASE.CSV) Power by phase.';
      ExportHelp[20] := '(Default file = CDPSM.XML) (IEC 61968-13, CDPSM Unbalanced load flow profile)';
      ExportHelp[21] := '(Default file = CDPSM_Connect.XML) (IEC 61968-13, CDPSM Unbalanced connectivity profile)';
      ExportHelp[22] := '(Default file = CDPSM_Balanced.XML) (IEC 61968-13, CDPSM Balanced profile)';
      ExportHelp[23] := '[Default file = EXP_BUSCOORDS.CSV] Bus coordinates in csv form.';
      ExportHelp[24] := '[Default file = EXP_LOSSES.CSV] Losses for each element.';
      ExportHelp[25] := '[Default file = EXP_GUIDS.CSV] Guids for each element.';
      ExportHelp[26] := '[Default file = EXP_Counts.CSV] (instance counts for each class)';
      ExportHelp[27] := '[Default file = EXP_Summary.CSV] Solution summary.';
End;

//----------------------------------------------------------------------------
FUNCTION DoExportCmd:Integer;

VAR
   ParamName,
   Parm1,
   Parm2,
   FileName :String;

   MVAopt :Integer;
   UEonlyOpt:Boolean;
   pMon      :TMonitorObj;
   ParamPointer :Integer;

Begin

   ParamName := Parser.NextParam;
   Parm1 := LowerCase(Parser.StrValue);
   ParamPointer := ExportCommands.Getcommand (Parm1);

   MVAOpt := 0;
   UEonlyOpt := FALSE;

   CASE ParamPointer OF
      9, 19: Begin { Trap export powers command and look for MVA/kVA option }
            ParamName := parser.nextParam;
            Parm2 := LowerCase(Parser.strvalue);
            MVAOpt := 0;
            IF Length(Parm2) > 0 THEN IF Parm2[1]='m' THEN MVAOpt := 1;
          End;

      8: Begin { Trap UE only flag  }
            ParamName := parser.nextParam;
            Parm2 := LowerCase(Parser.strvalue);
            UEonlyOpt := FALSE;
            IF Length(Parm2) > 0 THEN IF Parm2[1]='u' THEN UEonlyOpt := TRUE;
          End;

      15: Begin {Get monitor name for export monitors command}
             ParamName := Parser.NextParam;
             Parm2 := Parser.StrValue;
          End;

   End;

   {Pick up last parameter on line, alternate file name, if any}
   ParamName := Parser.NextParam;
   FileName := LowerCase(Parser.StrValue);    // should be full path name to work universally

   InShowResults := True;

   {Assign default file name if alternate not specified}
   IF Length(FileName) = 0 then Begin
       CASE ParamPointer OF
          1: FileName := 'EXP_VOLTAGES.CSV';
          2: FileName := 'EXP_SEQVOLTAGES.CSV';
          3: FileName := 'EXP_CURRENTS.CSV';
          4: FileName := 'EXP_SEQCURRENTS.CSV';
          5: FileName := 'EXP_ESTIMATION.CSV';   // Estimation error
          6: FileName := 'EXP_CAPACITY.CSV';
          7: FileName := 'EXP_OVERLOADS.CSV';
          8: FileName := 'EXP_UNSERVED.CSV';
          9: FileName := 'EXP_POWERS.CSV';
         10: FileName := 'EXP_SEQPOWERS.CSV';
         11: FileName := 'EXP_FAULTS.CSV';
         12: FileName := 'EXP_GENMETERS.CSV';
         13: FileName := 'EXP_LOADS.CSV';
         14: FileName := 'EXP_METERS.CSV';
         {15: Filename is assigned}
         16: Filename := 'EXP_YPRIM.CSV';
         17: Filename := 'EXP_Y.CSV';
         18: Filename := 'EXP_SEQZ.CSV';
         19: Filename := 'EXP_P_BYPHASE.CSV';
         20: FileName := 'CDPSM_Unbalanced.XML';
         21: FileName := 'CDPSM_Connect.XML';
         22: FileName := 'CDPSM_Balanced.XML';
         23: FileName := 'EXP_BUSCOORDS.CSV';
         24: FileName := 'EXP_LOSSES.CSV';
         25: FileName := 'EXP_GUIDS.CSV';
         26: FileName := 'EXP_Counts.CSV';
         27: FileName := 'EXP_Summary.CSV';
       ELSE
             FileName := 'EXP_VOLTAGES.CSV';    // default
       END;
       FileName := DSSDataDirectory + CircuitName_ + FileName;  // Explicitly define directory
   End;

   CASE ParamPointer OF
      1: ExportVoltages(FileName);
      2: ExportSeqVoltages(FileName);
      3: ExportCurrents(FileName);
      4: ExportSeqCurrents(FileName);
      5: ExportEstimation(FileName);   // Estimation error
      6: ExportCapacity(FileName);
      7: ExportOverLoads(FileName);
      8: ExportUnserved(FileName, UEOnlyOpt);
      9: ExportPowers(FileName, MVAOpt);
     10: ExportSeqPowers(FileName, MVAopt);
     11: ExportFaultStudy(FileName);
     12: ExportGenMeters(FileName);
     13: ExportLoads(FileName);
     14: ExportMeters(FileName);
     15: IF   Length(Parm2) > 0 THEN Begin
           pMon:=MonitorClass.Find(Parm2);
           IF   pMon <> NIL  THEN Begin pMon.TranslateToCSV(FALSE); FileName := GlobalResult; End
                             ELSE DoSimpleMsg('Monitor "'+Parm2+'" not found.'+ CRLF + parser.CmdString, 250);
         End
         ELSE   DoSimpleMsg('Monitor Name Not Specified.'+ CRLF + parser.CmdString, 251);
     16: ExportYprim(Filename);
     17: ExportY(Filename);
     18: ExportSeqZ(Filename);
     19: ExportPbyphase(Filename, MVAOpt);
     20: ExportCDPSM_UnBal(Filename);        // defaults to a load-flow model
     21: ExportCDPSM_UnBal(Filename, False); // not a load-flow model
     22: ExportCDPSM_Bal(Filename);
     23: ExportBusCoords(Filename);
     24: ExportLosses(Filename);
     25: ExportGuids(Filename);
     26: ExportCounts(Filename);
     27: ExportSummary(Filename);
   ELSE
         ExportVoltages(FileName);    // default
   END;

   Result := 0;
   InShowResults := False;

   If AutoShowExport then  FireOffEditor(FileName);

End;



Procedure DisposeStrings;
Var i:Integer;

Begin
    For i := 1 to NumExportOptions Do Begin
       ExportOption[i] := '';
       ExportHelp[i]   := '';
   End;

End;

Initialization

    DefineOptions;

    ExportCommands := TCommandList.Create(ExportOption);
    ExportCommands.Abbrev := True;

Finalization

    DisposeStrings;
    ExportCommands.Free;
end.
