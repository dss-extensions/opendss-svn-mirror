library OpenDSSDirect;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }



uses
  System.SysUtils,
  System.Classes,
  Dialogs,
  AutoAdd in '..\Common\AutoAdd.pas',
  Bus in '..\Common\Bus.pas',
  Circuit in '..\Common\Circuit.pas',
  CktElement in '..\Common\CktElement.pas',
  CktElementClass in '..\Common\CktElementClass.pas',
  Conductor in '..\Common\Conductor.pas',
  ControlQueue in '..\Common\ControlQueue.pas',
  DSSCallBackRoutines in '..\Common\DSSCallBackRoutines.pas',
  DSSClass in '..\Common\DSSClass.pas',
  DSSClassDefs in '..\Common\DSSClassDefs.pas',
  DSSForms in '..\Common\DSSForms.pas',
  DSSGlobals in '..\Common\DSSGlobals.pas',
  EventQueue in '..\Common\EventQueue.pas',
  ExportCIMXML in '..\Common\ExportCIMXML.pas',
  ExportResults in '..\Common\ExportResults.pas',
  Feeder in '..\Common\Feeder.pas',
  Notes in '..\Common\Notes.pas',
  ShowResults in '..\Common\ShowResults.pas',
  Solution in '..\Common\Solution.pas',
  SolutionAlgs in '..\Common\SolutionAlgs.pas',
  Terminal in '..\Common\Terminal.pas',
  TOPExport in '..\Common\TOPExport.pas',
  Utilities in '..\Common\Utilities.pas',
  Ymatrix in '..\Common\Ymatrix.pas',
  CapControl in '..\Controls\CapControl.pas',
  CapControlVars in '..\Controls\CapControlVars.pas',
  CapUserControl in '..\Controls\CapUserControl.pas',
  ControlClass in '..\Controls\ControlClass.pas',
  ControlElem in '..\Controls\ControlElem.pas',
  ExpControl in '..\Controls\ExpControl.pas',
  GenDispatcher in '..\Controls\GenDispatcher.pas',
  InvControl in '..\Controls\InvControl.pas',
  Recloser in '..\Controls\Recloser.pas',
  RegControl in '..\Controls\RegControl.pas',
  Relay in '..\Controls\Relay.pas',
  StorageController in '..\Controls\StorageController.pas',
  SwtControl in '..\Controls\SwtControl.pas',
  UPFCControl in '..\Controls\UPFCControl.pas',
  MyDSSClassDefs in '..\EXE\MyDSSClassDefs.Pas',
  ExecCommands in '..\Executive\ExecCommands.pas',
  ExecHelper in '..\Executive\ExecHelper.pas',
  ExecOptions in '..\Executive\ExecOptions.pas',
  Executive in '..\Executive\Executive.pas',
  ExportOptions in '..\Executive\ExportOptions.pas',
  PlotOptions in '..\Executive\PlotOptions.pas',
  ShowOptions in '..\Executive\ShowOptions.pas',
  About in '..\Forms\About.pas' {AboutBox},
  DlgComboBox in '..\Forms\DlgComboBox.pas' {OptionComboForm},
  dlgNumber in '..\Forms\dlgNumber.pas' {ValueEntryForm},
  DlgPlotOptions in '..\Forms\DlgPlotOptions.pas' {PlotOptionsForm},
  FrmCSVchannelSelect in '..\Forms\FrmCSVchannelSelect.pas' {ChannelSelectForm},
  FrmDoDSSCommand in '..\Forms\FrmDoDSSCommand.pas' {DoDSSCommandForm},
  HelpForm in '..\Forms\HelpForm.pas' {HelpForm1},
  ListForm in '..\Forms\ListForm.pas' {ListBoxForm},
  MessageForm in '..\Forms\MessageForm.pas' {MessageForm1},
  Panel in '..\Forms\Panel.pas' {ControlPanel},
  ProgressForm in '..\Forms\ProgressForm.pas' {Progress},
  PropEdit in '..\Forms\PropEdit.pas' {PropEditForm},
  Scriptform in '..\Forms\Scriptform.pas' {MainEditForm},
  ScriptformNormal in '..\Forms\ScriptformNormal.pas' {MainEditFormNormal},
  TViewer in '..\Forms\TViewer.pas' {TViewForm},
  CableConstants in '..\General\CableConstants.pas',
  CableData in '..\General\CableData.pas',
  CNData in '..\General\CNData.pas',
  CNLineConstants in '..\General\CNLineConstants.pas',
  ConductorData in '..\General\ConductorData.pas',
  DSSObject in '..\General\DSSObject.pas',
  GrowthShape in '..\General\GrowthShape.pas',
  LineCode in '..\General\LineCode.pas',
  LineConstants in '..\General\LineConstants.pas',
  LineGeometry in '..\General\LineGeometry.pas',
  LineSpacing in '..\General\LineSpacing.pas',
  LoadShape in '..\General\LoadShape.pas',
  NamedObject in '..\General\NamedObject.pas',
  OHLineConstants in '..\General\OHLineConstants.pas',
  PriceShape in '..\General\PriceShape.pas',
  Spectrum in '..\General\Spectrum.pas',
  TCC_Curve in '..\General\TCC_Curve.pas',
  TempShape in '..\General\TempShape.pas',
  TSData in '..\General\TSData.pas',
  TSLineConstants in '..\General\TSLineConstants.pas',
  WireData in '..\General\WireData.pas',
  XfmrCode in '..\General\XfmrCode.pas',
  XYcurve in '..\General\XYcurve.pas',
  EnergyMeter in '..\Meters\EnergyMeter.pas',
  MeterClass in '..\Meters\MeterClass.pas',
  MeterElement in '..\Meters\MeterElement.pas',
  Monitor in '..\Meters\Monitor.pas',
  ReduceAlgs in '..\Meters\ReduceAlgs.pas',
  Sensor in '..\Meters\Sensor.pas',
  Frm_RPNcalc in '..\Parser\Frm_RPNcalc.pas' {RPNForm},
  ParserDel in '..\Parser\ParserDel.pas',
  RPN in '..\Parser\RPN.pas',
  Equivalent in '..\PCElements\Equivalent.pas',
  generator in '..\PCElements\generator.pas',
  GeneratorVars in '..\PCElements\GeneratorVars.pas',
  GenUserModel in '..\PCElements\GenUserModel.pas',
  GICLine in '..\PCElements\GICLine.pas',
  Isource in '..\PCElements\Isource.pas',
  Load in '..\PCElements\Load.pas',
  PCClass in '..\PCElements\PCClass.pas',
  PCElement in '..\PCElements\PCElement.pas',
  PVsystem in '..\PCElements\PVsystem.pas',
  PVSystemUserModel in '..\PCElements\PVSystemUserModel.pas',
  Storage in '..\PCElements\Storage.pas',
  StorageVars in '..\PCElements\StorageVars.pas',
  StoreUserModel in '..\PCElements\StoreUserModel.pas',
  UPFC in '..\PCElements\UPFC.pas',
  VSConverter in '..\PCElements\VSConverter.pas',
  VSource in '..\PCElements\VSource.pas',
  Capacitor in '..\PDElements\Capacitor.pas',
  Fault in '..\PDElements\Fault.pas',
  fuse in '..\PDElements\fuse.pas',
  GICTransformer in '..\PDElements\GICTransformer.pas',
  Line in '..\PDElements\Line.pas',
  PDClass in '..\PDElements\PDClass.pas',
  PDElement in '..\PDElements\PDElement.pas',
  Reactor in '..\PDElements\Reactor.pas',
  Transformer in '..\PDElements\Transformer.pas',
  DSSGraph in '..\Plot\DSSGraph.pas',
  DSSPlot in '..\Plot\DSSPlot.pas',
  Arraydef in '..\Shared\Arraydef.pas',
  CktTree in '..\Shared\CktTree.pas',
  Command in '..\Shared\Command.pas',
  Dynamics in '..\Shared\Dynamics.pas',
  HashList in '..\Shared\HashList.pas',
  IniRegSave in '..\Shared\IniRegSave.pas',
  LineUnits in '..\Shared\LineUnits.pas',
  mathutil in '..\Shared\mathutil.pas',
  PointerList in '..\Shared\PointerList.pas',
  Pstcalc in '..\Shared\Pstcalc.pas',
  RCDList in '..\Shared\RCDList.pas',
  ReportForm in '..\Shared\ReportForm.pas' {Report},
  StackDef in '..\Shared\StackDef.pas',
  Ucmatrix in '..\Shared\Ucmatrix.pas',
  Ucomplex in '..\Shared\Ucomplex.pas',
  DText in 'DText.pas',
  DLoads in 'DLoads.pas',
  DIDSSProperty in 'DIDSSProperty.pas',
  OpenDSSEngine_TLB in '..\DLL\OpenDSSEngine_TLB.pas',
  DCktElement in 'DCktElement.pas',
  DError in 'DError.pas',
  DCircuit in 'DCircuit.pas',
  DBus in 'DBus.pas',
  DSolution in 'DSolution.pas',
  DMonitors in 'DMonitors.pas',
  DMeters in 'DMeters.pas',
  DGenerators in 'DGenerators.pas',
  DDSSProgress in 'DDSSProgress.pas',
  DSettings in 'DSettings.pas',
  DLines in 'DLines.pas',
  DCtrlQueue in 'DCtrlQueue.pas',
  DDSSElement in 'DDSSElement.pas',
  DActiveClass in 'DActiveClass.pas',
  DCapacitors in 'DCapacitors.pas',
  DTransformers in 'DTransformers.pas',
  DSwtControls in 'DSwtControls.pas',
  DCapControls in 'DCapControls.pas',
  DRegControls in 'DRegControls.pas',
  DTopology in 'DTopology.pas',
  DDSSExecutive in 'DDSSExecutive.pas',
  DSensors in 'DSensors.pas',
  DXYCurves in 'DXYCurves.pas',
  DPDELements in 'DPDELements.pas',
  DReclosers in 'DReclosers.pas',
  DRelays in 'DRelays.pas',
  DCmathLib in 'DCmathLib.pas',
  DParser in 'DParser.pas',
  DLoadShape in 'DLoadShape.pas',
  DFuses in 'DFuses.pas',
  DISource in 'DISource.pas',
  DPVSystems in 'DPVSystems.pas',
  DVSources in 'DVSources.pas',
  DDSS in 'DDSS.pas',
  DYMatrix in 'DYMatrix.pas',
  KLUSolve in '..\Common\KLUSolve.pas',
  ScriptEdit in '..\Forms\ScriptEdit.pas',
  vccs in '..\PCElements\vccs.pas',
  MemoryMap_lib in '..\Meters\MemoryMap_lib.pas',
  Parallel_Lib in '..\Parallel_Lib\Parallel_Lib.pas',
  DParallel in 'DParallel.pas',
  ESPVLControl in '..\Controls\ESPVLControl.pas',
  IndMach012 in '..\PCElements\IndMach012.pas',
  DLineCodes in 'DLineCodes.pas',
  TCP_IP in '..\TCP_IP\TCP_IP.pas',
  ConnectOptions in '..\Executive\ConnectOptions.pas',
  Diakoptics in '..\Common\Diakoptics.pas',
  Sparse_Math in '..\Common\Sparse_Math.pas',
  MeTIS_Exec in '..\Common\MeTIS_Exec.pas',
  GICsource in '..\PCElements\GICsource.pas',
  AutoTrans in '..\PDElements\AutoTrans.pas',
  fMonitor in '..\Meters\fMonitor.pas',
  LD_fm_infos in '..\Meters\LD_fm_infos.pas',
  VLNodeVars in '..\Meters\VLNodeVars.pas',
  Generic5OrderMach in '..\PCElements\Generic5OrderMach.pas',
  DGICSources in 'DGICSources.pas';

//**************************end of the functions*************************************
exports
   DSSPut_Command,
   DSSLoads, DSSLoadsF,DSSLoadsS, DSSLoadsV,
   DSSProperties,
   CktElementI, CktElementF, CktElementS, CktElementV,
   ErrorCode, ErrorDesc,
   CircuitI, CircuitF, CircuitS, CircuitV,
   BusI, BusF, BusS, BusV,
   SolutionI, SolutionF, SolutionS, SolutionV,
   MonitorsI, MonitorsS,MonitorsV,
   MetersI,MetersF,MetersS,MetersV,
   GeneratorsI,GeneratorsF,GeneratorsS,GeneratorsV,
   DSSProgressI,DSSProgressS,
   SettingsI,SettingsF,SettingsS,SettingsV,
   LinesI, LinesF, LinesS, LinesV,
   CtrlQueueI,CtrlQueueV,
   DSSElementI, DSSElementS, DSSElementV,
   ActiveClassI, ActiveClassS, ActiveClassV,
   CapacitorsI,CapacitorsF,CapacitorsS,CapacitorsV,
   TransformersI,TransformersF,TransformersS,TransformersV,
   SwtControlsI,SwtControlsF,SwtControlsS,SwtControlsV,
   CapControlsI,CapControlsF,CapControlsS,CapControlsV,
   RegControlsI,RegControlsF,RegControlsS,RegControlsV,
   TopologyI,TopologyS,TopologyV,
   DSSExecutiveI,DSSExecutiveS,
   SensorsI,SensorsF,SensorsS,SensorsV,
   XYCurvesI,XYCurvesF,XYCurvesS,XYCurvesV,
   PDElementsI,PDElementsF,PDElementsS,
   ReclosersI, ReclosersF, ReclosersS, ReclosersV,
   RelaysI,RelaysS,RelaysV,
   CmathLibF,CmathLibV,
   ParserI,ParserF,ParserS,ParserV,
   LoadShapeI,LoadShapeF,LoadShapeS,LoadShapeV,
   FusesI,FusesF,FusesS,FusesV,
   IsourceI,IsourceF,IsourceS,IsourceV,
   PVsystemsI,PVsystemsF,PVsystemsS,PVsystemsV,
   VsourcesI,VsourcesF,VsourcesS,VsourcesV,
   DSSI,DSSS,DSSV,
   InitAndGetYparams,GetCompressedYMatrix,ZeroInjCurr,GetSourceInjCurrents,GetPCInjCurr,
   SystemYChanged,BuildYMatrixD,UseAuxCurrents,AddInAuxCurrents,getIpointer,
   getVpointer,SolveSystem,ParallelI,ParallelV,LineCodesI,LineCodesF,LineCodesS,
   LineCodesV, GICSourcesI,GICSourcesF,GICSourcesS,GICSourcesV;

   {$R *.RES}

begin

{Library initialization code}
{Intialize the internal interfaces so they're ready to go}
  IsDLL := TRUE;
  IsMultiThread := True;
{Create one instance of DSS executive whenever the DSS Engine is init'd}
  DSSExecutive := TExecutive.Create;  // Start the DSS when DSS interface is created
  DSSExecutive.CreateDefaultDSSItems;

  //WriteDLLDebugFile(DSSDirectory);

end.
