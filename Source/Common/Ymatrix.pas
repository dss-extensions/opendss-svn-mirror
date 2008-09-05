unit Ymatrix;

{
   Unit to manage System Y matrix

   6-11-00  Created from Solution.Pas
}

interface

uses uComplex,SysUtils;


{Options for building Y matrix}
CONST
      SERIESONLY = 1;
      WHOLEMATRIX = 2;

TYPE
  EEsolv32Problem = class(Exception);


PROCEDURE BuildYMatrix(BuildOption :Integer; AllocateVI:Boolean);
PROCEDURE ResetSparseMatrix(var hY:Integer; size:integer);
PROCEDURE InitializeNodeVbase;

Function CheckYMatrixforZeroes:String;

{Declare FUNCTIONs in ETKSolve DLL}

{$INCLUDE Esolv32Declarations.pas}



implementation

Uses DSSGlobals, Circuit, CktElement, Utilities;



//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE ReCalcAllYPrims;

VAR
   pElem:TCktElement;

Begin

  WITH ActiveCircuit Do
  Begin
     If LogEvents Then LogThisEvent('Recalc All Yprims');
     pElem := CktElements.First;
     WHILE pElem<>nil Do Begin
       pElem.CalcYPrim;
       pElem := CktElements.Next;
     End;
  End;

End;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE ReCalcInvalidYPrims;
{Recalc YPrims only for those circuit elements that have had changes since last
 solution}
VAR
   pElem:TCktElement;

Begin

  WITH ActiveCircuit Do
  Begin
     If LogEvents Then LogThisEvent('Recalc Invalid Yprims');
     pElem := CktElements.First;
     WHILE pElem<>nil Do
     Begin
       WITH pElem Do
       IF YprimInvalid THEN CalcYPrim;
       pElem := CktElements.Next;
     End;
  End;

End;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
PROCEDURE ResetSparseMatrix(var hY:Integer; size:integer);


Begin

     IF hY<>0 THEN Begin
         IF DeleteSparseSet(hY) > 0  {Get rid of existing one beFore making a new one}
         THEN Raise EEsolv32Problem.Create('Error Deleting System Y Matrix in ResetSparseMatrix. Problem with Sparse matrix solver.');

         hY := 0;
     End;

     // Make a new sparse set
     hY := NewSparseSet(Size);
     If hY<1 THEN Begin   // Raise and exception
        Raise EEsolv32Problem.Create('Error Creating System Y Matrix. Problem WITH Sparse matrix solver.');
     End;
End;


Procedure InitializeNodeVbase;

Var
   i: Integer;

Begin

    WITH ActiveCircuit, Solution  Do Begin
       FOR i := 1 to NumNodes Do
         WITH MapNodeToBus^[i]  Do
         Begin
              NodeVbase^[i] := Buses^[BusRef].kvbase * 1000.0;
         End;
         VoltageBaseChanged := FALSE;
    End;
End;

PROCEDURE BuildYMatrix(BuildOption :Integer; AllocateVI:Boolean);

{Builds designated Y matrix for system and allocates solution arrays}

VAR
   Cvalue:Complex;
   i,j,k,iNode, jNode,
   YMatrixsize:Integer;
   CmatArray:pComplexArray;
   pElem:TCktElement;

   //{****} FTrace: TextFile;


Begin

  //{****} AssignFile(Ftrace, 'YmatrixTrace.txt');
  //{****} Rewrite(FTrace);

   CmatArray := Nil;
   WITH ActiveCircuit, ActiveCircuit.Solution  Do Begin

     If PreserveNodeVoltages Then UpdateVBus; // Update voltage values stored with Bus object

     // the following re counts the number of buses and resets meter zones and feeders
     // If radial but systemNodeMap not set then init for radial got skipped due to script sequence
     IF (BusNameRedefined) THEN ReProcessBusDefs;      // This changes the node references into the system Y matrix!!

     YMatrixSize := NumNodes;

     Case BuildOption of
         WHOLEMATRIX: ResetSparseMatrix(hY, YMatrixSize);
         SERIESONLY: ResetSparseMatrix(hYSeries, YMatrixSize);
     End;

     // tune up the Yprims if necessary
     IF  (FrequencyChanged)
     THEN ReCalcAllYPrims
     ELSE ReCalcInvalidYPrims;
     FrequencyChanged := FALSE;

     If LogEvents Then  Case BuildOption of
        WHOLEMATRIX: LogThisEvent('Building Whole Y Matrix');
        SERIESONLY: LogThisEvent('Building Series Y Matrix');
     End;
          // Add in Yprims for all devices
     pElem := CktElements.First;
     WHILE pElem <> Nil Do
       Begin
         WITH pElem Do
         IF  (Enabled) THEN           // Add stuff only if enabled
          Begin
           Case BuildOption of
              WHOLEMATRIX : CmatArray := GetYPrimValues(ALL_YPRIM);
              SERIESONLY:   CmatArray := GetYPrimValues(SERIES)
           End;
           // Here's where everything gets stuffed into the Y matrix
           // Diagonals
           k := 1;
           If (CmatArray <> Nil) THEN
            FOR i := 1 to Yorder  Do
              Begin
                iNode := NodeRef^[i];

                If   (iNode > 0)  THEN
                  Begin
                         jNode := iNode;
                         Cvalue :=  CmatArray^[k];
                         IF (Cvalue.re <> 0.0) OR (Cvalue.im <> 0.0)  THEN
                          Begin
                             //{****} Writeln(Ftrace, pElem.Name, iNode:4, jNode:4, Cvalue.re:12, Cvalue.im:12);
                             IF   AddMatrixElement(iNode, jNode, @Cvalue) > 0  THEN
                                  Raise EEsolv32Problem.Create('Error Adding to System Y Matrix. Problem with Sparse matrix solver.');
                          End;
                  End; // If iNode
                Inc(k, Yorder+1);
              End; // For i


           // Off Diagonals
           If (CmatArray <> Nil) THEN
            FOR i := 1 to Yorder  Do
              Begin
                iNode := NodeRef^[i];
                If   (iNode > 0)  THEN
                  Begin
                     k := i;
                     FOR j := 1 to i-1 Do
                       Begin
                         jNode := NodeRef^[j];
                         If   (jNode > 0) THEN
                           Begin
                             // Cvalue :=  CmatArray^[(j-1) * Yorder + i];
                             Cvalue :=  CmatArray^[k];
                             IF (Cvalue.re <> 0.0) OR (Cvalue.im <> 0.0)  THEN
                              Begin
                                 //{****} Writeln(Ftrace, pElem.Name, iNode:4, jNode:4, Cvalue.re:12, Cvalue.im:12);
                                 {AddMatrixElement automatically gets the symmetrical off-diagonal}
                                 IF AddMatrixElement(iNode, jNode, @Cvalue) > 0  THEN
                                      Raise EEsolv32Problem.Create('Error Adding to System Y Matrix. Problem with Sparse matrix solver.');
                                 IF iNode = jNode Then  // Do it again if on the diagonal of the target Y matrix
                                      IF AddMatrixElement(iNode, jNode, @Cvalue) > 0  THEN
                                           Raise EEsolv32Problem.Create('Error Adding to System Y Matrix. Problem with Sparse matrix solver.');
                              End;
                           End;
                         Inc(k, Yorder);
                       End; // For j
                  End; // If NodeRef[i]
              End; // For i

           End;   // If Enabled
         pElem := CktElements.Next;
       End;

     //{****} CloseFile(Ftrace);
     //{****} FireOffEditor(  'YmatrixTrace.txt');

     // Allocate voltage and current vectors if requested
     IF   AllocateVI
     THEN Begin
         If LogEvents Then LogThisEvent('ReAllocating Solution Arrays');
         ReAllocMem(NodeV,    SizeOf(NodeV^[1])        * (NumNodes+1)); // Allocate System Voltage array - allow for zero element
         NodeV^[0] := CZERO;
         ReAllocMem(Currents, SizeOf(Currents^[1]) * (NumNodes+1)); // Allocate System current array
         ReAllocMem(AuxCurrents, SizeOf(AuxCurrents^[1]) * NumNodes); // Allocate System current array
         IF (VMagSaved  <> Nil) THEN ReallocMem(VMagSaved, 0);
         IF (ErrorSaved <> Nil) THEN ReallocMem(ErrorSaved, 0);
         IF (NodeVBase  <> Nil) THEN ReallocMem(NodeVBase, 0);
         VMagSaved      := AllocMem(Sizeof(VMagSaved^[1])  * NumNodes);  // zero fill
         ErrorSaved     := AllocMem(Sizeof(ErrorSaved^[1]) * NumNodes);  // zero fill
         NodeVBase      := AllocMem(Sizeof(NodeVBase^[1]) * NumNodes);  // zero fill
         InitializeNodeVbase;

     End;

     Case BuildOption of
          WHOLEMATRIX: Begin
                           SeriesYInvalid := True;  // Indicate that the Series matrix may not match
                           SystemYChanged := False;
                       End;
          SERIESONLY: SeriesYInvalid := False;  // SystemYChange unchanged
     End;

    // Deleted RCD only done now on mode change
    // SolutionInitialized := False;  //Require initialization of voltages if Y changed

    If PreserveNodeVoltages Then RestoreNodeVfromVbus;
    
   End;
End;

Function CheckYMatrixforZeroes:String;

Var i:Integer;
    c:Complex;
Begin

  Result := '';
  With ActiveCircuit Do
   For i := 1 to Numnodes Do Begin
       GetMatrixElement(i, i, @c);
       If Cabs(C)=0.0 Then With MapNodeToBus^[i] Do Begin
           Result := Result + Format('%sZero diagonal for bus %s, node %d',[CRLF, BusList.Get(Busref), NodeNum]);
       End;
   End;

End;


end.
