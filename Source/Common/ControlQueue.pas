unit ControlQueue;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   11-1-00 added Handle and delete function
}

{$M+}

interface

Uses Arraydef, ControlElem;

Type

    TTimeRec = RECORD
          Hour:Integer;
          Sec  :Double;
    END;

    pControlElementArray = ^ControlElementArray;
    ControlElementArray = Array[1..100] of TControlElem;
    pTimeArray = ^TimeArray;
    TimeArray = Array[1..100] of TTimeRec;

    TControlQueue = class(Tobject)
    private
       NumActions,
       MaxActions :Integer;
       ActionTimes :pTimeArray;
       ActionCodes,
       ActionHandles :pIntegerArray;
       ControlElements :pControlElementArray;

       DebugTrace: Boolean;
       Tracefile: TextFile;

       ctrlHandle: Integer;


       PROCEDURE PushDownArrays(i:Integer);
       PROCEDURE BumpUpStorage;
       PROCEDURE AllocActionArrays(NewSize:Integer);
       FUNCTION  Pop(const ActionTime:TTimeRec; Var Code, Hdl:Integer): TControlElem;  // Pop action from queue <= given time
       PROCEDURE DeleteFromQueue(i: Integer; popped:Boolean);
       FUNCTION TimeRecToTime(Trec:TTimeRec):Double;
       PROCEDURE Set_Trace(const Value: Boolean);
       PROCEDURE WriteTraceRecord(const ElementName: String;const Code:Integer; TraceParameter:Double;const s:String);

    protected

    public
      constructor Create;
      destructor Destroy; override;

      FUNCTION Push(Const Hour:Integer; Const Sec:Double; Const Code:Integer; Const Owner:TControlElem):Integer;
      PROCEDURE Clear;
      PROCEDURE DoAllActions;
      FUNCTION DoNearestActions(VAR Hour:Integer; VAR Sec:Double):Boolean;  // Do only actions with lowest time
      FUNCTION DoActions(const Hour:Integer; const sec: Double):Boolean;  // Do actions with time <= t
      FUNCTION IsEmpty:Boolean;
      PROCEDURE Delete(Hdl:Integer);  // Delete queue item by handle

      Property  TraceLog:Boolean Read DebugTrace Write Set_Trace;

    published

    End;


implementation

Uses DSSGlobals, sysutils;

{ TControlQueue }

Function TControlQueue.Push(Const Hour:Integer; const Sec: Double;Const code:Integer;   const Owner: TControlElem):Integer;

{Add a control action to the queue, sorted by lowest time first}
{Returns handle to the action}

VAR
   i,
   AddIndex,
   Hr         :Integer;
   ActionTime,
   S          :Double;
   Trec      :TTimeRec;

Begin
     IF NumActions = MaxActions  // No room for another one
     THEN BumpUpStorage;

     Inc(NumActions);
     AddIndex := NumActions;

     Inc(ctrlHandle); // just a serial number

     {Normalize the time }
     Hr := Hour;
     S  := Sec;
     If S > 3600.0
     THEN REPEAT
       Begin
           Hr := Hr +1;
           S := S - 3600.0;
       End
     UNTIL S < 3600.0;

     Trec.Hour := Hr;
     Trec.Sec  := S;

     ActionTime := TimeRecToTime(Trec);

     FOR i := 1 to NumActions-1 Do
       Begin
           If ActionTime <= TimeRecToTime(ActionTimes^[i])
           THEN Begin
               PushDownArrays(i);
               AddIndex := i;
               Break;
           End;
       End;

     ActionTimes^[AddIndex]     := Trec;
     ActionCodes^[AddIndex]     := Code;
     ActionHandles^[AddIndex]   :=  ctrlHandle;
     ControlElements^[AddIndex] := Owner;

     Result := ctrlHandle;

     IF (DebugTrace)  THEN WriteTraceRecord(Owner.Name, Code, Owner.DblTraceParameter,
                               Format('Handle %d Pushed onto Stack',[ctrlHandle]));
End;

PROCEDURE TControlQueue.AllocActionArrays(NewSize: Integer);
Begin
     ReallocMem(ActionTimes, SizeOf(ActionTimes^[1])*NewSize);
     ReallocMem(ActionCodes, SizeOf(ActionCodes^[1])*NewSize);
     ReallocMem(ActionHandles, SizeOf(ActionHandles^[1])*NewSize);
     ReallocMem(ControlElements, SizeOf(ControlElements^[1])*NewSize);
End;

PROCEDURE TControlQueue.BumpUpStorage;
Begin

     MaxActions := 2 * MaxActions;
     AllocActionArrays(MaxActions);

End;

PROCEDURE TControlQueue.Clear;
Begin
    NumActions := 0;
End;

constructor TControlQueue.Create;
Begin
     Inherited Create;
     ActionTimes     := NIL;
     ActionCodes     := NIL;
     ActionHandles   := NIL;
     ControlElements := NIL;
     MaxActions      := 10;
     ctrlHandle:=0;
     AllocActionArrays(MaxActions);
     NumActions      := 0;

     DebugTrace := FALSE;
End;

destructor TControlQueue.Destroy;
Begin
   AllocActionArrays(0);
   Inherited Destroy;
End;

PROCEDURE TControlQueue.DoAllActions;

VAR
   i:Integer;

Begin
     FOR i := 1 to NumActions Do ControlElements^[i].DoPendingAction(ActionCodes^[i]);
     NumActions := 0;
End;

FUNCTION TControlQueue.DoNearestActions( VAR Hour:Integer; VAR Sec:Double):Boolean;

// Do only those actions with the same delay time as the first action time
// Return time

VAR
   pElem     :TControlElem   ;
   t         :TTimeRec;
   Code, hdl      :Integer;
Begin
   Result := FALSE;
   IF NumActions > 0
   THEN Begin

       t := ActionTimes^[1];
       Hour := t.Hour;
       Sec  := t.Sec;
       pElem := Pop(t, Code, hdl);
       While pElem <> NIL Do
       Begin
           IF DebugTrace Then WriteTraceRecord(pElem.Name, Code, pElem.DblTraceParameter, Format('Pop Handle %d Do Nearest Action',[hdl]) );
           pElem.DoPendingAction(Code);
           Result := TRUE;
           pElem := Pop(t, Code, hdl);
       End;

   End;

End;

function TControlQueue.IsEmpty: Boolean;
begin
     IF NumActions = 0
     THEN Result := True
     ELSE Result := False;
end;

PROCEDURE TControlQueue.PushDownArrays(i: Integer);

VAR
   j:integer;
Begin

     FOR j := NumActions downto i+1 Do
     Begin
         ActionTimes^[j]     := ActionTimes^[j-1];
         ActionCodes^[j]     := ActionCodes^[j-1];
         ActionHandles^[j]   := ActionHandles^[j-1];
         ControlElements^[j] := ControlElements^[j-1];
     End;
End;

FUNCTION TControlQueue.Pop(const ActionTime: TTimeRec; Var Code, Hdl:Integer): TControlElem;
 // pop off next control action with an action time <= ActionTime (sec)

VAR
   i    :Integer;
   t    :Double ;

Begin
      Result := NIL;
      t := TimeRecToTime(ActionTime);
      FOR i := 1 to NumActions Do
      Begin
          IF TimeRecToTime(ActionTimes^[i]) <= t
          THEN Begin
              Result :=  ControlElements^[i];
              Code := ActionCodes^[i];
              Hdl  := ActionHandles^[i];
              DeleteFromQueue(i, TRUE);
              Break;
          End;
      End;
End;

PROCEDURE TControlQueue.DeleteFromQueue(i: Integer; popped:Boolean);
// Delete i-th element from the Queue
VAR
   j         :Integer;
   pElem     :TControlElem;
   S         :String;

Begin
     pElem := ControlElements^[i];
     IF (DebugTrace)  THEN Begin
           If Popped Then S := 'by Pop function' Else S := 'by control device' ;
           WriteTraceRecord(pElem.Name, ActionCodes^[i], pelem.dbltraceParameter,
                           Format('Handle %d deleted from Queue %s',[ActionHandles^[i], S]));
     End;
     NumActions := NumActions -1;
     FOR j := i to NumActions Do
     Begin
          ActionTimes^[j] := ActionTimes^[j+1];
          ActionCodes^[j] := ActionCodes^[j+1];
          ActionHandles^[j] := ActionHandles^[j+1];
          ControlElements^[j]   := ControlElements^[j+1]
     End;
End;

FUNCTION TControlQueue.DoActions(const Hour:Integer; const sec: Double):Boolean;

// Do all actions having an action time <= t

VAR
   pElem     :TControlElem;
   t         :TTimeRec;
   Code, hdl      :Integer;

Begin
   Result := FALSE;
   IF NumActions > 0
   THEN Begin

       t.Hour := Hour;
       t.Sec  := Sec;
       pElem := Pop(t, Code, hdl);
       While pElem <> NIL Do
       Begin
           IF (DebugTrace)  THEN WriteTraceRecord(pElem.Name, Code, pelem.dbltraceParameter, Format('Pop Handle %d Do Action',[Hdl]));
           pElem.DoPendingAction(code);
           Result := TRUE;
           pElem := Pop(t, Code, hdl);
       End;
   End;

end;


FUNCTION TControlQueue.TimeRecToTime(Trec: TTimeRec): Double;
begin
     With Trec Do Result := Hour * 3600.0 + Sec
end;

PROCEDURE TControlQueue.Set_Trace(const Value: Boolean);
begin

     DebugTrace := Value;

     If DebugTrace
     THEN Begin
          AssignFile(TraceFile, DSSDataDirectory + 'Trace_ControlQueue.CSV');
          ReWrite(TraceFile);
          Writeln(TraceFile, '"Hour", "sec", "Control Iteration", "Element", "Action Code", "Trace Parameter", "Description"');
          CloseFile(Tracefile);
     End;

end;

PROCEDURE TControlQueue.WriteTraceRecord(const ElementName: String;const Code:Integer; TraceParameter:Double;const s:String);
VAR
   Separator :String;

Begin

      Try
        IF (Not InshowResults)
        THEN Begin
             Separator := ', ';
             Append(TraceFile);
             Writeln(TraceFile,
                      ActiveCircuit.Solution.intHour:0, Separator,
                      ActiveCircuit.Solution.DynaVars.t:0:3, Separator,
                      ActiveCircuit.Solution.ControlIteration:0, Separator,
                      ElementName, Separator,
                      Code:0, Separator,
                      Format('%-.g',[TraceParameter]), Separator,
                      S );

             CloseFile(TraceFile);
        End;

      Except
            On E:Exception Do Begin End;

      End;

end;

PROCEDURE TControlQueue.Delete(Hdl: Integer);

{Delete an item by its Handle reference}

Var
   i:Integer;
begin
     For i := 1 to NumActions Do Begin
         IF ActionHandles^[i] = Hdl THEN  Begin
              DeleteFromQueue(i, FALSE);
              Exit;
            End;
     End;
end;

end.
