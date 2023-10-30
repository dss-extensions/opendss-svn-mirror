unit DParallel;

interface

uses
    Arraydef,
    UComplex,
    Solution;

function ParallelI(mode: Longint; arg: Longint): Longint; CDECL;
procedure ParallelV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    SysUtils,
    Variants,
    CktElement,
    ParserDel,
    KLUSolve,
    {$IFDEF FPC_DLL}
Classes
    {$ELSE}
    Dialogs,
    System.Classes
    {$ENDIF}
    ;

function ParallelI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0;             // Default return value
    case mode of
        0:
        begin  // Parallel.NumCPUs Read
            Result := CPU_Cores;
        end;
        1:
        begin  // Parallel.NumCores Read
            Result := round(CPU_Cores / 2);
        end;
        2:
        begin  // Parallel.ActiveActor Read
            Result := ActiveActor;
        end;
        3:
        begin  // Parallel.ActiveActor Write
            if arg <= NumOfActors then
                ActiveActor := arg
            else
                DoSimpleMsg('The actor does not exists', 7002);
        end;
        4:
        begin  // Parallel.CreateActor Write
            New_Actor_Slot();
        end;
        5:
        begin  // Parallel.ActorCPU Read
            Result := ActorCPU[ActiveActor];
        end;
        6:
        begin  // Parallel.ActorCPU Write
            if arg < CPU_Cores then
            begin
                ActorCPU[ActiveActor] := arg;
                if ActorHandle[ActiveActor] <> nil then
                begin
                    ActorHandle[ActiveActor].CPU := ActorCPU[ActiveActor];
                    ActorHandle[ActiveActor].Priority := tpTimeCritical;
                end;
            end
            else
                DoSimpleMsg('The CPU does not exist', 7004);
        end;
        7:
        begin  // Parallel.NumActors Read
            Result := NumOfActors;
        end;
        8:
        begin  // Parallel.Wait
            if Parallel_enabled then
                Wait4Actors(0);
        end;
        9:
        begin  // Parallel.ActiveParallel Read
            if Parallel_enabled then
                Result := 1
            else
                Result := 0;
        end;
        10:
        begin  // Parallel.ActiveParallel Write
            if arg <> 0 then
                Parallel_enabled := true
            else
                Parallel_enabled := false;
        end;
        11:
        begin  // Parallel.ConcatenateReports Read
            if ConcatenateReports then
                Result := 1
            else
                Result := 0;
        end;
        12:
        begin  // Parallel.ConcatenateReports Write
            if arg <> 0 then
                ConcatenateReports := true
            else
                ConcatenateReports := false;
        end
    else
        Result := -1;
    end;
end;


procedure ParallelV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
var
    i: Integer;

begin
    case mode of
        0:
        begin  // Parallel.ActorProgress Read
            myType := 1;        // Integer
            setlength(myIntArray, NumOfActors);
            for i := 1 to NumOfActors do
                myIntArray[i - 1] := ActorPctProgress[i];
            myPointer := @(myIntArray[0]);
            mySize := SizeOf(myIntArray[0]) * Length(myIntArray);
        end;
        1:
        begin  // Parallel.AxtorState Read
            myType := 1;        // Integer
            setlength(myIntArray, NumOfActors);
            for i := 1 to NumOfActors do
                myIntArray[i - 1] := ActorStatus[i];
            myPointer := @(myIntArray[0]);
            mySize := SizeOf(myIntArray[0]) * Length(myIntArray);
        end
    else
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;

end;

end.
