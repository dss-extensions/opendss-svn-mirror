unit DDSSElement;

interface

function DSSElementI(mode: Longint; arg: Longint): Longint; CDECL;
function DSSElementS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure DSSElementV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    Variants,
    Sysutils;

function DSSElementI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // DSSElement.NumProperties
            Result := 0;
            if ActiveCircuit[ActiveActor] <> nil then
                with ActiveCircuit[ActiveActor] do
                begin
                    if ActiveDSSObject <> nil then
                        with ActiveDSSObject[ActiveActor] do
                        begin
                            Result := ParentClass.NumProperties;
                        end
                end;
        end
    else
        Result := -1;
    end;
end;

//*********************************String type properties**************************
function DSSElementS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
begin
    Result := Pansichar(Ansistring(''));// Default return value
    case mode of
        0:
        begin
            if ActiveCircuit[ActiveActor] <> nil then
                if ActiveDSSObject[ActiveActor] <> nil then
                    with ActiveDSSObject[ActiveActor] do
                    begin
                        Result := Pansichar(Ansistring(ParentClass.Name + '.' + Name));
                    end
                else
                    Result := Pansichar(Ansistring(''));
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not recognized'));
    end;
end;

//*****************************Variant type properties**************************
procedure DSSElementV(mode: Longint; out arg: Variant); CDECL;

var
    k: Integer;

begin
    case mode of
        0:
        begin  // DSSElement.AllPropertyNames
            arg := VarArrayCreate([0, 0], varOleStr);
            if ActiveCircuit[ActiveActor] <> nil then
                with ActiveCircuit[ActiveActor] do
                begin
                    if ActiveDSSObject[ActiveActor] <> nil then
                        with ActiveDSSObject[ActiveActor] do
                        begin
                            with ParentClass do
                            begin
                                arg := VarArrayCreate([0, NumProperties - 1], varOleStr);
                                for k := 1 to NumProperties do
                                begin
                                    arg[k - 1] := PropertyName^[k];
                                end;
                            end;
                        end
                end;
        end
    else
        arg[0] := 'Error, parameter not recognized';
    end;
end;

end.
