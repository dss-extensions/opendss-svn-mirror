unit DIDSSProperty;
// DSSProperties Interface
// This interface implements the DSSproperties (IDSSProperties) interface of OpenDSS by declaring 4 procedures for accessing the different properties included in this interface.

interface

function DSSProperties(mode: Longint; arg: Pansichar): Pansichar; CDECL;

implementation

uses
    DSSClass,
    DSSGlobals,
    Executive,
    SysUtils;

var
    FPropIndex: Integer;

function DSSProperties(mode: Longint; arg: Pansichar): Pansichar; CDECL;
var
    TempPropIndex: Integer;
    prop_name: String;
begin
    Result := Pansichar(Ansistring('')); // Default return value
    case mode of
        0:
        begin                                           // DSSProperties.Name
            if not TryStrToInt(String(arg), TempPropIndex) then
                TempPropIndex := -1;  // If this is the case, we are not getting an argument or it is not numeric. Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).

            if TempPropIndex > 0 then  // Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).
                FPropIndex := TempPropIndex + 0;

            if (ActiveCircuit[ActiveActor] <> nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
                with ActiveDSSObject[ActiveActor].ParentClass do
                    if (FPropIndex <= NumProperties) and (FPropIndex > 0) then
                        Result := Pansichar(Ansistring(PropertyName^[FPropIndex]));
        end;
        1:
        begin                                           // DSSProperties.Description
            if not TryStrToInt(String(arg), TempPropIndex) then
                TempPropIndex := -1;  // If this is the case, we are not getting an argument or it is not numeric. Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).

            if TempPropIndex > 0 then  // Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).
                FPropIndex := TempPropIndex + 0;

            if (ActiveCircuit[ActiveActor] <> nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
                with ActiveDSSObject[ActiveActor].ParentClass do
                    if (FPropIndex <= NumProperties) and (FPropIndex > 0) then
                        Result := Pansichar(Ansistring(PropertyHelp^[FPropIndex]));
        end;
        2:
        begin                                           // DSSProperties.Val - read
            if not TryStrToInt(String(arg), TempPropIndex) then
                TempPropIndex := -1;  // If this is the case, we are not getting an argument or it is not numeric. Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).

            if TempPropIndex > 0 then  // Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).
                FPropIndex := TempPropIndex + 0;

            if (ActiveCircuit[ActiveActor] <> nil) and (FPropIndex <> 0) then
                with ActiveDSSObject[ActiveActor] do
                    if (FPropIndex <= ParentClass.NumProperties) and (FPropIndex > 0) then
                        Result := Pansichar(Ansistring(PropertyValue[ParentClass.PropertyIdxMap^[FPropIndex]]));
        end;
        3:
        begin                                           // DSSProperties.Val - write
            if (ActiveCircuit[ActiveActor] <> nil) and (FPropIndex <> 0) then
                with ActiveDSSObject[ActiveActor] do
                    if (FPropIndex <= ParentClass.NumProperties) and (FPropIndex > 0) then
                        DSSExecutive[ActiveActor].Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' +
                            ParentClass.PropertyName^[FPropIndex] + '=' +
                            String(arg);
            Result := Pansichar(Ansistring(''));
        end;
        4:
        begin                                           // DSSProperties.ActiveProperty - read
            Result := Pansichar(Ansistring(IntToStr(FPropIndex)));
        end;
        5:
        begin                                           // DSSProperties.ActiveProperty - write

            if not TryStrToInt(String(arg), TempPropIndex) then
                TempPropIndex := -1;

            FPropIndex := 0;
            if (ActiveCircuit[ActiveActor] <> nil) then
                with ActiveDSSObject[ActiveActor] do
                    if TempPropIndex > -1 then
                    begin
                        if (TempPropIndex <= ParentClass.NumProperties) and (TempPropIndex > 0) then
                            FPropIndex := TempPropIndex + 0;
                    end
                    else
                    begin
                        TempPropIndex := 0;
                        prop_name := String(arg);
                        with ParentClass do
                            for TempPropIndex := 1 to ParentClass.NumProperties do
                            begin
                                if CompareText(prop_name, PropertyName^[TempPropIndex]) = 0 then
                                begin
                                    FPropIndex := TempPropIndex + 0;
                                    Break;
                                end;
                            end;
                    end;
        end
    else
        Result := Pansichar(Ansistring(''));
    end;
end;

end.
