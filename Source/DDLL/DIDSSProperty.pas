unit DIDSSProperty;
// DSSProperties Interface
// This interface implements the DSSproperties (IDSSProperties) interface of OpenDSS by declaring 4 procedures for accessing the different properties included in this interface.

interface
function DSSProperties(mode:longint; arg:pAnsiChar):pAnsiChar; cdecl;

implementation

uses DSSClass, DSSGlobals, Executive, SysUtils;

var
  FPropIndex   :Integer;

function DSSProperties(mode:longint; arg:pAnsiChar):pAnsiChar; cdecl;
var TempPropIndex   :Integer;
    prop_name:String;
begin
  Result := pAnsiChar(AnsiString('')); // Default return value
  case mode of
    0: begin                                           // DSSProperties.Name
      if not TryStrToInt(string(arg), TempPropIndex) then
          TempPropIndex := -1;  // If this is the case, we are not getting an argument or it is not numeric. Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).

      if TempPropIndex > 0 then  // Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).
         FPropIndex := TempPropIndex + 0;

      If (ActiveCircuit[ActiveActor]<> Nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} Then
        With  ActiveDSSObject[ActiveActor].ParentClass   Do
        If (FPropIndex <= NumProperties) and (FPropIndex > 0) Then
          Result := pAnsiChar(AnsiString(PropertyName^[FPropIndex]));
    end;
    1: begin                                           // DSSProperties.Description
      if not TryStrToInt(string(arg), TempPropIndex) then
          TempPropIndex := -1;  // If this is the case, we are not getting an argument or it is not numeric. Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).

      if TempPropIndex > 0 then  // Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).
         FPropIndex := TempPropIndex + 0;

      If (ActiveCircuit[ActiveActor]<> Nil) and (FPropIndex <> 0) {and (FPropClass <> Nil)} Then
      With  ActiveDSSObject[ActiveActor].ParentClass Do
        If (FPropIndex <= NumProperties) and (FPropIndex > 0) Then
          Result := pAnsiChar(AnsiString(PropertyHelp^[FPropIndex]));
    end;
    2: begin                                           // DSSProperties.Val - read
      if not TryStrToInt(string(arg), TempPropIndex) then
          TempPropIndex := -1;  // If this is the case, we are not getting an argument or it is not numeric. Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).

      if TempPropIndex > 0 then  // Leaving this here temporarily for backwards compatibility. This must be decrecated soon (06/17/2025).
         FPropIndex := TempPropIndex + 0;

      If (ActiveCircuit[ActiveActor]<> Nil) and (FPropIndex <> 0)
      THEN  With ActiveDSSObject[ActiveActor] Do
        If (FPropIndex <= ParentClass.NumProperties) and (FPropIndex > 0) Then
              Result := pAnsiChar(AnsiString(PropertyValue[ParentClass.PropertyIdxMap^[FPropIndex]]));
    end;
    3: begin                                           // DSSProperties.Val - write
       If (ActiveCircuit[ActiveActor]<> Nil) and (FPropIndex <> 0)
        THEN  With ActiveDSSObject[ActiveActor] Do
          If (FPropIndex <= ParentClass.NumProperties) and (FPropIndex > 0) Then
                DSSExecutive[ActiveActor].Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' +
                       ParentClass.PropertyName^[FPropIndex] + '=' +
                       string(arg);
                Result:=pAnsiChar(AnsiString(''));
    end;
    4: begin                                           // DSSProperties.ActiveProperty - read
       Result:=pAnsiChar(AnsiString(IntToStr(FPropIndex)));
    end;
    5: begin                                           // DSSProperties.ActiveProperty - write

       if not TryStrToInt(string(arg), TempPropIndex) then
           TempPropIndex := -1;

       FPropIndex := 0;
       If (ActiveCircuit[ActiveActor]<> Nil)
        THEN  With ActiveDSSObject[ActiveActor] Do
         If TempPropIndex > -1 then
           begin
             If (TempPropIndex <= ParentClass.NumProperties ) and (TempPropIndex > 0) Then
                FPropIndex := TempPropIndex + 0;
           end
         else begin
            TempPropIndex := 0;
            prop_name := string(arg);
             With ParentClass Do
             For TempPropIndex := 1 to ParentClass.NumProperties Do Begin
                 If CompareText(prop_name, PropertyName^[TempPropIndex]) = 0 Then Begin
                     FPropIndex := TempPropIndex + 0;
                     Break;
                 End;
             End;
         end;
    end
    else
      Result:=pAnsiChar(ansistring(''));
  end;
end;

end.
