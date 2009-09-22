unit NamedObject;
{
  ----------------------------------------------------------
  Copyright (c) 2009, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

TYPE

  TNamedObject = class(TObject)
  private
    PName: String;  // path name, or class name for DSS
    LName: String;  // localName is unique within a class, like the old FName
    DName: String;  // for optional display, does not have to be unique
    pGuid: ^TGuid;

    function Get_DisplayName: String;
    procedure Set_DisplayName(const Value: String);
    function Get_GUID: TGuid;
    function Get_ID: String;
    procedure Set_GUID(const Value: TGUID);
  public
    constructor Create(ClassName:String);
    destructor Destroy; override;

    Property DSSClassName:String Read PName Write PName;
    Property LocalName:String Read LName Write LName;
    Property DisplayName:String Read Get_DisplayName Write Set_DisplayName;
    Property GUID:TGuid Read Get_GUID Write Set_GUID;
    Property ID:String read Get_ID;
  end;

implementation

Uses Sysutils;

constructor TNamedObject.Create(ClassName:String);
BEGIN
   Inherited Create;
   PName := ClassName;
   LName := '';
   DName := '';
   pGuid := nil;
END;

destructor TNamedObject.Destroy;
BEGIN
   if pGuid <> nil then Dispose (pGuid);
   Inherited Destroy;
END;


procedure TNamedObject.Set_DisplayName(const Value: String);
begin
  DName := Value;
end;

function TNamedObject.Get_DisplayName: String;
begin
  if DName = '' then
    Result := PName + '_' + LName
  else
    Result := DName;
end;

procedure TNamedObject.Set_GUID(const Value: TGUID);
begin
  if pGuid = nil then New (pGuid);
  pGuid^ := Value;
end;

function TNamedObject.Get_GUID: TGuid;
begin
  if pGuid = nil then begin
    New (pGuid);
    CreateGuid (pGuid^);
  end;
  Result := pGuid^;
end;

function TNamedObject.Get_ID: String;
begin
  Result := GUIDToString (Get_GUID);
end;

end.
