unit DSSObject;
 {
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

Uses Arraydef, DSSClass;

TYPE

  TDSSObject = class(TObject)
    private
      FName  :String;
      function  Get_PropertyValue(Index: Integer): String;
      procedure Set_PropertyValue(Index: Integer; const Value: String);
      procedure Set_Name(const Value: String);

    protected
      PropSeqCount   :Integer;
      FPropertyValue :pStringArray;

    public

      DSSObjType    :Integer; // PD, PC, Monitor, CondCode, etc.
      DSSClassName  :String;
      ParentClass   :TDSSClass;
      ClassIndex    :Integer;    // Index into the class collection list

      HasBeenSaved  :Boolean;
      Flag          :Boolean;  // General purpose Flag for each object  don't assume inited

      PrpSequence   :pIntegerArray;

      constructor Create(ParClass:TDSSClass);
      destructor Destroy; override;

      Function Edit:Integer;  // Allow Calls to edit from object itself

      {Get actual values of properties}
      FUNCTION  GetPropertyValue(Index:Integer):String; Virtual;  // Use dssclass.propertyindex to get index by name
      PROCEDURE InitPropertyValues(ArrayOffset:Integer); Virtual;
      PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Virtual;

      Procedure ClearPropSeqArray;

      Property Name:String Read FName Write Set_Name;
      Property PropertyValue[Index:Integer]:String Read Get_PropertyValue Write Set_PropertyValue;
  END;


implementation

Uses Sysutils, Utilities;

procedure TDSSObject.ClearPropSeqArray;
Var
   i:Integer;
begin
     PropSeqCount := 0;
     For i := 1 to ParentClass.NumProperties Do PrpSequence^[i] := 0;

end;

constructor TDSSObject.Create(ParClass:TDSSClass);
BEGIN
   Inherited Create;
   Fname := '';
   DSSObjType := 0;
   PropSeqCount := 0;
   ParentClass := ParClass;
   DSSClassName := ParentClass.Name;
   FPropertyValue := Allocmem(SizeOf(FPropertyValue^[1])*ParentClass.NumProperties);

   // init'd to zero when allocated
   PrpSequence := Allocmem(SizeOf(PrpSequence^[1])*ParentClass.NumProperties);

   HasBeenSaved := False;

END;

destructor TDSSObject.Destroy;

Var i:Integer;

BEGIN
   For i := 1 to ParentClass.NumProperties DO FPropertyValue^[i] := '';
   Reallocmem(FPropertyValue,0);
   Reallocmem(PrpSequence,0);
   Inherited Destroy;
END;


Procedure TDSSObject.DumpProperties(Var F:TextFile; Complete:Boolean);
BEGIN
    Writeln(F);
    Writeln(F,'New ', DSSClassName, '.', Name);
END;

function TDSSObject.Edit: Integer;
begin
     ParentClass.Active := ClassIndex;
     Result := ParentClass.Edit;
end;

function TDSSObject.GetPropertyValue(Index: Integer): String;
begin
     Result := FPropertyValue^[Index];  // Default Behavior   for all DSS Objects
end;

function TDSSObject.Get_PropertyValue(Index: Integer): String;
begin
    Result := GetPropertyValue(Index);  // This is virtual function that may call routine
end;

procedure TDSSObject.InitPropertyValues(ArrayOffset: Integer);
begin
     PropertyValue[ArrayOffset+1] := ''; //Like   Property

     // Clear propertySequence Array  after initialization
     ClearPropSeqArray;

end;

procedure TDSSObject.Set_Name(const Value: String);
begin
// If renamed, then let someone know so hash list can be updated;
  If Length(Fname)>0 Then ParentClass.ElementNamesOutOfSynch := True;
  FName := Value;
end;

procedure TDSSObject.Set_PropertyValue(Index: Integer;
  const Value: String);
begin
    FPropertyValue^[Index] := Value;

    // Keep track of the order in which this property was accessed for Save Command
    Inc(PropSeqCount);
    PrpSequence^[Index] := PropSeqCount;
end;

end.
