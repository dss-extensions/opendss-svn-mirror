unit ImplStorages;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TStorages = class(TAutoObject, IStorages)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Storage,
    Variants,
    Sysutils;

function TStorages.Get_AllNames: Olevariant;
var
    StorageElem: TStorageObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> nil then
        with ActiveCircuit[ActiveActor] do
            if StorageElements.ListSize > 0 then
            begin
                VarArrayRedim(result, StorageElements.ListSize - 1);
                k := 0;
                StorageElem := StorageElements.First;
                while StorageElem <> nil do
                begin
                    Result[k] := StorageElem.Name;
                    Inc(k);
                    StorageElem := StorageElements.Next;
                end;
            end;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TStorages, Class_Storages,
        ciInternal, tmApartment);
end.
