unit ImplTopology;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TTopology = class(TAutoObject, ITopology)
  protected
    function Get_AllIsolatedElements: OleVariant; safecall;
    function Get_AllLoopElements: OleVariant; safecall;
    function Get_NumIsolated: Integer; safecall;
    function Get_NumLoops: Integer; safecall;
    function Get_Active: ICktElement; safecall;
    function Get_Backward: ICktElement; safecall;
    function Get_ElementName: WideString; safecall;
    function Get_Forward: ICktElement; safecall;
    function Get_Next: ICktElement; safecall;
    procedure Set_ElementName(const Value: WideString); safecall;

  end;

implementation

uses ComServ;

function TTopology.Get_AllIsolatedElements: OleVariant;
begin

end;

function TTopology.Get_AllLoopElements: OleVariant;
begin

end;

function TTopology.Get_NumIsolated: Integer;
begin

end;

function TTopology.Get_NumLoops: Integer;
begin

end;

function TTopology.Get_Active: ICktElement;
begin

end;

function TTopology.Get_Backward: ICktElement;
begin

end;

function TTopology.Get_ElementName: WideString;
begin

end;

function TTopology.Get_Forward: ICktElement;
begin

end;

function TTopology.Get_Next: ICktElement;
begin

end;

procedure TTopology.Set_ElementName(const Value: WideString);
begin

end;

initialization
  TAutoObjectFactory.Create(ComServer, TTopology, Class_Topology,
    ciInternal, tmApartment);
end.
