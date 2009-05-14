unit ImplCtrlQueue;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, OpenDSSengine_TLB, StdVcl;

type
  TCtrlQueue = class(TAutoObject, ICtrlQueue)
  private
  protected
    function ClearQueue: HResult; stdcall;
    function Delete(ActionHandle: Integer): HResult; stdcall;
    function Get_ActionCode(out Value: Integer): HResult; stdcall;
    function Get_DeviceHandle(out Value: Integer): HResult; stdcall;
    function Get_NumActions(out Value: Integer): HResult; stdcall;
    function Push(Hour: Integer; Seconds: Double; ActionCode,
      DeviceHandle: Integer): HResult; stdcall;
    function Show: HResult; stdcall;  
    function ClearActions: HResult; stdcall;
    function Get_PopAction(out Value: Integer): HResult; stdcall;
    function Set_Action(Value: Integer): HResult; stdcall;
    {Declare ICtrlQueue methods here}
  end;




implementation

uses ComServ, DSSGlobals, ControlQueue, ControlElem, DSSClass;

{Define class for proxy control object}

Type
  pAction = ^Taction;
  TAction = Record
       ActionCode :Integer;
       DeviceHandle :Integer;
  End;

  TCOMControlProxyObj = class(TControlElem)
     private
       ActionList :TList;
       Procedure ClearActionList;
       Function PopAction: Boolean;
     public

       constructor Create(ParClass:TDSSClass; const COMProxyName:String);
       destructor Destroy; override;

       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state
  end;

Var
    COMControlProxyObj :TCOMControlProxyObj;
    ActiveAction       :pAction;



function TCtrlQueue.Delete(ActionHandle: Integer): HResult;
begin
   Result := 0;
    If ActiveCircuit <> Nil then Begin
      ActiveCircuit.ControlQueue.Delete(ActionHandle);
   End;
end;


function TCtrlQueue.Get_ActionCode(out Value: Integer): HResult;
begin
   Result := 0;
    If ActiveAction<> NIl then   Value := ActiveAction^.ActionCode ;
end;

function TCtrlQueue.Get_DeviceHandle(out Value: Integer): HResult;
begin
   Result := 0;
    If ActiveAction<> NIl then   Value := ActiveAction^.DeviceHandle;
end;

function TCtrlQueue.Get_NumActions(out Value: Integer): HResult;
begin
   Result := 0;
     Value := COMControlProxyObj.ActionList.Count;
end;



function TCtrlQueue.Push(Hour: Integer; Seconds: Double; ActionCode,
  DeviceHandle: Integer): HResult;
{Return a Handle to the control action on the main control queue}
begin
   Result := 0;
   If ActiveCircuit <> Nil then Begin
      Result := ActiveCircuit.ControlQueue.push(Hour, Seconds, ActionCode, DeviceHandle, COMControlProxyObj);
   End;
end;

function TCtrlQueue.Show: HResult;
begin
     If ActiveCircuit <> Nil then
        ActiveCircuit.ControlQueue.ShowQueue(DSSDirectory + 'COMProxy_ControlQueue.CSV');
     Result := 0;
end;

{ TCOMControlProxyObj }

procedure TCOMControlProxyObj.ClearActionList;
begin
   while PopAction do   ;  // spin until it is done
end;

constructor TCOMControlProxyObj.Create(ParClass: TDSSClass;
  const COMProxyName: String);
begin
    Name := COMProxyName;
    ActionList := TList.Create;
end;

destructor TCOMControlProxyObj.Destroy;
begin
  ClearActionList;
  ActionList.Free;
  inherited;
end;

procedure TCOMControlProxyObj.DoPendingAction(const Code, ProxyHdl: Integer);
Var
   Action :pAction;
begin
     Action := Allocmem(SizeOf(TAction));
     With Action^ Do Begin         // Capture the Action
          ActionCode := Code;
          DeviceHandle := ProxyHdl;
     End;
     ActionList.Add(Action);
end;

function TCOMControlProxyObj.PopAction: Boolean;
begin
    If ActiveAction <> Nil then  Begin
      Freemem(ActiveAction, Sizeof(TAction));
      ActiveAction := Nil;
    End;
    Result := TRUE;
    If ActionList.Count>0 then Begin
       ActiveAction := ActionList.Items[0];
       ActionList.Delete(0);
    End Else Result := FALSE;
end;

procedure TCOMControlProxyObj.Reset;
begin
  ClearActionList;

end;


function TCtrlQueue.ClearActions: HResult;
begin
      Result := 0;
      COMControlProxyObj.ClearActionList;
end;

function TCtrlQueue.ClearQueue: HResult;
begin
   Result := 0;
   If ActiveCircuit <> Nil then Begin
      ActiveCircuit.ControlQueue.Clear;
   End;
end;


function TCtrlQueue.Get_PopAction(out Value: Integer): HResult;
begin
     Result := 0;
     COMControlProxyObj.PopAction;
     Value := COMControlProxyObj.ActionList.Count;
end;

function TCtrlQueue.Set_Action(Value: Integer): HResult;
begin
    Result := 0;
    With COMControlProxyObj Do
     If Value < ActionList.Count then Begin
       ActiveAction := ActionList.Items[Value-1];
     End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TCtrlQueue, Class_CtrlQueue,
    ciInternal, tmApartment);
 {Make a Proxy Control Object to receiving control actions}
    COMControlProxyObj := TCOMControlProxyObj.Create(Nil, 'COM_Proxy');
    ActiveAction := Nil;
end.
