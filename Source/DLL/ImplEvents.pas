unit ImplEvents;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, AxCtrls, Classes,
OpenDSSengine_TLB, StdVcl;

type
  TDSSEvents = class(TAutoObject, IConnectionPointContainer, IDSSEvents)
  private
    { Private declarations }
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint: TConnectionPoint;
    // FEvents: IDSSEventsEvents;
    { note: FEvents maintains a *single* event sink. For access to more
      than one event sink, use FConnectionPoint.SinkList, and iterate
      through the list of sinks. }
  public
    procedure Initialize; override;
    procedure Fire_InitControls;
    procedure Fire_StepControls;
  protected
    { Protected declarations }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;

  end;

implementation

uses ComServ;

procedure TDSSEvents.EventSinkChanged(const EventSink: IUnknown);
var
  evt: IDSSEventsEvents;
begin
  evt := EventSink as IDSSEventsEvents;
  FConnectionPoint.SinkList.Add(@evt);
end;

procedure TDSSEvents.Initialize;
begin
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoint := FConnectionPoints.CreateConnectionPoint(
      AutoFactory.EventIID, ckSingle, EventConnect)
  else FConnectionPoint := nil;
end;

procedure TDSSEvents.Fire_InitControls;
var
  I: Integer;
  EventSinkList: TList;
  EventSink: IDSSEventsEvents;
begin
  if FConnectionPoint <> nil then
  begin
    EventSinkList :=FConnectionPoint.SinkList; {get the list of client sinks }
    for I := 0 to EventSinkList.Count - 1 do
    begin
      EventSink := IUnknown(EventSinkList[I]) as IDSSEventsEvents;
      EventSink.InitControls;
    end;
  end;
end;

procedure TDSSEvents.Fire_StepControls;
var
  I: Integer;
  EventSinkList: TList;
  EventSink: IDSSEventsEvents;
begin
  if FConnectionPoint <> nil then
  begin
    EventSinkList :=FConnectionPoint.SinkList; {get the list of client sinks }
    for I := 0 to EventSinkList.Count - 1 do
    begin
      EventSink := IUnknown(EventSinkList[I]) as IDSSEventsEvents;
      EventSink.StepControls;
    end;
  end;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TDSSEvents, Class_DSSEvents,
    ciInternal, tmApartment);
end.
