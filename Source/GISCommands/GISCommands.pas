unit GISCommands;

interface

uses
    DSSGlobals,
    Windows,
    SysUtils,
    System.Classes,
    ShellApi,
    djson,
//   TCP Indy libraries
    IdBaseComponent,
    IdComponent,
    IdTCPConnection,
    IdTCPClient,
    IdThreadComponent;

function start_openDSSGIS(): Boolean;
function show_busGIS(BusName: String): String;

const
    GUEST_PORT = 20011;

var
    GISTCPClient: TIdTCPClient;  // ... TIdThreadComponent
    GISThreadComponent: TIdThreadComponent;

implementation

{*******************************************************************************
*             Starts openDSS-GIS and gets connected as client                  *
*******************************************************************************}

function start_openDSSGIS(): Boolean;
begin
    Result := false;
    if DSS_GIS_Installed then
    begin
        ShellExecute(0, 'open', Pwidechar(DSS_GIS_path), nil, nil, SW_SHOWNORMAL);
        sleep(5000);
    // ... create TIdTCPClient
        GISTCPClient := TIdTCPClient.Create();
    // ... set properties
        GISTCPClient.Host := 'localhost';
        GISTCPClient.Port := GUEST_PORT;
        GISTCPClient.ReadTimeout := 1000;
        GISThreadComponent := TIdThreadComponent.Create();
        try
            GISTCPClient.Connect;
            IsGISON := true;
        except
            on E: Exception do
            begin
                IsGISON := false;
            end;
        end;
        Result := IsGISON;
    end;
end;

{*******************************************************************************
*                         Shows the given bus on the map                       *
*******************************************************************************}
function show_busGIS(BusName: String): String;
var
    TCPJSON: TdJSON;
    i: Integer;
    lat,
    long: Double;
    msg: String;
begin
    if IsGISON then
    begin
        SetActiveBus(BusName);
        if (ActiveCircuit[ActiveActor] <> nil) then
        begin
            with ActiveCircuit[ActiveActor] do
            begin
                if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                    begin
                        lat := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x;
                        long := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].y;
                    end;
            end;
            if (lat > 0) and (long > 0) then
            begin
                msg := '{"command":"showlocation","coords":{"longitude":' + floattostr(long) + ',"latitude":' + floattostr(lat) + '}}';
                GISTCPClient.IOHandler.WriteLn(msg);
                msg := GISTCPClient.IOHandler.ReadLn(#10, 200);
                TCPJSON := TdJSON.Parse(msg);
                Result := TCPJSON['showlocation'].AsString;
            end
            else
                Result := 'One or both of the GIS coordinates are incorrect';
        end;
    end;

end;

end.
