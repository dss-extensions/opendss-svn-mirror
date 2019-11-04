unit GISCommands;

interface

uses
    Tlhelp32,
    DSSGlobals,
    Windows,
    SysUtils,
    System.Classes,
    ShellApi,
    djson,
    VCl.forms,
//   TCP Indy libraries
    IdBaseComponent,
    IdComponent,
    IdTCPConnection,
    IdTCPClient,
    IdThreadComponent,
    TCP_IP;

function start_openDSSGIS(): Boolean;
function show_busGIS(BusName: String): String;
function Get_routeGIS(): String;
function Get_edgesGIS(): String;
function Get_distanceGIS(): String;
function Show_routeGIS(): String;
function Get_JSONrouteGIS(): String;
function WindowLR(): String;
function WindowRL(): String;

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
    if not IsGISON then
    begin
        if DSS_GIS_Installed then
        begin
            if not processExists('OpenDSSGIS.exe') then
            begin
        // Starts OpenDSS-GIS if is not running
                ShellExecute(0, 'open', Pwidechar(DSS_GIS_path), nil, nil, SW_SHOWNORMAL);
                sleep(5000);
            end;
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
    end
    else
        Result := IsGISON;
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
    InMsg: String;
begin
    if IsGISON then
    begin
        SetActiveBus(BusName);
        if (ActiveCircuit[ActiveActor] <> nil) then
        begin
            with ActiveCircuit[ActiveActor] do
            begin
                if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoorddefined) then
                    begin
                        lat := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
                        long := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
                        InMsg := '{"command":"showlocation","coords":{"longitude":' + floattostr(long) + ',"latitude":' + floattostr(lat) + '}}';
                        try
                            GISTCPClient.IOHandler.WriteLn(InMsg);
                            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
                            TCPJSON := TdJSON.Parse(InMsg);
                            Result := TCPJSON['showlocation'].AsString;
                        except
                            on E: Exception do
                            begin
                                IsGISON := false;
                                Result := 'Error while communicating to OpenDSS-GIS';
                            end;
                        end;
                    end
                    else
                        Result := 'One or both of the GIS coordinates are incorrect or not defined';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';

end;

{*******************************************************************************
*                 Request to calculate a route between 2 buses                 *
*******************************************************************************}

function Get_routeGIS(): String;
var
    TCPJSON: TdJSON;
    JSONCmd,
    InMsg,
    busName: String;
    TryCom,
    error: Boolean;
    i: Integer;
    lat,
    long: Double;
begin
    if IsGISON then
    begin
        error := false;
        JSONCmd := '{"command":"route","coords":[';
        for i := 1 to 2 do                                                  // to extract both buses
        begin
            Parser[ActiveActor].NextParam;
            busName := Parser[ActiveActor].StrValue;
            SetActiveBus(busName);
            if (ActiveCircuit[ActiveActor] <> nil) and not error then         // is everything fine?
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoorddefined) then
                        begin
                            lat := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
                            long := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
                            JSONCmd := JSONCmd + '{"longitude":' + floattostr(long) + ',"latitude":' + floattostr(lat) + '},';
                        end
                        else
                            error := true;
                end;
            end;
        end;
        if not error then                                                 // No error so far
        begin
            JSONCmd := JSONCmd.Substring(0, length(JSONCmd) - 1) + ']}';
            TryCom := true;
            i := 0;
            while TryCom do
            begin
                try
                    GISTCPClient.IOHandler.WriteLn(JSONCmd);
                    InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
                    TCPJSON := TdJSON.Parse(InMsg);
                    InMsg := TCPJSON['route'].AsString;
                    if InMsg = 'done' then                                      // Route calculated successfully
                        Trycom := false
                    else
                    begin
            // If the route wasn't calculated because the server was busy, it tries up to 5 times
            // with 300 ms interval, if after that the server is still busy, return error message
                        sleep(300);
                        inc(i);
                        if i > 5 then
                            Trycom := false;
                    end;
                except
                    on E: Exception do
                    begin
                        IsGISON := false;
                        Trycom := false;
                        Result := 'Error while communicating to OpenDSS-GIS';
                    end;
                end;
            end;
            Result := InMsg;
        end
        else
            Result := 'One or more buses have no GIS coordinates';
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'
end;

{*******************************************************************************
*  Request to coordiantes of the edges that define the last route calculated   *
*******************************************************************************}
function Get_edgesGIS(): String;
var
    Coords,
    TCPJSON: TdJSON;
    JSONCmd,
    TempStr,
    InMsg: String;

begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"jsonroute"}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
            TCPJSON := TdJSON.Parse(InMsg);
            TempStr := '[';
            for Coords in TCPJSON['jsonroute'] do
            begin
                TempStr := TempStr + Coords['latitude'].AsString + ',' + Coords['longitude'].AsString + ',';
            end;
            Result := TempStr.substring(0, (length(TempStr) - 1)) + ']';
        except
            on E: Exception do
            begin
                IsGISON := false;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'
end;

{*******************************************************************************
*                Gets the distance of the last route calculated                *
*******************************************************************************}
function Get_distanceGIS(): String;
var
    TCPJSON: TdJSON;
    JSONCmd,
    TempStr,
    InMsg: String;

begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"distance"}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
            TCPJSON := TdJSON.Parse(InMsg);
            TempStr := TCPJSON['distance'].AsString + ' ' + TCPJSON['units'].AsString;
            Result := TempStr;
        except
            on E: Exception do
            begin
                IsGISON := false;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'
end;

{*******************************************************************************
*                 Shows on the map the last route calculated                   *
*******************************************************************************}

function Show_routeGIS(): String;
var
    TCPJSON: TdJSON;
    JSONCmd,
    TempStr,
    InMsg: String;

begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"showroute"}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
            TCPJSON := TdJSON.Parse(InMsg);
            TempStr := TCPJSON['showroute'].AsString;
            Result := TempStr;
        except
            on E: Exception do
            begin
                IsGISON := false;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'

end;

{*******************************************************************************
*       Exports to a file the last route calculated in JSON format             *
*******************************************************************************}

function Get_JSONrouteGIS(): String;
var
    F: TextFile;
    JSONCmd,
    FileName,
    InMsg: String;
begin
    if IsGISON then
    begin
        JSONCmd := '{"command":"jsonscript"}';
        try
            GISTCPClient.IOHandler.WriteLn(JSONCmd);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 20000);

            FileName := GetOutputDirectory + CircuitName_[ActiveActor] + 'JSONScript_route.txt';  // Explicitly define directory

            Assignfile(F, FileName);
            ReWrite(F);
            Write(F, inMsg);
            CloseFile(F);

            Result := FileName;
        except
            on E: Exception do
            begin
                IsGISON := false;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'

end;

{*******************************************************************************
*            Distributes the windows leaving OpenDSS on the left               *
*******************************************************************************}

function WindowLR(): String;
var
    TCPJSON: TdJSON;
    ScrSize: Integer;
    InMsg,
    TempStr,
    JSONCmd: String;
begin

    JSONCmd := '{"command":"resizewindow","coords":{"left":' + inttostr(Screen.Width div 2) +
        ',"top":0,"right":' + inttostr(Screen.Width) + ',"bottom":' + inttostr(Screen.Height - 40) + '}}';
    try
        GISTCPClient.IOHandler.WriteLn(JSONCmd);
        InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
        TCPJSON := TdJSON.Parse(InMsg);
        TempStr := TCPJSON['resizewindow'].AsString;
        Result := TempStr;
    except
        on E: Exception do
        begin
            IsGISON := false;
            Result := 'Error while communicating to OpenDSS-GIS';
        end;
    end;

end;

{*******************************************************************************
*            Distributes the windows leaving OpenDSS on the right              *
*******************************************************************************}

function WindowRL(): String;
var
    TCPJSON: TdJSON;
    ScrSize: Integer;
    InMsg,
    TempStr,
    JSONCmd: String;
begin
    JSONCmd := '{"command":"resizewindow","coords":{"left":0,"top":0,"right":' +
        inttostr(Screen.Width div 2) + ',"bottom":' + inttostr(Screen.Height - 40) + '}}';
    try
        GISTCPClient.IOHandler.WriteLn(JSONCmd);
        InMsg := GISTCPClient.IOHandler.ReadLn(#10, 2000);
        TCPJSON := TdJSON.Parse(InMsg);
        TempStr := TCPJSON['resizewindow'].AsString;
        Result := TempStr;
    except
        on E: Exception do
        begin
            IsGISON := false;
            Result := 'Error while communicating to OpenDSS-GIS';
        end;
    end;
end;

end.
