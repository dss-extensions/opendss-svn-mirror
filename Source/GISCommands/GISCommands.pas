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
    Line,
    Utilities,
    ArrayDef,
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
function ReSizeWindow(): String;
function GISDrawCircuit(): String;
function show_lineGIS(LineName: String): String;
function export_mapGIS(): String;
function find_treesGIS(LineName: String): String;
procedure get_line_Coords(LineName: String);
function set_map_View(myView: String): String;
function clear_map(): String;
function Draw_line_GIS(): String;
function Zoom_area_GIS(): String;
function GISPlotfile(myPath: String): String;

var
    GISTCPClient: TIdTCPClient;  // ... TIdThreadComponent
    GISThreadComponent: TIdThreadComponent;
    myCoords: array of Double;

implementation

{*******************************************************************************
*             Starts openDSS-GIS and gets connected as client                  *
*******************************************************************************}

function start_openDSSGIS(): Boolean;
begin
    Result := false;

    if DSS_GIS_Installed then
    begin
        if not processExists('OpenDSSGIS.exe') then
        begin
     // Starts OpenDSS-GIS if is not running
            ShellExecute(0, 'open', Pwidechar(DSS_GIS_path), nil, nil, SW_SHOWNORMAL);
            sleep(5000);
            IsGISON := false;
        end;
        if not IsGISON then
        begin
      // ... create TIdTCPClient
            GISTCPClient := TIdTCPClient.Create();
      // ... set properties
            GISTCPClient.Host := 'localhost';
            GISTCPClient.Port := DSSGISPort;
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
        end
        else
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
    if IsGISON then
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
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'

end;

{*******************************************************************************
*            Distributes the windows leaving OpenDSS to the right              *
*******************************************************************************}

function WindowRL(): String;
var
    TCPJSON: TdJSON;
    ScrSize: Integer;
    InMsg,
    TempStr,
    JSONCmd: String;
begin
    if IsGISON then
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
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'
end;

{*******************************************************************************
*    Resizes the OpenDSS-GIS window using the coordinates given by the user    *
*******************************************************************************}

function ReSizeWindow(): String;
var
    TCPJSON: TdJSON;
    j,
    ScrSize: Integer;
    InMsg,
    TempStr,
    JSONCmd: String;
    TStrArr: array of String;

begin
    if IsGISON then
    begin
        setlength(TStrArr, 4);
        TStrArr[0] := ',"top":';
        TStrArr[1] := ',"right":';
        TStrArr[2] := ',"bottom":';
        TStrArr[3] := '}}';

        JSONCmd := '{"command":"resizewindow","coords":{"left":';
        for j := 0 to High(TStrArr) do
        begin
            Parser[ActiveActor].NextParam;
            JSONCmd := JSONCmd + Parser[ActiveActor].StrValue + TStrArr[j];
        end;
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
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized'

end;

{*******************************************************************************
*      Generates the file required by DSS-GIS to draw the model on the map     *
*******************************************************************************}

function GISDrawCircuit(): String;
var
    LineElem: TLineObj;
    TxtRow,
    myBus: String;
    k: Integer;
    F: TextFile;
    InMsg,
    TempStr,
    JSONCmd: String;
    TCPJSON: TdJSON;
    Add2file: Boolean;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        if IsGISON then
        begin
            with ActiveCircuit[ActiveActor] do
            begin
                if Lines.ListSize > 0 then
                begin
                    Assignfile(F, 'GIS_desc.csv');
                    ReWrite(F);
                    LineElem := Lines.First;
                    while LineElem <> nil do
                    begin
                        TxtRow := '';
                        Add2File := true;
                        for k := 1 to 2 do
                        begin
                            myBus := StripExtension(LineElem.GetBus(k));
                            DSSGlobals.SetActiveBus(myBus);
                            if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoordDefined) then
                            begin
                                TxtRow := TxtRow + floattostr(Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Long) +
                                    ',' + floattostr(Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Lat) + ',';
                            end;
                            Add2File := Add2File and (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Long <> 0) and (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Lat <> 0);
                        end;
                        if Add2File then
                            Writeln(F, TxtRow);
                        LineElem := Lines.Next;

                    end;
                    CloseFile(F);
                    JSONCmd := '{"command":"plotcircuit","path":"' +
                        OutputDirectory[ActiveActor] + 'GIS_desc.csv","color":"' + GISColor +
                        '","thickness":' + GISThickness + '}';
            // Sends the command to OpenDSS-GIS
                    try
                        GISTCPClient.IOHandler.WriteLn(JSONCmd);
                        InMsg := GISTCPClient.IOHandler.ReadLn(#10, 5000);
                        TCPJSON := TdJSON.Parse(InMsg);
                        TempStr := TCPJSON['plotcircuit'].AsString;
                        Result := TempStr;
                    except
                        on E: Exception do
                        begin
                            IsGISON := false;
                            Result := 'Error while communicating to OpenDSS-GIS';
                        end;
                    end;

                end;
            end;
        end
        else
            result := 'OpenDSS-GIS is not installed or initialized'
    end;
end;

{*******************************************************************************
*                         Shows the given line on the map                       *
*******************************************************************************}
function show_lineGIS(LineName: String): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
  // First have to find the line

        if (ActiveCircuit[ActiveActor] <> nil) then
        begin
            get_line_Coords(LineName);

            InMsg := '{"command":"showline","coords":{"long1":' + floattostr(myCoords[0]) + ',"lat1":' + floattostr(myCoords[1]) +
                ',"long2":' + floattostr(myCoords[2]) + ',"lat2":' + floattostr(myCoords[3]) + '}}';
            try
                GISTCPClient.IOHandler.WriteLn(InMsg);
                InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
                TCPJSON := TdJSON.Parse(InMsg);
                Result := TCPJSON['showline'].AsString;
            except
                on E: Exception do
                begin
                    IsGISON := false;
                    Result := 'Error while communicating to OpenDSS-GIS';
                end;
            end;

        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';

end;

{*******************************************************************************
*             Exports the current map view into the models folder              *
*******************************************************************************}
function export_mapGIS(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"exportmap", "path":"' + OutputDirectory[ActiveActor] + '"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['exportmap'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := false;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;

    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*             Commands OpenDSS-GIS to verify if there are trees                *
*                     intersecting with the given line                         *
*******************************************************************************}
function find_treesGIS(LineName: String): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
  // to be implemented

        if (ActiveCircuit[ActiveActor] <> nil) then
        begin
            get_line_Coords(LineName);

            InMsg := '{"command":"findtrees","coords":{"long1":' + floattostr(myCoords[0]) + ',"lat1":' + floattostr(myCoords[1]) +
                ',"long2":' + floattostr(myCoords[2]) + ',"lat2":' + floattostr(myCoords[3]) + '}}';

            try
                GISTCPClient.IOHandler.WriteLn(InMsg);
                InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
                TCPJSON := TdJSON.Parse(InMsg);
                Result := TCPJSON['findtrees'].AsString;
            except
                on E: Exception do
                begin
                    IsGISON := false;
                    Result := 'Error while communicating to OpenDSS-GIS';
                end;
            end;
        end;
        result := 'No';
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;
{*******************************************************************************
*             Commands OpenDSS-GIS to update the map view to the               *
*                             one given by the user                            *
*******************************************************************************}
function set_map_View(myView: String): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"mapview","mymap":"' + myView + '"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['mapview'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := false;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*      Commands OpenDSS-GIS to remove all previous lines/draws from the map    *
*******************************************************************************}
function clear_map(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"clearmap"}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['clearmap'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := false;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*                 Draws a line in the map at the given coordinates             *
*******************************************************************************}
function Draw_line_GIS(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;

begin
    if IsGISON then
    begin

        InMsg := '{"command":"drawline","coords":{"long1":' + floattostr(GISCoords^[1]) + ',"lat1":' + floattostr(GISCoords^[2]) +
            ',"long2":' + floattostr(GISCoords^[3]) + ',"lat2":' + floattostr(GISCoords^[4]) + '},"color":"' + GISColor +
            '","thickness":' + GISThickness + '}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['drawline'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := false;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*          Zooms the map at the area described by the given coordinates        *
*******************************************************************************}
function Zoom_area_GIS(): String;
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    if IsGISON then
    begin
        InMsg := '{"command":"zoommap","coords":{"long1":' + floattostr(GISCoords^[1]) + ',"lat1":' + floattostr(GISCoords^[2]) +
            ',"long2":' + floattostr(GISCoords^[3]) + ',"lat2":' + floattostr(GISCoords^[4]) + '}}';
        try
            GISTCPClient.IOHandler.WriteLn(InMsg);
            InMsg := GISTCPClient.IOHandler.ReadLn(#10, 200);
            TCPJSON := TdJSON.Parse(InMsg);
            Result := TCPJSON['zoommap'].AsString;
        except
            on E: Exception do
            begin
                IsGISON := false;
                Result := 'Error while communicating to OpenDSS-GIS';
            end;
        end;
    end
    else
        result := 'OpenDSS-GIS is not installed or initialized';
end;

{*******************************************************************************
*       Commands OpenDSS-GIS to draw the content of a file over the map        *
*******************************************************************************}
function GISPlotfile(myPath: String): String;
var
    TxtRow,
    myBus: String;
    k: Integer;
    F: TextFile;
    InMsg,
    TempStr,
    JSONCmd: String;
    TCPJSON: TdJSON;

begin
    if ActiveCircuit[ActiveActor] <> nil then
    begin
        if IsGISON then
        begin
            JSONCmd := '{"command":"plotfromfile","path":"' +
                myPath + '"}';
      // Sends the command to OpenDSS-GIS
            try
                GISTCPClient.IOHandler.WriteLn(JSONCmd);
                InMsg := GISTCPClient.IOHandler.ReadLn(#10, 5000);
                TCPJSON := TdJSON.Parse(InMsg);
                TempStr := TCPJSON['plotfromfile'].AsString;
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
end;

{*******************************************************************************
*             Loads the line Long-lat into the global array "myCoords"         *
*******************************************************************************}
procedure get_line_Coords(LineName: String);
var
    TCPJSON: TdJSON;
    activesave,
    i: Integer;
    myBuses: array of String;
    S,
    InMsg: String;
    Found: Boolean;
    pLine: TLineObj;
begin
    setlength(myCoords, 4);
    setlength(myBuses, 2);

    S := LineName;  // Convert to Pascal String
    Found := false;

    with ActiveCircuit[ActiveActor].Lines do
    begin
        ActiveSave := ActiveIndex;
        pLine := First;
        while pLine <> nil do
        begin
            if (CompareText(pLine.Name, S) = 0) then
            begin
                ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
                Found := true;
                Break;
            end;
            pLine := Next;
        end;
    end;
  // Get the names of the buses for the line
    with ActiveCircuit[ActiveActor] do
    begin
        for i := 1 to 2 do
        begin
            myBuses[i - 1] := StripExtension(pLine.GetBus(i));
        end;

  // Get the coords of the buses
        for i := 0 to 1 do
        begin
            SetActiveBus(myBuses[i]);
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoorddefined) then
                begin
                    myCoords[i * 2] := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
                    myCoords[i * 2 + 1] := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
                end;
            end;
        end;
    end;

end;

end.
