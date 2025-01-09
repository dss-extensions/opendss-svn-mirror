unit PipeServerInstance;

interface

uses
    Classes,
    Windows,
    System.Generics.Collections;

const
    SHUT_DOWN_MSG = 'shutdown pipe ';
    PIPE_FORMAT = '\\%s\pipe\%s'; // \\ServerName\pipe\PipeName
    PIPE_TIMEOUT = 5000;
    BUFF_SIZE = 8095;

type
    TInstanceState = (isRun, isTerminate);

    RPIPEMessage = record
        Size: DWORD;
        Kind: Byte;
        Count: DWORD;
        Data: array[0..BUFF_SIZE + 5] of Byte;
    end;

// Named pipe server (instance)
    TPipeServerInstance = class(TThread)
    PRIVATE
        fID: Byte;
        fHandle: THandle;
        fPipeName: String;
        fRetQ: TThreadedQueue<Integer>;
        procedure SendMessage(Msg: String);
        function ReadMessage(): String;
        procedure Log(aLog: String);
    PROTECTED
        procedure Execute(); OVERRIDE;
    PUBLIC
        InstanceState: TInstanceState;
        LastCMD: String;
        constructor Create(aID: Byte; aPipeHandle: THandle; aPipeName: String; RetQ: TThreadedQueue<Integer>);
        destructor Destroy; OVERRIDE;

        procedure ShutDownServer();
    end;

implementation

uses
    SysUtils,
    Utilities,
    DSSGlobals;


{ TPipeServerInstance }
constructor TPipeServerInstance.Create(aID: Byte; aPipeHandle: THandle; aPipeName: String; RetQ: TThreadedQueue<Integer>);
begin
    inherited Create();
    FreeOnTerminate := false;
    Priority := tpLower;
    fID := aID;
    fHandle := aPipeHandle;
    fPipeName := aPipeName;
    InstanceState := isRun;
    fRetQ := RetQ;
  //Log('Constructor - Pipe name: ' + fPipeName);
end;


destructor TPipeServerInstance.Destroy;
begin
    if (fHandle <> INVALID_HANDLE_VALUE) then
        ShutDownServer;
  //Log('Destructor');
    inherited Destroy;
end;


procedure TPipeServerInstance.Log(aLog: String);
begin
  //myID := fID;
    Writeln('    TPipeServerInstance[' + IntToStr(0) + ']: ' + aLog);
end;


procedure TPipeServerInstance.Execute();
var
    NewMessage: Boolean;
    Written: Cardinal;
    InMsg, OutMsg: RPIPEMessage;
    ClientCmd: String;

begin

  //Log('Execute - start - server');
    while (InstanceState <> isTerminate) and (fHandle <> INVALID_HANDLE_VALUE) do
    begin

        ClientCmd := ReadMessage();

        if ClientCmd = 'closepipe' then
        begin
    // This means that the pyscript is done and we need to close the handler
            if LowerCase(LastCMD) = 'yes' then
                fRetQ.PushItem(1)
            else
                fRetQ.PushItem(0);
            break;
        end
        else
        begin
      // The py script is sending commands or something different
            LastCMD := ClientCmd;
            if (LowerCase(LastCMD) <> 'yes') and (LowerCase(LastCMD) <> 'no') then
            begin
                DSSExecutive[ActiveActor].Command := ClientCmd;
        //Log('Sending message');
                if GlobalResult = '' then
                    GlobalResult := 'OK';

                SendMessage(GlobalResult);
            end
            else
                SendMessage('OK');

            if (InstanceState = isTerminate) then // Emergency procedure to terminate
                break;
        end;
    end;
    InstanceState := isTerminate;
  //Log('Execute - end');
end;


procedure TPipeServerInstance.SendMessage(Msg: String);
var
    SendMessage: Boolean;
    Bytes: Cardinal;
    pMsg: Pchar;
    buf: array [0 .. BUFF_SIZE - 1] of Char;
begin
  // Prepare outgoing message
    pMsg := Pchar(Msg);
    fillchar(buf, BUFF_SIZE, #0);
    move(pMsg[0], buf[0], Length(pMsg) * Sizeof(Char));
  // Send message
    SendMessage := WriteFile(
        fHandle,   // pipe handle
        buf,       // message
        length(Msg) * Sizeof(Char),  // message length
        Bytes,     // bytes written
        nil
        );

{  if not SendMessage then
    Log('Message cound not be send'); }
end;

function TPipeServerInstance.ReadMessage(): String;
var
    MessageReceived: Boolean;
    MyMsg: array[0..BUFF_SIZE - 1] of Char;
    MsgSz: DWord;
    MsgStr: String;
    i: Integer;
    idx: Integer;

begin
    MsgStr := '';
    REsult := '';
    FillChar(MyMsg, BUFF_SIZE, #0);
    MessageReceived := ReadFile(
        fHandle,   // pipe handle
        MyMsg,       // buffer to receive reply
        BUFF_SIZE, // size of buffer
        MsgSz,  // number of bytes read
        nil);      // not overlapped
    if MessageReceived then
    begin
        SetString(MsgStr, Pchar(@MyMsg[1]), MsgSz);

    // Remove the null chars (if any)
        idx := 1;
        while idx <= Length(MsgStr) do
            if MsgStr[idx] = #0 then
                Delete(MsgStr, idx, 1)
            else
                Inc(idx);

    //Log('Received message: size = ' + IntToStr(MsgSz) + '; data = ' +  MsgStr);
        if (MsgStr = 'exit') then
        begin
      //Log('Is terminate');
            InstanceState := isTerminate;
        end
    end;

    Result := MsgStr;
end;


procedure TPipeServerInstance.ShutDownServer();
var
    BytesRead: Cardinal;
    OutMsg, InMsg: RPIPEMessage;
    ShutDownMsg: String;
begin
    if (fHandle <> INVALID_HANDLE_VALUE) then
    begin
    // Server still has pipe opened
        DisconnectNamedPipe(fHandle);
    // Close pipe on server
        CloseHandle(fHandle);
    // Clear handle
        fHandle := INVALID_HANDLE_VALUE;
    //Log('Shut down server: ' + fPipeName);
    end;
end;


end.
