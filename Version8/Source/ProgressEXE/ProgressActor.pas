unit ProgressActor;

interface

uses
  djson,
  StrUtils,
  math,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  System.Types,
  System.UITypes,
  IdTCPServer,
  IdBaseComponent,
  IdComponent,
  IdContext,
  IdGlobal,
  IdCustomTCPServer;

const
  SHUT_DOWN_MSG = 'shutdown pipe ';
  PIPE_FORMAT = '\\%s\pipe\%s'; // \\ServerName\pipe\PipeName
  PIPE_TIMEOUT = 5000;
  BUFF_SIZE = 8095;
  SERVER_PATH = ''; // Empty for local server
  SERVER_NAME = 'DSSProg'; // Server name

type

  TMyClient = class(TThread)
  private
    fID: Byte;
    fPipeName: String;
    First_run: Boolean;
    function CreateServer():Boolean;
    procedure SendMessage(Msg: String);
    Function ReadMessage(): String;

  protected
    procedure Execute(); override;
  public
    pHandle     : THandle;
    constructor Create(aID: Byte; aServer, aPipe: String);
    destructor Destroy(); override;

  end;

  TForm1 = class(TForm)
    Image1      : TImage;
    Button1     : TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Remove_from_top();

  private
    Client1   : TMyClient;
    NumActors,
    divDelta    : Integer;
    divSize     : array of integer;
    procedure Set_Progress(cmd : string);
    procedure Setup_Wnd(cmd : string);
    procedure ResetTimeOut;
    FUNCTION GetDSSExeFile: String;
    { Private declarations }
  public
    Timer       : Integer;
    AbortON     : Boolean;
    property WriteProgress:string write Set_Progress;
    property SetupWnd: string write Setup_Wnd;
    { Public declarations }
  end;

CONST
  NUMDIVISIONS  = 1;
  UPDTPROGRESS  = 2;
  QUITPRG       = 3;
  HIDE_FORM     = 4;
  RESET         = 5;

var
  Form1: TForm1;

implementation

const
  MaxTimeOutValue = 500;
var
  TimeOut : Integer;

{$R *.dfm}

FUNCTION TForm1.GetDSSExeFile: String;

Var
   TheFileName:Array[0..MAX_PATH] of char;

Begin

    FillChar(TheFileName, SizeOF(TheFileName), #0);  // Fill it with nulls
    GetModuleFileName(HInstance, TheFileName, SizeOF(TheFileName));
    Result := TheFileName;

End;

procedure TForm1.Button1Click(Sender: TObject);
begin
  AbortON :=  True;
end;

procedure TForm1.FormCreate(Sender: TObject);
var

  F         : TextFile;
  JSONCfg   : TdJSON;
  JSONStr,
  newPath   : String;
  RefPos    : Integer;

begin
  AbortON       :=  False;
  With Memo1 do
  Begin
    Height        :=  64;
    Text          :=  '';
  End;
  newPath             :=  GetDSSExeFile();

  Client1             :=  TMyClient.Create(1, SERVER_PATH, SERVER_NAME);
  if not Client1.CreateServer then
    Caption := 'Error creating server'
  else
    Caption :=  'Simulation progress';

  TimeOut             :=  0;
  with Image1 do
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(0, 0, Width, Height);
  end;

  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);

end;

procedure TForm1.Remove_from_top();
begin
  SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TForm1.ResetTimeOut;
begin
  TimeOut := 0;
end;

procedure TForm1.Set_Progress(cmd : string);
var
  ImgWidth,
  G1Width,
  G2Width,
  CRCount,
  I,
  X1,
  X2,
  Temp,
  StrLen  : Integer;
  B       : TBitmap;
  Separator,
  TxtProg : String;

Begin
  B := TBitmap.Create;
  try
    B.Width               := Image1.Width;
    B.Height              := Image1.Height;
    B.Canvas.Brush.Color  := clWhite;
    B.Canvas.Brush.Style  := bsSolid;
    B.Canvas.Pen.Style    := psClear;
    B.Canvas.Pen.Width    := 1;
    B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
    // Format the variables for start drawing
    StrLen                :=  length(cmd);
    B.Canvas.Brush.Color  := clBlue;
    TxtProg               :=  'Progress (%):' + #13 + #10;
    // Starts drawing the progress for each actor
    if (StrLen < NumActors*3) then
    Begin
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
    End
    else
    Begin
      CRCount :=  0;
      for I := 0 to (NumActors - 1) do
      Begin
        if I >= 9 then Separator := '= '
        else Separator := ' = ';
        TxtProg     :=  TxtProg + 'Actor ' + inttostr(I + 1) + Separator + cmd.Substring(3*I,3) + ' ';
        inc(CRCount);
        if CRCount >= 4 then
        Begin
          TxtProg     :=  TxtProg + #13 + #10;
          CRCount     :=  0;
        End;
        Temp        :=  strtoint(cmd.Substring(3*I,3));
        X1          :=  divSize[I];
        X2          :=  X1 + ( Temp * divDelta) div 100;
        B.Canvas.FillRect(Rect(X1, 0, X2, B.Height));
      End;
    End;

    B.Canvas.Pen.Color := clBlack;
    B.Canvas.Pen.Style := psSolid;
    B.Canvas.Brush.Style := bsClear;
    B.Canvas.Rectangle(0, 0, B.Width, B.Height);
    // Puts the draw in the image indicator
    Image1.Picture.Assign(B);
  finally
    B.Free;
  end;
  Memo1.Text  :=  TxtProg;
End;

procedure TForm1.Timer1Timer(Sender: TObject); // Waits for 5 secs, if there is no
begin                                          // interactions with the caller, the
//  Inc(TimeOut);                                // app will close
  if TimeOut = MaxTimeOutValue then
  begin
    Timer1.Enabled := False;
    TimeOut        :=  0;
//    Close;
  end;
  if TimeOut >= 1000 then
    Close;
end;

procedure TForm1.Setup_Wnd(cmd : string);
var
  I   :   Integer;
Begin
  SetRoundMode(rmUp);
  NumActors :=  strtoint(cmd);
  divDelta  :=  round(Image1.Width div NumActors);
  setlength(divSize,NumActors);
  for I := 0 to High(divSize) do
  Begin
    divSize[I]  :=  I * divDelta;
  End;

End;

constructor TMyClient.Create(aID: Byte; aServer, aPipe: String);
begin
  inherited Create();
  FreeOnTerminate := True;
  Priority        := tpLower;
  fID             := aID;
  First_run       := True;
  if aServer = '' then
    fPipeName := Format(PIPE_FORMAT, ['.', aPipe])
  else
    fPipeName := Format(PIPE_FORMAT, [aServer, aPipe]);
end;

destructor TMyClient.Destroy();
begin
  if (pHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(pHandle);
  inherited Destroy();
end;

function TMyClient.CreateServer():Boolean;
var
  LAPipeName   : String;
begin
    // ... create Pipe (replaces old TCP/IP server)
  LAPipeName := Format('\\%s\pipe\%s', ['.', 'DSSProg']);
    // Check whether pipe does exist
  if WaitNamedPipe(PChar(LAPipeName), NMPWAIT_WAIT_FOREVER) then // 100 [ms]
    raise Exception.Create('Pipe exists.');
  // Create the pipe
  pHandle := CreateNamedPipe(
    PChar(LAPipeName),                                   // Pipe name
    PIPE_ACCESS_DUPLEX,                                 // Read/write access
    PIPE_TYPE_BYTE OR PIPE_READMODE_BYTE OR PIPE_WAIT,  // Message-type pipe; message read mode OR blocking mode //PIPE_NOWAIT
    PIPE_UNLIMITED_INSTANCES,                           // Unlimited instances
    10000,                                              // Output buffer size
    10000,                                              // Input buffer size
    0,                                            // Client time-out 50 [ms] default
    nil                                                 // Default security attributes
  );
  result  := False;
  if pHandle <> INVALID_HANDLE_VALUE then
      result  := True;
end;

// Executes the Client routine, waits for an remote message to execute
//-------------------------------------------------------------------------------
procedure TMyClient.Execute();
var
  msgToClient,
  Msg         : String;
  k,
  msgType     : Integer;
begin
  k := 0;
  while k = 0 do
  begin
    try
      if not ConnectNamedPipe(pHandle, nil) AND (GetLastError() = ERROR_PIPE_CONNECTED) then
      begin
        Msg := ReadMessage();
        msgType :=  -1;
        if Msg.Substring(0,3) = 'num' then
          msgType :=  NUMDIVISIONS;
        if Msg.Substring(0,3) = 'prg' then
          msgType :=  UPDTPROGRESS;
        if Msg.Substring(0,3) = 'ext' then
          msgType :=  QUITPRG;
        if Msg.Substring(0,3) = 'hid' then
          msgType :=  HIDE_FORM;
        if Msg.Substring(0,3) = 'rst' then
          msgType :=  RESET;

        // Evaluates the message type
        if msgType > 0 then
        Begin
          msgToClient  :=  '';
          // evaluates the message type
          case msgType of
            NUMDIVISIONS  : Begin
                              Form1.SetupWnd       :=  Msg.Substring(3);
                              Form1.Hide;
//                              ShowMessage(Msg.Substring(3));
                            End;
            UPDTPROGRESS  : Begin
                              Form1.Show;
                              Form1.BringToFront;
                              Form1.WriteProgress  :=  Msg.Substring(3);
                            End;
            HIDE_FORM     : Begin
                              Form1.Hide;
//                              Form1.Remove_from_top();
                            End;
            RESET         : Begin
                              Form1.AbortON := False;
                            End;
            QUITPRG       : Begin
                              msgToClient := 'Q';
                              k := 1;
                            End
            else
            Begin
              msgToClient  :=  'E'
            End;
          end;


          if msgToClient = '' then
          Begin
            if Form1.AbortON then
            Begin
              msgToClient  :=  'T';
            End
            else msgToClient  :=  'F';
          End;

          if k = 0 then
            SendMessage(msgToClient);

        End;

      end
      Else
      begin
        sleep(10);
        // This sectione evaluates if the connection with OpenDSS is broken, the first
        // time is due to the application launch. If it occurs again, means that OpenDSS
        // was probably terminated abruptly, leading to terminate the service.
        if First_run then
          First_run := False
        else
          k := 11;
      end;
    except
      on E: Exception do
      Begin
        k := 10;
      End;
    end;
  end;

  TimeOut := 1001;
  DisconnectNamedPipe(pHandle);
  CloseHandle(pHandle);
end;


procedure TMyClient.SendMessage(Msg: String);
var
  SendMessage : Boolean;
  Bytes       : Cardinal;
  MsgDta      : pchar;
  buf: array [0 .. 8095 - 1] of char;
begin
  // Prepare outgoing message
   MsgDta := pchar(Msg);
   fillchar(buf, 8095, #0);
   move(MsgDta[0], buf[0], Length(MsgDta) * Sizeof(char));
  // Send message
  SendMessage := WriteFile(
    pHandle,          // pipe handle
    buf,           // message
    length(MsgDta) * Sizeof(char),   // message length
    Bytes,            // bytes written
    nil
  );

end;

Function TMyClient.ReadMessage():String;
var
  MessageReceived : Boolean;
  Msg             : array [0 .. 8095 - 1] of char;
  MsgSz           : DWord;
  MsgStr          : String;
begin
  MessageReceived := ReadFile(
    pHandle,   // pipe handle
    Msg,       // buffer to receive reply
    BUFF_SIZE, // size of buffer
    MsgSz,  // number of bytes read
    nil);      // not overlapped

  Result  := '';
  if MessageReceived then
  begin
    MsgStr := Copy(Msg, 0, MsgSz);
    Result := MsgStr;
  end;
end;

end.
