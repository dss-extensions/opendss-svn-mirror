unit DParser;

interface

function ParserI(mode: longint; arg:longint):longint;stdcall;
function ParserF(mode: longint; arg:double):double;stdcall;
function ParserS(mode: longint; arg:pAnsiChar):pAnsiChar;stdcall;
procedure ParserV(mode: longint; out arg:Olevariant);stdcall;

implementation

uses ComServ, ParserDel, Variants, ArrayDef;

Var ComParser : ParserDel.TParser;

function ParserI(mode: longint; arg:longint):longint;stdcall;
begin
  Result:=0;
  case mode of
  0: begin // Parser.IntValue
    Result := ComParser.IntValue ;
  end;
  1: begin // Parser.ResetDelimiters
     ComParser.ResetDelims;
  end;
  2: begin  // Parser.Autoincrement read
     if ComParser.AutoIncrement then Result := 1;
  end;
  3: begin  // Parser.Autoincrement write
     if arg=1 then
       ComParser.AutoIncrement := TRUE
     else
       ComParser.AutoIncrement := FALSE;
  end
  else
      Result:=-1;
  end;
end;

//***************************Floating point type properties*********************
function ParserF(mode: longint; arg:double):double;stdcall;
begin
  case mode of
  0: begin  // Parser.DblValue
      Result := ComParser.DblValue ;
  end
  else
      Result:=-1.0;
  end;
end;

//***************************String type properties*****************************
function ParserS(mode: longint; arg:pAnsiChar):pAnsiChar;stdcall;
begin
  Result := pAnsiChar(AnsiString(''));
  case mode of
  0: begin  // Parser.CmdString read
     Result := pAnsiChar(AnsiString(ComParser.CmdString));
  end;
  1: begin  // Parser.CmdString write
     ComParser.CmdString := widestring(arg);
  end;
  2: begin  // Parser.NextParam
     Result := pAnsiChar(AnsiString(ComParser.NextParam));
  end;
  3: begin  // Parser.StrValue
     Result := pAnsiChar(AnsiString(ComParser.StrValue));
  end;
  4: begin  // Parser.WhiteSpace read
     Result := pAnsiChar(AnsiString(Comparser.Whitespace));
  end;
  5: begin  // Parser.WhiteSpace write
     ComParser.Whitespace := widestring(arg);
  end;
  6: begin  // Parser.BeginQuote read
      Result := pAnsiChar(AnsiString(ComParser.BeginQuoteChars));
  end;
  7: begin  // Parser.BeginQuote write
      ComParser.BeginQuoteChars := widestring(arg);
  end;
  8: begin  // Parser.EndQuote read
      Result := pAnsiChar(AnsiString(ComParser.EndQuoteChars));
  end;
  9: begin  // Parser.EndQuote write
      ComParser.EndQuoteChars := widestring(arg);
  end;
  10: begin  // Parser.Delimiters read
      Result := pAnsiChar(AnsiString(ComParser.Delimiters));
  end;
  11: begin  // Parser.Delimiters write
      ComParser.Delimiters := widestring(arg);
  end
  else
      Result:= pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//***************************Variant type properties****************************
procedure ParserV(mode: longint; out arg:Olevariant);stdcall;

Var  i, ActualSize,MatrixSize:Integer;
     VectorBuffer:pDoubleArray;
     ExpectedSize,ExpectedOrder:integer;
     MatrixBuffer:pDoubleArray;

begin
  case mode of
  0: begin  // Parser.Vector
    ExpectedSize:=integer(arg);
    VectorBuffer := Allocmem(SizeOf(VectorBuffer^[1])*ExpectedSize);
    ActualSize := ComParser.ParseAsVector(ExpectedSize, VectorBuffer);
    arg := VarArrayCreate([0, (ActualSize-1)], varDouble);
    For i := 0 to (ActualSize-1) Do arg[i] := VectorBuffer^[i+1];
    Reallocmem(VectorBuffer, 0);
  end;
  1: begin  // Parser.Matrix
    ExpectedOrder:=integer(arg);
    MatrixSize := ExpectedOrder*ExpectedOrder;
    MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1])*MatrixSize);
    ComParser.ParseAsMatrix(ExpectedOrder, MatrixBuffer);

    arg := VarArrayCreate([0, (MatrixSize-1)], varDouble);
    For i := 0 to (MatrixSize-1) Do arg[i] := MatrixBuffer^[i+1];

    Reallocmem(MatrixBuffer, 0);
  end;
  2: begin  // Parser.SymMatrix
    ExpectedOrder:=integer(arg);
    MatrixSize := ExpectedOrder*ExpectedOrder;
    MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1])*MatrixSize);
    ComParser.ParseAsSymMatrix(ExpectedOrder, MatrixBuffer);

    arg := VarArrayCreate([0, (MatrixSize-1)], varDouble);
    For i := 0 to (MatrixSize-1) Do arg[i] := MatrixBuffer^[i+1];

    Reallocmem(MatrixBuffer, 0);
  end
  else
      arg[0]:='Error, parameter not valid';
  end;
end;

end.
