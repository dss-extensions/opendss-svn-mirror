TYPE

  {NOTE: Maxlen argument is to better accommodate Fortran strings.  VB also}
  {      Caller must allocate space for pchar values       }
   pDSSCallBacks = ^TDSSCallBacks;  {Pointer to callback structure}
   TDSSCallBacks = Packed Record
        MsgCallBack: Procedure (S: pchar; Maxlen:Cardinal);Stdcall; {Make use of DSS Message handling}

        {Routines for using DSS Parser.  This allows you to write models that accept
         syntax like other DSS scripts.}
        GetIntValue:Procedure(var i:Integer);Stdcall; {Get next param as an integer}
        GetDblValue:Procedure(var x:Double);Stdcall;  {Get next param as a double}
        GetStrValue:Procedure(s:pchar; maxlen:Cardinal);Stdcall;
             {Get next param as a string <= maxlen characters  (Cardinal = 32-bit unsigned)}
             {caller must allocate space for s (Maxlen chars)}
        LoadParser:Procedure(S:pchar; maxlen:Cardinal);Stdcall; // Copies a string into a special instance of the DSS parser
        NextParam: Function (ParamName:pchar; Maxlen:Cardinal):Integer;Stdcall;
             {Advance to the next parameter and
              Get name of the param just retrieved, if one was given.
              Returns length of parameter found.  If 0, then end of string.
              This is to handle the syntax "paramname=paramvalue" commonly used in DSS scripts
              Copies the string to the location specified by s up to maxlen characters.
              Caller must allocate space (Maxlen chars)}
        DoDSSCommand: Procedure(S: pChar; Maxlen:Cardinal);StdCall;
        GetActiveElementBusNames: Procedure(Name1:pchar; Len1:Cardinal; Name2:pchar; Len2:Cardinal); StdCall;
        GetActiveElementIndex: Function():Integer; StdCall;
        IsActiveElementEnabled: Function():Boolean; StdCall;
   End;
