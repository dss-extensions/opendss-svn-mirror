unit LoadShape;

{  8-18-00 Added call to InterpretDblArrayto allow File=Syntax }

Interface

{The LoadShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly, daily, and other load shapes.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LoadShape Class.
 This sets the active LoadShape object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveLoadShapeObj object and save the direct reference to the object.

 Loadshapes default to fixed interval data.  If the Interval is specified to be 0.0,
 then both time and multiplier data are expected.  If the Interval is  greater than 0.0,
 the user specifies only the multipliers.  The Hour command is ignored and the files are
 assumed to contain only the multiplier data.

 The user may place the data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 For fixed interval data, only the multiplier is expected.  Therefore, the CSV format would
 contain only one number per line.  The two binary formats are packed.

 For variable interval data, (hour, multiplier) pairs are expected in both formats.

 The Mean and Std Deviation are automatically computed when a new series of points is entered.

 The data may also be entered in unnormalized form.  The normalize=Yes command will force normalization.  That
 is, the multipliers are scaled so that the maximum value is 1.0.

 }

USES
   Command, DSSClass, DSSObject, UcMatrix, ucomplex, Arraydef;


TYPE

   TLoadShape = class(TDSSClass)
     private

       Function Get_Code:String;  // Returns active LoadShape string
       Procedure Set_Code(const Value:String);  // sets the  active LoadShape

       Procedure DoCSVFile(Const FileName:String);
       Procedure DoSngFile(Const FileName:String);
       Procedure DoDblFile(Const FileName:String);
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const ShapeName:String):Integer;  Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;     // uses global parser
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       Procedure TOPExport(ObjName:String);

       // Set this property to point ActiveLoadShapeObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;

   TLoadShapeObj = class(TDSSObject)
     private
        LastValueAccessed,
        FNumPoints :Integer;  // Number of points in curve
        ArrayPropertyIndex:Integer;

        // Function Get_FirstMult:Double;
        // Function Get_NextMult :Double;
        Function Get_Interval :Double;
    procedure Set_NumPoints(const Value: Integer);

      public

        Interval:Double;  //=0.0 then random interval     (hr)
        Hours,          // Time values (hr) if Interval > 0.0  Else nil
        PMultipliers,
        QMultipliers :pDoubleArray;  // Multipliers

        Mean,
        StdDev :Double;

        constructor Create(ParClass:TDSSClass; const LoadShapeName:String);
        destructor  Destroy; override;

        Function  GetMult(hr:double):Complex;  // Get multiplier at specified time
        Function  Mult(i:Integer):Double;  // get multiplier by index
        Function  Hour(i:Integer):Double;  // get hour corresponding to point index
        Procedure Normalize;  // Normalize the curve presently in memory
        Procedure CalcMeanandStdDev;

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

        Property NumPoints :Integer Read FNumPoints Write Set_NumPoints;
        Property PresentInterval :Double Read Get_Interval;
        {Property FirstMult :Double Read Get_FirstMult;}
        {Property NextMult  :Double Read Get_NextMult;}

   end;

VAR
   ActiveLoadShapeObj:TLoadShapeObj;

implementation

USES  ParserDel,  DSSGlobals, Sysutils,  MathUtil, Utilities, Classes, TOPExport, Math, PointerList;

Const NumPropsThisClass = 11;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLoadShape.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name := 'LoadShape';
     DSSClassType := DSS_OBJECT;

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;



END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TLoadShape.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShape.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1] := 'npts';     // Number of points to expect
     PropertyName[2] := 'interval'; // default = 1.0;
     PropertyName[3] := 'mult';     // vector of power multiplier values
     PropertyName[4] := 'hour';     // vextor of hour values
     PropertyName[5] := 'mean';     // set the mean (otherwise computed)
     PropertyName[6] := 'stddev';   // set the std dev (otherwise computed)
     PropertyName[7] := 'csvfile';   // Switch input to a csvfile
     PropertyName[8] := 'sngfile';  // switch input to a binary file of singles
     PropertyName[9] := 'dblfile';   // switch input to a binary file of singles
     PropertyName[10] := 'action'; // actions  Normalize
     PropertyName[11] := 'qmult'; // Q multiplier

     // define Property help values

     PropertyHelp[1] := 'Max number of points to expect in load shape vectors. This gets reset to the number of multiplier values found (in files only) if less than specified.';     // Number of points to expect
     PropertyHelp[2] := 'Time interval for fixed interval data. (hrs) Default = 1. '+
                        'If set = 0 then time data (in hours) is expected using either the Hour property or input files.'; // default = 1.0;
     PropertyHelp[3] := 'Array of multiplier values for active power (P).  Can also use the syntax: '+CRLF+
                        'mult = (file=filename)'+CRLF+
                        'where the file contains one value per line. In "file=" syntax, the number of points may be altered.';     // vector of multiplier values
     PropertyHelp[4] := 'Array of hour values. Only necessary to define for variable interval data.'+
                    ' If the data are fixed interval, do not use this property. ' +
                    'Can also use the syntax: '+CRLF+
                        'mult = (file=filename)'+CRLF+
                        'where the file contains one value per line.';     // vextor of hour values
     PropertyHelp[5] := 'Mean of the active power multipliers.  Automatically computed when a curve is defined.'+
                    'However, you may set it independently.  Used for Monte Carlo load simulations.';     // set the mean (otherwise computed)
     PropertyHelp[6] := 'Standard deviation of active power multipliers.  This is automatically computed when a '+
                     'vector or file of multipliers is entered.  However, you may set it to another value indepently.'+
                     'Is overwritten if you subsequently read in a curve' + CRLF + CRLF +
                     'Used for Monte Carlo load simulations.';   // set the std dev (otherwise computed)
     PropertyHelp[7] := 'Switch input of active power load curve data to a csv file '+
                        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, one per line. ' +
                        'NOTE: This action may reset the number of points to a lower value.';   // Switch input to a csvfile
     PropertyHelp[8] := 'Switch input of active power load curve data to a binary file of singles '+
                        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. ' +
                        'NOTE: This action may reset the number of points to a lower value.';  // switch input to a binary file of singles
     PropertyHelp[9] := 'Switch input of active power load curve data to a binary file of doubles '+
                        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. ' +
                        'NOTE: This action may reset the number of points to a lower value.';   // switch input to a binary file of singles
     PropertyHelp[10] := 'NORMALIZE is only defined action. After defining load curve data, setting action=normalize '+
                     'will modify the multipliers so that the peak is 1.0. ' +
                     'The mean and std deviation are recomputed.'; // Action
     PropertyHelp[11] := 'Array of multiplier values for reactive power (Q).  Can also use the syntax: '+CRLF+
                        'qmult = (file=filename)'+CRLF+
                        'where the file contains one value per line.';     // vector of multiplier values


     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.NewObject(const ObjName:String):Integer;
BEGIN
   // create a new object of this class and add to list
   With ActiveCircuit Do
   Begin
    ActiveDSSObject := TLoadShapeObj.Create(Self, ObjName);
    Result := AddObjectToList(ActiveDSSObject);
   End;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveLoadShapeObj := ElementList.Active;
  ActiveDSSObject := ActiveLoadShapeObj;

  WITH ActiveLoadShapeObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);
 
         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 610);
            1: NumPoints := Parser.Intvalue;
            2: Interval := Parser.DblValue;
            3: BEGIN
                 ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
                 // Allow possible Resetting (to a lower value) of num points when specifying multipliers not Hours
                 NumPoints := InterpretDblArray(Param, NumPoints, PMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
               END;
            4: BEGIN
                 ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
                 InterpretDblArray(Param, NumPoints, Hours);   // Parser.ParseAsVector(Npts, Hours);
               END;
            5: Mean := Parser.DblValue;
            6: StdDev := Parser.DblValue;
            7: DoCSVFile(Param);
            8: DoSngFile(Param);
            9: DoDblFile(Param);
           10: IF Param[1]='n' THEN Normalize;
           11: BEGIN
                 ReAllocmem(QMultipliers, Sizeof(QMultipliers^[1])*NumPoints);
                 InterpretDblArray(Param, NumPoints, QMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
               END;
         ELSE
           // Inherited parameters
             ClassEdit( ActiveLoadShapeObj, ParamPointer - NumPropsThisClass)
         END;

         CASE ParamPointer OF
           3,7,8,9,10: Begin
                            CalcMeanAndStdDev;
                            PropertyValue[5] := Str_Real(Mean, 3);
                            PropertyValue[6] := Str_Real(StdDev, 1);
                            If ParamPointer<>10 Then Begin
                               ArrayPropertyIndex := ParamPointer;
                               NumPoints := FNumPoints;  // Keep Properties in order for save command
                            End;
                       END;

         END;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     END; {WHILE}
  END; {WITH}
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.MakeLike(Const ShapeName:String):Integer;
VAR
   OtherLoadShape:TLoadShapeObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherLoadShape := Find(ShapeName);
   IF OtherLoadShape<>Nil THEN
    WITH ActiveLoadShapeObj DO BEGIN
        NumPoints := OtherLoadShape.NumPoints;
        Interval := OtherLoadShape.Interval;
        ReallocMem(PMultipliers, SizeOf(PMultipliers^[1])*NumPoints);
        FOR i := 1 To NumPoints DO PMultipliers^[i] := OtherLoadShape.PMultipliers^[i];
        If Assigned(OtherLoadShape.Qmultipliers) Then Begin
          ReallocMem(QMultipliers, SizeOf(QMultipliers^[1])*NumPoints);
          FOR i := 1 To NumPoints DO QMultipliers^[i] := OtherLoadShape.QMultipliers^[i];
        End;
        IF Interval>0.0 THEN ReallocMem(Hours, 0)
        ELSE BEGIN
          ReallocMem(Hours, SizeOf(Hours^[1])*NumPoints);
          FOR i := 1 To NumPoints DO Hours^[i] := OtherLoadShape.Hours^[i];
        END;
        Mean :=  OtherLoadShape.Mean;
        StdDev := OtherLoadShape.StdDev;

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherLoadShape.PropertyValue[i];
    END
   ELSE  DoSimpleMsg('Error in LoadShape MakeLike: "' + ShapeName + '" Not Found.', 611);


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.Init(Handle:Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TLoadShape.Init', -1);
   REsult := 0;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.Get_Code:String;  // Returns active line code string
VAR
  LoadShapeObj:TLoadShapeObj;

BEGIN

  LoadShapeObj := ElementList.Active;
  Result := LoadShapeObj.Name;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShape.Set_Code(const Value:String);  // sets the  active LoadShape

VAR
  LoadShapeObj:TLoadShapeObj;
  
BEGIN

    ActiveLoadShapeObj := Nil;
    LoadShapeObj := ElementList.First;
    WHILE LoadShapeObj<>Nil DO BEGIN

       IF CompareText(LoadShapeObj.Name, Value)=0 THEN BEGIN
          ActiveLoadShapeObj := LoadShapeObj;
          Exit;
       END;

       LoadShapeObj := ElementList.Next;
    END;

    DoSimpleMsg('LoadShape: "' + Value + '" not Found.', 612);

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShape.DoCSVFile(Const FileName:String);

VAR
    F:Textfile;
    i:Integer;

BEGIN
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 613);
       CloseFile(F);
       Exit;
    END;

    TRY

       WITH ActiveLoadShapeObj DO BEGIN
         ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
         IF Interval=0.0 THEN ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
         i := 0;
         WHILE (NOT EOF(F)) AND (i<FNumPoints) DO BEGIN
            Inc(i);
            IF Interval=0.0 THEN Read(F, Hours^[i]);
            Readln(F, PMultipliers^[i]);
          END;
         CloseFile(F);
         If i<>FNumPoints Then NumPoints := i;
        END;

    EXCEPT
       On E:Exception Do Begin
         DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message , 614);
         CloseFile(F);
         Exit;
       End;
    END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShape.DoSngFile(Const FileName:String);
VAR
    F:File of Single;
    Hr,M:Single;
    i:Integer;

BEGIN
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 615);
       CloseFile(F);
       Exit;
    END;

    TRY
       WITH ActiveLoadShapeObj DO BEGIN
         ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
         IF Interval=0.0 THEN ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
         i := 0;
         WHILE (NOT EOF(F)) AND (i<FNumPoints) DO BEGIN
          Inc(i);
          IF Interval=0.0 THEN BEGIN Read(F, Hr); Hours^[i] := Hr; END;
          Read(F, M ); PMultipliers^[i] := M;
         END;
         CloseFile(F);
         If i<>FNumPoints Then NumPoints := i;
       END;
    EXCEPT
       DoSimpleMsg('Error Processing LoadShape File: "' + FileName, 616);
       CloseFile(F);
       Exit;
    END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShape.DoDblFile(Const FileName:String);
VAR
    F:File of double;
    i:Integer;

BEGIN
    TRY
       AssignFile(F,FileName);
       Reset(F);
    EXCEPT
       DoSimpleMsg('Error Opening File: "' + FileName, 617);
       CloseFile(F);
       Exit;
    END;

    TRY
       WITH ActiveLoadShapeObj DO BEGIN
         ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
         IF Interval=0.0 THEN ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
         i := 0;
         WHILE (NOT EOF(F)) AND (i<FNumPoints) DO BEGIN
          Inc(i);
          IF Interval=0.0 THEN Read(F, Hours^[i]);
          Read(F, PMultipliers^[i]);
         END;
         CloseFile(F);
         If i<>FNumPoints Then NumPoints := i;
       END;
    EXCEPT
       DoSimpleMsg('Error Processing LoadShape File: "' + FileName, 618);
       CloseFile(F);
       Exit;
    END;


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLoadShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLoadShapeObj.Create(ParClass:TDSSClass; const LoadShapeName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(LoadShapeName);
     DSSObjType := ParClass.DSSClassType;

     LastValueAccessed := 1;
     FNumPoints := 0;
     Interval := 1.0;  // hr
     Hours := Nil;
     PMultipliers := Nil;
     QMultipliers := Nil;

     ArrayPropertyIndex := 0;

     InitPropertyValues(0);

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLoadShapeObj.Destroy;
BEGIN

    ReallocMem(Hours, 0);
    If Assigned(PMultipliers) Then  ReallocMem(PMultipliers, 0);
    If Assigned(QMultipliers) Then  ReallocMem(QMultipliers, 0);
    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShapeObj.GetMult(hr:double):Complex;

// This function returns a multiplier for the given hour.
// If no points exist in the curve, the result is  1.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.

VAR
   Index, i:Integer;

BEGIN

  Result.re := 1.0;
  Result.im := 1.0;    // default return value if no points in curve

  IF FNumPoints>0 THEN         // Handle Exceptional cases
  IF FNumPoints=1 THEN Begin
    Result.re := PMultipliers^[1];
    If Assigned(QMultipliers) Then Result.im := QMultipliers^[1] Else Result.im := Result.re;
  End
  ELSE
    BEGIN
      IF Interval>0.0 THEN  BEGIN
           Index := round(hr/Interval);
           IF Index>FNumPoints Then Index := Index Mod FNumPoints;  // Wrap around using remainder
           IF Index=0 THEN Index := FNumPoints;
           Result.Re := PMultipliers^[Index];
           If Assigned(QMultipliers) Then Result.im := QMultipliers^[Index] Else Result.im := Result.re;
        END
      ELSE  BEGIN
          // For random interval

        { Start with previous value accessed under the assumption that most
          of the time, this function will be called sequentially}

          {Normalize Hr to max hour in curve to get wraparound}
          If Hr>Hours^[FNumPoints] Then Begin
              Hr := Hr - Trunc(Hr/Hours^[FNumPoints])*Hours^[FNumPoints];
          End;

           IF Hours^[LastValueAccessed] > Hr THEN LastValueAccessed := 1;  // Start over from beginning
           FOR i := LastValueAccessed+1 TO FNumPoints DO
             BEGIN
               IF Abs(Hours^[i]-Hr)<0.00001 THEN  // If close to an actual point, just use it.
                 BEGIN
                   Result.re := PMultipliers^[i];
                   If Assigned(QMultipliers) Then Result.im := QMultipliers^[i] Else Result.im := Result.re;
                   LastValueAccessed := i;
                   Exit;
                 END
               ELSE IF Hours^[i]>Hr THEN      // Interpolate for multiplier
                 BEGIN
                   LastValueAccessed := i-1;
                   Result.re := PMultipliers^[LastValueAccessed] +
                             (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed])*
                             (PMultipliers^[i] -PMultipliers^[LastValueAccessed]);
                   If Assigned(QMultipliers) Then
                        Result.im := QMultipliers^[LastValueAccessed] +
                             (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed])*
                             (QMultipliers^[i] -QMultipliers^[LastValueAccessed])
                        Else Result.im := Result.re;
                   Exit ;
                 END;
             END;

           // If we fall through the loop, just use last value
           LastValueAccessed := FNumPoints-1;
           Result.re := PMultipliers^[FNumPoints];
           If Assigned(QMultipliers) Then Result.im := QMultipliers^[FNumPoints] Else Result.im := Result.re;
        END;
    END;

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShapeObj.Normalize;
// normalize this load shape
VAR

   MaxMult:Double;

 Procedure DoNormalize(Multipliers:pDoubleArray);
 Var i:Integer;
 Begin
   If FNumPoints>0 THEN BEGIN
     MaxMult := Abs(Multipliers^[1]);
     FOR i := 2 TO FNumPoints DO BEGIN
        IF Abs(Multipliers^[i]) > MaxMult THEN MaxMult := Abs(Multipliers^[i]);
     END;
     IF MaxMult = 0.0 THEN MaxMult := 1.0;
     FOR i := 1 TO FNumPoints DO BEGIN
        Multipliers^[i] := Multipliers^[i]/MaxMult;
     END;
   END;
 End;

BEGIN
   DoNormalize(PMultipliers);
   If Assigned(QMultipliers) Then  DoNormalize(QMultipliers);
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShapeObj.CalcMeanandStdDev;

BEGIN

   If FNumPoints>0 Then RCDMeanandStdDev(PMultipliers, FNumPoints, Mean, StdDev);
   { No Action is taken on Q multipliers}
END;

(*
Function TLoadShapeObj.Get_FirstMult:Double;
Begin

  If Npts>0 Then Begin
     Result :=  Multipliers^[1];
     LastValueAccessed := 1;
  End
  Else
      Result := 0.0;

End;

Function TLoadShapeObj.Get_NextMult :Double;
Begin

  If Npts>0 Then Begin
     Inc(LastValueAccessed);
     If LastValueAccessed>Npts Then Begin
         Result := 0.0;
         Dec(LastValueAccessed);
     End
     Else Begin
          Result :=  Multipliers^[LastValueAccessed];
     End;
  End Else
      Result := 0.0;

End;
*)
Function TLoadShapeObj.Get_Interval :Double;
Begin

     If Interval>0.0 Then Result := Interval
     Else Begin
         If LastValueAccessed>1 Then
            Result := Hours^[LastValueAccessed] - Hours^[LastValueAccessed - 1]
         Else
            Result := 0.0;
     End;


End;

Function TLoadShapeObj.Mult(i:Integer) :Double;
Begin

     If (i <= FNumPoints) and (i > 0) Then Begin
      Result := PMultipliers^[i];
      LastValueAccessed := i;
     End Else
      Result := 0.0;

End;

FUNCTION TLoadShapeObj.Hour(i:Integer) :Double;
Begin

   If Interval = 0 Then Begin
     If (i<= FNumPoints) and (i>0) Then Begin
      Result := Hours^[i];
      LastValueAccessed := i;
     End Else
      Result := 0.0;
   End Else Begin
       Result := Hours^[i] * Interval;
       LastValueAccessed := i;
   End;

End;


PROCEDURE TLoadShapeObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Begin
        CASE i of
          3, 4: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[i],')');
        ELSE
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
        END;
     End;


end;

FUNCTION TLoadShapeObj.GetPropertyValue(Index: Integer): String;
VAR
   i: Integer;
begin
        Case Index of
        3,4: Result := '(';
        Else
        Result := '';
        End;

        CASE Index of
          3: FOR i := 1 to FNumPoints Do Result := Result + Format('%-g, ' , [PMultipliers^[i]]);
          4: IF Hours <> Nil THEN FOR i := 1 to FNumPoints Do Result := Result + Format('%-g, ' , [Hours^[i]]) ;
          11: IF Assigned(QMultipliers) Then Begin
                Result := '(';
                FOR i := 1 to FNumPoints Do Result := Result + Format('%-g, ' , [QMultipliers^[i]]);
                Result := Result + ')';
              End;
        ELSE
           Result := Inherited GetPropertyValue(index);
        END;

        Case Index of
        3,4: Result := Result + ')';
        Else
        End;
end;

procedure TLoadShapeObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '0';     // Number of points to expect
     PropertyValue[2] := '1'; // default = 1.0;
     PropertyValue[3] := '';     // vector of multiplier values
     PropertyValue[4] := '';     // vextor of hour values
     PropertyValue[5] := '0';     // set the mean (otherwise computed)
     PropertyValue[6] := '0';   // set the std dev (otherwise computed)
     PropertyValue[7] := '';   // Switch input to a csvfile
     PropertyValue[8] := '';  // switch input to a binary file of singles
     PropertyValue[9] := '';   // switch input to a binary file of singles
     PropertyValue[10] := 'normalize'; // action option .
     PropertyValue[11] := ''; // Qmult.

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TLoadShape.TOPExport(ObjName:String);

Var
   NameList, CNames:TStringList;
   Vbuf, CBuf:pDoubleArray;
   Obj:TLoadShapeObj;
   MaxPts, i, j:Integer;
   MaxTime, MinInterval, Hr_Time :Double;
   ObjList:TPointerList;

begin
     TOPTransferFile.FileName := DSSDataDirectory + 'TOP_LoadShape.STO';
     TRY
         TOPTransferFile.Open;
     EXCEPT
        ON E:Exception Do
        Begin
          DoSimpleMsg('TOP Transfer File Error: '+E.message, 619);
          TRY
              TopTransferFile.Close;
          EXCEPT
              {OK if Error}
          End;
          Exit;
        End;
     END;

     {Send only fixed interval data}

     ObjList := TPointerList.Create(10);
     NameList := TStringList.Create;
     CNames := TStringList.Create;

     {Make a List of fixed interval data where the interval is greater than 1 minute}
     IF CompareText(ObjName, 'ALL')=0 Then Begin
       Obj := ElementList.First;
       While Obj <>  Nil Do Begin
          If Obj.Interval>(1.0/60.0) Then ObjList.Add(Obj);
          Obj := ElementList.Next;
       End;
     End
     ELSE Begin
       Obj := Find(ObjName);
       If Obj <>  Nil Then Begin
          If Obj.Interval>(1.0/60.0) Then ObjList.Add(Obj)
          Else DoSimpleMsg('Loadshape.'+ObjName+' is not hourly fixed interval.', 620);
       End
       Else Begin
           DoSimpleMsg('Loadshape.'+ObjName+' not found.', 621);
       End;

     End;

     {If none found, exit}
     If ObjList.ListSize >0 Then Begin

       {Find Max number of points}
       MaxTime := 0.0;
       MinInterval := 1.0;
       Obj := ObjList.First;
       While Obj <>  Nil Do Begin
          MaxTime :=  Max(MaxTime, Obj.NumPoints*Obj.Interval) ;
          MinInterval := Min(MinInterval, Obj.Interval);
          NameList.Add(Obj.Name);
          Obj := ObjList.Next;
       End;
      // SetLength(Xarray, maxPts);
       MaxPts := Round(MaxTime/MinInterval);

       TopTransferFile.WriteHeader(0.0, MaxTime, MinInterval, ObjList.ListSize, 0, 16,  'DSS (TM), Electrotek Concepts (R)');
       TopTransferFile.WriteNames(NameList, CNames);

       Hr_Time := 0.0;

       VBuf := AllocMem(Sizeof(VBuf^[1])* ObjList.ListSize);
       CBuf := AllocMem(Sizeof(VBuf^[1])* 1);   // just a dummy -- Cbuf is ignored here

       For i := 1 to MaxPts Do Begin
          For j := 1 to ObjList.ListSize Do Begin
              Obj := ObjList.Get(j);
              VBuf^[j] :=  Obj.GetMult (Hr_Time).Re;
          End;
          TopTransferFile.WriteData(HR_Time, Vbuf, Cbuf);
          HR_Time := HR_Time + MinInterval;
       End;

       TopTransferFile.Close;
       TopTransferFile.SendToTop;
       Reallocmem(Vbuf,0);
       Reallocmem(Cbuf,0);
     End;
     
     ObjList.Free;
     NameList.Free;
     CNames.Free;
end;

procedure TLoadShapeObj.Set_NumPoints(const Value: Integer);
begin
        FNumPoints := Value;
        PropertyValue[1] := IntToStr(Value);   // Update property list variable

        // Reset array property values to keep them in propoer order in Save

        If ArrayPropertyIndex>0  Then  PropertyValue[ArrayPropertyIndex] := PropertyValue[ArrayPropertyIndex];
        If Assigned(Qmultipliers) Then  PropertyValue[11] := PropertyValue[11];

end;

end.
