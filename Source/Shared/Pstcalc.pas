Unit Pstcalc;

 {
  ----------------------------------------------------------
  Copyright (c) 2011, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

Interface

Uses ArrayDef;


// IEC868 FlickerMeter
// note: the scaling factor being used has been corrected.  It is scaling the output of Block 4 to
// 1.0 for the values given in Table 1 and Table 2. The last table in the std should be used for checking Pst
// 4/12/05 The meter is verified using Table 2 and Table 5 (Pinst and Pst tables)
// 7/28/05: This cpp is designed to receive a 6-cycle rms data stream from the dss and return Pst
// 8/3/05: Updated to receive single to 6-cycle rms data streams.  The rms window "DeltaT" determines which scaling factor to use
// 8/4/05: added back functionality to receive AC data.  Now these are 2 separate loops
// 8/31/11 Converted to Object Pascal


// #include <stdlib.h>
// #include <math.h>

Function  PstAC (pVoltages:pDoubleArray; Delta_T:double; Npts, Lamp:Integer):Double;
Function  PstRMS(pVoltages:pdoubleArray; Freqbase:double;  NcyclesperSample,  Npts,  Lamp:Integer):double;


Implementation

Uses Math, sysutils;

{$DEFINE DEBUG}

{$UNDEF DEBUG}

Const MAXBINS=50000;

Type 
   BinArray     = Array[0..MAXBINS] of Double;
   pBinArray    = ^BinArray;
   Double6Array = Array[0..5] of double; 


Var

{$IF Defined(DEBUG)}
{****}  DebugFile:Textfile; // for debugging
{$IFEND}



	{static}  staticfn:double;
	{static}  staticN:Integer;
  {static}  rms_value :double;	// initialize to reference rms input
  rms_reference   :double;	// internal rms reference value (do not change)
  FirstTime1 :Boolean;
	Fbase      :double;			//not needed for AC signal input
  Tstep      :double;		//internal timestep, may or may not equal DeltaT
  Pst_Time        :double;
  Pst_Timer       :double;
  rms_input       :double;	// nominal line-to-neutral rms input
  RMS_sample      :double;
  DeltaT          :double;


  Vin, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, RMSVin :Double6Array;

  Bins0, Bins1 :pBinArray;
  bin_ceiling  :double;
  number_bins  :Integer;

  {Filter Coefficients}

  WA2,  WB2, WC2, WD2, WE2,  WF2,  WG2   :Double;  // weighting filter coefficients
  IVAA,  IVAB,   IVAC,  IVAD,  IVAE,
  BA,  BB,  BC,  BD, BE,  BG,  BH,  BI,
  BJ,  BK, BL, BM, BN, BP,
  SA, // time constant of sliding mean filter
  internal_reference      :double;

  lamp_type,     // 0 for 120V filters, 1 for 230V filters
  input_type       :Integer;  // 0 for AC, 1 for 1-cycle RMS, 6 for 6-cycle rms
  samples_per_RMS :double;	//number of samples in RMS calculation (unused at the moment, feeds Out_inst
  acquire_flicker :Boolean;


///////////////////////////////////////////////////////////////////////////////
// searches through the specified array for a bin and then
// interpolates (if needed)
///////////////////////////////////////////////////////////////////////////////
Function SB( y:double;  bins:pBinArray ):Double;

Var

	n  :Integer;
	found :Boolean;

Begin

	found := False;
	n := 0;

	while ((not found) and (n < number_bins)) Do
	Begin
		if (y <= bins^[n]) Then Begin found := TRUE; End
		                   else Begin n:=n+1; End;
	End;

	if (n > 0) then
	Begin           // Interpolate
      Result := bin_ceiling*(n-1)/number_bins +
                (y-bins^[n-1])*(bin_ceiling/number_bins)/(bins^[n]-bins^[n-1]);
	End
	else
  Begin Result := 0.0; End;

End;

Procedure ZeroOutBins;
Var
   n:Integer;
Begin

	for n := 0 to number_bins-1 do   Bins0^[n] := 0.0;
	for n := 0 to number_bins-1 do   Bins1^[n] := 0.0;

End;

///////////////////////////////////////////////////////////////////////////////
// Calculates the Pst
///////////////////////////////////////////////////////////////////////////////
Function CalcPst:Double;
Var

	num_pts :double;  // ?? long double Why??
	n       :Integer;
	P01, P1s, P3s, P10s, P50s :double;

Begin

  num_pts := 0;
	for n := 0 to number_bins-1 do
	Begin
      num_pts   := num_pts + bins0^[n];
      bins1^[n] := num_pts;
	End;

	for n := 0 to number_bins-1 do
	Begin
		   bins1^[n] := bins1^[n] / num_pts;
	End;

	P01 :=  SB(0.999, bins1);
	P1s := (SB(0.993, bins1)+
				  SB(0.990, bins1)+
				  SB(0.985, bins1))/3.0;
	P3s := (SB(0.978, bins1)+
				  SB(0.970, bins1)+
				  SB(0.960, bins1))/3.0;
	P10s :=(SB(0.940, bins1)+
				  SB(0.920, bins1)+
				  SB(0.900, bins1)+
				  SB(0.870, bins1)+
				  SB(0.830, bins1))/5.0;
	P50s :=(SB(0.700, bins1)+
				  SB(0.500, bins1)+
				  SB(0.200, bins1))/3.0;

 // This is the Pst

	Result := sqrt(0.0314*P01+0.0525*P1s+0.0657*P3s+0.28*P10s+0.08*P50s);

End;

//////////////////////////////////////////////////////////////////////
// Calculates the coefficients for the weighting filter
//////////////////////////////////////////////////////////////////////
Procedure Set_Filter_Coefficients(input_type:Integer);

var 
     K, Lambda, W1, W2, W3, W4:Double;
     
Begin

	// Coefficients for Input Voltage Adapter
	// L = 8.93125 H
	// C = 35.725 F
	// R = 1.0 Ohms

    IVAA := 8.93125*35.725;
    IVAB := 35.725;
    IVAC := 4.0*IVAA/(Tstep*Tstep) + 1.0 - 2.0*IVAB/Tstep;
    IVAD := 2.0 - 8.0*IVAA/(Tstep*Tstep);
    IVAE := 4.0*IVAA/(Tstep*Tstep) + 1.0 + 2.0*IVAB/Tstep;

	 
	// Bandpass centered at 8.5Hz
	// 120V lamp       
    if ( lamp_type = 0 ) Then
    Begin
      K := 1.6357;
      Lambda := 26.1843893695;
      W1 := 57.0335348916;
      W2 := 18.4719490509;
      W3 := 8.76170084893;
      W4 := 108.794107576;
    End

    else    // Bandpass centered at 8.8Hz
            // 230V lamp
    Begin
      K := 1.74802;
      Lambda := 25.5085385419;
      W1 := 57.5221844961;
      W2 := 14.3243430315;
      W3 := 7.69910111615;
      W4 := 137.601758227;
    End;

	// Coefficients for Bandpass
	// 1st set of substitutions
    BA := 0.314159265359;
    BB := 113.834561498;
    BC := 48361.06156533785;
    BD := 311.00180567;
    BE := 424.836367168;
    // 2nd set of substitutions
    BG := 1+BA*Tstep/2.0;
    BH := BA*Tstep/2.0-1.0;
    BI := 4.0/(Tstep*Tstep)+2.0*BB/Tstep+BC;
    BJ :=-8.0/(Tstep*Tstep)+2.0*BC;
    BK := 4.0/(Tstep*Tstep)-2.0*BB/Tstep+BC;
    BL := 4.0/(Tstep*Tstep)+2.0*BD/Tstep+BC;
    BM := 4.0/(Tstep*Tstep)-2.0*BD/Tstep+BC;
    BN := 4.0/(Tstep*Tstep)+2.0*BE/Tstep+BC;
    BP := 4.0/(Tstep*Tstep)-2.0*BE/Tstep+BC;

	// Coefficients for Weighting filter
    WA2 := 4.0*K*W1*W3*W4/(Tstep*Tstep);
    WB2 := 2.0*K*W1*W2*W3*W4/Tstep;
    WC2 := 16.0*W2/power(Tstep,4);
    WD2 := 8.0*W2*(2.0*Lambda + W3 + W4)/power(Tstep,3);
    WE2 := 4.0*W2*(W3*W4 + W1*W1 + 2.0*Lambda*(W3 + W4))/(Tstep*Tstep);
    WF2 := 2.0*W2*(2.0*Lambda*W3*W4 + W1*W1*(W3 + W4))/Tstep;
    WG2 := W2*W3*W4*W1*W1;

	// time constant of sliding mean filter
	  SA := 0.3;
	
	// internal reference
    if (input_type = 0) Then internal_reference := 676.372;  // See "new 868 testing and scaling.xls" for derivation
    if (input_type = 1) Then internal_reference := 0.01106784;	// new scaling factor 7/25/05, based on 1-cycle RMS
	// using greater than 1-cycle RMS may result in errors
    if (input_type = 3) Then internal_reference := 0.009;	    // new scaling factor 8/3/05, based on 3-cycle RMS
    if (input_type = 6) Then internal_reference := 0.008449;	// new scaling factor 7/25/05, based on 6-cycle RMS
End;

//////////////////////////////////////////////////////////////////////
// Put samples that get through the filter in the proper bins
//////////////////////////////////////////////////////////////////////
Procedure Gather_Bins( X10_value:double; bins:pBinArray);
var
	  Bindex:Integer;
    Procedure My_inc(Var x:Double);
    Begin X := X + 1.0; End;
Begin
	if (X10_value > bin_ceiling) then
	Begin
		   My_inc(bins^[number_bins - 1]);  // increment count
	End
	else
	Begin
      Bindex := trunc(number_bins * X10_value/bin_ceiling);
      My_inc(bins^[Bindex]);
	End;
End;

///////////////////////////////////////////////////////////////////////////////
// shifts every array value up (back in time)
///////////////////////////////////////////////////////////////////////////////
Procedure Sample_Shift;

var
    n:integer;

Begin
		
	For n := 5 downto 1 do
	Begin
      Vin[n] := Vin[n-1];
      RMSVin[n] := RMSVin[n-1];
      X1[n]  := X1[n-1];
      X2[n]  := X2[n-1];
      X3[n]  := X3[n-1];
      X4[n]  := X4[n-1];
      X5[n]  := X5[n-1];
      X6[n]  := X6[n-1];
      X7[n]  := X7[n-1];
      X8[n]  := X8[n-1];
      X9[n]  := X9[n-1];
      X10[n] := X10[n-1];
	End;
End;

///////////////////////////////////////////////////////////////////////////////
// Main Flicker Calculation Function
///////////////////////////////////////////////////////////////////////////////

Procedure Get_Pinst(VAR pst, max_flicker :double);

Begin

  If FirstTime1 Then  Begin        // To make this work like the C++ routine
	   rms_value  := rms_reference;
     FirstTime1 := FALSE;
  End;

	if ( input_type = 0 ) then	// AC input
	Begin
      //Scale input to rms_reference value
      Vin[0]   := rms_reference*Vin[0]/rms_input;
      staticfn := staticfn + Vin[0]*Vin[0];

      // Calculates single cycle rms of input
      if ( staticN >= 1.0/(Fbase*DeltaT) )	then
      Begin
        rms_value:= sqrt(staticfn/(staticN));
        staticN  := 0;
        staticfn := 0.0;
      End;
      staticN   := staticN + 1;
      RMSVin[0] := rms_value;
		
      X1[0] := (RMSVin[0]+2.0*RMSVin[1]+RMSVin[2]-IVAD*X1[1]-IVAC*X1[2])/IVAE;
      X2[0] := Vin[0]*(1.0 - (X1[0]-120.0)/RMSVin[0]);
      X3[0] := X2[0]*X2[0];
	End

	else	// RMS input

	Begin
      RMSVin[0] := rms_reference * RMS_sample/rms_input; // per unitize rms value
      X1[0]     := (RMSVin[0]+2.0*RMSVin[1]+RMSVin[2]-IVAD*X1[1]-IVAC*X1[2])/IVAE;
      X3[0]     := RMSVin[0]*(1.0 - (X1[0]-120.0)/RMSVin[0]);
	End;

	// Bandpass (HP at .05Hz and 6th order Butteworth LP at 35Hz)
	X4[0] := (X3[0]-X3[1]-BH*X4[1])/BG;
	X5[0] := (BC*(X4[0]+2*X4[1]+X4[2]) - (BJ*X5[1]+BK*X5[2]))/BI;
	X6[0] := (BC*(X5[0]+2*X5[1]+X5[2]) - (BJ*X6[1]+BM*X6[2]))/BL;
	X7[0] := (BC*(X6[0]+2*X6[1]+X6[2]) - (BJ*X7[1]+BP*X7[2]))/BN;

	// Weighting filter
	X8[0] := ((WA2+WB2)*X7[0] + 2*WB2*X7[1] - 2*WA2*X7[2] - 2*WB2*X7[3] +
            (WA2-WB2)*X7[4] - (2*WF2+4*WG2-4*WC2-2*WD2)*X8[1] -
            (6*WC2-2*WE2+6*WG2)*X8[2] - (2*WD2+4*WG2-4*WC2-2*WF2)*X8[3] -
            (WC2-WD2+WE2-WF2+WG2)*X8[4])/(WC2+WD2+WE2+WF2+WG2);

	// Sliding Mean filter
	X9[0] := (X8[0]*X8[0] + X8[1]*X8[1] - (1-2*SA/Tstep)*X9[1])/(1 + 2*SA/Tstep);
	X10[0]:=  X9[0]/internal_reference;

	{////////////// This starts the Pst calculations //////////////}
	if acquire_flicker Then
	Begin
      Pst_Timer := Pst_Timer + Tstep;
      if (X10[0] > max_flicker) Then max_flicker := X10[0];
      Gather_Bins(X10[0], bins0);
	End;

	if (Pst_Timer >= Pst_Time) then
  // OK, we got everything in the bins, let's compute Pst
	Begin
      pst          := CalcPst;

{$IF Defined(DEBUG)}
{****}
{****}Writeln(DebugFile, Format('Max_flicker=%.8g',[ Max_Flicker]));
{****}For i := 0 to Number_Bins-1 Do Writeln(Debugfile, Format('%d, %.8g, %.8g',[i, Bins0^[i], Bins1^[i]]));
{****}
{$IFEND}

      Pst_Timer    := 0.0;
      ZeroOutBins;   // Zero Bins0 and Bins1 out for next time
	End;

	Sample_Shift;

End;

//*******************************************************************
//*******************************************************************
Procedure Init6Array (var Y:Double6Array; V1, V2, V3, V4, V5, V6: Double);
Begin
    Y[0] := V1;
    Y[1] := V2;
    Y[2] := V3;
    Y[3] := V4;
    Y[4] := V5;
    Y[5] := V6;
End;

//*******************************************************************
//*******************************************************************
Function _Pst(Varray:pDoubleArray;   Npts:integer):Double;

Var

    pst             :double;
    max_flicker     :double;
    time            :double;   // long double???

    V               :pDouble;   // This will be used to index the Varray

    Vindex          :Integer;

    FirstSample     :double;


    N               :Integer;
    fn              :double;
    Vinput          :double;
    pV              :pdouble;
    sp              :Integer;

    SynthesizedSamples  :Integer;
    SamplesPerDeltaT  :double;	// this value is used when RMS data is used as input

Begin

// DEBUG file
{$IF Defined(DEBUG)}
{****}AssignFile(DebugFile, 'DebugOut.CSV');
{****}Rewrite(DebugFile);
{$IFEND}

  V := @Varray^[1];  // Initialize pointer to first element in the array

	samples_per_RMS := 1.0;	//number of samples in RMS calculation (unused at the moment, feeds Out_inst
	
	rms_reference   := 120.0;	// internal rms reference value (do not change)

	init6Array( Vin, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
	init6Array( RMSVin, rms_reference,rms_reference,rms_reference,rms_reference,rms_reference,rms_reference);	// RMS input voltage

	init6Array( X1, rms_reference,rms_reference,rms_reference,rms_reference,rms_reference,rms_reference);
	init6Array( X2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);		// Output of Block 1
	init6Array( X3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);		// Output of Block 2
	init6Array( X4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);	
	init6Array( X5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);	
	init6Array( X6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);	
	init6Array( X7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);	
	init6Array( X8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);		// Output of Block 3
	init6Array( X9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);	
	init6Array( X10, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);	// Output of Block 4


	bin_ceiling := 350.0;	// previously used 215, increased to be encompass high flicker levels
	number_bins := 16000;

  {Allocate Memory for bins}
	Bins0 := Allocmem(Sizeof(Bins0^[0])*number_bins); // 50k is max. # of bins
	Bins1 := Allocmem(Sizeof(Bins1^[0])*number_bins); // 50k is max. # of bins

	time        := 0;		// time clock
	Pst_Timer   := 0.0;
	max_flicker := 0;		//maximum instantaneous flicker

	ZeroOutBins;

	if (input_type = 0) Then Tstep := DeltaT  // if input_type is AC, then internal timestep = DeltaT
	                    else Tstep := 1.0/(16*Fbase);	// time step for each cycle, fixed to 16 samples/cycle

	Pst_Time := Npts*DeltaT - 6.0 ;  // -6.0;	//use the entire data set sent to calculate the flicker

	Set_Filter_Coefficients(input_type);

	fn     := 0;

	////*********************************************************************************
	// Main AC routine
	////*********************************************************************************
	if (input_type = 0) then
	Begin
		//first calculate what the rms input waveform
		sp := trunc(1.0/(Fbase * DeltaT));
		pV := V;  
		// Calculates single cycle rms of input	
		for N := 0 to sp-1 Do
		Begin
        Vinput := V^;
        fn := fn + Vinput*Vinput;
        inc(V); // *V++;
		End;
		rms_input := sqrt(fn/(sp-1));
		V := pV;	//rewind to beginning

    acquire_flicker := FALSE;	//flag for when to measure flicker

		while (time < 30.0) Do
		Begin
        //Generate an AC signal for input
        Vin[0] := 1.414*rms_input*sin(2*3.14159*Fbase*time);

        Get_Pinst(  pst, max_flicker);
        time := time + Tstep;
		End;

		for Vindex := 0 to  Npts-1 Do
		Begin          
        Vin[0] := V^;  //*V;  // no timestamp in file

        // let it settle down after the transition to real data, this is when we start calc flicker
        if (time >= 35.0 ) Then  acquire_flicker := TRUE;

        Get_Pinst( pst, max_flicker);
        time := time + Tstep;
        inc(V);  // *V++;	//increment pointer
		End;

	End

	// //*********************************************************************************
	// Main RMS routine
	// ///*********************************************************************************

	else
	Begin

    acquire_flicker := FALSE;	//flag for when to actually compute flicker
    SamplesPerDeltaT := DeltaT/Tstep;

{$IF Defined(DEBUG)}
{****}
{****}Writeln(Debugfile, Format('Tstep=%.8g, DeltaT=%.8g, Samples=%.8g, Pst_Time=%.8g, npts=%d ',
{****}                        [Tstep, DeltaT, SamplesPerDeltaT, Pst_Time, npts ]));
{$IFEND}

    max_flicker := 0.0;
		FirstSample := Varray^[1];
		rms_input   := FirstSample;
    RMS_sample  := FirstSample;
		while (time < 30) Do    // basically inits filter to 1 PU for 30 s
		Begin
        time := time + Tstep ;
        Get_Pinst(pst, max_flicker); // each call shift samples arrays
		End;

		for Vindex := 1 to Npts Do
		Begin
        RMS_sample := Varray^[Vindex];
        // The following loop holds the rms input samples constant over the RMS period
        for SynthesizedSamples := 1 to round(SamplesPerDeltaT) Do
        Begin
            if (time >= 35 )Then acquire_flicker := TRUE; // let it settle down after the transition to real data, this is when we start calc flicker
            Get_Pinst(pst, max_flicker);    // max_flicker is cumulative but dont do anything with it
            time :=  time + Tstep;
        End;
		End;
	End;

	Result :=  pst;

{$IF Defined(DEBUG)}
{****}CloseFile(DebugFile);
{$IFEND}

	reallocmem(Bins0,0);
	reallocmem(Bins1,0);

End;

// Function call for executing PST calculator using AC data

Function  PstRMS(pVoltages:pDoubleArray; Freqbase:double; NcyclesperSample,  Npts,  Lamp:Integer):double;

Begin

  Fbase := Freqbase;

	// lamp_type  := 0;			// 0 for 120V filters, 1 for 230V filters
	input_type := 6;			// 0 for AC, 1 for 1-cycle RMS, 6 for 6-cycle rms

	//Check for the lamp type (120 or 230), default to 120 if not read properly
	if (Lamp = 230) then Begin lamp_type := 1; End
	                else Begin lamp_type := 0; End;
	DeltaT := NcyclesperSample/Fbase;

	Result   := _Pst(pVoltages,  Npts);

End;

// Function call for executing PST calculator using RMS data  

Function  PstAC(pVoltages:pDoubleArray; Delta_T:double; Npts, Lamp:Integer):Double;


Begin

	// lamp_type := 0;			// 0 for 120V filters, 1 for 230V filters
	input_type := 0;			// 0 for AC, 1 for 1-cycle RMS, 6 for 6-cycle rms
	Fbase      := 60.0;			//not needed for AC signal input

  DeltaT := Delta_T;

	//Check for the lamp type (120 or 230), default to 120 if not read properly
	if (Lamp = 230) Then Begin lamp_type := 1; End
	                else Begin lamp_type := 0; End;

	Result := _Pst(pVoltages, Npts);

End;

initialization
        staticfn := 0.0;
        staticN  := 0;
        FirstTime1 := TRUE;

End.