Unit Ucomplex;
{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

  type
    pcomplex = ^complex;
    complex=record
              re,im:Double;
            end;
    pComplexArray = ^ComplexArray;
    ComplexArray = Array [1..10] of Complex;

    polar=record
            mag,ang:Double;
          end;

  Function cmplx(const a,b:Double):complex;
  Function cinv(const A:COMPLEX):COMPLEX;
  Function cabs(const a:complex):double;
  Function cang(const a:complex):double;
  Function cdang(const a:complex):double; // angle of complex number, degrees
  Function ctopolar(const a:complex):polar;
  Function ctopolardeg(const a:complex):polar;  // complex to polar, degrees
  Function cadd(const a,b:complex):complex;
  procedure caccum(Var a:complex;const  b:complex); {a := a + b}
  Function csub(const a,b:complex):complex;
  Function cmul(const a,b:complex):complex;
  Procedure caccumarray(a,b:pComplexArray; N:SmallInt);
  Function cmulreal(const a:complex;const b:Double):Complex;  { := a*b }
  Procedure cmulrealaccum(Var  a:complex;const b:Double); { a=a*b}
  Function cdiv(const a,b:complex):complex;
  Function cdivreal(const a:complex;const b:Double):Complex;  { := a /b}
  Function conjg(const a:complex):complex;
  Function cnegate(const a:complex):complex;
  Function csqrt(const a:complex):complex;
  Function cln(const a:complex):complex;
  Function topolar(const a,b:Double):polar;   // scalar to polar
  Function prel(const a:polar):double;  // real part of polar number   |a| cos()
  Function pimg(const a:polar):double;  // imag part of polar number   |a| sin()
  Function ptocomplex(const a:polar):complex;
  Function padd(const a,b:polar):polar;
  Function psub(const a,b:polar):polar;
  Function pmul(const a,b:polar):polar;
  Function pdiv(const a,b:polar):polar;
  Function pdegtocomplex(const magn,angle:double):complex;
  Function pclx(const magn,angle:double):complex;

  VAR
    cZERO, cONE:Complex;

Implementation

  Function CMPLX;
  BEGIN
    CMPLX.RE:=A;
    CMPLX.IM:=B
  END;

  Function CInv{(A:COMPLEX):COMPLEX};
  VAR
    DNOM:Double;
  BEGIN
    DNOM:=A.RE*A.RE+A.IM*A.IM;
    CINV.RE:=A.RE/DNOM;
    CINV.IM:=(-A.IM)/DNOM
  END;

  Function Cabs{(A:COMPLEX):REAL};
  BEGIN
    Cabs:=SQRT(A.RE*A.RE+A.IM*A.IM)
  END;

  Function Conjg{(A:Complex):Complex};
  BEGIN
      Conjg.RE := A.RE;
      Conjg.im := -A.im;
  END;
  
  Function ATAN2 (x, iy : double) : double ;
  CONST
    PI=3.14159265359; { 180 DEGREES }
  BEGIN         
    if       (x < 0.0) and (iy >= 0 )
       then ATAN2 := arctan(iy/x) + PI
    else if (x < 0.0) and (iy < 0 )
       then ATAN2 := arctan(iy/x) -PI
    else if (x > 0.0)
       then ATAN2 := arctan(iy/x)
    else if (iy < 0.0)
       then ATAN2 := -PI/2
    else if (iy > 0.0)
       then ATAN2 := PI/2
    else ATAN2 := 0.0
  END; { ATAN2 }
  
  Function CANG{(A:COMPLEX):REAL};     
  BEGIN
    CANG:=ATAN2(A.RE,A.IM)
  END;

  Function CDANG{(A:COMPLEX):REAL};     
  BEGIN
    CDANG:=ATAN2(A.RE,A.IM)*57.29577951;
  END;
  
  Function CtoPOLAR{(A:COMPLEX):POLAR};
  BEGIN
    With Result Do  Begin
      MAG:=Cabs(A);
      ANG:=CANG(A)
    End;
  END;

  Function CtoPOLARdeg{(A:COMPLEX):POLAR};
  BEGIN
    With Result Do  Begin
      MAG:=Cabs(A);
      ANG:=CDANG(A)
    End;
  END;

  Function CADD{(A,B:COMPLEX):COMPLEX};
  BEGIN
    CADD.RE:=A.RE+B.RE;
    CADD.IM:=A.IM+B.IM
  END;

  PROCEDURE CACCUM(Var a:complex; const b:complex);
  BEGIN
      a.re := a.re + b.re;
      a.im := a.im + b.im;
  END;

  Procedure CACCUMARRAY(a,b:pComplexArray; N:SmallInt);
  Var i:Integer;
  BEGIN
       For i := 1 to N Do Begin
           a^[i].re := a^[i].re + b^[i].re;
           a^[i].im := a^[i].im + b^[i].im;
       End;
  END;


  Function CSUB{(A,B:COMPLEX):COMPLEX};
  BEGIN
    CSUB.RE:=A.RE-B.RE;
    CSUB.IM:=A.IM-B.IM
  END;
      
  Function CMUL{(A,B:COMPLEX):COMPLEX};
  BEGIN
    CMUL.RE:=A.RE*B.RE-A.IM*B.IM;
    CMUL.IM:=A.RE*B.IM+A.IM*B.RE
  END;

  function cmulreal(const a:complex;const b:Double):Complex;  { := a*b }
  Begin
      cmulreal.re := a.re * b;
      cmulreal.im := a.im * b;
  End;

  Procedure cmulrealaccum(Var a:complex;const b:Double); { a=a*b}
  Begin
      a.re := a.re * b;
      a.im := a.im * b;
  End;

  Function CDIV{(A,B:COMPLEX):COMPLEX};
  VAR
    DNOM:double;
  BEGIN
    DNOM:=B.RE*B.RE+B.IM*B.IM;
    CDIV.RE:=(A.RE*B.RE+A.IM*B.IM)/DNOM;
    CDIV.IM:=(A.IM*B.RE-A.RE*B.IM)/DNOM
  END;

  function cdivreal(const a:complex;const b:Double):Complex;  { := a /b}
  Begin
      cdivreal.re := a.re / b;
      cdivreal.im := a.im / b;
  End;

  Function cnegate{(a:complex):complex};

  BEGIN
      cnegate.re := -a.re;
      cnegate.im := -a.im;
  END;

  Function csqrt(const a:complex):complex;
    VAR x:Polar;
  BEGIN
      // algorithm: sqrt of magnitude/ half the angle
      x := ctopolar(A);
      Result := ptocomplex(topolar(sqrt(x.mag),x.ang/2.0));
  END;

  Function cln(const a:complex):complex;
    VAR x:Polar;
  BEGIN
        // algorithm: ln of mag + j(angle), radians
      x := ctopolar(A);
      Result := cmplx(ln(x.mag), x.ang);
  END;

  Function toPOLaR{(A,B:REAL):POLAR};
  BEGIN
    With Result Do Begin
      MAG:=A;
      ANG:=B ;
    End;
  END;

  Function PREL{(A:POLAR):REAL};
  BEGIN
    PREL:=A.MAG * COS(A.ANG)
  END;

  Function PIMG{(A:POLAR):REAL};
  BEGIN
    PIMG:=A.MAG * SIN(A.ANG)
  END;
  
  Function PCLX{(Magn,Angle:Real):COMPLEX};
  Begin
    PCLX.RE:=Magn*Cos(Angle);
    PCLX.IM:=Magn*Sin(Angle);
  End;

  Function PDEGtoCompLeX{(Magn,Angle:Real):COMPLEX};
  VAR
     Ang:Double;
  Begin
    Ang:=Angle/57.29577951;
    With Result Do Begin
      RE:=Magn*Cos(Ang);
      IM:=Magn*Sin(Ang);
    End;
  End;

  Function PtoCOMPLEX{(A:POLAR):COMPLEX};
  BEGIN
    With Result Do Begin
        RE:=A.MAG * COS(A.ANG) ;
        IM:=A.MAG * SIN(A.ANG);
    End;
  END;

  Function PADD{(A,B:POLAR):POLAR};
  BEGIN
    PADD:=CtoPOLAR(CADD(PtoCOMPLEX(A),PtoCOMPLEX(B)))
  END;

  Function PSUB{(A,B:POLAR):POLAR};
  BEGIN
    PSUB:=CtoPOLAR(CSUB(PtoCOMPLEX(A),PtoCOMPLEX(B)))
  END;

  Function PMUL{(A,B:POLAR):POLAR};
  BEGIN
    PMUL.MAG:=A.MAG*B.MAG;
    PMUL.ANG:=A.ANG+B.ANG
  END;

  Function PDIV{(A,B:POLAR):POLAR};
  BEGIN
    PDIV.MAG:=A.MAG/B.MAG;
    PDIV.ANG:=A.ANG-B.ANG
  END;




Initialization

  cZERO := cmplx(0.0, 0.0);
  cONE  := cmplx(1.0, 0.0);

End.


