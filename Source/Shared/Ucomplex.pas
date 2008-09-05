Unit Ucomplex;

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

  function cmplx(const a,b:Double):complex;
  function cinv(const A:COMPLEX):COMPLEX;
  function cabs(const a:complex):double;
  function cang(const a:complex):double;
  function cdang(const a:complex):double; // angle of complex number, degrees
  function ctopolar(const a:complex):polar;
  function ctopolardeg(const a:complex):polar;  // complex to polar, degrees
  function cadd(const a,b:complex):complex;
  procedure caccum(Var a:complex;const  b:complex); {a := a + b}
  function csub(const a,b:complex):complex;
  function cmul(const a,b:complex):complex;
  Procedure caccumarray(a,b:pComplexArray; N:SmallInt);
  function cmulreal(const a:complex;const b:Double):Complex;  { := a*b }
  Procedure cmulrealaccum(Var  a:complex;const b:Double); { a=a*b}
  function cdiv(const a,b:complex):complex;
  function cdivreal(const a:complex;const b:Double):Complex;  { := a /b}
  function conjg(const a:complex):complex;
  function cnegate(const a:complex):complex;
  function csqrt(const a:complex):complex;
  function cln(const a:complex):complex;
  function topolar(const a,b:Double):polar;   // scalar to polar
  function prel(const a:polar):double;  // real part of polar number   |a| cos()
  function pimg(const a:polar):double;  // imag part of polar number   |a| sin()
  function ptocomplex(const a:polar):complex;
  function padd(const a,b:polar):polar;
  function psub(const a,b:polar):polar;
  function pmul(const a,b:polar):polar;
  function pdiv(const a,b:polar):polar;
  function pdegtocomplex(const magn,angle:double):complex;
  function pclx(const magn,angle:double):complex;

  VAR
    cZERO, cONE:Complex;

Implementation

  FUNCTION CMPLX;
  BEGIN
    CMPLX.RE:=A;
    CMPLX.IM:=B
  END;

  FUNCTION CInv{(A:COMPLEX):COMPLEX};
  VAR
    DNOM:Double;
  BEGIN
    DNOM:=A.RE*A.RE+A.IM*A.IM;
    CINV.RE:=A.RE/DNOM;
    CINV.IM:=(-A.IM)/DNOM
  END;

  FUNCTION Cabs{(A:COMPLEX):REAL};
  BEGIN
    Cabs:=SQRT(A.RE*A.RE+A.IM*A.IM)
  END;

  FUNCTION Conjg{(A:Complex):Complex};
  BEGIN
      Conjg.RE := A.RE;
      Conjg.im := -A.im;
  END;
  
  FUNCTION ATAN2 (x, iy : double) : double ;
  CONST
    PI=3.14159; { 180 DEGREES }
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
  
  FUNCTION CANG{(A:COMPLEX):REAL};     
  BEGIN
    CANG:=ATAN2(A.RE,A.IM)
  END;

  FUNCTION CDANG{(A:COMPLEX):REAL};     
  BEGIN
    CDANG:=ATAN2(A.RE,A.IM)*57.29577951;
  END;
  
  FUNCTION CtoPOLAR{(A:COMPLEX):POLAR};
  BEGIN
    With Result Do  Begin
      MAG:=Cabs(A);
      ANG:=CANG(A)
    End;
  END;

  FUNCTION CtoPOLARdeg{(A:COMPLEX):POLAR};
  BEGIN
    With Result Do  Begin
      MAG:=Cabs(A);
      ANG:=CDANG(A)
    End;
  END;

  FUNCTION CADD{(A,B:COMPLEX):COMPLEX};
  BEGIN
    CADD.RE:=A.RE+B.RE;
    CADD.IM:=A.IM+B.IM
  END;

  PROCEDURE caccum(Var a:complex; const b:complex);
  BEGIN
      a.re := a.re + b.re;
      a.im := a.im + b.im;
  END;

  Procedure caccumarray(a,b:pComplexArray; N:SmallInt);
  Var i:Integer;
  BEGIN
       For i := 1 to N Do Begin
           a^[i].re := a^[i].re + b^[i].re;
           a^[i].im := a^[i].im + b^[i].im;
       End;
  END;


  FUNCTION CSUB{(A,B:COMPLEX):COMPLEX};
  BEGIN
    CSUB.RE:=A.RE-B.RE;
    CSUB.IM:=A.IM-B.IM
  END;
      
  FUNCTION CMUL{(A,B:COMPLEX):COMPLEX};
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

  FUNCTION CDIV{(A,B:COMPLEX):COMPLEX};
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

  FUNCTION cnegate{(a:complex):complex};

  BEGIN
      cnegate.re := -a.re;
      cnegate.im := -a.im;
  END;

  FUNCTION csqrt(const a:complex):complex;
    VAR x:Polar;
  BEGIN
      // algorithm: sqrt of magnitude/ half the angle
      x := ctopolar(A);
      Result := ptocomplex(topolar(sqrt(x.mag),x.ang/2.0));
  END;

  FUNCTION cln(const a:complex):complex;
    VAR x:Polar;
  BEGIN
        // algorithm: ln of mag + j(angle), radians
      x := ctopolar(A);
      Result := cmplx(ln(x.mag), x.ang);
  END;

  FUNCTION toPOLaR{(A,B:REAL):POLAR};
  BEGIN
    With Result Do Begin
      MAG:=A;
      ANG:=B ;
    End;
  END;

  FUNCTION PREL{(A:POLAR):REAL};
  BEGIN
    PREL:=A.MAG * COS(A.ANG)
  END;

  FUNCTION PIMG{(A:POLAR):REAL};
  BEGIN
    PIMG:=A.MAG * SIN(A.ANG)
  END;
  
  FUNCTION PCLX{(Magn,Angle:Real):COMPLEX};
  Begin
    PCLX.RE:=Magn*Cos(Angle);
    PCLX.IM:=Magn*Sin(Angle);
  End;

  FUNCTION PDEGtoCompLeX{(Magn,Angle:Real):COMPLEX};
  VAR
     Ang:Double;
  Begin
    Ang:=Angle/57.29577951;
    With Result Do Begin
      RE:=Magn*Cos(Ang);
      IM:=Magn*Sin(Ang);
    End;
  End;

  FUNCTION PtoCOMPLEX{(A:POLAR):COMPLEX};
  BEGIN
    With Result Do Begin
        RE:=A.MAG * COS(A.ANG) ;
        IM:=A.MAG * SIN(A.ANG);
    End;
  END;

  FUNCTION PADD{(A,B:POLAR):POLAR};
  BEGIN
    PADD:=CtoPOLAR(CADD(PtoCOMPLEX(A),PtoCOMPLEX(B)))
  END;

  FUNCTION PSUB{(A,B:POLAR):POLAR};
  BEGIN
    PSUB:=CtoPOLAR(CSUB(PtoCOMPLEX(A),PtoCOMPLEX(B)))
  END;

  FUNCTION PMUL{(A,B:POLAR):POLAR};
  BEGIN
    PMUL.MAG:=A.MAG*B.MAG;
    PMUL.ANG:=A.ANG+B.ANG
  END;

  FUNCTION PDIV{(A,B:POLAR):POLAR};
  BEGIN
    PDIV.MAG:=A.MAG/B.MAG;
    PDIV.ANG:=A.ANG-B.ANG
  END;




Initialization

  cZERO := cmplx(0.0,0.0);
  cONE := cmplx(1.0,0.0);

End.


