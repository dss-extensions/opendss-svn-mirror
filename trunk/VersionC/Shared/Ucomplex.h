#ifndef UcomplexH
#define UcomplexH

#include "System.h"
#include "Sysutils.h"
#include <cmath>


namespace Ucomplex
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

struct complex
{
	double re;
	double im;
};

typedef complex* pcomplex;
typedef complex* ComplexArray;
typedef ComplexArray pComplexArray;

struct polar
{
	double mag;
	double ang;
};

extern complex CZero;
extern complex cONE;

inline complex cmplx(double A, double B)
{
	complex result = {};
	result.re = A;
	result.im = B;
	return result;
}

inline complex cinv(const complex& A)
{
	complex result = {};
	double DNOM = 0.0;
	DNOM = A.re * A.re + A.im * A.im;
	result.re = A.re / DNOM;
	result.im = (-A.im) / DNOM;
	return result;
}

inline double cabs(const complex& A)
{
	double result = 0.0;
	result = sqrt(A.re * A.re + A.im * A.im);
	return result;
}

inline double cabs2(const complex& A)
{
	double result = 0.0;
	result = A.re * A.re + A.im * A.im;
	return result;
}

inline complex conjg(const complex& A)
{
	complex result = {};
	result.re = A.re;
	result.im = -A.im;
	return result;
}

inline double ATAN2(double X, double IY)
{
	double result = 0.0;
	const double Pi = 3.14159265359; /* 180 DEGREES */
	if((X < 0.0) && (IY >= 0))
		result = atan(IY / X) + Pi;
	else
	{
		if((X < 0.0) && (IY < 0))
			result = atan(IY / X) - Pi;
		else
		{
			if(X > 0.0)
				result = atan(IY / X);
			else
			{
				if(IY < 0.0)
					result = -Pi / 2;
				else
				{
					if(IY > 0.0)
						result = Pi / 2;
					else
						result = 0.0;
				}
			}
		}
	}
	return result;
} /* ATAN2 */

inline double cang(const complex& A)
{
	double result = 0.0;
	result = ATAN2(A.re, A.im);
	return result;
}

inline double cdang(const complex& A)
{
	double result = 0.0;
	result = ATAN2(A.re, A.im) * 57.29577951;
	return result;
}

inline polar ctopolar(const complex& A)
{
	polar result = {};
	/*# with result do */
	{
		auto& with0 = result;
		with0.mag = cabs(A);
		with0.ang = cang(A);
	}
	return result;
}

inline polar ctopolardeg(const complex& A)
{
	polar result = {};
	/*# with result do */
	{
		auto& with0 = result;
		with0.mag = cabs(A);
		with0.ang = cdang(A);
	}
	return result;
}

inline complex cadd(const complex& A, const complex& B)
{
	complex result = {};
	result.re = A.re + B.re;
	result.im = A.im + B.im;
	return result;
}

inline void caccum(complex& A, const complex& B)
{
	A.re = A.re + B.re;
	A.im = A.im + B.im;
}

inline void caccumarray(pComplexArray A, pComplexArray B, short int n)
{
	int i = 0;
	int stop = 0;
	for(stop = n, i = 1; i <= stop; i++)
	{
		(A)[i - 1].re = (A)[i - 1].re + (B)[i - 1].re;
		(A)[i - 1].im = (A)[i - 1].im + (B)[i - 1].im;
	}
}

inline complex csub(const complex& A, const complex& B)
{
	complex result = {};
	result.re = A.re - B.re;
	result.im = A.im - B.im;
	return result;
}

inline complex cmul(const complex& A, const complex& B)
{
	complex result = {};
	result.re = A.re * B.re - A.im * B.im;
	result.im = A.re * B.im + A.im * B.re;
	return result;
}

inline complex cmulreal(const complex& A, double B)
{
	complex result = {};
	result.re = A.re * B;
	result.im = A.im * B;
	return result;
}  /* := a*b */

inline void cmulrealaccum(complex& A, double B)
{
	A.re = A.re * B;
	A.im = A.im * B;
} /* a=a*b*/

inline  complex cdiv(const complex& A, const complex& B)
{
	complex result = {};
	double DNOM = 0.0;
	DNOM = B.re * B.re + B.im * B.im;
	result.re = (A.re * B.re + A.im * B.im) / DNOM;
	result.im = (A.im * B.re - A.re * B.im) / DNOM;
	return result;
}

inline complex cdivreal(const complex& A, double B)
{
	complex result = {};
	result.re = A.re / B;
	result.im = A.im / B;
	return result;
}  /* := a /b*/

inline complex cnegate(const complex& A)
{
	complex result = {};
	result.re = -A.re;
	result.im = -A.im;
	return result;
}

inline complex ptocomplex(const polar& A)
{
	complex result = {};
	/*# with result do */
	{
		auto& with0 = result;
		with0.re = A.mag * cos(A.ang);
		with0.im = A.mag * sin(A.ang);
	}
	return result;
}

inline polar topolar(double A, double B)
{
	polar result = {};
	/*# with result do */
	{
		auto& with0 = result;
		with0.mag = A;
		with0.ang = B;
	}
	return result;
}

inline complex csqrt(const complex& A)
{
	complex result = {};
	polar X = {};
      // algorithm: sqrt of magnitude/ half the angle
	X = ctopolar(A);
	result = ptocomplex(topolar(sqrt(X.mag), X.ang / 2.0));
	return result;
}

inline complex CLn(const complex& A)
{
	complex result = {};
	polar X = {};
        // algorithm: ln of mag + j(angle), radians
	X = ctopolar(A);
	result = cmplx(log(X.mag), X.ang);
	return result;
}

inline double prel(const polar& A)
{
	double result = 0.0;
	result = A.mag * cos(A.ang);
	return result;
}

inline double pimg(const polar& A)
{
	double result = 0.0;
	result = A.mag * sin(A.ang);
	return result;
}

inline complex pclx(double magn, double Angle)
{
	complex result = {};
	result.re = magn * cos(Angle);
	result.im = magn * sin(Angle);
	return result;
}

inline complex pdegtocomplex(double magn, double Angle)
{
	complex result = {};
	double ang = 0.0;
	ang = Angle / 57.29577951;
	/*# with result do */
	{
		auto& with0 = result;
		with0.re = magn * cos(ang);
		with0.im = magn * sin(ang);
	}
	return result;
}

inline polar padd(const polar& A, const polar& B)
{
	polar result = {};
	result = ctopolar(cadd(ptocomplex(A), ptocomplex(B)));
	return result;
}

inline polar psub(const polar& A, const polar& B)
{
	polar result = {};
	result = ctopolar(csub(ptocomplex(A), ptocomplex(B)));
	return result;
}

inline polar pmul(const polar& A, const polar& B)
{
	polar result = {};
	result.mag = A.mag * B.mag;
	result.ang = A.ang + B.ang;
	return result;
}

inline polar pdiv(const polar& A, const polar& B)
{
	polar result = {};
	result.mag = A.mag / B.mag;
	result.ang = A.ang - B.ang;
	return result;
}

}  // namespace Ucomplex

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Ucomplex;
#endif

#endif // UcomplexH




