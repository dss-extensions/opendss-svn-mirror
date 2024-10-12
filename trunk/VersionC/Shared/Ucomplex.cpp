

#pragma hdrstop

#include "Ucomplex.h"

using namespace std;


namespace Ucomplex
{

complex CZero = cmplx(0,0);
complex cONE = cmplx(1,0);

void Ucomplex_initialization()
{
	CZero = cmplx(0.0, 0.0);
	cONE = cmplx(1.0, 0.0);
}

		class 		Ucomplex_unit
		{
		public:
		Ucomplex_unit()
		{
			//AssertSystemInitialization();
			Ucomplex_initialization();
		}
		};
		Ucomplex_unit _Ucomplex_unit;

}  // namespace Ucomplex




