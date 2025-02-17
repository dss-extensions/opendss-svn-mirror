

#pragma hdrstop

#include "Ucomplex.h"
#include "d2c_Structures.h"

using namespace std;


namespace Ucomplex
{

complex CZero = cmplx(0,0);
complex cONE = cmplx(1,0);

std::string Cmplx2Str(complex myCNumber)
{
    std::string Result = std::to_string(myCNumber.re);
    if (myCNumber.im >= 0)
        Result = Result + '+';
    Result = Result + std::to_string(myCNumber.im) + 'i';
    
	return Result;
}

complex Str2Cmplx(std::string myStr)
{
    complex			Result = CZero;
    std::string		SBuffer = "";		// Stores the substring extracted from the source
    bool			RealNum = true;		// Indicates if the string processed corresponds to the real or imaginary part
    int				TokenF = 0,			// Indicates if a Token (+, - , i, j) was found by index
					i = 0,
					k = 0,
					idx = 0;

	const char Tokens[4] = { '+', '-', 'i', 'j' };

	for (i = 0; i < myStr.size(); i++)
    {
		TokenF = -1;
        for (k = 0; k <= 3; k++)
        {
            if (myStr[i] == Tokens[k])
            {
                TokenF = k;
                break;
            }
        }

		if (TokenF < 0)
            SBuffer = SBuffer + myStr[i];
		else
		{
			switch(TokenF)
			{
				case 0:
				case 1:
				{
                    // Ideally, this is the end of the real part
					if (!SBuffer.empty())
					{
						if (RealNum)
						{
                            // If this is not the real part then something is wrong with the expression
							Result.re = std::atof(trim(SBuffer).c_str());
                            RealNum = false;
							if (TokenF == 1)
                                SBuffer = "-";    // Adding the (-) char for negative numbers
							else
								SBuffer = "";

						}
					}
					else
					{
                        if (TokenF == 1)
                            SBuffer = "-"; // Adding the (-) char for negative numbers
					}
				}
				break;
                case 2:
                case 3:
                {
                    // Here we've found the imaginary part
                    if (!SBuffer.empty())
                    {
                        if (RealNum)
                        {
                            Result.re = 0;
                            RealNum = false;
                        }
                        Result.im = std::atof(trim(SBuffer).c_str());
                        SBuffer = "";
                    }
				}
                break;
				default:
				{
					// Nothing really
				}
			}
		}
    }

	return Result;
}

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




