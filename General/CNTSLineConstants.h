#ifndef CNTSLineConstantsH
#define CNTSLineConstantsH

#include "System.h"
#include "Sysutils.h"
#include <math.h>

#include "Arraydef.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LineUnits.h"
#include "LineConstants.h"
#include "CableConstants.h"

namespace CNTSLineConstants
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TCNTSLineConstants : public CableConstants::TCableConstants
{
public:
	typedef CableConstants::TCableConstants inherited;	
//private:
        // For All
        Arraydef::pIntegerArray FCondType; // Use as 1: CN, 2: TS, 3: Bare wire

        // For CN
	Arraydef::pIntegerArray FkStrand;
	Arraydef::pDoubleArray FDiaStrand;
	Arraydef::pDoubleArray FGmrStrand;
	Arraydef::pDoubleArray FRStrand;
	BooleanArray FSemicon;

        // For TS
        Arraydef::pDoubleArray FDiaShield;
        Arraydef::pDoubleArray FTapeLayer;
        Arraydef::pDoubleArray FTapeLap;

        // For All
        int Get_CondType(int i);

	// For CN
        int Get_kStrand(int i);
	double Get_DiaStrand(int i, int Units);
	double Get_GmrStrand(int i, int Units);
	double Get_RStrand(int i, int Units);
        bool Get_Semicon(int i);

        // For TS
        double Get_DiaShield(int i, int Units);
        double Get_TapeLayer(int i, int Units);
        double Get_TapeLap(int i);

        // For All
        void Set_CondType(int i, int Value);

        // For CN
        void Set_kStrand(int i, int Value);
	void Set_DiaStrand(int i, int Units, double Value);
	void Set_GmrStrand(int i, int Units, double Value);
	void Set_RStrand(int i, int Units, double Value);
	void Set_Semicon(int i, bool Value);

        // For TS
        void Set_DiaShield(int i, int Units, double Value);
        void Set_TapeLayer(int i, int Units, double Value);
        void Set_TapeLap(int i, double Value);
protected:
public:
	virtual void Calc(double f);
	TCNTSLineConstants(int NumConductors);
	virtual ~TCNTSLineConstants();
	TCNTSLineConstants();
};


}  // namespace CNLineConstants

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CNTSLineConstants;
#endif

#endif // CNLineConstantsH





