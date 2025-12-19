#ifndef RecloserH
#define RecloserH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlActionsDefs.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "TCC_Curve.h"
#include <math.h>
#include "d2c_structures.h"

namespace Recloser
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
    Created 11-1-00 from Relay Control


*/
/*
  A Recloser is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  CktElement to be controlled must already exist.

  7-18-2002  Fixed typos in help
  5-1-2006  Added Time Delays to be compatible with relays

*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

const int RECLOSERCONTROLMAXDIM = 6;
typedef EControlAction StateArray[6 /*# range 1..RECLOSERCONTROLMAXDIM*/];
typedef StateArray* pStateArray; // 0 = open 1 = close

class TRecloser : public ControlClass::TControlClass
{
	friend class TRecloserObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String RecloserName);
public:
	TRecloser();
	virtual ~TRecloser();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TRecloserObj : public ControlElem::TControlElem
{
	friend class TRecloser;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
	TCC_Curve::TTCC_CurveObj* PhSlowCurve;
	TCC_Curve::TTCC_CurveObj* GndSlowCurve;
	TCC_Curve::TTCC_CurveObj* PhFastCurve;
	TCC_Curve::TTCC_CurveObj* GndFastCurve;
	double ResetTime;
	double MechanicalDelay;
	double TDGndSlow;
	double TDPhSlow;
	double TDGndFast;
	double TDPhFast;

	pStateArray FPresentState;
	pStateArray FNormalState;
	pIntegerArray OperationCount;

	BooleanArray LockedOut;
	BooleanArray ArmedForClose;
	BooleanArray ArmedForOpen;
	BooleanArray PhaseTarget;
	bool GroundTarget;
    int IdxMultiPh; // Index used for accessing arrays for multi-phase, ganged operation

	pStringArray RecloserTarget;

	bool NormalStateSet;
    bool SinglePhTrip;
    bool SinglePhLockout;
    bool FLocked;

	int CondOffset; // Offset for monitored terminal
	Ucomplex::pComplexArray cBuffer;    // Complexarray buffer

	bool DebugTrace;

	void InterpretRecloserState(int ActorID, const String param, const String property_name);
	EControlAction get_States(int Idx);
	void set_States(int Idx, const EControlAction Value);
	EControlAction get_NormalStates(int Idx);
	void set_NormalStates(int Idx, const EControlAction Value);

	void set_Flocked(bool Value);

public:
	Arraydef::pDoubleArray RecloseIntervals;
	int NumFast;
	int NumReclose;
	String MonitoredElementName;
	int MonitoredElementTerminal;
	double PhFastPickup;
	double GndFastPickup;
	double PhSlowPickup;
	double GndSlowPickup;
	double PhInst;
	double GndInst;
	double RatedCurrent;
	double InterruptingRating;
	TRecloserObj(DSSClass::TDSSClass* ParClass, const String RecloserName);
	virtual ~TRecloserObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a Recloser
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

	bool get_FLocked();

	TRecloserObj(DSSClass::TDSSClass* ParClass);
	TRecloserObj(String ClassName);
	TRecloserObj();
};
extern TRecloserObj* ActiveRecloserObj;
extern TRecloser* RecloserClass;


/*--------------------------------------------------------------------------*/


}  // namespace Recloser

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Recloser;
#endif

#endif // RecloserH





