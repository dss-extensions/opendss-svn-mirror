#ifndef SwtControlH
#define SwtControlH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "ControlActionsDefs.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "d2c_structures.h"
#include <math.h>

namespace SwtControl
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------*/
const int SWTCONTROLMAXDIM = 6;
typedef EControlAction StateArray[6 /*# range 1..SWTCONTROLMAXDIM*/];
typedef StateArray* pStateArray; // 0 = open 1 = close

class TSwtControl : public ControlClass::TControlClass
{
	friend class TSwtControlObj;
public:
	typedef ControlClass::TControlClass inherited;	
protected:
	void DefineProperties();
	virtual int MakeLike(const String SwtControlName);
public:
	TSwtControl();
	virtual ~TSwtControl();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

class TSwtControlObj : public ControlElem::TControlElem
{
	friend class TSwtControl;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
	pStateArray FPresentState;
	pStateArray FNormalState;
	//EControlAction LockCommand;
	bool FLocked;

	bool NormalStateSet;

	void InterpretSwitchState(int ActorID, const String Param, const String property_name);
	EControlAction get_States(int Idx);
	void set_States(int Idx, const EControlAction Value);
	EControlAction get_NormalStates(int Idx);
	void set_NormalStates(int Idx, const EControlAction Value);

	void set_Flocked(bool Value);

public:
	TSwtControlObj(DSSClass::TDSSClass* ParClass, const String SwtControlName);
	virtual ~TSwtControlObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a SwtControl
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

	//EControlAction get_FNormalState();
	//EControlAction get_FPresentState();
	bool get_FLocked();
	//EControlAction get_ActionCommand();

	TSwtControlObj(DSSClass::TDSSClass* ParClass);
	TSwtControlObj(String ClassName);
	TSwtControlObj();
};
extern TSwtControlObj* ActiveSwtControlObj;

/*--------------------------------------------------------------------------*/


}  // namespace SwtControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace SwtControl;
#endif

#endif // SwtControlH





