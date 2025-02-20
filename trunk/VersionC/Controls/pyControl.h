#ifndef pyControlH
#define pyControlH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "PointerList.h"
#include "d2c_structures.h"

namespace pyControl
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2025, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
  The pyControl is n interface for controllers developped in Python.
  THis structure follows the black box operational principle,
  where it is expected for the Python code to consider some basic
  structure to be compatible with control operations. 
*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TpyControl : public ControlClass::TControlClass
{
	friend class TpyControlObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String pyControlName);
public:
	TpyControl();
	virtual ~TpyControl();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TpyControlObj : public ControlElem::TControlElem
{
	friend class TpyControl;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
    String	pyScript,
			LastCMD;

public:
	TpyControlObj(DSSClass::TDSSClass* ParClass, const String pyControlName);
	virtual ~TpyControlObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a pyControl
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
    virtual int HandlePIPE(int ActorID);


      // Public properties
	TpyControlObj(DSSClass::TDSSClass* ParClass);
	TpyControlObj(String ClassName);
	TpyControlObj();
};
extern TpyControlObj* ActivepyControlObj;

/*--------------------------------------------------------------------------*/


}  // namespace pyControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace pyControl;
#endif

#endif // pyControlH





