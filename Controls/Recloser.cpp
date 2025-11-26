
#pragma hdrstop

#include "Recloser.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Ucmatrix.h"
#include "mathutil.h"
#include "Utilities.h"

using namespace std;
using namespace Arraydef;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace ControlClass;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace ParserDel;
using namespace System;
using namespace TCC_Curve;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace Recloser
{

TRecloserObj::TRecloserObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TRecloserObj::TRecloserObj(String ClassName) : inherited(ClassName) {}
TRecloserObj::TRecloserObj() {}


TRecloserObj* ActiveRecloserObj = nullptr;
TRecloser* RecloserClass = nullptr;
const int NumPropsThisClass = 30;
const int Current = 0;  /*Default*/
const int VOLTAGE = 1;
const int REVPOWER = 3;
TDSSClass* TCC_CurveClass = nullptr;

/*General Module Function*/

TTCC_CurveObj* GetTccCurve(const String CurveName)
{
    TTCC_CurveObj* result = nullptr;

    if (LowerCase(CurveName) == "none")
    {
        return result;
    }

	result = ((TTCC_CurveObj*) TCC_CurveClass->Find(CurveName));
	if(result == nullptr)
		DoSimpleMsg(String("TCC Curve object: \"") + CurveName + "\" not found.", 388);
	return result;
}


/*--------------------------------------------------------------------------*/  // Creates superstructure for all Recloser objects

TRecloser::TRecloser()
{
	;
	Class_Name = "Recloser";
	DSSClassType = DSSClassType + RECLOSER_CONTROL;
	DefineProperties();
	auto&& slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc.data(), NumProperties);
	CommandList.set_AbbrevAllowed(true);
	TCC_CurveClass = (TDSSClass*)GetDSSClassPtr("TCC_Curve");
	RecloserClass = this;
}

/*--------------------------------------------------------------------------*/

TRecloser::~TRecloser()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TRecloser::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "MonitoredObj";
	PropertyName[2 - 1] = "MonitoredTerm";
	PropertyName[3 - 1] = "SwitchedObj";
	PropertyName[4 - 1] = "SwitchedTerm";
	PropertyName[5 - 1] = "NumFast";
	PropertyName[6 - 1] = "PhaseFast";
	PropertyName[7 - 1] = "PhaseDelayed";
	PropertyName[8 - 1] = "GroundFast";
	PropertyName[9 - 1] = "GroundDelayed";
	PropertyName[10 - 1] = "PhaseTrip";
	PropertyName[11 - 1] = "GroundTrip";
	PropertyName[12 - 1] = "PhaseInst";
	PropertyName[13 - 1] = "GroundInst";
	PropertyName[14 - 1] = "ResetTime";
	PropertyName[15 - 1] = "Shots";
	PropertyName[16 - 1] = "RecloseIntervals";
	PropertyName[17 - 1] = "Delay";
	PropertyName[18 - 1] = "Action";
	PropertyName[19 - 1] = "TDPhFast";
	PropertyName[20 - 1] = "TDGrFast";
	PropertyName[21 - 1] = "TDPhDelayed";
	PropertyName[22 - 1] = "TDGrDelayed";
	PropertyName[23 - 1] = "Normal";
	PropertyName[24 - 1] = "State";
	PropertyName[25 - 1] = "SinglePhTrip";
    PropertyName[26 - 1] = "SinglePhLockout";
    PropertyName[27 - 1] = "Lock";
    PropertyName[28 - 1] = "Reset";
    PropertyName[29 - 1] = "EventLog";
    PropertyName[30 - 1] = "DebugTrace";


	PropertyHelp[1 - 1] = "Full object name of the circuit element, typically a line, transformer, load, or generator, "
	           "to which the Recloser's PT and/or CT are connected."
	           " This is the \"monitored\" element. "
	           "There is no default; must be specified.";
	PropertyHelp[2 - 1] = "Number of the terminal of the circuit element to which the Recloser is connected. "
	           "1 or 2, typically.  Default is 1.";
	PropertyHelp[3 - 1] = "Name of circuit element switch that the Recloser controls. "
	           "Specify the full object name."
	           "Defaults to the same as the Monitored element. "
	           "This is the \"controlled\" element.";
	PropertyHelp[4 - 1] = "Number of the terminal of the controlled element in which the switch is controlled by the Recloser. "
	           "1 or 2, typically.  Default is 1.";
	PropertyHelp[5 - 1] = "Number of Fast (fuse saving) operations.  Default is 1. (See \"Shots\")";
	PropertyHelp[6 - 1] = "Name of the TCC Curve object that determines the Phase Fast trip. Must have been previously defined as a TCC_Curve object or specified as \"none\" (ignored). "
	           " Default is \"none\". "
	           "Multiplying the current values in the curve by the \"phasetrip\" value gives the actual current.";
	PropertyHelp[7 - 1] = "Name of the TCC Curve object that determines the Phase Delayed trip. Must have been previously defined as a TCC_Curve object or specified as \"none\" (ignored). "
	           " Default is \"none\". "
	           "Multiplying the current values in the curve by the \"phasetrip\" value gives the actual current.";
	PropertyHelp[8 - 1] = "Name of the TCC Curve object that determines the Ground Fast trip. Must have been previously defined as a TCC_Curve object or specified as \"none\" (ignored). "
	           " Default is \"none\". "
	           "Multiplying the current values in the curve by the \"groundtrip\" value gives the actual current.";
	PropertyHelp[9 - 1] = "Name of the TCC Curve object that determines the Ground Delayed trip. Must have been previously defined as a TCC_Curve object or specified as \"none\" (ignored). "
	           " Default is \"none\". "
	           "Multiplying the current values in the curve by the \"groundtrip\" value gives the actual current.";
	PropertyHelp[10 - 1] = "Multiplier or actual phase amps for the phase TCC curve. Defaults to 1.0.";
	PropertyHelp[11 - 1] = "Multiplier or actual ground amps (3I0) for the ground TCC curve. Defaults to 1.0.";
	PropertyHelp[12 - 1] = "Actual amps for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip.";
	PropertyHelp[13 - 1] = "Actual amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip.";
	PropertyHelp[14 - 1] = "Reset time in sec for Recloser. Default is 15.";
	PropertyHelp[15 - 1] = "Total Number of fast and delayed shots to lockout.  Default is 4. This is one more than the number of reclose intervals.";
	PropertyHelp[16 - 1] = "Array of reclose intervals.  Default for Recloser is (0.5, 2.0, 2.0) seconds. "
	           "A locked out Recloser must be closed manually (action=close).";
	PropertyHelp[17 - 1] = "Fixed delay time (sec) added to Recloser trip time. Default is 0.0. Used to represent breaker time or any other delay.";
	PropertyHelp[18 - 1] = "DEPRECATED. See \"State\" property";
	PropertyHelp[19 - 1] = "Time dial for Phase Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.";
	PropertyHelp[20 - 1] = "Time dial for Ground Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.";
	PropertyHelp[21 - 1] = "Time dial for Phase Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.";
	PropertyHelp[22 - 1] = "Time dial for Ground Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.";
	PropertyHelp[23 - 1] = "ARRAY of strings {Open | Closed} representing the Normal state of the recloser in each phase of the controlled element. "
	           "The recloser reverts to this state for reset, change of mode, etc. "
			   "Defaults to \"State\" if not specifically declared.  Setting this property to {Open | Closed} sets the normal state to the specified value for all phases (ganged operation).";
	PropertyHelp[24 - 1] = "{Open | Closed} Actual state of the recloser. Upon setting, immediately forces state of the recloser, overriding the Recloser control. "
	           "Simulates manual control on recloser. Defaults to Closed. \"Open\" causes the controlled element to open and lock out. \"Closed\" causes the "
	           "controlled element to close and the recloser to reset to its first operation.";
    PropertyHelp[24 - 1] = "ARRAY of strings {Open | Closed} representing the Actual state of the recloser in each phase of the controlled element. " 
			   "Upon setting, immediately forces the state of the recloser. Simulates manual control on Recloser. Defaults to Closed for all phases. Setting this property to {Open | Closed} " 
			   "sets the actual state to the specified value for all phases (ganged operation). \"Open\" causes the controlled element or respective phase to open and lock out. \"Closed\" causes the "
			   "controlled element or respective phase to close and the recloser to reset to its first operation.";
    PropertyHelp[25 - 1] = "{Yes | No*} Enables single-phase tripping and reclosing for multi-phase controlled elements. Previously locked out phases do not operate/reclose even considering multi-phase tripping.";
    PropertyHelp[26 - 1] = "{Yes | No*} Enables single-phase lockout for multi-phase controlled elements with single-phase tripping. Does not have impact if single-phase trip is not enabled.";
    PropertyHelp[27 - 1] = "{Yes | No*} Controlled switch is locked in its present open / closed state or unlocked. " 
			   "When locked, the recloser will not respond to either a manual state change issued by the user or a state change issued internally by OpenDSS when reseting the control. "
			   "Note this locking mechanism is different from the recloser automatic lockout after specifed number of shots.";
    PropertyHelp[28 - 1] = "{Yes | No} If Yes, forces Reset of recloser to Normal state and removes Lock independently of any internal "
	           "reset command for mode change, etc.";
    PropertyHelp[29 - 1] = "{Yes/True* | No/False} Default is Yes for Recloser. Write trips, reclose and reset events to EventLog.";
    PropertyHelp[30 - 1] = "{Yes/True* | No/False} Default is No for Recloser. Write extra details to Eventlog.";
	
	
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TRecloser::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new Recloser and add it to Recloser class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TRecloserObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}
/*--------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------*/

int TRecloser::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	int i = 0;
	String ParamName;
	String Param;

  // continue parsing WITH contents of Parser
	ActiveRecloserObj = (TRecloserObj*)  ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveRecloserObj);
	result = 0;
	/*# with ActiveRecloserObj do */
	{
		auto with0 = ActiveRecloserObj;
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while(Param.size() > 0)
		{
			if(ParamName.size() == 0)
				++ParamPointer;
			else
				ParamPointer = CommandList.Getcommand(ParamName);
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))
				with0->Set_PropertyValue(ParamPointer, Param);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 390);
				break;
				case 	1:
				with0->MonitoredElementName = LowerCase(Param);
				break;
				case 	2:
				with0->MonitoredElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->ElementName = LowerCase(Param);
				break;
				case 	4:
				with0->ElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	5:
				with0->NumFast = Parser[ActorID]->MakeInteger_();
				break;
				case 	6:
				with0->PhaseFast = GetTccCurve(Param);
				break;
				case 	7:
				with0->PhaseDelayed = GetTccCurve(Param);
				break;
				case 	8:
				with0->GroundFast = GetTccCurve(Param);
				break;
				case 	9:
				with0->GroundDelayed = GetTccCurve(Param);
				break;
				case 	10:
				with0->PhaseTrip = Parser[ActorID]->MakeDouble_();
				break;
				case 	11:
				with0->GroundTrip = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->PhaseInst = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->GroundInst = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->ResetTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				with0->NumReclose = Parser[ActorID]->MakeInteger_() - 1;
				break;   // one less than number of shots
				case 	16:
				with0->NumReclose = Parser[ActorID]->ParseAsVector(4, with0->RecloseIntervals);
				break;   // max of 4 allowed
				case 	17:
				with0->DelayTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	19:
				with0->TDPhFast = Parser[ActorID]->MakeDouble_();
				break;
				case 	20:
				with0->TDGrFast = Parser[ActorID]->MakeDouble_();
				break;
				case 	21:
				with0->TDPhDelayed = Parser[ActorID]->MakeDouble_();
				break;
				case 	22:
				with0->TDGrDelayed = Parser[ActorID]->MakeDouble_();
				break;
				case 	23:
				{
					with0->InterpretRecloserState(ActorID, Param, ParamName);   // set normal state
					if(!with0->NormalStateSet)
						with0->NormalStateSet = true;
				}
				break;
				case 	18: case 24:
				with0->InterpretRecloserState(ActorID, Param, ParamName);
				break;    // set state
				case 	25:
				with0->SinglePhTrip = InterpretYesNo(Param);
				break;
				case 	26:
				with0->SinglePhLockout = InterpretYesNo(Param);
				break;
				case 	27:
				with0->set_Flocked(InterpretYesNo(Param));
				break;
				case 	28:
					if (InterpretYesNo(Param)) // force a reset
					{
						with0->set_Flocked(false);
						with0->Reset(ActorID);
						with0->Set_PropertyValue(28, "n");
					}
				break;
				case 	29:
				with0->ShowEventLog = InterpretYesNo(Param);
				break;
				case 	30:
				with0->DebugTrace = InterpretYesNo(Param);
				break;

           // Inherited parameters
				default:
				ClassEdit(ActiveRecloserObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	1:
              /*Default the controlled element to the monitored element*/
				with0->ElementName = with0->MonitoredElementName;
				break;
				case 	2:
				with0->ElementTerminal = with0->MonitoredElementTerminal;
				break;
				case 	18: case 24:
				{
					int stop = 0;
					for (stop = ((TDSSCktElement*)with0)->Fnphases, i = 1; i <= stop; i++)
					{
						if (!with0->NormalStateSet)
							(*with0->FNormalState)[i - 1] = (*with0->FPresentState)[i - 1];
					}
					with0->NormalStateSet = true; // normal state will default to state only the 1st state is specified.
				}
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		with0->RecalcElementData(ActorID);
	}
	return result;
}



/*--------------------------------------------------------------------------*/

int TRecloser::MakeLike(const String RecloserName)
{
	int result = 0;
	TRecloserObj* OtherRecloser = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Recloser name in the present collection*/
	OtherRecloser = ((TRecloserObj*) Find(RecloserName));
	if(OtherRecloser != nullptr)
		/*# with ActiveRecloserObj do */
		{
			auto with0 = ActiveRecloserObj;
			int stop = 0;
			with0->Set_NPhases(OtherRecloser->Fnphases);
			with0->Set_Nconds(OtherRecloser->Fnconds); // Force Reallocation of terminal stuff
			with0->ShowEventLog = OtherRecloser->ShowEventLog; // but leave DebugTrace off
			with0->ElementName = OtherRecloser->ElementName;
			with0->ElementTerminal = OtherRecloser->ElementTerminal;
			with0->Set_ControlledElement(OtherRecloser->get_FControlledElement());  // Pointer to target circuit element
			with0->Set_MonitoredElement(OtherRecloser->get_FMonitoredElement());  // Pointer to target circuit element
			with0->MonitoredElementName = OtherRecloser->MonitoredElementName;  // Pointer to target circuit element
			with0->MonitoredElementTerminal = OtherRecloser->MonitoredElementTerminal;  // Pointer to target circuit element
			with0->PhaseDelayed = OtherRecloser->PhaseDelayed;
			with0->GroundDelayed = OtherRecloser->GroundDelayed;
			with0->PhaseFast = OtherRecloser->PhaseFast;
			with0->GroundFast = OtherRecloser->GroundFast;
			with0->PhaseTrip = OtherRecloser->PhaseTrip;
			with0->GroundTrip = OtherRecloser->GroundTrip;
			with0->PhaseInst = OtherRecloser->PhaseInst;
			with0->GroundInst = OtherRecloser->GroundInst;
			with0->ResetTime = OtherRecloser->ResetTime;
			with0->NumReclose = OtherRecloser->NumReclose;
			with0->NumFast = OtherRecloser->NumFast;
			with0->SinglePhTrip = OtherRecloser->SinglePhTrip;
			with0->SinglePhLockout = OtherRecloser->SinglePhLockout;
			with0->RecloseIntervals = (pDoubleArray) realloc(with0->RecloseIntervals, sizeof(double) * 4);      // Always make a max of 4
			for(stop = with0->NumReclose, i = 1; i <= stop; i++)
			{
				(with0->RecloseIntervals)[i - 1] = (OtherRecloser->RecloseIntervals)[i - 1];
			}
			with0->set_Flocked(OtherRecloser->get_FLocked());
			for(stop = min(RECLOSERCONTROLMAXDIM, with0->get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
			{
				(*with0->FPresentState)[i - 1] = (*OtherRecloser->FPresentState)[i - 1];
				(*with0->FNormalState)[i - 1] = (*OtherRecloser->FNormalState)[i - 1];
			}
			with0->CondOffset = OtherRecloser->CondOffset;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherRecloser->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in Recloser MakeLike: \"") + RecloserName
	           + "\" Not Found.", 391);
	return result;
}




/*==========================================================================*/
/*                    TRecloserObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TRecloserObj::TRecloserObj(TDSSClass* ParClass, const String RecloserName)
 : inherited(ParClass),
			PhaseDelayed(nullptr),
			GroundDelayed(nullptr),
			PhaseFast(nullptr),
			GroundFast(nullptr),
			ResetTime(0.0),
			DelayTime(0.0),
			TDGrDelayed(0.0),
			TDPhDelayed(0.0),
			TDGrFast(0.0),
			TDPhFast(0.0),
			OperationCount(0),
			LockedOut(false),
			ArmedForClose(false),
			ArmedForOpen(false),
			GroundTarget(false),
			PhaseTarget(false),
			NormalStateSet(false),
			CondOffset(0),
			cBuffer(nullptr),
			RecloseIntervals(nullptr),
			NumFast(0),
			NumReclose(0),
			MonitoredElementTerminal(0),
			PhaseTrip(0.0),
			GroundTrip(0.0),
			PhaseInst(0.0),
			GroundInst(0.0)
{
    int i = 0;
	int stop = 0;
	Set_Name(LowerCase(RecloserName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	ElementName = "";
	Set_ControlledElement(nullptr);
	ElementTerminal = 1;
	MonitoredElementName = "";
	MonitoredElementTerminal = 1;
	Set_MonitoredElement(nullptr);
	PhaseFast = nullptr;
	PhaseDelayed =nullptr;
	GroundFast = nullptr;
	GroundDelayed = nullptr;
	PhaseTrip = 1.0;
	GroundTrip = 1.0;
	PhaseInst = 0.0;
	GroundInst = 0.0;
	TDGrDelayed = 1.0;
	TDPhDelayed = 1.0;
	TDGrFast = 1.0;
	TDPhFast = 1.0;
	ResetTime = 15.0;
	NumReclose = 3;
	NumFast = 1;
	RecloseIntervals = nullptr;
	RecloseIntervals = new double[4]; // fixed allocation of 4
	(RecloseIntervals)[1 - 1] = 0.5;
	(RecloseIntervals)[2 - 1] = 2.0;
	(RecloseIntervals)[3 - 1] = 2.0;
	
	FPresentState = nullptr;
	FNormalState = nullptr;
    LockedOut.clear();
    ArmedForClose.clear();
    ArmedForOpen.clear();
    GroundTarget = false;
    PhaseTarget.clear();
    OperationCount = nullptr;
    RecloserTarget = nullptr;
	SinglePhTrip = false;
	SinglePhLockout = false;
	IdxMultiPh = Get_NPhases() + 1;

	 // Reallocate arrays  (Must be initialized to nil for first call)
	FPresentState = (pStateArray) realloc(FPresentState, sizeof((*FPresentState)[1 - 1]) * Fnphases);
	FNormalState = (pStateArray) realloc(FNormalState, sizeof((*FNormalState)[1 - 1]) * Fnphases);
	LockedOut.resize(IdxMultiPh);
	ArmedForClose.resize(IdxMultiPh);
    ArmedForOpen.resize(IdxMultiPh);
    PhaseTarget.resize(IdxMultiPh);
    OperationCount = (pIntegerArray)realloc(OperationCount, sizeof(long) * IdxMultiPh);
    RecloserTarget = AllocStringArray(IdxMultiPh);

	for (stop = min(RECLOSERCONTROLMAXDIM, IdxMultiPh), i = 1; i <= stop; i++)
	{
		if (i <= Get_NPhases()) 
		{
			(*FPresentState)[i - 1] = CTRL_CLOSE;
			(*FNormalState)[i - 1] = CTRL_CLOSE;  // default to present state;
		}
		LockedOut[i - 1] = false;
		ArmedForOpen[i - 1] = false;
		ArmedForClose[i - 1] = false;
		PhaseTarget[i - 1] = false;
		OperationCount[i - 1] = 1;
		RecloserTarget[i - 1] = "";
	}

	cBuffer = nullptr; // Complex buffer
	DSSObjType = ParClass->DSSClassType;
	InitPropertyValues(0);



   //  RecalcElementData;
}

TRecloserObj::~TRecloserObj()
{
	MonitoredElementName = "";
	free(RecloseIntervals);
	free(cBuffer); // Should it be deallocated with this? cBuffer = (pComplexArray) realloc(cBuffer, 0);
    FPresentState = (pStateArray)realloc(FPresentState, 0);
    FNormalState = (pStateArray)realloc(FNormalState, 0);
    LockedOut.clear();
    ArmedForOpen.clear();
    ArmedForClose.clear();
    PhaseTarget.clear();
    OperationCount = (pIntegerArray)realloc(OperationCount, 0);
    FreeStringArray(RecloserTarget, IdxMultiPh);
	// inherited::Destroy();
}


/*--------------------------------------------------------------------------*/

void TRecloserObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	int i = 0;
	DevIndex = GetCktElementIndex(MonitoredElementName); // Global function
	if(DevIndex > 0)
	{
		Set_MonitoredElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
		Set_NPhases(get_FMonitoredElement()->Get_NPhases());       // Force number of phases to be same
		if(Fnphases > RECLOSERCONTROLMAXDIM)
		{
			DoSimpleMsg(String("Warning: Recloser ") + this->get_Name() + ": Number of phases > Max Recloser dimension.", 392);
		}
		if(MonitoredElementTerminal > get_FMonitoredElement()->Get_NTerms())
		{
			DoErrorMsg(String("Recloser: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Re-specify terminal no.", 392);
		}
		else

               // Sets name of i-th terminal's connected bus in Recloser's buslist
		{
			SetBus(1, get_FMonitoredElement()->GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
			cBuffer = (pComplexArray) realloc(cBuffer, sizeof(complex) * get_FMonitoredElement()->Yorder);
			CondOffset = (MonitoredElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
		}
	}

/*Check for existence of Controlled Element*/

         // If previously assigned, reset HasOCPDevice flag in case this is a move
	if(ASSIGNED(get_FControlledElement()))
	{
		get_FControlledElement()->HasOCPDevice = false;
		get_FControlledElement()->HasAutoOCPDevice = false;
	}
	DevIndex = GetCktElementIndex(ElementName); // Global function
	if(DevIndex > 0)  // Both CktElement and monitored element must already exist
	{
		Set_ControlledElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Make the 1 st terminal active

             // If the recloser becomes disabled, leave at False
		if(Get_Enabled())
		{
			get_FControlledElement()->HasOCPDevice = true;  // For Reliability calcs
			get_FControlledElement()->HasAutoOCPDevice = true;  // For Reliability calcs
		}
		
		int stop = 0;
		// Open/Closed State of controlled element based on state assigned to the control
		for(stop = min(RECLOSERCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)  // TODO --- evaluate if we need to do anything here....
		{
			if((*FPresentState)[i - 1] == CTRL_CLOSE)
			{
				get_FControlledElement()->Set_ConductorClosed(0, ActorID, true);
				LockedOut[i - 1] = false;
				OperationCount[i - 1] = 1;
				ArmedForOpen[i - 1] = false;
			}
			else
			{
				get_FControlledElement()->Set_ConductorClosed(0, ActorID, false);
				LockedOut[i - 1] = true;
				OperationCount[i - 1] = NumReclose + 1;
				ArmedForClose[i - 1] = false;
			}
		}
	}
	else
	{
		Set_ControlledElement(nullptr);   // element not found
		DoErrorMsg(String("Recloser: \"") + this->get_Name() + "\"", String("CktElement Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 393);
	}
}

void TRecloserObj::MakePosSequence(int ActorID)
{
	if(get_FMonitoredElement() != nullptr)
	{
		Set_NPhases(get_FMonitoredElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
		cBuffer = (pComplexArray) realloc(cBuffer, sizeof(complex) * get_FMonitoredElement()->Yorder);
		CondOffset = (ElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
	}
	inherited::MakePosSequence(ActorID);
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}
/*--------------------------------------------------------------------------*/

void TRecloserObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	int i = 0;
    int PhIdx = 0;
	if (SinglePhTrip) 
	{
		PhIdx = ProxyHdl; 
	}
	else
	{ 
		PhIdx = IdxMultiPh; // Proxy holds phase information for single-phase trip
	}
	/*# with ControlledElement do */
	{
		auto with0 = get_FControlledElement();
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal of CktElement to terminal 1
		switch(Code)
		{
			case ( CTRL_OPEN):
            if (SinglePhTrip)
			{
                switch ((*FPresentState)[PhIdx - 1])
				{
					case 	CTRL_CLOSE:
					if(ArmedForOpen[PhIdx - 1])   // ignore if we became disarmed in meantime
					{
						get_FControlledElement()->Set_ConductorClosed(PhIdx, ActorID, false);   // Open phase of active terminal
						(*FPresentState)[PhIdx - 1] = CTRL_OPEN;

						if(OperationCount[PhIdx - 1] > NumReclose)
						{
							LockedOut[PhIdx - 1] = true;
							if (SinglePhLockout and ShowEventLog) 
							{
								AppendToEventLog(String("Recloser.") + this->get_Name(), Format("Phase %d opened on %s (1ph trip) & locked out (1ph lockout)", PhIdx, RecloserTarget[PhIdx - 1]), ActorID);
							}
							else
							{
								if (ShowEventLog) AppendToEventLog(String("Recloser.") + this->get_Name(), Format("Phase %d opened on %s (1ph trip) & locked out (3ph lockout)", PhIdx, RecloserTarget[PhIdx - 1]), ActorID); // 3-Phase Lockout
								// Lockout other phases
								int stop = 0;
								for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
								{
									if (i != PhIdx and !LockedOut[i - 1])  // Check LockedOut[i - 1] to skip individual phase that were previously locked out
									{
										get_FControlledElement()->Set_ConductorClosed(i, ActorID, false);
                                        (*FPresentState)[i - 1] = CTRL_OPEN;
                                        LockedOut[i - 1] = true;
                                        if (ArmedForOpen[PhIdx - 1]) ArmedForOpen[PhIdx - 1] = false;  // TODO: confirm if we should be setting ArmedForOpen[i - 1] = false instead of ArmedForOpen[PhIdx - 1] = false
                                        if (ShowEventLog) AppendToEventLog(String("Recloser.") + this->get_Name(), Format("Phase %d opened on 3ph lockout (1ph trip) & locked out (3ph lockout)", i), ActorID);
									}
								}
							}
						}
						else
						{
							if (ShowEventLog) AppendToEventLog(String("Recloser.") + this->get_Name(), Format("Phase %d opened on %s (1ph trip)", PhIdx, RecloserTarget[PhIdx - 1]), ActorID);
						}
						ArmedForOpen[PhIdx - 1] = false;
					}
					break; /*nada*/
					default:
					  ;
					break;
				}
			}
			else // 3-Ph Trip
			{
                // Analyze each phase separately as states may not be the same
				int stop = 0;
				for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					switch((*FPresentState)[i - 1])
						{
							case 	CTRL_CLOSE:
							if(ArmedForOpen[PhIdx - 1])   // ignore if we became disarmed in meantime
							{
								get_FControlledElement()->Set_ConductorClosed(i, ActorID, false);   // Open phases of active terminal
								(*FPresentState)[i - 1] = CTRL_OPEN;

								if (OperationCount[PhIdx - 1] > NumReclose)
								{
									LockedOut[PhIdx - 1] = true;
									if (ShowEventLog) AppendToEventLog(String("Recloser.") + this->get_Name(), Format("Phase %d opened on %s (3ph trip) & locked out (3ph lockout)", i, RecloserTarget[PhIdx - 1]), ActorID);
								}
								else
								{
									if (ShowEventLog) AppendToEventLog(String("Recloser.") + this->get_Name(), Format("Phase %d opened on %s (3ph trip)", i, RecloserTarget[PhIdx - 1]), ActorID);
								}
							}
							break; /*nada*/
							default:
							  ;
							break;
						}
				}
				ArmedForOpen[PhIdx - 1] = false;
			}
			break;
			case ( CTRL_CLOSE):
			if (SinglePhTrip)
			{
                switch ((*FPresentState)[PhIdx - 1])
				{
					case 	CTRL_OPEN:
					if(ArmedForClose[PhIdx - 1] and !LockedOut[PhIdx - 1])
					{
						get_FControlledElement()->Set_ConductorClosed(PhIdx, ActorID, true);   // Close phase of active terminal
						(*FPresentState)[PhIdx - 1] = CTRL_CLOSE;
						if (ShowEventLog) AppendToEventLog(String("Recloser.") + this->get_Name(), Format("Phase %d closed (1ph reclosing)", PhIdx), ActorID); 
						
						// Count reclosing operations for each phase on single ph trip
                        ++OperationCount[PhIdx - 1];
						ArmedForClose[PhIdx - 1] = false;
					}
					break; /*nada*/
					default:
					  ;
					break;
				}
			}
			else
			{
				int stop = 0;
				for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					switch((*FPresentState)[i - 1])
						{   // Check LockedOut[i - 1] to skip individual phases that were previously locked out
							case 	CTRL_OPEN:
							if(ArmedForClose[PhIdx - 1]  and !LockedOut[i - 1]  and !LockedOut[PhIdx - 1])
							{
								get_FControlledElement()->Set_ConductorClosed(i, ActorID, true);   // Close phases of active terminal
								(*FPresentState)[i - 1] = CTRL_CLOSE;

								if (ShowEventLog) AppendToEventLog(String("Recloser.") + this->get_Name(), Format("Phase %d closed (3ph reclosing)", i), ActorID);
							}
							break; /*nada*/
							default:
							  ;
							break;
						}
				}
				ArmedForClose[PhIdx - 1] = false;
				++OperationCount[PhIdx - 1];
			}
			break;
			case ( CTRL_RESET):
            if (SinglePhTrip)
			{
                switch ((*FPresentState)[PhIdx - 1])
				{
					case 	CTRL_CLOSE:
					if(ArmedForOpen[PhIdx - 1]) 
					{
						OperationCount[PhIdx - 1] = 1;       // Don't reset if we just rearmed
					}
					break; /*nada*/
					default:
					  ;
					break;
				}
			}
			else
			{
                // Analyze each phase separately as states may not be the same
				int stop = 0;
				for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					switch((*FPresentState)[i - 1])
						{
							case 	CTRL_CLOSE:
							if(ArmedForOpen[PhIdx - 1])   // ignore if we became disarmed in meantime
							{
								OperationCount[PhIdx - 1] = 1;       // Don't reset if we just rearmed
							}
							if (!SinglePhTrip) break;  // no need to loop at all phases
							break; /*nada*/
							default:
							  ;
							break;
						}
				}
			}
			break;
            /*Do Nothing */
			default:
			  ;
			break;
		}
	}
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::InterpretRecloserState(int ActorID, const String param, const String property_name)
{
	int i = 0;
    string DataStr1 = "";
	string DataStr2 = "";
    // Only allowed to change normal state if locked.
    if (get_FLocked() and (LowerCase(property_name)[0] == 'a' or LowerCase(property_name)[0] == 's'))
    {
        return;
    }

	if (LowerCase(property_name)[0] == 'a') // Interpret ganged specification to state and normal when using action
    { // action (deprecated) will be removed
        int stop = 0;
        for (stop = RECLOSERCONTROLMAXDIM, i = 1; i <= stop; i++)
        {
            switch (LowerCase(param)[0])
            {
            case L'o':
                set_States(i, CTRL_OPEN);
                break;
            case L'c':
                set_States(i, CTRL_CLOSE);
                break;
            default:;
                break;
            }
        }
    }
    else
    {
        if (!AuxParser[ActorID]->IsQuotedString) // Interpret ganged specification to state and normal when not quoted
        {
            int stop = 0;
            for (stop = RECLOSERCONTROLMAXDIM, i = 1; i <= stop; i++)
            {
                if (LowerCase(property_name)[0] == 's') // state
                {
                    switch (LowerCase(param)[0])
                    {
                    case L'o':
                        set_States(i, CTRL_OPEN);
                        break;
                    case L'c':
                        set_States(i, CTRL_CLOSE);
                        break;
                    default:;
                        break;
                    }
                }
                else // 'normal
                {
                    switch (LowerCase(param)[0])
                    {
                    case L'o':
                        set_NormalStates(i, CTRL_OPEN);
                        break;
                    case L'c':
                        set_NormalStates(i, CTRL_CLOSE);
                        break;
                    default:;
                        break;
                    }
                }
            }
        }
        else // process phase by phase
        {
            AuxParser[ActorID]->SetCmdString(param); // Load up Parser
            DataStr1 = AuxParser[ActorID]->GetNextParam(); // ignore
            DataStr2 = AuxParser[ActorID]->MakeString_();
            i = 1;
            while ((DataStr2.size() > 0) && (i < RECLOSERCONTROLMAXDIM))
            {
                if (LowerCase(property_name)[0] == 's') // state
                {
                    switch (LowerCase(DataStr2)[0])
                    {
                    case L'o':
                        set_States(i, CTRL_OPEN);
                        break;
                    case L'c':
                        set_States(i, CTRL_CLOSE);
                        break;
                    default:;
                        break;
                    }
                }
                else // 'normal'
                {
                    switch (LowerCase(DataStr2)[0])
                    {
                    case L'o':
                        set_NormalStates(i, CTRL_OPEN);
                        break;
                    case L'c':
                        set_NormalStates(i, CTRL_CLOSE);
                        break;
                    default:;
                        break;
                    }
                }
                DataStr1 = AuxParser[ActorID]->GetNextParam(); // ignore
                DataStr2 = AuxParser[ActorID]->MakeString_();
                ++i;
            }
        }
    }
}

/*--------------------------------------------------------------------------*/

bool TRecloserObj::get_FLocked()
{
    return FLocked;
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::sample(int ActorID)
{
	int i = 0;
	double cmag = 0.0;
	complex Csum = {};
	int MaxOperatingCount = 0;
	TTCC_CurveObj* GroundCurve = nullptr;
	TTCC_CurveObj* PhaseCurve = nullptr;
	double Groundtime = 0.0;
	double PhaseTime = 0.0;
	double TripTime = 0.0;
	double TimeTest = 0.0;
	double TDPhase = 0.0;
	double TDGround = 0.0;
    string PhaseCurveType = ""; 
	string GroundCurveType = "";

	get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);
    get_FMonitoredElement()->GetCurrents(cBuffer, ActorID);

	// Check state of phases of active terminal as they could have changed through other mechanisms
    int stop = 0;
    for (stop = min(RECLOSERCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
    {
        if (get_FControlledElement()->Get_ConductorClosed(i, ActorID))
        {
            (*FPresentState)[i - 1] = CTRL_CLOSE;
        }
        else
        {
            (*FPresentState)[i - 1] = CTRL_OPEN;
        }
    }

	if (DebugTrace) AppendToEventLog(String("Debug Sample: Recloser.") + this->get_Name(), Format("FPresentState: %s ", this->GetPropertyValue(24)), ActorID);

	for (stop = min(RECLOSERCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = stop; i >= 1; i--)
    {
		if ((*FPresentState)[i - 1] == CTRL_CLOSE)	break; // Continue sampling if at least one phase is closed.
        if (i == 1) return;  // Exit sampling if none of the phases is closed.
    }

	// Identify number of operations to pick appropriate curve.
    // Pending to identify phase to trip when considering single-phase tripping as in modern microprocessed relays
	if (SinglePhTrip)
    {
		for (stop = min(RECLOSERCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			if (LockedOut[i - 1]) continue;  // Skip locked out phases (includes phases that have been manually opened).
			if (i == 1) 
			{ 
				MaxOperatingCount = OperationCount[i - 1];             // TODO: check if we need to use IdxMultiPh for OperationCount here
			}
			else 
			{
				MaxOperatingCount = max(MaxOperatingCount, OperationCount[i - 1]);
			}
		}
    }
    else
    {
		MaxOperatingCount = OperationCount[IdxMultiPh - 1];
    }

	if (MaxOperatingCount > NumFast)
    {
        GroundCurve = GroundDelayed;
        TDGround = TDGrDelayed;
        GroundCurveType = "Delayed";
    }
    else
    {
        GroundCurve = GroundFast;
        TDGround = TDGrFast;
        GroundCurveType = "Fast";
    }
    Groundtime = -1.0;
    /* Check Ground Trip, if any */
    auto with0 = get_FMonitoredElement();
    if (GroundCurve != nullptr)
    {
        int stop = 0;
        Csum = CZero;
        for (stop = (with0->Fnphases + CondOffset), i = (1 + CondOffset); i <= stop; i++)
        {
            caccum(Csum, (cBuffer)[i - 1]);
        }
        cmag = cabs(Csum);
        if ((GroundInst > 0.0) && (cmag >= GroundInst) && (MaxOperatingCount == 1))
        {
            Groundtime = 0.01 + DelayTime;      // Inst trip on first operation
			if (DebugTrace)
				AppendToEventLog(String("Debug Sample: Recloser.") + this->get_Name(), Format("Ground Instantaneous Trip: Mag=%.3g, Time=%.3g", cmag, Groundtime), ActorID);
        }
		else
		{
            Groundtime = TDGround * GroundCurve->GetTCCTime(cmag / GroundTrip);
			if (DebugTrace)
                AppendToEventLog(String("Debug Sample: Recloser.") + this->get_Name(), Format("Ground %s Curve Trip: Mag=%.3g, Time=%.3g", GroundCurveType, cmag / GroundTrip, Groundtime), ActorID);
		}
    }
    if (Groundtime > 0.0) GroundTarget = true;
    // If GroundTime > 0 then we have a ground trip

	if (SinglePhTrip)
    {
		for (stop = min(RECLOSERCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			if ((*FPresentState)[i - 1] != CTRL_CLOSE) continue;
			if (Groundtime > 0.0) // initialize trip time for this phase
				TripTime = Groundtime; 
			else 
				TripTime = -1.0;  

			if (OperationCount[i - 1] > NumFast)
			{
				PhaseCurve = PhaseDelayed;
                TDPhase = TDPhDelayed;
                PhaseCurveType = "Delayed";
			}
			else
			{
				PhaseCurve = PhaseFast;
				TDPhase = TDPhFast;
                PhaseCurveType = "Fast";
			}

			if ((*FPresentState)[i - 1] == CTRL_CLOSE)
			{
				PhaseTime = -1.0;  /* No trip */
				/* Check Phase Trip, if any*/ // Check current at i phase of monitored element
				if(PhaseCurve != nullptr)
				{
					cmag =  cabs(cBuffer[i + CondOffset - 1]);
					if((PhaseInst > 0.0) && (cmag >= PhaseInst) && (OperationCount[i - 1] == 1))
					{
						PhaseTime = 0.01 + DelayTime;  // Inst trip on first operation
						if (DebugTrace)
							AppendToEventLog(String("Debug Sample: Recloser.") + this->get_Name(), Format("Ph Instantaneous (1-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", i, cmag, PhaseTime), ActorID);
					}
					else
					{
						TimeTest = TDPhase * PhaseCurve->GetTCCTime(cmag / PhaseTrip);
						if(TimeTest > 0.0)
						{
							PhaseTime = TimeTest;
							if (DebugTrace)
								AppendToEventLog(String("Debug Sample: Recloser.") + this->get_Name(), Format("Ph %s (1-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", PhaseCurveType, i, cmag/PhaseTrip, PhaseTime), ActorID);
						}
					}
				}

				// If PhaseTime > 0 then we have a phase trip
				if(PhaseTime > 0.0)
				{
					PhaseTarget[i - 1] = true;
					if(TripTime > 0.0)
						TripTime = min(TripTime, PhaseTime);
					else
						TripTime = PhaseTime;
				}
				if(TripTime > 0.0)
				{
					if(!ArmedForOpen[i - 1])
						/*# with ActiveCircuit[ActorID] do */
						{
							// Then arm for an open operation
							RecloserTarget[i - 1] = "";
							if (TripTime == Groundtime)
							{
								if (Groundtime == 0.01 + DelayTime) 
									RecloserTarget[i - 1] = "Gnd Instantaneous";
								else 
									RecloserTarget[i - 1] = Format("Ground %s", GroundCurveType);
							}
							if (TripTime == PhaseTime)
							{
								if (RecloserTarget[i - 1] != "")
									RecloserTarget[i - 1] = RecloserTarget[i - 1] + " + ";
								if (PhaseTime == 0.01 + DelayTime) 
									RecloserTarget[i - 1] = "Ph Instantaneous";
								else 
									RecloserTarget[i - 1] = Format("Ph %s", PhaseCurveType);
							}

							ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + DelayTime, CTRL_OPEN, i, this, ActorID);
							if(OperationCount[i - 1] <= NumReclose)
								ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + DelayTime + (RecloseIntervals)[OperationCount[i - 1] - 1], CTRL_CLOSE, i, this, ActorID);
							ArmedForOpen[i - 1] = true;
							ArmedForClose[i - 1] = true;
						}
				}
				else
				{
					if(ArmedForOpen[i - 1])
						/*# with ActiveCircuit[ActorID] do */
						{
								// If current dropped below pickup, disarm trip and set for reset
							ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
							ArmedForOpen[i - 1] = false;
							ArmedForClose[i - 1] = false;
							GroundTarget = false;
							PhaseTarget[i - 1] = false;
						}
				}
			}			
		}
    }
    else  // 3-Phase Trip
    {
        if (MaxOperatingCount > NumFast)
		{
			PhaseCurve = PhaseDelayed;
            TDPhase = TDPhDelayed;
            PhaseCurveType = "Delayed";
		}
		else
		{
			PhaseCurve = PhaseFast;
			TDPhase = TDPhFast;
            PhaseCurveType = "Fast";
		}

		if (Groundtime > 0.0) // initialize trip time for this phase
            TripTime = Groundtime;
        else
            TripTime = -1.0; 

		/* Check Phase Trip, if any*/
		if(PhaseCurve != nullptr)
		{
			for (stop = (with0->Fnphases + CondOffset), i = (1 + CondOffset); i <= stop; i++)
			{
				cmag =  cabs(cBuffer[i - 1]);
				if((PhaseInst > 0.0) && (cmag >= PhaseInst) && (OperationCount[IdxMultiPh - 1] == 1))
				{
					PhaseTime = 0.01 + DelayTime;  // Inst trip on first operation
					if (DebugTrace)
						AppendToEventLog(String("Debug Sample: Recloser.") + this->get_Name(), Format("Ph Instantaneous (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", i, cmag, PhaseTime), ActorID);
					break;  /*FOR - if Inst, no sense checking other phases*/
				}
				else
				{
					TimeTest = TDPhase * PhaseCurve->GetTCCTime(cmag / PhaseTrip);
					if(TimeTest > 0.0)
					{
						PhaseTime = TimeTest;
						if (DebugTrace)
							if (PhaseCurve == PhaseFast)
								AppendToEventLog(String("Debug Sample: Recloser.") + this->get_Name(), Format("Ph %s (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", "Fast", i, cmag/PhaseTrip, PhaseTime), ActorID);
							else
								AppendToEventLog(String("Debug Sample: Recloser.") + this->get_Name(), Format("Ph %s (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", "Delayed", i, cmag / PhaseTrip, PhaseTime), ActorID);
					}
					if (PhaseTime < 0.0)
						PhaseTime = TimeTest;
					else
						PhaseTime = min(PhaseTime, TimeTest);
				}
			}
		}

		// If PhaseTime > 0 then we have a phase trip
		if(PhaseTime > 0.0)
		{
			PhaseTarget[IdxMultiPh - 1] = true;
			if(TripTime > 0.0)
				TripTime = min(TripTime, PhaseTime);
			else
				TripTime = PhaseTime;
		}
		if(TripTime > 0.0)
		{
			if(!ArmedForOpen[IdxMultiPh - 1])
				/*# with ActiveCircuit[ActorID] do */
				{
					// Then arm for an open operation
					RecloserTarget[IdxMultiPh - 1] = "";
					if (TripTime == Groundtime)
					{
						if (Groundtime == 0.01 + DelayTime) 
							RecloserTarget[IdxMultiPh - 1] = "Gnd Instantaneous";
						else 
							RecloserTarget[IdxMultiPh - 1] = Format("Ground %s", GroundCurveType);
					}
					if (TripTime == PhaseTime)
					{
						if (RecloserTarget[IdxMultiPh - 1] != "")
							RecloserTarget[IdxMultiPh - 1] = RecloserTarget[IdxMultiPh - 1] + " + ";
						if (PhaseTime == 0.01 + DelayTime) 
							RecloserTarget[IdxMultiPh - 1] = "Ph Instantaneous";
						else 
							RecloserTarget[IdxMultiPh - 1] = Format("Ph %s", PhaseCurveType);
					}

					ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + DelayTime, CTRL_OPEN, 0, this, ActorID);
					if(MaxOperatingCount <= NumReclose)
						ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + DelayTime + (RecloseIntervals)[MaxOperatingCount - 1], CTRL_CLOSE, 0, this, ActorID);
					ArmedForOpen[IdxMultiPh - 1] = true;
					ArmedForClose[IdxMultiPh - 1] = true;
				}
		}
		else
		{
			if(ArmedForOpen[IdxMultiPh - 1])
				/*# with ActiveCircuit[ActorID] do */
				{
						// If current dropped below pickup, disarm trip and set for reset
					ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, i, this, ActorID);
					ArmedForOpen[IdxMultiPh - 1] = false;
					ArmedForClose[IdxMultiPh - 1] = false;
					GroundTarget = false;
					PhaseTarget[IdxMultiPh - 1] = false;
				}
		}
		
    }
}



/*--------------------------------------------------------------------------*/

void TRecloserObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
	{
		WriteLn(f);
	}
}

String TRecloserObj::GetPropertyValue(int Index)
{
	String result;
    result = "";
    int i = 0;
    switch (Index)
    {
    case 23: case 24:
        result = "[";
        break;
    default:
        result = "";
        break;
    }
	switch(Index)
	{
		case 	15:
		result = Format("%d", NumReclose + 1);
		break;
		case 	16:
		{
			int stop = 0;
			result = "(";
			for(stop = NumReclose, i = 1; i <= stop; i++)
			{
				result = result + Format("%-g, ", (RecloseIntervals)[i - 1]);
			}
			result = result + ")";
		}
		break;
		case 	23:
		{
			if(get_FControlledElement() != nullptr)
			{
				int stop = 0;
				for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					switch((*FNormalState)[i - 1])
					{
						case 	CTRL_OPEN:
						result = result + "open" + ", ";
						break;
						  /*CTRL_CLOSE:*/
						default:
						result = result + "closed" + ", ";
						break;
					}
				}
			}
		}
		break;
		case 24:
		{
			if(get_FControlledElement() != nullptr)
			{
				int stop = 0;
				for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					switch((*FPresentState)[i - 1])
					{
						case 	CTRL_OPEN:
						result = result + "open" + ", ";
						break;
						  /*CTRL_CLOSE:*/
						default:
						result = result + "closed" + ", ";
						break;
					}
				}
			}
		}
		break;
		case 	27:
		{
			if (get_FLocked())
			{
				result = "Yes";
			}
			else
			{
				result = "No";
			}
		}
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	switch (Index)
    {
    case 23: case 24:
        result = result + "]";
        break;
    default:
        break;
    }
	return result;
}

void TRecloserObj::Reset(int ActorID)
{
	int i = 0;
    if (!get_FLocked() and get_FControlledElement() != nullptr)
    {
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
        int stop = 0;
        for(stop = min(RECLOSERCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
        {
            (*FPresentState)[i - 1] = (*FNormalState)[i - 1]; // reset to normal state
			ArmedForOpen[i - 1] = false;
            ArmedForClose[i - 1] = false;
			GroundTarget = false;
            PhaseTarget[i - 1] = false;
			switch((*FNormalState)[i - 1])
			{
				case 	CTRL_OPEN:
				{
					get_FControlledElement()->Set_ConductorClosed(i, ActiveActor, false);
					LockedOut[i - 1] = true;
					OperationCount[i - 1] = NumReclose + 1;
				}
				break;
			  /*CTRL_CLOSE:*/
				default:
				get_FControlledElement()->Set_ConductorClosed(i, ActiveActor, true);
				LockedOut[i - 1] = false;
				OperationCount[i - 1] = 1;
				break;
			}
		}
	}
}

void TRecloserObj::set_Flocked(bool Value)
{
    FLocked = Value;
}

EControlAction TRecloserObj::get_States(int Idx)
{
    EControlAction result;
    if (get_FControlledElement() != nullptr)
    {
        get_FControlledElement()->Set_ActiveTerminal(ElementTerminal); // Set active terminal
        if (get_FControlledElement()->Get_ConductorClosed(Idx, ActiveActor))
        {
            /*TRUE:*/
            (*FPresentState)[Idx - 1] = CTRL_CLOSE;
        }
        else
        {
            /* FALSE */
            (*FPresentState)[Idx - 1] = CTRL_OPEN;
        }
    }
    result = (*FPresentState)[Idx - 1];
    return result;
}

void TRecloserObj::set_States(int Idx, const EControlAction Value)
{
    if (get_States(Idx) != Value)
    {
        if (get_FControlledElement() != nullptr)
        {
            get_FControlledElement()->Set_ActiveTerminal(ElementTerminal); // Set active terminal
            switch (Value)
            {
            case CTRL_OPEN:
                get_FControlledElement()->Set_ConductorClosed(Idx, ActiveActor, false);
                LockedOut[Idx - 1] = true;
                OperationCount[Idx - 1] = NumReclose + 1;
                ArmedForClose[Idx - 1] = false;
                break;
                /*CTRL_CLOSE:*/
            default:
                get_FControlledElement()->Set_ConductorClosed(Idx, ActiveActor, true);
                LockedOut[Idx - 1] = false;
                OperationCount[Idx - 1] = 1;
                ArmedForOpen[Idx - 1] = false;
                break;
            }
        }
        (*FPresentState)[Idx - 1] = Value;
    }
}

EControlAction TRecloserObj::get_NormalStates(int Idx)
{
	EControlAction result;
    result = (*FNormalState)[Idx - 1];
	return result;
}

void TRecloserObj::set_NormalStates(int Idx, const EControlAction Value)
{
    if ((*FNormalState)[Idx - 1] != Value)
	{
        (*FNormalState)[Idx - 1] = Value;
	}
}

void TRecloserObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,""); //'element';
	Set_PropertyValue(2,"1"); //'terminal';
	Set_PropertyValue(3,"");
	Set_PropertyValue(4,"1"); //'terminal';
	Set_PropertyValue(5,IntToStr(NumFast));
	Set_PropertyValue(6,"none");
	Set_PropertyValue(7,"none");
	Set_PropertyValue(8,"none");
	Set_PropertyValue(9,"none");
	Set_PropertyValue(10,"1.0");
	Set_PropertyValue(11,"1.0");
	Set_PropertyValue(12,"0");
	Set_PropertyValue(13,"0");
	Set_PropertyValue(14,"15");
	Set_PropertyValue(15,"4");
	Set_PropertyValue(16,"(0.5, 2.0, 2.0)");
	Set_PropertyValue(17,"0.0");
	Set_PropertyValue(18,""); // action
	Set_PropertyValue(19,"1.0");
	Set_PropertyValue(20,"1.0");
	Set_PropertyValue(21,"1.0");
	Set_PropertyValue(22,"1.0");
	Set_PropertyValue(23,"[closed, closed, closed]");  // normal
	Set_PropertyValue(24,"[closed, closed, closed]");  // state
	Set_PropertyValue(25,"No");  // SinglePhTripping
	Set_PropertyValue(26,"No");  // SinglePhLockout
	Set_PropertyValue(27,"No");  // Lock
	Set_PropertyValue(28,"n");  // Reset
	inherited::InitPropertyValues(NumPropsThisClass);
}




}  // namespace Recloser





