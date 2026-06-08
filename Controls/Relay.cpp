
#pragma hdrstop

#include "Relay.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "PCElement.h"
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
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace TCC_Curve;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace Relay
{

TRelayObj::TRelayObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TRelayObj::TRelayObj(String ClassName) : inherited(ClassName) {}
TRelayObj::TRelayObj() {}


TRelayObj*	ActiveRelayObj = nullptr;
TRelay*		RelayClass = nullptr;
const int NumPropsThisClass = 71;
const int Current = 0;  /*Default*/
const int VOLTAGE = 1;
const int REVPOWER = 3;
const int NEGCURRENT = 4;
const int NEGVOLTAGE = 5;
const int GENERIC = 6; /*Use this for frequency, etc.  Generic over/under relay*/
const int Distance = 7;
const int TD21 = 8;
const int DOC = 9;

const double MIN_DISTANCE_REACTANCE = -1.0e-8; /*allow near-bolted faults to be detected*/
const double DEG_TO_RAD = M_PI / 180.0;

/*--------------------------------------------------------------------------*/  // Creates superstructure for all Relay objects

TRelay::TRelay()
 : TCC_CurveClass( (TDSSClass*) GetDSSClassPtr("TCC_Curve"))
{
	;
	Class_Name = "Relay";
	DSSClassType = DSSClassType + RELAY_CONTROL;
	DefineProperties();
	CommandList = TCommandList(PropertyName, NumProperties);
	CommandList.set_AbbrevAllowed(true);
	RelayClass = this;
}

/*--------------------------------------------------------------------------*/

TRelay::~TRelay()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TRelay::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();   /*see DSSClass*/


     // Define Property names
     // Addproperty (property name,  internal property index (see Edit), Help string);
	AddProperty("MonitoredObj", 1, "Full object name of the circuit element, typically a line, transformer, load, or generator, "
	           "to which the relay's PT and/or CT are connected."
	           " This is the \"monitored\" element. "
	           "There is no default; must be specified.");
	AddProperty("MonitoredTerm", 2, "Number of the terminal of the circuit element to which the Relay is connected. "
	           "1 or 2, typically.  Default is 1.");
	AddProperty("SwitchedObj", 3, "Name of circuit element switch that the Relay controls. "
	           "Specify the full object name."
	           "Defaults to the same as the Monitored element. "
	           "This is the \"controlled\" element.");
	AddProperty("SwitchedTerm", 4, "Number of the terminal of the controlled element in which the switch is controlled by the Relay. "
	           "1 or 2, typically.  Default is 1.");
	AddProperty("type", 5, String("One of a legal relay type:") + CRLF
	           + "  Current"
	           + CRLF
	           + "  Voltage"
	           + CRLF
	           + "  Reversepower"
	           + CRLF
	           + "  46 (neg seq current)"
	           + CRLF
	           + "  47 (neg seq voltage)"
	           + CRLF
	           + "  Generic (generic over/under relay)"
	           + CRLF
	           + "  Distance"
	           + CRLF
	           + "  TD21"
	           + CRLF
	           + "  DOC (directional overcurrent)"
	           + CRLF
	           + CRLF
	           + "Default is overcurrent relay (Current). "
	           + "Specify the curve and pickup settings appropriate for each type. "
	           + "Generic relays monitor PC Element Control variables and trip on out of over/under range in definite time.");
	AddProperty("PhCurve", 6, "Name of the TCC Curve object that determines the phase trip.  "
	           "Must have been previously defined as a TCC_Curve object or specified as \"none\" (ignored). "
	           "Default is \"none\". "
	           "For overcurrent relay, multiplying the current values in the curve by the \"PhPickup\" value gives the actual current.");
	AddProperty("OC_GndCurve", 7, "Name of the TCC Curve object that determines the ground trip for overcurrent relay.  Must have been previously defined as a TCC_Curve object or specified as \"none\" (ignored). "
	           "Default is \"none\". "
	           "For overcurrent relay, multiplying the current values in the curve by the \"GndPickup\" value gives the actual current.");
	AddProperty("PhPickup", 8, "Multiplier for the phase TCC curve for overcurrent relay OR actual phase amps when operating with definite time (see \"DefiniteTimeDelay\" property). Defaults to 1.0.");
	AddProperty("OC_GndPickup", 9, "Multiplier for the ground TCC curve for overcurrent relay OR actual ground amps (3I0) when operating with definite time (see \"DefiniteTimeDelay\" property). Defaults to 1.0.");
	AddProperty("TDPh", 28, "Time dial for Phase trip curve. Multiplier on time axis of specified curve. Default=1.0.");
	AddProperty("OC_TDGnd", 29, "Time dial for Ground trip curve for overcurrent relay. Multiplier on time axis of specified curve. Default=1.0.");
	AddProperty("PhInst", 10, "Actual  amps (Current relay) or kW (reverse power relay) for instantaneous phase trip which is assumed to happen in 0.01 sec + Mechanical Delay Time. Default is 0.0, which signifies no inst trip. "
	           "Use this value for specifying the Reverse Power threshold (kW) for reverse power relays.");
	AddProperty("OC_GndInst", 11, "Actual amps for instantaneous ground trip for overcurrent relay which is assumed to happen in 0.01 sec + Mechanical Delay Time. Default is 0.0, which signifies no inst trip.");
	AddProperty("ResetTime", 12, "Reset time in sec for relay.  Default is 15. If this much time passes between the last pickup event, and the relay has not locked out, the operation counter resets.");
	AddProperty("Shots", 13, "Number of shots to lockout.  Default is 4. This is one more than the number of reclose intervals.");
	AddProperty("RecloseIntervals", 14, "Array of reclose intervals. If none, specify \"NONE\". Default for overcurrent relay is (0.5, 2.0, 2.0) seconds. "
	           "Default for a voltage relay is (5.0). In a voltage relay, this is  seconds after restoration of "
	           "voltage that the reclose occurs. "
	           "Reverse power relay is one shot to lockout, "
	           "so this is ignored.  A locked out relay must be closed manually (set action=close).");
	AddProperty("DefiniteTimeDelay", 24, "Trip time delay (sec) for DEFINITE TIME relays. Default is 0.0 for current and DOC relays. "
	           "For overcurrent relays, if>0 and specified pickups (ground and/or phase) are excedeed, definite time operation is used instead of curves. "
	           "For DOC relay, if>0 definite time operation is used instead of curves. "
	           "Used by Generic, RevPower, 46 and 47 relays. Defaults to 0.1 s for these relays.");
	AddProperty("Voltage_OVCurve", 15, "TCC Curve object to use for overvoltage relay. Must have been previously defined as a TCC_Curve object or specified as \"none\" (ignored). "
	           "Default is \"none\". Curve is assumed to be defined with per unit voltage values. "
	           "Voltage base should be defined for the relay. Default is none (ignored).");
	AddProperty("Voltage_UVCurve", 16, "TCC Curve object to use for undervoltage relay. Must have been previously defined as a TCC_Curve object or specified as \"none\" (ignored). "
	           "Default is \"none\". Curve is assumed to be defined with per unit voltage values. "
	           "Voltage base should be defined for the relay. Default is none (ignored).");
	AddProperty("kvbase", 17, "Voltage base (kV) for the relay. Specify line-line for 3 phase devices); line-neutral for 1-phase devices.  Relay assumes "
	           "the number of phases of the monitored element.  Default is 0.0, which results in assuming the voltage "
	           "values in the \"TCC\" curve are specified in actual line-to-neutral volts.");
	AddProperty("47%Pickup", 25, "Percent voltage pickup for 47 relay (Neg seq voltage). Default is 2. Specify also base voltage (kvbase) and delay time value.   ");
	AddProperty("46BaseAmps", 23, "Base current, Amps, for 46 relay (neg seq current)."
	           "  Used for establishing pickup and per unit I-squared-t.");
	AddProperty("46%Pickup", 21, "Percent pickup current for 46 relay (neg seq current).  Default is 20.0. "
	           "  When current exceeds this value * BaseAmps, I-squared-t calc starts.");
	AddProperty("46isqt", 22, "Negative Sequence I-squared-t trip value for 46 relay (neg seq current)."
	           "  Default is 1 (trips in 1 sec for 1 per unit neg seq current).  Should be 1 to 99.");
	AddProperty("Generic_Variable", 20, "Name of variable in PC Elements being monitored.  Only applies to Generic relay.");
	AddProperty("Generic_OverTrip", 26, "Trip setting (high value) for Generic relay variable.  Relay trips in definite time if value of variable exceeds this value.");
	AddProperty("Generic_UnderTrip", 27, "Trip setting (low value) for Generic relay variable.  Relay trips in definite time if value of variable is less than this value.");
	AddProperty("MechanicalDelay", 18, "Fixed delay time (sec) added to relay time. Default is 0.0. Designed to represent breaker time or some other delay after a trip decision is made."
	           "Use Delay property for setting a fixed trip time delay."
	           "Added to trip time of current and voltage relays. Could use in combination with inst trip value to obtain a definite time overcurrent relay.");
	AddProperty("Action", 19, "DEPRECATED. See \"State\" property");
	AddProperty("Z1mag", 30, "Positive sequence reach impedance in primary ohms for Distance and TD21 functions. Default=0.7");
	AddProperty("Z1ang", 31, "Positive sequence reach impedance angle in degrees for Distance and TD21 functions. Default=64.0");
	AddProperty("Z0mag", 32, "Zero sequence reach impedance in primary ohms for Distance and TD21 functions. Default=2.1");
	AddProperty("Z0ang", 33, "Zero sequence reach impedance angle in degrees for Distance and TD21 functions. Default=68.0");
	AddProperty("Mphase", 34, "Phase reach multiplier in per-unit for Distance and TD21 functions. Default=0.7");
	AddProperty("Mground", 35, "Ground reach multiplier in per-unit for Distance and TD21 functions. Default=0.7");
	AddProperty("EventLog", 36, "{Yes/True* | No/False} Default is Yes for Relay. Write trips, reclose and reset events to EventLog.");
	AddProperty("DebugTrace", 37, "{Yes/True* | No/False} Default is No for Relay. Write extra details to Eventlog.");
	AddProperty("DistReverse", 38, "{Yes/True* | No/False} Default is No; reverse direction for distance and td21 types.");
	AddProperty("Normal", 39, "ARRAY of strings {Open | Closed} representing the Normal state of the relay in each phase of the controlled element. "
	           "The relay reverts to this state for reset, change of mode, etc. "
	           "Defaults to \"State\" if not specifically declared.  Setting this property to {Open | Closed} sets the normal state to the specified value for all phases (ganged operation).");
	AddProperty("State", 40, "ARRAY of strings {Open | Closed} representing the Actual state of the relay in each phase of the controlled element. "
	           "Upon setting, immediately forces the state of the relay. Simulates manual control on the controlled relay. Defaults to Closed for all phases. Setting this property to {Open | Closed} "
	           "sets the actual state to the specified value for all phases (ganged operation). \"Open\" causes the controlled element or respective phase to open and lock out. \"Closed\" causes the "
	           "controlled element or respective phase to close and the relay to reset to its first operation.");
    AddProperty("DOC_TiltAngleLow", 41, "Tilt angle for low-current trip line. Default is 90.");
    AddProperty("DOC_TiltAngleHigh", 42, "Tilt angle for high-current trip line. Default is 90.");
    AddProperty("DOC_TripSettingLow", 43, "Resistive trip setting for low-current line.  Default is 0.");
    AddProperty("DOC_TripSettingHigh", 44, "Resistive trip setting for high-current line.  Default is -1 (deactivated). To activate, set a positive value. Must be greater than \"DOC_TripSettingLow\".");
    AddProperty("DOC_TripSettingMag", 45, "Trip setting for current magnitude (defines a circle in the relay characteristics). Default is -1 (deactivated). To activate, set a positive value.");
    AddProperty("DOC_DelayInner", 46, "Trip time delay (sec) for operation in inner region for DOC relay, defined when \"DOC_TripSettingMag\" or \"DOC_TripSettingHigh\" are activate. Default is -1.0 (deactivated), meaning that "
        "the relay characteristic is insensitive in the inner region (no trip). Set to 0 for instantaneous trip and >0 for a definite time delay. "
        "If \"DOC_PhaseCurveInner\" is specified, time delay from curve is utilized instead.");
    AddProperty("DOC_PhaseCurveInner", 47, "Name of the TCC Curve object that determines the phase trip for operation in inner region for DOC relay. Must have been previously defined as a TCC_Curve object or specified as \"none\" (ignored). "
        "Default is \"none\". Multiplying the current values in the curve by the \"DOC_PhaseTripInner\" value gives the actual current.");
    AddProperty("DOC_PhaseTripInner", 48, "Multiplier for the \"DOC_PhaseCurveInner\" TCC curve.  Defaults to 1.0.");
    AddProperty("DOC_TDPhaseInner", 49, "Time dial for \"DOC_PhaseCurveInner\" TCC curve. Multiplier on time axis of specified curve. Default=1.0.");
    AddProperty("DOC_P1Blocking", 50, "{Yes/True* | No/False} Blocking element that impedes relay from tripping if balanced net three-phase active power is in the forward direction (i.e., flowing into the monitored terminal). "
		"For a delayed trip, if at any given time the reverse power flow condition stops, the tripping is reset. Default=True.");
	AddProperty("SinglePhTrip", 51, "{Yes/True | No*/False} Enables single-phase tripping and reclosing for multi-phase controlled elements. Previously locked out phases do not operate/reclose even considering multi-phase tripping. "
	           "Applies to overcurrent relays only (type=current). Ignored for other types.");
	AddProperty("SinglePhLockout", 52, "{Yes/True | No*/False} Enables single-phase lockout for multi-phase controlled elements with single-phase tripping. Does not have impact if single-phase trip is not enabled.");
	AddProperty("Lock", 53, "{Yes/True | No*/False} Controlled switch is locked in its present open / closed state or unlocked. "
	           "When locked, the relay will not respond to either a manual state change issued by the user or a state change issued internally by OpenDSS when Reseting the control. "
	           "Note this locking mechanism is different from the relay automatic lockout after specifed Shots.");
	AddProperty("Reset", 54, "{Yes/True | No*/False} If Yes, forces Reset of relay to Normal state and removes Lock independently of any internal "
	           "reset command for mode change, etc.");
	AddProperty("RatedCurrent", 55, "Controlled conducting element's continuous rated current in Amps. Defaults to 0. Not used internally for either power flow or reporting.");
	AddProperty("InterruptingRating", 56, "Controlled conducting element's rated interrupting current in Amps. Defaults to 0. Not used internally for either power flow or reporting.");

     // Deprecated properties
	AddProperty("Breakertime", 57, "DEPRECATED. See \"MechanicalDelay\" property.");
	AddProperty("Delay", 58, "DEPRECATED. See \"DefiniteTimeDelay\" property.");
	AddProperty("GroundCurve", 59, "DEPRECATED. See \"OC_GndCurve\" property.");
	AddProperty("GroundTrip", 60, "DEPRECATED. See \"OC_GndPickup\" property.");
	AddProperty("GroundInst", 61, "DEPRECATED. See \"OC_GndInst\" property.");
	AddProperty("TDGround", 62, "DEPRECATED. See \"OC_TDGnd\" property.");
	AddProperty("Phasecurve", 63, "DEPRECATED. See \"PhCurve\" property.");
	AddProperty("PhaseTrip", 64, "DEPRECATED. See \"PhPickup\" property.");
	AddProperty("PhaseInst", 65, "DEPRECATED. See \"PhInst\" property.");
	AddProperty("TDPhase", 66, "DEPRECATED. See \"TDPh\" property.");
	AddProperty("overtrip", 67, "DEPRECATED. See \"Generic_OverTrip\" property.");
	AddProperty("undertrip", 68, "DEPRECATED. See \"Generic_UnderTrip\" property.");
	AddProperty("Variable", 69, "DEPRECATED. See \"Generic_Variable\" property.");
	AddProperty("Overvoltcurve", 70, "DEPRECATED. See \"Voltage_OVCurve\" property.");
	AddProperty("Undervoltcurve", 71, "DEPRECATED. See \"Voltage_UVCurve\" property.");

	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TRelay::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new Relay and add it to Relay class list
	/*# with ActiveCircuit[ActiveActor] do */
	{

		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TRelayObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}
/*--------------------------------------------------------------------------*/

TTCC_CurveObj* TRelay::GetTccCurve(const String CurveName)
{
	TTCC_CurveObj* result = nullptr;
	if(LowerCase(CurveName) == "none")
		return result;
	result = ((TTCC_CurveObj*) TCC_CurveClass->Find(CurveName));
	if(result == nullptr)
		DoSimpleMsg(String("TCC Curve object: \"") + CurveName + "\" not found.", 380);
	return result;
}

/*--------------------------------------------------------------------------*/

int TRelay::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	int i = 0;
	String ParamName;
	String Param;

  // continue parsing WITH contents of Parser
	ActiveRelayObj = (TRelayObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveRelayObj);
	result = 0;
	/*# with ActiveRelayObj do */
	{
		auto with0 = ActiveRelayObj;
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
				with0->Set_PropertyValue((PropertyIdxMap)[ParamPointer - 1],Param);
			else
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Relay \""
	           + with0->get_Name()
	           + "\"", 381);
			if(ParamPointer > 0)
				switch((PropertyIdxMap)[ParamPointer - 1])
				{
					case 	0:
           /*internal Relay Property commands*/
					DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 382);
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
					with0->InterpretRelayType(Param);
					break;
					case 	6: case 63:
					with0->PhCurve = GetTccCurve(Param);
					break;
					case 	7: case 59:
					with0->GndCurve = GetTccCurve(Param);
					break;
					case 	8: case 64:
					with0->PhPickup = Parser[ActorID]->MakeDouble_();
					break;
					case 	9: case 60:
					with0->GndPickup = Parser[ActorID]->MakeDouble_();
					break;
					case 	10: case 65:
					with0->PhInst = Parser[ActorID]->MakeDouble_();
					break;
					case 	11: case 61:
					with0->GndInst = Parser[ActorID]->MakeDouble_();
					break;
					case 	12:
					with0->ResetTime = Parser[ActorID]->MakeDouble_();
					break;
					case 	13:
					with0->NumReclose = Parser[ActorID]->MakeInteger_() - 1;
					break;   // one less than number of shots
					case 	14:
					if(CompareText(Param, "NONE") == 0)
						with0->NumReclose = 0;
					else
						with0->NumReclose = Parser[ActorID]->ParseAsVector(4, with0->RecloseIntervals);
					break;   // max of 4 allowed
					case 	15: case 70:
					with0->OVcurve = GetTccCurve(Param);
					break;
					case 	16: case 71:
					with0->UVCurve = GetTccCurve(Param);
					break;
					case 	17:
					with0->kVBase = Parser[ActorID]->MakeDouble_();
					break;
					case 	18: case 57:
					with0->MechanicalDelay = Parser[ActorID]->MakeDouble_();
					break;
					case 	20: case 69:
					with0->MonitorVariable = LowerCase(Param);
					break;  // for pc elements
					case 	21:
					with0->PctPickup46 = Parser[ActorID]->MakeDouble_();
					break;
					case 	22:
					with0->Isqt46 = Parser[ActorID]->MakeDouble_();
					break;
					case 	23:
					with0->BaseAmps46 = Parser[ActorID]->MakeDouble_();
					break;
					case 	24: case 58:
					with0->DefiniteTimeDelay = Parser[ActorID]->MakeDouble_();
					break;
					case 	25:
					with0->PctPickup47 = Parser[ActorID]->MakeDouble_();
					break;
					case 	26: case 67:
					with0->OverTrip = Parser[ActorID]->MakeDouble_();
					break;
					case 	27: case 68:
					with0->UnderTrip = Parser[ActorID]->MakeDouble_();
					break;
					case 	28: case 66:
					with0->TDPh = Parser[ActorID]->MakeDouble_();
					break;
					case 	29: case 62:
					with0->TDGnd = Parser[ActorID]->MakeDouble_();
					break;
					case 	30:
					with0->Z1Mag = Parser[ActorID]->MakeDouble_();
					break;
					case 	31:
					with0->Z1Ang = Parser[ActorID]->MakeDouble_();
					break;
					case 	32:
					with0->Z0Mag = Parser[ActorID]->MakeDouble_();
					break;
					case 	33:
					with0->Z0Ang = Parser[ActorID]->MakeDouble_();
					break;
					case 	34:
					with0->Mphase = Parser[ActorID]->MakeDouble_();
					break;
					case 	35:
					with0->Mground = Parser[ActorID]->MakeDouble_();
					break;
					case 	36:
					with0->ShowEventLog = InterpretYesNo(Param);
					break;
					case 	37:
					with0->DebugTrace = InterpretYesNo(Param);
					break;
					case 	38:
					with0->Dist_Reverse = InterpretYesNo(Param);
					break;
					case 	39:
					{
						with0->InterpretRelayState(ActorID, Param, ParamName);  // set normal state
						if(!with0->NormalStateSet)
							with0->NormalStateSet = true;
					}
					break;
					case 	19: case 40:
					with0->InterpretRelayState(ActorID, Param, ParamName);
					break;  // set state
                    case 	41:
					with0->DOC_TiltAngleLow = Parser[ActorID]->MakeDouble_();
					break;
                    case 	42:
					with0->DOC_TiltAngleHigh = Parser[ActorID]->MakeDouble_();
					break;
                    case 	43:
					with0->DOC_TripSetLow = Parser[ActorID]->MakeDouble_();
					break;
                    case 	44:
					with0->DOC_TripSetHigh = Parser[ActorID]->MakeDouble_();
					break;
                    case 	45:
					with0->DOC_TripSetMag = Parser[ActorID]->MakeDouble_();
					break;
                    case 	46:
					with0->DOC_DelayInner = Parser[ActorID]->MakeDouble_();
					break;
                    case 	47:
					with0->DOC_PhaseCurveInner = GetTccCurve(Param);
					break;
                    case 	48:
					with0->DOC_PhaseTripInner = Parser[ActorID]->MakeDouble_();
					break;
                    case 	49:
					with0->DOC_TDPhaseInner = Parser[ActorID]->MakeDouble_();
					break;
                    case 	50:
					with0->DOC_P1Blocking = InterpretYesNo(Param);
					break;
                    case 	51:
					with0->SinglePhTrip = InterpretYesNo(Param);
					break;
                    case 	52:
					with0->SinglePhLockout = InterpretYesNo(Param);
					break;
                    case 	53:
					with0->set_Flocked(InterpretYesNo(Param));
					break;
                    case 	54:
					if(InterpretYesNo(Param))   // force a reset
					{
						with0->set_Flocked(false);
						with0->Reset(ActorID);
						with0->Set_PropertyValue(54, "n");
					}
					break;
                    case 	55:
					with0->RatedCurrent = Parser[ActorID]->MakeDouble_();
					break;
                    case 	56:
					with0->InterruptingRating = Parser[ActorID]->MakeDouble_();
					break;
           // Inherited parameters
					default:
					ClassEdit(ActiveRelayObj, ParamPointer - NumPropsThisClass);
					break;
				}
			if(ParamPointer > 0)
				switch((PropertyIdxMap)[ParamPointer - 1])
				{
					case 	1:
              /*Default the controlled element to the monitored element*/
					with0->ElementName = with0->MonitoredElementName;
					break;
					case 	2:
					with0->ElementTerminal = with0->MonitoredElementTerminal;
					break;        /*Set Default Reclose Intervals*/
					case 	5:
					{
						switch(LowerCase(Param)[0])
						{
							case 	L'c':
							with0->Set_PropertyValue(14,"[0.5, 2.0, 2.0]");
							break;
							case 	L'v':
							with0->Set_PropertyValue(14,"[5.0]");
							break;
							case 	'd':
							switch(Param.size() >= 2 ? LowerCase(Param)[1] : '\0')
							{
								case 	'o':
								with0->Set_PropertyValue(14,"NONE");
								with0->NumReclose = 0;
								break;
								default:
								break;
							}
							break;
							default:
							  ;
							break;
						}
						if (with0->Get_PropertyValue(14) != "NONE")
						{
							AuxParser[ActorID]->SetCmdString(with0->Get_PropertyValue(14));
							ParamName = AuxParser[ActorID]->GetNextParam();
							with0->NumReclose = AuxParser[ActorID]->ParseAsVector(4, with0->RecloseIntervals);
						}
						// Side-effect: disable single-phase tripping and lockout.
						switch(LowerCase(Param)[0])
						{
							case 	L'4': case L'g': case L't': case L'd': case L'v': case L'r':
							{
								with0->SinglePhTrip = false;
								with0->SinglePhLockout = false;
								with0->Set_PropertyValue(51, "No");
								with0->Set_PropertyValue(52, "No");
							}
							break;
							default:
							  ;
							break;
						}
					}
					break;
					case 	19: case 40:
					if(!with0->NormalStateSet)
					{
						int stop = 0;
						for(stop = ((TDSSCktElement*) with0)->Fnphases, i = 1; i <= stop; i++)
						{
							if(!with0->NormalStateSet)
								(*with0->FNormalState)[i - 1] = (*with0->FPresentState)[i - 1];
						}
						with0->NormalStateSet = true;  // normal state will default to state only the 1st time state is specified.
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

int TRelay::MakeLike(const String RelayName)
{
	int result = 0;
	TRelayObj* OtherRelay = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Relay name in the present collection*/
	OtherRelay = ((TRelayObj*) Find(RelayName));
	if(OtherRelay != nullptr)
		/*# with ActiveRelayObj do */
		{
			auto with0 = ActiveRelayObj;
			int stop = 0;
			with0->Set_NPhases(OtherRelay->Fnphases);
			with0->Set_Nconds(OtherRelay->Fnconds); // Force Reallocation of terminal stuff
			with0->ShowEventLog = OtherRelay->ShowEventLog; // but leave DebugTrace off
			with0->ElementName = OtherRelay->ElementName;
			with0->ElementTerminal = OtherRelay->ElementTerminal;
			with0->Set_ControlledElement(OtherRelay->get_FControlledElement());  // Pointer to target circuit element
			with0->Set_MonitoredElement(OtherRelay->get_FMonitoredElement());  // Pointer to target circuit element
			with0->MonitoredElementName = OtherRelay->MonitoredElementName;  // Pointer to target circuit element
			with0->MonitoredElementTerminal = OtherRelay->MonitoredElementTerminal;  // Pointer to target circuit element
			with0->PhCurve = OtherRelay->PhCurve;
			with0->GndCurve = OtherRelay->GndCurve;
			with0->OVcurve = OtherRelay->OVcurve;
			with0->UVCurve = OtherRelay->UVCurve;
			with0->PhPickup = OtherRelay->PhPickup;
			with0->GndPickup = OtherRelay->GndPickup;
			with0->TDPh = OtherRelay->TDPh;
			with0->TDGnd = OtherRelay->TDGnd;
			with0->PhInst = OtherRelay->PhInst;
			with0->GndInst = OtherRelay->GndInst;
			with0->ResetTime = OtherRelay->ResetTime;
			with0->NumReclose = OtherRelay->NumReclose;
			with0->DefiniteTimeDelay = OtherRelay->DefiniteTimeDelay;
			with0->MechanicalDelay = OtherRelay->MechanicalDelay;
			with0->SinglePhTrip = OtherRelay->SinglePhTrip;
			with0->SinglePhLockout = OtherRelay->SinglePhLockout;
			with0->RatedCurrent = OtherRelay->RatedCurrent;
			with0->InterruptingRating = OtherRelay->InterruptingRating;
			with0->RecloseIntervals = (pDoubleArray) realloc(with0->RecloseIntervals, sizeof(double) * 4);      // Always make a max of 4
			for(stop = with0->NumReclose, i = 1; i <= stop; i++)
			{
				(with0->RecloseIntervals)[i - 1] = (OtherRelay->RecloseIntervals)[i - 1];
			}
       // deleted... if DebugTrace then AppendToEventLog ('Relay.'+self.Name, Format ('MakeLike NumReclose=%d',[NumReclose]), ActorID);
			with0->set_Flocked(OtherRelay->get_FLocked());
			for(stop = min(RELAYCONTROLMAXDIM, with0->get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
			{
				(*with0->FPresentState)[i - 1] = (*OtherRelay->FPresentState)[i - 1];
				(*with0->FNormalState)[i - 1] = (*OtherRelay->FNormalState)[i - 1];
			}
			with0->kVBase = OtherRelay->kVBase;
			with0->ControlType = OtherRelay->ControlType;
			with0->CondOffset = OtherRelay->CondOffset;

        /*46 Relay  Neg Seq Current*/
			with0->PickupAmps46 = OtherRelay->PickupAmps46;
			with0->PctPickup46 = OtherRelay->PctPickup46;
			with0->BaseAmps46 = OtherRelay->BaseAmps46;
			with0->Isqt46 = OtherRelay->Isqt46;

        /*47 Relay*/
			with0->PickupVolts47 = OtherRelay->PickupVolts47;
			with0->PctPickup47 = OtherRelay->PctPickup47;

        /*Generic Relay*/
			with0->MonitorVariable = OtherRelay->MonitorVariable;
			with0->OverTrip = OtherRelay->OverTrip;
			with0->UnderTrip = OtherRelay->UnderTrip;

        /*Distance Relays*/
			with0->Z1Mag = OtherRelay->Z1Mag;
			with0->Z1Ang = OtherRelay->Z1Ang;
			with0->Z0Mag = OtherRelay->Z0Mag;
			with0->Z0Ang = OtherRelay->Z0Ang;
			with0->Mphase = OtherRelay->Mphase;
			with0->Mground = OtherRelay->Mground;
			with0->Dist_Reverse = OtherRelay->Dist_Reverse;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherRelay->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in Relay MakeLike: \"") + RelayName + "\" Not Found.", 383);
	return result;
}



/*==========================================================================*/
/*                    TRelayObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TRelayObj::TRelayObj(TDSSClass* ParClass, const String RelayName)
 : inherited(ParClass)
{
	int i = 0;
	int stop = 0;
	Set_Name(LowerCase(RelayName));
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
	ControlType = Current;
	PhCurve = nullptr;
	GndCurve = nullptr;
	OVcurve = nullptr;
	UVCurve = nullptr;
	PhPickup = 1.0;
	GndPickup = 1.0;
	TDPh = 1.0;
	TDGnd = 1.0;
	PhInst = 0.0;
	GndInst = 0.0;
	ResetTime = 15.0;
	RatedCurrent = 0.0;
	InterruptingRating = 0.0;
	DefiniteTimeDelay = 0.0;
	MechanicalDelay = 0.0;
	NumReclose = 3;
	RecloseIntervals = nullptr;
	RecloseIntervals = new double[4]; // fixed allocation of 4
	(RecloseIntervals)[1 - 1] = 0.5;
	(RecloseIntervals)[2 - 1] = 2.0;
	(RecloseIntervals)[3 - 1] = 2.0;
	Isqt46 = 1.0;
	BaseAmps46 = 100.0;
	PctPickup46 = 20.0;
	PickupAmps46 = BaseAmps46 * PctPickup46 * 0.01;
	PctPickup47 = 2.0;
	OverTrip = 1.2;
	UnderTrip = 0.8;
	Z1Mag = 0.7;
	Z1Ang = 64.0;
	Z0Mag = 2.1;
	Z0Ang = 68.0;
	Mphase = 0.7;
	Mground = 0.7;
	td21_i = -1;
	td21_h = nullptr;
	td21_dV = nullptr;
	td21_Uref = nullptr;
	td21_dI = nullptr;
	td21_pt = 0;
	td21_stride = 0;
	td21_quiet = 0;
	Dist_Reverse = false;
    DOC_TiltAngleLow = 90.0;
    DOC_TiltAngleHigh = 90.0;
    DOC_TripSetLow = 0;
    DOC_TripSetHigh = -1.0;
    DOC_TripSetMag = -1.0;
    DOC_DelayInner = -1.0;
    DOC_PhaseCurveInner = nullptr;
    DOC_PhaseTripInner = 1.0;
    DOC_TDPhaseInner = 1.0;
    DOC_P1Blocking = true;

	FPresentState = nullptr;
	FNormalState = nullptr;
	LockedOut.clear();
	ArmedForOpen.clear();
	ArmedForClose.clear();
	ArmedForReset.clear();
	GroundTarget = false;
	PhaseTarget.clear();
	OperationCount = nullptr;
	RelayTarget = nullptr;
	SinglePhTrip = false;
	SinglePhLockout = false;
	IdxMultiPh = Get_NPhases() + 1;

     // Reallocate arrays  (Must be initialized to nil for first call)
	FPresentState = (pStateArray) realloc(FPresentState, sizeof((*FPresentState)[1 - 1]) * Fnphases);
	FNormalState = (pStateArray) realloc(FNormalState, sizeof((*FNormalState)[1 - 1]) * Fnphases);
	LockedOut.resize(IdxMultiPh);
	ArmedForOpen.resize(IdxMultiPh);
	ArmedForClose.resize(IdxMultiPh);
	ArmedForReset.resize(IdxMultiPh);
	PhaseTarget.resize(IdxMultiPh);
	OperationCount = (pIntegerArray) realloc(OperationCount, sizeof(long) * IdxMultiPh);
	RelayTarget = AllocStringArray(IdxMultiPh);

	for(stop = min(RELAYCONTROLMAXDIM, IdxMultiPh), i = 1; i <= stop; i++)
	{
		if(i <= Get_NPhases())
		{
			(*FPresentState)[i - 1] = CTRL_CLOSE;
			(*FNormalState)[i - 1] = CTRL_CLOSE;  // default to present state;
		}
		LockedOut[i - 1] = false;
		ArmedForOpen[i - 1] = false;
		ArmedForClose[i - 1] = false;
		ArmedForReset[i - 1] = false;
		PhaseTarget[i - 1] = false;
		OperationCount[i - 1] = 1;
		RelayTarget[i - 1] = "";
	}

	NormalStateSet = false;
	set_Flocked(false);
	NextTriptime = -1.0;  // not set to trip
	cBuffer = nullptr; // Complex buffer
	cvBuffer = nullptr;
	DSSObjType = ParClass->DSSClassType; //cap_CONTROL;
	InitPropertyValues(0);



   //  RecalcElementData;
}

TRelayObj::~TRelayObj()
{
	MonitoredElementName = "";
	free(RecloseIntervals);
	if(ASSIGNED(cBuffer))
		free(cBuffer);
	if(ASSIGNED(cvBuffer))
		free(cvBuffer);
	if(ASSIGNED(td21_h))
		free(td21_h);
	if(ASSIGNED(td21_dV))
		free(td21_dV);
	if(ASSIGNED(td21_Uref))
		free(td21_Uref);
	if(ASSIGNED(td21_dI))
		free(td21_dI);
	FPresentState = (pStateArray) realloc(FPresentState, 0);
	FNormalState = (pStateArray) realloc(FNormalState, 0);
	LockedOut.clear();
	ArmedForOpen.clear();
	ArmedForClose.clear();
	ArmedForReset.clear();
	PhaseTarget.clear();
	OperationCount = (pIntegerArray) realloc(OperationCount, 0);
	FreeStringArray(RelayTarget, IdxMultiPh);
	// inherited::Destroy();
}


/*--------------------------------------------------------------------------*/

void TRelayObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	int i = 0;
	DevIndex = GetCktElementIndex(MonitoredElementName); // Global function
	if(DevIndex > 0)
	{
		Set_MonitoredElement((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		Set_NPhases(get_FMonitoredElement()->Get_NPhases());       // Force number of phases to be same
		if(MonitoredElementTerminal > get_FMonitoredElement()->Get_NTerms())
		{
			DoErrorMsg(String("Relay: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Re-specify terminal no.", 384);
		}
		else

               // Sets name of i-th terminal's connected bus in Relay's buslist
		{
			SetBus(1, get_FMonitoredElement()->GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
			cBuffer = (pComplexArray) realloc(cBuffer, sizeof(complex) * get_FMonitoredElement()->Yorder);
			if((ControlType == Distance) || (ControlType == TD21) || (ControlType == DOC))
				cvBuffer = (pComplexArray)realloc(cvBuffer, sizeof(complex) * get_FMonitoredElement()->Yorder);
			CondOffset = (MonitoredElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
			switch(ControlType)
			{
				case 	GENERIC:
				{
					if(( ( (TDSSObject*) get_FMonitoredElement() )->DSSObjType & BaseClassMask) != PC_ELEMENT)
						DoSimpleMsg(String("Relay ") + get_Name() + ": Monitored element for Generic relay is not a PC Element.", 385);
					else
					{
						MonitorVarIndex = ((TPCElement*) get_FMonitoredElement())->LookupVariable(MonitorVariable);
						if(MonitorVarIndex < 1)    // oops
						{
							DoSimpleMsg(String("Relay ") + get_Name()
	           + ": Monitor variable \""
	           + MonitorVariable
	           + "\" does not exist.", 386);
						}
					}
				}
				break;
				default:
				  ;
				break;
			}
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
		Set_ControlledElement((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Make the 1 st terminal active

             // If the relay becomes disabled, leave at False
		if(Get_Enabled())
		{
			get_FControlledElement()->HasOCPDevice = true;  // For Reliability calcs
			get_FControlledElement()->HasAutoOCPDevice = true;  // For Reliability calcs
		}

		int stop = 0;
		// Open/Closed State of controlled element based on state assigned to the control
		for(stop = min(RELAYCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			if((*FPresentState)[i - 1] == CTRL_CLOSE)
			{
				get_FControlledElement()->Set_ConductorClosed(i, ActorID, true);
				LockedOut[i - 1] = false;
				OperationCount[i - 1] = 1;
				ArmedForOpen[i - 1] = false;
			}
			else
			{
				get_FControlledElement()->Set_ConductorClosed(i, ActorID, false);
				LockedOut[i - 1] = true;
				OperationCount[i - 1] = NumReclose + 1;
				ArmedForClose[i - 1] = false;
			}
		}
	}
	else
	{
		Set_ControlledElement(nullptr);   // element not found
		DoErrorMsg(String("Relay: \"") + this->get_Name() + "\"", String("CktElement Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 387);
	}

         /*Misc stuff*/
	PickupAmps46 = BaseAmps46 * PctPickup46 * 0.01;
	switch(Fnphases)
	{
		case 	1:
		VBase = kVBase * 1000.0;
		break;
		default:
		VBase = kVBase / SQRT3 * 1000.0;
		break;
	}
	PickupVolts47 = VBase * PctPickup47 * 0.01;
	if((ControlType == Distance) || (ControlType == TD21))
	{
		Dist_Z1 = pclx(Z1Mag, Z1Ang / RadiansToDegrees);
		Dist_Z0 = pclx(Z0Mag, Z0Ang / RadiansToDegrees);
		Dist_K0 = cdiv(cdivreal(csub(Dist_Z0, Dist_Z1), 3.0), Dist_Z1);
	}
}

void TRelayObj::MakePosSequence(int ActorID)
{
	if(get_FMonitoredElement() != nullptr)
	{
		Set_NPhases(get_FMonitoredElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
    // Allocate a buffer big enough to hold everything from the monitored element
		cBuffer = (pComplexArray)realloc(cBuffer, sizeof(complex) * get_FMonitoredElement()->Yorder);
		if((ControlType == Distance) || (ControlType == TD21) || (ControlType == DOC))
			cvBuffer = (pComplexArray)realloc(cvBuffer, sizeof(complex) * get_FMonitoredElement()->Yorder);
		CondOffset = (ElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
	}
	switch(Fnphases)
	{
		case 	1:
		VBase = kVBase * 1000.0;
		break;
		default:
		VBase = kVBase / SQRT3 * 1000.0;
		break;
	}
	PickupVolts47 = VBase * PctPickup47 * 0.01;
	inherited::MakePosSequence(ActorID);
}

/*--------------------------------------------------------------------------*/

void TRelayObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}

/*--------------------------------------------------------------------------*/

void TRelayObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}
/*--------------------------------------------------------------------------*/

void TRelayObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TRelayObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	int i = 0;
	int PhIdx = 0;
	String ph_debug;
	if(SinglePhTrip)
		PhIdx = ProxyHdl;
	else
		PhIdx = IdxMultiPh; // Proxy holds phase information for single-phase trip
	if(DebugTrace)
	{
		if(SinglePhTrip)
			ph_debug = IntToStr(PhIdx);
		else
			ph_debug = "ALL";
		AppendToEventLog(String("Relay.") + this->get_Name(),
		Format("Debug DoPendingAction Code=%d Phase=%s State=%s ArmedOpen=%s ArmedForClose=%s ArmedForReset=%s Count=%d NumReclose=%d",
		Code, ph_debug.c_str(), this->GetPropertyValue(40).c_str(), BoolToStr(ArmedForOpen[PhIdx - 1], true).c_str(), BoolToStr(ArmedForClose[PhIdx - 1], true).c_str(), BoolToStr(ArmedForReset[PhIdx - 1], true).c_str(),
		OperationCount[PhIdx - 1], NumReclose), ActorID);
	}
	/*# with ControlledElement do */
	{
		auto with0 = get_FControlledElement();
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal of CktElement to terminal 1
		switch(Code)
		{
			case ( CTRL_OPEN):
			if(SinglePhTrip)
			{
				switch((*FPresentState)[PhIdx - 1])
				{
					case 	CTRL_CLOSE:
					if(ArmedForOpen[PhIdx - 1])   // ignore if we became disarmed in meantime
					{
						get_FControlledElement()->Set_ConductorClosed(PhIdx, ActorID, false);   // Open phase of active terminal
						(*FPresentState)[PhIdx - 1] = CTRL_OPEN;
						if(OperationCount[PhIdx - 1] > NumReclose)
						{
							LockedOut[PhIdx - 1] = true;
							if(SinglePhLockout)
							{
								if(ShowEventLog)
									AppendToEventLog(String("Relay.") + this->get_Name(), Format("Phase %d opened on %s (1ph trip) & locked out (1ph lockout)", PhIdx, RelayTarget[PhIdx - 1].c_str()), ActorID);
							}
							else
							{
								if(ShowEventLog)
									AppendToEventLog(String("Relay.") + this->get_Name(), Format("Phase %d opened on %s (1ph trip) & locked out (3ph lockout)", PhIdx, RelayTarget[PhIdx - 1].c_str()), ActorID); // 3-Phase Lockout
								// Lockout other phases
								int stop = 0;
								for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
								{
									if((i != PhIdx) && (!LockedOut[i - 1]))  // Check LockedOut[i - 1] to skip individual phase that were previously locked out
									{
										get_FControlledElement()->Set_ConductorClosed(i, ActorID, false);
										(*FPresentState)[i - 1] = CTRL_OPEN;
										LockedOut[i - 1] = true;
										if(ArmedForOpen[i - 1])
											ArmedForOpen[i - 1] = false;
										if(ShowEventLog)
											AppendToEventLog(String("Relay.") + this->get_Name(), Format("Phase %d opened (1ph trip) & locked out (3ph lockout)", i), ActorID);
									}
								}
							}
						}
						else if(ShowEventLog)
							AppendToEventLog(String("Relay.") + this->get_Name(), Format("Phase %d opened on %s (1ph trip)", PhIdx, RelayTarget[PhIdx - 1].c_str()), ActorID);
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
				// Analyze each phase separately even if using 3-phase trip as states may not be the same.
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
							if(OperationCount[PhIdx - 1] > NumReclose)
							{
								LockedOut[PhIdx - 1] = true;
								if(ShowEventLog)
									AppendToEventLog(String("Relay.") + this->get_Name(), Format("Phase %d opened on %s (3ph trip) & locked out (3ph lockout)", i, RelayTarget[PhIdx - 1].c_str()), ActorID);
							}
							else if(ShowEventLog)
								AppendToEventLog(String("Relay.") + this->get_Name(), Format("Phase %d opened on %s (3ph trip)", i, RelayTarget[PhIdx - 1].c_str()), ActorID);
						}
						break;
						default:
						  ;
						break;
					}
				}
				ArmedForOpen[PhIdx - 1] = false; // Report target only once for 3ph trip.
				if(ControlType == TD21)
					td21_quiet = td21_pt + 1;
			}
			break;
			case ( CTRL_CLOSE):
			if(SinglePhTrip)
			{
				switch((*FPresentState)[PhIdx - 1])
				{
					case 	CTRL_OPEN:
					if(ArmedForClose[PhIdx - 1] && !LockedOut[PhIdx - 1])
					{
						get_FControlledElement()->Set_ConductorClosed(PhIdx, ActorID, true);    // Close phase of active terminal
						(*FPresentState)[PhIdx - 1] = CTRL_CLOSE;
						if(ShowEventLog)
							AppendToEventLog(String("Relay.") + this->get_Name(), Format("Phase %d closed (1ph reclosing)", PhIdx), ActorID);
						// Count reclosing operations for each phase on single ph trip
						++OperationCount[PhIdx - 1];
						ArmedForClose[PhIdx - 1] = false;
					}
					break;
					default:
					  ;
					break;
				}
			}
			else // 3-Ph Trip
			{
				int stop = 0;
				for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					switch((*FPresentState)[i - 1])
					{   // Check LockedOut[i - 1] to skip individual phases that were previously locked out
						case 	CTRL_OPEN:
						if(ArmedForClose[PhIdx - 1] && !LockedOut[i - 1] && !LockedOut[PhIdx - 1])
						{
							get_FControlledElement()->Set_ConductorClosed(i, ActorID, true);    // Close phases of active terminal
							(*FPresentState)[i - 1] = CTRL_CLOSE;
							if(ShowEventLog)
								AppendToEventLog(String("Relay.") + this->get_Name(), Format("Phase %d closed (3ph reclosing)", i), ActorID);
						}
						break;
						default:
						  ;
						break;
					}
				}
				ArmedForClose[PhIdx - 1] = false;
				++OperationCount[PhIdx - 1];
				if(ControlType == TD21)
					td21_quiet = (td21_pt / 2);
			}
			break;
			case ( CTRL_RESET):
			if(SinglePhTrip)
			{
				switch((*FPresentState)[PhIdx - 1])
				{
					case 	CTRL_CLOSE:
					if(!ArmedForOpen[PhIdx - 1]) // Don't reset if we just rearmed
					{
						OperationCount[PhIdx - 1] = 1;
						if(ShowEventLog)
							AppendToEventLog(String("Relay.") + this->get_Name(), Format("Phase %d reset (1ph reset)", PhIdx), ActorID);
					}
					break;
					default:
					  ;
					break;
				}
			}
			else // 3-Phase Trip
			{
				int stop = 0;
				for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					if((*FPresentState)[i - 1] == CTRL_CLOSE)
					{
						if(!ArmedForOpen[PhIdx - 1])
						{
							OperationCount[PhIdx - 1] = 1;       // Don't reset if we just rearmed
							if(ShowEventLog)
								AppendToEventLog(String("Relay.") + this->get_Name(), "Phase ALL reset (3ph reset)", ActorID);
						}
						break;  // no need to loop at all closed phases
					}
				}
				if(ArmedForReset[PhIdx - 1] && !LockedOut[PhIdx - 1])
				{
					if(ControlType == TD21)
						td21_quiet = (td21_pt / 2);
				}
			}
			break;
			default:
			  ;
			break;
		}
	}
}

/*--------------------------------------------------------------------------*/

void TRelayObj::InterpretRelayState(int ActorID, const String Action, const String property_name)
{
	int i = 0;
	string DataStr1 = "";
	string DataStr2 = "";
    // Only allowed to change normal state if locked.
	if(get_FLocked() && ((LowerCase(property_name)[0] == 'a') || (LowerCase(property_name)[0] == 's')))
		return;
	if(LowerCase(property_name)[0] == 'a') // Interpret ganged specification to state when using action
	{ // action (deprecated) will be removed
		int stop = 0;
		for(stop = RELAYCONTROLMAXDIM, i = 1; i <= stop; i++)
		{
			switch(LowerCase(Action)[0])
			{
				case 	L'o':
				set_States(i, CTRL_OPEN);
				break;
				case 	L'c':
				set_States(i, CTRL_CLOSE);
				break;
				default:
				  ;
				break;
			}
		}
	}
	else
	{
		if(!Parser[ActorID]->IsQuotedString) // Interpret ganged specification to state and normal when not quoted
		{
			int stop = 0;
			for(stop = RELAYCONTROLMAXDIM, i = 1; i <= stop; i++)
			{
				if(LowerCase(property_name)[0] == 's')  // state
				{
					switch(LowerCase(Action)[0])
					{
						case 	L'o':
						set_States(i, CTRL_OPEN);
						break;
						case 	L'c':
						set_States(i, CTRL_CLOSE);
						break;
						default:
						  ;
						break;
					}
				}
				else // 'normal
				{
					switch(LowerCase(Action)[0])
					{
						case 	L'o':
						set_NormalStates(i, CTRL_OPEN);
						break;
						case 	L'c':
						set_NormalStates(i, CTRL_CLOSE);
						break;
						default:
						  ;
						break;
					}
				}
			}
		}
		else // process phase by phase
		{
			AuxParser[ActorID]->SetCmdString(Action);  // Load up Parser
			DataStr1 = AuxParser[ActorID]->GetNextParam();  // ignore
			DataStr2 = AuxParser[ActorID]->MakeString_();
			i = 1;
			while((DataStr2.size() > 0) && (i < RELAYCONTROLMAXDIM))
			{
				if(LowerCase(property_name)[0] == 's')  // state
				{
					switch(LowerCase(DataStr2)[0])
					{
						case 	L'o':
						set_States(i, CTRL_OPEN);
						break;
						case 	L'c':
						set_States(i, CTRL_CLOSE);
						break;
						default:
						  ;
						break;
					}
				}
				else // 'normal'
				{
					switch(LowerCase(DataStr2)[0])
					{
						case 	L'o':
						set_NormalStates(i, CTRL_OPEN);
						break;
						case 	L'c':
						set_NormalStates(i, CTRL_CLOSE);
						break;
						default:
						  ;
						break;
					}
				}
				DataStr1 = AuxParser[ActorID]->GetNextParam();  // ignore
				DataStr2 = AuxParser[ActorID]->MakeString_();
				++i;
			}
		}
	}
}

/*--------------------------------------------------------------------------*/

void TRelayObj::sample(int ActorID)
{
	int i = 0;
	int stop = 0;
	get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);
	// Check state of phases of active terminal as they could have changed through other mechanisms
	for(stop = min(RELAYCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
	{
		if(get_FControlledElement()->Get_ConductorClosed(i, ActorID))
			(*FPresentState)[i - 1] = CTRL_CLOSE;
		else
			(*FPresentState)[i - 1] = CTRL_OPEN;
	}
	if(DebugTrace)
		AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("FPresentState: %s ", this->GetPropertyValue(40).c_str()), ActorID);
	switch(ControlType)
	{
		case 	Current:
		OvercurrentLogic(ActorID);
		break; /*Current*/
		case 	VOLTAGE:
		VoltageLogic(ActorID);
		break; /*Reclosing Voltage Relay - definite time*/
		case 	REVPOWER:
		RevPowerLogic(ActorID);
		break;    // one shot to lockout
		case 	NEGCURRENT:
		NegSeq46Logic(ActorID);
		break; // one shot to lockout
		case 	NEGVOLTAGE:
		NegSeq47Logic(ActorID);
		break; // one shot to lockout
		case 	GENERIC:
		GenericLogic(ActorID);
		break;// one shot to lockout
		case 	Distance:
		DistanceLogic(ActorID);
		break;
		case 	TD21:
		TD21Logic(ActorID);
		break;
		case 	DOC:
		DirectionalOvercurrentLogic(ActorID);
		break;
		default:
		  ;
		break;
	}
}

/*--------------------------------------------------------------------------*/

void TRelayObj::Reset(int ActorID)
{
	int i = 0;
	if(!get_FLocked() && (get_FControlledElement() != nullptr))
	{
		if(ShowEventLog)
			AppendToEventLog(String("Relay.") + this->get_Name(), "Resetting", ActorID);
		NextTriptime = -1.0;  // not set to trip
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
		int stop = 0;
		for(stop = min(RELAYCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			(*FPresentState)[i - 1] = (*FNormalState)[i - 1];  // reset to normal state
			ArmedForOpen[i - 1] = false;
			ArmedForClose[i - 1] = false;
			ArmedForReset[i - 1] = false;
			GroundTarget = false;
			PhaseTarget[i - 1] = false;
			switch((*FNormalState)[i - 1])
			{
				case 	CTRL_OPEN:
				{
					get_FControlledElement()->Set_ConductorClosed(i, ActiveActor, false); // Open all phases of active terminal
					LockedOut[i - 1] = true;
					OperationCount[i - 1] = NumReclose + 1;
				}
				break;
           /*CTRL_CLOSE*/
				default:
				get_FControlledElement()->Set_ConductorClosed(i, ActiveActor, true);    // Close all phases of active terminal
				LockedOut[i - 1] = false;
				OperationCount[i - 1] = 1;
				break;
			}
		}
	}
}

void TRelayObj::set_Flocked(bool Value)
{
	FLocked = Value;
}

bool TRelayObj::get_FLocked()
{
	return FLocked;
}

EControlAction TRelayObj::get_States(int Idx)
{
	EControlAction result;
	if(get_FControlledElement() != nullptr)
	{
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
		if(get_FControlledElement()->Get_ConductorClosed(Idx, ActiveActor))
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

void TRelayObj::set_States(int Idx, const EControlAction Value)
{
	if(get_States(Idx) != Value)
	{
		if(get_FControlledElement() != nullptr)
		{
			get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
			switch(Value)
			{
				case 	CTRL_OPEN:
				get_FControlledElement()->Set_ConductorClosed(Idx, ActiveActor, false);
				LockedOut[Idx - 1] = true;
				OperationCount[Idx - 1] = NumReclose + 1;
				ArmedForClose[Idx - 1] = false;
				ArmedForReset[Idx - 1] = false;
				break;
                /*CTRL_CLOSE:*/
				default:
				get_FControlledElement()->Set_ConductorClosed(Idx, ActiveActor, true);
				LockedOut[Idx - 1] = false;
				OperationCount[Idx - 1] = 1;
				ArmedForOpen[Idx - 1] = false;
				ArmedForReset[Idx - 1] = false;
				break;
			}
		}
		(*FPresentState)[Idx - 1] = Value;
	}
}

EControlAction TRelayObj::get_NormalStates(int Idx)
{
	EControlAction result;
	result = (*FNormalState)[Idx - 1];
	return result;
}

void TRelayObj::set_NormalStates(int Idx, const EControlAction Value)
{
	if((*FNormalState)[Idx - 1] != Value)
	{
		(*FNormalState)[Idx - 1] = Value;
	}
}

/*Note PropertyValue is aligned with the internal indices*/

void TRelayObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue((with0->PropertyIdxMap)[i - 1])); }
		}
	}
	if(Complete)
	{
		WriteLn(f);
	}
}

String TRelayObj::GetPropertyValue(int Index)
{
	String result;
	int i = 0;
	switch(Index)
	{
		case 	39: case 40:
		result = "[";
		break;
		default:
		result = "";
		break;
	}
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		switch(Index)
		{
			case 	6: case 63:
			if(PhCurve != nullptr)
				result = PhCurve->get_Name();
			else
				result = "none";
			break;
			case 	7: case 59:
			if(GndCurve != nullptr)
				result = GndCurve->get_Name();
			else
				result = "none";
			break;
			case 	8: case 64:
			result = Format("%.3f", PhPickup);
			break;
			case 	9: case 60:
			result = Format("%.3f", GndPickup);
			break;
			case 	10: case 65:
			result = Format("%.3f", PhInst);
			break;
			case 	11: case 61:
			result = Format("%.3f", GndInst);
			break;
			case 	15: case 70:
			if(OVcurve != nullptr)
				result = OVcurve->get_Name();
			else
				result = "none";
			break;
			case 	16: case 71:
			if(UVCurve != nullptr)
				result = UVCurve->get_Name();
			else
				result = "none";
			break;
			case 	18: case 57:
			result = Format("%.3f", MechanicalDelay);
			break;
			case 	20: case 69:
			result = MonitorVariable;
			break;
			case 	24: case 58:
			result = Format("%.3f", DefiniteTimeDelay);
			break;
			case 	26: case 67:
			result = Format("%.3f", OverTrip);
			break;
			case 	27: case 68:
			result = Format("%.3f", UnderTrip);
			break;
			case 	28: case 66:
			result = Format("%.3f", TDPh);
			break;
			case 	29: case 62:
			result = Format("%.3f", TDGnd);
			break;
			case 	13:
			result = Format("%d", NumReclose + 1);
			break;
			case 	14:
			{
				if(NumReclose == 0)
					result = result + "NONE";
				else
				{
					int stop = 0;
					result = "(";
					for(stop = NumReclose, i = 1; i <= stop; i++)
					{
						result = result + Format("%-g, ", (RecloseIntervals)[i - 1]);
					}
					result = result + ")";
				}
			}
			break;
			case 	39:
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
			case 	40:
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
			case 	53:
			if(get_FLocked())
				result = "Yes";
			else
				result = "No";
			break;
			case 	55:
			result = Format("%-.6g", RatedCurrent);
			break;
			case 	56:
			result = Format("%-.6g", InterruptingRating);
			break;
			default:
			result = inherited::GetPropertyValue(Index);
			break;
		}
	}
	switch(Index)
	{
		case 	39: case 40:
		result = result + "]";
		break;
		default:
		break;
	}
	return result;
}

void TRelayObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,""); //'element';
	Set_PropertyValue(2,"1"); //'terminal';
	Set_PropertyValue(3,"");
	Set_PropertyValue(4,"1"); //'terminal';
	Set_PropertyValue(5,"current");
	Set_PropertyValue(6,"none");
	Set_PropertyValue(7,"none");
	Set_PropertyValue(8,"1.0");
	Set_PropertyValue(9,"1.0");
	Set_PropertyValue(10,"0.0");
	Set_PropertyValue(11,"0.0");
	Set_PropertyValue(12,"15");
	Set_PropertyValue(13,"4");
	Set_PropertyValue(14,"(0.5, 2.0, 2.0)");
	Set_PropertyValue(15,"none");
	Set_PropertyValue(16,"none");
	Set_PropertyValue(17,"0.0");
	Set_PropertyValue(18,"0.0");
	Set_PropertyValue(19,"closed");
	Set_PropertyValue(20,"");
	Set_PropertyValue(21,"20");
	Set_PropertyValue(22,"1");
	Set_PropertyValue(23,"100");
	Set_PropertyValue(24,"0");
	Set_PropertyValue(25,"2");
	Set_PropertyValue(26,"1.2");
	Set_PropertyValue(27,"0.8");
	Set_PropertyValue(28,"1.0");
	Set_PropertyValue(29,"1.0");
	Set_PropertyValue(30,"0.7");
	Set_PropertyValue(31,"64.0");
	Set_PropertyValue(32,"2.1");
	Set_PropertyValue(33,"68.0");
	Set_PropertyValue(34,"0.7");
	Set_PropertyValue(35,"0.7");
	if (ShowEventLog) Set_PropertyValue(36,"YES"); else Set_PropertyValue(36,"NO");
	Set_PropertyValue(37,"No");
	Set_PropertyValue(39,"[closed, closed, closed]");  // normal
	Set_PropertyValue(40,"[closed, closed, closed]");  // state
	Set_PropertyValue(41,"90.0");
	Set_PropertyValue(42,"90.0");
	Set_PropertyValue(43,"0.0");
	Set_PropertyValue(44,"-1.0");
	Set_PropertyValue(45,"-1.0");
	Set_PropertyValue(46,"-1.0");
	Set_PropertyValue(47,"none");
	Set_PropertyValue(48,"1.0");
	Set_PropertyValue(49,"1.0");
	Set_PropertyValue(50,"True"); // DOC_P1Blocking
	Set_PropertyValue(51,"No");   // SinglePhTripping
	Set_PropertyValue(52,"No");   // SinglePhLockout
	Set_PropertyValue(53,"No");   // Lock
	Set_PropertyValue(54,"n");    // Reset
	Set_PropertyValue(55,"0");    // RatedCurrent
	Set_PropertyValue(56,"0");    // InterruptingRating

     // Deprecated Properties
	Set_PropertyValue(57,"0");    // Breakertime -> MechanicalDelay
	Set_PropertyValue(58,"0");    // Delay -> DefiniteTimeDelay
	Set_PropertyValue(59,"0");    // GroundCurve -> OC_GndCurve
	Set_PropertyValue(60,"0");    // GroundTrip -> OC_GndPickup
	Set_PropertyValue(61,"0");    // GroundInst -> OC_GndInst
	Set_PropertyValue(62,"0");    // TDGround -> OC_TDGnd
	Set_PropertyValue(63,"0");    // Phasecurve -> PhCurve
	Set_PropertyValue(64,"0");    // PhaseTrip -> PhPickup
	Set_PropertyValue(65,"0");    // PhaseInst -> PhInst
	Set_PropertyValue(66,"0");    // TDPhase -> TDPh
	Set_PropertyValue(67,"0");    // overtrip -> Generic_OverTrip
	Set_PropertyValue(68,"0");    // undertrip -> Generic_UnderTrip
	Set_PropertyValue(69,"0");    // Variable -> Generic_Variable
	Set_PropertyValue(70,"0");    // Overvoltcurve -> Voltage_OVCurve
	Set_PropertyValue(71,"0");    // Undervoltcurve -> Voltage_UVCurve
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TRelayObj::InterpretRelayType(const String s)
{
	switch(LowerCase(s)[0])
	{
		case 	L'c':
		ControlType = Current;
		break;
		case 	L'v':
		ControlType = VOLTAGE;
		break;
		case 	L'r':
		ControlType = REVPOWER;
		break;
		case 	L'4':
		switch(s.size() >= 2 ? LowerCase(s)[1] : '\0')
		{
			case 	L'6':
			ControlType = NEGCURRENT;
			break;
			case 	L'7':
			ControlType = NEGVOLTAGE;
			break;
			default:
			  ;
			break;
		}
		break;
		case 	L'g':
		ControlType = GENERIC;
		break;
		case 	L'd':
		switch(s.size() >= 2 ? LowerCase(s)[1] : '\0')
		{
			case 	'i':
			ControlType = Distance;
			break;
			case 	'o':
			ControlType = DOC;
			break;
			default:
			break;
		}
		break;
		case 	L't':
		ControlType = TD21;
		break;
		default:
		ControlType = Current;
		break;
	}

              /*Set Definite Time Defaults*/
	switch(LowerCase(s)[0])
	{
		case 	L'c':
		DefiniteTimeDelay = 0.0;
		break;
		case 	L'v':
		DefiniteTimeDelay = 0.0;
		break;
		case 	L'r':
		DefiniteTimeDelay = 0.1;
		break;
		case 	L'4':
		DefiniteTimeDelay = 0.1;
		break;
		case 	L'g':
		DefiniteTimeDelay = 0.1;
		break;
		case 	L'd':
		switch(s.size() >= 2 ? LowerCase(s)[1] : '\0')
		{
			case 	'i':
			DefiniteTimeDelay = 0.1;
			break;
			case 	'o':
			DefiniteTimeDelay = 0.0;
			break;
			default:
			break;
		}
		break;
		case 	L't':
		DefiniteTimeDelay = 0.1;
		break;
		default:
		DefiniteTimeDelay = 0.0;
		break;
	}
	Set_PropertyValue(24,Format("%-g", DefiniteTimeDelay));
}

/* Generic relays only work on PC Elements With control terminals
*/

void TRelayObj::GenericLogic(int ActorID)
{
	double VarValue = 0.0;
  // Per-phase trip and lockout don't apply. 3-Phase trip only.
	/*# with MonitoredElement do */
	{
		auto with0 = get_FMonitoredElement();
		VarValue = ((TPCElement*) get_FMonitoredElement())->Get_Variable(MonitorVarIndex);

      /*Check for Trip*/
		if((VarValue > OverTrip) || (VarValue < UnderTrip))
		{
			if(!ArmedForOpen[IdxMultiPh - 1])  // push the trip operation and arm to trip
				/*# with ActiveCircuit[ActorID] do */
				{
					RelayTarget[IdxMultiPh - 1] = ((TPCElement*) get_FMonitoredElement())->VariableName(MonitorVarIndex);
					LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + DefiniteTimeDelay + MechanicalDelay, CTRL_OPEN, 0, this, ActorID);
					OperationCount[IdxMultiPh - 1] = NumReclose + 1;  // force a lockout
					ArmedForOpen[IdxMultiPh - 1] = true;
				}
		}
		else   /*Within bounds*/  /*Less Than pickup value: reset if armed*/
		{
			if(ArmedForOpen[IdxMultiPh - 1])    // We became unarmed, so reset and disarm
				/*# with ActiveCircuit[ActorID] do */
				{
					LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
					ArmedForOpen[IdxMultiPh - 1] = false;
				}
		}
	}  /*With MonitoredElement*/
}

/*
  Negative Sequence Current Relay
  Patterned after Basler relay
*/

void TRelayObj::NegSeq46Logic(int ActorID)
{
	double		NegSeqCurrentMag = 0.0;
	double		TripTime = 0.0;
	int			IOffset = 0;
	complex		I012[3] = {cmplx(0,0), cmplx(0,0) ,cmplx(0,0) };
  // Per-phase trip and lockout don't apply. 3-Phase trip only.
	/*# with MonitoredElement do */
	{
		auto with0 = get_FMonitoredElement();
		get_FMonitoredElement()->Set_ActiveTerminal(MonitoredElementTerminal);
		get_FMonitoredElement()->GetCurrents(cBuffer, ActorID);
		IOffset = (MonitoredElementTerminal - 1) * get_FMonitoredElement()->Get_NConds();  // offset for active terminal
		Phase2SymComp((pComplexArray) &(cBuffer)[IOffset + 1 - 1], &I012[0]);
		NegSeqCurrentMag = cabs(I012[3 - 1]);
		if(NegSeqCurrentMag >= PickupAmps46)
		{
			if(!ArmedForOpen[IdxMultiPh - 1])  // push the trip operation and arm to trip
				/*# with ActiveCircuit[ActorID] do */
				{
					RelayTarget[IdxMultiPh - 1] = "-Seq Curr";
              /*simple estimate of trip time assuming current will be constant*/
					if(DefiniteTimeDelay > 0.0)
						TripTime = DefiniteTimeDelay;
					else
						TripTime = Isqt46 / Sqr(NegSeqCurrentMag / BaseAmps46); // Sec
					LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + MechanicalDelay, CTRL_OPEN, 0, this, ActorID);
					OperationCount[IdxMultiPh - 1] = NumReclose + 1;  // force a lockout
					ArmedForOpen[IdxMultiPh - 1] = true;
				}
		}
		else  /*Less Than pickup value: reset if armed*/
		{
			if(ArmedForOpen[IdxMultiPh - 1])    // We became unarmed, so reset and disarm
				/*# with ActiveCircuit[ActorID] do */
				{
					LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
					ArmedForOpen[IdxMultiPh - 1] = false;
				}
		}
	}  /*With MonitoredElement*/
}

void TRelayObj::OvercurrentLogic(int ActorID)
{
	int i = 0;
	double Cmag = 0.0;
	complex Csum = {};
	int MaxOperatingCount = 0;
	double GroundTime = 0.0;
	double PhaseTime = 0.0;
	double TripTime = 0.0;
	double TimeTest = 0.0;
	int stop = 0;

     // Check largest Current of all phases of monitored element
	get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);
	get_FMonitoredElement()->GetCurrents(cBuffer, ActorID);

	for(i = min(RELAYCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()); i >= 1; i--)
	{
		if((*FPresentState)[i - 1] == CTRL_CLOSE)
			break; // Continue sampling if at least one phase is closed.
		if(i == 1)
			return;  // Exit sampling if none of the phases is closed.
	}

     // Identify number of operations.
     // Pending to identify phase to trip for ground element when considering single-phase tripping as in modern microprocessed relays.
	if(SinglePhTrip)
	{
		for(stop = min(RELAYCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			if(LockedOut[i - 1])
				continue; // Skip locked out phases (includes phases that have been manually opened).
			if(i == 1)
				MaxOperatingCount = OperationCount[i - 1];
			else
				MaxOperatingCount = max(MaxOperatingCount, OperationCount[i - 1]);
		}
	}
	else
		MaxOperatingCount = OperationCount[IdxMultiPh - 1];

	GroundTime = -1.0;
     /*Check Ground Trip, if any*/
	if(((GndCurve != nullptr) || (DefiniteTimeDelay > 0.0)) && (GndPickup > 0.0))
	{
		Csum = CZero;
		for(stop = (Fnphases + CondOffset), i = (1 + CondOffset); i <= stop; i++)
			caccum(Csum, (cBuffer)[i - 1]);
		Cmag = cabs(Csum);
		if((GndInst > 0.0) && (Cmag >= GndInst) && (MaxOperatingCount == 1))
		{
			GroundTime = 0.01;      // Inst trip on first operation
			if(DebugTrace)
				AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("Gnd Instantaneous Trip: Mag=%.3g, Time=%.3g", Cmag, GroundTime), ActorID);
		}
		else
		{
			if(DefiniteTimeDelay > 0.0) // Definite Time Ground Relay
			{
				if(Cmag >= GndPickup)
				{
					GroundTime = DefiniteTimeDelay;
					if(DebugTrace)
						AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("Gnd Definite Time Trip: Mag=%.3g, Time=%.3g", Cmag, GroundTime), ActorID);
				}
			}
			else
			{
				GroundTime = TDGnd * GndCurve->GetTCCTime(Cmag / GndPickup);
				if((GroundTime > 0.0) && DebugTrace)
					AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("Gnd Curve Trip: Mag=%.3g, Time=%.3g", Cmag / GndPickup, GroundTime), ActorID);
			}
		}
	}

	if(GroundTime > 0.0)
		GroundTarget = true;
     // If GroundTime > 0 then we have a ground trip

	if(SinglePhTrip)
	{
		for(stop = min(RELAYCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			if((*FPresentState)[i - 1] != CTRL_CLOSE)
				continue;
			if(GroundTime > 0.0)
				TripTime = GroundTime;
			else
				TripTime = -1.0;  // initialize trip time for this phase.

			PhaseTime = -1.0;  /*No trip*/

            /*Check Phase Trip, if any*/ // Check current at i phase of monitored element
			if(((PhCurve != nullptr) || (DefiniteTimeDelay > 0.0)) && (PhPickup > 0.0))
			{
				Cmag = cabs((cBuffer)[i + CondOffset - 1]);
				if((PhInst > 0.0) && (Cmag >= PhInst) && (OperationCount[i - 1] == 1))
				{
					PhaseTime = 0.01;  // Inst trip on first operation
					if(DebugTrace)
						AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("Ph Instantaneous (1-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", i, Cmag, PhaseTime), ActorID);
				}
				else
				{
					if(DefiniteTimeDelay > 0.0) // Definite Time Phase Relay
					{
						if(Cmag >= PhPickup)
						{
							TimeTest = DefiniteTimeDelay;
							if(DebugTrace)
								AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("Ph Definite Time (1-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", i, Cmag, TimeTest), ActorID);
						}
						else
							TimeTest = -1.0;
					}
					else
					{
						TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
						if((TimeTest > 0.0) && DebugTrace)
							AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("Ph Curve (1-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", i, Cmag / PhPickup, TimeTest), ActorID);
					}
					if(TimeTest > 0.0)
						PhaseTime = TimeTest;
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
					/*# with ActiveCircuit[ActorID] do */   // Then arm for an open operation
					{
						RelayTarget[i - 1] = "";
						if(TripTime == GroundTime)
						{
							if(Abs(GroundTime - 0.01) < EPSILON)
								RelayTarget[i - 1] = "Gnd Instantaneous";
							else if(GroundTime == DefiniteTimeDelay)
								RelayTarget[i - 1] = "Gnd Definite Time";
							else
								RelayTarget[i - 1] = "Gnd Curve";
						}
						if(TripTime == PhaseTime)
						{
							if(RelayTarget[i - 1] != "")
								RelayTarget[i - 1] = RelayTarget[i - 1] + " + ";
							if(Abs(PhaseTime - 0.01) < EPSILON)
								RelayTarget[i - 1] = RelayTarget[i - 1] + "Ph Instantaneous";
							else if(PhaseTime == DefiniteTimeDelay)
								RelayTarget[i - 1] = RelayTarget[i - 1] + "Ph Definite Time";
							else
								RelayTarget[i - 1] = RelayTarget[i - 1] + "Ph Curve";
						}
						LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + MechanicalDelay, CTRL_OPEN, i, this, ActorID);
						if(OperationCount[i - 1] <= NumReclose)
							ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + MechanicalDelay + (RecloseIntervals)[OperationCount[i - 1] - 1], CTRL_CLOSE, i, this, ActorID);
						ArmedForOpen[i - 1] = true;
						ArmedForClose[i - 1] = true;
					}
			}
			else
			{
				if(ArmedForOpen[i - 1])
					/*# with ActiveCircuit[ActorID] do */    // If current dropped below pickup, disarm trip and set for reset
					{
						LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, i, this, ActorID);
						ArmedForOpen[i - 1] = false;
						ArmedForClose[i - 1] = false;
						GroundTarget = false;
						PhaseTarget[i - 1] = false;
					}
			}
		}
	}
	else // 3-Phase Trip
	{
		if(GroundTime > 0.0)
			TripTime = GroundTime;
		else
			TripTime = -1.0;  // initialize trip time
		PhaseTime = -1.0;

        /*Check Phase Trip, if any*/
		if(((PhCurve != nullptr) || (DefiniteTimeDelay > 0.0)) && (PhPickup > 0.0))
		{
			for(stop = (Fnphases + CondOffset), i = (1 + CondOffset); i <= stop; i++)
			{
				Cmag = cabs((cBuffer)[i - 1]);
				if((PhInst > 0.0) && (Cmag >= PhInst) && (OperationCount[IdxMultiPh - 1] == 1))
				{
					PhaseTime = 0.01;  // Inst trip on first operation
					if(DebugTrace)
						AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("Ph Instantaneous (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", i - CondOffset, Cmag, PhaseTime), ActorID);
					break;  /*FOR - if Inst, no sense checking other phases*/
				}
				else
				{
					if(DefiniteTimeDelay > 0.0) // Definite Time Phase Relay
					{
						if(Cmag >= PhPickup)
						{
							PhaseTime = DefiniteTimeDelay;
							if(DebugTrace)
								AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("Ph Definite Time (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", i - CondOffset, Cmag, PhaseTime), ActorID);
							break;  /*FOR - if Definite Time, no sense checking other phases*/
						}
					}
					else
					{
						TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
						if((TimeTest > 0.0) && DebugTrace)
							AppendToEventLog(String("Debug Sample: Relay.") + this->get_Name(), Format("Ph Curve (3-Phase) Trip: Phase=%d, Mag=%.3g, Time=%.3g", i - CondOffset, Cmag / PhPickup, TimeTest), ActorID);
						if(TimeTest > 0.0)
						{
							if(PhaseTime < 0.0)
								PhaseTime = TimeTest;
							else
								PhaseTime = min(PhaseTime, TimeTest);
						}
					}
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
				/*# with ActiveCircuit[ActorID] do */   // Then arm for an open operation
				{
					RelayTarget[IdxMultiPh - 1] = "";
					if(TripTime == GroundTime)
					{
						if(Abs(GroundTime - 0.01) < EPSILON)
							RelayTarget[IdxMultiPh - 1] = "Gnd Instantaneous";
						else if(GroundTime == DefiniteTimeDelay)
							RelayTarget[IdxMultiPh - 1] = "Gnd Definite Time";
						else
							RelayTarget[IdxMultiPh - 1] = "Gnd Curve";
					}
					if(TripTime == PhaseTime)
					{
						if(RelayTarget[IdxMultiPh - 1] != "")
							RelayTarget[IdxMultiPh - 1] = RelayTarget[IdxMultiPh - 1] + " + ";
						if(Abs(PhaseTime - 0.01) < EPSILON)
							RelayTarget[IdxMultiPh - 1] = RelayTarget[IdxMultiPh - 1] + "Ph Instantaneous";
						else if(PhaseTime == DefiniteTimeDelay)
							RelayTarget[IdxMultiPh - 1] = RelayTarget[IdxMultiPh - 1] + "Ph Definite Time";
						else
							RelayTarget[IdxMultiPh - 1] = RelayTarget[IdxMultiPh - 1] + "Ph Curve";
					}
					LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + MechanicalDelay, CTRL_OPEN, 0, this, ActorID);
					if(MaxOperatingCount <= NumReclose)
						ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + MechanicalDelay + (RecloseIntervals)[MaxOperatingCount - 1], CTRL_CLOSE, 0, this, ActorID);
					ArmedForOpen[IdxMultiPh - 1] = true;
					ArmedForClose[IdxMultiPh - 1] = true;
				}
		}
		else
		{
			if(ArmedForOpen[IdxMultiPh - 1])
				/*# with ActiveCircuit[ActorID] do */    // If current dropped below pickup, disarm trip and set for reset
				{
					LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
					ArmedForOpen[IdxMultiPh - 1] = false;
					ArmedForClose[IdxMultiPh - 1] = false;
					GroundTarget = false;
					PhaseTarget[IdxMultiPh - 1] = false;
				}
		}
	}
}

void TRelayObj::DistanceLogic(int ActorID)
{
	int i = 0;
	int j = 0;
	complex Vloop = {};
	complex Iloop = {};
	complex Zloop = {};
	complex Ires = {};
	complex kIres = {};
	complex Zreach = {};
	double I2 = 0.0;
	double min_distance = 0.0;
	double fault_distance = 0.0;
	double t_event = 0.0;
	TStringList Targets;
	bool PickedUp = false;
  // Per-phase trip and lockout don't apply. 3-Phase trip only.
	if(!LockedOut[IdxMultiPh - 1])
		/*# with MonitoredElement do */
		{
			auto with0 = get_FMonitoredElement();
			int stop = 0;
			PickedUp = false;
			min_distance = 1.0e30;
			get_FMonitoredElement()->GetCurrents(cBuffer, ActorID);
			if(Dist_Reverse)
			{
				int stop = 0;
				for(stop = get_FMonitoredElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					(cBuffer)[i + CondOffset - 1] = cnegate((cBuffer)[i + CondOffset - 1]);
				}
			}
			Ires = CZero;
			for(stop = get_FMonitoredElement()->Get_NPhases(), i = 1; i <= stop; i++)
			{
				caccum(Ires, (cBuffer)[i + CondOffset - 1]);
			}
			kIres = cmul(Dist_K0, Ires);
			get_FMonitoredElement()->GetTermVoltages(MonitoredElementTerminal, cvBuffer, ActorID);
			for(stop = get_FMonitoredElement()->Get_NPhases(), i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(stop1 = get_FMonitoredElement()->Get_NPhases(), j = i; j <= stop1; j++)
				{
					if(i == j)
					{
						Vloop = (cvBuffer)[i - 1];
						Iloop = cadd((cBuffer)[i + CondOffset - 1], kIres);
						Zreach = cmulreal(Dist_Z1, Mground); // not Dist_Z0 because it's included in Dist_K0
					}
					else
					{
						Vloop = csub((cvBuffer)[i - 1], (cvBuffer)[j - 1]);
						Iloop = csub((cBuffer)[i + CondOffset - 1], (cBuffer)[j + CondOffset - 1]);
						Zreach = cmulreal(Dist_Z1, Mphase);
					}
					I2 = Iloop.re * Iloop.re + Iloop.im * Iloop.im;
					if(I2 > 0.1)
					{
						Zloop = cdiv(Vloop, Iloop);
          // start with a very simple rectangular characteristic
						if (DebugTrace && (ActiveCircuit[ActiveActor]->Solution->DynaVars.T > 0.043))
							AppendToEventLog(String("Relay.") + this->get_Name(), Format("Zloop[%d,%d]=%.4f+j%.4f",
							i, j, Zloop.re, Zloop.im), ActorID);
						if((Zloop.re >= 0) && (Zloop.im >= MIN_DISTANCE_REACTANCE) && (Zloop.re <= Zreach.re) && (Zloop.im <= Zreach.im))
						{
							if(!PickedUp)
							{
								Targets.clear();
								//Targets->Sorted = true;
							}
							if(i == j)
							{
								Targets.push_back(Format("G%d", i));
							}
							else
							{
								Targets.push_back(Format("P%d%d", i, j));
							}
							fault_distance = cabs2(Zloop) / cabs2(Zreach);
							if(fault_distance < min_distance)
								min_distance = fault_distance;
							PickedUp = true;
						}
					}
				}
			}
			if(PickedUp)
			{
				if(DebugTrace)
				{
					AppendToEventLog(String("Relay.") + this->get_Name(), "Picked up", ActorID);
				}
				if(ArmedForReset[IdxMultiPh - 1])
				{
					ActiveCircuit[ActorID]->ControlQueue.Delete(LastEventHandle, ActorID);
					ArmedForReset[IdxMultiPh - 1] = false;
				}
				if(!ArmedForOpen[IdxMultiPh - 1])
					/*# with ActiveCircuit[ActorID] do */
					{

						int stop = 0;
						RelayTarget[IdxMultiPh - 1] = Format("21 %.3f pu dist", min_distance);
						t_event = ActiveCircuit[ActorID]->Solution->DynaVars.T + DefiniteTimeDelay + MechanicalDelay;
						sort(Targets.begin(), Targets.end());  // Delphi keeps Targets sorted (TStringList.Sorted := True)
						for(stop = Pred(Targets.size()), i = 0; i <= stop; i++)
						{
							RelayTarget[IdxMultiPh - 1] = RelayTarget[IdxMultiPh - 1] + " " + (Targets)[i];
						}
						LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, t_event, CTRL_OPEN, 0, this, ActorID);
						ArmedForOpen[IdxMultiPh - 1] = true;
						if(OperationCount[IdxMultiPh - 1] <= NumReclose)
						{
							LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, t_event + (RecloseIntervals)[OperationCount[IdxMultiPh - 1] - 1], CTRL_CLOSE, 0, this, ActorID);
							ArmedForClose[IdxMultiPh - 1] = true;
						}
					}
				Targets.clear();
			}
			else  // not picked up; reset if necessary
			{
				if((OperationCount[IdxMultiPh - 1] > 1) && (ArmedForReset[IdxMultiPh - 1] == false)) // this implements the reset, whether picked up or not
				{
					ArmedForReset[IdxMultiPh - 1] = true;
					/*# with ActiveCircuit[ActorID] do */
					{
						LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
					}
				}
				if(ArmedForOpen[IdxMultiPh - 1]) // this implements the drop-out, if picked up
				{
					ArmedForOpen[IdxMultiPh - 1] = false;
					ArmedForClose[IdxMultiPh - 1] = false;
				}
			}
		}  /*With MonitoredElement*/
}

void TRelayObj::TD21Logic(int ActorID)
{
	int i = 0;
	int j = 0;
	complex Vloop = {};
	complex Iloop = {};
	complex Zhsd = {};
	complex Zdir = {};
	complex Uhsd = {};
	complex Uref = {};
	complex Ires = {};
	complex kIres = {};
	double I2 = 0.0;
	double i2fault = 0.0;
	double min_distance = 0.0;
	double fault_distance = 0.0;
	double Uref2 = 0.0;
	double Uhsd2 = 0.0;
	double t_event = 0.0;
	double DT = 0.0;
	TStringList Targets;
	bool PickedUp = false;
	bool FaultDetected = false;
	int iB = 0;
	int iv = 0;
	int II = 0;
  // Per-phase trip and lockout don't apply. 3-Phase trip only.
	DT = ActiveCircuit[ActorID]->Solution->DynaVars.h;
	if(DT > 0.0)
	{
		if(DT > 1.0 / ActiveCircuit[ActorID]->Solution->get_FFrequency())
			DoErrorMsg(String("Relay: \"") + get_Name() + "\"", "Has type TD21 with time step greater than one cycle.", "Reduce time step, or change type to Distance.", 388);
		i =  Round(1.0 / 60.0 / DT + 0.5);
		if(i > td21_pt)
		{
			td21_i = 0; // ring buffer index to be incremented before actual use
			td21_pt = i;
			td21_quiet = td21_pt + 1;
			td21_stride = 2 * Get_NPhases();
			td21_h = (pComplexArray)realloc(td21_h, sizeof(complex) * td21_stride * td21_pt);
			td21_dV = (pComplexArray)realloc(td21_dV, sizeof(complex) * Get_NPhases());
			td21_Uref = (pComplexArray)realloc(td21_Uref, sizeof(complex) * Get_NPhases());
			td21_dI = (pComplexArray)realloc(td21_dI, sizeof(complex) * Get_NPhases());
			if(DebugTrace)
				AppendToEventLog(String("Relay.") + this->get_Name(), Format("TD21 prep %d phases, %.3g dt, %d points, %d elements",
				Get_NPhases(), DT, td21_pt, td21_stride * td21_pt), ActorID);
		}
	}
	if(!LockedOut[IdxMultiPh - 1])
		/*# with MonitoredElement do */
		{
			auto with0 = get_FMonitoredElement();
			int stop = 0;
			FaultDetected = false;
			get_FMonitoredElement()->GetCurrents(cBuffer, ActorID);
			if(Dist_Reverse)
			{
				int stop = 0;
				for(stop = get_FMonitoredElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					(cBuffer)[i + CondOffset - 1] = cnegate((cBuffer)[i + CondOffset - 1]);
				}
			}
			i2fault = PhPickup * PhPickup;
			for(stop = with0->Get_NPhases(), i = 1; i <= stop; i++)
			{
				I2 = cabs2((cBuffer)[i + CondOffset - 1]);
				if(I2 > i2fault)
					FaultDetected = true;
			}
			if(DebugTrace)
				AppendToEventLog(String("Relay.") + this->get_Name(), "FaultDetected=" +BoolToStr(FaultDetected), ActorID);
			get_FMonitoredElement()->GetTermVoltages(MonitoredElementTerminal, cvBuffer, ActorID);
			if(td21_i < 1)
			{
				int stop = 0;
				if(DebugTrace)
					AppendToEventLog(String("Relay.") + this->get_Name(), "Initialize cqueue", ActorID);
				for(stop = td21_pt, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					iB = (i - 1) * td21_stride;
					for(stop1 = with0->Get_NPhases(), j = 1; j <= stop1; j++)
					{
						iv = iB + j;
						(td21_h)[iv - 1] = (cvBuffer)[j - 1];
						II = iB + with0->Get_NPhases() + j;
						(td21_h)[II - 1] = (cBuffer)[j + CondOffset - 1];
					}
				}
				td21_i = 1;
			}
			td21_next = (td21_i % td21_pt) + 1;  // this points to the oldest sample, and the next write location
    // calculate the differential currents and voltages
			iB = (td21_next - 1) * td21_stride;
			for(stop = with0->Get_NPhases(), j = 1; j <= stop; j++)
			{
				iv = iB + j;
				(td21_Uref)[j - 1] = (td21_h)[iv - 1];
				(td21_dV)[j - 1] = csub((cvBuffer)[j - 1], (td21_h)[iv - 1]);
				II = iB + with0->Get_NPhases() + j;
				(td21_dI)[j - 1] = csub((cBuffer)[j + CondOffset - 1], (td21_h)[II - 1]);
			}
    // do the relay processing
			if(ActiveCircuit[ActorID]->Solution->DynaVars.IterationFlag < 1)
			{
				int stop = 0;
				iB = (td21_i - 1) * td21_stride;
				for(stop = with0->Get_NPhases(), j = 1; j <= stop; j++)
				{
					iv = iB + j;
					(td21_h)[iv - 1] = (cvBuffer)[j - 1];
					II = iB + with0->Get_NPhases() + j;
					(td21_h)[II - 1] = (cBuffer)[j + CondOffset - 1];
				}
				td21_i = td21_next;
				if(td21_quiet > 0)
					--td21_quiet;
			}
			if(td21_quiet <= 0)  // one cycle since we started, or since the last operation
			{
				int stop = 0;
				PickedUp = false;
				min_distance = 1.0e30;
				Ires = CZero;
				for(stop = get_FMonitoredElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					caccum(Ires, (td21_dI)[i - 1]);
				}
				kIres = cmul(Dist_K0, Ires);
				for(stop = get_FMonitoredElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					for(stop1 = get_FMonitoredElement()->Get_NPhases(), j = i; j <= stop1; j++)
					{
						if(i == j)
						{
							Uref = (td21_Uref)[i - 1];
							Vloop = (td21_dV)[i - 1];
							Iloop = cadd((td21_dI)[i - 1], kIres);
							Zhsd = cmulreal(Dist_Z1, Mground); // not Dist_Z0 because it's included in Dist_K0
						}
						else
						{
							Uref = csub((td21_Uref)[i - 1], (td21_Uref)[j - 1]);
							Vloop = csub((td21_dV)[i - 1], (td21_dV)[j - 1]);
							Iloop = csub((td21_dI)[i - 1], (td21_dI)[j - 1]);
							Zhsd = cmulreal(Dist_Z1, Mphase);
						}
						I2 = cabs2(Iloop);
						Uref2 = cabs2(Uref);
						if(FaultDetected && (I2 > 0.1) && (Uref2 > 0.1))
						{
							Zdir = cnegate(cdiv(Vloop, Iloop));
							if(DebugTrace)
								AppendToEventLog(String("Relay.") + this->get_Name(), Format("Zhsd[%d,%d]=%.4f+j%.4f, Zdir=%.4f+j%.4f",
								i, j, Zhsd.re, Zhsd.im, Zdir.re, Zdir.im), ActorID);
							if((Zdir.re > 0.0) && (Zdir.im > 0.0))
							{
								Uhsd = csub(cmul(Zhsd, Iloop), Vloop);
								Uhsd2 = cabs2(Uhsd);
								if(DebugTrace)
									AppendToEventLog(String("Relay.") + this->get_Name(), Format("     Uhsd=%.2f, Uref=%.2f",
									cabs(Uhsd), cabs(Uref)), ActorID);
								if(Uhsd2 / Uref2 > 1.0) // this loop trips
								{
									if(!PickedUp)
									{
										Targets.clear();
										//Targets->Sorted = true;
									}
									if(i == j)
									{
										Targets.push_back(Format("G%d", i));
									}
									else
									{
										Targets.push_back(Format("P%d%d", i, j));
									}
									fault_distance = 1.0L / sqrt(Uhsd2 / Uref2);
									if(fault_distance < min_distance)
										min_distance = fault_distance;
									PickedUp = true;
								}
							}
						}
					}
				}
				if(PickedUp)
				{
					if(DebugTrace)
					{
						AppendToEventLog(String("Relay.") + this->get_Name(), "Picked up", ActorID);
					}
					if(ArmedForReset[IdxMultiPh - 1])
					{
						ActiveCircuit[ActorID]->ControlQueue.Delete(LastEventHandle, ActorID);
						ArmedForReset[IdxMultiPh - 1] = false;
						if(DebugTrace)
							AppendToEventLog(String("Relay.") + this->get_Name(), "Dropping last event.", ActorID);
					}
					if(!ArmedForOpen[IdxMultiPh - 1])
						/*# with ActiveCircuit[ActorID] do */
						{

							int stop = 0;
							RelayTarget[IdxMultiPh - 1] = Format("TD21 %.3f pu dist", min_distance);
							t_event = ActiveCircuit[ActorID]->Solution->DynaVars.T + DefiniteTimeDelay + MechanicalDelay;
							sort(Targets.begin(), Targets.end());  // Delphi keeps Targets sorted (TStringList.Sorted := True)
							for(stop = Pred(Targets.size()), i = 0; i <= stop; i++)
							{
								RelayTarget[IdxMultiPh - 1] = RelayTarget[IdxMultiPh - 1] + " " + (Targets)[i];
							}
							LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, t_event, CTRL_OPEN, 0, this, ActorID);
							if(DebugTrace)
								AppendToEventLog(String("Relay.") + this->get_Name(), Format("Pushing trip event for %.3f", t_event), ActorID);
							ArmedForOpen[IdxMultiPh - 1] = true;
							if(OperationCount[IdxMultiPh - 1] <= NumReclose)
							{
								LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, t_event + (RecloseIntervals)[OperationCount[IdxMultiPh - 1] - 1], CTRL_CLOSE, 0, this, ActorID);
								if(DebugTrace)
									AppendToEventLog(String("Relay.") + this->get_Name(), Format("Pushing reclose event for %.3f",
									t_event + (RecloseIntervals)[OperationCount[IdxMultiPh - 1] - 1]), ActorID);
								ArmedForClose[IdxMultiPh - 1] = true;
							}
						}
					Targets.clear();
				}
				if(!FaultDetected)  // not picked up; reset if necessary
				{
					if((OperationCount[IdxMultiPh - 1] > 1) && (ArmedForReset[IdxMultiPh - 1] == false)) // this implements the reset, whether picked up or not
					{
						ArmedForReset[IdxMultiPh - 1] = true;
						/*# with ActiveCircuit[ActorID] do */
						{
							LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
						}
						if(DebugTrace)
							AppendToEventLog(String("Relay.") + this->get_Name(), Format("Pushing reset event for %.3f",
							ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime), ActorID);
					}
					if(ArmedForOpen[IdxMultiPh - 1])
					{
						td21_quiet = td21_pt + 1;
						ArmedForOpen[IdxMultiPh - 1] = false;
						ArmedForClose[IdxMultiPh - 1] = false;
						if(DebugTrace)
							AppendToEventLog(String("Relay.") + this->get_Name(), Format("Dropping out at %.3f", ActiveCircuit[ActorID]->Solution->DynaVars.T), ActorID);
					}
				}
			} /* td21_quiet*/
		}  /*With MonitoredElement*/
}

// Get power to control based on active power
void TRelayObj::GetControlPower(Ucomplex::complex &ControlPower, int ActorID)
{
	int i, k;
	complex Vph[3];
	complex V012[3];
	complex Iph[3];
	complex I012[3];

	auto MonitoredElement = get_FMonitoredElement();
	if (MonitoredElement->Fnphases >= 3)
	{

		MonitoredElement->GetCurrents(cBuffer, ActorID);
		MonitoredElement->GetTermVoltages(MonitoredElementTerminal, cvBuffer, ActorID);

		for (i = 0; i < 3; i++)
		{
			k = (MonitoredElementTerminal - 1) * MonitoredElement->Fnconds + i;
			Iph[i] = cBuffer[k];
			Vph[i] = cvBuffer[i];
		}

		Phase2SymComp(Iph, I012);
		Phase2SymComp(Vph, V012);

		ControlPower = cmulreal(cmul(V012[1], conjg(I012[1])), 0.003);  // Convert to kilo

	}
	else
	{
	  // just take the total power (works also for 1ph elements with 2 conductors)
		ControlPower = MonitoredElement->Get_Power(MonitoredElementTerminal, ActorID);
	}

}

void TRelayObj::DirectionalOvercurrentLogic(int ActorID)
{
	int i;
	double TripTime, TimeTest;
	double Cmag, Cangle;
	complex ControlPower;

	// with   MonitoredElement
	for(i = min(RELAYCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()); i >= 1; i--)
	{
		if((*FPresentState)[i - 1] == CTRL_CLOSE)
			break; // Continue sampling if at least one phase is closed.
		if(i == 1)
			return;  // Exit sampling if none of the phases is closed.
	}

	// Identify net balanced power flow.
	if (DOC_P1Blocking)
	{
		GetControlPower(ControlPower, ActorID);

		if (ControlPower.re >= 0.0)  // Forward Power
		{

			if (ArmedForOpen[IdxMultiPh - 1])
			// If net balanced active power is forward, disarm trip and set for reset
			{
				LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
				ArmedForOpen[IdxMultiPh - 1] = false;
				ArmedForClose[IdxMultiPh - 1] = false;

				if (DebugTrace)
					AppendToEventLog(String("Relay.") + this->get_Name(), Format("DOC - Reset on Forward Net Balanced Active Power: %.2f kW", ControlPower.re), ActorID);

			}
			else
			{

				if (DebugTrace)
					AppendToEventLog(String("Relay.") + this->get_Name(), Format("DOC - Forward Net Balanced Active Power: %.2f kW. DOC Element blocked.", ControlPower.re), ActorID);

			}

			return;  // Do not evaluate trip if power is forward.

		}
	}


	TripTime = -1.0;

	auto MonitoredElement = get_FMonitoredElement();
	MonitoredElement->GetCurrents(cBuffer, ActorID);
	MonitoredElement->GetTermVoltages(MonitoredElementTerminal, cvBuffer, ActorID);

	// Shift angle to cBuffer to be relative to cvBuffer
	for (i = CondOffset; i < Fnphases + CondOffset; i++)
		cBuffer[i] = pdegtocomplex(cabs(cBuffer[i]), cdang(cBuffer[i]) - cdang(cvBuffer[i - CondOffset]));

	for (i = CondOffset; i < Fnphases + CondOffset; i++)
	{

		TimeTest = -1.0;
		Cmag = cabs(cBuffer[i]);
		Cangle = cdang(cBuffer[i]);

		if ((DOC_TiltAngleLow == 90.0) || (DOC_TiltAngleLow == 270.0))
		{

			if (cBuffer[i].re <= -1 * DOC_TripSetLow)
			{

				if (DOC_TripSetMag > 0.0)
				{ // Circle Specified.

					if (Cmag <= DOC_TripSetMag)
					{ // Within the Circle

						if (DOC_TripSetHigh > 0.0) // High Straight-Line Specified.
						{

							if ((DOC_TiltAngleHigh == 90.0) || (DOC_TiltAngleHigh == 270.0))
							{

								if (cBuffer[i].re < -1 * DOC_TripSetHigh)
								{ // Left-side of High Straight-Line

									if (DefiniteTimeDelay > 0.0)
										TimeTest = DefiniteTimeDelay;
									else
									if (PhCurve != nullptr)
										TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
									else
									if (DefiniteTimeDelay == 0.0)
										TimeTest = DefiniteTimeDelay;

								}
								else
								{  // Right-Side of High Straight-Line

									if (DOC_DelayInner > 0.0)
										TimeTest = DOC_DelayInner;
									else
									if (DOC_PhaseCurveInner != nullptr)
										TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
									else
									if (DOC_DelayInner == 0.0)
										TimeTest = DefiniteTimeDelay;

								}

							}
							else
							{

								if (cBuffer[i].im < tan((DEG_TO_RAD * DOC_TiltAngleHigh)) * (cBuffer[i].re + DOC_TripSetHigh))
								{ // Left-side of High Straight-Line

									if (DefiniteTimeDelay > 0.0)
										TimeTest = DefiniteTimeDelay;
									else
									if (PhCurve != nullptr)
										TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
									else
									if (DefiniteTimeDelay == 0.0)
										TimeTest = DefiniteTimeDelay;

								}
								else
								{ // Right-Side of High Straight-Line

									if (DOC_DelayInner > 0.0)
										TimeTest = DOC_DelayInner;
									else
									if (DOC_PhaseCurveInner != nullptr)
										TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
									else
									if (DOC_DelayInner == 0.0)
										TimeTest = DefiniteTimeDelay;

								}

							}

						}
						else
						{ // High Straight-Line Not Specified.

							if (DOC_DelayInner > 0.0)
								TimeTest = DOC_DelayInner;
							else
							if (DOC_PhaseCurveInner != nullptr)
								TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
							else
							if (DOC_DelayInner == 0.0)
								TimeTest = DefiniteTimeDelay;

						}

					}
					else
					{ // Out of the Circle

						if (DefiniteTimeDelay > 0.0)
							TimeTest = DefiniteTimeDelay;
						else
						if (PhCurve != nullptr)
							TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
						else
						if (DefiniteTimeDelay == 0.0)
							TimeTest = DefiniteTimeDelay;

					}

				}
				else
				{ // Circle not Specified

					if (DOC_TripSetHigh > 0.0)
					{ // High Straight-Line Specified.

						if ((DOC_TiltAngleHigh == 90.0) || (DOC_TiltAngleHigh == 270.0))
						{

							if (cBuffer[i].re < -1 * DOC_TripSetHigh)
							{ // Left-side of High Straight-Line

								if (DefiniteTimeDelay > 0.0)
									TimeTest = DefiniteTimeDelay;
								else
								if (PhCurve != nullptr)
									TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
								else
								if (DefiniteTimeDelay == 0.0)
									TimeTest = DefiniteTimeDelay;

							}
							else
							{  // Right-Side of High Straight-Line

								if (DOC_DelayInner > 0.0)
									TimeTest = DOC_DelayInner;
								else
								if (DOC_PhaseCurveInner != nullptr)
									TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
								else
								if (DOC_DelayInner == 0.0)
									TimeTest = DefiniteTimeDelay;

							}

						}
						else
						{

							if (cBuffer[i].im < tan((DEG_TO_RAD * DOC_TiltAngleHigh)) * (cBuffer[i].re + DOC_TripSetHigh))
							{ // Left-side of High Straight-Line

								if (DefiniteTimeDelay > 0.0)
									TimeTest = DefiniteTimeDelay;
								else
								if (PhCurve != nullptr)
									TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
								else
								if (DefiniteTimeDelay == 0.0)
									TimeTest = DefiniteTimeDelay;

							}
							else
							{ // Right-Side of High Straight-Line

								if (DOC_DelayInner > 0.0)
									TimeTest = DOC_DelayInner;
								else
								if (DOC_PhaseCurveInner != nullptr)
									TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
								else
								if (DOC_DelayInner == 0.0)
									TimeTest = DefiniteTimeDelay;

							}

						}

					}
					else
					{  // High Straight-Line Not Specified.

						if (DefiniteTimeDelay > 0.0)
							TimeTest = DefiniteTimeDelay;
						else
						if (PhCurve != nullptr)
							TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
						else
						if (DefiniteTimeDelay == 0.0)
							TimeTest = DefiniteTimeDelay;

					}

				}

			}

		}
		else
		{ /*90, 270*/

			if (cBuffer[i].im < tan((DEG_TO_RAD * DOC_TiltAngleLow)) * (cBuffer[i].re + DOC_TripSetLow))
			{

				if (DOC_TripSetMag > 0.0)
				{ // Circle Specified.

					if (Cmag <= DOC_TripSetMag)
					{ // Within the Circle

						if (DOC_TripSetHigh > 0.0) // High Straight-Line Specified.
						{

							if ((DOC_TiltAngleHigh == 90.0) || (DOC_TiltAngleHigh == 270.0))
							{

								if (cBuffer[i].re < -1 * DOC_TripSetHigh)
								{ // Left-side of High Straight-Line

									if (DefiniteTimeDelay > 0.0)
										TimeTest = DefiniteTimeDelay;
									else
									if (PhCurve != nullptr)
										TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
									else
									if (DefiniteTimeDelay == 0.0)
										TimeTest = DefiniteTimeDelay;

								}
								else
								{  // Right-Side of High Straight-Line

									if (DOC_DelayInner > 0.0)
										TimeTest = DOC_DelayInner;
									else
									if (DOC_PhaseCurveInner != nullptr)
										TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
									else
									if (DOC_DelayInner == 0.0)
										TimeTest = DefiniteTimeDelay;

								}

							}
							else
							{

								if (cBuffer[i].im < tan((DEG_TO_RAD * DOC_TiltAngleHigh)) * (cBuffer[i].re + DOC_TripSetHigh))
								{ // Left-side of High Straight-Line

									if (DefiniteTimeDelay > 0.0)
										TimeTest = DefiniteTimeDelay;
									else
									if (PhCurve != nullptr)
										TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
									else
									if (DefiniteTimeDelay == 0.0)
										TimeTest = DefiniteTimeDelay;

								}
								else
								{ // Right-Side of High Straight-Line

									if (DOC_DelayInner > 0.0)
										TimeTest = DOC_DelayInner;
									else
									if (DOC_PhaseCurveInner != nullptr)
										TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
									else
									if (DOC_DelayInner == 0.0)
										TimeTest = DefiniteTimeDelay;

								}

							}

						}
						else
						{ // High Straight-Line Not Specified.

							if (DOC_DelayInner > 0.0)
								TimeTest = DOC_DelayInner;
							else
							if (DOC_PhaseCurveInner != nullptr)
								TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
							else
							if (DOC_DelayInner == 0.0)
								TimeTest = DefiniteTimeDelay;

						}

					}
					else
					{ // Out of the Circle

						if (DefiniteTimeDelay > 0.0)
							TimeTest = DefiniteTimeDelay;
						else
						if (PhCurve != nullptr)
							TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
						else
						if (DefiniteTimeDelay == 0.0)
							TimeTest = DefiniteTimeDelay;

					}

				}
				else
				{ // Circle not Specified

					if (DOC_TripSetHigh > 0.0)
					{ // High Straight-Line Specified.

						if ((DOC_TiltAngleHigh == 90.0) || (DOC_TiltAngleHigh == 270.0))
						{

							if (cBuffer[i].re < -1 * DOC_TripSetHigh)
							{ // Left-side of High Straight-Line

								if (DefiniteTimeDelay > 0.0)
									TimeTest = DefiniteTimeDelay;
								else
								if (PhCurve != nullptr)
									TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
								else
								if (DefiniteTimeDelay == 0.0)
									TimeTest = DefiniteTimeDelay;

							}
							else
							{  // Right-Side of High Straight-Line

								if (DOC_DelayInner > 0.0)
									TimeTest = DOC_DelayInner;
								else
								if (DOC_PhaseCurveInner != nullptr)
									TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
								else
								if (DOC_DelayInner == 0.0)
									TimeTest = DefiniteTimeDelay;

							}

						}
						else
						{

							if (cBuffer[i].im < tan((DEG_TO_RAD * DOC_TiltAngleHigh)) * (cBuffer[i].re + DOC_TripSetHigh))
							{ // Left-side of High Straight-Line

								if (DefiniteTimeDelay > 0.0)
									TimeTest = DefiniteTimeDelay;
								else
								if (PhCurve != nullptr)
									TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
								else
								if (DefiniteTimeDelay == 0.0)
									TimeTest = DefiniteTimeDelay;

							}
							else
							{ // Right-Side of High Straight-Line

								if (DOC_DelayInner > 0.0)
									TimeTest = DOC_DelayInner;
								else
								if (DOC_PhaseCurveInner != nullptr)
									TimeTest = DOC_TDPhaseInner * DOC_PhaseCurveInner->GetTCCTime(Cmag / DOC_PhaseTripInner);
								else
								if (DOC_DelayInner == 0.0)
									TimeTest = DefiniteTimeDelay;

							}

						}

					}
					else
					{  // High Straight-Line Not Specified.

						if (DefiniteTimeDelay > 0.0)
							TimeTest = DefiniteTimeDelay;
						else
						if (PhCurve != nullptr)
							TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
						else
						if (DefiniteTimeDelay == 0.0)
							TimeTest = DefiniteTimeDelay;

					}

				}

			}
			else
			{
				// There might be an intersection between Straight Line Low and High depending on their angles.
				// Straight Line High takes precedence.
				if (DOC_TripSetHigh > 0.0)
				{

					if ((DOC_TiltAngleHigh == 90.0) || (DOC_TiltAngleHigh == 270.0))
					{
						if (cBuffer[i].re < -1 * DOC_TripSetHigh)
						{ // Left-side of High Straight-Line

							if (DefiniteTimeDelay > 0.0)
								TimeTest = DefiniteTimeDelay;
							else
							if (PhCurve != nullptr)
								TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
							else
							if (DefiniteTimeDelay == 0.0)
								TimeTest = DefiniteTimeDelay;

						}
					}
					else
					{

						if (cBuffer[i].im < tan((DEG_TO_RAD * DOC_TiltAngleHigh)) * (cBuffer[i].re + DOC_TripSetHigh))
						{ // Left-side of High Straight-Line

							if (DefiniteTimeDelay > 0.0)
								TimeTest = DefiniteTimeDelay;
							else
							if (PhCurve != nullptr)
								TimeTest = TDPh * PhCurve->GetTCCTime(Cmag / PhPickup);
							else
							if (DefiniteTimeDelay == 0.0)
								TimeTest = DefiniteTimeDelay;

						}

					}

				}


			}


		}

		if (TimeTest >= 0.0)
		{

			if (DebugTrace)
				AppendToEventLog(String("Relay.") + this->get_Name(), Format("Directional Overcurrent - Phase %d Trip: Mag=%.5g, Ang=%.5g, Time=%.5g", i - CondOffset, Cmag, Cangle, TimeTest), ActorID);

			if (TripTime < 0.0)
				TripTime = TimeTest;
			else
				TripTime = min(TripTime, TimeTest);

		}

	}


	if (TripTime >= 0.0)
	{
		if (!ArmedForOpen[IdxMultiPh - 1])
		// Then arm for an open operation
		{
			RelayTarget[IdxMultiPh - 1] = "DOC";
			LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + MechanicalDelay, CTRL_OPEN, 0, this, ActorID);
			if (OperationCount[IdxMultiPh - 1] <= NumReclose)
				LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + MechanicalDelay + RecloseIntervals[OperationCount[IdxMultiPh - 1] - 1], CTRL_CLOSE, 0, this, ActorID);
			ArmedForOpen[IdxMultiPh - 1] = true;
			ArmedForClose[IdxMultiPh - 1] = true;
		}
	}
	else
	{
		if (ArmedForOpen[IdxMultiPh - 1])
		// If current dropped below pickup, disarm trip and set for reset
		{
			LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
			ArmedForOpen[IdxMultiPh - 1] = false;
			ArmedForClose[IdxMultiPh - 1] = false;
		}
	}
}

void TRelayObj::RevPowerLogic(int ActorID)
{
	complex s = {};
  // Per-phase trip and lockout don't apply. 3-Phase trip only.
	/*# with MonitoredElement do */
	{
		auto with0 = get_FMonitoredElement();
      //----MonitoredElement.ActiveTerminalIdx := MonitoredElementTerminal;
		s = get_FMonitoredElement()->Get_Power(MonitoredElementTerminal, ActorID);
		if(s.re < 0.0)
		{
			if(Abs( s.re) > PhInst * 1000.0)
			{
				if(!ArmedForOpen[IdxMultiPh - 1])
					/*# with ActiveCircuit[ActorID] do */
					{
						  // push the trip operation and arm to trip
						RelayTarget[IdxMultiPh - 1] = "Rev P";
						LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + DefiniteTimeDelay + MechanicalDelay, CTRL_OPEN, 0, this, ActorID);
						OperationCount[IdxMultiPh - 1] = NumReclose + 1;  // force a lockout
						ArmedForOpen[IdxMultiPh - 1] = true;
					}
			}
			else
			{
				if(ArmedForOpen[IdxMultiPh - 1])
					/*# with ActiveCircuit[ActorID] do */
					{
						    // We became unarmed, so reset and disarm
						LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
						ArmedForOpen[IdxMultiPh - 1] = false;
					}
			}
		}
	}  /*With MonitoredElement*/
}

void TRelayObj::VoltageLogic(int ActorID)
{
	int i = 0;
	double Vmax = 0.0;
	double Vmax_closed = 0.0;
	double Vmin = 0.0;
	double Vmin_closed = 0.0;
	double Vmag = 0.0;
	double OVTime = 0.0;
	double UVTime = 0.0;
	double TripTime = 0.0;
	int stop = 0;
  // Per-phase trip and lockout don't apply. 3-Phase trip only.
	if(!LockedOut[IdxMultiPh - 1])
	{
     /***** Fix so that fastest trip time applies *****/
		get_FMonitoredElement()->GetTermVoltages(MonitoredElementTerminal, cBuffer, ActorID);
		Vmin = 1.0e50;
		Vmax = 0.0;
		Vmin_closed = 1.0e50;
		Vmax_closed = 0.0;
		Vmag = -1.0;
		for(stop = min(RELAYCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			Vmag = cabs((cBuffer)[i - 1]);
			if((*FPresentState)[i - 1] == CTRL_CLOSE)
			{
				if(Vmag > Vmax_closed)
					Vmax_closed = Vmag;
				if(Vmag < Vmin_closed)
					Vmin_closed = Vmag;
			}
			if(Vmag > Vmax)
				Vmax = Vmag;
			if(Vmag < Vmin)
				Vmin = Vmag;
		}

     /*Convert to Per Unit*/
		Vmax = Vmax / VBase;
		Vmin = Vmin / VBase;
		Vmax_closed = Vmax_closed / VBase;
		Vmin_closed = Vmin_closed / VBase;

		TripTime = -1.0;
		OVTime = -1.0;
		UVTime = -1.0;

           /*Check OverVoltage Trip, if any*/
		if((OVcurve != nullptr) && (Vmag > 0.0))
			OVTime = OVcurve->GetOVTime(Vmax_closed);

         // If OVTime > 0 then we have a OV trip
		if(OVTime > 0.0)
		{
			TripTime = OVTime;
			if(DebugTrace)
				AppendToEventLog(String("Relay.") + this->get_Name(), Format("OV (3-Phase) Trip: Mag=%.3g, Time=%.3g", Vmax_closed, OVTime), ActorID);
		}

           /*Check UV Trip, if any*/
		if((UVCurve != nullptr) && (Vmag > 0.0))
			UVTime = UVCurve->GetUVTime(Vmin_closed);
         // If UVTime > 0 then we have a UV trip

		if(UVTime > 0.0)
		{
			if(TripTime > 0.0)
			{
				TripTime = min(TripTime, UVTime);   // Min of UV or OV time
			}
			else
			{
				TripTime = UVTime;
			}
			if(DebugTrace)
				AppendToEventLog(String("Relay.") + this->get_Name(), Format("UV (3-Phase) Trip: Mag=%.3g, Time=%.3g", Vmin_closed, UVTime), ActorID);
		}

		if(TripTime > 0.0)
			/*# with ActiveCircuit[ActorID] do */
			{
				if(ArmedForOpen[IdxMultiPh - 1] && ((ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + MechanicalDelay) < NextTriptime))
				{
					ActiveCircuit[ActorID]->ControlQueue.Delete(LastEventHandle, ActorID);  // Delete last event from Queue
					ArmedForOpen[IdxMultiPh - 1] = false;  // force it to go through next IF
				}
				if(!ArmedForOpen[IdxMultiPh - 1])
				{  // Then arm for an open operation
					if(TripTime == UVTime)
					{
						if(TripTime == OVTime)
							RelayTarget[IdxMultiPh - 1] = "UV + OV";
						else
							RelayTarget[IdxMultiPh - 1] = "UV";
					}
					else
						RelayTarget[IdxMultiPh - 1] = "OV";
					NextTriptime = ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + MechanicalDelay;
					LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, NextTriptime, CTRL_OPEN, 0, this, ActorID);
					ArmedForOpen[IdxMultiPh - 1] = true;
				}
			}
		else if((TripTime < 0.0) && (ArmedForOpen[IdxMultiPh - 1]))  // if voltage dropped below pickup, disarm and set for reset
		{
			/*# with ActiveCircuit[ActorID] do */
			{
				ActiveCircuit[ActorID]->ControlQueue.Delete(LastEventHandle, ActorID);  // Delete last event from Queue
				NextTriptime = -1.0;
				LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
				ArmedForOpen[IdxMultiPh - 1] = false;
			}
		}

		// Check for reclosing - all phases must be opened.
		for(i = min(RELAYCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()); i >= 1; i--)
		{
			if((*FPresentState)[i - 1] == CTRL_CLOSE)
				return;
		}

		{     /*Present state is Open, Check for Voltage and then set reclose Interval*/
			if(OperationCount[IdxMultiPh - 1] <= NumReclose)
			{
				if(!ArmedForClose[IdxMultiPh - 1])
				{
					if(Vmax > 0.9)
						/*# with ActiveCircuit[ActorID] do */  // OK if voltage > 90%
						{
							LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + (RecloseIntervals)[OperationCount[IdxMultiPh - 1] - 1], CTRL_CLOSE, 0, this, ActorID);
							ArmedForClose[IdxMultiPh - 1] = true;
						}
				}
				else  /*Armed, but check to see if voltage dropped before it reclosed and cancel action*/
				{
					if(Vmax < 0.9)
						ArmedForClose[IdxMultiPh - 1] = false;
				}
			}
		}
	}
}

/*Neg Seq voltage Relay*/

void TRelayObj::NegSeq47Logic(int ActorID)
{
	double	NegSeqVoltageMag	= 0.0;
	complex V012[3]				= { cmplx(0,0), cmplx(0,0) , cmplx(0,0) };
  // Per-phase trip and lockout don't apply. 3-Phase trip only.
	/*# with MonitoredElement do */
	{
		auto with0 = get_FMonitoredElement();
		get_FMonitoredElement()->GetTermVoltages(MonitoredElementTerminal, cBuffer, ActorID);
		Phase2SymComp(cBuffer, &V012[0]); // Phase to symmetrical components
		NegSeqVoltageMag = cabs(V012[3 - 1]);
		if(NegSeqVoltageMag >= PickupVolts47)
		{
			if(!ArmedForOpen[IdxMultiPh - 1])
				/*# with ActiveCircuit[ActorID] do */
				{
					  // push the trip operation and arm to trip
					RelayTarget[IdxMultiPh - 1] = "-Seq V";
					LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + DefiniteTimeDelay + MechanicalDelay, CTRL_OPEN, 0, this, ActorID);
					OperationCount[IdxMultiPh - 1] = NumReclose + 1;  // force a lockout
					ArmedForOpen[IdxMultiPh - 1] = true;
				}
		}
		else  /*Less Than pickup value: reset if armed*/
		{
			if(ArmedForOpen[IdxMultiPh - 1])
				/*# with ActiveCircuit[ActorID] do */
				{
					    // We became unarmed, so reset and disarm
					LastEventHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
					ArmedForOpen[IdxMultiPh - 1] = false;
				}
		}
	}  /*With MonitoredElement*/
}




}  // namespace Relay
