
#pragma hdrstop

#include "SwtControl.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Utilities.h"
#include "Solution.h"
#include "mathutil.h"

using namespace std;
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
using namespace Solution;
using namespace System;
using namespace Ucomplex;
using namespace Utilities;
using namespace mathutil;

namespace SwtControl
{

TSwtControlObj::TSwtControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TSwtControlObj::TSwtControlObj(String ClassName) : inherited(ClassName) {}
TSwtControlObj::TSwtControlObj() {}


TSwtControlObj* ActiveSwtControlObj = nullptr;
const int NumPropsThisClass = 8;  // Creates superstructure for all SwtControl objects

TSwtControl::TSwtControl()
{
	;
	Class_Name = "SwtControl";
	DSSClassType = DSSClassType + SWT_CONTROL;
	DefineProperties();
	auto&& slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc.data(), NumProperties);
	CommandList.set_AbbrevAllowed(true);
}

//----------------------------------------------------------------------------------------

TSwtControl::~TSwtControl()
{
	// inherited::Destroy();
}


void TSwtControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();   /*see DSSClass*/
	PropertyName[1 - 1] = "SwitchedObj";
	PropertyName[2 - 1] = "SwitchedTerm";
	PropertyName[3 - 1] = "Action";
	PropertyName[4 - 1] = "Lock";
	PropertyName[5 - 1] = "Delay";
	PropertyName[6 - 1] = "Normal";
	PropertyName[7 - 1] = "State";
	PropertyName[8 - 1] = "Reset";

	PropertyHelp[1 - 1] = "Name of circuit element switch that the SwtControl operates. "
	           "Specify the full object class and name.";
	PropertyHelp[2 - 1] = "Terminal number of the controlled element switch. " "1 or 2, typically.  Default is 1.";
	PropertyHelp[3 - 1] = "DEPRECATED. See \"State\" property. ";
	PropertyHelp[4 - 1] = "{Yes | No} Controlled switch is locked in its present open / closed state or unlocked. "
	           "When locked, the switch will not respond to either a manual state change issued by the user or a state change issued internally by OpenDSS when reseting the control.";
	PropertyHelp[5 - 1] = "DEPRECATED.";
	PropertyHelp[6 - 1] = "ARRAY of strings {Open | Closed} representing the Normal state of the switch in each phase of the controlled element. "
	           "The switch reverts to this state for reset, change of mode, etc. "
			   "Defaults to \"State\" if not specifically declared.  Setting this property to {Open | Closed} sets the normal state to the specified value for all phases (ganged operation).";
	PropertyHelp[7 - 1] = "ARRAY of strings {Open | Closed} representing the Actual state of the switch in each phase of the controlled element. "
			   "Upon setting, immediately forces the state of the switch(es). Simulates manual control on Switch. Defaults to Closed for all phases. Setting this property to {Open | Closed} "
			   "sets the actual state to the specified value for all phases (ganged operation).";
	PropertyHelp[8 - 1] = "{Yes | No} If Yes, forces Reset of switch to Normal state and removes Lock independently of any internal "
	           "reset command for mode change, etc.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

int TSwtControl::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new SwtControl and add it to SwtControl class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TSwtControlObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

int TSwtControl::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int DevIndex = 0;
	int i = 0;

  // continue parsing WITH contents of Parser
	ActiveSwtControlObj = (TSwtControlObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveSwtControlObj);
	result = 0;
	/*# with ActiveSwtControlObj do */
	{
		auto with0 = ActiveSwtControlObj;
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
				with0->Set_PropertyValue(ParamPointer,Param);
			switch(ParamPointer)
			{
				case 	0:
           /*internal SwtControl Property commands*/
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 382);
				break;
				case 	1:
				with0->ElementName = LowerCase(Param);
				break;
				case 	2:
                with0->ElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	4:
				with0->set_Flocked(InterpretYesNo(Param));
				break;    // set the normal state
				case 	6: 
				{
					with0->InterpretSwitchState(ActorID, Param, ParamName);
					if(!with0->NormalStateSet)
						with0->NormalStateSet = true;
				}
				break;    // set the present state
				case 	3: case 7:
				{
					with0->InterpretSwitchState(ActorID, Param, ParamName);
				}
				break;
				case 	8:
				if(InterpretYesNo(Param))  // force a reset
				{
					with0->set_Flocked(false);
					with0->Reset(ActorID);
					with0->Set_PropertyValue(8,"n");
				}
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveSwtControlObj, ParamPointer - NumPropsThisClass);
				break;
			}

         /*supplemental actions*/
			switch(ParamPointer)
			{
				case 	3:case 	7:
				{
					int stop = 0;
					for(stop = ( (TDSSCktElement*) with0 )->Fnphases, i = 1; i <= stop; i++)
					{
						if(!with0->NormalStateSet)
							(*with0->FNormalState)[i - 1] = (*with0->FPresentState)[i - 1];
					}
					with0->NormalStateSet = true;   // normal state will default to state only the 1st state is specified.
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

int TSwtControl::MakeLike(const String SwtControlName)
{
	int result = 0;
	TSwtControlObj* OtherSwtControl = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this SwtControl name in the present collection*/
	OtherSwtControl = ((TSwtControlObj*) Find(SwtControlName));
	if(OtherSwtControl != nullptr)
		/*# with ActiveSwtControlObj do */
		{
			auto with0 = ActiveSwtControlObj;
			int stop = 0;
			with0->Set_NPhases(OtherSwtControl->Fnphases);
			with0->Set_Nconds(OtherSwtControl->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherSwtControl->ElementName;
			with0->ElementTerminal = OtherSwtControl->ElementTerminal;
			with0->Set_ControlledElement(OtherSwtControl->get_FControlledElement());  // Pointer to target circuit element
			with0->TimeDelay = OtherSwtControl->TimeDelay;
			with0->set_Flocked(OtherSwtControl->get_FLocked());

			for(stop = min(SWTCONTROLMAXDIM, with0->get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
			{
				(*with0->FPresentState)[i - 1] = (*OtherSwtControl->FPresentState)[i - 1];
				(*with0->FNormalState)[i - 1] = (*OtherSwtControl->FNormalState)[i - 1];
			}

			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherSwtControl->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in SwtControl MakeLike: \"") + SwtControlName
	           + "\" Not Found.", 383);
	return result;
}

/*==========================================================================*/
/*                    TSwtControlObj                                           */
/*==========================================================================*/

TSwtControlObj::TSwtControlObj(TDSSClass* ParClass, const String SwtControlName)
 : inherited(ParClass),
			FLocked(false)
{
    int i = 0;
    int stop = 0;
	Set_Name(LowerCase(SwtControlName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors in base class
	ElementName = "";
	Set_ControlledElement(nullptr);
	ElementTerminal = 1;
	
	FPresentState = nullptr;
	FNormalState = nullptr;

     // Reallocate arrays  (Must be initialized to nil for first call)
	FPresentState = (pStateArray) realloc(FPresentState, sizeof((*FPresentState)[1 - 1]) * Fnphases);
	FNormalState = (pStateArray) realloc(FNormalState, sizeof((*FNormalState)[1 - 1]) * Fnphases);
	for(stop = min(SWTCONTROLMAXDIM, Fnphases), i = 1; i <= stop; i++)
	{
		(*FPresentState)[i - 1] = CTRL_CLOSE;
		(*FNormalState)[i - 1] = CTRL_CLOSE;  // default to present state;
	}
	NormalStateSet = false;

	set_Flocked(false);
	TimeDelay = 120.0; // 2 minutes
	InitPropertyValues(0);
}

TSwtControlObj::~TSwtControlObj()
{
    FPresentState = (pStateArray)realloc(FPresentState, 0);
    FNormalState = (pStateArray)realloc(FNormalState, 0);
	// inherited::Destroy();
}


void TSwtControlObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
    int i = 0;
	DevIndex = GetCktElementIndex(ElementName);
	if(DevIndex > 0)
	{
        int myElmTerminal = 0;  // This to prevent exceptions for bad indexing 
        if (ElementTerminal > 0)
            myElmTerminal = ElementTerminal;
        else
            myElmTerminal = 1;

		Set_ControlledElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));		
		Set_NPhases(get_FControlledElement()->Get_NPhases());
		if(Fnphases > SWTCONTROLMAXDIM)
			DoSimpleMsg(String("Warning: SwitchControl ") + this->get_Name()
	           + ": Number of phases > Max SwtControl dimension.", 384);
		if(ElementTerminal > get_FControlledElement()->Get_NTerms())
		{
			DoErrorMsg(String("SwtControl: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Re-specify terminal no.", 384);
		}
		Set_Nconds(Fnphases);

		get_FControlledElement()->Set_ActiveTerminal(myElmTerminal);

		get_FControlledElement()->HasSwtControl = true;  // For Reliability calcs

		int stop = 0;
		// Open/Closed State of controlled element based on state assigned to the control
		for(stop = min(SWTCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			if((*FPresentState)[i - 1] == CTRL_OPEN)
			{
				get_FControlledElement()->Set_ConductorClosed(i, ActorID, false);
			}
			else
			{
				get_FControlledElement()->Set_ConductorClosed(i, ActorID, true);
			}
		}

    // attach controller bus to the switch bus - no space allocated for monitored variables
        SetBus(1, get_FControlledElement()->GetBus(myElmTerminal));
	}
	else
	{
		Set_ControlledElement(nullptr);   // element not found
		DoErrorMsg(String("SwtControl: \"") + this->get_Name() + "\"", String("CktElement Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 387);
	}
}

void TSwtControlObj::MakePosSequence(int ActorID)
{
	if(get_FControlledElement() != nullptr)
	{
		Set_NPhases(get_FControlledElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		SetBus(1, get_FControlledElement()->GetBus(ElementTerminal));
	}
	inherited::MakePosSequence(ActorID);
}

/*--------------------------------------------------------------------------*/

void TSwtControlObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil
}

void TSwtControlObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TSwtControlObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TSwtControlObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
    /*
	EControlAction ctrl_code;
	ctrl_code = EControlAction(Code);  // change type
	get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);
	switch(ctrl_code)
	{
		case 	CTRL_LOCK:
		set_Flocked(true);
		break;
		case 	CTRL_UNLOCK:
		set_Flocked(false);
		break;
		default:
		if(!get_FLocked())
		{
			if((Code == ( CTRL_OPEN)) && (get_FPresentState() == CTRL_CLOSE))
			{
				get_FControlledElement()->Set_ConductorClosed(0, ActorID, false); // Open all phases of active terminal
				Set_PresentState(CTRL_OPEN);
				AppendToEventLog(String("SwtControl.") + this->get_Name(), "Opened", ActorID);
			}
			if((Code == ( CTRL_CLOSE)) && (get_FPresentState() == CTRL_OPEN))
			{
				get_FControlledElement()->Set_ConductorClosed(0, ActorID, true);    // Close all phases of active terminal
				Set_PresentState(CTRL_CLOSE);
				AppendToEventLog(String("SwtControl.") + this->get_Name(), "Closed", ActorID);
			}
			Armed = false;  // reset the switch
		}
		break;
	}
	*/
}

/*--------------------------------------------------------------------------*/

void TSwtControlObj::InterpretSwitchState(int ActorID, const String Param, const String property_name)
{
    int i = 0;
    String DataStr1;
    String DataStr2;
    // Only allowed to change normal state if locked.
    if (get_FLocked() and (LowerCase(property_name)[0] == 'a' or LowerCase(property_name)[0] == 's'))
    {
		return;
    }

	if (LowerCase(property_name)[0] == 'a') // Interpret ganged specification to state and normal when using action
    {   // action (deprecated) will be removed
        int stop = 0;
        for (stop = SWTCONTROLMAXDIM, i = 1; i <= stop; i++)
        {
            switch (LowerCase(Param)[0])
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
		if (!AuxParser[ActorID]->IsQuotedString)  // Interpret ganged specification to state and normal when not quoted
        {
			int stop = 0;
			for (stop = SWTCONTROLMAXDIM, i = 1; i <= stop; i++)
			{
				if (LowerCase(property_name)[0] == 's')  // state
				{
					switch (LowerCase(Param)[0])
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
				else  // 'normal
				{
					switch (LowerCase(Param)[0])
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
        else
        {
			AuxParser[ActorID]->SetCmdString(Param); // Load up Parser
			DataStr1 = AuxParser[ActorID]->GetNextParam(); // ignore
			DataStr2 = AuxParser[ActorID]->MakeString_();
			i = 1;
			while ((DataStr2.size() > 0) && (i < SWTCONTROLMAXDIM))
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
				else
				// 'normal'
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

bool TSwtControlObj::get_FLocked()
{
	return FLocked;
}

//-------------------------------------------------------------------------------------

void TSwtControlObj::sample(int ActorID)
{

	/*
	// push on the Lock command if any at the present time delay
	if(LockCommand != CTRL_NONE)
		// with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
		{
			
			auto with1 = ActiveCircuit[ActorID]->Solution;
			ActiveCircuit[ActorID]->ControlQueue.Push(with1->DynaVars.intHour, with1->DynaVars.T + TimeDelay, LockCommand, 0, this, ActorID);
			LockCommand = CTRL_NONE;  // reset the lock command for next time
		}
	if((ActionCommand != get_FPresentState()) && !Armed)
		// with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
		{
			
			auto with3 = ActiveCircuit[ActorID]->Solution;   // we need to operate this switch
			ActiveCircuit[ActorID]->ControlQueue.Push(with3->DynaVars.intHour, with3->DynaVars.T + TimeDelay, ActionCommand, 0, this, ActorID);
			Armed = true;
		}
	*/
  /*ControlledElement.ActiveTerminalIdx := ElementTerminal;
  IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
  THEN PresentState := CTRL_CLOSE
  ELSE PresentState := CTRL_OPEN; */
}

void TSwtControlObj::set_Flocked(bool Value)
{
	FLocked = Value;
}

EControlAction TSwtControlObj::get_States(int Idx)
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

void TSwtControlObj::set_States(int Idx, const EControlAction Value)
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
                break;
                /*CTRL_CLOSE:*/
            default:
                get_FControlledElement()->Set_ConductorClosed(Idx, ActiveActor, true);
                break;
            }
        }
        (*FPresentState)[Idx - 1] = Value;
    }
}

EControlAction TSwtControlObj::get_NormalStates(int Idx)
{
    EControlAction result;
    result = (*FNormalState)[Idx - 1];
    return result;
}

void TSwtControlObj::set_NormalStates(int Idx, const EControlAction Value)
{
    if ((*FNormalState)[Idx - 1] != Value)
    {
        (*FNormalState)[Idx - 1] = Value;
    }
}

void TSwtControlObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue((with0->PropertyIdxMap)[i - 1])); }
		}
	}
	if(Complete)
		WriteLn(f);
}

String TSwtControlObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	int i = 0;
	switch(Index)
	{
		case 	6: case 	7:
		result = "[";
		break;
		default:
		result = "";
		break;
	}
	switch(Index)
	{
		case 	1:
		result = ElementName;
		break;
		case 	2:
		result = Format("%d", ElementTerminal);
		break;
		case 	4:
		if(get_FLocked())
			result = "Yes";
		else
			result = "No";
		break;
		case 	5:
		result = Format("%-.7g", TimeDelay);
		break;
		case 	6:
		if(get_FControlledElement() != nullptr)  // Special cases
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
		break;
		case 	7:
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
		break;
		case 	8:
		result = "n";
		break;  // Always no; yes is executed immediately
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
    switch (Index)
    {
    case 6: case 7:
        result = result + "]";
        break;
    default:
        break;
    }
	return result;
}

void TSwtControlObj::Reset(int ActorID)
{	
	int i = 0;
	if(!get_FLocked())
	{
		int stop = 0;
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
		for(stop = min(SWTCONTROLMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			(*FPresentState)[i - 1] = (*FNormalState)[i - 1];  // reset to normal state
			switch((*FNormalState)[i - 1])
			{
				case 	CTRL_OPEN:
				get_FControlledElement()->Set_ConductorClosed(i, ActiveActor, false);
				break;
            /*CTRL_CLOSE:*/
				default:
				get_FControlledElement()->Set_ConductorClosed(i, ActiveActor, true);
				break;
			}
		}
	}
}

void TSwtControlObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,""); //'element';
	Set_PropertyValue(2,"1"); //'terminal';
	Set_PropertyValue(3,"");  //'action';
	Set_PropertyValue(4,"n");
	Set_PropertyValue(5,"120.0");
	Set_PropertyValue(6,"");
	Set_PropertyValue(7,"[closed, closed, closed]");
	Set_PropertyValue(8,"[closed, closed, closed]");
	inherited::InitPropertyValues(NumPropsThisClass);
}




}  // namespace SwtControl





