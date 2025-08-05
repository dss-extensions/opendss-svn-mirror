
#pragma hdrstop

#include "pyControl.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Ucmatrix.h"
#include "mathutil.h"
#include <math.h>
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
using namespace UPFC;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace pyControl
{

TpyControlObj::TpyControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TpyControlObj::TpyControlObj(String ClassName) : inherited(ClassName) {}
TpyControlObj::TpyControlObj() {}


TpyControlObj* ActivepyControlObj = nullptr;
const int NumPropsThisClass = 1;
const string PIPE_FORMAT = "\\\\% s\\pipe\\% s"; // \\ServerName\pipe\PipeName 
const int PIPE_TIMEOUT = 5000;
const int BUFF_SIZE = 10000;


/*--------------------------------------------------------------------------*/  // Creates superstructure for all UPFCControl objects

TpyControl::TpyControl()
{
	;
	Class_Name = "pyControl";
	DSSClassType = DSSClassType + PY_CONTROLLER;
	DefineProperties();
	auto&& slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc.data(), NumProperties);
	CommandList.set_AbbrevAllowed(true);
}

/*--------------------------------------------------------------------------*/

TpyControl::~TpyControl()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TpyControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "pyScript";
	PropertyHelp[1 - 1] = "This is the path to the controller script, it is expected to be a python script (*.py). ";

	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TpyControl::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new pyControl and add it to pyControl class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TpyControlObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TpyControl::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int i = 0;

  // continue parsing WITH contents of Parser
	ActivepyControlObj = (TpyControlObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActivepyControlObj);
	result = 0;
	/*# with ActivepyControlObj do */
	{
		auto with0 = ActivepyControlObj;
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
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 364);
				break;
				case 1:
				{
					String RedirFile = ExpandFileName(Param);
					if (FileExists(RedirFile))
						with0->pyScript = RedirFile; 
					else
                        DoSimpleMsg("The given path '" + ParamName + "' for Object '" + Class_Name + "." + with0->get_Name() + "' does not exist", 36400);
				}
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveUPFCControlObj, ParamPointer - NumPropsThisClass);
				break;
			}
            ParamName = Parser[ActorID]->GetNextParam();
            Param = Parser[ActorID]->MakeString_();
		}
	}

  //RecalcElementData(ActorID);
	return result;
}



/*--------------------------------------------------------------------------*/

int TpyControl::MakeLike(const String pyControlName)
{
	int result = 0;
	TpyControlObj* OtherpyControl = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this UPFCControl name in the present collection*/
	OtherpyControl = ((TpyControlObj*) Find(pyControlName));
        if (OtherpyControl != nullptr)
		/*# with ActiveUPFCControlObj do */
		{
			auto with0 = ActiveUPFCControlObj;
			int stop = 0;
			with0->Set_NPhases(OtherpyControl->Fnphases);
            with0->Set_Nconds(OtherpyControl->Fnconds); // Force Reallocation of terminal stuff
            with0->ElementName = OtherpyControl->ElementName;
            with0->Set_ControlledElement(OtherpyControl->get_FControlledElement()); // Pointer to target circuit element
            with0->Set_MonitoredElement(OtherpyControl->get_FMonitoredElement()); // Pointer to target circuit element
            with0->ElementTerminal = OtherpyControl->ElementTerminal;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherpyControl->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in UPFCControl MakeLike: \"") + pyControlName
	           + "\" Not Found.", 370);
	return result;
}




/*==========================================================================*/
/*                    TpyControlObj                                           */
/*==========================================================================*/
/*--------------------------------------------------------------------------*/

TpyControlObj::TpyControlObj(TDSSClass* ParClass, const String pyControlName)
 : inherited(ParClass)
{
	Set_Name(LowerCase(pyControlName));
	DSSObjType = ParClass->DSSClassType;
    LastCMD = "";
    pyScript = "";
}

TpyControlObj::~TpyControlObj()
{
	ElementName = "";
	// inherited::Destroy();
}

/*--------------------------------------------------------------------------*/

void TpyControlObj::RecalcElementData(int ActorID)
{

/*Do nothing*/

}

/*--------------------------------------------------------------------------*/

void TpyControlObj::MakePosSequence(int ActorID)
{
	// This is here but it does nothing
	if(get_FMonitoredElement() != nullptr)
	{
		Set_NPhases(get_FControlledElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
	}
	inherited::MakePosSequence(ActorID);
}

/*--------------------------------------------------------------------------*/

void TpyControlObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}

/*--------------------------------------------------------------------------*/

void TpyControlObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TpyControlObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TpyControlObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		for (i = 1; i <= with0->NumProperties; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, "="); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
	{
		WriteLn(f);
	}
}

/*--------------------------------------------------------------------------*/
int TpyControlObj::HandlePIPE(int ActorID)
{
    bool POnline = true;
    int Written = 0;
    int Result = 0;
    String ClientCmd = "";
#ifdef windows
	//Windows routine
	while ((pyServer[ActorID] != INVALID_HANDLE_VALUE) && POnline)
    {
		ClientCmd = Read_From_PyServer(ActorID);
        if (ClientCmd == "closepipe")
        {
            // This means that the pyscript is done and we need to close the handler
            if (LowerCase(LastCMD) == "yes")
            {
                Result = 1;
            }
            POnline = false;
        }
        else
        {
            // The py script is sending commands or something different
            LastCMD = ClientCmd;
            if ((LowerCase(LastCMD) != "yes") && (LowerCase(LastCMD) != "no"))
            {
                DSSExecutive[ActorID]->Set_Command(ClientCmd);
                if (GlobalResult.empty())
                    GlobalResult = "OK";

                Write_2_PyServer(GlobalResult, ActorID);
            }
			else
                Write_2_PyServer("OK", ActorID);
        }
    }
#elif __linux__
    // Windows routine
    while ((pyServer[ActorID] != "") && POnline)
    {
        ClientCmd = Read_From_PyServer(ActorID);
        if (ClientCmd == "closepipe")
        {
            // This means that the pyscript is done and we need to close the handler
            if (LowerCase(LastCMD) == "yes")
            {
                Result = 1;
            }
            POnline = false;
        }
        else
        {
            // The py script is sending commands or something different
            LastCMD = ClientCmd;
            if ((LowerCase(LastCMD) != "yes") && (LowerCase(LastCMD) != "no"))
            {
                DSSExecutive[ActorID]->Set_Command(ClientCmd);
                if (GlobalResult.empty())
                    GlobalResult = "OK";

                Write_2_PyServer(GlobalResult, ActorID);
            }
            else
                Write_2_PyServer("OK", ActorID);
        }
    }
#endif
    return Result;
}

/*--------------------------------------------------------------------------*/

void TpyControlObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	// Do nothing
}

/*--------------------------------------------------------------------------*/

void TpyControlObj::sample(int ActorID)
{
	bool Update = false;
    String LPipeName = "";

#ifdef windows
    HANDLE pHandle = NULL;

	// First, check if the instance's pyServer is running, otherwise do nothing

    if (ASSIGNED(pyServer[ActorID]))
    {
        Write_2_PyServer(pyScript, ActorID);
        Update = (HandlePIPE(ActorID) == 1);

        /*Checks if the controller commands to implement control actions*/
        if (Update)
        {
            // This action is just to sync DSS with the external control action
            auto with0 = ActiveCircuit[ActorID];
            auto with1 = with0->Solution;

            with0->ControlQueue.Push(with1->DynaVars.intHour, with1->DynaVars.T, 0, 0, this, ActorID);
        }
        GlobalResult = "";
    }
#elif __linux__
    if (!(pyServer[ActorID].empty()))
    {
        Write_2_PyServer(pyScript, ActorID);
		Update = (HandlePIPE(ActorID) == 1);

		/*Checks if the controller commands to implement control actions*/
		if (Update)
		{
			// This action is just to sync DSS with the external control action
            auto with0 = ActiveCircuit[ActorID];
			auto with1 = with0->Solution;

			with0->ControlQueue.Push(with1->DynaVars.intHour, with1->DynaVars.T, 0, 0, this, ActorID);
		}
		GlobalResult = "";
    }
#endif
}

void TpyControlObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"");   //'pyScript';
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TpyControlObj::Reset(int ActorID)
{

  // inherited;
}




}  // namespace UPFCControl





