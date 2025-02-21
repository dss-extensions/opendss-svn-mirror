

#pragma hdrstop

#include "DSSGlobals.h"
#include "DynamicExp.h"

#include <errno.h>
#include <iostream>
#include <string>
#include "Sysutils.h"
#ifndef windows
#include <stdlib.h> // getenv
#include <unistd.h> // access
#include <time.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <pwd.h>
#include <errno.h>
#else
#include <synchapi.h>
#include <strsafe.h>
#include <tchar.h>
#include <conio.h>
#endif
#include "dirsep.h"
#include "d2c_structures.h"
#include <string>
#include <locale>
#include <codecvt>
#include <stdio.h>


#define BUFSIZE 10000;

namespace DSSGlobals
{

     bool DLLFirstTime;
     TTextRec DLLDebugFile;
     String ProgramName;
     TIniRegSave DSS_Registry; // Registry   (See Executive)

       // Global variables for the OpenDSS Viewer

     bool DSS_Viz_installed; // OpenDSS viewer (flag to mark a local installation)

     String DSS_Viz_path;
     bool DSS_Viz_enable;

    // Global variables for OpenDSS-GIS

     bool DSS_GIS_installed; // OpenDSS-GIS (flag to mark a local installation)

     String DSS_GIS_path;
     bool IsDLL, NoFormsAllowed;
     std::vector < TDSSCircuit* > ActiveCircuit;
     std::vector < TDSSClass* > ActiveDSSClass;
     std::vector < int > LastClassReferenced;  // index of class of last thing edited

     std::vector < void* > ActiveDSSObject;
     int NumCircuits;
     int MaxCircuits;
     int MaxBusLimit; // Set in Validation

     int MaxAllocationIterations;
     TPointerList Circuits;
     std::vector < TPointerList > DSSObjs;
     std::vector < TParser* > AuxParser;  // Auxiliary parser for use by anybody for reparsing values

    //{****} DebugTrace:TextFile;

     bool ErrorPending;
     int CmdResult, ErrorNumber;
     String LastErrorMessage;
     int DefaultEarthModel;
     std::vector < int > ActiveEarthModel;
     String LastFileCompiled;
     bool LastCommandWasCompile;
     complex CALPHA;  /*120-degree shift constant*/
     double SQRT2;
     double SQRT3;
     double InvSQRT3;
     double InvSQRT3x1000;
     bool SolutionAbort;
     bool InShowResults;
     bool Redirect_Abort;
     bool In_Redirect;
     std::vector < bool > DIFilesAreOpen;
     bool AutoShowExport;
     bool AutoDisplayShowReport;
     std::vector < bool > SolutionWasAttempted;
     String GlobalHelpString;
     String GlobalPropertyValue;
     String GlobalResult;
     String LastResultFile;
     String VersionString;
     String pyPath;
     bool LogQueries;
     bool QueryFirstTime;
     String QueryLogFileName;
     TTextRec QueryLogFile;
     String DefaultEditor;     // normally, Notepad

     int DefaultFontSize;
     String DefaultFontName;
     TFontStyles DefaultFontStyles;
     String DSSFileName;     // Name of current exe or DLL

     String DSSDirectory;     // where the current exe resides

     String StartupDirectory;     // Where we started

     std::vector < String > DataDirectory;     // used to be DSSDataDirectory

     std::vector < String > OutputDirectory;     // output files go here, same as DataDirectory if writable

     std::vector < String > CircuitName_;     // Name of Circuit with a "_" appended

     std::vector < pComplexArray > ActiveYPrim; // Created to solve the problems

     double DefaultBaseFreq;
     double DaisySize;

    // Some commonly used classes   so we can find them easily

     std::vector < TLoadShape* > LoadShapeClass;
     std::vector < TTShape* > TShapeClass;
     std::vector < TPriceShape* > PriceShapeClass;
     std::vector < TXYcurve* > XYCurveClass;
     std::vector < TGrowthShape* > GrowthShapeClass;
     std::vector < TSpectrum* > SpectrumClass;
     std::vector < TDSSSolution* > SolutionClass;
     std::vector < TEnergyMeter* > EnergyMeterClass;
     std::vector < TDSSFMonitor* > FMonitorClass;      // By dahei UCF
     std::vector < TDynamicExp* > TDynamicExpClass;
       // FeederClass        :TFeeder;

     std::vector < TDSSMonitor* > MonitorClass;
     std::vector < TSensor* > SensorClass;
     std::vector < TTCC_Curve* > TCC_CurveClass;
     std::vector < TWireData* > WireDataClass;
     std::vector < TCNData* > CNDataClass;
     std::vector < TTSData* > TSDataClass;
     std::vector < TLineSpacing* > LineSpacingClass;
     std::vector < TStorage* > StorageClass;
     std::vector < TPVSystem* > PVSystemClass;
     std::vector <TWindGen*> WindGenClass;
     std::vector <TReactor*> ReactorClass;
     std::vector < TInvControl* > InvControlClass;
     std::vector < TExpControl* > ExpControlClass;
     std::vector < TVsource* > ActiveVSource;   // created on 01/14/2019 to facilitate actors to modify VSources while simulating
     std::vector < TpyControl* > pyControlClass;

     std::vector < TStringList > EventStrings;
     std::vector < TStringList > SavedFileList;
     std::vector < TStringList > ErrorStrings;
     std::vector < TPointerList > DSSClassList; // pointers to the base class types

     std::vector < THashList > ClassNames;
     bool UpdateRegistry;  // update on program exit

#ifdef windows
     __int64 CPU_Freq;          // Used to store the CPU frequency
#else
     int64_t CPU_Freq;          // Used to store the CPU frequency
#endif

     const int32 max_CPU_Cores = 256;
     int32 CPU_Cores;
     int NumNUMA;        // To store the number of NUMA nodes (should be the same as sockets)
     bool EventLogDefault = false;
     int32 CPU_Physical;
     int ActiveActor;
     int NumOfActors;
     std::vector < int > ActorCPU;
     std::vector < std::atomic<int> > ActorStatus(max_CPU_Cores+1);
     std::vector < int > ActorProgressCount;
#ifdef windows
     std::vector<HANDLE> pyServer;
#elif __linux__
     std::vector<string> pyServer;
#endif

    // TProgress* ActorProgress;
     std::vector < int > ActorPctProgress;
     std::vector < TSolver* > ActorHandle;
     std::vector < TThread* > ActorThread;

    //***********************A-Diakoptics suite globals*****************************
       // To indicate if the tearing process will take place using the link branches given by the user

     bool AllActors, ADiakoptics, ADiak_Init, ADiak_PCInj, UseUserLinks, Parallel_enabled, ConcatenateReports, ProgressCmd, IncMat_Ordered;
     std::vector < TParser* > Parser;
     std::vector < TEvent > ActorMA_Msg;  // Array to handle the events of each actor

       // Default ports

     int DSSPrgPort, DSSGISPort;


    /********************************************************************************
    *    Nomenclature:                                                             *
    *                  OV_ Overloads                                               *
    *                  VR_ Voltage report                                          *
    *                  DI_ Demand interval for each meter. Moved to EnergyMeter.pas*
    *                  SDI_ System Demand interval                                 *
    *                  TDI_ DI Totals                                              *
    *                  FM_  Meter Totals                                           *
    *                  SM_  System Meter                                           *
    *                  EMT_  Energy Meter Totals                                   *
    *                  PHV_  Phase Voltage Report. Moved to EnergyMeter.pas        *
    *     These prefixes are applied to the variables of each file mapped into     *
    *     Memory using the MemoryMap_Lib                                           *
    ********************************************************************************
    */
     std::vector < TBytesStream* > OV_MHandle;  // a. Handle to the file in memory

     std::vector < TBytesStream* >  VR_MHandle;
     std::vector < TBytesStream* >  SDI_MHandle;
     std::vector < TBytesStream* >  TDI_MHandle;
     std::vector < TBytesStream* >  SM_MHandle;
     std::vector < TBytesStream* >  EMT_MHandle;
     std::vector < TBytesStream* >  FM_MHandle;

    //*********** Flags for appending Files*****************************************

     std::vector < bool > OV_Append;
     std::vector < bool > VR_Append;
     std::vector < bool > DI_Append;
     std::vector < bool > SDI_Append;
     std::vector < bool > TDI_Append;
     std::vector < bool > SM_Append;
     std::vector < bool > EMT_Append;
     std::vector < bool > PHV_Append;
     std::vector < bool > FM_Append;

     string RepTermination = ".csv";

    //***********************Seasonal QSTS variables********************************

     bool SeasonalRating;    // Tells the energy meter if the seasonal rating feature is active

     String SeasonSignal;     // Stores the name of the signal for selecting the rating dynamically

     std::vector < TExecutive* > DSSExecutive;
     TDSSClasses* DSSClasses;
     std::vector < TIsource* > IsourceClass;
     std::vector < TVsource* > VSourceClass;

    //************************ Progress actor Global defs***************************

     bool DSSProgressFrm, IsProgressON;
     std::vector < TProgressActor* > Progress_Actor;
     String DSSProgressPath;

    //************************ OpenDSS-GIS Global defs***************************

     bool IsGISON;
     String GISThickness, GISColor;
     pDoubleArray GISCoords;

     String DSSpyServerPath;
     vector <String> LPipeName;

     TCommandList LineTypeList;

     vector<uint8_t>     myStrArray;
     vector<double>   myDblArray;
     vector<complex>  myCmplxArray;
     vector<polar>    myPolarArray;
     vector<int>      myIntArray;
     int FPropIndex;

     // For functions previously in DYMatrix.pas
     void* Yhandle;
     unsignedint NumNZ, NumBuses;
     int* YColumns;
     int* YRows;
     complex *YValues;

    //typedef int THandle;
    typedef int (*TDSSRegister)(Char*&);  // Returns base class 1 or 2 are defined
       // Users can only define circuit elements at present


    THandle LastUserDLLHandle = 0;
    TDSSRegister DSSRegisterProc;        // of last library loaded

    //TIdTCPClient IdTCPClient;  // ... TIdThreadComponent

    //TIdThreadComponent IdThreadComponent;

    TSQueue<int> WaitQ;

    
    //*******************************************************************************************************


    string GetHomeDir()
    {
#ifdef windows
        // These are static so that we only call getenv once or report if unset:
        static const char* home_path = getenv("HOMEPATH");
        static const char* home_drive = getenv("HOMEDRIVE");

        if (!home_drive || !home_path) { // getenv returned NULL indicating the variable was unset,
            if (!home_drive)
                DoSimpleMsg(String("Cannot determine user home drive from " "HOMEDRIVE" " environment variable."), 0); // TODO: replace last 0 parameter with unique number?
            if (!home_path)
                DoSimpleMsg(String("Cannot determine user home path from " "HOMEPATH" " environment variable."), 0); // TODO: replace last 0 parameter with unique number?
            DoSimpleMsg(String("Falling back to using the current working directory."), 0); // TODO: replace last 0 parameter with unique number?
            home_path = "."; // so fall back to using the current working directory.
            home_drive = ""; // so fall back to using the current drive.
        }
        string home_dir = string(home_drive) + string(home_path);
        if (!DirectoryExists(home_dir.c_str())) {
            DoSimpleMsg(string("Cannot find user home directory: ")+home_dir, 0); // TODO: replace last 0 parameter with unique number?
        }
        return home_dir;
#else
        // home_dir is static so that we only call getenv once or report if unset
        static const char* home_dir = getenv("HOME");

        if (!home_dir) { // getenv returned NULL indicating the variable was unset,
            DoSimpleMsg(String("Cannot determine user home directory from " "HOME" " environment variable."), 0); // TODO: replace last 0 parameter with unique number?
            DoSimpleMsg(String("Falling back to using the current working directory."), 0); // TODO: replace last 0 parameter with unique number?
            home_dir = "."; // so fall back to using the current working directory.
        }
        if (!DirectoryExists(home_dir)) {
            DoSimpleMsg(string("Cannot find user home directory: ")+home_dir, 0); // TODO: replace last 0 parameter with unique number?
        }
        return string(home_dir);
#endif
    }


    inline String GetDefaultDataDirectory()
    {
        return GetHomeDir();
    }


    inline String GetDefaultScratchDirectory()
    {
        return GetHomeDir();
    }


    String GetOutputDirectory()
    {
        String result;
        result = OutputDirectory[ActiveActor];
        return result;
    }

    /*--------------------------------------------------------------*/


    bool IsDSSDLL(String Fname)
    {
        bool result = false;
        result = false;
        /*
            // Ignore if "DSSLIB.DLL"
          if ( CompareText( ExtractFileName( Fname ), "dsslib.dll" ) == 0 )
            return result;
          LastUserDLLHandle = LoadLibrary( Fname.c_str());
          if ( LastUserDLLHandle != 0 )
          {

           // Assign the address of the DSSRegister proc to DSSRegisterProc variable
            DSSRegisterProc = GetProcAddress( LastUserDLLHandle, "DSSRegister" );
            if ( DSSRegisterProc != NULL )
              result = true;
            else
              FreeLibrary( LastUserDLLHandle );
          }*/
        return result;
    }


//***********************DirectDLL interfacing globals**************************

    void WriteStr2Array(String myStr)
    {
        int i = 0;
        for (i = 0; i < myStr.size(); i++)
        {
            myStrArray.push_back(uint8_t(myStr[i]));
        }
    }

    String BArray2Str(void* myPtr, int* idx)
    {
        string S = "";
        uint8_t* PChar;
        int     i = *idx;

        PChar = *(uint8_t**)myPtr;
        while (PChar[i] != 0)
        {
            S = S + char(PChar[i]);
            i++;
        }
        *idx = i + 1;
        return S;
    }

    String Char0()
    {
        string S(1, char(0));
        return S;
    }

    //----------------------------------------------------------------------------

    void DoErrorMsg(const String S, const String Emsg, const String ProbCause, int ErrNum)
    {
        String Msg;
        int Retval = 0;
        Msg = "Error " + to_string(ErrNum) + " Reported From OpenDSS Intrinsic Function : " + CRLF + S + CRLF + CRLF + "Error Description : " + CRLF + Emsg + CRLF + CRLF + "Probable Cause : "
            + CRLF + ProbCause;
        if (!NoFormsAllowed)
        {
            if (In_Redirect)
            {
                Retval = DSSMessageDlg(Msg, false);
                if (Retval == -1)
                    Redirect_Abort = true;
            }
            else
                DSSMessageDlg(Msg, true);
        }
        LastErrorMessage = Msg;
        ErrorNumber = ErrNum;
        AppendGlobalResultCRLF(Msg);
        SolutionAbort = true;
    }

    //----------------------------------------------------------------------------



    void AppendGlobalResultCRLF(String S)
    {
        if (GlobalResult.length() > 0)
            GlobalResult = GlobalResult + CRLF + S;
        else
            GlobalResult = S;
        ErrorStrings[ActiveActor].insert(ErrorStrings[ActiveActor].end(), to_string(ErrorNumber) + S);  // Add to Error log
    }

    //----------------------------------------------------------------------------



    void DoSimpleMsg(const String S, int ErrNum)
    {
        int Retval = 0;
        if (!NoFormsAllowed)
        {
            if (In_Redirect)
            {
                Retval = DSSMessageDlg(to_string(ErrNum) + " OpenDSS " + CRLF + S, false);
                if (Retval == -1)
                    Redirect_Abort = true;
            }
            else
                DSSInfoMessageDlg(to_string(ErrNum) + " OpenDSS " + CRLF + S);
        }
        LastErrorMessage = S;
        ErrorNumber = ErrNum;
        AppendGlobalResultCRLF(S);
    }

    //----------------------------------------------------------------------------



    void DoThreadSafeMsg(const String S, int ErrNum)
        // generates a dialog window thread safe using windows API

    {
        int Retval = 0;
        if (!NoFormsAllowed)
        {
            if (In_Redirect)
            {
                Retval = DSSMessageDlg(to_string(ErrNum) + " OpenDSS " + CRLF + S, false);
                if (Retval == 3)
                    Redirect_Abort = true;
            }
            else
                DSSInfoMessageDlg(to_string(ErrNum) + " OpenDSS " + CRLF + S);
        }
        LastErrorMessage = S;
        ErrorNumber = ErrNum;
        AppendGlobalResultCRLF(S);
    }
    //----------------------------------------------------------------------------



    void SetObject(const String param)

        /*Set object active by name*/
    {
        String ObjName, ObjClass;

        // Split off Obj class and name
        size_t dotpos = param.find(".");
        switch (dotpos)
        {
        case String::npos:
            ObjName = param.substr(0, param.length());
            break;  // assume it is all name; class defaults
        default:
        {
            ObjClass = param.substr(0, dotpos);
            ObjName = param.substr(dotpos + 1, param.length());
        }
        }
        if (ObjClass.length() > 0)
            SetObjectClass(ObjClass);
        ActiveDSSClass[ActiveActor] = (TDSSClass*)DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
        if (ActiveDSSClass[ActiveActor] != NULL)
        {
            if (!ActiveDSSClass[ActiveActor]->SetActive(ObjName))
            { // scroll through list of objects untill a match
                DoSimpleMsg(String("Error! Object \"") + ObjName + "\" not found." + CRLF + Parser[ActiveActor]->get_CmdBuffer(), 904);
            }
            else
            {
                /*# with ActiveCircuit[ActiveActor] do */
                TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
                {
                    switch (((TDSSObject*)ActiveDSSObject[ActiveActor])->DSSObjType)
                    {
                    case DSS_OBJECT:
                        break;  // do nothing for general DSS object
                    default:
                    {   // for circuit types, set ActiveCircuit Element, too
                        with0->Set_ActiveCktElement((TDSSCktElement*)ActiveDSSClass[ActiveActor]->GetActiveObj());
                    }
                    }
                }
            }
        }
        else
            DoSimpleMsg("Error! Active object type/class is not set.", 905);
    }

    int SetActiveBus(const String BusName)
    {
        int result = 0;

        // Now find the bus and set active
        result = 0;
        /*# with ActiveCircuit[ActiveActor] do */
        {
            TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
            {
                if (with0->BusList.Get_NumElements() == 0)
                    return result;   // Buslist not yet built
                with0->ActiveBusIndex = with0->BusList.Find(BusName) - 1;
                if (with0->ActiveBusIndex < 0)
                {
                    result = 1;
                    AppendGlobalResult(String("SetActiveBus: Bus ") + BusName + " Not Found.");
                }
            }
        }
        return result;
    }


    void ClearAllCircuits()
    {
        bool isJoinable;
        int I = 0;
        for (int stop = NumOfActors, I = 1; I <= stop; I++)
        {
            if (ActiveCircuit[I] != NULL)
            {
                ActiveActor = I;
                ActiveCircuit[I]->NumCircuits = 0;
                delete ActiveCircuit[I];
                ActiveCircuit[I] = NULL;

                // In case the actor hasn't been destroyed
                if (ActorHandle[I] != NULL)
                {
                    ActorHandle[I]->Send_Message(EXIT_ACTOR);
                    ActorHandle[I] = NULL;
                }
            }
        }
        Circuits = TPointerList(2);   // Make a new list of circuits
          // Revert on key global flags to Original States
        DefaultEarthModel = DERI;
        LogQueries = false;
        MaxAllocationIterations = 2;
        ActiveActor = 1;
    }


    void MakeNewCircuit(const String Name)

        //Var
        //   handle :Integer;

    {
        String S;
        if (ActiveActor <= CPU_Cores)
        {
            if (ActiveCircuit[ActiveActor] == NULL)
            {
                ActiveCircuit[ActiveActor] = new TDSSCircuit(Name);
                ActiveDSSObject[ActiveActor] = ActiveSolutionObj;
                /**Handle := **/
                Circuits.Add(ActiveCircuit[ActiveActor]);
                ActiveCircuit[ActiveActor]->NumCircuits++;
                S = Parser[ActiveActor]->Get_Remainder();    // Pass remainder of string on to vsource.
                     /*Create a default Circuit*/
                SolutionAbort = false;
                /*Voltage source named "source" connected to SourceBus*/
                DSSExecutive[ActiveActor]->Set_Command( String("New object=vsource.source Bus1=SourceBus ") + S);  // Load up the parser as if it were read in
                     // Creates the thread for the actor if not created before
                if (ActorHandle[ActiveActor] == NULL)
                    New_Actor(ActiveActor);
            }
            else
            {
                DoErrorMsg("MakeNewCircuit", "Cannot create new circuit.", "Max. Circuits Exceeded." + CRLF + "(Max no. of circuits=" + to_string(MaxCircuits) + ")", 906);
            }
        }
        else
        {
            DoErrorMsg("MakeNewCircuit", "Cannot create new circuit.", "All the available CPUs have being assigned", 7000);
        }
    }

    //----------------------------------------------------------------------------

    void AppendGlobalResult(const String S)

        // Append a string to Global result, separated by commas

    {
        if (GlobalResult.length() == 0)
            GlobalResult = S;
        else
            GlobalResult = GlobalResult + ", " + S;
    }


    std::string GetDSSVersion()
    {
        String result;
        //DWORD InfoSize = 0, Wnd = 0;
        //void* VerBuf = NULL;
        //PVSFixedFileInfo FI;
        //DWORD VerSize = 0;
        //DWORD MajorVer = 0, MinorVer = 0, BuildNo = 0, RelNo = 0;
        //DWORD iLastError = 0;
        //result = "Unknown.";
        //InfoSize = GetFileVersionInfoSize( DSSFileName.c_str(), Wnd );
        //if ( InfoSize != 0 )
        //{
        //  GetMem( VerBuf, InfoSize );
        //  try
        //  {
        //    if ( GetFileVersionInfo( DSSFileName.c_str(), Wnd, InfoSize, VerBuf ) )
        //      if ( VerQueryValue( VerBuf, DIRSEP_CHAR, ((void*) FI ), VerSize ) )
        //      {
        //        MinorVer = FI.dwFileVersionMS & 0xFFFF;
        //        MajorVer = ( FI.dwFileVersionMS & 0xFFFF0000 ) >> 16;
        //        BuildNo = FI.dwFileVersionLS & 0xFFFF;
        //        RelNo = ( FI.dwFileVersionLS & 0xFFFF0000 ) >> 16;
        //        result = Format( "%d.%d.%d.%d", ARRAYOFCONST(( MajorVer, MinorVer, RelNo, BuildNo )) );
        //      }
        //  }
        //  __finally
        //  {
        //    FreeMem( VerBuf );
        //  }
        //}
        //else
        //{
        //  iLastError = GetLastError;
        //  result = Format( "GetFileVersionInfo failed: (%d) %s", ARRAYOFCONST(( iLastError, SysErrorMessage( iLastError ) )) );
        //}
        result = OPENDSSC_PROJECT_VERSION " @ C++";  // returns a string for now
        return result;
    }

    void WriteDLLDebugFile(const String S)
    {
        AssignFile(DLLDebugFile, OutputDirectory[ActiveActor] + "DSSDLLDebug.TXT");
        if (DLLFirstTime)
        {
            Rewrite(DLLDebugFile);
            DLLFirstTime = false;
        }
        else
            Append(DLLDebugFile);
        IOResultToException();
        WriteLn(DLLDebugFile, S);
        CloseFile(DLLDebugFile);
    }


    bool IsDirectoryWritable(const String Dir)
    {
       
        bool result = false;

        using namespace std::chrono;
        milliseconds ms = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
        std::string myTS = to_string(ms.count());
        
        std::string myPath = Dir + myTS;

        std::ofstream outfile(myPath.c_str());
        outfile.close();

        if (!outfile.fail() && !outfile.bad())
            result = DeleteFile(myPath);
        else
            result = false;
        return result;
    }

    void SetDataPath(const String PathName)
    {
        String ScratchPath;
        // Pathname may be null
        if ((PathName.length() > 0) && !DirectoryExists(PathName))
        {
            // Try to create the directory
            if (!CreateDir(PathName))
            {
                DoSimpleMsg(String("Cannot create ") + PathName + " directory.", 907);
                return;
            }
        }
        DataDirectory[ActiveActor] = PathName;

        // Put a \ on the end if not supplied. Allow a null specification.
        if (DataDirectory[ActiveActor].length() > 0)
        {
            ChDir(DataDirectory[ActiveActor]);   // Change to specified directory
            if (DataDirectory[ActiveActor][DataDirectory[ActiveActor].size() - 1] != DIRSEP_CHAR)
                DataDirectory[ActiveActor] = DataDirectory[ActiveActor] + DIRSEP_STR;
        }

        // see if DataDirectory is writable. If not, set OutputDirectory to the user's appdata
        if (IsDirectoryWritable(DataDirectory[ActiveActor]))
        {
            OutputDirectory[ActiveActor] = DataDirectory[ActiveActor];
        }
        else
        {
            ScratchPath = GetDefaultScratchDirectory() + DIRSEP_STR + ProgramName + DIRSEP_STR "Scratch" DIRSEP_STR;
            if (!DirectoryExists(ScratchPath))
                CreateDir(ScratchPath);
            OutputDirectory[ActiveActor] = ScratchPath;
        }
    }

#ifndef windows
    static std::string FindDefaultEditor()
    {
      const char *editors[] = {
        getenv("VISUAL"),
        getenv("EDITOR"),
        "/usr/local/bin/nano", // See if there's a friendly editor available.
        "/usr/bin/nano",
        "/usr/bin/vi", // This editor is friendly but picky about its friends.
      };
      unsigned num_editors = sizeof(editors)/sizeof(editors[0]);
      for (unsigned ndx=0; ndx<num_editors; ndx++) {
        const char *editor = editors[ndx];
        if (editor && 0==access(editor, X_OK)) // access returns 0 for success
          return std::string(editor);
      }
      return std::string("/bin/vi"); // Yep.  vi is in IEEE Std 1003.1
    }
#endif

    void ReadDSS_Registry()
    {
        String TestDataDirectory;
        DSS_Registry.Set_FSection("MainSect");
#ifdef windows
        DefaultEditor = "Notepad.exe";// DSS_Registry.ReadString("Editor", "Notepad.exe");
#else
        DefaultEditor = FindDefaultEditor();
#endif
        DefaultFontSize = StrToInt(DSS_Registry.ReadString("ScriptFontSize", "8"));
        DefaultFontName = DSS_Registry.ReadString("ScriptFontName", "MS Sans Serif");
        DefaultFontStyles = 1;
        DefaultBaseFreq = StrToInt(DSS_Registry.ReadString("BaseFrequency", "60"));
        LastFileCompiled = DSS_Registry.ReadString("LastFile", "");
        TestDataDirectory = DSS_Registry.ReadString("DataPath", DataDirectory[ActiveActor]);
        if (DirectoryExists(TestDataDirectory))
            SetDataPath(TestDataDirectory);
        else
            SetDataPath(DataDirectory[ActiveActor]);
    }

    void WriteDSS_Registry()
    {
        if (UpdateRegistry)
        {
            DSS_Registry.Set_FSection("MainSect");
            DSS_Registry.WriteString("Editor", DefaultEditor);
            DSS_Registry.WriteString("ScriptFontSize", Format("%d", DefaultFontSize));
            DSS_Registry.WriteString("ScriptFontName", DefaultFontName);
            DSS_Registry.WriteBool("ScriptFontBold", false);
            DSS_Registry.WriteBool("ScriptFontItalic", false);
            DSS_Registry.WriteString("BaseFrequency", Format("%d", Round(DefaultBaseFreq)));
            DSS_Registry.WriteString("LastFile", LastFileCompiled);
            DSS_Registry.WriteString("DataPath", DataDirectory[ActiveActor]);
        }
    }

    void ResetQueryLogFile()
    {
        QueryFirstTime = true;
    }


    void WriteQueryLogFile(const String Prop, const String S)

        /*Log file is written after a query command if LogQueries is true.*/
    {
        try
        {
            QueryLogFileName = OutputDirectory[ActiveActor] + "QueryLog.CSV";
            AssignFile(QueryLogFile, QueryLogFileName);
            if (QueryFirstTime)
            {
                Rewrite(QueryLogFile);  // clear the file
                IOResultToException();
                WriteLn(QueryLogFile, "Time(h), Property, Result");
                QueryFirstTime = false;
            }
            else
            {
                Append(QueryLogFile);
                IOResultToException();
            }
            WriteLn(QueryLogFile, Format("%.10g, ", (int64_t) ActiveCircuit[ActiveActor]->Solution->DynaVars.dblHour) + Prop + ", " + S);
            CloseFile(QueryLogFile);
        }
        catch (exception& E)
        {
            DoSimpleMsg("Error writing Query Log file: " + (string)E.what(), 908);
        }
    }


    void SetLastResultFile(const String Fname)
    {
        LastResultFile = Fname;
        ParserVars->Add("@lastfile", Fname);
    }


    void* MyAllocMem(unsignedint nbytes)
    {
        void* result = NULL;
        result = malloc(nbytes);
        WriteDLLDebugFile(Format("Allocating %d bytes @ %p", nbytes, 0));
        return result;
    }

    void MyReallocMem(void*& p, int newsize)
    {
        WriteDLLDebugFile(Format("Reallocating @ %p, new size= %d", p, newsize));
        ReallocMem(p, newsize);
    }

    //******************************************************************************
    // 
    // Waits for all the actors running tasks
    // 
    void Wait4Actors(int WType)
    {
#ifndef _WIN32
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 1000; // 1 ms
#endif
        int I = 0,
            j = 0;
        // uses the thread safe queue to wait for all the actors to confirm
        // replaces previous apporach based on delays
        if (!NoFormsAllowed)
            CoutLn("Waiting...");
        try
        {
            while (I < NumOfActors)
            {
                I = 0;
                for (j = 1; j <= NumOfActors; j++)
                {
                    if (ActorStatus[j] == 1)
                        I++;
                }
                if (I < NumOfActors)    // This to prevent waiting when the command is called in th wrong place
                    WaitQ.pop();
            }
        }
        catch (EOutOfMemory&)
        {
            DoSimpleMsg("Exception Waiting for the parallel thread to finish a job", 7006);
        }

    }

    //******************************************************************************
    // Waits for all the other actors except 1 (used while solving the A-Diakoptics solution method)
    void Wait4AD() // To differentiate the wait from general calls
    {
        bool Flag = false;
        for (int i = 2; i <= NumOfActors; i++)
        {
            try 
            {
                while (ActorStatus[i] == 0)
                    Flag = true;
            }
            catch (...)
            {
                DoSimpleMsg("Exception Waiting for the parallel thread to finish a job", 7006);
            }
        }
    }

    std::vector<char> encode_utf16le(const std::string& utf8_string)
    {
        std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> converter;
        std::u16string utf16_string = converter.from_bytes(utf8_string);

        std::vector<char> utf16le_bytes((utf8_string.size() + 1) * 2);

        int idx = 0;
        for (char16_t c : utf16_string)
        {
            utf16le_bytes[idx] = (c & 0xFF); // Low byte
            idx++;
            utf16le_bytes[idx] = ((c >> 8) & 0xFF); // High byte
            idx++;
        }
        return utf16le_bytes;
    }
 

    void Write_2_PyServer(String Msg, int ActorID)
    {
        bool SMessage = false;
        DWORD Bytes = 0,
              cbReplyBytes = 0;

        vector <char> lpvMessage = encode_utf16le(Msg);
        cbReplyBytes = Msg.size() * (sizeof(lpvMessage[0]) * 2);
#ifdef windows
        SMessage = WriteFile(
            pyServer[ActorID],
            &lpvMessage[0],
            cbReplyBytes, // = length of string + terminating '\0' !!!
            &Bytes,
            NULL);
#elif __linux__
        int fd = open(pyServer[ActorID].c_str(), O_WRONLY); 
        int wr = write(fd, &lpvMessage[0], cbReplyBytes);
        close(fd);
#endif
    }

    String Read_From_PyServer(int ActorID)
    {
        char MyMsg[10000];
        String MsgStr = "";
        String Result = "";
        DWORD MsgSz = 0;

        for (int i = 0; i < 10000; i++)
            MyMsg[i] = 0;
        bool MessageReceived = false;
#ifdef windows
        MessageReceived = ReadFile(
            pyServer[ActorID], // pipe handle
            MyMsg, // buffer to receive reply
            10000, // size of buffer
            &MsgSz, // number of bytes read
            NULL); // not overlapped
#elif __linux__

        std::string received_data;
        ssize_t bytes_read;
        try
        {
            int fd = open(pyServer[ActorID].c_str(), O_RDONLY);
            // Read from the FIFO. This will block until data is available.
            while ((bytes_read = read(fd, MyMsg, sizeof(MyMsg) - 1)) > 0)
            {
                MyMsg[bytes_read] = '\0'; // Null-terminate the buffer.
                received_data += MyMsg;
            }
            close(fd);
            MessageReceived = true;

        }
        catch (...)
        {
            DoSimpleMsg("Exception reading data from DSSpyServer", 7010);
        }
#endif
        
        if (MessageReceived)
        {
            bool eofFlg = false;
            for (int i = 0; i < 10000; i++)
            {
                if (MyMsg[i] > 0)
                {
                    Result = Result + MyMsg[i];
                    eofFlg = false;
                }
                else
                {
                    if (eofFlg && (MyMsg[i] == 0))
                        break;
                    eofFlg = true;
                }
            }
        }
        
        return Result;
    }

    void DoClone()
    {
        String  dummy = "";
        int     I = 0, 
                NumClones = 0;
        String Ref_Ckt = "";

        Ref_Ckt     = LastFileCompiled;
        dummy       = Parser[ActiveActor]->GetNextParam();
        NumClones   = Parser[ActiveActor]->MakeInteger_();
        Parallel_enabled = false;

        if (((NumOfActors + NumClones) <= CPU_Cores) && (NumClones > 0))
        {
            for (int stop = NumClones, I = 1; I <= stop; I++)
            {
                New_Actor_Slot();
                DSSExecutive[ActiveActor]->Set_Command( "compile \"" + Ref_Ckt + "\"");
                // sets the previous maxiterations and controliterations
                ActiveCircuit[ActiveActor]->Solution->MaxIterations = ActiveCircuit[1]->Solution->MaxIterations;
                ActiveCircuit[ActiveActor]->Solution->MaxControlIterations = ActiveCircuit[1]->Solution->MaxControlIterations;
                // Solves the circuit
                CmdResult = ExecOptions::DoSetCmd(1);
            }

        }
        else
        {
            if (NumClones > 0)
                DoSimpleMsg("There are no more CPUs available", 7001);
            else
                DoSimpleMsg("The number of clones requested is invalid", 7004);
        }
    }

    // Prepares memory to host a new actor



    void New_Actor_Slot()
    {
        if (NumOfActors < CPU_Cores)
        {
            NumOfActors++;
            GlobalResult = to_string(NumOfActors);
            ActiveActor = NumOfActors;
            ActorCPU[ActiveActor] = -1;       // By default, the actor will have affinity to all processors (-1)
            DSSExecutive[ActiveActor] = new TExecutive();  // Make a DSS object
            Parser[ActiveActor] = new TParser();
            AuxParser[ActiveActor] = new TParser();
            DSSExecutive[ActiveActor]->CreateDefaultDSSItems();
        }
        else
            DoSimpleMsg("There are no more CPUs available", 7001);
    }

    // Creates a new actor


    void New_Actor(int ActorID)
    {
        ActorHandle[ActorID] = new TSolver(ActorCPU[ActorID], ActorID); // TEMC: TODO: text-mode callback
        ActorStatus[ActorID] = 1;
    }

    String WString2Str(wstring wstr)
    {
        size_t len = wcstombs(nullptr, wstr.c_str(), 0) + 1;
        // Creating a buffer to hold the multibyte string
        char* buffer = new char[len];
        // Converting wstring to string
        wcstombs(buffer, wstr.c_str(), len);
        // Creating std::string from char buffer
        string str(buffer);
        // Cleaning up the buffer
        delete[] buffer;

        return str;
    }

    // Reads the given .ini file and returns the value for the given
    // "Prop" (Property) at the given "key". 
#ifdef windows
    String GetIni(wstring IniPath, String Prop, String key)
    {
        String Result = "";
        TTextRec F;
        String InputLine = "";
        bool keyFound = false;
        AssignFile(F, WString2Str(IniPath));
        Reset(F);
        IOResultToException();
        while (!Eof(F))
        {
            ReadLn(F, InputLine);
            if (InputLine.size() > 0)
            {
                int Spos = InputLine.find("[" + key + "]");
                if (Spos == 0)
                {
                    keyFound = true;
                }
                else
                {
                    Spos = InputLine.find("[");
                    if (Spos == 0)
                        keyFound = false;  // means we are in a different key
                    else
                    {
                        //it is probably content, check if the property is here, if so and the key was previously found, then we are done
                        Spos = InputLine.find(Prop);
                        if ((Spos == 0) && keyFound)
                        {
                            // Here it is, take the value and break the cycle
                            Spos = InputLine.find("=");
                            Result = InputLine.substr(Spos + 1, InputLine.size() - 1);
                            break;
                        }
                    }
                }

            }
        }
        CloseFile(F);

        return Result;
    }
#else
    String GetIni(string IniPath, String Prop, String key)
    {
        String Result = "";
        TTextRec F;
        String InputLine = "";
        bool keyFound = false;
        AssignFile(F, IniPath);
        Reset(F);
        IOResultToException();
        while (!Eof(F))
        {
            ReadLn(F, InputLine);
            if (InputLine.size() > 0)
            {
                int Spos = InputLine.find("[" + key + "]");
                if (Spos == 0)
                {
                    keyFound = true;
                }
                else
                {
                    Spos = InputLine.find("[");
                    if (Spos == 0)
                        keyFound = false; // means we are in a different key
                    else
                    {
                        // it is probably content, check if the property is here, if so and the key was previously found, then we are done
                        Spos = InputLine.find(Prop);
                        if ((Spos == 0) && keyFound)
                        {
                            // Here it is, take the value and break the cycle
                            Spos = InputLine.find("=");
                            Result = InputLine.substr(Spos + 1, InputLine.size() - 1);
                            break;
                        }
                    }
                }
            }
        }
        CloseFile(F);

        return Result;
    }
#endif

    // Validates if the given DSS Add-on is locally installed
    bool CheckOpenDSSAddOn(int App_folder)
    {
        String FileName = "",
               iniFile  = "";
        bool Result = false;
        // First, get the path to the Home folder
        // In Windows = AppData
        // In Linux = TBC
#ifdef windows
        PWSTR appdata = NULL;
        if (SHGetKnownFolderPath(FOLDERID_RoamingAppData, KF_FLAG_CREATE, NULL, &appdata) == S_OK)
        {
            // This means the folder exists, go for it
            char dest[MAX_PATH];
            wcstombs(dest, appdata, MAX_PATH);
            wstringstream myFile = {};
            switch (App_folder)
            {
                case 1:
                {
                    myFile << appdata << L"\\opendss_viewer\\settings.ini";
                    FileName = WString2Str(myFile.str());
                }
                break;
                case 2:
                {
                    myFile << appdata << L"\\opendss_gis\\settings.ini";
                    FileName = WString2Str(myFile.str());
                }
                break;
                default:
                {
                    myFile << appdata << L"\\dsspyserver\\settings.ini";
                    FileName = WString2Str(myFile.str());
                }
            }
            if (FileExists(FileName))
            {
                iniFile = GetIni(myFile.str(), "path", "Application");
                if (!iniFile.empty())
                {
                    TReplaceFlags yFlg;
                    yFlg.Include(rfReplaceAll);
                    iniFile = StringReplace(iniFile, "\\\\", "\\", yFlg);
                    iniFile = StringReplace(iniFile, "\"", "", yFlg);
                    iniFile = trim(iniFile);

                    Result = FileExists(iniFile);
                    if (Result)
                    {
                        switch (App_folder)
                        {
                            case 1:
                                DSS_Viz_path = iniFile;
                                break;
                            case 2:
                                DSS_GIS_path = iniFile;
                                break;
                            default:
                                DSSpyServerPath = iniFile;
                        }
                    }
                }
            }
        }
        // if not found, something went wrong
#elif __linux__
        struct passwd* pw = getpwuid(getuid());
        const char* homedir = pw->pw_dir;
        string myFile = "";
        
        switch (App_folder)
        {
            case 1:
            { 
                FileName = homedir;
                FileName = FileName  + "/opendss_viewer/settings.ini";
            }
            break;
            case 2:
            {
                FileName = homedir;
                FileName = FileName + "/opendss_gis/settings.ini";
            }
            break;
            default:
            {
                FileName = homedir;
                FileName = FileName + "/dsspyserver/settings.ini";
            }
        }
            
        if (FileExists(FileName))
        {
            iniFile = GetIni(FileName, "path", "Application");
            if (!iniFile.empty())
            {
                TReplaceFlags yFlg;
                yFlg.Include(rfReplaceAll);
                iniFile = StringReplace(iniFile, "//", "/", yFlg);
                iniFile = StringReplace(iniFile, "\"", "", yFlg);
                iniFile = trim(iniFile);
                Result = FileExists(iniFile);
                if (Result)
                {
                    switch (App_folder)
                    {
                        case 1:
                            DSS_Viz_path = iniFile;
                            break;
                        case 2:
                            DSS_GIS_path = iniFile;
                            break;
                        default:
                            DSSpyServerPath = iniFile;
                    }
                }
            }
        }
#endif
        return Result;
    }

    //***********************************************************************************
    // Launches the DSSpyServer for allowing users to insert Python developed models
    // into the simulation loop.
    // For this purpose opens a named pipe to facilitate the interprocess communication.
    void Launch_pyServer(bool DbugServer)
    {
        if (CheckOpenDSSAddOn(DSSPYSERVER))
        {
            int DBugMode = 0;
            string pyScript = DSSpyServerPath;
#ifdef windows
            // Windows Named pipes

            LPipeName[ActiveActor] = "\\\\.\\pipe\\pyServer_" + to_string(ActiveActor);
            pyServer[ActiveActor] = CreateNamedPipe(
                LPipeName[ActiveActor].c_str(), // Pipe name
                PIPE_ACCESS_DUPLEX, // Read/write access
                PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, // Message-type pipe; message read mode OR blocking mode //PIPE_NOWAIT
                PIPE_UNLIMITED_INSTANCES, // Unlimited instances
                20000, // Output buffer size
                20000, // Input buffer size
                0, // Client time-out 50 [ms] default
                NULL); // Default security attributes

            String pyExec = pyPath + "\\python.exe";
            String pyargs = pyScript + " " + LPipeName[ActiveActor];
            // This to make visible/invisible the server interface (this will help users debugging their code)
            if (DbugServer)
                DBugMode = SW_NORMAL;
            else
                DBugMode = SW_HIDE;

            SHELLEXECUTEINFO sei = { sizeof(sei) };
            sei.lpVerb = "open";
            sei.lpFile = pyExec.c_str();
            sei.lpParameters = pyargs.c_str();
            sei.nShow = DBugMode;

            if (ShellExecuteEx(&sei))
            {
                Sleep(500);

                // Check if new client is connected
                if (!ConnectNamedPipe(pyServer[ActiveActor], NULL) && (GetLastError() == ERROR_PIPE_CONNECTED))
                {
                    GlobalResult = Read_From_PyServer(ActiveActor);
                }
            }
            else
            {
                GlobalResult = "There was an error connecting to the DSSpyServer.";
            }
#elif __linux__
            // Linux pipes

            pyServer[ActiveActor] = "pyServer_" + to_string(ActiveActor);

            bool PipRdy = true;
            int FFo = mkfifo(pyServer[ActiveActor].c_str(), 0666);
            if (FFo == -1)
            {
                if (errno != EEXIST)
                {
                    perror("mkfifo");
                }
            }
                
            if (PipRdy)
            {
                bool XtermRdy = false;
                int QryCmd = system("xterm -v 2>&1 >/dev/null");
                if (QryCmd == 0)
                    XtermRdy = true;

                pid_t p = fork();
                if (p == 0)
                {
                    if (DbugServer && XtermRdy)
                    {
                        string args = "xterm -e python3 " + pyScript + " " + pyServer[ActiveActor];
                        int Result = system(args.c_str());
                        if (Result != 0)
                        {
                            DoSimpleMsg("There was an error while launching the DSSpyServer", 7017);
                        }
                    }
                    else
                    {
                        int Result = execl("/usr/bin/python3", "python3", pyScript.c_str(), pyServer[ActiveActor].c_str(), NULL);
                    }
                }
                GlobalResult = Read_From_PyServer(ActiveActor);
                if (!NoFormsAllowed)
                    cout << GlobalResult + "\n";
            }

#endif
        }
        else
        {
            GlobalResult            = "The pyServer does not exists in this version of OpenDSS";
#ifdef windows
            pyServer[ActiveActor] = NULL;
#elif __linux__
            pyServer[ActiveActor] = "";
#endif
        }
    }


    static string remove_duplicate_directory_separators(const string& in)
    { // This function is static, because it is not called from any other compilation unit.
        string out = in;
        String doubleslash = DIRSEP_STR DIRSEP_STR;
        size_t ndx = out.find(doubleslash);
        while (ndx != string::npos) {
            out.replace(ndx, doubleslash.length(), DIRSEP_STR);
            ndx = out.find(doubleslash);
        }
        return out;
    }


    void Delay(int TickTime)
    {
        int Past = 0;
        Past = (int) GetTickCount64();
        do
        {
        } while (!((GetTickCount64() - Past) >= ((int)TickTime)));
    }

    //*********Downloads a file from the internet into the folder specified*********


    bool DownLoadInternetFile(String Source, String Dest)
    {
        bool result = false;
        try
        {
//            result = URLDownloadToFile(NULL, Source.c_str(), Dest.c_str(), 0, NULL) == 0;
        }
        catch (...)
        {
            result = false;
        }
        return result;
    }

    //******Verifies the OpenDSS version using the reference at Sourceforge*********



#ifdef windows
    void Check_DSS_WebVersion()
    {
        String myVersion, myText, myWebSrc, myPath;
        TTextRec myFile;
        int myIdx = 0;
        char fpath[MAX_PATH + 20];

        GetTempPath(sizeof(fpath), fpath);
        std::string s;
        std::stringstream ss;

        ss << fpath;
        ss >> s;

        myPath = s + (string)DIRSEP_STR "myDSSVersion.txt";
        myWebSrc = "https://sourceforge.net/p/electricdss/code/HEAD/tree/trunk/Version8/Source/Current_ver.txt";
        // Download the file into the Windows temporary folder
        if (DownLoadInternetFile(myWebSrc, myPath))
        {
            AssignFile(myFile, myPath);
            Reset(myFile);
            IOResultToException();
            while (!Eof(myFile))
            {
                ReadLn(myFile, myText);
                myIdx = Pos("mydssversion=", LowerCase(myText));
                if (myIdx > 0)
                    break;
            }
            CloseFile(myFile);
        }
        myText = myText.substr(myIdx + 12);
        myVersion = VersionString.substr(8);
        myIdx = (int) myVersion.find(" ");
        myVersion = myVersion.substr(0, myIdx - 1);
        if (myText != myVersion)
        {
            myPath = "There is a new version of OpenDSS avaialable for download" + CRLF + "The new version can be located at:" + CRLF + CRLF + "https://sourceforge.net/projects/electricdss/";
            DoSimpleMsg(myPath, 0);
        }
    }
#endif

    //**********************Launches the COM Help file******************************



    void Show_COM_Help()
    {
        string url = "https://opendss.epri.com";
        #ifdef windows
            ShellExecuteA(0, "open", url.c_str(), 0, 0, SW_SHOWNORMAL);
        #elif __linux__
        std::string command = "xdg-open " + url + " > /dev/null 2>&1";
            int hnd = system(command.c_str());
        #endif
    }

    //*********************Gets the processor information***************************


    void Get_Processor_Info()
    {
        int idx = 0;
        NumNUMA = 1;

        #ifdef _WIN32
            SYSTEM_INFO sysinfo;
            GetSystemInfo(&sysinfo);

            CPU_Physical = std::thread::hardware_concurrency();
            CPU_Cores = sysinfo.dwNumberOfProcessors; // for now is the same
        #elif _WIN64
            SYSTEM_INFO sysinfo;
            GetSystemInfo(&sysinfo);

            CPU_Physical = std::thread::hardware_concurrency();
            CPU_Cores = sysinfo.dwNumberOfProcessors; // for now is the same
        #elif __linux__
            CPU_Physical = std::thread::hardware_concurrency();
            CPU_Cores = sysconf(_SC_NPROCESSORS_ONLN);
        #endif
        if(CPU_Cores > max_CPU_Cores) {
            std::cerr << "CPU_Cores exceeds max_CPU_Cores.  Please increase max_CPU_Cores to " << CPU_Cores;
            // limit CPU_Cores because size of ActorStatus is fixed by`max_CPU_Cores
            CPU_Cores = max_CPU_Cores;
        }
    }


    TProgressActor::TProgressActor()
    {
        int J = 0;    // Does nothing in this version
      /*  ShellExecute(NULL, "open", DSSProgressPath.c_str(), NULL, NULL, SW_SHOWNORMAL);
        sleep(200);
        // ... create TIdTCPClient
        IdTCPClient = TIdTCPClient.Create( );
        // ... set properties
        IdTCPClient.Host = "localhost";
        IdTCPClient.Port = DSSPrgPort;
        IdThreadComponent = TIdThreadComponent.Create( );
        if ( ADiakoptics && ( ActiveActor == 1 ) )
          J = 1;
        else
          J = NumOfActors;
        try
        {
          IdTCPClient.Connect;
          IdTCPClient.IOHandler.Writeln( "num" + inttostr( J ) );
          IsProgressON = true;
        }
        catch( Exception & E )
        {
        {
          IsProgressON = false;
          throw;
        }
        }
        */
    }

    void TProgressActor::Execute()
    {
        int I = 0, J = 0;
        String AbortBtn, progStr;
        bool RunFlag = false;
        /*                       // Does nothing for now
        if ( IsProgressON )
        {
          RunFlag = true;
          while ( RunFlag )
          {
            Sleep( 100 );
            progStr = "";
            RunFlag = false;
            if ( ADiakoptics && ( ActiveActor == 1 ) )
              J = 1;
            else
              J = NumOfActors;
            for ( int stop = J, I = 1; I <= stop; I++)
            {
              progStr = progStr + Format( "%.*d",  3, ActorPctProgress[I] ) );
              RunFlag = RunFlag || ( ActorStatus[I] == 0 );
            }
            IdTCPClient.IOHandler.Writeln( String( "prg" ) + progStr );
            AbortBtn = IdTCPClient.IOHandler.ReadLn;
            if ( AbortBtn.substr( 0, 1 ) == "T")
              SolutionAbort = true;
          }
          IdTCPClient.IOHandler.Writeln( "ext" );
        }
        */
    }

    void TProgressActor::Doterminate()        // Is the end of the thread

    {
        IsProgressON = false;
        //inherited::Doterminate();
    }


    TProgressActor::~TProgressActor()
    {
        // todo check:  inherited::Destroy;
    }


    void GetDefaultPorts()
    {
        /* TTextRec f;

      TdJSON JSONCfg;
      String JSONStr, iniFilePath;
      iniFilePath = DSSDirectory + "ComPorts.ini";
      if ( FileExists( iniFilePath ) )
      {
        AssignFile( f, iniFilePath );
        Reset( f );
        IOResultToException();
        ReadLn( f, JSONStr );
        CloseFile( f );
        // parse the JSON string and extract the values
        JSONCfg = TdJSON.Parse( JSONStr );
        DSSPrgPort = JSONCfg["dssprogress"].AsInteger;
        DSSGISPort = JSONCfg["dssgis"].AsInteger;
      }
      else
      { */                                     // Since the Cfg file is missing, use the defaults
        DSSPrgPort = 20010;                    // uses defaults for now
        DSSGISPort = 20011;
        //  }
    }

    //***************Initialization for Parallel Processing*************************

    void DSSGlobals_initialization()
    {
      Get_Processor_Info();
      ActiveCircuit.resize(CPU_Cores + 1);
      ActorCPU.resize(CPU_Cores + 1);
      ActorProgressCount.resize(CPU_Cores + 1);
      pyServer.resize(CPU_Cores + 1);
      ActiveDSSClass.resize(CPU_Cores + 1);
      DataDirectory.resize(CPU_Cores + 1);
      OutputDirectory.resize(CPU_Cores + 1);
      CircuitName_.resize(CPU_Cores + 1);
      ActorPctProgress.resize(CPU_Cores + 1);
      ActiveDSSObject.resize(CPU_Cores + 1);
      LastClassReferenced.resize(CPU_Cores + 1);
      DSSObjs.resize(CPU_Cores + 1);
      ActiveEarthModel.resize(CPU_Cores + 1);
      ClassNames.resize(CPU_Cores + 1);
      DSSClassList.resize(CPU_Cores + 1);
      MonitorClass.resize(CPU_Cores + 1);
      LoadShapeClass.resize(CPU_Cores + 1);
      TShapeClass.resize(CPU_Cores + 1);
      PriceShapeClass.resize(CPU_Cores + 1);
      XYCurveClass.resize(CPU_Cores + 1);
      GrowthShapeClass.resize(CPU_Cores + 1);
      SpectrumClass.resize(CPU_Cores + 1);
      SolutionClass.resize(CPU_Cores + 1);
      EnergyMeterClass.resize(CPU_Cores + 1);
      SensorClass.resize(CPU_Cores + 1);
      TCC_CurveClass.resize(CPU_Cores + 1);
      WireDataClass.resize(CPU_Cores + 1);
      CNDataClass.resize(CPU_Cores + 1);
      TSDataClass.resize(CPU_Cores + 1);
      LineSpacingClass.resize(CPU_Cores + 1);
      StorageClass.resize(CPU_Cores + 1);
      PVSystemClass.resize(CPU_Cores + 1);
      WindGenClass.resize(CPU_Cores + 1);
      ReactorClass.resize(CPU_Cores + 1);
      InvControlClass.resize(CPU_Cores + 1);
      ExpControlClass.resize(CPU_Cores + 1);
      EventStrings.resize(CPU_Cores + 1);
      SavedFileList.resize(CPU_Cores + 1);
      ErrorStrings.resize(CPU_Cores + 1);
      ActorHandle.resize(CPU_Cores + 1);
      Parser.resize(CPU_Cores + 1);
      AuxParser.resize(CPU_Cores + 1);
      ActiveYPrim.resize(CPU_Cores + 1);
      SolutionWasAttempted.resize(CPU_Cores + 1);
      TDynamicExpClass.resize(CPU_Cores + 1);
      LPipeName.resize(CPU_Cores + 1);

      // ActorStatus was changed from vector<int> to vector<atomic<int>> for
      // memory-ordering guarantees when threads are signaling between each
      // other.  So ActorStatus no longer supports .resize() since atomic<int>
      // is not copyable or movable.
      //
      // ActorStatus.resize(CPU_Cores + 1);
      if(ActorStatus.size() < CPU_Cores+1)
        throw std::runtime_error("CPU_Cores+1 exceeds ActorStatus.size()");

      ActorMA_Msg.resize(CPU_Cores + 1);
      ActiveVSource.resize(CPU_Cores + 1);
      FMonitorClass.resize(CPU_Cores + 1);    // by Dahei UCF
       // Init pointer repositories for the EnergyMeter in multiple cores
      OV_MHandle.resize(CPU_Cores + 1);
      VR_MHandle.resize(CPU_Cores + 1);
      SDI_MHandle.resize(CPU_Cores + 1);
      TDI_MHandle.resize(CPU_Cores + 1);
      SM_MHandle.resize(CPU_Cores + 1);
      EMT_MHandle.resize(CPU_Cores + 1);
      FM_MHandle.resize(CPU_Cores + 1);
      OV_Append.resize(CPU_Cores + 1);
      VR_Append.resize(CPU_Cores + 1);
      DI_Append.resize(CPU_Cores + 1);
      SDI_Append.resize(CPU_Cores + 1);
      TDI_Append.resize(CPU_Cores + 1);
      SM_Append.resize(CPU_Cores + 1);
      EMT_Append.resize(CPU_Cores + 1);
      PHV_Append.resize(CPU_Cores + 1);
      FM_Append.resize(CPU_Cores + 1);
      DIFilesAreOpen.resize(CPU_Cores + 1);
      DSSExecutive.resize(CPU_Cores + 1);
      IsourceClass.resize(CPU_Cores + 1);
      VSourceClass.resize(CPU_Cores + 1);
      pyControlClass.resize(CPU_Cores + 1);
      for ( int stop = CPU_Cores, ActiveActor = 1; ActiveActor <= stop; ActiveActor++)
      {
        ActiveCircuit[ActiveActor] = nullptr;
        ActiveDSSClass[ActiveActor] = NULL;
        EventStrings[ActiveActor].clear();
        SavedFileList[ActiveActor].clear();
        ErrorStrings[ActiveActor].clear();
        ActorHandle[ActiveActor] = NULL;
        Parser[ActiveActor] = NULL;
        ActorStatus[ActiveActor] = 1;
        OV_MHandle[ActiveActor] = NULL;
        VR_MHandle[ActiveActor] = NULL;
        SDI_MHandle[ActiveActor] = NULL;
        TDI_MHandle[ActiveActor] = NULL;
        SM_MHandle[ActiveActor] = NULL;
        EMT_MHandle[ActiveActor] = NULL;
        FM_MHandle[ActiveActor] = NULL;
        DIFilesAreOpen[ActiveActor] = false;
        ActiveVSource[ActiveActor] = NULL;
#ifdef windows
        pyServer[ActiveActor] = NULL;
#elif __linux__
        pyServer[ActiveActor] = "";
#endif
        pyControlClass[ActiveActor] = NULL;
        // DSSObjs[ActiveActor] = NULL;
        // DSSClassList[ActiveActor] = NULL;
        LPipeName[ActiveActor] = "";
      }

      AutoDisplayShowReport = true;
      DefaultBaseFreq = 60;
      GISThickness = "3";
      GISColor = "FF0000";
      GISCoords = new double[ 4 ];
      UseUserLinks = false;
      IsProgressON = false;
      //Progress_Actor = NULL;
      DSSClasses = NULL;
      ProgressCmd = false;
      AllActors = false;
      ActiveActor = 1;
      NumOfActors = 1;
      ActorCPU[ActiveActor] = - 1;
      Parser[ActiveActor] = new TParser();
      ProgramName = "OpenDSS";
      DSSFileName = GetDSSExeFile();
      DSSDirectory = ExtractFilePath( DSSFileName );
      ADiakoptics = false;  // Disabled by default
      ADiak_Init = false;
      GetDefaultPorts( );                 // Gets the default ports to get connected to other add-ons
      SeasonalRating = false;
      SeasonSignal = "";

      DSSpyServerPath = "";
      DSS_Viz_path = "";
      DSS_GIS_path = "";

       /*Various Constants and Switches*/
      CALPHA = cmplx( - 0.5, - 0.866025 ); // -120 degrees phase shift
      SQRT2 = sqrt( 2.0 );
      SQRT3 = sqrt( 3.0 );
      InvSQRT3 = 1.0 / SQRT3;
      InvSQRT3x1000 = InvSQRT3 * 1000.0;
      CmdResult = 0;
       //DIFilesAreOpen        := FALSE;
      ErrorNumber = 0;
      ErrorPending = false;
      GlobalHelpString = "";
      GlobalPropertyValue = "";
      LastResultFile = "";
      In_Redirect = false;
      InShowResults = false;
      IsDLL = false;
      LastCommandWasCompile = false;
      LastErrorMessage = "";
      MaxCircuits = 1;  //  Not required anymore. planning to remove it
      MaxAllocationIterations = 2;
      SolutionAbort = false;
      AutoShowExport = false;

      SolutionWasAttempted[ActiveActor] = false;
      DefaultBaseFreq = 60.0;
      DaisySize = 1.0;
      DefaultEarthModel = DERI;
      ActiveEarthModel[ActiveActor] = DefaultEarthModel;
      Parallel_enabled = false;
      ConcatenateReports = false;
  
      ProgramName = "OpenDSS";
      DSSFileName = GetDSSExeFile();
      DSSDirectory = ExtractFilePath( DSSFileName );
       // want to know if this was built for 64-bit, not whether running on 64 bits
       // (i.e. we could have a 32-bit build running on 64 bits; not interested in that
      VersionString = "Version " + GetDSSVersion() + " (" + std::to_string(sizeof(void*)*8u) + "-bit build)";
      pyPath = "";

      StartupDirectory = GetCurrentDir() + DIRSEP_STR;
      SetDataPath( GetDefaultDataDirectory() + DIRSEP_STR + ProgramName + DIRSEP_STR );
      //DSS_Registry = TIniRegSave.Create( DataDirectory[ActiveActor] + "opendsscmd.ini" );
      AuxParser[ActiveActor] = new TParser();

#ifdef windows
      DefaultEditor = "NotePad.exe";
#else
      DefaultEditor = FindDefaultEditor();
#endif
      DefaultFontSize = 8;
      DefaultFontName = "MS Sans Serif";
      LogQueries = false;
      QueryLogFileName = "";
      UpdateRegistry = true;
#ifdef windows
      QueryPerformanceFrequency( ((LARGE_INTEGER*) &CPU_Freq) );
#endif

      string myList[12] = { "OH", "UG", "UG_TS", "UG_CN", "SWT_LDBRK", "SWT_FUSE", "SWT_SECT", "SWT_REC", "SWT_DISC", "SWT_BRK", "SWT_ELBOW", "BUSBAR"};
      LineTypeList = TCommandList(myList, 12);
      LineTypeList.set_AbbrevAllowed(true);  // Allow abbreviations for line type code

      myStrArray.resize(0);
      myDblArray.resize(0);
      myCmplxArray.resize(0);
      myPolarArray.resize(0);
      myIntArray.resize(0);
      FPropIndex = 0;

      Yhandle = nullptr;
      NumNZ = 0;
      NumBuses = 0;
      YColumns = nullptr;
      YRows = nullptr;
      YValues = nullptr;


      //IsMultithread = true;
      DSS_Viz_installed = CheckOpenDSSAddOn(OPENDSS_VIEWER);    // OpenDSS Viewer (flag for detected installation)
      DSS_GIS_installed = CheckOpenDSSAddOn(OPENDSS_GIS);       // OpenDSS GIS (flag for detected installation)
#ifdef windows
      if ( ! IsDLL )
      {
//        Check_DSS_WebVersion();  // to be checked
      } 
#endif

    }

    string GetLineTypes()
    {
        // Returns a string containing the line types
        // the string format is the standard DSS array format (comma separated)
        int idx  = 0;
        string  separator = "",
                Result = "";
        
        Result = "[";
        for (idx = 1; idx <= LineTypeList.Get_NumCommands(); idx++)
        {
            Result = Result + separator + LineTypeList.Get(idx);
            separator = ", ";
        }
        Result = Result + "]";

        return Result;
    }

    void DSSGlobals_finalization()
    {
        ClearAllCircuits();
        for (int stop = NumOfActors, ActiveActor = 1; ActiveActor <= stop; ActiveActor++)
        {
            /*# with DSSExecutive[ActiveActor] do */
            {
                auto with0 = DSSExecutive[ActiveActor];
                if (with0 != NULL && with0->get_FRecorderOn())
                    with0->Set_RecorderOn(false);
            }
            delete DSSExecutive[ActiveActor];  /*Writes to Registry*/
            /*TODO: Close Registry? */ 
            EventStrings[ActiveActor].clear();
            SavedFileList[ActiveActor].clear();
            ErrorStrings[ActiveActor].clear();
            if (ActorHandle[ActiveActor] != NULL)
            {
                delete ActorHandle[ActiveActor];
                ActorHandle[ActiveActor] = NULL;
            }
            delete AuxParser[ActiveActor];//AuxParser[ActiveActor]->~TParser();
            if (Parser[ActiveActor] != NULL){
                delete Parser[ActiveActor];
                Parser[ActiveActor] = NULL;
            }
#ifdef __linux__
            if (!pyServer[ActiveActor].empty())
            {
                unlink(pyServer[ActiveActor].c_str());
                pyServer[ActiveActor] = "";
            }

#endif
        }
    }

    class 		DSSGlobals_unit
    {
    public:
        DSSGlobals_unit()
        {
            //AssertSystemInitialization();
            DSSGlobals_initialization();
        }
        ~DSSGlobals_unit() { DSSGlobals_finalization(); }
    };
    DSSGlobals_unit _DSSGlobals_unit;

}









