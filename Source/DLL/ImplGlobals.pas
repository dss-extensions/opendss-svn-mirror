unit ImplGlobals;
// Globals for the implementation of the interface
{ Change Log
 8-14-99 Added DSSProgress
 8-??-00  Added Settings,Lines etc.

}
interface

Uses
       ImplBus,
       ImplCircuit,
       ImplCktElement,
       ImplError,
       ImplDSS,
       ImplSolution,
       ImplText,
       ImplDSSProperty,
       ImplGenerators,
       ImplMonitors,
       ImplMeters,
       ImplDSSProgress,
       ImplSettings,
       ImplLines,
       DSSClass,
       OpenDSSengine_TLB;

Var

{ Vars for Interfaces }
   Ftext        :IText;
   FCircuit     :ICircuit;
   FBus         :IBus;
   FCktElement  :ICktElement;
   FError       :IError;
   FSolution    :ISolution;
   FDSS         :IDSS;
   FDSSProperty :IDSSProperty;
   FGenerators  :IGenerators;
   FMonitors    :IMonitors;
   FMeters      :IMeters;
   FDSSProgress :IDSSProgress;
   FSettings    :ISettings;
   FLines       :ILines;

   FPropIndex   :Integer;
   FPropClass   :TDSSClass;

   FIntfInitialized :Boolean;

Procedure InitializeInterfaces;

{Special Interface Routines for Text Interface w/o COM}
Procedure DSS_PutCommand(S:pchar);StdCall;
Function DSS_GetResult:pchar;Stdcall; // Returns a pointer to global result String


implementation

uses DSSGlobals, Executive;

Procedure InitializeInterfaces;

Begin
   // Create some references to interfaces so we can return them
   // Called from Main DLL

     FBus         := TBus.Create;
     FCircuit     := TCircuit.Create;
     FCktElement  := TCktElement.Create;
     FText        := TText.Create;
     FSolution    := TSolution.Create;
     FDSSProperty := TDSSProperty.Create;
     FError       := TError.Create;

     FGenerators  := TGenerators.Create;
     FMonitors    := TMonitors.Create;
     FMeters      := TMeters.Create;
     FDSSProgress := TDSSProgress.Create;
     FSettings    := TSettings.Create;
     FLines       := TLines.Create;

     FPropIndex := 0;
     FPropClass := Nil;

     {MessageDlg('Interfaces initialized', mtInformation, [mbOK], 0);}
     FIntfInitialized := True;
End;



Procedure DSS_PutCommand(S:pchar);StdCall;

Begin
     DSSExecutive.Command := S;
End;


Function DSS_GetResult:pchar;Stdcall; // Returns a pointer to global result String

Begin
      Result := PChar(GlobalResult);
End;


Initialization

  FIntfInitialized := False;

end.
