unit Notes;
 {
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{Dummy module just for development notes.}
interface

implementation

{
TO DO:

 * = done

   *1. Fix so changing solution modes that requires changing the System Y matrix
      causes the Y matrix to be rebuilt.
   *2. Add capacitor.
   *3. Add Harmonic source injection to the general load model
   4. Add a specific harmonic source.
   *5. Create a monitor mode that takes only magnitudes.
   6. Create a monitor mode that does statistics on the fly.
   7. Test other transformer connections.
   *8. Add parsing of winding quantities as arrays Buses="HBus  LBus Tbus"  etc.
   *9. Fix PCElement GetCurrent routine so that after a Solve Direct, it doesn't use inj currents.
      Add a LastSolutionType variable.
   *10. (Done this in conjunction with 14 - bus lists get rebuilt if changes)
      Make Bus reference a usage count so that if bus.node is deleted from the network,
      it is eliminated from the matrix.  OR, add a small conductance to diagonal of the
      System Y so that it will always solve although there is nothing connected to the node.
  *11. Consolidate the Mode command interpreter in Solution and Exec into one function in Exec?,
  *12. Add enable/disable property to all circuit elements.
  *13.  Code for opening and closing terminal switches?
  *14. Handle reorder of system nodelist if a circuit element is disabled (or any other
      change to the terminal connections.)
  15. Add persistence - would make a great OO circuit database.
  *16. Check power factor designation on load to see if leading (-) will work.
  *16a. Make sure that the power factor is interpreted correctly for negative kW.
  17. Resolve how multiple circuits will be handled.  2nd, 3rd, circuits are created with
      a source with the same name as the first circuit.  Edit command can find only first.
      Also, some circuits might share elements with another circuit.
  *18. help panel showing properties in a device's command list (Create a new form and let it
      float freely).
  *19. Add properties for symmetrical component quantities.
  *20. Change syntax parser to allow default Edit command.
  *21. Add Select to Interface.
  *22. Check consistency of voltage definition between devices
  23. Revise DSS design to have user-written stuff implemented as COM instead of DLL.
  24. Look into Aggregation for ActiveX.
  *25. Add UE-type calculations; Return in interface.
  *26. Add EDC-like generator model that is negative load following load curves or dispatched
  27. Current report prints a bogus number for single-phase loads.
  ?28. The Disable/Enable and Open/Close commands do not differentiate between objects of the same name in different ckts.
  29. Problem with open terminal logic with delta-connected loads.  IF open 2 phases, leaves 1 by 1 which is not correct.
  *30. Monitor is not searching for full element name -- neither is Meter
  *31. Monitor defaults to 3-phases, not the number of phases of the element.
  *32. Set Yprim in Monitor to 1.e-12 on diagonal so it won't cause problem when misapplied
  *33. Check Vsource short circuit data for correct computation.
  *34. Add history window or file from which you can cut and paste commands or select and
      execute commands.
  *35. Monitor not indexing the currents right on transformers if not on the first terminal.
  *36. Add Cancel button to simple error message and abort redirects and compile commands
  37. Repair time should be part of base ckt element.
  38. check the open terminal logic of loads
  39. Save circuit will not save circuit over top of existing one.
  40. Finish Equivalent model
  41. Finish Sensor model for DSE
  42. Connect Loadloss property on transformer to %R property
  43. Implement NoLoadLoss property on transformer to insert a shunt resistance across a winding (1?)

LOG:

7-2-98: Notes unit added. Transformer was completed night before.  Short circuit
        testing done for 3-phase transformer.  Need to test other configurations. Solve
        Direct mode added.  However, it was noted that there was no flag/logic to figure
        out that the system Y needed to be rebuilt
7-4-98: Modified transformer to allow entry of connections and buses as arrays.
7-6-98: Added single quote and parens as legal for enclosing parameters with white space in the parser
7-9-98: Fixed bug where setting nphases for load was resetting the bus name to default value
        Reversed order on Phases and Bus1 in Load.
        Revised way reallocation of the BusNames array is done so that previous changes are preserved
        and default names are only put in on initial allocation.
8-1-98: Added Capacitor, Tested.
        Added Enabled flag to CktElement Class.  No way to turn it off yet.
        Fixed error in load connection interpretation (lower case)
        Fixed up display in ShowResults
        Removed Dynasolve command.  Use 'set mode=dynamic' instead
        Added 'enable' and 'disable' commands to Exec.
        Changed Type=xxxx and Name=zzzz To Object=Class.Name to agree with IEEE PSAD.
8-2-98: Revised processing of bus lists so that bus name can be changed on the fly and
        devices can be enabled and disabled.
        Modified HashList to add a Clear Function, which leaves allocation alone, but
        zeroes out the counters.  Should be fast way to reestablish buslists, etc.
        Enable/disable tested and refined.  BusNameRedefined made property.  Also sets
        the flag to rebuild the SystemY.
        Fixed up Show Voltages to order voltages by bus and node.
        Added Open/Close command.
        Moved check for open conductors to Base CktElement Class and called
        'Inherited CalcYPrim' near the end of the routine. Works.
        Tested Redirect.
        Added facilities to handle blank lines and comments:
          '//'at beginning of lines  '/'  may work as an abbreviation
          Parser also allows ! anywhere on line.
        Changed global systemY matrix flag to SystemYChanged.
8-3-98  Fixed bug where Enable was not always causing SystemY to be rebuilt
8-5-98  Add code to invalidate yprims of PCelements whenever solution is changed
        from DIRECT mode to any of the power flow modes.
        Added Substation classification as an option for Transformer class.
           Set Sub=yes.  Then when we look for substation losses, we can find them
           by checking the IsSubstation value of a transformer.
        Add lists of lines and transformers to the Circuit object so we can find
        them more quickly too.  Both are also kept in PDElement lists.
        Found and fixed error in YEQ in load model - imaginary part had wrong sign.
8-6-98  Built VCL object.
        Added DSSClass string to all elements because there was no way to figure out
        the class of an object if you found it. (for reporting back through interface)
8-7-87  Built ActiveX control.
        DSSXIMpl1.pas.   Delphi converted most every thing but methods with arguments.
        Fixed GetCurrents in monitor because it was default to base class.  Had it report
        current of monitored object terminal. Seems too work.
        Wrote VB test app. Seems to work OK, except had to explicitely add
        chdrive and chdir statements to get program to switch to working directory.
        Discovered why when doing a new circuit twice the solution appeared to be messed up.
          You can't have two objects with the same fully qualified name in the problem set
          or the Edit command will only work on the first.  Changed the TestForm to use the
          More command instead of the Edit for changing the source.
8-12-98
       Added Help command.
       Added Compile as a synonym for Redirect.
       Fixed bug in the bus redefinition that was not handling cases where not all
        terminal-node connections were specified (in Circuit).
       Fixed bug in transformer model where 1-phase wye connected voltage base
        was being misinterpreted.  (Recalc...)
       Added Select Command to Executive
       Added assumed 'edit' capability and parsing of element properties.
       Added a TreeView box for Help that will automatically show all the property names.
       Moved the Editcommand array to the DSSClass base class and dynamically allocated it 
8-22-98
       Added Fault object
       Added Monte Carlo Fault Study.
       Added modes to monitor to save various quantities other then just V&I.
       Changed the way the buffer in Monitor object works.
8-23-98
       Debugged monitor - indexing problem in MonBuffer.  All modes check out.
       Tested MFault algorithm.  Seems to work.  Kind of slow because rebuilds Y
       matrix for each solution.
       fixed bug in solution where the old sparseset was not being thrown away before
       building a new one. This caused Esolve32 to crash after a bit, which was trapped
       nicely by the raise exception in BuildYMatrix.
8-26-09
       Fixed bug where fault random multiplier was inverse of what it should have been.

10-14-98
        Delphi 4 introduced a Tmonitor to the system so I had to change the name of
        the monitor type to TDSSMonitor.
        Added Max kVA ratings to Transformer.
        Added kvas=  array parameter to transformer.
        Changed way transformer voltage ratings are interpreted. Now like capacitor.

10-23-98 to 10-24-98

         Added Property values, PropHelp to objects.
         Added ? command to display a message dlg with the value of a property
         Modified Dump command to dump only properties of an element when there
         is a parameter on the Dump command line.
         Modified Monitor to include the Mode on the header record.

10-26-98
        Started addition of kWHMeter.
        Completed revision of properties.
        Made double-paned Help panel.

10-27-98
        Fixed problem with sign of power factor in load.
        Worked more on energy meter in prep for Poland IRP study.
        
10-28-98
        Continued work on kWHMeter.  Got zone established.
        Changed voltage base in Load to agree with capacitor.
        Got all help notes in.
        Found some issues with multiple circuits.
        Fixed load class/model issues.
10-29-98
        Modified Ucmatrix to give Order property.
        Issue worked at correcting load model.
        Added solution counter to minimize recalc of current for UE search.
11-3/98
       Moved NormAmps and EmergAmps to PDElement.
       Added NormAmps, EmergAmps calculation to the Transformer based on the
       first winding.
12-1-98
       Fixed NormAmps calc in Transformer to properly account for line amps in 1-phase
       L-L transformers.
       Fixed the neutral definition of transformer Wye windings so that it automatically
       gets set to something if neutral Z is specified.

12-2-98 - 12-5-98  Added kWhMeter

12-5-98
       Found power flow algorithm had trouble converging if source was not somewhat grounded.
       Solved just fine in Admittance mode, but not in current injection mode.  Solved just
       fine if a grounding transformer were added.

12-7-09
       Fixed CktTree so that the correct pointers were being saved.
       Added Save option to energy meter. MTR_metername.CSV
       Added Take Action to Monitor to be consistent with meter.

12-21-98 Fixed indexing problem in transformer which resulted in improper YPrim for
       3-winding transformer.
12-22-98 Removed base frequency conversion upon transferring data from Linecode to Line.
       Set Line.Basefrequency = LineCode.BaseFrequency.  thus, frequency compensation
       is automatic in CalcYPrim.

12-30-98
       Fixed bug in 3-winding transformer that caused the impedances to be rotated
       by one winding.

1-4-99  Converted energy meter to trapezoidal integration so we can do load duration solutions.
1-9-99 Fixed offset in Monitor so it would properly index the currents when connected to
       other than the first terminal of transformers etc which do not have numconds=numphases
1-11-99 to 1-13-99 Added generator and control panel.  Miriad of misc fixes. New interface (4).

1-18-99 .. 1-20-99
       Fixed DSSGlobal function to report Mode to interface correctly
       Open command resulted in short circuit by eliminating row and column.
       Fixed by doing a Kron reduction before eliminating row.
       Set ErrorResult on ActiveElement property.  =0 then it doesn't work!
       NormalAmps and EmergAmps were not being brought over from line code

2-23-99
       Added Abort to message dialog when in Redirect or Compile command.
       Moved HelpFormX variable to Globals so we can dispose of the Form when we quit.

6/7/99
       Fixed assignment problem in ImplSolution
       Fixed bug in transformer creation that would sporadically give an overflow.
       Added DefaultGrowthFactor.  (%Growth)
6/8/99
       Found that the Ellipse function in DSSGraph messed up the pen or brush or something.
       Drew nodes after drawing the lines and it worked.
6/15/99
       Added Version to interface and About box to DSS
       Added startup validation and checking.

6/23/99 Fixed bug in which disabled device gets checked by energy meter
       before energy meter is ready to be used.
6/24/99 Added IsOpen function to DSS Interface

4/18/03 Added propertysequence array and then implemented the SAVE command.

4/3/06 Changed Error Handling and Error Number
}

end.
