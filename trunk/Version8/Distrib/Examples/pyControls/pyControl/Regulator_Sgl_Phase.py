# -*- coding: utf-8 -*-
"""
Created on Thu Dec  5 10:23:05 2024

@author: Davis Montenegro

Implements a regualtor using the pyControl object strcuture to be used within an
OpenDSS simulation. Controllers in OpenDSS rquire a "Sample" and "Do..." sections as 
implemented in this example. Use this structure as base for implementing your own
controls.
 
"""

import pywintypes
import pyPIPES_API as DSSPipe  # This is the library that allows Python to talk to OpenDSS during the simulation
import sys, time

# Some globals
puV     = 0.0                                                 # Stores the actual Vpu at the bus observed
Target  = 1.0                                                 # This is the target
Band    = 0.02                                                # This is the error band for control
myXfmr = 'reg4c'
DSSQry = '? Transformer.'                                     # To prevent unnecessary copies

DSSText = DSSPipe.DSSText()

'''
Routine for parsing a DSS String into array of strings
'''  
def Str2StrArray(myString):
    myResult = myString.replace('[','').replace(']','').split(',')
    return myResult

    
# Here starts the control 
'''
This routine is for checking if a control action is required and notify OpenDSS about it
'''  
def Sample(mypipename, master):
    global myXfmr
    global DSSQry
    global puV
    global Target
    global Band
    
#    print('Entering sample')
#    print(master)
    if master:
        DSSPipe.Connect(mypipename)
#        print('connected sample')

    Result = 'no'                                             # Indicates if the control action is needed
    DSSText.Command = 'set class=transformer'
    DSSText.Command = 'ClassMembers'                                      # Gets the list of Xfmrs (not needed, just as example)
    Xfmrs = Str2StrArray(DSSText.Result)
    if myXfmr in Xfmrs:
        try:
            # First, get the bus I'm interested in
            DSSText.Command = DSSQry + myXfmr + '.buses'
            myBuses = Str2StrArray(DSSText.Result)
            myBus = myBuses[1].split('.')[0].replace(' ','')            # Stores the bus name
            myBPhase = int(myBuses[1].split('.')[1].replace(' ',''))    # Stores the phase of interest (single phase)   
    
            # Gets some data from the Xfmr for control purposes
            DSSText.Command = DSSQry + myXfmr + '.kVs'                   # Voltage ratings
            myVratStr = Str2StrArray(DSSText.Result)[1]
            myVrat = float(myVratStr) * 1e3                             # voltage rating at the secondary
            
            # Second, get the voltage at this bus
            DSSText.Command = 'set Bus=' + myBus
            DSSText.Command = 'voltages'
            VperPhase = DSSText.Result.replace(' ','').split(',')
            myVolt = float(VperPhase[((myBPhase - 1) * 2)])
            puV = myVolt / myVrat                                       # Here is the actual Vpu
            # Finally, evaluate if is within band and if a control action is needed
            if (puV > (Target + Band)) | (puV < (Target - Band)):
                Result = 'yes'
        except:
            if master:
                DSSPipe.NeedsControlAction(Result)                     # Something happened, cancel the ctrl action, tell DSS
    
        finally:
            if master:
                DSSPipe.NeedsControlAction(Result)                     # Indicates if the control action is needed or not 2 DSS
     
    if master:   
        DSSPipe.CloseConn()

'''
This routine is for implementing the control actions required and send the changes to OpenDSS
'''        
def DoControlAction(mypipename):
    global myXfmr
    global DSSQry
    global puV
    global Target
    global Band
    
    try:
        DSSPipe.Connect(mypipename)
        # First, sample the transformer to see where are we
        Sample(mypipename, False)

        # Get some xfmr features for calculations
        XfmrVals = []
        props = ['MaxTap', 'MinTap','NumTaps', 'Tap']
        DSSText.Command = DSSQry + myXfmr + '.wdg=2'                # Activate the winding of interest
        for myprop in props:
            DSSText.Command = DSSQry + myXfmr + '.' + myprop
            XfmrVals.append(float(DSSText.Result))
        
        # This to determine the tap movement direction (up or down)
        SMult = -1             
        if puV < Target:
            SMult = 1
        
        TapStep = ( ( XfmrVals[0] - XfmrVals[1] ) / XfmrVals[2] ) * SMult

        # Reduce the current tap 1 step and keeps going with the simulation
        DSSText.Command = DSSQry.replace('? ','') + myXfmr + '.' + props[3] + '=' + str(XfmrVals[3] + TapStep)

        DSSPipe.CloseConn()
        
    except pywintypes.error as e:
        print("Error: " + e.args[1]) 

    

if __name__ == '__main__':
    '''
    This section remains unchanged
    It is needed for OpenDSS to understand the global structure of the script
    and for passing arguments
    '''
    if len(sys.argv) < 2:
        print("need an argument")
        exit
    myCall = sys.argv[1][0]
    myPipeName = sys.argv[1][2:]
    
    if myCall == 's':
        Sample(myPipeName, True)
    elif myCall == 'd':
        DoControlAction(myPipeName)
        

