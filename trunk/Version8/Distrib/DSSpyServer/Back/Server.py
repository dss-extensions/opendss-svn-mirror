# -*- coding: utf-8 -*-
"""
Created on Mon Jan  6 15:46:05 2025

Electric Power Research Institute (EPRI)
This code implements a server for handling commands between OpenDSS and user pyObj/Controls

@author: Davis Montenegro
"""

import sys
import pyPIPES_API as DSSPipe
import numpy as np
import traceback
import time


    
def init_server(PipeName):
    IMsg = ''
    try:
        DSSPipe.Connect(PipeName)
        print("here")
        time.sleep(5)
        DSSPipe.Write2PIPE('Connected to DSSpyServer')
        print('Electric Power Research Institute (EPRI) - 2025')
        print('')
        print('Connected to DSSpyServer')
        time.sleep(5)
        DSSText = DSSPipe.DSSText()
        if sys.platform != 'win32':
            time.sleep(0.5)
        while IMsg != PipeName:
            IMsg = DSSPipe.ReadFromPIPE()
            if IMsg != PipeName:
                with open(IMsg) as file:
                    try:
                        LocFile = file.read()
                        exec(LocFile)
                    except Exception as e:
                        pass
                        error_type = type(e).__name__
                        print("Error found in python: " + error_type + "-> ", e)
                        traceback.print_exc()
                    
                DSSPipe.CloseConn()                                            # This MUST be done to let DSS know that we are done here

        # Geting here means the end of the program
        print('Disconnected')
    except Exception as e:
        pass
        print("Error while managing the Pipe: ", e)
        traceback.print_exc()
        DSSPipe.CloseConn()  
        time.sleep(5)
        
if __name__ == "__main__":
    '''
    This section remains unchanged
    It is needed for OpenDSS to understand the global structure of the script
    and for passing arguments
    '''
    if len(sys.argv) < 2:
        print("need an argument")
        exit
    myPipeName = sys.argv[1]
    init_server(myPipeName)


