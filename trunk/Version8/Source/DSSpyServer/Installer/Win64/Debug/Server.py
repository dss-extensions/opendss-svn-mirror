# -*- coding: utf-8 -*-
"""
Created on Mon Jan  6 15:46:05 2025

@author: pdmo005
"""

import subprocess, sys
import pyPIPES_API as DSSPipe
import time

def run_script_with_exec(script_path):
    
    data = script_path.split(' ')
    process = subprocess.Popen(['python', data[0], data[1]])

    
def init_server(PipeName):
    DSSPipe.Connect(PipeName)
    DSSPipe.Write2PIPE('Connected to DSSpyServer')
    IMsg = ''
    while IMsg != 'quit':
        IMsg = DSSPipe.ReadFromPIPE()
        if IMsg != 'quit':
            run_script_with_exec(IMsg)    
    # Geting here means the end of the program
    DSSPipe.CloseConn()
    print('Disconnected')
    time.sleep(10)

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


