# -*- coding: utf-8 -*-
"""
Created on Tue Nov 26 14:39:40 2024

@author: Davis Montenegro
This library enables python to communicate with OpenDSS using NamedPipes. 
Updated on 02/18/2025 enabling Linux compatibility
"""

import sys, os, time

try:
    import win32file  # for Windows OS
except ImportError:
    pass
    

handle = None
DSSReply = ''
PipeNm = ''  #Used in other OS different than MS Windows

'''
Here we declare the class to make it similar to the COM interface and compatible APIs
'''
class DSSText:
    global handle
    global DSSReply

    @property
    def Command(self):
        return ''
    @Command.setter
    def Command(self, value):
        CommandS(value)
        
    @property
    def Result(self):
        return DSSReply

def Connect(pipe_path):
    global handle
    global PipeNm
    
    try:
       if sys.platform == 'win32':
           # Windows OS
            handle = win32file.CreateFile(
                pipe_path,
                win32file.GENERIC_READ | win32file.GENERIC_WRITE,
                0,
                None,
                win32file.OPEN_EXISTING,
                0,
                None
            )
       else:
            # Linux OS
           PipeNm = pipe_path
    except Exception as e:
        pass
        error_type = type(e).__name__
        print("Error found in pyPIPES - Connect: " + error_type + "-> ", e)

def CommandS(DSSCmd):
    global handle
    global DSSReply
    
    try:
        Write2PIPE(DSSCmd)
        DSSReply = ReadFromPIPE()

    except Exception as e:
        pass
        error_type = type(e).__name__
        print("Error found in pyPIPES - CommandS: " + error_type + "-> ", e)

def NeedsControlAction(DSSMsg):
    global handle
    global DSSReply
    
    try:
        Write2PIPE(DSSMsg)
        DSSReply = ReadFromPIPE()

    except Exception as e:
        pass
        error_type = type(e).__name__
        print("Error found in pyPIPES - NeedsControlAction: " + error_type + "-> ", e)
        
def Write2PIPE(Command):
    global handle
    global DSSReply
    global PipeNm
    
    try:
        if sys.platform == 'win32':
            some_data = Command.encode(encoding="utf-16")
            win32file.WriteFile(handle, some_data)
        else:
            pipe_fd = os.open(PipeNm, os.O_WRONLY)
            os.write(pipe_fd, Command.encode(encoding="utf-16"))
            os.close(pipe_fd)


    except Exception as e:
        pass
        error_type = type(e).__name__
        print("Error found in pyPIPES-Write2PIPE: " + error_type + "-> ", e)
        
def ReadFromPIPE():
    global handle
    global DSSReply
    global PipeNm
    
    try:
        if sys.platform == 'win32':
            resp = win32file.ReadFile(handle, 64*2048)
            return (resp[1].decode(encoding="utf-16"))
        else:
            pipe_rd = os.open(PipeNm, os.O_RDONLY)
            resp = os.read(pipe_rd, 64*2048)
            os.close(pipe_rd)
            return resp.decode(encoding="utf-16")


    except Exception as e:
        pass
        error_type = type(e).__name__
        print("Error found in pyPIPES-ReadFromPIPE: " + error_type + "-> ", e)
        
def CloseConn():
        global PipeNm
    
        DSSMsg = 'closepipe'

        if sys.platform == 'win32':
            some_data = DSSMsg.encode(encoding="utf-16")
            win32file.WriteFile(handle, some_data)
        else:
            pipe_fd = os.open(PipeNm, os.O_WRONLY)
            os.write(pipe_fd, DSSMsg.encode(encoding="utf-16"))
            os.close(pipe_fd)


   
