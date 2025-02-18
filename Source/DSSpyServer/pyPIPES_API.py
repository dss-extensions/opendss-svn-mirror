# -*- coding: utf-8 -*-
"""
Created on Tue Nov 26 14:39:40 2024

@author: Davis Montenegro
This library enables python to communicate with OpenDSS using NamedPipes. 
Updated on 02/18/2025 enabling Linux compatibility
"""

import win32file, pywintypes
import sys, os


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
            os.mkfifo(PipeNm)
    except pywintypes.error as e:
        print('Error: ' + e.args[2])

def CommandS(DSSCmd):
    global handle
    global DSSReply
    
    try:
        Write2PIPE(DSSCmd)
        DSSReply = ReadFromPIPE()

    except pywintypes.error as e:
        print(e.args[2])

def NeedsControlAction(DSSMsg):
    global handle
    global DSSReply
    
    try:
        Write2PIPE(DSSMsg)
        DSSReply = ReadFromPIPE()

    except pywintypes.error as e:
        print(e.args[2])
        
def Write2PIPE(Command):
    global handle
    global DSSReply
    global PipeNm
    
    try:
        some_data = Command.encode(encoding="utf-16")
        if sys.platform == 'win32':
            win32file.WriteFile(handle, some_data)
        else:
            with open(PipeNm, "w") as pipe_fd:
                pipe_fd.write(some_data)

    except pywintypes.error as e:
        print(e.args[2])
        
def ReadFromPIPE():
    global handle
    global DSSReply
    global PipeNm
    
    try:
        if sys.platform == 'win32':
            resp = win32file.ReadFile(handle, 64*2048)
        else:
            with open(PipeNm, "r") as pipe_fd:
                resp = pipe_fd.readline()
        return (resp[1].decode(encoding="utf-16"))

    except pywintypes.error as e:
        pass
        print(e.args[2]) 
        return ''
        
def CloseConn():
        global PipeNm
    
        DSSMsg = 'closepipe'
        some_data = DSSMsg.encode(encoding="utf-16")
        if sys.platform == 'win32':
            win32file.WriteFile(handle, some_data)
        else:
            with open(PipeNm, "w") as pipe_fd:
                pipe_fd.write(some_data)

   
