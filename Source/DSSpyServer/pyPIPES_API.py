# -*- coding: utf-8 -*-
"""
Created on Tue Nov 26 14:39:40 2024

@author: Davis Montenegro
This library enables python to communicate with OpenDSS using NamedPipes. In its current
for it works only for windows, but it is expected to be updated to work with Linux
and MacOS
"""

import win32file, pywintypes


handle = None
DSSReply = ''

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
    
    try:
        handle = win32file.CreateFile(
            pipe_path,
            win32file.GENERIC_READ | win32file.GENERIC_WRITE,
            0,
            None,
            win32file.OPEN_EXISTING,
            0,
            None
        )
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
    
    try:
        some_data = Command.encode(encoding="utf-16")
        win32file.WriteFile(handle, some_data)

    except pywintypes.error as e:
        print(e.args[2])
        
def ReadFromPIPE():
    global handle
    global DSSReply
    
    try:
        resp = win32file.ReadFile(handle, 64*2048)
        return (resp[1].decode(encoding="utf-16"))

    except pywintypes.error as e:
        pass
        print(e.args[2]) 
        return ''
        
def CloseConn():
        DSSMsg = 'closepipe'
        some_data = DSSMsg.encode(encoding="utf-16")
        win32file.WriteFile(handle, some_data)


   
