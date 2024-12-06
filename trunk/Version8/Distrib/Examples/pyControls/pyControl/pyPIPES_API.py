# -*- coding: utf-8 -*-
"""
Created on Tue Nov 26 14:39:40 2024

@author: pdmo005
"""

import win32file, pywintypes, time


handle = None
DSSReply = ''

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

def Command(DSSCmd):
    global handle
    global DSSReply
    
    try:
        some_data = DSSCmd.encode(encoding="utf-16")
        win32file.WriteFile(handle, some_data)
        resp = win32file.ReadFile(handle, 64*1024)
        DSSReply = (resp[1].decode(encoding="utf-16"))

    except pywintypes.error as e:
        print(e.args[2])

        
def Result():
    global DSSReply
    
    return DSSReply

def NeedsControlAction(DSSMsg):
    global handle
    global DSSReply
    
    try:
        some_data = DSSMsg.encode(encoding="utf-16")
        win32file.WriteFile(handle, some_data)
        resp = win32file.ReadFile(handle, 64*1024)
        DSSReply = (resp[1].decode(encoding="utf-16"))

    except pywintypes.error as e:
        print(e.args[2])
        
def CloseConn():
        DSSMsg = 'closepipe'
        some_data = DSSMsg.encode(encoding="utf-16")
        win32file.WriteFile(handle, some_data)


   
