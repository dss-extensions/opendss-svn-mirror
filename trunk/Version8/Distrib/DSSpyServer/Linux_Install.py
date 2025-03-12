# -*- coding: utf-8 -*-
"""
Created on Fri Feb 21 10:14:28 2025

@author: Davis Montenegro
This script registers the DSSpyServer for Linux OS systems. To execute it:
    Open a new terminal window and type - python3 Linux_Install.py
    
"""

import os

# Method 1: Using os.path.expanduser()
home_dir_os = os.path.expanduser("~")
try:
    CRLF = "\n"
    print('Copyright (c) 2008-2025, Electric Power Research Institute, Inc.'+ CRLF +
         'All rights reserved.'+ CRLF +
         ''+ CRLF +
         'Redistribution and use in source and binary forms, with or without'+ CRLF +
         'modification, are permitted provided that the following conditions are met:'+ CRLF +
         '    * Redistributions of source code must retain the above copyright'+ CRLF +
         '      notice, this list of conditions and the following disclaimer.'+ CRLF +
         '    * Redistributions in binary form must reproduce the above copyright'+ CRLF +
         '      notice, this list of conditions and the following disclaimer in the'+ CRLF +
         '      documentation and/or other materials provided with the distribution.'+ CRLF +
         '    * Neither the name of the Electric Power Research Institute, Inc., nor'+ CRLF +
         '      the names of its contributors may be used to endorse or promote'+ CRLF +
         '      products derived from this software without specific prior written'+ CRLF +
         '      permission.'+ CRLF +
         ''+ CRLF +
         'THIS SOFTWARE IS PROVIDED BY Electric Power Research Institute, Inc., "AS IS"'+ CRLF +
         'AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE'+ CRLF +
         'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR'+ CRLF +
         'PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Electric Power Research Institute, Inc.,'+ CRLF +
          'OR ANY OTHER ENTITY CONTRIBUTING TO OR INVOLVED IN THE PROVISION OF THE SOFTWARE,'+ CRLF +
          'BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR'+ CRLF +
          'CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF'+ CRLF +
          'SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS'+ CRLF +
          'INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN'+ CRLF +
          'CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)'+ CRLF +
          'ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE'+ CRLF +
          'POSSIBILITY OF SUCH DAMAGE.'+ CRLF + CRLF);
    
    folder_path = home_dir_os + "/dsspyserver"
    isExist = os.path.exists(folder_path)
    if not isExist:
        os.mkdir(folder_path)
        
    current_directory = os.getcwd()  
    f = open(folder_path + "/settings.ini", "w")
    f.write('[Application]\n')
    localpath = ('path ="' + current_directory + '/Server.py"').replace('/','//')
    f.write(localpath)
    f.close()
    print('DSSpyServer registered succesfully.')
except Exception as e:
    pass
    error_type = type(e).__name__
    print("Erro found when registering DSSpyServer, try manual installation: " + error_type + "-> ", e)

