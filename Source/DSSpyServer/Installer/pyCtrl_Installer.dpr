program pyCtrl_Installer;

{$APPTYPE CONSOLE}

{$R *.res}

{ ----------------------------------------------------------
  Copyright (c) 2008-2025, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
*	Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
*	Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
*	Neither the name of the Electric Power Research Institute, Inc.,
  nor the names of its contributors may be used to endorse or promote products
  derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY Electric Power Research Institute, Inc., "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL Electric Power Research Institute, Inc.,
  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
}


uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  inifiles,
  System.IOUtils;

var
  Valid     : Boolean;
  localdir,
  SvrDir,
  HostDir,
  cmd       : String;
  i         : Integer;
  appINI : TIniFile;




const
  CRLF = sLineBreak; // cross-platform
  DSSpyFiles : Array[0..1] of String = ('server.py' , 'pyPIPES_API.py');



begin
  try
    { TODO -oUser -cConsole Main : Insert code here }

    writeln(     'Copyright (c) 2008-2025, Electric Power Research Institute, Inc.'+ CRLF +
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
           'POSSIBILITY OF SUCH DAMAGE.'+ CRLF);
    writeln(CRLF);
    localdir := GetCurrentDir;
    writeln('This application will register the DSSpyServer at the current location, to proceed hit the ENTER key...');
    readln(cmd);

    // First, check that the files exist
    Valid :=  True;
    for i := 0 to High(DSSpyFiles) do
    Begin
      Valid := Valid and FileExists(DSSpyFiles[i])
    End;

    if valid then
    Begin
      // Create file and put it at the right destination depending on the OS
      HostDir := TPath.GetHomePath+'\DSSpyServer';
      if not TDirectory.Exists(TPath.GetHomePath+'\DSSpyServer') then
        TDirectory.CreateDirectory(HostDir);

      appINI := TIniFile.Create(HostDir +'\settings.ini');
      try
        SvrDir  := '"' + StringReplace(localdir,'\','\\', [rfReplaceAll, rfIgnoreCase]) + '\\Server.py"';
        appINI.WriteString('Application', 'path', SvrDir);
        appINI.UpdateFile;
      finally
        appINI.Free;
      end;
      writeln('DSSpyServer registered succesfully. Press the ENTER key to exit...');
    End
    Else
    Begin
      writeln('One or more files required are not at the current location, process aborted. Press the ENTER key...');
    End;
    readln(cmd);

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
