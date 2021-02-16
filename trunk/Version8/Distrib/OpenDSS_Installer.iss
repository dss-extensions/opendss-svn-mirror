; ----------------------------------------------------------
; Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
; All rights reserved.
; ----------------------------------------------------------

; OpenDSS installer for inno 6.X

[Setup]
AppName=OpenDSS
AppVersion=9.2.1.0
WizardStyle=modern
DefaultDirName={autopf}\OpenDSS
DefaultGroupName=OpenDSS
UninstallDisplayIcon={app}\OpenDSS.exe
Compression=lzma2
SolidCompression=yes
OutputDir="C:\OpenDSS\Version8\Installer"
ChangesAssociations=yes
UserInfoPage=yes
PrivilegesRequiredOverridesAllowed=dialog
LicenseFile=C:\OpenDSS\Version8\Distrib\License.txt
ArchitecturesInstallIn64BitMode=x64

[Files]
; 64 bit version
Source: "x64\*"; DestDir: "{app}\x64"; Check: InstallX64
Source: "x86\*"; DestDir: "{app}\x86"; Check: InstallX64
Source: "x64\OpenDSSEngine.dll"; DestDir: "{app}\x64"; Flags: onlyifdoesntexist regserver 64bit; Check: InstallX64
Source: "x86\OpenDSSEngine.dll"; DestDir: "{app}\x86"; Flags: onlyifdoesntexist regserver 32bit; Check: InstallX64

; 32 bit version, first one should be marked 'solidbreak'
Source: "x86\*"; DestDir: "{app}\x64"; Check: InstallOtherArch; Flags: solidbreak; 
Source: "x86\OpenDSSEngine.dll"; DestDir: "{app}\x64"; Check: InstallOtherArch; Flags: onlyifdoesntexist regserver 32bit 

; Common files
Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme
Source: "Doc\*"; DestDir: "{app}\Doc"; Flags: solidbreak ignoreversion recursesubdirs
Source: "Examples\*"; DestDir: "{app}\Examples"; Flags: solidbreak ignoreversion recursesubdirs
Source: "IEEETestCases\*"; DestDir: "{app}\IEEETestCases"; Flags: solidbreak ignoreversion recursesubdirs
Source: "Training\*"; DestDir: "{app}\Training"; Flags: solidbreak ignoreversion recursesubdirs
Source: "EPRITestCircuits\*"; DestDir: "{app}\EPRITestCircuits"; Flags: solidbreak ignoreversion recursesubdirs

[Icons]
Name: "{group}\OpenDSS"; Filename: "{app}\x64\OpenDSS.exe"; WorkingDir: "{app}"
Name: "{group}\User Manual"; Filename: "{app}\Doc\OpenDSSManual.pdf"; WorkingDir: "{app}"



; NOTE: Most apps do not need registry entries to be pre-created. If you
; don't know what the registry is or if you need to use it, then chances are
; you don't need a [Registry] section.

[Code]
function InstallX64: Boolean;
begin
  Result := Is64BitInstallMode and (ProcessorArchitecture = paX64);
end;

function InstallOtherArch: Boolean;
begin
  Result := not InstallX64;
end;


