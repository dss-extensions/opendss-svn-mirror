set SOURCE=c:\cmdtools\bin
set TARGET=z:\Documents\windows_executable

rd /s /q %TARGET%
md %TARGET%

rem copy %SOURCE%\fncs_broker.exe %TARGET%
rem copy %SOURCE%\fncs_player.exe %TARGET%
rem copy %SOURCE%\fncs_tracer.exe %TARGET%
copy .\test\opendsscmd.exe %SOURCE%
copy .\test\*.dll %SOURCE%
copy %SOURCE%\opendsscmd.exe %TARGET%
copy %SOURCE%\helics_broker.exe %TARGET%
copy %SOURCE%\helics_player.exe %TARGET%
copy %SOURCE%\helics_recorder.exe %TARGET%
copy %SOURCE%\*.dll %TARGET%

dir %TARGET%
