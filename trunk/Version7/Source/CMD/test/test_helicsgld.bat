start /b cmd /c helics_broker -f 4 --name=mainbroker --loglevel=7 --debugging ^>helicsbrokergld.log 2^>^&1
start /b cmd /c helics_player --input=gldopendsshelics.playergld --local --time_units=ns --stop 90000s --loglevel=7 --debugging ^>helicsplayergld.log 2^>^&1
start /b cmd /c helics_recorder --input=gldrecorderhelics.json --stop 90000s --loglevel=7 --debugging ^>helicsrecordergld.log 2^>^&1
set HELICS_CONFIG_FILE=gldopendsshouseshelics.json
start /b cmd /c opendsscmd -l 25h ^>helicsopendssgld.log 2^>^&1
set HELICS_CONFIG_FILE=
start /b cmd /c gridlabd HousesHelics.glm ^>helicsgridlabd.log 2^>^&1 

