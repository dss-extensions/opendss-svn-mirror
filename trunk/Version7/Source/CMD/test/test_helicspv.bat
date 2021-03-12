start /b cmd /c helics_broker -f 3 --name=mainbroker ^>helicsbrokerpv.log 2^>^&1
start /b cmd /c helics_player --input=opendsshelics.playerpv --local --time_units=ns --stop 90000s ^>helicsplayerpv.log 2^>^&1
start /b cmd /c helics_recorder --input=recorderpv.json --period 0.000000001s --stop 90000s ^>helicsrecorderpv.log 2^>^&1
set HELICS_CONFIG_FILE=opendss.json
start /b cmd /c opendsscmd -l 25h ^>helicsopendsspv.log 2^>^&1

