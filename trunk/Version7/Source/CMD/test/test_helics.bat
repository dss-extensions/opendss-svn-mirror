start /b cmd /c helics_broker -f 3 --name=mainbroker ^>helicsbroker.log 2^>^&1
start /b cmd /c helics_player --input=opendsshelics.player --local --time_units=ns --stop 21600s ^>helicsplayer.log 2^>^&1
start /b cmd /c helics_recorder --input=recorder.json --period 1s --stop 21600s ^>helicsrecorder.log 2^>^&1
set HELICS_CONFIG_FILE=opendss.json
start /b cmd /c opendsscmd -l 6h ^>helicsopendss.log 2^>^&1

