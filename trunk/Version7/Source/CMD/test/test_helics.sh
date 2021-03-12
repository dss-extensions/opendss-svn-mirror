(exec helics_broker -f 3 --name=mainbroker --loglevel=7 --debugging &> helicsbroker.log &)
(exec helics_player --input=opendsshelics.player --local --time_units=ns --stop 21600s --loglevel=7 --debugging &> helicsplayer.log &)
(exec helics_recorder --input=recorder.json --period 1s --stop 21600s --loglevel=7 --debugging &> helicsrecorder.log &)
(export HELICS_CONFIG_FILE=opendss.json && exec ./opendsscmd -l 6h &> helicsopendss.log &)
