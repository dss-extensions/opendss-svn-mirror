(exec helics_broker -f 3 --name=mainbroker --loglevel=7 --debugging &> helicsbrokerpv.log &)
(exec helics_player --input=opendsshelics.playerpv --local --time_units=ns --stop 90000s --loglevel=7 --debugging &> helicsplayerpv.log &)
(exec helics_recorder --input=recorderpv.json --period 0.000000001s --stop 90000s --loglevel=7 --debugging&> helicsrecorderpv.log &)
(export HELICS_CONFIG_FILE=opendss.json && exec ./opendsscmd -l 25h &> helicsopendsspv.log &)

