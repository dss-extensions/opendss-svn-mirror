(exec helics_broker -f 4 --name=mainbroker &> helicsbrokergld_ep.log &)
(exec helics_player --input=gldopendsshelics_endpoint_combo.playergld --local --time_units=ns --stop 90000s &> helicsplayergld_ep.log &)
(exec helics_recorder --input=gldrecorderhelics_combo.json --stop 90000s &> helicsrecordergld_ep.log &)
(export HELICS_CONFIG_FILE=gldopendsshelics_endpoints_combo.json && exec ./opendsscmd -l 25h &> opendssgld_ep.log &)
(exec gridlabd HousesHelics_combo.glm &> helicsgridlabd_ep.log &)
