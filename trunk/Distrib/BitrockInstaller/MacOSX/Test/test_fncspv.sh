(exec fncs_broker 3 &> brokerpv.log &)
(exec fncs_player 25h opendss.playerpv &> playerpv.log &)
(export FNCS_CONFIG_FILE=tracer.yaml && exec fncs_tracer 25h tracerpv.out &> tracerpv.log &)
(export FNCS_CONFIG_FILE=opendss.yaml && exec ./opendsscmd -f 25h &> opendsspv.log &)
