(exec fncs_broker 3 &> broker.log &)
(exec fncs_player 25h opendss.playerout &> player.log &)
(export FNCS_CONFIG_FILE=tracerout.yaml && exec fncs_tracer 25h tracerout.out &> tracer.log &)
(export FNCS_CONFIG_FILE=opendss.yaml && exec ./opendsscmd -f 25h &)
