start /b cmd /c fncs_broker 3 ^>brokerpv.log 2^>^&1
start /b cmd /c fncs_player 25h opendss.playerpv ^>playerpv.log 2^>^&1
set FNCS_CONFIG_FILE=tracer.yaml
start /b cmd /c fncs_tracer 25h tracerpv.out ^>tracerpv.log 2^>^&1
set FNCS_CONFIG_FILE=opendss.yaml
start /b cmd /c opendsscmd -f 25h ^>opendsspv.log 2^>^&1


