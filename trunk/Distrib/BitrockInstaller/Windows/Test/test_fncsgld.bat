set FNCS_LOG_LEVEL=
set FNCS_TRACE=
set FNCS_LOG_STDOUT=yes
start /b cmd /c fncs_broker 4 ^>brokergld.log 2^>^&1
start /b cmd /c fncs_player 25h opendss.playergld ^>playergld.log 2^>^&1
set FNCS_CONFIG_FILE=tracergld.yaml
start /b cmd /c fncs_tracer 25h tracergld.out ^>tracergld.log 2^>^&1
set FNCS_CONFIG_FILE=opendssgld.yaml
start /b cmd /c opendsscmd -f 25h ^>opendssgld.log 2^>^&1
set FNCS_CONFIG_FILE=
start /b cmd /c gridlabd -D USE_FNCS Houses.glm ^>gridlabd.log 2^>^&1 




