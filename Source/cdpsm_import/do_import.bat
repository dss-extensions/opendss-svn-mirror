@echo off

rem call with -{c|b|u} instance.xml output_root

call make_jena_classpath.bat c:\jena-2.6.2

java -classpath .;%JENA_CP% CDPSM_to_DSS %1 %2 %3

