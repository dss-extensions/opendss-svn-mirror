@echo off

call make_jena_classpath.bat c:\jena-2.6.2

java -classpath .;%JENA_CP% CDPSM_to_DSS -c ieee13connectb.xml  connect
rem java -classpath .;%JENA_CP% CDPSM_to_DSS -b ieee13balanceda.xml balanced
rem java -classpath .;%JENA_CP% CDPSM_to_DSS -u ieee13g.xml         unbalanced
