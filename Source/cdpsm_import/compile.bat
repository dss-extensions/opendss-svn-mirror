@echo off
call make_jena_classpath.bat c:\jena-2.6.2
@echo on

rem javac -classpath .;%JENA_CP% CDPSM_Triplets.java
javac -classpath .;%JENA_CP% CDPSM_to_DSS.java
