@echo off

call make_jena_classpath.bat c:\jena-2.6.2

javac -classpath .;%JENA_CP% Tutorial06.java

java -classpath .;%JENA_CP% Tutorial06
