@echo off
SetLocal EnableDelayedExpansion
set the_classpath=out\redline-smalltalk.jar;
for %%i in (lib\*.jar) do set the_classpath=!the_classpath!%%i;
java -cp "%the_classpath%" st.redline.stout.Run %*
EndLocal
