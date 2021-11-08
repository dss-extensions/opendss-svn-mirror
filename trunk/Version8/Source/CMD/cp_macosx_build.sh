declare -r SOURCE="/usr/local"
declare -r TARGET="/users/mcde601/Documents/macosx_executable"

rm -rf $TARGET
mkdir $TARGET
mkdir $TARGET/bin
mkdir $TARGET/lib
mkdir $TARGET/java

cp $SOURCE/bin/opendsscmd $TARGET/bin
cp $SOURCE/bin/fncs_broker $TARGET/bin
cp $SOURCE/bin/fncs_player $TARGET/bin
cp $SOURCE/bin/fncs_tracer $TARGET/bin
cp $SOURCE/bin/helics_broker $TARGET/bin
cp $SOURCE/bin/helics_player $TARGET/bin
cp $SOURCE/bin/helics_recorder $TARGET/bin

#cp $SOURCE/java/*fncs*.* $TARGET/java
cp -a $SOURCE/java/*helics*.* $TARGET/java

cp -a $SOURCE/lib/*fncs*.* $TARGET/lib
cp -a $SOURCE/lib/*helics*.* $TARGET/lib
cp -a $SOURCE/lib/liblinenoise.dylib $TARGET/lib
cp -a $SOURCE/lib/libklusolve.dylib $TARGET/lib

ls -altR $TARGET
