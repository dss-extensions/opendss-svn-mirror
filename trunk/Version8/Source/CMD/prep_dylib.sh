TARGET="./test"
cp ../../../KLUSolve/Lib/libklusolve.dylib $TARGET
install_name_tool -add_rpath @loader_path/. $TARGET/libopendssdirect.dylib