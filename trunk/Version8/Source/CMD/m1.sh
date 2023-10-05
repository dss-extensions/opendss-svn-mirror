ppca64 @m1opts.cfg -B opendsscmd.lpr
cp ../../../KLUSolve/Lib/libklusolve.dylib test
install_name_tool -add_rpath @loader_path/. test/opendsscmd
