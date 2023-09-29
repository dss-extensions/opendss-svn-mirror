OS="`uname`"
if [[ "$OS" == "Linux" ]]; then
	ppcx64 @linuxopts.cfg -B opendsscmd.lpr
elif [[ "$OS" == "Darwin" ]]; then
	ppcx64 @darwinopts.cfg -B opendsscmd.lpr
        cp ../../../KLUSolve/Lib/libklusolve.dylib test
        install_name_tool -add_rpath @loader_path/. test/opendsscmd
fi
