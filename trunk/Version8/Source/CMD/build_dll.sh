OS="`uname`"
if [[ "$OS" == "Linux" ]]; then
	ppcx64 @linuxdll.cfg -B opendssdirect.lpr
elif [[ "$OS" == "Darwin" ]]; then
	ppcx64 @darwindll.cfg -B opendssdirect.lpr
        install_name_tool -add_rpath @loader_path/. test/libopendssdirect.dylib
fi
