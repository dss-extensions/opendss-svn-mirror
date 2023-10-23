ppca64 @m1dll.cfg -B opendssdirect.lpr
install_name_tool -add_rpath @loader_path/. test/libopendssdirect.dylib
