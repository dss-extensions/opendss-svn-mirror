cmake .. -DBUILD_SHARED_LIBS=ON -DHELICS_DISABLE_BOOST=ON -DHELICS_ZMQ_SUBPROJECT=OFF -DCMAKE_INSTALL_PREFIX=C:\cmdtools
git submodule update --init
cmake --build . --config Release --target install
