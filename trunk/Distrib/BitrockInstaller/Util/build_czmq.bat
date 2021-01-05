cmake .. -DCZMQ_BUILD_SHARED=ON -DCZMQ_BUILD_STATIC=OFF -DCMAKE_PREFIX_PATH=C:\cmdtools -DCMAKE_INSTALL_PREFIX=C:\cmdtools
cmake --build . --config Release --target install