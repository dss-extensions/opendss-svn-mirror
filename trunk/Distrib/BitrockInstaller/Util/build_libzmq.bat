cmake .. -DBUILD_STATIC=OFF -DBUILD_SHARED=ON -DZMQ_BUILD_TESTS=OFF -DENABLE_CPACK=OFF -DCMAKE_INSTALL_PREFIX=C:\cmdtools
cmake --build . --config Release --target install
