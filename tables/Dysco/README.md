The Dysco compression technique is explained in the article "Compression of interferometric radio-astronomical data",
A. R. Offringa (2016; http://arxiv.org/abs/1609.02019). If you use this software, please cite the paper.

# dysco
A compressing storage manager for Casacore mearement sets

To install:

    mkdir build
    cd build
    cmake ../
    make -j 4
    make install

To be able to open compressed measurement sets, the dysco library ("libdyscostman.so") must be in your path.

The Dysco compression technique is explained in [the article](http://arxiv.org/abs/1609.02019). Further documentation for the storage manager, including documentation of the `dscompress` tool:

https://github.com/aroffringa/dysco/wiki
