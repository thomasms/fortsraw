#!/bin/bash

dobuild(){
    mkdir -p $1
    cd $1
    mkdir -p toast
    cd toast
    cmake -DCMAKE_BUILD_TYPE=Release ../../external/toast
    make
    cd ..
    cmake -DCMAKE_BUILD_TYPE=Release ..
    make
    ./bin/fortsrawtests
    cd ..
}

echo "Building with GNU..."
export FC=/usr/local/bin/gfortran
rm -rf buildgfortran
dobuild buildgfortran

echo "Building with Intel..."
export FC=/opt/intel/bin/ifort
rm -rf buildifort
dobuild buildifort
