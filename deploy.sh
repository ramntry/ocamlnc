#!/bin/bash

cwd=`pwd`

./run_configure.sh || exit 1
make world.opt || exit 2
make install || exit 4
cp -r scripts $OCAML_ROOT/bin || exit 8
./build_dummy_gc.sh || exit 16

cd $cwd/gc/tests/trees
./check.sh || exit 32
cd $cwd

exit 0
