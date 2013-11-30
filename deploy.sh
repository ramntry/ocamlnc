#!/bin/bash

if [ -z "$OCAML_ROOT" ] \
  || [ -z "$LLVM_ROOT" ] \
  || [ -z "$LLVM_SRC_ROOT" ] \
  || [ -z "$LLVM_OBJ_ROOT" ]
then
  echo "Environment is not set! Do '. setenv.sh' first"
  exit 1
fi

if [ ! -f "$LLVM_ROOT/bin/llc" ]
then
  echo "llc not found. Make sure you edited setenv.sh properly"
  exit 2
fi

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
