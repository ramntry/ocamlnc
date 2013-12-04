#!/bin/bash

./scripts/checkenv --before-install || exit 1

if [ "$1" = '-s' ]; then
  cp -r scripts $OCAML_ROOT/bin
  exit 0
fi

cwd=`pwd`

./run_configure.sh && cp _depend .depend || exit $1
make world && make bootstrap || exit 2
make depend && make opt || exit 4
make opt.opt || exit 8
make install && cp -r scripts $OCAML_ROOT/bin || exit 16

./build_dummy_gc.sh || exit 32
make test || exit 64

exit 0
