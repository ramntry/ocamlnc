export CC="gcc"
export CXX="g++"
export TEST_OCAMLOPT="ocamlopt.opt"

export OCAML_SRC_ROOT=`dirname $(readlink -f $BASH_ARGV)`

export OCAML_ROOT="PLACE OCAML INSTALLATION DIRECTORY (FULL PATH) HERE (e.g. /home/user/ocamlnc/dist)"
export LLVM_ROOT="PLACE LLVM INSTALLATION DIRECTORY (FULL PATH) HERE (e.g. /home/user/ocamlnc/llvm/dist)"
export LLVM_SRC_ROOT="PLACE LLVM SOURCE DIRECTORY (FULL PATH) HERE (e.g. /home/user/ocamlnc/llvm/llvm)"
export LLVM_OBJ_ROOT="PLACE LLVM BUILD DIRECTORY (FULL PATH) HERE (e.g. /home/user/ocamlnc/llvm/build)"
export JBLABGC="$OCAML_SRC_ROOT/gc/jblab-gc-dummy/build/Release+Asserts/lib/jblab-gc-dummy.so"

export OCAML_INCLUDE="$OCAML_ROOT/lib/ocaml"
export LLVM_INCLUDE="$LLVM_ROOT/include"

export PATH="$OCAML_ROOT/bin:$OCAML_ROOT/bin/scripts:$PATH"
export PATH="$LLVM_ROOT/bin:$PATH"

export C_INCLUDE_PATH="$LLVM_INCLUDE:$OCAML_INCLUDE:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="$LLVM_INCLUDE:$OCAML_INCLUDE:$CPLUS_INCLUDE_PATH"
