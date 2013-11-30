export CC="gcc"
export CXX="g++"
export OCAML_ROOT=/home/ramntry/projects/ocaml/ocaml/testbuild/dist
export LLVM_ROOT=/opt/llvm/3.3/with_ocaml_bindings_again_build
export LLVM_SRC_ROOT=/home/ramntry/projects/ocaml/llvm/3.2/llvm
export LLVM_OBJ_ROOT=/home/ramntry/projects/ocaml/llvm/3.2/build_with_ocaml_bindings_again
export JBLABGC="$PWD/gc/jblab-gc-dummy/build/Release+Asserts/lib/jblab-gc-dummy.so"

export OCAML_INCLUDE="$OCAML_ROOT/lib/ocaml"
export LLVM_INCLUDE="$LLVM_ROOT/include"

export PATH="$OCAML_ROOT/bin:$OCAML_ROOT/bin/scripts:$PATH"
export PATH="$LLVM_ROOT/bin:$PATH"

export C_INCLUDE_PATH="$LLVM_INCLUDE:$OCAML_INCLUDE:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="$LLVM_INCLUDE:$OCAML_INCLUDE:$CPLUS_INCLUDE_PATH"
