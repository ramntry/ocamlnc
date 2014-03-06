#!/bin/bash

numof_old_builds_to_save=12

#host_llvm_root="/opt/llvm-3.3/dist"
#host_CC="$host_llvm_root/bin/clang"
#host_CXX="$host_llvm_root/bin/clang++"
host_CC="gcc"
host_CXX="g++"

this_script_full_path="$(dirname $(readlink -f $0))"

function remove_old_builds {
  bs_topdir="$this_script_full_path/bs"
  echo "#### Remove old builds"
  if [ ! -d "$bs_topdir" ]; then
    echo "bs directory not found, that's ok"
    return 0
  fi
  numof_old_builds=`ls "$bs_topdir" | wc -l`
  echo "$numof_old_builds old build(s) was found"
  if [ "$numof_old_builds" -le "$numof_old_builds_to_save" ]; then
    echo "there is nothing to remove"
    return 0
  else
    toremove_list=`ls $bs_topdir | sort | head -n $((numof_old_builds - numof_old_builds_to_save))`
  fi
  for build in $toremove_list; do
    echo "remove $bs_topdir/$build"
    rm -rf "$bs_topdir/$build"
  done
  return 0
}

cwd=`pwd`

echo
echo
echo "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
echo "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*" 1>&2
remove_old_builds

bs_dirname="$(date +%Y.%m.%d-%H.%M.%S)-$RANDOM"
echo "#### BOOTSTRAP in $bs_dirname"
bs_root="$this_script_full_path/bs/$bs_dirname"

mkdir -p $bs_root/{ocamlnc,llvm-3.3}
echo "############################ getting repo ############################"
cd "$bs_root/ocamlnc"
git clone git@github.com:ramntry/ocamlnc.git

echo "##################### building of original tools #####################"
mkdir 4.01.0-original_build
original_ocaml_root="$bs_root/ocamlnc/4.01.0-original_build"
cd ocamlnc
./configure -cc "$host_CC" -prefix "$original_ocaml_root"
make world
make opt
make install

echo "############################ getting llvm ############################"
cd ../../llvm-3.3
wget http://llvm.org/releases/3.3/llvm-3.3.src.tar.gz
echo "#### Unpack llvm sources"
tar -xzf llvm-3.3.src.tar.gz
mv llvm-3.3.src llvm

mkdir build dist
llvm_root="$bs_root/llvm-3.3/dist"
llvm_src_root="$bs_root/llvm-3.3/llvm"
llvm_obj_root="$bs_root/llvm-3.3/build"
echo "########################## building of llvm ##########################"
cd build
PATH="$original_ocaml_root/bin:$PATH" CC="$host_CC" CXX="$host_CXX" ../llvm/configure \
  --enable-debug-runtime --enable-keep-symbols --enable-bindings=ocaml \
  --enable-targets=x86_64 --prefix="$llvm_root"

rm -rf "$llvm_src_root/projects/jblab-gc-dummy/"
make -j 4 || make -j 2 || make
make install

echo "#### Switch to jblab-gc-testing branch"
cd ../../ocamlnc/ocamlnc
git fetch origin jblab-gc-testing
git checkout -b jblab-gc-testing origin/jblab-gc-testing
cp _setenv.sh setenv.sh
mkdir ../dist
ocaml_root="$bs_root/ocamlnc/dist"

echo "#### Edit and source setenv.sh"
sed -i -e "s|CC=\\\".*\\\"|CC=\\\"$host_CC\\\"|" setenv.sh
sed -i -e "s|CXX=\\\".*\\\"|CXX=\\\"$host_CXX\\\"|" setenv.sh
sed -i -e "s|OCAML_ROOT=\\\".*\\\"|OCAML_ROOT=\\\"$ocaml_root\\\"|" setenv.sh
sed -i -e "s|LLVM_ROOT=\\\".*\\\"|LLVM_ROOT=\\\"$llvm_root\\\"|" setenv.sh
sed -i -e "s|LLVM_SRC_ROOT=\\\".*\\\"|LLVM_SRC_ROOT=\\\"$llvm_src_root\\\"|" setenv.sh
sed -i -e "s|LLVM_OBJ_ROOT=\\\".*\\\"|LLVM_OBJ_ROOT=\\\"$llvm_obj_root\\\"|" setenv.sh
. setenv.sh

echo "############################## deploy ##############################"
./deploy.sh

cd "$cwd"
echo
echo "#### DONE at $(date +%Y.%m.%d-%H.%M.%S)"
exit 0
