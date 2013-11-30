#!/bin/bash

cwd=`pwd`
dummy_gc_soname=`basename $JBLABGC`
dummy_gc_name=${dummy_gc_soname/%.so/}
dummy_gc_dir=$LLVM_SRC_ROOT/projects/$dummy_gc_name

rm -rf $dummy_gc_dir
cp -r $LLVM_SRC_ROOT/projects/sample $dummy_gc_dir
rm -rf $dummy_gc_dir/{tools,include,docs}
sed -i -e 's/ tools//' -e 's/ include//' $dummy_gc_dir/Makefile
rm -f $dummy_gc_dir/lib/sample/sample.c
mv $dummy_gc_dir/lib/sample $dummy_gc_dir/lib/$dummy_gc_name
sed -i -e  "s/sample/$dummy_gc_name/" $dummy_gc_dir/lib/Makefile
cp -r gc/jblab-gc-dummy $dummy_gc_dir/lib
sed -i -e "s/sample/$dummy_gc_name\nLOADABLE_MODULE=1/" $dummy_gc_dir/lib/$dummy_gc_name/Makefile
sed -i -e 's/^AC_INIT.*$/AC_INIT([[[jblab-gc-dummy]]],[[[0.01]]],[tereshin.y.roman@gmail.com])/' \
       -e "s/^AC_CONFIG_MAKEFILE(lib\/sample\/Makefile)/AC_CONFIG_MAKEFILE(lib\/$dummy_gc_name\/Makefile)/" \
       -e '/^AC_CONFIG_MAKEFILE(tools\/.*)$/d' \
       -e "" \
    $dummy_gc_dir/autoconf/configure.ac

cd $dummy_gc_dir
./autoconf/AutoRegen.sh 2> AutoRegen.log
cd $cwd

mkdir gc/jblab-gc-dummy/build
cd gc/jblab-gc-dummy/build
$dummy_gc_dir/configure --enable-debug-runtime --enable-keep-symbols --enable-bindings=ocaml --prefix=`pwd`
make && make install
cd $cwd
