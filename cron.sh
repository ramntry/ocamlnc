#!/bin/bash

BASEDIR=/root/ocamlnc

$BASEDIR/bs.sh > $BASEDIR/bs.stdout.log.running 2> $BASEDIR/bs.stderr.log.running
cat bs.stdout.log.running >> bs.stdout.log
cat bs.stderr.log.running >> bs.stderr.log
./extract_test_log bs.stdout.log.running | mail -s '*** ocamlnc build server log ***' ramntry@gmail.com
