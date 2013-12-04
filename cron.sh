#!/bin/bash

BASEDIR=/root/ocamlnc

$BASEDIR/bs.sh > $BASEDIR/bs.stdout.log.running 2> $BASEDIR/bs.stderr.log.running
cat $BASEDIR/bs.stdout.log.running >> $BASEDIR/bs.stdout.log
cat $BASEDIR/bs.stderr.log.running >> $BASEDIR/bs.stderr.log
$BASEDIR/extract_test_log $BASEDIR/bs.stdout.log.running | mail -s 'ocamlnc build server log' ramntry@gmail.com
