#!/bin/bash
mkdir -p $1
pushd $1
/home/evgeny/build/phantomjs-2.0.0/bin/phantomjs /home/evgeny/projects/mehirimShkufim/pullerMatrixLinks.js
curl `awk '{ print "-O", $0 }' < matrixLinks.txt`
gunzip *.gz
