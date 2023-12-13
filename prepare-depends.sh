#!/bin/sh
# This script fetches, builds and locally installs Joshua Kroll's fork of libsnark.
# (Adapted from https://github.com/scipr-lab/libsnark.)

set -x -e

DEPSRC=./depsrc
DEPINST=/usr

mkdir -p $DEPSRC

cd $DEPSRC
[ ! -d libsnark ] && git clone git://github.com/kejace/libsnark
cd libsnark
./prepare-depends.sh
make -j
# make check
sudo make install PREFIX=$DEPINST


