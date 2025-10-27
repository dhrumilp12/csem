#!/bin/bash

set -x

make csem_exe INPUT=$1
make ref_csem_exe INPUT=$1

./csem_exe > ./test/csem_exe.out
./ref_csem_exe > ./test/ref_csem_exe.out

diff ./test/csem_exe.out ./test/ref_csem_exe.out
rm -f ./test/csem_exe.out
rm -f ./test/ref_csem_exe.out

