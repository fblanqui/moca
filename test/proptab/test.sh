#!/bin/sh

./configure
make

echo "Binary Booleans Without Sharing"
./bbns/test

echo "Binary Booleans No Sharing Closure"
./bbns+more/more_test

echo "Binary Booleans No Sharing False-Reduction"
./bbns+all/all_test

echo "Binary Booleans With Sharing"
./bbs/sharing_test
