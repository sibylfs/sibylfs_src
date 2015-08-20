#!/bin/bash

cp -R -L result result.tmp
rm -rf result
mv result.tmp result
chmod u+wx result
chmod u+wx result/bin


for f in result/bin/*; do
    patchelf --set-rpath "" --set-interpreter "/lib64/ld-linux-x86-64.so.2" $f
done
