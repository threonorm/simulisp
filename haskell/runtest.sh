#!/bin/bash
rm -f test/*-sorted*;

for file in `ls test/ | grep .net | sed -e 's/\.net//g'`;do
  mv test/$file.net test/$file.nl;
done

for file in `ls test/ | grep .nl | sed -e 's/\.nl//g'`; do
  echo $file "--" `./Main test/$file`;
done
