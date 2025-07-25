#!/bin/bash

set -x

#for f in *.F90 *.h
for f in *.h
do
  for branch in local main
  do
    g=$(find /home/gmap/mrpm/marguina/pack/50_sitest.01.IMPIIFCI2302REPRODP.y/src/$branch/ -name $f | grep -v -E '\.include/|\.inftb/')
    if [ "x$g" != "x" ]
    then   
      break
    fi
  done
  if [ "x$g" = "x" ]
  then
    echo "$f was not found"
  else
    echo "$f -> $g"
    diff $f $g
  fi
done
