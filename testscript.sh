#!/usr/bin/env bash

cd $1
haskell = "../ir.hs"
for d in $PWD/*
do
    for f in $d/*.c
    do
      echo "gcc $f -o ${f%.c}"
      gcc "$f" -o "${f%.c}"
    done

    for obj in $d/*
    do
      if [[ -x $obj ]]
      then
        echo "runhaskell ../ir.hs $obj $2"
        runhaskell $(haskell) $(obj) $2
      fi
    done
done

cd ..
