#!/usr/bin/env bash

cd $1

for d in $PWD/*
do
    for f in $d/*.c
    do
      echo "gcc $f -o ${f%.c}"
      gcc "$f" -o ${f%.c}""
    done

    for obj in $d/*
    do
      if [[ -x $obj ]]
      then
        echo "../ir $obj $2"
        ../ir "$obj"
      fi
    done
done

cd ..
