#!/bin/bash
IFS=:
for p in $PATH; do
    ls -1 $p
done
