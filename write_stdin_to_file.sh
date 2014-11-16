#!/bin/bash

rm "$1"                         # destructo

while read in; do
    echo "$in" >> "$1"
done
