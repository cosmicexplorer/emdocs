#!/bin/bash

if [ -f "$1" ]; then
    rm "$1"                     # destructo
fi

while read -r in; do
    echo "$in" >> "$1"
done
