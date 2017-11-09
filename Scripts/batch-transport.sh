#!/bin/sh

printf %s\\n {1..42} | xargs -t -n1 -P8 -I{} RScript pairwise.distance.R {}
