#!/bin/sh

printf %s\\n {1..41} | xargs -t -n1 -P8 -I{} RScript pairwise.distance.R {}
