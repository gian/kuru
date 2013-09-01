#!/bin/sh

# Generate the Kuru build file by munging the MLB
cat kuruc.mlb | grep -v SML_LIB | sed 's/\.sml$/.k/' | sed 's/\.sig$/.ks/' > kuruc.kb

../../bin/kuruc -d -o SelfKuru kuruc.kb


