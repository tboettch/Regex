#!/bin/sh
# Runs GHCI with the modules and sources loaded
ghci -isrc/ -icabal-dev/lib testsuite/tests/Regex.hs
