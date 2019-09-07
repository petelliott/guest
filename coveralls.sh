#!/usr/bin/env bash

lcov -a $1 -b $(realpath .) -o coverage2.info
coveralls-lcov --repo-token $COVERALLS_TOKEN coverage2.info
