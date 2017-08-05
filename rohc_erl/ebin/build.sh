#!/bin/sh
find ../src -name "*.erl" |xargs erlc -I ../include
