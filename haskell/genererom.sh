#!/bin/sh

echo `./dist/build/generate-microprogram/generate-microprogram`> $1
echo `./dist/build/clock-program/clock-program`>> $1
