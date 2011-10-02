#!/bin/sh

SRC=$1
OBJ=$(basename $1 .S).o
BIN=$(basename $1 .S).bin

nasm -f macho $SRC && \
ld -o $BIN $OBJ && \
./$BIN
echo $?
