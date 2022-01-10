#!/bin/bash

stack run -- tac < "$2" > temp.tac
if [ "interpreter" == "$1" ]; then
    ./tac-interpreter/tac-runner temp.tac
else if [ "compiler" == "$1" ]; then
    ./tac2mips/tac2mips temp.tac
fi
fi