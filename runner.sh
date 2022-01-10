#!/bin/bash

stack run -- tac < "$2" > temp.tac
if [ $? -eq 0 ]; then
    case $1 in
        "interpreter")
            ./tac-interpreter/tac-runner temp.tac
            ;;
        "compiler")
            ./tac2mips/tac2mips temp.tac > out.asm
            ;;
        "optimizer")
            ./tac2mips/tac2mips temp.tac | ./optimips-prime/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/optimips-prime-exe/optimips-prime-exe > out.asm
            ;;
    esac
fi