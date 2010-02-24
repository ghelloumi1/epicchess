#!/bin/bash

rm debug/*.ml
sed -e 's/\(^open .*\)/(* \1 *)/' board.ml > debug/board.ml
sed -e 's/\(^open .*\)/(* \1 *)/' chess.ml > debug/chess.ml
sed -e 's/\(^open .*\)/(* \1 *)/' xboard.ml > debug/xboard.ml
cd debug/
ocaml -init .ocamlinit