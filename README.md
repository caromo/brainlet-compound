# RPN

An RPN implementation in OCaml

## To Compile:

ocamlc str.cma rpninput.ml -o rpn-raw
ocamlc str.cma rpntext.ml -o rpn-text

## Details
The program takes in strings of operators and operands separated by whitespace (e.g. 3 4 5 + -) and evaluates its RPN representation.
