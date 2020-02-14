# Arithmetic Encoding
This repo contains a Haskell implementation of arithmetic encoding as outlined in "Arithmetic Coding for Data Compression". A PDF of the original paper is available in the */docs* directory. There is also a C implementation in */cbits*.

# Building
This project uses the stack build tool, just run `stack build` or `stack install` in project root.
```
USAGE: arith -[s|a|d] SRC DST
        -s encode with a static model
        -a encode with an adaptive model
        -d decode
```
