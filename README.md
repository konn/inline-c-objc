`inline-c-objc` - Inline Obj-C Interface for Haskell build on top of `inline-c`
================================================================================


INSTALL
-------
First, you might need install LLVM 3.7 by Homebrew or something:

```sh
$ brew install llvm37
```

Then you have to add the binary path to PATH env var:

```sh
$ export PATH=/usr/local/opt/llvm37/lib/llvm-3.7/bin:$PATH
# $ cabal install inline-c-objc  # We haven't upload this package on hackage yet...
$ stack build
```



