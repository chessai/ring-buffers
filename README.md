# ring-buffers

[![Hackage](https://img.shields.io/hackage/v/ring-buffers.svg)](https://hackage.haskell.org/package/ring-buffers)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)

This package provides concurrent, mutable ring buffers with atomic updates in GHC Haskell.

It uses the [contiguous](http://hackage.haskell.org/package/contiguous) package to provide multiple array backends found in the [primitive](http://hackage.haskell.org/package/primitive) package.
