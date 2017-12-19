# min-caml [![Build Status](https://travis-ci.org/prg-titech/min-caml.svg?branch=master)](https://travis-ci.org/prg-titech/min-caml)

An educational compiler and interpreter for a minimal subset of OCaml, written in
~2000 lines of OCaml.  For details, see:

- http://esumii.github.io/min-caml/ (Japanese Web page)
- http://esumii.github.io/min-caml/jpaper.pdf (Japanese academic paper)
- http://esumii.github.io/min-caml/index-e.html (English Web page)
- http://esumii.github.io/min-caml/paper.pdf (English academic paper)

## Setup

### Build

``` bash
$ make
```

### Test

``` bash
$ make test
$ ./<target>
```

### Interpreter

```bash
$ make interp
$ ./min-camli <target>
```

## CHANGELOG

[Updates on October 9, 2013]

- Moved from SourceForge https://sourceforge.net/p/min-caml/code/ to
  GitHub https://github.com/esumii/min-caml

- Merged the Mac OS patch by shinh
  https://twitter.com/shinh/status/322043108021907458

[Update on July 24, 2012]

- 32-bit x86 (with SSE2, that is, Pentium IV or later) is now
  supported (on Linux and Cygwin); execute ./to_x86 before make.

[Updates on September 17, 2008]

- PowerPC is now supported (in addition to SPARC), thanks to
  Ms. Masuko and Prof. Asai in Ochanomizu University.  You _must_
  execute either ./to_ppc or ./to_sparc _before_ make.

- The register allocator now uses a simpler algorithm.  It omits the
  backtracking (ToSpill and NoSpill) in previous versions.

