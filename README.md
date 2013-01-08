# ocp-indent

A simple tool to indent OCaml programs

Authors: Louis Gesbert (OCamlPro), Thomas Gazagnaire (OCamlPro), Jun Furuse

License: GPL v3.0

## Compilation

You need OCaml and ocp-build installed to compile ocp-indent.

```bash
./configure
make
make install
```

If you use opam and want it installed alongside ocaml, you may want to use
`./configure --prefix $(opam config -var prefix)`.

## Usage

* `ocp-indent foo.ml` or `ocp-indent <foo.ml` outputs the file re-indented
* From emacs, load `emacs.el` to have tuareg-mode automatically use
  ocp-indent. _Warning_: it is in a very early stage, intended for testing and
  debug only.
* From emacs, you can also do a quick test with `C-u M-| ocp-indent`

These options should be mostly useful for binding in editors:
* `ocp-indent --lines <m>-<n>` reindents only from line `<m>` to line `<n>`
* `ocp-indent --numeric` only outputs the indentation values as integers, for
  the lines that should be reindented.

## Testing

* `tests/passing` contains tests that are properly indented and should be left
  unchanged by ocp-indent.
* `tests/failing` contains tests for which ocp-indent currently returns the
  results in `tests/failing-output`, hence `meld tests/failing{,-output}` should
  give an overview of currently known bugs.
* `tests/test.sh` checks the current state against the reference state (checked
  into git).
* `tests/test.sh --[git-]update` updates the current reference state.
* See `tests/test.sh --help` for more

Please make sure tu run `make && tests/test.sh --git-update` before any commit,
so that the repo always reflects the state of the program.

## Configuration options

A proper configuration engine is being written. For the time being you can use a
few environment variables defined at the beginning of `src/block.ml`: of most
use should be `match_clause_indent` (usually 2 or 4, default is 2) and
`type_indent`.
