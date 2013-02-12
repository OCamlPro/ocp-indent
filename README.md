# ocp-indent

A simple tool to indent OCaml programs

Authors: Louis Gesbert (OCamlPro), Thomas Gazagnaire (OCamlPro), Jun Furuse

License: GPL v3.0

## Installation

### Using OPAM

The simplest way to install `ocp-indent` is using [OPAM](http://opam.ocamlpro.com):

```bash
opam install ocp-indent
```

### From sources

You can also install `ocp-indent` from sources. You'll need `ocaml (>= 3.12.1)`
and `ocp-build (>= 1.99.3-beta)`:

```bash
./configure
make
make install
```

If you use opam and want it installed alongside ocaml, you may want to use
`./configure --prefix $(opam config var prefix)`.

## Usage

The above installation step copies elisp and vim scripts to
`<prefix>/share/typerex/ocp-indent/`. You then need to load them in
the editor of your choice to automatically use ocp-indent.

### Emacs

Run the following command to setup tuareg-mode to use `ocp-indent`: 

```bash
echo '(load-file "$(opam config var prefix)/share/typerex/ocp-indent/ocp-indent.el")' >>~/.emacs
```

### Vim

Use the following command to tell Vim to use `ocp-indent`:

```bash
echo 'autocmd FileType ocaml source $(opam config var prefix)/share/typerex/ocp-indent/ocp-indent.vim' >>~/.vimrc
```

### Other editors

As `ocp-indent` is a command-line tool, you can easily integrate it with other editors.

```bash
ocp-indent <src-file> > <dst-file>
```

You can also tell it to indent only a subsets of lines, and to output only the indentation level:

```bash
ocp-indent <src-file> --lines <l1>-<l2> --numeric
```

## How does it compare to tuareg ?

We've run some benchmarks on real code-bases and the result is quite
conclusive. Keep in mind than most of existing source files are
either indented manually or following tuareg standards. You can
see the results [here](http://htmlpreview.github.com/?https://github.com/AltGr/ocp-indent-tests/blob/master/status.html).

Moreover, as `ocp-indent` has a deep understanding of the OCaml syntax
it shines on specific cases. See for instance the collection of
unit-tests
[here](https://github.com/OCamlPro/ocp-indent/tree/master/tests/passing). The
currently failing tests can be seen
[here](http://htmlpreview.github.com/?https://github.com/OCamlPro/ocp-indent/blob/master/tests/failing.html).


## Testing

It's hard to deliver a great indenter without tests. We've built
`ocp-indent` based on a growing collection of unit-tests. If you find an
indentation bug, feel free to send us a code snippet that we will
incorporate into our test suite.

The tests are organized as follows:

* `tests/passing` contains tests that are properly indented and should be left
  unchanged by ocp-indent.
* `tests/failing` contains tests for which ocp-indent currently returns the
  results in `tests/failing-output`, hence `meld tests/failing{,-output}` should
  give an overview of currently known bugs (also available online
  [here](http://htmlpreview.github.com/?https://github.com/OCamlPro/ocp-indent/blob/master/tests/failing.html)).
* `tests/test.sh` checks the current state against the reference state (checked
  into git).
* `tests/test.sh --[git-]update` updates the current reference state.
* See `tests/test.sh --help` for more

Please make sure tu run `make && tests/test.sh --git-update` before any commit,
so that the repo always reflects the state of the program.

## Configuration options

By default, `ocp-indent` comes with sensible default options. However,
you can customize some of the indentation options using command-line
options. For more details, see:

```bash
ocp-indent --config help
```

There is no built-in support for in-file configuration directives. Yet, some
editors already provide that features, and with emacs, starting your file with a
line like:

```
(* -*- ocp-indent-config: in=2 -*- *)
```

will enable you to have the indentation after `in` setup to 2 locally on this
file.
