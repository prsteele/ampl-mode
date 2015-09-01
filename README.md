# AMPL mode

This project provides an Emacs major mode, `ampl-mode` for editing AMPL model and
data files. This mode provides an alias of `ampl-mode` called
`mathprog-mode`, as the GNU MathProg language is largely compatible
with AMPL syntax.

This mode currently supports basic syntax highlighting and
indentation.

## Installation

Place the file `ampl-mode.el` somewhere on your Emacs load path, and
add

> (load-library "ampl-mode")

to your init file, generally located at `~/.emacs`, `~/.emacs.el`, or
`~/.emacs.d/init.el`. The interactive command `ampl-mode` is now
available, although opening files ending in `.mod` or `.dat` will
automatically open those files with `ampl-mode` enabled.

## References

1. AMPL: A modeling language for mathematical programming. Robert
   Fourer, David M. Gay, and Brian W. Kernighan.
