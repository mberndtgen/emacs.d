# My Personal Emacs Configuration

This is my personal Emacs configuration. Most of it was inspired by
(or better: stolen from:) [skeet's repo](https://github.com/skeeto/dotfiles) --
and then adapted to my own needs. I'm using `package.el`, so **this
requires at least Emacs 24.4!**

To use this repo, clone it into your home directory. Move your existing
`.emacs` file out of the way, since it being there prevents Emacs from using the
`init.el` in this repository.

If you did everything right Emacs should simply launch with no
errors. You will be greeted with a featureless, empty gray box
awaiting your instructions. Note: I'm a Mac user, so you probably want to
change some path settings.

## Features

Since I'm still learning Emacs myself, I usually don't question the design decisions
of people using Emacs for years. I tried, however, to keep my configuration as easy and
modular as possible, omitting stuff I don't need. You will find a lot of good friends,
though. That's **powerline**, **company**, **paredit**, **helm**, and others. Slime
calls clisp (but sbcl is fine, too), clojure makes use of cider (what else), haskell
calls the cool development environment [intero](http://commercialhaskell.github.io/intero/).
Thus this configuration will give you REPLs for all languages mentioned above and
guarantees maximum joy. Assuming that you like `paredit`.


### Paredit

[Paredit](http://www.emacswiki.org/emacs/ParEdit) is a very
significant behavioral change for Lisp and Clojure modes. It enforces parenthesis
balance and provides all sorts of shortcuts for manipulating entire
s-expressions at once. It may feel annoying at first, but it quickly
becomes indispensable. Keep looking at the
[cheatsheet](http://www.emacswiki.org/emacs/PareditCheatsheet) until
you've got the hang of it.
