Noctilux
========

A dark color theme for Emacs 24+ (using deftheme), inspired by the default dark theme in [Light Table](http://www.lighttable.com/) 0.4.0. This color theme is based off the definitions and format in sellout's awesome [emacs-color-theme-solarized](https://github.com/sellout/emacs-color-theme-solarized), providing support for a *lot* of modes.

This is still pretty rough around the edges; I think some color tweaks are still needed, and pull requests are definitely welcome.

Screenshot
==========

![Showing ido and some syntax highlighting](http://i.imgur.com/M3DPhCh.png)

Setup
-----

1. Add the directory containing `noctilux-definitions.el` and `noctilux-theme.el` to your `custom-theme-load-path` (e.g. `(add-to-list 'custom-theme-load-path "~/.emacs.d/lib/color-themes")`).
2. Add `(load-theme 'noctilux t)` somewhere to your Emacs initialisation.

Prerequisites
-------------

You'll at least need Emacs 24 for deftheme color-theme system. Used and tested with 24.3 and an sRGB graphical Emacs setup.
