# xit-mode

[x]it! is a plain-text file format for todos and check lists. This is an Emacs major mode that gives highlighting for .xit files.

![Screenshot](https://raw.githubusercontent.com/ryanolsonx/xit-mode/main/screenshot.png "Screenshot of Emacs [x]it!")

## Installation

### Melpa

`xit-mode` has a recipe in the [melpa repository](https://github.com/melpa/melpa) to ease its installation.

You can use the command `M-x package-install` and then select the `xit-mode` package to install it automatically.

### use-package

You can also rely on the [use-package](https://github.com/jwiegley/use-package) configuration helper to ease the installation and configuration from your `.emacs` file.

Just add the following snippet to your config file:

``` elisp
(use-package xit-mode :ensure t)
```

### Manual

Alternatively, you can still install the mode manually by following these steps:

#### 1. Download the package

```bash
cd ~/.emacs.d/
git clone https://github.com/ryanolsonx/xit-mode
```

#### 2. Load it in Emacs

In your .emacs or init.el:

```elisp
(load "~/.emacs.d/xit-mode/xit-mode.el")
(require 'xit-mode)
```

## Features

- Syntax highlighting with customizable faces
- Item state and priority management via keybindings
- `imenu` support

## Key bindings

- `C-c C-n` (`M-x xit-new-item`) : Create a new open item
- `C-c C-o` (`M-x xit-open-item`) : Set an item as open (`[ ]`)
- `C-c C-d` (`M-x xit-checked-item`) : Set an item as checked (`[x]`)
- `C-c C-p` (`M-x xit-ongoing-item`) : Set an item as ongoing (`[@]`)
- `C-c C-a` (`M-x xit-obsolete-item`) : Set an item as obsolete (`[!]`)
- `C-c C-C` (`M-x xit-state-cycle-item `) : Cycle through the different states (`open` -> `ongoing` -> `checked` -> `obsolete`)
- `C-c C-<up>` (`M-x xit-inc-priority-item`) : Increase the priority by adding a `!`
- `C-c C-<down>` (`M-x xit-dec-priority-item`) : Decrease the priority by removing a `!` or a `.`

## Customizable variables

### Faces

Here is the list of the faces used in the `xit-faces` group:

- `xit-open-checkbox-face`
- `xit-open-description-face`
- `xit-checked-checkbox-face`
- `xit-checked-description-face`
- `xit-ongoing-checkbox-face`
- `xit-ongoing-description-face`
- `xit-obsolete-checkbox-face`
- `xit-obsolete-description-face`
- `xit-priority-face`
- `xit-tag-face`

### Imenu

Imenu can be customized via the variable `xit-imenu-function`. Its possible values are:

- `'xit-imenu-groups` to list only groups
- `'xit-imenu-groups-and-items` to list groups and items
