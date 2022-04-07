# xit-mode

[x]it! is a plain-text file format for todos and check lists. This is an Emacs major mode that gives highlighting for .xit files.

![Screenshot](https://raw.githubusercontent.com/ryanolsonx/xit-mode/main/screenshot.png "Screenshot of Emacs [x]it!")

## Installation

As this is WIP, it's not available on Melpa. When it's more complete, it'll be in Melpa. So for now, to use this, you'll need to:

### 1. Download the package

```bash
cd ~/.emacs.d/
git clone https://github.com/ryanolsonx/xit-mode
```

### 2. Load it in Emacs

In your .emacs or init.el:

```elisp
(load "~/.emacs.d/xit-mode/xit-mode.el")
(require 'xit-mode)
```
