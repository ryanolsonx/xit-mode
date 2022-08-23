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

## Key bindings

### Status

When placing the cursor on a line containing an item, some keybindings can help to change its status:

- `C-c C-o`: set the item as open (`[ ]`)
- `C-c C-d`: set the item as done (`[x]`)
- `C-c C-p`: set the item as in progress (`[@]`)
- `C-c C-a`: set the item as archived, a.k.a obsolete (`[~]`)

Additionally, there is a keybinding that cycle through these statuses:

- `C-c C-c`: cycle through the different statuses (`open` -> `done` -> `in progress` -> `archived`)

### Priority

In the same condition, it is possible to change the item's priority with the following keybindings:

- `C-c C-<up>`: increase the priority (by adding a `!`)
- `C-c C-<down>`: decrease the priority (by removing a `!` or a `.`)
