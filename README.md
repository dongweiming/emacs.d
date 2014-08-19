emacs.d
=======

A python web developer's .emacs.d

Installation
---

```
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
git clone https://github.com/dongweiming/emacs.d .emacs.d
cd .emacs.d
cask
sudo pip install jedi pep8 autopep8 flake8

```

Keymap
----

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-c SPC</kbd> |ace-jump-word-mode
<kbd>C-c C-c SPC</kbd> |ace-jump-line-mode
<kbd>C-></kbd> |mc/mark-next-like-this
<kbd>C-<</kbd> |mc/mark-previous-like-this
<kbd>M-%</kbd> |anzu-query-replace
<kbd>C-M-%</kbd> |anzu-query-replace-regexp
<kbd>M-e</kbd> |eshell
<kbd>C-x C-b</kbd> |ibuffer
<kbd>C-c x</kbd> |expand-region
<kbd>C-x C-f</kbd> |helm-find-files
<kbd>M-l</kbd> |helm-eshell-history
<kbd>C-z</kbd> |undo
<kbd>C-c b</kbd> |switch-to-previous-buffer
<kbd>M-n</kbd> |hold-line-scroll-up
<kbd>M-p</kbd> |hold-line-scroll-down
<kbd>C-c v</kbd> |A simple python tablist
<kbd>C-c w</kbd> |hs-hide-block
<kbd>C-c W</kbd> |hs-show-block
<kbd>C-c s</kbd> |hs-hide-all
<kbd>C-c S</kbd> |hs-show-all
<kbd>C-c l</kbd> |hs-hide-level
<kbd>C-c c</kbd> |hs-toggle-hiding
<kbd>C-x C-f</kbd> |helm-find-files
<kbd>C-tab</kbd> |helm-select-4th-action
<kbd>M-l</kbd> |helm-eshell-history
<kbd>C-x o</kbd> |switch-window
<kbd>C-x C-r</kbd> |recentf-ido-find-file
<kbd>C-x C-a</kbd> |ag-project
<kbd>C-x <up></kbd> |windmove-up
<kbd>C-x <down></kbd> |windmove-down
<kbd>C-x <left></kbd> |windmove-left
<kbd>C-x <right></kbd> |windmove-right
<kbd>M <up></kbd> |drag-code-block-up
<kbd>M <down></kbd> |drag-code-block-down
<kbd>C-x C-r</kbd> |recentf-ido-find-file
