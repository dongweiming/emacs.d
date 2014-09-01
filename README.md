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
sudo pip install epc jedi pep8 autopep8 flake8
# or use virtualenv
virtualenv venv
source ~/venv/bin/activate
pip install jedi pep8 autopep8 flake8
# Start jedi Server
M-x jedi:install-server
```

Keymap
----

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-c h</kbd> |helm-mini
<kbd>M-x</kbd> |smex
<kbd>M-l</kbd> |helm-locate
<kbd>M-t</kbd> |helm-top
<kbd>c-x h</kbd> |emacs-bindings-help.
<kbd>C-x o</kbd> |switch-window
<kbd>M-h</kbd> |mark-paragraph
<kbd>M-{</kbd> |backward-paragraph
<kbd>M-}</kbd> |forward-paragraph
<kbd>C-c SPC</kbd> |ace-jump-word-mode
<kbd>C-c TAB</kbd> |ace-jump-line-mode
<kbd>C-c m</kbd> |mc/mark-next-like-this
<kbd>C-c N</kbd> |mc/mark-previous-like-this
<kbd>C-c ;</kbd> |edit-lines
<kbd>M-%</kbd> |anzu-query-replace
<kbd>C-M-%</kbd> |anzu-query-replace-regexp
<kbd>M-e</kbd> |eshell
<kbd>C-c e</kbd> |run-ipython
<kbd>C-x C-b</kbd> |ibuffer
<kbd>C-x C-a</kbd> |ag-project
<kbd>C-c x</kbd> |expand-region
<kbd>M-TAB</kbd> |zencoding-expand-line
<kbd>C-x C-f</kbd> |helm-find-files
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
<kbd>C-x C-r</kbd> |open-recentf-file
<kbd>C-x C-a</kbd> |ag-project
<kbd>C-x C-c <down></kbd> |smart-down
<kbd>C-x C-c <up></kbd> |smart-up
<kbd>C-x C-c <left></kbd> |smart-backward
<kbd>C-x C-c <right></kbd> |smart-forward
<kbd>C-c t</kbd> |isend-send
<kbd>C-c y</kbd> |isend-associate
<kbd>M-i</kbd> |change-inner
<kbd>M-o</kbd> |change-outer
<kbd>C-x <up></kbd> |windmove-up
<kbd>C-x <down></kbd> |windmove-down
<kbd>C-x <left></kbd> |windmove-left
<kbd>C-x <right></kbd> |windmove-right
<kbd>M <up></kbd> |drag-code-block-up
<kbd>M <down></kbd> |drag-code-block-down
<kbd>C-c v</kbd> |func/class list
<kbd>C-c f</kbd> |toggle-fullscreen ; Only for GUI
<kbd>C-c G</kbd> |search github
<kbd>C-c g</kbd> |search google
<kbd>C-c q</kbd> |search douban code
<kbd>C-c j</kbd> |add or delete comment
<kbd>C-c k</kbd> |align-text by =
<kbd>C-c w</kbd> |hs-hide-block
<kbd>C-c W</kbd> |hs-show-block
<kbd>C-c s</kbd> |hs-hide-all
<kbd>C-c S</kbd> |hs-show-all
<kbd>C-c c</kbd> |hs-toggle-hiding
<kbd>C-z</kbd> |undo
