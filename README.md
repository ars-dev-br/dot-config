# dot-config

These are my `.config` files for the editors I usually use.

## Emacs

The Emacs config file is set to bootstrap
[straight.el](https://github.com/raxod502/straight.el).  Just start
Emacs normally and it will take care of everything.

## Neovim

The Neovim config file is set to bootstrap
[packer.nvim](https://github.com/wbthomason/packer.nvim).  All you
need to do is running:

```shell
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
```
