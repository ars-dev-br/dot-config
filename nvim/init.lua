-- if not installed, install packer for plugin management
local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.api.nvim_exec(
  [[
    augroup Packer
      autocmd!
      autocmd BufWritePost init.lua PackerCompile
    augroup end
  ]],
  false
)

-- install plugins with packer
local packer = require('packer')
packer.startup(function()
  packer.use('wbthomason/packer.nvim')

  -- color theme
  packer.use('rakr/vim-one')
  packer.use('cocopon/iceberg.vim')

  -- use git commands from vim
  packer.use('tpope/vim-fugitive')
  packer.use({
    'TimUntersberger/neogit',
    requires = 'nvim-lua/plenary.nvim'
  })

  -- comment selected lines with gc
  packer.use('tpope/vim-commentary')

  packer.use({ 'hoob3rt/lualine.nvim', requires = {
    'kyazdani42/nvim-web-devicons', opt = true
  }})

  packer.use('nvim-treesitter/nvim-treesitter')
  packer.use('nvim-treesitter/nvim-treesitter-textobjects')
  packer.use('neovim/nvim-lspconfig')
  packer.use({ 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } })
  packer.use({ 'ms-jpq/coq_nvim', branch = 'coq' })
  packer.use({ 'ms-jpq/coq.artifacts', branch = 'artifacts' })
  packer.use({
    'nvim-telescope/telescope.nvim', requires = {
      { 'nvim-lua/popup.nvim' },
      { 'nvim-lua/plenary.nvim' }
    }
  })
  packer.use('easymotion/vim-easymotion')
  packer.use('Raimondi/delimitMate')
  packer.use('p00f/nvim-ts-rainbow')
  packer.use('udalov/kotlin-vim')
  packer.use('ggandor/lightspeed.nvim')
  packer.use('knsh14/vim-github-link')
  packer.use('vhyrro/neorg')
end)

require('lualine').setup({
  options = {
    icons_enabled = false,
    theme = 'onelight'
  }
})

local lsp = require("lspconfig")
local coq = require("coq")

lsp.solargraph.setup(coq.lsp_ensure_capabilities({}))
lsp.kotlin_language_server.setup(coq.lsp_ensure_capabilities({
  cmd = {"/Users/user/src/fwcd/kotlin-language-server/server/build/install/server/bin/kotlin-language-server"}
}))
lsp.tsserver.setup(coq.lsp_ensure_capabilities({
  cmd = {"/Users/user/.nvm/versions/node/v12.16.3/bin/typescript-language-server", "--stdio"}
}))

vim.lsp.handlers["textDocument/publishDiagnostics"] =
  vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { update_in_insert = false })

-- set some sane defaults
vim.o.mouse = 'a'
vim.o.syntax = 'on'
vim.o.errorbells = false
vim.o.smartcase = true
vim.o.showmode = false
vim.bo.swapfile = false
vim.o.backup = false
vim.o.undodir = vim.fn.stdpath('config') .. '/undodir'
vim.o.undofile = true
vim.o.incsearch = true
vim.o.hidden = true
vim.o.completeopt='menuone,noinsert,noselect'
vim.bo.autoindent = true
vim.bo.smartindent = true
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.wo.signcolumn = 'yes'
vim.wo.wrap = false

vim.o.termguicolors = true
vim.cmd [[set background=light]]
vim.cmd [[colorscheme one]]

-- set relative line numbers with auto-toggle
vim.wo.number = true
vim.wo.relativenumber = true
vim.api.nvim_exec(
  [[
    augroup numbertoggle
      autocmd!
      autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
      autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
    augroup end
  ]],
  false
)

-- set space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Git
require('neogit').setup({})
require('gitsigns').setup({})

vim.api.nvim_set_keymap('n', '<leader>gg', [[<cmd>lua require('neogit').open()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>gl', [[<cmd>GetCommitLink<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<leader>gl', [[<cmd>GetCommitLink<CR>]], { noremap = true, silent = true })

-- Telescope
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
    layout_config = {
      horizontal = { preview_width = 0.6 }
    },
  },
}

vim.api.nvim_set_keymap('n', '<leader>fb', [[<cmd>lua require('telescope.builtin').buffers()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>ff', [[<cmd>lua require('telescope.builtin').find_files()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fn', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>hh', [[<cmd>lua require('telescope.builtin').help_tags()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fg', [[<cmd>lua require('telescope.builtin').live_grep()<CR>]], { noremap = true, silent = true })

-- rainbow delimiters
require('nvim-treesitter.configs').setup({
  rainbow = {
    enable = true,
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
  }
})

-- apply ruby syntax to .thor files
vim.api.nvim_exec('autocmd BufNewFile,BufRead *.thor set filetype=ruby', false)
