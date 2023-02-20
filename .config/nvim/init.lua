-- Install packer

local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
local is_bootstrap = false
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.system { 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path }
  vim.cmd [[packadd packer.nvim]]
end

require('packer').startup(function(use)
  -- Package manager
  use 'wbthomason/packer.nvim'

  use { -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    requires = {
      -- Automatically install LSPs to stdpath for neovim
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',

      -- Useful status updates for LSP
      'j-hui/fidget.nvim',

      -- Additional lua configuration, makes nvim stuff amazing
      'folke/neodev.nvim',
    },
  }

  use { "hrsh7th/cmp-cmdline" }
  use { -- Autocompletion
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp',
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip'
    },
  }

  use { -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    run = function()
      pcall(require('nvim-treesitter.install').update { with_sync = true })
    end,
  }

  use { -- Additional text objects via treesitter
    'nvim-treesitter/nvim-treesitter-textobjects',
    after = 'nvim-treesitter',
  }

  -- Git related plugins
  use 'tpope/vim-fugitive'
  use 'tpope/vim-rhubarb'
  use {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup {
        current_line_blame = true,
        signcolumn = false
      }
    end
  }

  use { 'nvim-lualine/lualine.nvim',
    config = function()
      require('lualine').setup {
        options = {
          icons_enabled = false,
          theme = 'auto',
          component_separators = '|',
          section_separators = '',
        },
      }
    end
  }

  use 'lukas-reineke/indent-blankline.nvim' -- Add indentation guides even on blank lines
  use 'tpope/vim-sleuth' -- Detect tabstop and shiftwidth automatically

  -- Fuzzy Finder (files, lsp, etc)
  use {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    requires = { 'nvim-lua/plenary.nvim' }
  }

  -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
  use {
    'nvim-telescope/telescope-fzf-native.nvim',
    run = 'make',
    cond = vim.fn.executable 'make' == 1
  }

  -- my plugins that are not supposed to be here
  use {
    "folke/which-key.nvim",
    config = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
      require("which-key").setup {}
    end
  }

  use {
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup {
        fast_wrap = {
          map = '<M-e>',
          chars = { '{', '[', '(', '"', "'" },
          pattern = [=[[%'%"%>%]%)%}%,]]=],
          end_key = '$',
          keys = 'qwertyuiopzxcvbnmasdfghjkl',
          check_comma = true,
          highlight = 'Search',
          highlight_grey = 'Comment'
        }
      }
    end
  }

  use {
    "ray-x/lsp_signature.nvim",
    config = function()
      require "lsp_signature".setup({
        bind = true,
        hint_enable = false,
        toggle_key = 'C-k',
        handler_opts = {
          border = "rounded"
        }
      })
    end
  }

  use { "onsails/lspkind.nvim" }


  use({
    "kylechui/nvim-surround",
    tag = "*", -- Use for stability; omit to use `main` branch for the latest features
    config = function()
      require("nvim-surround").setup {}
    end
  })

  use { "nvim-lua/plenary.nvim" }

  use {
    "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("todo-comments").setup {}
    end
  }

  use {
    "Pocco81/abbrev-man.nvim",
    config = function()
      require('abbrev-man').setup {
        load_natural_dictionaries_at_startup = true,
        load_programming_dictionaries_at_startup = false,
        natural_dictionaries = {
          ["nt_en"] = {
            ["ba"] = "!",
            ["ne"] = "!=",
            ["le"] = "<=",
            ["ge"] = ">=",
          }
        },
      }
    end
  }

  use {
    "nvim-telescope/telescope-frecency.nvim",
    config = function()
      require "telescope".load_extension("frecency")
    end,
    requires = { "kkharji/sqlite.lua" }
  }

  use { "tommcdo/vim-exchange" }

  use {
    "ggandor/lightspeed.nvim",
    config = function()
      require('lightspeed').setup {
        ignore_case = true,
      }
    end
  }

  use { "liuchengxu/vista.vim" }

  use {
    "jakemason/ouroboros.nvim",
    requires = "nvim-lua/plenary.nvim"
  }

  use { "gauteh/vim-cppman" }

  use { "kevinhwang91/rnvimr" }
  use { "mfussenegger/nvim-dap" }
  -- use { "francoiscabrol/ranger.vim" }
  use { "tpope/vim-commentary" }
  use { "svban/YankAssassin.vim" }
  use { "wellle/targets.vim" }
  use { "c60cb859/bufMov.nvim" }
  use { "qpkorr/vim-bufkill" }

  use {
    "ahmedkhalf/project.nvim",
    config = function()
      require('project_nvim').setup {
        scope_chdir = 'tab'
      }
    end
  }

  use { "farmergreg/vim-lastplace" }
  use { "kana/vim-textobj-user" }
  use { "nvim-telescope/telescope-file-browser.nvim" }
  -- use {
  --   "bazelbuild/vim-bazel",
  --   requires = "google/vim-maktaba"
  -- }
  use { "lakshayg/vim-bazel" }
  -- use { "bazelbuild/vim-bazel" }
  use { "neomake/neomake" }
  use { "nvim-tree/nvim-web-devicons" }
  -- use {
  --   "nvim-tree/nvim-tree.lua",
  --   requires = "nvim-tree/nvim-web-devicons"
  -- }
  use { "tpope/vim-dispatch" }


  use {
    "folke/trouble.nvim",
    requires = "nvim-tree/nvim-web-devicons",
    config = function()
      require('trouble').setup {}
    end
  }

  use {
    "kana/vim-textobj-entire",
    requires = {
      "kana/vim-textobj-user"
    }
  }

  use {
    "kana/vim-textobj-line",
    requires = {
      "kana/vim-textobj-user"
    }
  }

  use {
    "beloglazov/vim-textobj-punctuation",
    requires = {
      "kana/vim-textobj-user"
    }
  }

  use {
    "Julian/vim-textobj-variable-segment",
    requires = {
      "kana/vim-textobj-user"
    }
  }

  use({
    'NTBBloodbath/doom-one.nvim',
    setup = function()
      -- Add color to cursor
      vim.g.doom_one_cursor_coloring = false
      -- Set :terminal colors
      vim.g.doom_one_terminal_colors = true
      -- Enable italic comments
      vim.g.doom_one_italic_comments = true
      -- Enable TS support
      vim.g.doom_one_enable_treesitter = true
      -- Color whole diagnostic text or only underline
      vim.g.doom_one_diagnostics_text_color = false
      -- Enable transparent background
      vim.g.doom_one_transparent_background = false

      -- Pumblend transparency
      vim.g.doom_one_pumblend_enable = false
      vim.g.doom_one_pumblend_transparency = 20

      -- Plugins integration
      vim.g.doom_one_plugin_neorg = false
      vim.g.doom_one_plugin_barbar = false
      vim.g.doom_one_plugin_telescope = true
      vim.g.doom_one_plugin_neogit = false
      vim.g.doom_one_plugin_nvim_tree = false
      vim.g.doom_one_plugin_dashboard = false
      vim.g.doom_one_plugin_startify = false
      vim.g.doom_one_plugin_whichkey = true
      vim.g.doom_one_plugin_indent_blankline = true
      vim.g.doom_one_plugin_vim_illuminate = false
      vim.g.doom_one_plugin_lspsaga = false
    end,
    config = function()
      vim.cmd("colorscheme doom-one")
    end,
  })


  -- Add custom plugins to packer from ~/.config/nvim/lua/custom/plugins.lua
  local has_plugins, plugins = pcall(require, 'custom.plugins')
  if has_plugins then
    plugins(use)
  end

  if is_bootstrap then
    require('packer').sync()
  end
end)

-- When we are bootstrapping a configuration, it doesn't
-- make sense to execute the rest of the init.lua.
--
-- You'll need to restart nvim, and then it will work.
if is_bootstrap then
  print '=================================='
  print '    Plugins are being installed'
  print '    Wait until Packer completes,'
  print '       then restart nvim'
  print '=================================='
  return
end

-- Automatically source and re-compile packer whenever you save this init.lua
local packer_group = vim.api.nvim_create_augroup('Packer', { clear = true })

-- vim.api.nvim_create_autocmd('BufWritePost', {
--   command = 'source <afile> | silent! LspStop | silent! LspStart | PackerCompile',
--   group = packer_group,
--   pattern = vim.fn.expand '$MYVIMRC',
-- })

-- Set highlight on search
vim.o.hlsearch = true

vim.o.scrolloff = 10

-- Disable line numbers
vim.wo.number = false

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 250

-- Do not show side gutter
vim.wo.signcolumn = 'no'

-- Wrapping options
vim.o.wrap = false
vim.o.linebreak = true
vim.o.list = false
vim.opt.colorcolumn = "81"
vim.wo.cursorline = true
vim.opt.clipboard:append { "unnamedplus" }
-- vim.o.autochdir = true -- TODO find a better way to do this
-- vim.wo.cursorcolumn = true

-- Set colorscheme
vim.o.termguicolors = true

-- Set completeopt to have a better completion experience
-- TODO: what is this?
vim.o.completeopt = 'menuone,noselect'

-- [[ Basic Keymaps ]]
-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

vim.keymap.set('n', 'H', "^")
vim.keymap.set('n', 'L', "$")

-- vim.keymap.set('i', 'jk', "<ESC>")

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- Enable `lukas-reineke/indent-blankline.nvim`
-- See `:help indent_blankline.txt`
require('indent_blankline').setup {
  -- char = '.',
  show_trailing_blankline_indent = false,
  show_current_context = true,
  enabled = false
}

-- Gitsigns
-- See `:help gitsigns.txt`
require('gitsigns').setup {
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
  },
}

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  tiebreak = function(_, _) return false end,
  defaults = {
    mappings = {
      i = {
        -- ['<C-u>'] = false,
        -- ['<C-d>'] = false,
      },
    },
    layout_config = {
      horizontal = { width = 0.9 }
    }
  },
  pickers = {
    current_buffer_fuzzy_find = {
      tiebreak = function(_, _) return false end,
      sorting_strategy = 'ascending',
      fuzzy = false,
      layout_strategy = 'vertical',
      layout_config = {
        preview_cutoff = 0,
        preview_height = 25,
        height = 0.99,
        width = 0.99
        -- horizontal = {
        --   width = 0.99,
        --   height = 1 don't make this too high becasue I think that is what is messing things up
        -- }
        -- preview_height
      }
    }
  },
  extensions = {
    file_browser = {
      hijack_netrw = false
    },
    fzf = {
      fuzzy = false,
    }
  }
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')
pcall(require('telescope').load_extension, 'projects')
pcall(require('telescope').load_extension, 'frecency')
require("telescope").load_extension "file_browser"

local builtin = require('telescope.builtin')

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>ss', builtin.current_buffer_fuzzy_find, { desc = '[/] Fuzzily search in current buffer]' })
vim.keymap.set('n', '<leader>sr', builtin.resume, { desc = 'Resume the previous search' })
vim.keymap.set('n', '<leader>sc', builtin.commands, { desc = 'Search commands' })
vim.keymap.set('n', '<leader>sC', builtin.command_history, { desc = 'Search recently used commands' })
vim.keymap.set('n', '<leader>sd', builtin.live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sm', builtin.man_pages, { desc = 'Search man pages' })
vim.keymap.set('n', '<leader>si', builtin.lsp_document_symbols, { desc = 'Search man pages' })

vim.keymap.set('n', '<leader><space>', builtin.buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>sf', builtin.find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>fp', function() vim.cmd('e ~/.config/nvim/init.lua') end, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sh', builtin.help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', builtin.grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>bd', function() vim.cmd("BD") end, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sD', builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>ff', function()
  require("telescope").extensions.file_browser.file_browser({
    path = "%:p:h"
  })
end,
  { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>fs', function() vim.cmd('w') end)
vim.keymap.set('n', '<leader>fo', function() vim.cmd("Ouroboros") end)

vim.keymap.set('n', '<leader>.', function()
  require("telescope").extensions.file_browser.file_browser({
    path = "%:p:h"
  })
end,
  { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>pp', function() require('telescope').extensions.projects.projects {} end)


vim.keymap.set('n', '<leader>fr', builtin.oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>dd', function() vim.cmd("RnvimrToggle") end, { desc = '[?] Find recently opened files' })

local function wincmd(command)
  return function() vim.cmd.wincmd(command) end
end

vim.keymap.set('n', '<leader>wh', wincmd('h'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>wj', wincmd('j'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>wk', wincmd('k'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>wl', wincmd('l'), { desc = '[?] Find recently opened files' })

vim.keymap.set('n', '<leader>wL', function() vim.cmd('MoveBufferRight') end, { noremap = true })
vim.keymap.set('n', '<leader>wJ', function() vim.cmd('MoveBufferDown') end, { noremap = true })
vim.keymap.set('n', '<leader>wH', function() vim.cmd('MoveBufferLeft') end, { noremap = true })
vim.keymap.set('n', '<leader>wK', function() vim.cmd('MoveBufferUp') end, { noremap = true })

vim.keymap.set('n', '<leader>w<C-l>', wincmd('L'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>wt', wincmd('L'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>w<C-j>', wincmd('J'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>w<C-k>', wincmd('K'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>w<C-h>', wincmd('H'), { desc = '[?] Find recently opened files' })

vim.keymap.set('n', '<leader>wv', wincmd('v'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>ws', wincmd('s'), { desc = '[?] Find recently opened files' })

vim.keymap.set('n', '<leader>wS', function()
  local prev = vim.o.splitbelow
  vim.o.splitbelow = true
  vim.cmd.wincmd('s')
  vim.o.splitbelow = prev
end, { desc = '[?] Find recently opened files' })

vim.keymap.set('n', '<leader>wV', function()
  local prev = vim.o.splitright
  vim.o.splitright = true
  vim.cmd.wincmd('v')
  vim.o.splitright = prev
end, { desc = '[?] Find recently opened files' })

vim.keymap.set('n', '<leader>wd', wincmd('c'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>wn', wincmd('n'), { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader>wmm', wincmd('o'), { desc = '[?] Find recently opened files' })


-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
require('nvim-treesitter.configs').setup {
  -- Add languages to be installed here that you want installed for treesitter
  ensure_installed = {
    'c',
    'cpp',
    'go',
    'lua',
    'python',
    'java',
    'rust',
    'typescript',
    'help',
    'vim'
  },

  highlight = { enable = true },
  indent = { enable = true, disable = { 'python' } },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<c-space>',
      node_incremental = '<c-space>',
      scope_incremental = '<c-s>',
      node_decremental = '<c-backspace>',
    },
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ['aa'] = '@parameter.outer',
        ['ia'] = '@parameter.inner',
        ['am'] = '@function.outer',
        ['im'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
    },
    swap = {
      -- enable = true,
      -- swap_next = {
      --   ['<leader>a'] = '@parameter.inner',
      -- },
      -- swap_previous = {
      --   ['<leader>A'] = '@parameter.inner',
      -- },
    },
  },
}

-- Diagnostic keymaps
-- vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
-- vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
-- vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
-- vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

-- LSP settings.
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>cr', vim.lsp.buf.rename, '[R]e[n]ame')

  nmap('<leader>co', function()
    vim.cmd("Vista!!")
  end, '[R]e[n]ame')

  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')
  nmap('<leader>cx', function() vim.cmd("Trouble") end, '[C]ode [A]ction')
  nmap('<leader>cc', function()
    vim.cmd("Dispatch")
    vim.cmd("cgetfile")
  end, '[C]ode [A]ction')

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  -- nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

  -- See `:help K` for why this keymap
  -- nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  -- nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  -- nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  -- nmap('<leader>wl', function()
  --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  -- end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

local servers = {
  clangd = {},
  -- gopls = {},
  -- pyright = {},
  -- rust_analyzer = {},
  -- tsserver = {},

  sumneko_lua = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
      diagnostics = {
        globals = { 'vim' }
      }
    },
  },
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Setup mason so it can manage external tooling
require('mason').setup()

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
    }
  end,
}

-- Turn on lsp status information
require('fidget').setup()

-- nvim-cmp setup
local cmp = require 'cmp'
local luasnip = require 'luasnip'
local lspkind = require 'lspkind'

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['jk'] = function(fallback)
      cmp.mapping.abort()
      fallback()
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  },
  enabled = function()
    -- disable completion in comments
    local context = require 'cmp.config.context'
    -- keep command mode completion enabled when cursor is in a comment
    if vim.api.nvim_get_mode().mode == 'c' then
      return true
    else
      return not context.in_treesitter_capture("comment")
          and not context.in_syntax_group("Comment")
    end
  end,
  formatting = {
    format = lspkind.cmp_format({
      mode = "symbol",
    }),
  }
}

cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    {
      name = 'cmdline',
      option = {
        ignore_cmds = { 'Man', '!' }
      }
    }
  })
})

local cmp_autopairs = require('nvim-autopairs.completion.cmp')
cmp.event:on(
  'confirm_done',
  function()
    cmp_autopairs.on_confirm_done()
  end
)

vim.keymap.set({ 'n' }, '<C-k>', function()
  require('lsp_signature').toggle_float_win()
end, { silent = true, noremap = true, desc = 'toggle signature' })

local wk = require("which-key")
wk.register(mappings, opts)

vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]

vim.keymap.set('i', 'jk', "<ESC>`^", { silent = true, noremap = true })
vim.keymap.set('n', 'ga', "<Plug>Lightspeed_omni_s")

vim.keymap.set('n', '<leader>ti', function()
  vim.cmd("IndentBlanklineToggle!")
end, { desc = 'do the thing' })

vim.keymap.set('n', '<leader>tb', function()
  vim.cmd("Gitsigns toggle_current_line_blame")
end, { desc = 'toggle blame' })

vim.g.dispatch_no_maps = 1
vim.g.bazel_make_command = "Make"

-- local M = {}

-- M.toggle = function(character)
--   local api = vim.api
--   local delimiters = { ',', ';' }
--   local line = api.nvim_get_current_line()
--   local comment_delim = vim.o.commentstring:gsub(" %%s", "")


--   local first, _ = vim.regex(vim.o.commentstring:gsub(" %%s", ""))
--       :match_str(vim.api.nvim_get_current_line())
--   local last_char = line:sub(first, first)

--   if last_char == character then
--     -- return api.nvim_set_current_line(line:sub(1, #line - 1)
--     -- return api.nvim_set_current_line(line:sub(1, #line - 1))
--     print("aweooo")
--     return api.nvim_set_current_line(line:sub(1, first - 1) .. line:sub(first + 1))
--   else
--     return api.nvim_set_current_line(line:sub(1, first) .. character .. line:sub(first + 1))
--   end
-- end

-- M.map = function(mode, target, source, opts)
--   vim.keymap.set(mode, target, source, opts)
-- end

-- vim.keymap.set('n', ';', function() M.toggle(';') end)

--   local test = 5 -- test commment
--   local line = vim.api.nvim_get_current_line()
--   local first, last = vim.regex(vim.o.commentstring:gsub(" %%s", ""))
--       :match_str(vim.api.nvim_get_current_line())
--   -- print(line:sub(first - 1))
--   -- print(first)
--   print(string.sub(line, first, first))
-- end

-- M.setup = function(options)
--   for _, key in ipairs(options.keys) do
--     M.map('n', options.leader .. key, ':lua require("chartoggle").toggle("' .. key .. '")<CR>',
--       { noremap = true, silent = true })
--   end
-- end

local dap = require('dap')
dap.adapters.cppdbg = {
  id = 'cppdbg',
  type = 'executable',
  command = '/home/peter/.local/share/nvim/mason/packages/cpptools/extension/debugAdapters/bin/OpenDebugAD7'

}

dap.configurations.cppdbg = {
  {
    name = "Launch file",
    type = "cppdbg",
    request = "launch",
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
    cwd = '${workspaceFolder}',
    stopAtEntry = true,
    setupCommands = {
      {
        text = '-enable-pretty-printing',
        description = 'enable pretty printing',
        ignoreFailures = false
      },
    },
  },
  {
    name = 'Attach to gdbserver :1234',
    type = 'cppdbg',
    request = 'launch',
    MIMode = 'gdb',
    miDebuggerServerAddress = 'localhost:1234',
    miDebuggerPath = '/usr/bin/gdb',
    cwd = '${workspaceFolder}',
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
    setupCommands = {
      {
        text = '-enable-pretty-printing',
        description = 'enable pretty printing',
        ignoreFailures = false
      },
    },
  },
}
