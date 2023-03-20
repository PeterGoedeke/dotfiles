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

  use {
    "rafamadriz/friendly-snippets"
  }

  use {
    "L3MON4D3/LuaSnip",
    config = function()
      require("luasnip.loaders.from_vscode").lazy_load()
      require("luasnip.loaders.from_snipmate")
          .lazy_load({ paths = "~/.config/nvim/snippets" })
    end
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
        current_line_blame = false,
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
          map = 'jj',
          chars = { '{', '[', '(', '"', "'" },
          pattern = [=[[%'%"%>%]%)%}%,]]=],
          end_key = 'L',
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
      require("nvim-surround").setup {
        keymaps = {
          insert = false,
          insert_line = false,
          normal = "ys",
          normal_cur = "yss",
          normal_line = "yS",
          normal_cur_line = "ySS",
          visual = "S",
          visual_line = "gS",
          delete = "ds",
          change = "cs",
        }
      }
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

  use {
    "luukvbaal/nnn.nvim",
    config = function()
      require("nnn").setup {
        replace_netrw = 'picker'
      }
    end
  }

  use {
    "ThePrimeagen/harpoon",
    requires = { "nvim-lua/plenary.nvim" }
  }

  -- plugins for improving search performance
  use { "haya14busa/is.vim" }
  use { "gabrielpoca/replacer.nvim" }
  use { "haya14busa/vim-asterisk" }

  use {
    "AndrewRadev/switch.vim",
    config = function()
      vim.g.switch_custom_definitions = {
        { "std::string_view ", "const std::string &" },
        { "class",             "struct" }
      }
    end
  }

  use { "junegunn/fzf" }
  use { "junegunn/fzf.vim" }

  use { "liuchengxu/vista.vim" }

  use {
    "jakemason/ouroboros.nvim",
    requires = "nvim-lua/plenary.nvim"
  }

  use { "gauteh/vim-cppman" }

  use { "mfussenegger/nvim-dap" }
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
  use { "lakshayg/vim-bazel" }
  use { "nvim-tree/nvim-web-devicons" }
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
      vim.g.doom_one_plugin_nemake = false
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

vim.api.nvim_create_autocmd('BufWritePost', {
  command = 'source <afile> | silent! LspStop | silent! LspStart | PackerCompile',
  group = packer_group,
  pattern = vim.fn.expand '$MYVIMRC',
})

-- Set highlight on search
vim.o.hlsearch = true

vim.o.scrolloff = 10
vim.o.sidescrolloff = 5

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

--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

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
      }
    }
  },
  extensions = {
    file_browser = {
      hijack_netrw = false,
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
  indent = { enable = false, disable = { 'python' } },
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
  },
}

-- LSP settings.
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- Create a command `:Format` local to the LSP buffer

  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = bufnr })
  vim.keymap.set('n', 'gr', require('telescope.builtin').lsp_references, { buffer = bufnr })
  vim.keymap.set('n', 'gI', vim.lsp.buf.implementation, { buffer = bufnr })
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, { buffer = bufnr })
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { buffer = bufnr })

  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

local servers = {
  clangd = {
  },
  -- gopls = {},
  -- pyright = {},
  -- rust_analyzer = {},
  -- tsserver = {},
  lua_ls = {
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
    local config = {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
    }

    if (server_name == "clangd") then
      config["cmd"] = { 'clangd', '--clang-tidy',
        '--function-arg-placeholders=false', '--background-index',
        '--completion-style=bundled', '--header-insertion=iwyu',
        '--header-insertion-decorators' }
    end

    require('lspconfig')[server_name].setup(config)
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
    ['<C-d>'] = cmp.mapping.scroll_docs( -4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<C-j>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<C-k>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<C-f>'] = cmp.mapping(function(fallback)
      if luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<C-d>'] = cmp.mapping(function(fallback)
      if luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<C-g>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.mapping.abort()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<C-CR>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.mapping.abort()
      end
      fallback()
    end, { 'i', 's' }),
    ['jk'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.mapping.abort()
      end
      fallback()
    end, { 'i', 's' })
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


vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]
vim.cmd [[autocmd FileType cpp setlocal commentstring=//%s]]

vim.g.dispatch_no_maps = 1
vim.g.dispatch_no_tmux_make = 1
vim.g.bazel_make_command = "Make"

local dap = require('dap')
dap.adapters.cpp = {
  id = 'cpp',
  type = 'executable',
  command = '/home/peter/.local/share/nvim/mason/packages/cpptools/extension/debugAdapters/bin/OpenDebugAD7'
}

dap.configurations.cpp = {
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

vim.keymap.set('n', '*', "ms<Plug>(asterisk-z*)")
vim.keymap.set('n', '#', "ms<Plug>(asterisk-z#)")
vim.keymap.set('v', '*', "ms<Plug>(asterisk-z*)")
vim.keymap.set('v', '#', "ms<Plug>(asterisk-z#)")
vim.keymap.set('n', '/', "ms/")
vim.keymap.set('n', '?', "ms?")

local wk = require("which-key")

local function open_file(path)
  return function()
    vim.cmd("e " .. path)
  end
end

local function command(cmd)
  return function()
    vim.cmd(cmd)
  end
end

local function win_command(cmd)
  return function()
    pcall(vim.cmd.wincmd, cmd)
  end
end

local function harpoon_open(pos)
  return function()
    require("harpoon.ui").nav_file(pos)
  end
end

local function file_searcher(options)
  local exe = options.exe or 'fd'
  local sink = options.sink or 'e'

  local source = exe
  if options.flags then
    source = source .. ' ' .. options.flags
  end

  return function()
    vim.fn['fzf#run']({ source = source, dir = options.dir, sink = sink })
  end
end

local function testfunc(dir, options)
  return function()
    if not options then
      options = {}
      options.dir = dir
    end
    if not options.source then
      options.source = 'fd'
      print(options.source)
    end
    if not options.sink then
      options.sink = 'e'
    end
    vim.fn['fzf#run'](options)
  end
end

vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set('n', 'H', "^")
vim.keymap.set('n', 'L', "$")

vim.keymap.set({ 'n' }, '<C-k>', function()
  require('lsp_signature').toggle_float_win()
end, { silent = true, noremap = true, desc = 'toggle signature' })

vim.keymap.set('i', 'jk', "<ESC>`^", { silent = true, noremap = true })
vim.keymap.set('n', 'ga', "<Plug>Lightspeed_omni_s")

wk.register({
  [":"] = {
    command("History:"),
    "Search history"
  },
  ["/"] = {
    command("History/"),
    "Search searches"
  },
  ["."] = {
    command("NnnPicker %:p:h"),
    "Files from file"
  },
  ["~"] = {
    command("NnnPicker $HOME"),
    "Files from home"
  },
  r = {
    command("History!"),
    "Recent files"
  },
  d = {
    command("NnnPicker"),
    "Files from cwd"
  },
  f = {
    name = "file",
    s = {
      command("w"),
      "Save file"
    },
    o = {
      command("Ouroboros"),
      "source/header"
    },
    b = {
      open_file("%:h/BUILD.bazel"),
      "Bazel file"
    },
    r = {
      -- "<Cmd>lua require('telescope').extensions.frecency.frecency()<CR>",
      command("History!"),
      "Recent files"
    },
    p = {
      open_file("~/.config/nvim/init.lua"),
      "Edit config"
    }
  },
  s = {
    name = "search",
    ["~"] = {
      function()
        vim.fn['fzf#run']({
          source = 'fd', dir = '$HOME', sink = 'e'
        })
      end, "Search from home"
    },
    d = {
      function()
        vim.fn['fzf#run']({
          source = 'fd', dir = vim.fn.getcwd(), sink = 'e'
        })
      end, "Search from cwd"
    },
    ["."] = {
      function()
        vim.fn['fzf#run']({
          source = 'fd', dir = vim.fn.expand('%:p:h'), sink = 'e'
        })
      end, "Search from file"
    },
    u = {
      name = "ignored",
      ["~"] = {
        function()
          vim.fn['fzf#run']({
            source = 'fd -u', dir = '$HOME', sink = 'e'
          })
        end, "Search from home"
      },
      d = {
        function()
          vim.fn['fzf#run']({
            source = 'fd -u', dir = vim.fn.getcwd(), sink = 'e'
          })
        end, "Search from cwd"
      },
      ["."] = {
        function()
          vim.fn['fzf#run']({
            source = 'fd -u', dir = vim.fn.expand('%:p:h'), sink = 'e'
          })
        end, "Search from file"
      },
    },
    h = {
      name = "hidden",
      ["~"] = {
        function()
          vim.fn['fzf#run']({
            source = 'fd -H', dir = '$HOME', sink = 'e'
          })
        end, "Search from home"
      },
      d = {
        function()
          vim.fn['fzf#run']({
            source = 'fd -H', dir = vim.fn.getcwd(), sink = 'e'
          })
        end, "Search from cwd"
      },
      ["."] = {
        function()
          vim.fn['fzf#run']({
            source = 'fd -H', dir = vim.fn.expand('%:p:h'), sink = 'e'
          })
        end, "Search from file"
      },
    },
    e = {
      name = "extension",
      s = {
        ["~"] = {
          function()
            vim.fn['fzf#run']({
              source = 'fd -e java -e=cpp -e=c -e=h -e=hpp -e=java', dir = '$HOME', sink = 'e'
            })
          end, "Search from home"
        },
        d = {
          function()
            vim.fn['fzf#run']({
              source = 'fd -e java -e=cpp -e=c -e=h -e=hpp -e=java', dir = vim.fn.getcwd(), sink = 'e'
            })
          end, "Search from cwd"
        },
        ["."] = {
          function()
            vim.fn['fzf#run']({
              source = 'fd -e java -e=cpp -e=c -e=h -e=hpp -e=java', dir = vim.fn.expand('%:p:h'), sink = 'e'
            })
          end, "Search from file"
        },
      },
      j = {
        ["~"] = {
          function()
            vim.fn['fzf#run']({
              source = 'fd -e=json', dir = '$HOME', sink = 'e'
            })
          end, "Search from home"
        },
        d = {
          function()
            vim.fn['fzf#run']({
              source = 'fd -e=json', dir = vim.fn.getcwd(), sink = 'e'
            })
          end, "Search from cwd"
        },
        ["."] = {
          function()
            vim.fn['fzf#run']({
              source = 'fd -e=json', dir = vim.fn.expand('%:p:h'), sink = 'e'
            })
          end, "Search from file"
        },
      },
    },
    m = {
      builtin.man_pages,
      "Search man pages"
    },
    i = {
      function()
        vim.cmd("mark s")
        builtin.lsp_document_symbols()
      end,
      "Search symbols"
    },
    H = {
      command("Helptags!"),
      "Search help"
    },
  },
  b = {
    name = "buffer",
    d = {
      command("BD"),
      "Close buffer"
    }
  },
  ["-"] = {
    name = "harpoon",
    m = {
      require("harpoon.mark").add_file,
      "Add file"
    },
    ["."] = {
      require("harpoon.ui").toggle_quick_menu,
      "Toggle menu"
    },
    a = {
      harpoon_open(1),
      "To file 1"
    },
    s = {
      harpoon_open(2),
      "To file 2"
    },
    d = {
      harpoon_open(3),
      "To file 3"
    },
    f = {
      harpoon_open(4),
      "To file 4"
    },
    h = {
      require("harpoon.ui").nav_prev,
      "Previous file"
    },
    l = {
      require("harpoon.ui").nav_next,
      "Next file"
    }
  },
  o = {
    name = "open",
    gi = { open_file(".gitignore"), ".gitignore" },
    ct = { open_file(".clang-tidy"), ".clang-tidy" },
    cd = { open_file(".clangd"), ".clangd" },
    sm = { open_file("src/main.cpp"), "src/main.cpp" },
    bc = { open_file(".bazelrc"), ".bazelrc" },
    p = { open_file("package.json"), "package.json" },
    df = { open_file("Dockerfile"), "Dockerfile" },
    rm = { open_file("README.md"), "README.md" },
    to = { open_file("todos.md"), "todos.md" },
  },
  O = {
    name = "~/open",
    zs = { open_file("~/.zshrc"), "~/.zshrc" },
    zh = { open_file("~/.zshrc"), "~/.zshrc" },
    rc = { open_file("~/.config/ranger/rc.conf"), "~/.config/ranger/rc.conf" },
    tm = { open_file("~/.config/tmux/tmux.conf"), "~/.config/tmux/tmux.conf" },
    gi = { open_file("~/.gitignore"), "~/.gitignore" },
    gc = { open_file("~/.gitconfig"), "~/.gitconfig" },
  },
  p = {
    name = "project",
    p = {
      require("telescope").extensions.projects.projects,
      "open project"
    },
    r = {
      function()
        require("telescope").extensions.frecency.frecency({ workspace = "CWD" })
      end,
      "Recent project files"
    }
  },
  w = {
    name = "window",
    h = {
      win_command("h"),
      "Window left"
    },
    j = {
      win_command("j"),
      "Window down"
    },
    k = {
      win_command("k"),
      "Window up"
    },
    l = {
      win_command("l"),
      "Window right"
    },
    H = {
      command("MoveBufferLeft"),
      "Move window left"
    },
    J = {
      command("MoveBufferDown"),
      "Move window down"
    },
    K = {
      command("MoveBufferUp"),
      "Move window up"
    },
    L = {
      command("MoveBufferRight"),
      "Move window right"
    },
    ["<C-h>"] = {
      win_command("H"),
      "Move window to left"
    },
    ["<C-j>"] = {
      win_command("J"),
      "Move window to bottom"
    },
    ["<C-k>"] = {
      win_command("K"),
      "Move window to top"
    },
    ["<C-l>"] = {
      win_command("L"),
      "Move window to right"
    },
    t = {
      win_command("L"),
      "Move window to right"
    },
    v = {
      win_command("v"),
      "Split vertically"
    },
    s = {
      win_command("s"),
      "Split horizontally"
    },
    S = {
      function()
        local prev = vim.o.splitbelow
        vim.o.splitbelow = true
        vim.cmd.wincmd('s')
        vim.o.splitbelow = prev
      end,
      "Split horizontally, focus"
    },
    V = {
      function()
        local prev = vim.o.splitright
        vim.o.splitright = true
        vim.cmd.wincmd('v')
        vim.o.splitright = prev
      end,
      "Split vertically, focus"
    },
    d = {
      win_command("c"),
      "Close window"
    },
    mm = {
      win_command("o"),
      "Maximize"
    }
  },

  d = {
    command("NnnPicker %:h"),
    "File picker"
  },

  D = {
    command("NnnPicker " .. vim.fn.getcwd()),
    "File picker"
  },

  t = {
    name = "tab/toggle",
    n = {
      command("tabnew"),
      "New tab"
    },
    d = {
      command("tabclose"),
      "Close tab"
    },
    l = {
      command("tabnext"),
      "Next tab"
    },
    h = {
      command("tabprevious"),
      "Previous tab"
    },
    i = {
      command("IndentBlanklineToggle!"),
      "Toggle indent guide"
    },
    w = {
      function() vim.wo.wrap = not vim.wo.wrap end,
      "Toggle word wrap"
    },
    b = {
      command("Gitsigns toggle_current_line_blame"),
      "Toggle blame"
    },
    m = {
      win_command("T"),
      "Move window to new tab"
    }
  },
  l = {
    name = "list",
    n = {
      command("copen"),
      "Open qflist"
    },
    g = {
      command("cgetfile"),
      "Refresh qflist"
    },
    f = {
      function() require("replacer").run { rename_files = false } end,
      "Replace, no rename"
    },
    r = {
      function() require("replacer").run { rename_files = true } end,
      "Replace, rename"
    }
  },
  h = {
    name = "config",
    c = {
      command("PackerCompile"),
      "Compile config"
    },
    i = {
      command("PackerInstall"),
      "Install packages"
    },
    l = {
      command("PackerClean"),
      "Clean packages"
    }
  },
  c = {
    name = "code",
    r = {
      vim.lsp.buf.rename,
      "Rename"
    },
    o = {
      command("Vista!!"),
      "View file symbols"
    },
    a = {
      vim.lsp.buf.code_action,
      "Code action"
    },
    x = {
      command("Trouble"),
      "Project issues"
    },
    c = {
      command("exe 'Dispatch' dispatch#request().command"),
      "Rerun compile"
    },
    ["~"] = {
      function()
        vim.fn['fzf#run']({
          source = 'fd -e java -e=cpp -e=c -e=h -e=hpp -e=java', dir = '$HOME', sink = 'e'
        })
      end, "Search from home"
    },
    d = {
      function()
        vim.fn['fzf#run']({
          source = 'fd -e java -e=cpp -e=c -e=h -e=hpp -e=java', dir = vim.fn.getcwd(), sink = 'e'
        })
      end, "Search from cwd"
    },
    ["."] = {
      function()
        vim.fn['fzf#run']({
          source = 'fd -e java -e=cpp -e=c -e=h -e=hpp -e=java', dir = vim.fn.expand('%:p:h'), sink = 'e'
        })
      end, "Search from file"
    },
  }
}, { prefix = "<leader>" })

vim.g.fzf_preview_window = { 'up,80%' }

vim.cmd [[
    function! s:build_quickfix_list(lines)
      call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
      copen
      cc
    endfunction

    let g:fzf_action = {
      \ 'ctrl-q': function('s:build_quickfix_list'),
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }

    let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all+accept'
]]
