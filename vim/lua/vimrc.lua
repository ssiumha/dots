local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

vim.cmd [[packadd packer.nvim]]

require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  use 'neovim/nvim-lspconfig'
  use 'williamboman/mason.nvim'
  use 'williamboman/mason-lspconfig.nvim'

  use 'nvimdev/lspsaga.nvim'

  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-cmdline'

  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use 'nvim-treesitter/nvim-treesitter-context'
  use 'nvim-treesitter/nvim-treesitter-textobjects'

  use 'lukas-reineke/indent-blankline.nvim'
  use 'HiPhish/rainbow-delimiters.nvim'

  use 'lewis6991/gitsigns.nvim'

  use 'ibhagwan/fzf-lua'

  -- Require plugins for avante.nvim
  use 'stevearc/dressing.nvim'
  use 'nvim-lua/plenary.nvim'
  use 'MunifTanjim/nui.nvim'
  use 'MeanderingProgrammer/render-markdown.nvim'

  -- Optional dependencies for avante.nvim
  -- use 'hrsh7th/nvim-cmp'
  -- use 'nvim-tree/nvim-web-devicons' -- or use 'echasnovski/mini.icons'
  -- use 'HakonHarnes/img-clip.nvim'
  -- use 'zbirenbaum/copilot.lua'
  use {
    'yetone/avante.nvim',
    branch = 'main',
    run = 'make',
    config = function()
      require('avante_lib').load()
      require('avante').setup{
        provider = 'copilot',
      }
    end
  }

  if packer_bootstrap then
    require('packer').sync()
  end
end)

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("user_lsp_config", { clear = true }),
  callback = function(ev)
    local opts = { buffer = ev.buf, silent = true, noremap = true }

    vim.keymap.set("n", "gd",         vim.lsp.buf.declaration, opts)
    vim.keymap.set("n", "gi",         vim.lsp.buf.implementation, opts)
    vim.keymap.set("n", "<c-]>",      vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "K",          vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "<space>lD",  function() vim.diagnostic.setqflist({ open = true }) end, opts)
    vim.keymap.set('n', '<space>la', ':Lspsaga code_action<cr>', opts)
    vim.keymap.set('n', '<space>ld', ':Lspsaga show_cursor_diagnostics<cr>', opts)
    -- vim.keymap.set("n", "<space>la", vim.lsp.buf.code_action, opts)
  end,
})

-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
-- TODO: add auto installer
--   scripting: python deno bash
--   devops: terraform yaml dockerfile compose bash
--   rails: MasonInstall ruby-lsp stimulus tailwind rubocop...
--   nestjs: tsserver vue
--   web: css sjon nginx perl postgres prisma sql syntax_tree html htmx
--   markdown: unison
--   vim: lua, vimls
--   ttags, typst, unison

-------------------
-- CUSTOM
-------------------
local function get_keys(t)
  local keys = {}
  for k, _ in pairs(t) do table.insert(keys, k) end
  return keys
end

-- @param commands table
-- @usage
-- local commands = {
--   ['Test'] = function() print('test') end,
--   ['Open'] = function() vim.cmd('edit ~/.config') end,
-- }
-- @return nil
local function fzf_command(commands)
  vim.fn['fzf#run']({
    source = get_keys(commands),
    -- window = { width = 0.9, height = 0.6 },
    tmux = '90%,70%',
    sink = function(selected)
      if commands[selected] then
        commands[selected]()
      else
        vim.print('command not found: ' .. selected)
      end
    end,
  })
end

vim.api.nvim_create_user_command('FzfLuaTest', function()
  fzf_command({
    ['print'] = function() print('test') end,
    ['open'] = function() vim.cmd('edit ~/.config') end,
  })
end, {})

-------------------
-- williamboman/mason.nvim
-- neovim/nvim-lspconfig
-- nvimdev/lspsaga.nvim
-------------------
require('mason').setup {
  PATH = "prepend"
}
require("mason-lspconfig").setup {
  ensure_installed = {},
  handlers = {
    function(server_name)
      require("lspconfig")[server_name].setup {}
    end,
    ["html"] = function()
      require("lspconfig").html.setup {
        filetypes = { "html", "eruby" }
      }
    end,
    ["yamlls"] = function()
      require("lspconfig").yamlls.setup {
        settings = {
          yaml = {
            schemas = {
              -- TODO: use environment variable?
              ["https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.29.2-standalone-strict/deployment.json"] = "deployment/**/deployment/*.yaml",
              ["https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.29.2-standalone-strict/statefulset.json"] = "deployment/**/statefulset/*.yaml",
              ["https://json.schemastore.org/github-workflow.json"] = ".github/workflows/*",
              ["https://json.schemastore.org/github-action.json"] = ".github/actions/*",
            }
          }
        }
      }
    end,
    ["docker_compose_language_service"] = function()
      require("lspconfig").docker_compose_language_service.setup {
        filetypes = { "yaml" }
      }
    end,
    ["biome"] = function()
      local lspconfig = require("lspconfig")
      lspconfig.biome.setup {
        root_dir = lspconfig.util.root_pattern("biome.json")
      }
    end,
    ["ts_ls"] = function()
      local lspconfig = require("lspconfig")
      lspconfig.ts_ls.setup {
        root_dir = lspconfig.util.root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git"),
        init_options = {
          plugins = {
            -- {
            --   name = "@vue/typescript-plugin",
            --   location = "/usr/local/lib/node_modules/@vue/typescript-plugin",
            --   languages = {"javascript", "typescript", "vue"},
            -- },
          },
          preferences = {
            includeCompletionsForModuleExports = true,
            includeCompletionsForImportStatements = true,
          },
        },
      }
    end,
  }
}

vim.api.nvim_create_autocmd('LspAttach', {
  callback = function()
    require('lspsaga').setup{
      -- diagnostic = {
      --   on_insert = false,
      -- }
    }
  end
})


-------------------
-- hrsh7th/nvim-cmp
-------------------
-- local capabilities = require('cmp_nvim_lsp').default_capabilities()
local cmp = require'cmp'
cmp.setup({
  snippet = {
    expand = function(args)
      -- require('snippy').expand_snippet(args.body)
      -- vim.snippet.expand(args.body) -- For native neovim snippets (Neovim v0.10+)
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  mapping = cmp.mapping.preset.insert({
    -- ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    -- ['<C-f>'] = cmp.mapping.scroll_docs(4),
    -- ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true })
  }),
  sources = cmp.config.sources(
    { { name = 'nvim_lsp' }, },
    { { name = 'buffer' }, })
})


----------------------------------
-- nvim-treesitter/nvim-treesitter
----------------------------------
-- vim.opt.rtp:append("~/.cache/treesitter")
--
-- ~/.local/share/mise/installs/neovim/0.10.1/lib/nvim
require'nvim-treesitter.configs'.setup {
  -- parser_install_dir = "~/.cache/treesitter",
  ensure_installed = { 'ruby', 'yaml' },
  auto_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
    -- disable = { 'embedded_template' }
  },

  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ['ib'] = '@block.inner',
        ['ab'] = '@block.outer',
        ['if'] = '@function.inner',
        ['af'] = '@function.outer',
        ['ip'] = '@parameter.inner',
        ['ap'] = '@parameter.outer',
        ['aS'] = '@statement.outer',
      },
      selection_modes = {
        ['@block.outer'] = 'V',
        ['@block.inner'] = 'V',
        ['@function.outer'] = 'V',
        ['@function.inner'] = 'V',
        ['@statement.outer'] = 'V',
      },
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start     = { [']m'] = '@function.outer', },
      goto_next_end       = { [']M'] = '@function.outer', },
      goto_previous_start = { ['[m'] = '@function.outer', },
      goto_previous_end   = { ['[M'] = '@function.outer', },
    },
  },
}

vim.api.nvim_set_hl(0, "TreesitterContext", { ctermbg = 8, bg = "#393939" })
require'treesitter-context'.setup{
  enable = true,
  line_numbers = true,
  max_lines = 10,
  mode = 'topline',
  multiline_threshold = 1,
  patterns = {
    default = { 'class', 'function', 'method' }
  }
}

vim.opt.foldmethod = 'expr'
vim.opt.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
vim.opt.foldtext = "v:lua.neovim_foldtext()"

-- ref: https://github.com/neovim/neovim/pull/25391/files
---@package
---@return { [1]: string, [2]: string[] }[]|string
function neovim_foldtext()
  local ts = vim.treesitter
  local api = vim.api
  ---

  local foldstart = vim.v.foldstart
  local bufnr = api.nvim_get_current_buf()

  ---@type boolean, LanguageTree
  local ok, parser = pcall(ts.get_parser, bufnr)
  if not ok then
    return vim.fn.foldtext()
  end

  local query = ts.query.get(parser:lang(), 'highlights')
  if not query then
    return vim.fn.foldtext()
  end

  local tree = parser:parse({ foldstart - 1, foldstart })[1]

  local line = api.nvim_buf_get_lines(bufnr, foldstart - 1, foldstart, false)[1]
  if not line then
    return vim.fn.foldtext()
  end

  ---@type { [1]: string, [2]: string[], range: { [1]: integer, [2]: integer } }[] | { [1]: string, [2]: string[] }[]
  local result = {}

  local line_pos = 0

  for id, node, metadata in query:iter_captures(tree:root(), 0, foldstart - 1, foldstart) do
    local name = query.captures[id]
    local start_row, start_col, end_row, end_col = node:range()

    local priority = tonumber(metadata.priority or vim.highlight.priorities.treesitter)

    if start_row == foldstart - 1 and end_row == foldstart - 1 then
      -- check for characters ignored by treesitter
      if start_col > line_pos then
        table.insert(result, {
          line:sub(line_pos + 1, start_col),
          { { 'Folded', priority } },
          range = { line_pos, start_col },
        })
      end
      line_pos = end_col

      local text = line:sub(start_col + 1, end_col)
      table.insert(result, { text, { { '@' .. name, priority } }, range = { start_col, end_col } })
    end
  end

  local i = 1
  while i <= #result do
    -- find first capture that is not in current range and apply highlights on the way
    local j = i + 1
    while
      j <= #result
      and result[j].range[1] >= result[i].range[1]
      and result[j].range[2] <= result[i].range[2]
    do
      for k, v in ipairs(result[i][2]) do
        if not vim.tbl_contains(result[j][2], v) then
          table.insert(result[j][2], k, v)
        end
      end
      j = j + 1
    end

    -- remove the parent capture if it is split into children
    if j > i + 1 then
      table.remove(result, i)
    else
      -- highlights need to be sorted by priority, on equal prio, the deeper nested capture (earlier
      -- in list) should be considered higher prio
      if #result[i][2] > 1 then
        table.sort(result[i][2], function(a, b)
          return a[2] < b[2]
        end)
      end

      result[i][2] = vim.tbl_map(function(tbl)
        return tbl[1]
      end, result[i][2])
      result[i] = { result[i][1], result[i][2] }

      i = i + 1
    end
  end

  -- custom
  local count = vim.v.foldend - vim.v.foldstart + 1
  if count > 0 then
    --  '| ' .. vim.v.foldlevel .. ' dpeth'
    local endline = api.nvim_buf_get_lines(bufnr, vim.v.foldend - 1, vim.v.foldend, false)[1]
    endline = endline:gsub('^%s+', '')

    table.insert(result, i, { ' ...' .. count .. ' lines... ' .. endline, { 'Folded' } })
  end

  return result
end

----------------------------------
-- HiPhish/rainbow-delimiters.nvim
----------------------------------
local rainbow_delimiters = require 'rainbow-delimiters'

vim.api.nvim_create_autocmd('LspAttach', {
  pattern = '*.html.erb',
  callback = function()
    if not vim.b.rainbow_delimiters_enabled
       and vim.treesitter.get_parser() ~= nil
    then
      rainbow_delimiters.enable()
      vim.b.rainbow_delimiters_enabled = true
    end
  end
})

---@type rainbow_delimiters.config
vim.g.rainbow_delimiters = {
  strategy = {
    [''] = rainbow_delimiters.strategy['global'],
    vim = rainbow_delimiters.strategy['local'],
  },
  query = {
    [''] = 'rainbow-delimiters',
    lua = 'rainbow-blocks',
  },
  priority = {
    [''] = 110,
    lua = 210,
  },
  highlight = {
    'RainbowDelimiterRed',
    'RainbowDelimiterYellow',
    'RainbowDelimiterBlue',
    'RainbowDelimiterOrange',
    'RainbowDelimiterGreen',
    'RainbowDelimiterViolet',
    'RainbowDelimiterCyan',
  },
}

--------------------------------------
-- lukas-reineke/indent-blankline.nvim
--------------------------------------
-- local highlight = {
--     "RainbowRed",
--     "RainbowYellow",
--     "RainbowBlue",
--     "RainbowOrange",
--     "RainbowGreen",
--     "RainbowViolet",
--     "RainbowCyan",
-- }

-- local hooks = require "ibl.hooks"
-- hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
--     vim.api.nvim_set_hl(0, "RainbowRed",    { bg = "#E06C75" })
--     vim.api.nvim_set_hl(0, "RainbowYellow", { bg = "#E5C07B" })
--     vim.api.nvim_set_hl(0, "RainbowBlue",   { bg = "#61AFEF" })
--     vim.api.nvim_set_hl(0, "RainbowOrange", { bg = "#D19A66" })
--     vim.api.nvim_set_hl(0, "RainbowGreen",  { bg = "#98C379" })
--     vim.api.nvim_set_hl(0, "RainbowViolet", { bg = "#C678DD" })
--     vim.api.nvim_set_hl(0, "RainbowCyan",   { bg = "#56B6C2" })
-- end)
-- require("ibl").setup {
--     indent = { char = "" },
--     whitespace = { highlight = highlight, remove_blankline_trail = true },
--     scope = { highlight = highlight, enabled = true, char = "▎", show_exact_scope = true },
-- }


--------------------------------------
-- lewis6991/gitsigns.nvim
--------------------------------------
require('gitsigns').setup()
