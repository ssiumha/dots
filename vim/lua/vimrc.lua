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
-- 'esmuellert/nvim-eslint'
-------------------
require('nvim-eslint').setup{} -- mason eslint not working on pnpm

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
  }
}

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

-- TODO
-- vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
-- vim.opt.foldtext = "v:lua.vim.treesitter.foldtext()"

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
