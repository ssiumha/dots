-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
-- TODO: brew install yaml-language-server
-- TODO: npm install -g dockerfile-language-server-nodejs @microsoft/compose-language-service bash-language-server
-- TODO: ast-grep? css json nginx? perl? postgres prisma rubocop? sorbat? sql stimulus_ls syntax_tree
-- TODO: html, htmx, terraform? tsserver, vue(or volar, vuels), ttags, typst?, unison(markdown), vimls,

local capabilities = require('cmp_nvim_lsp').default_capabilities()
local cmp = require'cmp'
cmp.setup({
  snippet = {
    expand = function(args)
      require('snippy').expand_snippet(args.body)
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

-- echo nvim_get_runtime_file('parser', v:true)
lspconfig = require'lspconfig'
-- lspconfig.stimulus_ls.setup{}
-- lspconfig.tailwindcss.setup{}
-- lspconfig.tsserver.setup{}
-- lspconfig.bashls.setup{}
-- lspconfig.dockerls.setup{}
-- lspconfig.docker_compose_language_service.setup{}
-- -- lspconfig.typos_lsp.setup{}
-- -- lspconfig.ruby_ls.setup{
-- --   capabilities = capabilities
-- -- }
-- lspconfig.pyright.setup {
--   analysis = {
--     autoSearchPaths = true,
--     diagnosticMode = "openFilesOnly",
--     useLibraryCodeForTypes = true
--   }
-- }
-- lspconfig.jsonls.setup {}
-- lspconfig.yamlls.setup {
--   settings = {
--     yaml = {
--       schemas = {
--         -- TODO: use environment variable?
--         ["https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.29.2-standalone-strict/deployment.json"] = "deployment/**/deployment/*.yaml",
--         ["https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.29.2-standalone-strict/statefulset.json"] = "deployment/**/statefulset/*.yaml",
--         ["https://json.schemastore.org/github-workflow.json"] = ".github/workflows/*",
--         ["https://json.schemastore.org/github-action.json"] = ".github/actions/*",
--       }
--     }
--   }
-- }

require'nvim-treesitter.configs'.setup {
  parser_install_dir = "~/.cache/treesitter",
  ensure_installed = { 'ruby', 'yaml' },
  auto_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = true,
    disable = { 'embedded_template' }
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

require'treesitter-context'.setup{
  enable = true,
  line_numbers = true,
  max_lines = 10,
  patterns = {
    default = { 'class', 'function', 'method' }
  }
}

----------------------------------
-- HiPhish/rainbow-delimiters.nvim
----------------------------------
local rainbow_delimiters = require 'rainbow-delimiters'

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
