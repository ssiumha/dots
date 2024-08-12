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
lspconfig.stimulus_ls.setup{}
lspconfig.tailwindcss.setup{}
lspconfig.tsserver.setup{}
lspconfig.bashls.setup{}
lspconfig.dockerls.setup{}
lspconfig.docker_compose_language_service.setup{}
-- lspconfig.ruby_ls.setup{
--   capabilities = capabilities
-- }
lspconfig.pyright.setup {
  analysis = {
    autoSearchPaths = true,
    diagnosticMode = "openFilesOnly",
    useLibraryCodeForTypes = true
  }
}
lspconfig.jsonls.setup {}
lspconfig.yamlls.setup {
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
