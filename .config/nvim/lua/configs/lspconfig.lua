local nvlsp     = require("nvchad.configs.lspconfig")
local lspconfig = require("lspconfig")
local caps      = require("cmp_nvim_lsp").default_capabilities()

local function common_on_attach(client, bufnr)
  nvlsp.on_attach(client, bufnr)
  vim.keymap.set("n", "<F8>", function() vim.lsp.buf.format({ async = true }) end, { buffer = bufnr })
  vim.keymap.set("n", "gd", function()
    vim.lsp.buf.definition()
    vim.cmd("normal! zz")
  end, { buffer = bufnr })
end

local function go_on_attach(client, bufnr)
  common_on_attach(client, bufnr)

  if client.server_capabilities.codeActionProvider then
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = vim.api.nvim_create_augroup("GoAutoFormat", { clear = true }),
      buffer = bufnr,
      callback = function()
        vim.lsp.buf.code_action({
          context = { only = { "source.organizeImports" } },
          apply   = true,
        })
        vim.lsp.buf.format({ async = false })
      end,
    })
  end
end

local servers = {
  clangd = {},
  html   = {},
  cssls  = {},
  glslls = {},

  ts_ls  = {
    on_attach = function(c, b)
      common_on_attach(c, b)
      if c.server_capabilities.documentFormattingProvider then
        vim.api.nvim_create_autocmd("BufWritePre", {
          group    = vim.api.nvim_create_augroup("TSFormat", { clear = true }),
          buffer   = b,
          callback = function() vim.lsp.buf.format({ async = false }) end
        })
      end
    end,
  },

  pylsp  = {},
  lua_ls = {
    settings = {
      Lua = {
        workspace   = {},
        diagnostics = { globals = { "vim" } },
      },
    },
  },

  gopls  = {
    on_attach = go_on_attach,
    settings = {
      gopls = {
        gofumpt     = true,
        staticcheck = true,
        analyses    = { unusedparams = true },
      },
    },
  },
}

for name, opts in pairs(servers) do
  lspconfig[name].setup(vim.tbl_deep_extend("force", {
    on_attach    = common_on_attach,
    on_init      = nvlsp.on_init,
    capabilities = caps,
  }, opts))
end
