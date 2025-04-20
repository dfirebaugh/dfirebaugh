local options = {
  formatters_by_ft = {
    lua = { "stylua" },
    css = { "prettier" },
    html = { "prettier" },
    go = { "goimports", "gofmt" },
    python = { "isort", "black" },
    -- c = { "clang-format" },
  },

  -- uncomment for format on save
  -- format_on_save = {
  --   -- These options will be passed to conform.format()
  --   timeout_ms = 500,
  --   lsp_fallback = true,
  -- },
  format_on_save = function(bufnr)
    -- if either the global flag or this buffer's flag is set, skip formatting
    if vim.g.disable_autoformat or vim.b.disable_autoformat then
      return
    end
    return { timeout_ms = 500, lsp_fallback = true }
  end,
}

return options
