require("nvim-treesitter.configs").setup {
  ensure_installed = {
    "vim", "lua", "vimdoc", "html", "css", "go", "wgsl", "rust"
  },
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}

vim.treesitter.language.register("rust", "punch")
