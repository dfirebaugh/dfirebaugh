vim.opt.list = true
vim.opt.listchars = {
  space = '⋅',
  tab = '→ ',
  trail = '•',
  extends = '⟩',
  precedes = '⟨',
  eol = '↴'
}


vim.g.base46_cache = vim.fn.stdpath "data" .. "/nvchad/base46/"
vim.g.mapleader = " "

-- bootstrap lazy and all plugins
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  local repo = "https://github.com/folke/lazy.nvim.git"
  vim.fn.system { "git", "clone", "--filter=blob:none", repo, "--branch=stable", lazypath }
end

vim.opt.rtp:prepend(lazypath)

local lazy_config = require "configs.lazy"

-- load plugins
require("lazy").setup({
  {
    "NvChad/NvChad",
    lazy = false,
    branch = "v2.5",
    import = "nvchad.plugins",
    config = function()
      require "options"
    end,
  },

  { import = "plugins" },
}, lazy_config)

-- load theme
dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

require "nvchad.autocmds"

vim.schedule(function()
  require "mappings"
end)

vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "*.pun",
  command = "setfiletype punch"
})

-- vim.opt.cindent = true
-- vim.opt.cino = ":0"       -- Use tabs for indentation
-- vim.opt.tabstop = 4       -- Set the width of a tab character
-- vim.opt.shiftwidth = 4    -- Use 4 spaces for each indentation level
-- vim.opt.expandtab = false -- Use tabs instead of spaces

vim.api.nvim_create_user_command("FormatDisable", function(opts)
  if opts.bang then
    -- With !, disable only in this buffer
    vim.b.disable_autoformat = true
  else
    -- Without !, disable globally
    vim.g.disable_autoformat = true
  end
end, {
  desc = "Disable autoformat-on-save (use ! to target just this buffer)",
  bang = true,
})

vim.api.nvim_create_user_command("FormatEnable", function()
  vim.b.disable_autoformat = false
  vim.g.disable_autoformat = false
end, {
  desc = "Re-enable autoformat-on-save",
})

vim.wo.relativenumber = true
