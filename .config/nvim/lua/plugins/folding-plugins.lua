local foldIcon = ""
local hlgroup = "NonText"
local function foldTextFormatter(virtText, lnum, endLnum, width, truncate)
  -- Check if the buffer is valid and exists
  if not vim.api.nvim_buf_is_valid(0) or vim.api.nvim_buf_get_option(0, 'bufhidden') == 'wipe' then
    return virtText
  end

  if vim.api.nvim_buf_line_count(0) == 0 then
    return virtText
  end

  local newVirtText = {}
  local suffix = "  " .. foldIcon .. "  " .. tostring(endLnum - lnum)
  local sufWidth = vim.fn.strdisplaywidth(suffix)
  local targetWidth = width - sufWidth
  local curWidth = 0
  for _, chunk in ipairs(virtText) do
    local chunkText = chunk[1]
    local chunkWidth = vim.fn.strdisplaywidth(chunkText)
    if targetWidth > curWidth + chunkWidth then
      table.insert(newVirtText, chunk)
    else
      chunkText = truncate(chunkText, targetWidth - curWidth)
      local hlGroup = chunk[2]
      table.insert(newVirtText, { chunkText, hlGroup })
      chunkWidth = vim.fn.strdisplaywidth(chunkText)
      if curWidth + chunkWidth < targetWidth then
        suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
      end
      break
    end
    curWidth = curWidth + chunkWidth
  end
  table.insert(newVirtText, { suffix, hlgroup })
  return newVirtText
end

return {
  {
    "chrisgrieser/nvim-origami",
    event = "BufReadPost",
    opts = true,
    config = function()
      local origami = require("origami")
      origami.remember = function()
        if vim.fn.bufname('%') ~= '' then
          local success, err = pcall(vim.api.nvim_command, 'mkview')
          if not success then
            vim.notify('Error saving view: ' .. err, vim.log.levels.ERROR)
          end
        end
      end
    end
  },
  {
    "jghauser/fold-cycle.nvim",
    keys = {
      {
        "^",
        function()
          require("fold-cycle").close()
        end,
        desc = " Cycle-Close Folds",
      },
      {
        "\\",
        function()
          require("fold-cycle").open()
        end,
        desc = " Cycle-Open Folds",
      },
    },
  },
  {
    "kevinhwang91/nvim-ufo",
    dependencies = "kevinhwang91/promise-async",
    event = "BufReadPost",
    keys = {
      {
        "zr",
        function()
          require("ufo").openFoldsExceptKinds { "comment" }
        end,
        desc = " 󱃄 Open All Folds except comments",
      },
      {
        "zm",
        function()
          require("ufo").closeAllFolds()
        end,
        desc = " 󱃄 Close All Folds",
      },
      {
        "z1",
        function()
          require("ufo").closeFoldsWith(1)
        end,
        desc = " 󱃄 Close L1 Folds",
      },
      {
        "z2",
        function()
          require("ufo").closeFoldsWith(2)
        end,
        desc = " 󱃄 Close L2 Folds",
      },
      {
        "z3",
        function()
          require("ufo").closeFoldsWith(3)
        end,
        desc = " 󱃄 Close L3 Folds",
      },
      {
        "z4",
        function()
          require("ufo").closeFoldsWith(4)
        end,
        desc = " 󱃄 Close L4 Folds",
      },
    },
    init = function()
      vim.opt.foldlevel = 99
      vim.opt.foldlevelstart = 99
    end,
    opts = {
      provider_selector = function(_, ft, _)
        local lspWithOutFolding = { "markdown", "bash", "sh", "bash", "zsh", "css", "html", "python" }
        if vim.tbl_contains(lspWithOutFolding, ft) then
          return { "treesitter", "indent" }
        end
        return { "lsp", "indent" }
      end,
      close_fold_kinds_for_ft = { ["*"] = { "imports" } },
      open_fold_hl_timeout = 500,
      fold_virt_text_handler = foldTextFormatter,
    },
  },
}

