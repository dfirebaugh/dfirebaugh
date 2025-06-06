local map = vim.keymap.set

map("n", ";", ":", { desc = "Enter command mode" })
map("i", "jk", "<ESC>", { desc = "Exit insert mode" })
map("n", "gd", function()
	vim.lsp.buf.definition()
	vim.cmd("normal! zz")
end, { desc = "Go to definition and center" })
map("n", "<F8>", function()
	vim.lsp.buf.format({ async = true })
end, { desc = "LSP: Format buffer" })
map("n", "<C-n>", "<cmd>Oil<CR>", { desc = " Open Oil" })
map("n", "gn", "<cmd>BufferLineCycleNext<CR>", { desc = "BufferLine: Next Tab" })
map("n", "gp", "<cmd>BufferLineCyclePrev<CR>", { desc = "BufferLine: Prev Tab" })
map("n", "g<C-w>", "<cmd>BufferLineCloseOthers<CR>", { desc = "BufferLine: Close Others" })
map("n", "gq", "<cmd>BufferLineCloseRight<CR>", { desc = "BufferLine: Close Right" })
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { desc = "Telescope: List Buffers" })
map("n", "<leader>/", function()
	require("Comment.api").toggle.linewise.current()
end, { desc = "Comment: toggle current line" })
map("v", "<leader>/",
	"<ESC><CMD>lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>",
	{ silent = true, desc = "Comment: toggle visual selection" }
)
map("n", "<leader>x", "<cmd>bdelete<CR>", { desc = "Close buffer" })

map(
	"n",
	"<leader>fw",
	"<cmd>Telescope live_grep<CR>",
	{ desc = "Telescope: Live Grep" }
)
map("n", "<leader>ff", "<cmd>Telescope find_files<CR>", { desc = "Find file" })

map("n", "<leader>bb", "<cmd>Telescope buffers<CR>", { desc = "Switch buffer" })
map("n", "<leader>bi", "<cmd>Telescope buffers<CR>", { desc = "Ibuffer (list buffers)" })
map("n", "<leader>bk", "<cmd>bdelete<CR>", { desc = "Kill buffer" })
map("n", "<leader>bn", "<cmd>bnext<CR>", { desc = "Next buffer" })
map("n", "<leader>bp", "<cmd>bprevious<CR>", { desc = "Previous buffer" })


map("n", "<M-i>", function()
	vim.cmd("normal ==")
	pcall(vim.cmd, [[%s/\r//ge]])
end, { desc = "Fix indentation and remove ^M" })

map("v", "<M-i>", function()
	vim.cmd("normal =")
	pcall(vim.cmd, [[<,'>s/\r//ge]])
end, { desc = "Fix indentation and remove ^M in selection" })

vim.keymap.set("n", "<leader>ghp", "<cmd>CopilotChatToggle<CR>", { desc = "Copilot Chat" })
