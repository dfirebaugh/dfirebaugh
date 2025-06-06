vim.api.nvim_create_autocmd("FileType", {
	pattern = { "c", "cpp" },
	callback = function()
		local o = vim.opt_local
		vim.bo.commentstring = "/* %s */"
		o.tabstop = 4
		o.shiftwidth = 4
		o.expandtab = false
		o.cindent = true
		o.cinoptions = ":0"

		vim.b.disable_autoformat = true
	end,
})

vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
	pattern = "*.pun",
	command = "setfiletype punch",
})

vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
	pattern = "*.LUA",
	command = "setfiletype lua",
})

vim.api.nvim_create_user_command("FormatDisable", function(opts)
	if opts.bang then
		vim.b.disable_autoformat = true
	else
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
