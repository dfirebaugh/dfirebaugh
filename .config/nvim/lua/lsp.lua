local nvim_lsp = require("lspconfig")
local cmp_caps = require("cmp_nvim_lsp").default_capabilities()
require("mason-lspconfig")

local cmp_nvim_lsp = require("cmp_nvim_lsp")

local capabilities = cmp_nvim_lsp.default_capabilities(
	vim.lsp.protocol.make_client_capabilities()
)

local function common_on_attach(client, bufnr)
	if client.server_capabilities.documentFormattingProvider then
		vim.api.nvim_create_autocmd("BufWritePre", {
			group = vim.api.nvim_create_augroup("LspFormat." .. bufnr, { clear = true }),
			buffer = bufnr,
			callback = function()
				if not (vim.g.disable_autoformat or vim.b.disable_autoformat) then
					vim.lsp.buf.format({ async = false })
				end
			end,
		})
	end
end

local function go_on_attach(client, bufnr)
	common_on_attach(client, bufnr)
	if client.server_capabilities.codeActionProvider then
		vim.api.nvim_create_autocmd("BufWritePre", {
			group = vim.api.nvim_create_augroup("GoImports." .. bufnr, { clear = true }),
			buffer = bufnr,
			callback = function()
				vim.lsp.buf.code_action({
					context = { only = { "source.organizeImports" } },
					apply = true,
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
	lua_ls = {
		on_attach = common_on_attach,
		capabilities = capabilities,
		settings = {
			Lua = {
				-- diagnostics = {
				-- 	globals = { "vim" },
				-- },
				workspace = {
					checkThirdParty = false,
					telemetry = { enable = false },
					library = {
						vim.fn.stdpath("data") .. "/lazy",
					},
				},
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

for name, cfg in pairs(servers) do
	nvim_lsp[name].setup(vim.tbl_deep_extend("force", {
		on_attach = cfg.on_attach or common_on_attach,
		capabilities = cmp_caps,
		on_init = function(client)
			-- disable third‐party checks, etc. (if needed)
		end,
	}, cfg))
end
