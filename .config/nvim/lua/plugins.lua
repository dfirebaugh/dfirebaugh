return require("lazy").setup({
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts = {
			plugins = {
				marks = true,
				registers = true,
				spelling = {
					enabled = true,
					suggestions = 20,
				},
			},
		},
		config = function(_, opts)
			require("which-key").setup(opts)
		end,
	},
	{
		"akinsho/toggleterm.nvim",
		version = "*",
		keys = { { "<M-h>", mode = { "n", "i", "t" } } },
		opts = {
			size              = 15,
			direction         = "horizontal",
			hide_numbers      = true,
			shading_factor    = 2,
			start_in_insert   = true,
			persist_size      = true,
			insert_mappings   = true,
			terminal_mappings = true,
			persist_mode      = true,
			open_mapping      = [[<M-h>]],
		},
		config = function(_, opts)
			require("toggleterm").setup(opts)
		end,
	},
	{
		"catppuccin/nvim",
		name = "catppuccin",
		priority = 1000,
		config = function()
			require("catppuccin").setup {
				flavour = "mocha",
				background = { light = "latte", dark = "mocha" },
				transparent_background = false,
				term_colors = true,
				styles = {
					comments = { "italic" },
				},
			}
		end,
	},
	{
		"Vigemus/iron.nvim",
		lazy = false,
		config = function()
			require("iron.core").setup {
				config = {
					scratch_repl = true,
					repl_definition = {
						sh = { command = { "zsh" } },
						python = { command = { "ipython", "--no-autoindent" } },
						lua = { command = { "lua" } },
					},
					repl_open_cmd = "vertical botright 80 split",
				},
				keymaps = {
					send_motion = "<leader>sc",
					visual_send = "<leader>sc",
					send_file = "<leader>sf",
					send_line = "<leader>sl",
					interrupt = "<leader>sI",
					exit = "<leader>sq",
					clear = "<leader>cl",
				},
				highlight = { italic = true },
				ignore_blank_lines = true,
			}

			vim.api.nvim_create_autocmd("FileType", {
				pattern = "org",
				callback = function()
					local opts = { noremap = true, silent = true, buffer = true }
					vim.keymap.set("n", "<leader>sb", function()
						local start_line = vim.fn.search("^#\\+BEGIN_SRC", "bnW")
						local end_line = vim.fn.search("^#\\+END_SRC", "nW")
						if start_line > 0 and end_line > 0 then
							local lines = vim.api.nvim_buf_get_lines(0, start_line, end_line,
								false)
							require("iron.core").send(lines)
						else
							vim.notify("No source block found", vim.log.levels.WARN)
						end
					end, opts)
				end,
			})
		end,
	},
	{
		"stevearc/oil.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		cmd = { "Oil" },
		config = function()
			require("oil").setup({
				columns = {
					"icon",
					"permissions",
					"size",
					"mtime",
					"filename",
				},
				view_options = {
					show_hidden = true,
					is_real_file = true,
				},
				float = {
					win_options = {
						border = "rounded",
						winblend = 20,
					},
				},
			})
		end,
	},
	{
		"williamboman/mason.nvim",
		build = ":MasonUpdate",
		config = function()
			require("mason").setup()
		end,
	},
	{
		"williamboman/mason-lspconfig.nvim",
		dependencies = { "mason.nvim" },
		config = function()
			require("mason-lspconfig").setup {
				ensure_installed       = {
					"clangd",
					"html",
					"cssls",
					-- "glslls",
					"ts_ls",
					-- "pylsp",
					"lua_ls",
					"gopls",
				},
				automatic_installation = true,
				automatic_setup        = true,
				automatic_enable       = true,
			}
		end,
	},
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		dependencies = {
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-nvim-lua",
			"L3MON4D3/LuaSnip",
			"saadparwaiz1/cmp_luasnip",
		},
		config = function()
			local cmp = require("cmp")
			local luasnip = require("luasnip")

			cmp.setup {
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert {
					["<C-d>"] = cmp.mapping.scroll_docs(-4),
					["<C-f>"] = cmp.mapping.scroll_docs(4),
					["<C-Space>"] = cmp.mapping.complete(),
					["<CR>"] = cmp.mapping.confirm { select = true },
					["<Tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_next_item()
						elseif luasnip.expand_or_jumpable() then
							luasnip.expand_or_jump()
						else
							fallback()
						end
					end, { "i", "s" }),
					["<S-Tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_prev_item()
						elseif luasnip.jumpable(-1) then
							luasnip.jump(-1)
						else
							fallback()
						end
					end, { "i", "s" }),
				},
				sources = {
					{ name = "nvim_lsp" },
					{ name = "nvim_lua" },
					{ name = "buffer" },
					{ name = "path" },
					{ name = "luasnip" },
				},
			}
		end,
	},
	{
		"neovim/nvim-lspconfig",
		event = { "BufReadPre", "BufNewFile" },
	},
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		event = { "BufReadPost", "BufNewFile" },
		opts = {
			ensure_installed = { "vim", "lua", "vimdoc", "html", "css", "go", "wgsl", "rust" },
			highlight = { enable = true, additional_vim_regex_highlighting = false },
		},
		config = function(_, opts)
			require("nvim-treesitter.configs").setup(opts)
			vim.treesitter.language.register("rust", "punch")
		end,
	},
	{ "SidOfc/carbon.nvim",           event = "VeryLazy" },
	{
		"nvim-orgmode/orgmode",
		ft = { "org" },
		event = "VeryLazy",
		config = function()
			require("orgmode").setup {
				org_agenda_files = { "~/org/**/*" },
				org_default_notes_file = "~/org/refile.org",
			}
		end,
	},
	{
		"quarto-dev/quarto-nvim",
		dependencies = {
			"jmbuhr/otter.nvim",
			"nvim-treesitter/nvim-treesitter",
			"nvim-lua/plenary.nvim",
		},
		ft = { "quarto", "rmarkdown", "rmd" },
		config = function()
			require("quarto").setup({})
		end,
	},

	{
		"chrisgrieser/nvim-origami",
		event = "BufReadPost",
		config = function()
			local origami = require("origami")
			origami.remember = function()
				if vim.fn.bufname("%") ~= "" then
					local ok, err = pcall(vim.api.nvim_command, "mkview")
					if not ok then
						vim.notify("Error saving view: " .. err, vim.log.levels.ERROR)
					end
				end
			end
		end,
	},
	{
		"jghauser/fold-cycle.nvim",
		keys = {
			{
				"^",
				function() require("fold-cycle").close() end,
				desc = "Cycle-Close Folds",
			},
			{
				"\\",
				function() require("fold-cycle").open() end,
				desc = "Cycle-Open Folds",
			},
		},
	},
	{
		"kevinhwang91/nvim-ufo",
		dependencies = { "kevinhwang91/promise-async" },
		event = "BufReadPost",
		init = function()
			vim.opt.foldlevel = 99
			vim.opt.foldlevelstart = 99
		end,
		keys = {
			{ "zr", function() require("ufo").openFoldsExceptKinds { "comment" } end, desc = "Open all folds except comments" },
			{ "zm", function() require("ufo").closeAllFolds() end,                    desc = "Close all folds" },
			{ "z1", function() require("ufo").closeFoldsWith(1) end,                  desc = "Close L1 folds" },
			{ "z2", function() require("ufo").closeFoldsWith(2) end,                  desc = "Close L2 folds" },
			{ "z3", function() require("ufo").closeFoldsWith(3) end,                  desc = "Close L3 folds" },
			{ "z4", function() require("ufo").closeFoldsWith(4) end,                  desc = "Close L4 folds" },
		},
		config = function()
			local foldIcon = ""
			local hlgroup = "NonText"
			local function foldTextFormatter(virtText, lnum, endLnum, width, truncate)
				if not vim.api.nvim_buf_is_valid(0) or vim.api.nvim_buf_get_option(0, "bufhidden") == "wipe" then
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

			require("ufo").setup {
				provider_selector = function(_, ft, _)
					local lspNoFold = { "markdown", "bash", "sh", "zsh", "css", "html", "python" }
					if vim.tbl_contains(lspNoFold, ft) then
						return { "treesitter", "indent" }
					end
					return { "lsp", "indent" }
				end,
				close_fold_kinds_for_ft = { ["*"] = { "imports" } },
				open_fold_hl_timeout = 500,
				fold_virt_text_handler = foldTextFormatter,
			}
		end,
	},
	{ "nvim-lua/plenary.nvim",        lazy = true },
	{ "ludovicchabant/vim-gutentags", lazy = true },
	{ "nvim-tree/nvim-web-devicons",  lazy = true },
	{
		"akinsho/bufferline.nvim",
		version = "v3.*",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {
			options = {
				numbers = "buffer_id",
				close_command = "bdelete! %d",
				right_mouse_command = "bdelete! %d",
				left_mouse_command = "buffer %d",
				middle_mouse_command = nil,
				indicator = { style = "underline" },
				buffer_close_icon = "",
				modified_icon = "●",
				close_icon = "",
				left_trunc_marker = "",
				right_trunc_marker = "",
				max_name_length = 18,
				max_prefix_length = 15,
				tab_size = 18,
				diagnostics = "nvim_lsp",
				offsets = {
					{
						filetype = "NvimTree",
						text = "File Explorer",
						text_align = "center",
						padding = 1,
					},
				},
				show_buffer_icons = true,
				show_buffer_close_icons = true,
				show_close_icon = false,
				show_tab_indicators = true,
				enforce_regular_tabs = false,
				always_show_bufferline = true,
				separator_style = "slant",
				view = "multiwindow",
			},
		},
	},
	{
		"numToStr/Comment.nvim",
	},
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		},
		cmd = "Telescope",
		config = function()
			require("telescope").setup {
				pickers = {
					live_grep = {},
					buffers = {
						sort_mru = true,
						ignore_current_buffer = true,
						previewer = false,
					},
				},
			}
			pcall(require("telescope").load_extension, "fzf")
		end,
	},
	{
		"github/copilot.vim",
		lazy = false,
		config = function()
			vim.g.copilot_enabled = false
		end,
	},
	{
		"CopilotC-Nvim/CopilotChat.nvim",
		dependencies = {
			"github/copilot.vim",
			"nvim-lua/plenary.nvim",
		},
		build = "make tiktoken",
		opts = {},
		event = "VeryLazy",
	}
}, {
	defaults = { lazy = true },
	install = { colorscheme = { "catppuccin" } },
	ui = {
		icons = {
			ft = "",
			lazy = "󰂠 ",
			loaded = "",
			not_loaded = "",
		},
	},
	performance = {
		rtp = {
			disabled_plugins = {
				"2html_plugin", "tohtml", "getscript", "getscriptPlugin", "gzip", "logipat", "netrw",
				"netrwPlugin", "netrwSettings", "netrwFileHandlers", "matchit", "tar", "tarPlugin",
				"rrhelper", "spellfile_plugin", "vimball", "vimballPlugin", "zip", "zipPlugin", "tutor",
				"rplugin", "syntax", "synmenu", "optwin", "compiler", "bugreport", "ftplugin",
			},
		},
	},
})
