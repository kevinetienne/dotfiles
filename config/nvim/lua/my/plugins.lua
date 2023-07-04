local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	{
		"folke/trouble.nvim",
		opts = {
			icons = false,
			fold_open = "v", -- icon used for open folds
			fold_closed = ">", -- icon used for closed folds
			indent_lines = false, -- add an indent guide below the fold icons
			signs = {
				-- icons / text used for a diagnostic
				error = "error",
				warning = "warn",
				hint = "hint",
				information = "info"
			},
			use_diagnostic_signs = false -- enabling this will use the signs defined in your lsp client
		},
		init = function()
			local opts = {silent = true, noremap = true}

			vim.keymap.set("n", "<leader>xx", "<cmd>TroubleToggle<cr>", opts)
			vim.keymap.set("n", "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", opts)
			vim.keymap.set("n", "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>", opts)
			vim.keymap.set("n", "<leader>xl", "<cmd>TroubleToggle loclist<cr>", opts)
			vim.keymap.set("n", "<leader>xq", "<cmd>TroubleToggle quickfix<cr>", opts)
			vim.keymap.set("n", "gR", "<cmd>TroubleToggle lsp_references<cr>", opts)
		end
	}, {
		'lunarvim/lunar.nvim',
		config = function()
		end
	}, {
		'dracula/vim',
	}, {
		'loctvl842/monokai-pro.nvim',
	}, {
		'navarasu/onedark.nvim',
		config = function()
			require('onedark').setup({
				style = 'warmer'
			})
			vim.cmd.colorscheme('onedark')
		end
	}, {
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			require('nvim-treesitter.configs').setup({
				"dockerfile",
				"go",
				"gomod",
				"gosum",
				"gowork",
				"html",
				"lua",
				"vim",
				"vimdoc",
				"python",
				"query",
				ensure_installed = {},
				sync_install = false,
				auto_install = true,
				highlight = {
					enable = true,
					additional_vim_regex_highlighting = false,
				}
			})
		end
	},
"RRethy/vim-illuminate",
	{ 'nvim-telescope/telescope-fzf-native.nvim', build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build' },
	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		keys = {
			{ "<leader>ff", "<cmd>Telescope find_files<CR>", desc = "Find Files" },
			{ "<leader>fg", "<cmd>Telescope live_grep<CR>", desc = "Grep" },
			{ "<leader>fb", "<cmd>Telescope buffers<CR>", desc = "Buffers" },
			{ "<leader>fc", "<cmd>Telescope colorscheme<CR>", desc = "Theme" },
		},
		config = function()
			require("telescope").setup({
				pickers = {
					colorscheme = {
						enable_preview = true
					}
				},
				extensions = {
					fzf = {
						fuzzy = true,
						override_generic_sorter = true,
						override_file_sorter = true,
						case_mode = "smart_case",
					}
				}
			})

			require('telescope').load_extension('fzf')
		end
	}, {

	},
	{
		"nvim-pack/nvim-spectre",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = function()
			require('spectre').setup({
				highlight = {
					ui = "String",
					search = "DiffChange",
					replace = "DiffDelete"
				},
			})
			vim.keymap.set('n', '<leader>S', '<cmd>lua require("spectre").open()<CR>', {
				desc = "Open Spectre"
			})
			vim.keymap.set('n', '<leader>sw', '<cmd>lua require("spectre").open_visual({select_word=true})<CR>', {
				desc = "Search current word"
			})
			vim.keymap.set('v', '<leader>sw', '<esc><cmd>lua require("spectre").open_visual()<CR>', {
				desc = "Search current word"
			})
			vim.keymap.set('n', '<leader>sp', '<cmd>lua require("spectre").open_file_search({select_word=true})<CR>', {
				desc = "Search on current file"
			})
		end
	},
	{
		"neovim/nvim-lspconfig",
		dependencies = { "nvim-lua/lsp-status.nvim" },
		version = false,

		config = function()
			local lspconfig = require('lspconfig')
			local lsp_status = require('lsp-status')
			lsp_status.register_progress()

			local capabilities = vim.lsp.protocol.make_client_capabilities()
			--local capabilities = require('cmp_nvim_lsp').default_capabilities()

			local on_attach = function(client, bufnr)
				local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
				local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

				buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

				local opts = { noremap = true, silent = true, buffer = bufnr }

				vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
				vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
				vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
				vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
				vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
				vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
				vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
				vim.keymap.set('n', '<leader>wl', function()
					print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
				end, opts)
				vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, opts)
				vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
				vim.keymap.set({ 'n', 'v' }, '<leader>ca', vim.lsp.buf.code_action, opts)
				vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
				vim.keymap.set('n', '<leader>f', function()
					vim.lsp.buf.format { async = true }
				end, opts)

				lsp_status.on_attach(client)
			end

			lspconfig.gopls.setup({
				cmd = { 'gopls' },
				on_attach = on_attach,
				capabilities = capabilities,
				settings = {
					gopls = {
						codelenses = {
							gc_details = true,
							run_govulncheck = true,
						},
						analyses = {
							nilness = true,
							shadow = true,
							unusedparams = true,
							unusedwrite = true,
							unusedvariable = true,
							useany = true,
						},
						staticcheck = true,
						vulncheck = "Imports",
						experimentalPostfixCompletions = true,
						hints = {
							assignVariableTypes = true,
							compositeLiteralFields = true,
							compositeLiteralTypes = true,
							constantValues = true,
							functionTypeParameters = true,
							parameterNames = true,
							rangeVariableTypes = true
						},
						gofumpt = true,
					},
				},
			})

			local configs = require('lspconfig/configs')

			if not configs.golangcilsp then

				configs.golangcilsp = {
					default_config = {
						cmd = { 'golangci-lint-langserver' },
						root_dir = lspconfig.util.root_pattern('.git', 'go.mod'),
						init_options = {
							command = { "golangci-lint", "run", "--enable-all", "--disable", "lll", "--out-format", "json",
								"--issues-exit-code=1" },
						}
					},
				}
			end

			local on_publish_diagnostics = vim.lsp.handlers["textDocument/publishDiagnostics"]

			lspconfig.golangci_lint_ls.setup({
				filetypes = { 'go', 'gomod' },
				handlers = {
					-- stops an out-of-range column error when viewing diagnostics with Trouble.nvim
					["textDocument/publishDiagnostics"] = function(_, result, ctx, config)
						for idx, diag in ipairs(result.diagnostics) do
							for position, value in pairs(diag.range) do
								if value.character == -1 then
									result.diagnostics[idx].range[position].character = 0
								end
							end
						end

						return on_publish_diagnostics(_, result, ctx, config)
					end,
				}
			})
		end
	}, {
	'hrsh7th/nvim-cmp',
	dependencies = {
		'hrsh7th/cmp-nvim-lsp',
		'hrsh7th/cmp-nvim-lua',
		'hrsh7th/cmp-path',
		'hrsh7th/cmp-buffer',
		'hrsh7th/cmp-nvim-lsp-signature-help',
		'hrsh7th/cmp-vsnip',
		'hrsh7th/vim-vsnip',
	},
	config = function()
		local cmp = require 'cmp'
		cmp.setup({

			snippet = {
				expand = function(args)
					vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
				end,
			},
			mapping = cmp.mapping.preset.insert({
				['<C-b>'] = cmp.mapping.scroll_docs(-4),
				['<C-f>'] = cmp.mapping.scroll_docs(4),
				['<C-Space>'] = cmp.mapping.complete(),
				['<C-e>'] = cmp.mapping.abort(),
				['<CR>'] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = false }),
			}),

			sources = cmp.config.sources({
				{ name = 'path' },
				{ name = 'nvim_lsp',               keyword_length = 1 },
				{ name = 'vsnip' },
				{ name = 'nvim_lsp_signature_help' },
				{ name = 'nvim_lua',               keyword_length = 2 },
			}, {
				{ name = 'buffer',                 keyword_length = 2 },
			}),

			window = {
			  completion = cmp.config.window.bordered(),
				documentation = cmp.config.window.bordered(),
			},

			formatting = {
				fields = { 'menu', 'abbr', 'kind' }
			},

		})
	end
}, {
  -- CODE ACTION LIGHTBULB
  "kosayoda/nvim-lightbulb",
  config = function()
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
      pattern = "*",
      command = "lua require('nvim-lightbulb').update_lightbulb()"
    })
  end
}, {
	'lewis6991/gitsigns.nvim',
	config = function()
		require('gitsigns').setup({
			current_line_blame = true
		})
	end
},{
	"lukas-reineke/indent-blankline.nvim",
	config = function()
		require("indent_blankline").setup({})
	end
}, {
	'nvim-lualine/lualine.nvim',
	config = function()
		require('lualine').setup({
			options = {
				component_separators = { left = '', right = ''},
				section_separators = { left = '', right = ''},
			}
		})
	end
}, {
	'akinsho/bufferline.nvim',
	version = "*",
	config = function()
		local bufferline = require('bufferline')
		bufferline.setup({
			options = {
				style_preset=bufferline.style_preset.minimal,
				indicator = {
					icon = '▎', -- this should be omitted if indicator style is not 'icon'
					style = 'icon',
				},
				modified_icon = 'M',
			},
		})
	end
}
})
