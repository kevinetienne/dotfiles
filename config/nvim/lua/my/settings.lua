vim.g.mapleader = ' '
vim.opt.number = true
vim.opt.cursorline = true
vim.opt.scrolloff = 3
vim.opt.mouse = 'a'
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4

-- set t_ut=
-- 
-- " Enable true color 启用终端24位色
-- let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
-- let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
-- set termguicolors




-- completeopt is used to manage code suggestions
-- menuone: show popup even when there is only one suggestion
-- noinsert: Only insert text when selection is confirmed
-- noselect: force us to select one from the suggestions
vim.opt.completeopt = { 'menu', 'menuone', 'noselect' } --, 'noinsert', 'preview' }
-- shortmess is used to avoid excessive messages
vim.opt.shortmess = vim.opt.shortmess + { c = true }

vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

function org_imports()
	local clients = vim.lsp.buf_get_clients()
	for _, client in pairs(clients) do
		local params = vim.lsp.util.make_range_params(nil, client.offset_encoding)
		params.context = { only = { "source.organizeImports" } }

		local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 5000)
		for _, res in pairs(result or {}) do
			for _, r in pairs(res.result or {}) do
				if r.edit then
					vim.lsp.util.apply_workspace_edit(r.edit, client.offset_encoding)
				else
					vim.lsp.buf.execute_command(r.command)
				end
			end
		end
	end
end

autocmd('FileType', {
	pattern = { 'html', 'lua', 'yaml' },
	command = 'setlocal shiftwidth=2 tabstop=2'
})

autocmd('BufWritePre', {
	pattern = { '*' },
	command = 'lua vim.lsp.buf.format()'
})

autocmd("BufWritePre", {
	pattern = { "*.go" },
	callback = org_imports,
})

vim.diagnostic.config({ virtual_text = false })

vim.api.nvim_set_keymap('i', 'jj', '<ESC>', { noremap = true })
vim.api.nvim_set_keymap('i', 'jk', '<ESC>', { noremap = true })
vim.api.nvim_set_keymap('n', ';', ':', { noremap = true })
