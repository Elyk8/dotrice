---remove a server from the skipped list, e.g. eslint, or emmet_ls. !!Requires `:LvimCacheReset` to take effect!!
---`:LvimInfo` lists which server(s) are skipped for the current filetype
lvim.lsp.automatic_configuration.skipped_servers = vim.tbl_filter(function(server)
  return server ~= "ccls"
end, lvim.lsp.automatic_configuration.skipped_servers)

local formatters = require "lvim.lsp.null-ls.formatters"
formatters.setup {
  { command = "uncrustify", filetypes = { "c", "cpp" } },
}

-- Set a linter.
local linters = require "lvim.lsp.null-ls.linters"
linters.setup {
  { command = "cppcheck", filetypes = { "c", "cpp" } },
}

local lsp_manager = require "lvim.lsp.manager"
lsp_manager.setup("ccls", {
  filetypes = { "c", "cpp" },
})
