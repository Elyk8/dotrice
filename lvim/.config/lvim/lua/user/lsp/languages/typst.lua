local lsp_manager = require "lvim.lsp.manager"
lsp_manager.setup("typst", {
  filetypes = { "typst" },
  on_init = require("lvim.lsp").common_on_init,
  capabilities = require("lvim.lsp").common_capabilities(),
})
