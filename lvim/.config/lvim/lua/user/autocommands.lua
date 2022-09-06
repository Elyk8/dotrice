local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup
ElykGroup = augroup("ElykGroup", {})

-- vim.api.nvim_create_autocmd("BufEnter", {
--   pattern = { "*.json", "*.jsonc" },
--   -- enable wrap mode for json files only
--   command = "setlocal wrap",
-- })
autocmd("FileType", {
  group = ElykGroup,
  pattern = "zsh",
  callback = function()
    -- let treesitter use bash highlight for zsh files as well
    require("nvim-treesitter.highlight").attach(0, "bash")
  end,
})

