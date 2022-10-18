-- Use which-key to add extra bindings with the leader-key prefix
lvim.builtin.which_key.mappings["b"] = { "<cmd>Telescope buffers<cr>", "Buffers" }
lvim.builtin.which_key.mappings["w"] = nil
lvim.builtin.which_key.mappings["P"] = { "<cmd>Telescope projects<CR>", "Projects" }
lvim.builtin.which_key.mappings["f"] = {
  name = "Files",
  f = { require("lvim.core.telescope.custom-finders").find_project_files, "Find File" },
  c = { "<cmd>e ~/.config/lvim/config.lua<CR>", "Open config.lua" },
  r = { "<cmd>Telescope oldfiles<cr>", "Recent File" },
  s = { "<cmd>w<CR>", "Save File" },
  S = { "<cmd>wa<CR>", "Save All" },
}

lvim.builtin.which_key.mappings["t"] = {
  name = "Toggle",
  z = { "<cmd>TZNarrow<CR>", "Zen Mode" },
}
-- lvim.builtin.which_key.mappings["t"] = {
--   name = "+Trouble",
--   r = { "<cmd>Trouble lsp_references<cr>", "References" },
--   f = { "<cmd>Trouble lsp_definitions<cr>", "Definitions" },
--   d = { "<cmd>Trouble document_diagnostics<cr>", "Diagnostics" },
--   q = { "<cmd>Trouble quickfix<cr>", "QuickFix" },
--   l = { "<cmd>Trouble loclist<cr>", "LocationList" },
--   w = { "<cmd>Trouble workspace_diagnostics<cr>", "Workspace Diagnostics" },
-- }
