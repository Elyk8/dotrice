-- Use which-key to add extra bindings with the leader-key prefix
lvim.builtin.which_key.mappings["b"] = { "<cmd>Telescope buffers<cr>", "Buffers" }
lvim.builtin.which_key.mappings["w"] = {
  name = "Windows",
  v = { "<cmd>vsp<cr>", "Vertical Split" },
  s = { "<cmd>sp<cr>", "Split" },
}
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
