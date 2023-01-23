-- Keymappings [view all the defaults by pressing <leader>Lk]
lvim.leader = "space"
-- add your own keymapping
lvim.keys.normal_mode["<C-s>"] = ":w<cr>"
-- lvim.keys.normal_mode["<S-l>"] = ":BufferLineCycleNext<CR>"
-- lvim.keys.normal_mode["<S-h>"] = ":BufferLineCyclePrev<CR>"
-- unmap a default keymapping
vim.keymap.del("n", "<A-j>", {})
vim.keymap.del("n", "<A-k>", {})
-- override a default keymapping
-- lvim.keys.normal_mode["<C-q>"] = ":q<cr>" -- or vim.keymap.set("n", "<C-q>", ":q<cr>" )

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "J", "mzJ`z")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- Which-key --
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
  x = { "<cmd>!chmod +x %<CR>", "Chmod +x"}
}

lvim.builtin.which_key.mappings["t"] = {
  name = "Toggle",
  z = { "<cmd>TZNarrow<CR>", "Zen Mode" },
}
