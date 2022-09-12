vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.spell = true

-- Shorten function name
local keymap = vim.keymap.set
-- Silent keymap option
local opts = { silent = true }
keymap("i", "<C-i>",
  [[<Esc>: silent exec '.!inkscape-figures create "'.getline('.').'" "'.b:vimtex.root.'/figures/"'<CR><CR>:w<CR>]], opts)
keymap("n", "<C-i>",
  [[<cmd> silent exec '!inkscape-figures edit "'.b:vimtex.root.'/figures/" > /dev/null 2>&1 &'<CR><CR>:redraw!<CR>]],
  opts)

lvim.builtin.which_key.mappings["m"] = {
  name = "LaTex",
  c = { "<cmd>VimtexCompile<cr>", "Toggle Compilation Mode" },
  C = { "<cmd>VimtexClean<cr>", "Clean build files" },
  e = { "<cmd>VimtexErrors<cr>", "Show Errors" },
  f = { "<cmd>call vimtex#fzf#run()<cr>", "Fzf Find" },
  i = { "<cmd>VimtexInfo<cr>", "Project Information" },
  s = { "<cmd>VimtexStop<cr>", "Stop Project Compilation" },
  t = { "<cmd>VimtexTocToggle<cr>", "Toggle Table Of Content" },
  v = { "<cmd>VimtexView<cr>", "View PDF" },
}
