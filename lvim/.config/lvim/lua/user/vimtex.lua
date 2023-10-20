-- Viewer options: One may configure the viewer either by specifying a built-in
-- viewer method:
vim.g.vimtex_view_method = "zathura_simple"
vim.g.vimtex_callback_progpath = "lvim" --  This is because we are using Lunarvim as wrapper
vim.g.vimtex_fold_enabled = 0
vim.g.vimtex_quickfix_open_on_warning = 0
vim.g.vimtex_quickfix_autoclose_after_keystrokes = 5
vim.g.vimtex_syntax_enabled = 0
vim.g.vimtex_syntax_conceal_disable = 1

-- vim.g.vimtex_view_general_viewer = "zathura"
-- vim.g.vimtex_view_general_options = [[--synctex-forward %l:1:%f %p]]

-- VimTeX uses latexmk as the default compiler backend. If you use it, which is
-- strongly recommended, you probably don't need to configure anything. If you
-- want another compiler backend, you can change it as follows. The list of
-- supported backends and further explanation is provided in the documentation,
-- see ":help vimtex-compiler".
vim.g.vimtex_compiler_method = "tectonic"

-- Compile on initialization, cleanup on quit
vim.cmd [[
augroup vimtex_event_1
  au!
  au User VimtexEventQuit     call vimtex#compiler#clean(0)
augroup END
]]
