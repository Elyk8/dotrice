local opts = {
  -- char = "▏",
  filetype_exclude = {
    "alpha",
    "help",
    "terminal",
    "dashboard",
    "lspinfo",
    "lsp-installer",
    "mason",
  },

  buftype_exclude = { "terminal", "nofile" },

  char = "▏",
  show_trailing_blankline_indent = false,
  show_first_indent_level = true,
  use_treesitter = true,
  show_current_context = true,
  show_current_context_start = false,
  -- use_treesitter = false,
}

local status_ok, indent_blankline = pcall(require, "indent_blankline")
if not status_ok then
  return
end

indent_blankline.setup(opts)
