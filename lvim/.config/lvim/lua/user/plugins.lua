-- Additional Plugins
lvim.plugins = {
  "lunarvim/darkplus.nvim",
  {
    "folke/todo-comments.nvim",
    event = "BufRead",
    config = function()
      require("todo-comments").setup()
    end,
  },
  {
    "aserowy/tmux.nvim",
    config = function()
      require "user.tmux"
    end,
  },
  {
    "lervag/vimtex",
    ft = { "tex", "latex", "bib" },
    config = function()
      require "user.vimtex"
    end,
  },
  { "p00f/nvim-ts-rainbow" },
  {
    "ethanholz/nvim-lastplace",
    event = "BufRead",
    config = function()
      require "user.nvim-lastplace"
    end,
  },
  {
    "Pocco81/true-zen.nvim",
    config = function()
      require "user.true-zen"
    end,
  },
  {
    "kylechui/nvim-surround",
    config = function()
      require "user.surround"
    end,
  },
  { "kmonad/kmonad-vim" },
}