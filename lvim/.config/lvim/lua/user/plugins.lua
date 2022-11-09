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
    "lervag/vimtex",
    ft = { "tex", "latex", "bib" },
    config = function()
      require "user.vimtex"
    end,
  },
  {
    "ethanholz/nvim-lastplace",
    event = "BufRead",
    config = function()
      require "user.nvim-lastplace"
    end,
  },
  "Pocco81/true-zen.nvim",
  "kylechui/nvim-surround",
  "kmonad/kmonad-vim",
  "aserowy/tmux.nvim",
  "p00f/nvim-ts-rainbow",
}
