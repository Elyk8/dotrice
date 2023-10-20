-- Additional Plugins
lvim.plugins = {
  "Mofiqul/vscode.nvim",
  {
    "folke/todo-comments.nvim",
    event = "BufRead",
    config = function()
      require("todo-comments").setup()
    end,
  },
  "lervag/vimtex",
  {
    "ethanholz/nvim-lastplace",
    event = "BufRead",
    config = function()
      require "user.nvim-lastplace"
    end,
  },
  "pocco81/true-zen.nvim",
  "kylechui/nvim-surround",
  "kmonad/kmonad-vim",
  -- "sunaku/tmux-navigate",
  -- "karb94/neoscroll.nvim",
  "opalmay/vim-smoothie",
  "lvimuser/lsp-inlayhints.nvim",
  "NvChad/nvim-colorizer.lua",
  "nathom/filetype.nvim",
}
