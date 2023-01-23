-- Additional Plugins
lvim.plugins = {
  {
    "Mofiqul/vscode.nvim",
    config = function()
      require("vscode").setup {
        italic_comments = true,
      }
    end,
  },
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
  "ChristianChiarulli/nvim-ts-rainbow",
  -- "karb94/neoscroll.nvim",
  "opalmay/vim-smoothie",
  "lvimuser/lsp-inlayhints.nvim",
  "NvChad/nvim-colorizer.lua",
  "nathom/filetype.nvim",
}
