local status_ok, filetype = pcall(require, "filetype")
if not status_ok then
  return
end

-- In init.lua or filetype.nvim's config file
filetype.setup {
  overrides = {
    -- The same as the ones above except the keys map to functions
    function_extensions = {
      ["cpp"] = function()
        vim.bo.filetype = "cpp"
        -- Remove annoying indent jumping
        vim.bo.cinoptions = vim.bo.cinoptions .. "L0"
      end,
      ["pdf"] = function()
        vim.bo.filetype = "pdf"
        vim.fn.jobstart("zathura " .. '"' .. vim.fn.expand "%" .. '"')
      end,
    },
    function_literal = {
      Brewfile = function()
        vim.cmd "syntax off"
      end,
      sxhkdrc = function()
        vim.bo.filetype = "sxhkdrc"
      end,
    },
    function_complex = {
      [".*/i3/config"] = function()
        vim.bo.filetype = "i3config"
      end,
    },

    -- shebang = {
    --     -- Set the filetype of files with a dash shebang to sh
    --     dash = "sh",
    -- },
  },
}
