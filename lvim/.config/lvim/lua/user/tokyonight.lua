local tokyonight_ok, tokyonight = pcall(require, "tokyonight")
if not tokyonight_ok then
	return
end

tokyonight.setup({
	-- your configuration comes here
	-- or leave it empty to use the default settings
	style = "night", -- The theme comes in three styles, `storm`, a darker variant `night` and `day`
	transparent = true, -- Enable this to disable setting the background color
	terminal_colors = true, -- Configure the colors used when opening a `:terminal` in Neovim                                                                                                  |
	styles = {
		comments = "italic",
		keywords = "italic",
		functions = "NONE",
		variables = "NONE",
		sidebars = "dark", -- style for sidebars, see below
		floats = "dark", -- style for floating windows
	},
	sidebars = { "qf", "help" }, -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`                                                      |
	day_brightness = 0.3, -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors                                                  |
	hide_inactive_statusline = false, -- Enabling this option, will hide inactive statuslines and replace them with a thin border instead. Should work with the standard **StatusLine** and **LuaLine**. |
	dim_inactive = true, -- dims inactive windows
	lualine_bold = true, -- When `true`, section headers in the lualine theme will be bold                                                                                                  |
})

-- vim.cmd "colorscheme tokyonight"
