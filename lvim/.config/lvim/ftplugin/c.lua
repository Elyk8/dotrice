local o = vim.opt_local

o.expandtab = false
o.ts = 4
o.tw = 4
o.sw = 4

require("lvim.lsp.manager").setup("ccls")

local notify = vim.notify

vim.notify = function(msg, ...)
    if msg:match("warning: multiple different client offset_encodings") then
        return
    end

    notify(msg, ...)
end
