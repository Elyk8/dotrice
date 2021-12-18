nmap <s-l> g$
nmap <s-h> g0
nmap j gj
nmap k gk
vmap j gj
vmap k gk

imap jk <Esc>
imap kj <Esc>

nmap Y y$

" Yank to system clipboard
set clipboard=unnamed

" Go back and forward with Ctrl+O and Ctrl+I
" (make sure to remove default Obsidian shortcuts for these to work)
"exmap back obcommand app:go-back
"nmap <C-o> :back
"exmap forward obcommand app:go-forward
"nmap <C-p> :forward
