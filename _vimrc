set number  "This turns on line numbering
"set numberwidth=4       "Set the line numbers to 4 spaces
"set expandtab
"set shiftwidth=4
"set softtabstop=4
set smartindent
nnoremap <F2> :set invpaste paste?<CR>
vnoremap <C-c> :call system('xclip -sel c',@0)<CR>
set pastetoggle=<F2>
set showmode

function! InsertStatuslineColor(mode)
  if a:mode == 'i'
    hi statusline guibg=Blue ctermfg=4 guifg=Black ctermbg=0
  elseif a:mode == 'R'
    hi statusline guibg=Purple ctermfg=5 guifg=Black ctermbg=0
  else
    hi statusline guibg=DarkRed ctermfg=1 guifg=Black ctermbg=0
  endif
endfunction
"
au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi statusline guibg=Black ctermfg=0 guifg=DarkGray ctermbg=Black

autocmd VimEnter * hi statusline guibg=Black ctermfg=0 guifg=DarkGray ctermbg=Black
set laststatus=2
set statusline=%f%m%r%h\ [%L]\ [%{&ff}]\ %y%=[%p%%]\ [%05l,%c-%v]

"set statusline=\ %F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c


"augroup coloring
"    autocmd VimEnter * hi NormalColor guifg=Black guibg=Green ctermbg=46    ctermfg=0
"    autocmd VimEnter * hi InsertColor guifg=Black guibg=Cyan ctermbg=51 ctermfg=0
"    autocmd VimEnter * hi ReplaceColor guifg=Black guibg=maroon1 ctermbg=165 ctermfg=0
"    autocmd VimEnter * hi VisualColor guifg=Black guibg=Orange ctermbg=202 ctermfg=0
"augroup END
"
"set laststatus=2
"set statusline=
"set statusline+=%#NormalColor#%{(mode()=='n')?'\ \ NORMAL\ ':''}
"set statusline+=%#InsertColor#%{(mode()=='i')?'\ \ INSERT\ ':''}
"set statusline+=%#ReplaceColor#%{(mode()=='R')?'\ \ RPLACE\ ':''}
"set statusline+=%#VisualColor#%{(mode()=='v')?'\ \ VISUAL\ ':''}

"set cursorline

nnoremap <F5> "=strftime("%Y-%m-%d %T")<CR>P
inoremap <F5> <C-R>=strftime("%Y-%m-%d %T")<CR>

set nocompatible
syntax on
set nowrap
set autoread
"set nobackup
"set nowritebackup
set noswapfile
set backspace=indent,eol,start
set ruler
set noeb
set vb
set t_vb= 
if has('autocmd')
        autocmd GUIEnter * set vb t_vb=
endif

set ignorecase
set hlsearch
"set relativenumber

filetype plugin indent on
let vimclojure#HighlightBuiltins = 1

if v:progname =~? "gvim"
        set guifont=AR\ PL\ UMing\ TW\ Light\ 11
        set guioptions=agimrLtT
        unmenu ToolBar
        unmenu! ToolBar
endif

"set guifont=DejaVu\ Sans\ Mono\ 12
set guifont=ProFontWindows\ 14

"colorscheme osx_like
"colorscheme desert
colorscheme peachpuff

set diffopt+=internal,algorithm:patience
