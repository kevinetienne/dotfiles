set nocompatible               " be iMproved

 " set 256 colors
 set t_Co=256
 " font
 set guifont=Inconsolata:h18
 " colorscheme
 colorscheme wombat
 syntax on

 filetype off                   " required!

 " remove menu and toolbar
 set guioptions-=m
 set guioptions-=T

 set rtp+=~/.vim/bundle/vundle/
 call vundle#rc()

 " let Vundle manage Vundle
 " required! 
 Bundle 'gmarik/vundle'

 " My Bundles here:
 " python
 Bundle 'pep8' 
 Bundle 'fs111/pydoc.vim'
 Bundle 'lambdalisue/nose.vim'
 Bundle 'jmcantrell/vim-virtualenv'

 " CoffeeScript
 Bundle 'kchmck/vim-coffee-script'

 " JavaScript
 Bundle 'pangloss/vim-javascript'
 Bundle 'JavaScript-Indent'
 Bundle 'hallettj/jslint.vim'

 " Jade
 Bundle 'digitaltoad/vim-jade'

 " node.js
 Bundle 'jamescarr/snipmate-nodejs'
 Bundle 'guileen/vim-node'

 " code agnostic 
 Bundle 'msanders/snipmate.vim'
 Bundle 'tpope/vim-surround'
 Bundle 'sjl/gundo.vim'

 " original repos on github
 Bundle 'tpope/vim-fugitive'
 Bundle 'Lokaltog/vim-easymotion'
 Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
 Bundle 'tpope/vim-rails.git'

 " vim-scripts repos
 Bundle 'L9'
 Bundle 'FuzzyFinder'

 " Navigation 
 Bundle 'git://git.wincent.com/command-t.git'
 Bundle 'fholgado/minibufexpl.vim'
 Bundle 'scrooloose/nerdtree'
 Bundle 'ervandew/supertab'
 " ...

 filetype plugin indent on     " required! 
 "
 " Brief help
 " :BundleList          - list configured bundles
 " :BundleInstall(!)    - install(update) bundles
 " :BundleSearch(!) foo - search(or refresh cache first) for foo
 " :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
 "
 " see :h vundle for more details or wiki for FAQ
 " NOTE: comments after Bundle command are not allowed..
 
 let mapleader=','

 " ctrl-jklm  changes to that split
 map <c-j> <c-w>j
 map <c-k> <c-w>k
 map <c-l> <c-w>l
 map <c-h> <c-w>h
 
 " Open NerdTree
 map <leader>n :NERDTreeToggle<CR>
 " Open gundo
 map <leader>g :GundoToggle<CR>
 
 set number

 " show a line at column 79
 if exists("&colorcolumn")
     set colorcolumn=79
     endif
 
 """ Moving Around/Editing
 set cursorline              " have a line indicate the cursor location
 set ruler                   " show the cursor position all the time
 set nostartofline           " Avoid moving cursor to BOL when jumping around
 set virtualedit=block       " Let cursor move past the last char in <C-v> mode
 set scrolloff=3             " Keep 3 context lines above and below the cursor
 set backspace=2             " Allow backspacing over autoindent, EOL, and BOL
 set showmatch               " Briefly jump to a paren once it's balanced
 set nowrap                  " don't wrap text
 set linebreak               " don't wrap textin the middle of a word
 set autoindent              " always set autoindenting on
 set smartindent             " use smart indent if there is no indent file
 set tabstop=4               " <tab> inserts 4 spaces
 set shiftwidth=4            " but an indent level is 2 spaces wide.
 set softtabstop=4           " <BS> over an autoindent deletes both spaces.
 set expandtab               " Use spaces, not tabs, for autoindent/tab key.
 set shiftround              " rounds indent to a multiple of shiftwidth
 set matchpairs+=<:>         " show matching <> (html mainly) as well
 set foldmethod=indent       " allow us to fold on indents
 set foldlevel=99            " don't fold by default   

 """" Messages, Info, Status                                                    
 set ls=2                    " allways show status line                         
 set vb t_vb=                " Disable all bells.  I hate ringing/flashing.     
 set confirm                 " Y-N-C prompt if closing with unsaved changes.    
 set showcmd                 " Show incomplete normal mode commands as I type.  
 set report=0                " : commands always print changed line count.      
 set shortmess+=a            " Use [+]/[RO]/[w] for modified/readonly/written.  
 set ruler                   " Show some info, even without statuslines.        
 set laststatus=2            " Always show statusline, even if only 1 window.   
 set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ %{fugitive#statusline()}   

 " displays tabs with :set list & displays when a line runs off-screen          
 set listchars=tab:>-,eol:$,trail:-,precedes:<,extends:>                        
 set list                                                                       

 """ Searching and Patterns                                                     
 set ignorecase              " Default to using case insensitive searches,      
 set smartcase               " unless uppercase letters are used in the regex.                                                                            
 set smarttab                " Handle tabs more intelligently                   
 set hlsearch                " Highlight searches by default.                   
 set incsearch               " Incrementally search while typing a /regex 

 " minibufexpl
 let g:miniBufExplMapWindowNavVim = 1
 let g:miniBufExplMapWindowNavArrows = 1
 let g:miniBufExplMapCTabSwitchBufs = 1
 let g:miniBufExplModSelTarget = 1

 " omnicompletion
 """ Insert completion                                                                                                                                    
 " don't select first item, follow typing in autocomplete                       
 set completeopt=menuone,longest,preview                                        
 set pumheight=6             " Keep a small completion window 

 " python
 au BufRead *.py compiler nose                                              
 au FileType python set omnifunc=pythoncomplete#Complete                     
 au FileType python setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4 smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class,with
 au BufRead *.py set efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\
 " Don't let pyflakes use the quickfix window                                
 let g:pyflakes_use_quickfix = 0                                             
 let g:SuperTabDefaultCompletionType = "context"  

 autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
 autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
 autocmd FileType css set omnifunc=csscomplete#CompleteCSS

 " supertab
 let g:SuperTabDefaultCompletionType = "context"
