" Vim syntax
" Language: tlang
" Maintainer: memroytoco@gmail.com
" Latest Revision: 23 Jan 2023
"

if exists("b:current_syntax")
    finish
endif

" Keywords
" syn keyword syntaxElementKeyword keyword1 keyword2 nextgroup=syntaxElement2
" Matches
" syn match syntaxElementMatch 'regexp' contains=syntaxElement1
" nextgroup=syntaxElement2 skipwhite
" Regions
" syn region syntaxElementRegion start='x' end='y'

syn keyword SyntaxTodo contained TODO FIXME XXX NOTE WARN
syn match Comment "//.*$" contains=SyntaxTodo
syn keyword Keyword data type unsafe safe let module use foreign module in operator
syn match Keyword '\(->\|=>\|=\|:\|,\)'

syn match Number '\d\+'
syn match Number '\d\+\.\d*'

syn match Name '\(\a\|_\)\(\a\|\d\|_\)*'

syn region Block start="{" end="}" fold transparent
syn region Bracket start="\[" end="\]" fold transparent

syn region String start='"' end='"'

let b:current_syntax = "tlang"

hi def link SyntaxTodo Todo
hi def link Comment Comment
hi def link Block Statement
hi def link String Constant
hi def link Number Constant
