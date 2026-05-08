if exists("b:current_syntax")
  finish
endif

" Clear existing syntax
syntax clear

" Comments
syntax match enfoldComment "--.*$" contains=@Spell
highlight link enfoldComment Comment

" String
syntax region enfoldString start=/"/ skip=/\\"/ end=/"/
highlight link enfoldString String

" Keywords
syntax keyword enfoldKeyword global globals effect multi list object do
highlight link enfoldKeyword Keyword

" Types
" syntax keyword enfoldType effect multi list object
" highlight link enfoldType Type

" Booleans
syntax keyword enfoldConstant true false
highlight link enfoldConstant Boolean

" Constants
syntax keyword enfoldConstant require
highlight link enfoldConstant Function

syntax match enfoldIdentifier /\.\w\+/
highlight link enfoldIdentifier @variable.member

" Numbers
syntax match enfoldNumber "\v(\d+\.\d*|\d*\.\d+|\d+)"
highlight link enfoldNumber Number

" Modifiers
syntax match enfoldDirective /#\%(noalign\|curry\)\>/
highlight link enfoldDirective PreProc

" Operators
syntax match enfoldOperator /:=/
syntax match enfoldOperator /=/
highlight link enfoldOperator Operator

" Delimiters
syntax match enfoldDelim /[()]/
syntax match enfoldDelim /,/
highlight link enfoldDelim Delimiter

let b:current_syntax = "enfold"
