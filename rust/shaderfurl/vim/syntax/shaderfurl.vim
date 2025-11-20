if exists("b:current_syntax")
  finish
endif

syn clear

syn keyword furlKeyword import module where frag vert
syn keyword furlKeyword return discard continue break if then elif else for do proc
syn keyword furlKeyword struct attribute buffer uniform varying
hi link furlKeyword Keyword

syn match furlIdentifier /\v\h\w*/
hi link furlIdentifier @variable

syn match furlProperty /\v\.\h\w*/
hi link furlProperty @variable.member

syn match furlType /\v[A-Z]\w*/
syn keyword furlType f32 f64 i32 i64 u32 u64 bool sampler2D
hi link furlType Type

syn match furlOperator /\v\\\/|\/\\|\*|\/|\+|\-|\&|\||\^|\!|\~/
syn match furlOperator /\v\=|\=\=|\-\>|\<\=|\>\=|\<\<|\>\>|\<|\>/
hi link furlOperator Operator

syn match furlPunctuation /\v[\:\;\,\(\)\{\}\[\]\?]/
hi link furlPunctuation Operator

syntax match furlFloat /\v\-?\d*\.\d+/
highlight link furlFloat Float

syntax match furlInteger /\v\-?\d+/
highlight link furlInteger Number


syn region furlString start=/"/ end=/"/
hi link furlString String


syn region furlComment start=/\/\// end=/$/ contains=@Spell
hi link furlComment Comment

" let b:current_syntax = "shaderfurl"
