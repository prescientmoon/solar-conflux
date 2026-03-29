#!/usr/bin/env -S sh -c 'odin run "$1" -file -o:none -debug -linker:mold' --
package tunnels

import "core:fmt"
import "core:strings"

Symbol :: enum {
  Space, Unknown,
  TU,    TL,  TD,  TR,
  TUL,   TLD, TDR, TRU,
  VU,    VL,  VD,  VR,
  VUL,   VLD, VDR, VRU,
  VLU,   VDL, VRD, VUR,
  UU,    UL,  UD,  UR,
  LH,    LV,  LD,  LU,
  Dot,
}

@rodata
SYMBOL_NEXT := [Symbol]Symbol{
  .Space = .Space, .Unknown = .Unknown,
  .TU    = .TL,    .TL      = .TD,  .TD  = .TR,  .TR  = .TU,
  .TUL   = .TLD,   .TLD     = .TDR, .TDR = .TRU, .TRU = .TUL,
  .VU    = .VL,    .VL      = .VD,  .VD  = .VR,  .VR  = .VU,
  .VUL   = .VLD,   .VLD     = .VDR, .VDR = .VRU, .VRU = .VUL,
  .VLU   = .VDL,   .VDL     = .VRD, .VRD = .VUR, .VUR = .VLU,
  .UU    = .UL,    .UL      = .UD,  .UD  = .UR,  .UR  = .UU,
  .LH    = .LV,    .LV      = .LH,  .LD  = .LU,  .LU  = .LD,
  .Dot   = .Dot,
}

@rodata
SYMBOL_V_MIRROR := [Symbol]Symbol{
  .Space = .Space, .Unknown = .Unknown,
  .TU    = .TU,    .TL      = .TR,  .TD  = .TD,  .TR  = .TL,
  .TUL   = .TRU,   .TLD     = .TDR, .TDR = .TLD, .TRU = .TUL,
  .VU    = .VU,    .VL      = .VR,  .VD  = .VD,  .VR  = .VL,
  .VUL   = .VUR,   .VLD     = .VRD, .VDR = .VDL, .VRU = .VLU,
  .VLU   = .VRU,   .VDL     = .VDR, .VRD = .VLD, .VUR = .VUL,
  .UU    = .UU,    .UL      = .UR,  .UD  = .UD,  .UR  = .UL,
  .LH    = .LH,    .LV      = .LV,  .LD  = .LU,  .LU  = .LD,
  .Dot   = .Dot,
}

// unknown jqvxz
@rodata
SYMBOL_ENGLISH := [Symbol]rune{
  .Space = ' ', .Unknown = '?',
  .TU    = 'P', .TL      = 'K', .TD  = 'M', .TR  = 'Y',
  .TUL   = 'D', .TLD     = 'R', .TDR = 'H', .TRU = 'B',
  .VU    = '7', .VL      = '6', .VD  = '4', .VR  = '3',
  .VUL   = 'N', .VLD     = 'S', .VDR = 'C', .VRU = 'L',
  .VLU   = 'F', .VDL     = '8', .VRD = '5', .VUR = 'T',
  .UU    = '0', .UL      = '9', .UD  = 'W', .UR  = 'G',
  .LH    = 'E', .LV      = 'O', .LD  = 'U', .LU  = 'I',
  .Dot   = 'A',
}

@rodata
SYMBOL_RUNES := [Symbol]rune{
  .Space = ' ', .Unknown = '?',
  .TU    = '△', .TL      = '◁', .TD  = '▽', .TR  = '▷',
  .TUL   = '◤', .TLD     = '◣', .TDR = '◢', .TRU = '◥',
  .VU    = '⌃', .VL      = '‹', .VD  = '⌄', .VR  = '›',
  .VUL   = '⦬', .VLD     = '⦩', .VDR = '⦯', .VRU = '⦪',
  .VLU   = '⦫', .VDL     = '⦮', .VRD = '⦨', .VUR = '⦭',
  .UU    = '∪', .UL      = '⊃', .UD  = '∩', .UR  = '⊂',
  .LH    = '─', .LV      = '│', .LD  = '∖', .LU  = '∕',
  .Dot   = '∙',
}

Text :: []Symbol

text_to_string :: proc(t: Text) -> string {
  builder := strings.builder_make_none(context.temp_allocator)

  for s in t {
    if s == .Space {
      strings.write_rune(&builder, '\n')
    } else {
      strings.write_rune(&builder, SYMBOL_RUNES[s])
      strings.write_rune(&builder, ' ')
    }
  }

  return strings.to_string(builder)
}

text_to_english :: proc(t: Text) -> string {
  builder := strings.builder_make_none(context.temp_allocator)

  for s in t {
    strings.write_rune(&builder, SYMBOL_ENGLISH[s])
  }

  return strings.to_string(builder)
}

english_to_text :: proc(en: string) -> Text {
  txt := make([dynamic]Symbol, context.temp_allocator)

  outer: for r in en {
    for s in Symbol {
      if SYMBOL_ENGLISH[s] == r {
        append(&txt, s)
        continue outer
      }
    }

    append(&txt, Symbol.Unknown)
  }

  return txt[:]
}

rotate_text :: proc(txt: Text) -> Text {
  out := make([dynamic]Symbol, 0, len(txt), context.temp_allocator)
  for s in txt do append(&out, SYMBOL_NEXT[s])
  return out[:]
}

mirror_text :: proc(txt: Text) -> Text {
  out := make([dynamic]Symbol, 0, len(txt), context.temp_allocator)
  for s in txt do append(&out, SYMBOL_V_MIRROR[s])
  return out[:]
}

main :: proc() {
  fire := english_to_text("POWER OF FIRE DESTROY ONE SMALL ROCK")
  leaf := english_to_text("POWER OF LEAF USE THE GATES")
  water := english_to_text("POWER OF WATER RETRACE YOUR STEPS ONCE")
  alphabet := english_to_text("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  fmt.println(text_to_string(alphabet))

  line := Text{
    .Dot, .UD, .TUL, .LV, .Dot, .VRD, .VUL, .VRU, .Dot, .TU, .LV, .VRD, .TLD,
    .Dot, .VRD, .LV, .Dot, .VRD, .VUL, .LD, .VRD, .VUL,
    .Space,
    .LV, .UU, .VDR, .VDL, .Dot,
    .Space,
    .LD, .VDR, .VDR, .LD, .LV, .VUL, .LD, .VRU, .VRD, .TLD, .LV, .VRD,
    .Space,
    .LD, .VDR, .VDR, .LH, .VLD, .VRU, .TD, .TRU,
    .Space,
    .LH, .VLU, .LV, .VLU, .VRU, .LD, .VLD, .UU, .Dot, .VUL,
    .Space,
    .VLD, .VRD, .LV, .LD, .TRU, .UL, .LH, .TU, .VRU,
    .Space,
    .TUL, .TU, .LD, .VLU, .VDR, .VDR, .LD, .TLD, .LV, .Dot, .VLU, .TDR, .LU,
    .LV, .TRU, .LV,
    .Space,
    .TRU, .LV, .UL, .Dot, .LU, .VDR, .VRU,
    .Space,
    .Dot, .TL, .LH, .UR, .LV, .TDR, .TUL,
    .Space,
    .TL, .TD, .Dot, .VRU, .VRU, .LU, .VDR, .UL, .Dot, .UL, .LH, .TU, .Dot,
    .Space,
    .VRU, .TRU, .VRD, .TLD, .LV, .TR, .LH, .LD, .VUL, .LH, .VRU, .VUL, .TR, .LH,
    .UL, .LV, .TUL, .LD, .VUL, .VRD, .LH, .VUL, .VDR, .Dot, .TD, .VRD, .TLD,
    .LV, .VUL, .VRU, .Dot, .TU, .LV
  }

  for i in 0..<4 {
    if i == 4 do line = mirror_text(line)
    line = rotate_text(line)
    fmt.println(text_to_english(line))
  }
}
