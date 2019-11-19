import { Type } from '@thi.ng/api'

export const uintType = (num: number) =>
  num <= 0x100 ? Type.U8 : num <= 0x10000 ? Type.U16 : Type.U32
