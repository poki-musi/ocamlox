open Batteries
module Vector = Batteries.DynArray

let bytes_of_int16 addr = (Char.chr (addr / 256), Char.chr (addr mod 256))
let rec apply_n f o n = if n <= 0 then o else apply_n f (f o) (pred n)
