open! Base
open! Stdio

let (^^) = Stdlib.(^^)
let log fmt =
  printf ("[Alloy] " ^^ fmt ^^ "\n%!")
