(**pp -syntax camlp5o *)
open OUnit2

let _ = [%match "abc" / dynamic]
let _ = [%match "abc"/pcre2 dynamic]
let _ = [%split "a"/  dynamic]
let _ = [%split "a"/pcre2 dynamic]
let _ = [%subst ".+" / {|<<$0>>|} / dynamic]
let _ = [%subst ".+" / {|<<$0>>|}/pcre2 dynamic]
