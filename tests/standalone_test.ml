(**pp -syntax camlp5o -ppopt -pa_ppx_regexp-nostatic *)
open OUnit2

let _ = [%match "abc"]
let _ = [%match "abc"/pcre]
let _ = [%split "a"]
let _ = [%split "a"/pcre]
let _ = [%subst ".+" / {|<<$0>>|}]
let _ = [%subst ".+" / {|<<$0>>|}/pcre]
