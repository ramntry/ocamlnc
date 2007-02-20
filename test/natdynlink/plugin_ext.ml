external fact: int -> int = "factorial"

let () =
  Api.reg_mod "plugin_ext";
  Printf.printf "fact 10 = %i\n" (fact 10)
