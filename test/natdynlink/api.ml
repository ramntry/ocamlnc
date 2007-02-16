let mods = ref []

let reg_mod name =
  if List.mem name !mods then 
    Printf.printf "Reloading module %s\n" name
  else (
    mods := name :: !mods;
    Printf.printf "Registering module %s\n" name
  )
