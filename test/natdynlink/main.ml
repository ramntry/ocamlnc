let ()  =
  Natdynlink.init ();
  try 
    for i = 1 to Array.length Sys.argv - 1 do
      Natdynlink.loadfile Sys.argv.(i)
    done
  with Natdynlink.Error err ->
    Printf.eprintf "Natdynlink error: %s\n"
      (Natdynlink.error_message err)
