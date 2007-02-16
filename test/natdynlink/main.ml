let ()  =
  Natdynlink.init ();
  for i = 1 to Array.length Sys.argv - 1 do
    let name = Sys.argv.(i) in
    Printf.eprintf "Loading %s\n" name;
    try 
      if name.[0] = '-'
      then Natdynlink.loadfile_private 
	(String.sub name 1 (String.length name - 1))
      else Natdynlink.loadfile name
    with
      | Natdynlink.Error err ->
	  Printf.eprintf "Natdynlink error: %s\n" 
	    (Natdynlink.error_message err)
      | exn ->
	  Printf.eprintf "Error: %s\n" (Printexc.to_string exn)
  done


