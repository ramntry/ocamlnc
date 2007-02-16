let ()  =
  Natdynlink.init ();
  for i = 1 to Array.length Sys.argv - 1 do
    let name = Sys.argv.(i) in
    Printf.eprintf "Loading %s\n" name;
    try Natdynlink.loadfile name
    with
      | Natdynlink.Error err ->
	  Printf.eprintf "Natdynlink error: %s\n" 
	    (Natdynlink.error_message err)
      | exn ->
	  Printf.eprintf "Error: %s\n" (Printexc.to_string exn)
  done

