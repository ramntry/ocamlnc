
type pers_flags = Rectypes

type cmi_file = {
  cmi_name : string;
  cmi_sign : Types.signature_item list;
  cmi_crcs : (string * Digest.t) list;
  cmi_flags : pers_flags list;
}

exception No_such_magic

let input_cmi_file ic magic =
    if magic <> Config.cmi_magic_number then 
      raise No_such_magic 
    else
    let (cmi_name, cmi_sign) = (input_value ic : string *  Types.signature_item list) in
    let cmi_crcs = (input_value ic : (string * Digest.t) list) in
    let cmi_flags = (input_value ic : pers_flags list) in
      { cmi_name ; cmi_sign; cmi_crcs; cmi_flags }
