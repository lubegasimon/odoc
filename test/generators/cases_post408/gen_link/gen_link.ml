let () =
  let stanzas = Gen_link_lib.gen_rule "4.08" in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
