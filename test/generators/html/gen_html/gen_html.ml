let html_target_rule odocl targets : Gen_backend.sexp =
  List
    [
      Atom "rule";
      List
        [
          Atom "action";
          List
            (Atom "progn"
             ::
             List
               [
                 Atom "run";
                 Atom "odoc";
                 Atom "html-generate";
                 Atom "--indent";
                 Atom "-o";
                 Atom "html.gen";
                 Atom ("%{dep:" ^ Fpath.to_string odocl ^ "}");
               ]
             :: Gen_backend.gen_targets targets);
        ];
    ]

let () =
  let stanzas =
    Gen_backend.gen_backend_rules "html" html_target_rule Gen_backend.files
  in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
