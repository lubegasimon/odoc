type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

let read_file_from_dir dir =
  let filenames =
    let arr = Sys.readdir dir in
    Array.sort String.compare arr;
    Array.to_list arr
  in
  let dir = Fpath.v dir in
  List.map (Fpath.( / ) dir) filenames

let cases = Fpath.v "cases"

let cases_paths = read_file_from_dir (Fpath.filename cases)

let odocl_files =
  Gen_backend.lines_of_command "dune exec compile "
    [ String.concat " " (List.map Fpath.to_string cases_paths) ]
  |> List.filter (fun f ->
         match String.split_on_char '.' f with
         | [ _; "odocl" ] -> true
         | _ :: _ -> false
         | [] -> false)

let set_odocl_ext = Fpath.set_ext ".odocl"

let set_odoc_ext = Fpath.set_ext ".odoc"

let path' () f = Filename.quote (Fpath.to_string f)

let ext' () f = Filename.quote (Fpath.get_ext f)

let is_dot_ocamlformat p = Fpath.filename p = ".ocamlformat"

let html, latex, man = ("html", "latex", "man")

let dune_inc, dune_inc_gen, gen_, exe =
  (".dune.inc", ".dune.inc.gen", "gen_", ".exe")

type backend = {
  subdir : Fpath.t;
  dune_inc : string;
  dune_inc_gen : string;
  dune_inc' : string;
  dune_inc_gen' : string;
  gen_exe : string;
}

let html =
  {
    subdir = Fpath.v html;
    dune_inc = html ^ dune_inc;
    dune_inc_gen = html ^ dune_inc_gen;
    dune_inc' = html ^ "/" ^ html ^ dune_inc;
    dune_inc_gen' = html ^ "/" ^ html ^ dune_inc_gen;
    gen_exe = gen_ ^ html ^ "/" ^ gen_ ^ html ^ exe;
  }

let latex =
  {
    subdir = Fpath.v latex;
    dune_inc = latex ^ dune_inc;
    dune_inc_gen = latex ^ dune_inc_gen;
    dune_inc' = latex ^ "/" ^ latex ^ dune_inc;
    dune_inc_gen' = latex ^ "/" ^ latex ^ dune_inc_gen;
    gen_exe = gen_ ^ latex ^ "/" ^ gen_ ^ latex ^ exe;
  }

let man =
  {
    subdir = Fpath.v man;
    dune_inc = man ^ dune_inc;
    dune_inc_gen = man ^ dune_inc_gen;
    dune_inc' = man ^ "/" ^ man ^ dune_inc;
    dune_inc_gen' = man ^ "/" ^ man ^ dune_inc_gen;
    gen_exe = gen_ ^ man ^ "/" ^ gen_ ^ man ^ exe;
  }

let backends = [ html; latex; man ]

let dep_atom p = Atom (Printf.sprintf "%%{dep:%s}" (Fpath.to_string p))

let odocls backend =
  List.map Fpath.v odocl_files
  |> List.map (fun p ->
         let path = Fpath.relativize ~root:backend p in
         match path with Some p -> dep_atom p | None -> assert false)

let gen_backend_diff_rule =
  List.map
    (fun b ->
      List
        [
          Atom "subdir";
          Atom (Fpath.to_string b.subdir);
          List
            [
              Atom "rule";
              List
                [
                  Atom "with-stdout-to";
                  Atom b.dune_inc_gen;
                  List
                    [
                      Atom "pipe-stdout";
                      List (Atom "run" :: Atom b.gen_exe :: odocls b.subdir);
                      List [ Atom "run"; Atom "dune"; Atom "format-dune-file" ];
                    ];
                ];
            ];
        ])
    backends

let diff_rules ocaml_ver =
  List.map
    (fun b ->
      List
        [
          Atom "rule";
          List [ Atom "alias"; Atom "runtest" ];
          List
            [
              Atom "action";
              List [ Atom "diff"; Atom b.dune_inc'; Atom b.dune_inc_gen' ];
            ];
          List
            [
              Atom "enabled_if";
              List [ Atom ">="; Atom "%{ocaml_version}"; Atom ocaml_ver ];
            ];
        ])
    backends

let gen_backend_rule ocaml_ver =
  [ gen_backend_diff_rule; diff_rules ocaml_ver ] |> List.flatten

let gen_rule ocaml_ver = List.flatten [ gen_backend_rule ocaml_ver ]
