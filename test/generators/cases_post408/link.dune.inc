(rule
 (target labels.cmti)
 (deps cases/labels.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target labels.odoc)
 (deps labels.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target labels.odocl)
 (deps labels.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target recent.cmti)
 (deps cases/recent.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target recent.odoc)
 (deps recent.cmti)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target recent.odocl)
 (deps recent.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(rule
 (target recent_impl.cmt)
 (deps cases/recent_impl.ml)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target recent_impl.odoc)
 (deps recent_impl.cmt)
 (action
  (run odoc compile -o %{target} %{deps})))

(rule
 (target recent_impl.odocl)
 (deps recent_impl.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    labels.gen
    (run odoc html-targets -o . %{dep:../labels.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/labels.targets html/labels.gen))
 (enabled_if
  (< %{ocaml_version} 4.08)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    recent.gen
    (run odoc html-targets -o . %{dep:../recent.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/recent.targets html/recent.gen))
 (enabled_if
  (< %{ocaml_version} 4.08)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    recent_impl.gen
    (run odoc html-targets -o . %{dep:../recent_impl.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/recent_impl.targets html/recent_impl.gen))
 (enabled_if
  (< %{ocaml_version} 4.08)))
