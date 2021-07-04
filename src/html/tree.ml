module Html = Tyxml.Html

type uri = Absolute of string | Relative of Odoc_document.Url.Path.t option

let page_creator ?(theme_uri = Relative None) ?(support_uri = Relative None)
    ~url name header toc content =
  let path = Link.Path.for_printing url in

  let head : Html_types.head Html.elt =
    let title_string = Printf.sprintf "%s (%s)" name (String.concat "." path) in

    let file_uri base file =
      match base with
      | Absolute uri -> uri ^ "/" ^ file
      | Relative uri ->
          let page =
            Odoc_document.Url.Path.{ kind = `File; parent = uri; name = file }
          in
          Link.href ~resolve:(Current url) (Odoc_document.Url.from_path page)
    in

    let odoc_css_uri = file_uri theme_uri "odoc.css" in
    let highlight_js_uri = file_uri support_uri "highlight.pack.js" in

    Html.head
      (Html.title (Html.txt title_string))
      [
        Html.link ~rel:[ `Stylesheet ] ~href:odoc_css_uri ();
        Html.meta ~a:[ Html.a_charset "utf-8" ] ();
        Html.meta
          ~a:[ Html.a_name "generator"; Html.a_content "odoc %%VERSION%%" ]
          ();
        Html.meta
          ~a:
            [
              Html.a_name "viewport";
              Html.a_content "width=device-width,initial-scale=1.0";
            ]
          ();
        Html.script ~a:[ Html.a_src highlight_js_uri ] (Html.txt "");
        Html.script (Html.txt "hljs.initHighlightingOnLoad();");
      ]
  in

  let breadcrumbs =
    let rec get_parents x =
      match x with
      | [] -> []
      | x :: xs -> (
          match Odoc_document.Url.Path.of_list (List.rev (x :: xs)) with
          | Some x -> x :: get_parents xs
          | None -> get_parents xs)
    in
    let parents =
      get_parents (List.rev (Odoc_document.Url.Path.to_list url)) |> List.rev
    in
    let has_parent = List.length parents > 1 in
    let href page =
      Link.href ~resolve:(Current url) (Odoc_document.Url.from_path page)
    in
    if has_parent then
      let up_url = List.hd (List.tl (List.rev parents)) in
      let l =
        [
          Html.a ~a:[ Html.a_href (href up_url) ] [ Html.txt "Up" ];
          Html.txt " – ";
        ]
        @
        (* Create breadcrumbs *)
        let space = Html.txt " " in
        parents
        |> Utils.list_concat_map
             ?sep:(Some [ space; Html.entity "#x00BB"; space ])
             ~f:(fun url' ->
               [
                 [
                   (if url = url' then Html.txt url.name
                   else
                     Html.a
                       ~a:[ Html.a_href (href url') ]
                       [ Html.txt url'.name ]);
                 ];
               ])
        |> List.flatten
      in
      [ Html.nav ~a:[ Html.a_class [ "odoc-nav" ] ] l ]
    else []
  in

  let body =
    breadcrumbs
    @ [ Html.header ~a:[ Html.a_class [ "odoc-preamble" ] ] header ]
    @ toc
    @ [ Html.div ~a:[ Html.a_class [ "odoc-content" ] ] content ]
  in
  Html.html head (Html.body ~a:[ Html.a_class [ "odoc" ] ] body)

let make ?theme_uri ?support_uri ~indent ~url ~header ~toc title content
    children =
  let filename = Link.Path.as_filename url in
  let html =
    page_creator ?theme_uri ?support_uri ~url title header toc content
  in
  let content ppf = (Html.pp ~indent ()) ppf html in
  { Odoc_document.Renderer.filename; content; children }

let open_details = ref true
