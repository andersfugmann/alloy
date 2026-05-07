open! Base
open! Stdio
open Js_of_ocaml

let ( let* ) = Lwt.bind

(* -- DOM elements -- *)

let pattern_input = Page_util.input_by_id "pattern"
let tenant_select = Page_util.select_by_id "tenant"
let error_div = Page_util.get_by_id "error"

(* -- Pre-fill pattern from URL params (set by context menu) -- *)

let () =
  match Page_util.get_search_param "url" with
  | None -> ()
  | Some url ->
    let origin = Page_util.url_origin url in
    (match origin with
     | Some o ->
       let pattern =
         Page_util.escape_regexp o ^ "/.*"
         |> String.substr_replace_first ~pattern:"https:" ~with_:"https?:"
         |> String.substr_replace_first ~pattern:"http:" ~with_:"https?:"
       in
       pattern_input##.value := Js.string pattern
     | None -> ())

(* -- Entry point -- *)

let () =
  Page_util.connect_port ~name:"add_rule"
    ~on_ready:(fun conn ->
      (* Fetch tenants from config *)
      Lwt.async (fun () ->
        let* result = Client.call conn Get_config () in
        begin match result with
        | Ok cfg ->
          begin match List.is_empty cfg.tenants with
          | true ->
            Page_util.set_html
              (tenant_select :> Dom_html.element Js.t)
              {|<option value="">No tenants</option>|}
          | false ->
            Page_util.set_html (tenant_select :> Dom_html.element Js.t) "";
            let doc = Dom_html.document in
            List.iter cfg.tenants ~f:(fun (name, tc) ->
              let label =
                match String.is_empty tc.Protocol.label with
                | true -> name
                | false -> tc.label
              in
              let opt =
                Page_util.create_option doc ~value:name ~text:label
                  ~selected:false
              in
              Dom.appendChild tenant_select opt)
          end
        | Error _ ->
          Page_util.set_html
            (tenant_select :> Dom_html.element Js.t)
            {|<option value="">Failed to load</option>|}
        end;
        Lwt.return_unit);

      (* Cancel button *)
      Page_util.on_click (Page_util.get_by_id "btnCancel") (fun () ->
        Dom_html.window##close);

      (* Save button *)
      Page_util.on_click (Page_util.get_by_id "btnSave") (fun () ->
        let pattern = Js.to_string pattern_input##.value |> String.strip in
        let tenant = Js.to_string tenant_select##.value in
        Page_util.set_text error_div "";

        match String.is_empty pattern with
        | true -> Page_util.set_text error_div "Pattern is required."
        | false ->
          match String.is_empty tenant with
          | true -> Page_util.set_text error_div "Select a tenant."
          | false ->
            match Page_util.validate_regexp pattern with
            | Error msg ->
              Page_util.set_text error_div
                (Printf.sprintf "Invalid regex: %s" msg)
            | Ok () ->
              let new_rule : Protocol.rule =
                { pattern; target = tenant; enabled = true }
              in
              Lwt.async (fun () ->
                let* rules_result = Client.call conn Get_rules () in
                begin match rules_result with
                | Ok existing ->
                  let updated = existing @ [ new_rule ] in
                  let* set_result = Client.call conn Set_rules updated in
                  begin match set_result with
                  | Ok () ->
                    Dom_html.window##close;
                    Lwt.return_unit
                  | Error msg ->
                    Page_util.set_text error_div
                      (Printf.sprintf "Error: %s" msg);
                    Lwt.return_unit
                  end
                | Error msg ->
                  Page_util.set_text error_div
                    (Printf.sprintf "Error fetching rules: %s" msg);
                  Lwt.return_unit
                end)))
    ~on_event:(fun _ev -> ())
