open! Base
open! Stdio
open Js_of_ocaml

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

(* -- Fetch tenants from config -- *)

let () =
  Page_util.send_protocol_command Get_config
    ~on_response:(fun result ->
      match result with
      | Ok (Ok_config cfg) ->
        (match List.is_empty cfg.tenants with
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
             Dom.appendChild tenant_select opt))
      | _ ->
        Page_util.set_html
          (tenant_select :> Dom_html.element Js.t)
          {|<option value="">Failed to load</option>|})

(* -- Cancel button -- *)

let () =
  Page_util.on_click (Page_util.get_by_id "btnCancel") (fun () ->
    Dom_html.window##close)

(* -- Save button -- *)

let () =
  Page_util.on_click (Page_util.get_by_id "btnSave") (fun () ->
    let pattern = Js.to_string pattern_input##.value |> String.strip in
    let tenant = Js.to_string tenant_select##.value in
    Page_util.set_text error_div "";

    match String.is_empty pattern with
    | true -> Page_util.set_text error_div "Pattern is required."
    | false ->
      (match String.is_empty tenant with
       | true -> Page_util.set_text error_div "Select a tenant."
       | false ->
         (match Page_util.validate_regexp pattern with
          | Error msg ->
            Page_util.set_text error_div
              (Printf.sprintf "Invalid regex: %s" msg)
          | Ok () ->
            let new_rule : Protocol.rule =
              { pattern; target = tenant; enabled = true }
            in
            Page_util.send_protocol_command Get_rules
              ~on_response:(fun result ->
                match result with
                | Ok (Ok_rules existing) ->
                  let updated = existing @ [ new_rule ] in
                  Page_util.send_protocol_command (Set_rules { rules = updated })
                    ~on_response:(fun result ->
                      match result with
                      | Ok Ok_unit -> Dom_html.window##close
                      | Ok (Err { message }) ->
                        Page_util.set_text error_div
                          (Printf.sprintf "Error: %s" message)
                      | Ok _ ->
                        Page_util.set_text error_div "Unexpected response"
                      | Error msg ->
                        Page_util.set_text error_div
                          (Printf.sprintf "Error: %s" msg))
                | Ok (Err { message }) ->
                  Page_util.set_text error_div
                    (Printf.sprintf "Error fetching rules: %s" message)
                | _ ->
                  Page_util.set_text error_div "Failed to fetch current rules"))))
