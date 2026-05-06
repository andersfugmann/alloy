open! Base
open! Stdio

let ( let* ) = Lwt.bind

type t = {
  lwt_fd : Lwt_unix.file_descr;
  incoming : string Lwt_stream.t;
  send_raw : string -> unit;
}

let connect ~host ~port =
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let lwt_fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect lwt_fd addr in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input lwt_fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output lwt_fd in
  let (incoming, push) = Lwt_stream.create () in
  (* Background reader: read lines and push to stream *)
  Lwt.async (fun () ->
    Lwt.catch
      (fun () ->
        let rec loop () =
          let* line = Lwt_io.read_line ic in
          push (Some line);
          loop ()
        in
        loop ())
      (fun _exn ->
        push None;
        Lwt.return_unit));
  (* send_raw writes via Lwt_io and flushes asynchronously *)
  let send_raw msg =
    Lwt.async (fun () ->
      let* () = Lwt_io.write_line oc msg in
      Lwt_io.flush oc)
  in
  Lwt.return { lwt_fd; incoming; send_raw }

let close t =
  Lwt_unix.close t.lwt_fd
