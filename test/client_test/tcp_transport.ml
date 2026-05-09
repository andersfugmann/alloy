open! Base
open! Stdio

let ( let* ) = Lwt.bind

type t = {
  lwt_fd : Lwt_unix.file_descr;
  recv_s : Protocol.json Protocol.frame Lwt_stream.t;
  send_f : Protocol.json Protocol.frame -> unit;
}

let connect ~host ~port =
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let lwt_fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect lwt_fd addr in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input lwt_fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output lwt_fd in
  let (recv_s, push) = Lwt_stream.create () in
  (* Background reader: read lines, parse as frames, push to stream *)
  Lwt.async (fun () ->
    Lwt.catch
      (fun () ->
        let rec loop () =
          let* line = Lwt_io.read_line ic in
          begin match Protocol.deserialize_frame line with
          | Ok frame -> push (Some frame)
          | Error _ -> ()
          end;
          loop ()
        in
        loop ())
      (fun _exn ->
        push None;
        Lwt.return_unit));
  let (write_stream, push_write) = Lwt_stream.create () in
  (* Writer fiber — processes frames sequentially, guaranteeing order *)
  Lwt.async (fun () ->
    Lwt.catch
      (fun () ->
        Lwt_stream.iter_s (fun frame ->
          let msg = Protocol.serialize_frame frame in
          let* () = Lwt_io.write_line oc msg in
          Lwt_io.flush oc) write_stream)
      (fun _exn -> Lwt.return_unit));
  let send_f frame = push_write (Some frame) in
  Lwt.return { lwt_fd; recv_s; send_f }

let close t =
  Lwt_unix.close t.lwt_fd
