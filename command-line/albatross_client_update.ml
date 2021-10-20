open Lwt.Infix

let job_and_build loc =
  match String.split_on_char '/' loc with
  | "" :: "job" :: jobname :: "build" :: uuid :: "" :: [] -> Ok (jobname, uuid)
  | _ -> Error (`Msg ("expected '/job/<jobname>/build/<uuid>', got: " ^ loc))

let http_get_redirect ~happy_eyeballs uri =
  Http_lwt_client.one_request ~happy_eyeballs ~follow_redirect:false uri >|= function
  | Error _ as e -> e
  | Ok (resp, _body) ->
    match resp.Http_lwt_client.status with
    | #Http_lwt_client.Status.redirection ->
      (match Http_lwt_client.Headers.get resp.Http_lwt_client.headers "location" with
       | None -> Error (`Msg "no Location header received in HTTP reply")
       | Some loc -> Ok loc)
    | _ ->
      Logs.warn (fun m -> m "received HTTP reply: %a" Http_lwt_client.pp_response resp);
      Error (`Msg "unexpected HTTP reply")

let retrieve_build ~happy_eyeballs host hash =
   let uri = host ^ "/hash?sha256=" ^ hash in
   Lwt_result.bind_result (http_get_redirect ~happy_eyeballs uri) job_and_build

let retrieve_latest_build ~happy_eyeballs host jobname =
  let uri = host ^ "/job/" ^ jobname ^ "/build/latest/" in
  Lwt_result.bind_result (http_get_redirect ~happy_eyeballs uri) job_and_build

let can_update ~happy_eyeballs host hash =
  let open Lwt_result.Infix in
  retrieve_build ~happy_eyeballs host hash >>= fun (job, build) ->
  retrieve_latest_build ~happy_eyeballs host job >|= fun (_, build') ->
  job, build, build'

let http_get_binary ~happy_eyeballs host job build =
  let uri = host ^ "/job/" ^ job ^ "/build/" ^ build ^ "/main-binary" in
  Http_lwt_client.one_request ~happy_eyeballs uri >|= function
  | Error _ as e -> e
  | Ok (resp, None) ->
    Logs.warn (fun m -> m "received HTTP reply without body: %a" Http_lwt_client.pp_response resp);
    Error (`Msg "unexpected HTTP reply")
  | Ok (resp, Some body) ->
    match resp.Http_lwt_client.status with
    | #Http_lwt_client.Status.successful -> Ok body
    | _ ->
      Logs.warn (fun m -> m "received HTTP reply: %a" Http_lwt_client.pp_response resp);
      Error (`Msg "unexpected HTTP reply")

let prepare_update ~happy_eyeballs level host dryrun = function
  | Ok (_hdr, `Success (`Unikernel_info
      [ _name, Vmm_core.Unikernel.{ digest ; bridges ; block_devices ; argv ; cpuid ; memory ; fail_behaviour ; typ = `Solo5 as typ ; _ } ])) ->
    begin
      let `Hex hash = Hex.of_cstruct digest in
      can_update ~happy_eyeballs host hash >>= function
      | Error `Msg msg ->
        Logs.err (fun m -> m "error in HTTP interaction: %s" msg);
        Lwt.return (Error Albatross_cli.Http_error)
      | Ok (_, old_uuid, new_uuid) when String.equal old_uuid new_uuid ->
        Logs.app (fun m -> m "already up to date");
        Lwt.return (Error Albatross_cli.Success)
      | Ok (_, old_uuid, new_uuid) when dryrun ->
        Logs.app (fun m -> m "compare at %s/compare/%s/%s/opam-switch"
                     host old_uuid new_uuid);
        Lwt.return (Error Albatross_cli.Success)
      | Ok (job, old_uuid, new_uuid) ->
        Logs.app (fun m -> m "compare at %s/compare/%s/%s/opam-switch"
                     host old_uuid new_uuid);
        http_get_binary ~happy_eyeballs host job new_uuid >>= function
        | Error `Msg msg ->
          Logs.err (fun m -> m "error in HTTP interaction: %s" msg);
          Lwt.return (Error Albatross_cli.Http_error)
        | Ok unikernel ->
          let tmpfile = Fpath.v (Filename.temp_file "albatross" "unikernel") in
          let r =
            Result.bind
              (Bos.OS.File.write tmpfile unikernel)
              (fun () -> Vmm_unix.manifest_devices_match ~bridges ~block_devices tmpfile)
          in
          ignore (Bos.OS.File.delete tmpfile);
          match r with
          | Error `Msg msg ->
            Logs.err (fun m -> m "manifest failed: %s" msg);
            Lwt.return (Error Albatross_cli.Internal_error)
          | Ok () ->
            let compressed, image =
              match level with
              | 0 -> false, unikernel |> Cstruct.of_string
              | _ -> true, Vmm_compress.compress ~level unikernel |> Cstruct.of_string
            in
            let config = { Vmm_core.Unikernel.typ ; compressed ; image ; fail_behaviour ; cpuid; memory ; block_devices ; bridges ; argv } in
            Lwt.return (Ok (`Unikernel_force_create config))
    end
  | Ok w ->
    Logs.err (fun m -> m "unexpected reply: %a" Vmm_commands.pp_wire w);
    Lwt.return (Error Albatross_cli.Communication_failed)
  | Error _ -> Lwt.return (Error Albatross_cli.Communication_failed)
