type exit_status =
  | Success
  | Local_authentication_failed
  | Chain_failure
  | Remote_authentication_failed
  | Communication_failed
  | Connect_failed
  | Remote_command_failed
  | Cli_failed
  | Internal_error
  | Http_error

let classify_tls_error = function
  | Tls_lwt.Tls_alert
      (Tls.Packet.BAD_CERTIFICATE
      | Tls.Packet.UNSUPPORTED_CERTIFICATE
      | Tls.Packet.CERTIFICATE_REVOKED
      | Tls.Packet.CERTIFICATE_EXPIRED
      | Tls.Packet.CERTIFICATE_UNKNOWN) as exn ->
    Logs.err (fun m -> m "local authentication failure %s"
                 (Printexc.to_string exn));
    Local_authentication_failed
  | Tls_lwt.Tls_failure (`Error (`AuthenticationFailure _)) as exn ->
    Logs.err (fun m -> m "remote authentication failure %s"
                 (Printexc.to_string exn));
    Remote_authentication_failed
  | exn ->
    Logs.err (fun m -> m "failed to establish TLS connection: %s"
                 (Printexc.to_string exn));
    Communication_failed
