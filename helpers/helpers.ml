open Lwt
open Lwt_process
open Cohttp
open Cohttp_lwt_unix

let getcookie =
  let cmd = shell "GNUPGHOME=~/configs/gnupg/ pass show aoccookie" in
  let _ = exec ~stdout:`Dev_null cmd in
  pread cmd >|= CCString.rtrim
  

let input day =
  getcookie >>= fun cookie ->
  let headers = Header.init_with "Cookie" ("session=" ^ cookie) in
  Client.get ~headers (Uri.of_string ("https://adventofcode.com/2021/day/" ^ string_of_int(day) ^ "/input")) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string
