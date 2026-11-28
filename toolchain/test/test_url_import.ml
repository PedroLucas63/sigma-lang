open Sigma_common
open Sigma_driver

let write_file path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

let test_url_import_parity () =
  let tmp_dir = Filename.temp_file "sigma_url_test_" "" in
  (if Sys.file_exists tmp_dir then Sys.remove tmp_dir);
  Sys.mkdir tmp_dir 0o755;
  let cleanup () =
    (try
       Array.iter (fun f -> Sys.remove (Filename.concat tmp_dir f)) (Sys.readdir tmp_dir);
       Sys.rmdir tmp_dir
     with _ -> ())
  in
  let prev_cache = Sys.getenv_opt "SIGMA_CACHE_DIR" in
  let prev_offline = Sys.getenv_opt "SIGMA_OFFLINE" in
  Unix.putenv "SIGMA_CACHE_DIR" tmp_dir;
  Unix.putenv "SIGMA_OFFLINE" "1";
  let helper_url = "https://raw.githubusercontent.com/sigma/demo/main/helper.sgm" in
  let lib_raw_url = "https://raw.githubusercontent.com/sigma/demo/main/lib.sgm" in
  let lib_blob_url = "https://github.com/sigma/demo/blob/main/lib.sgm" in
  let write_cached url code =
    let h = Digest.to_hex (Digest.string url) in
    write_file (Filename.concat tmp_dir (h ^ ".sgm")) code
  in
  write_cached helper_url "public fun triplo(x: int): int { return x * 3; }\n";
  write_cached lib_raw_url
    "import \"helper.sgm\";\npublic fun calc(x: int): int { return helper.triplo(x) + 1; }\n";
  let prog_src =
    Source.of_string ~filename:"<url_test>"
      ~content:
        (Printf.sprintf
           "import %S as lib;\nfrom %S import calc;\nassert(lib.calc(4), 13);\nassert(calc(5), 16);\n"
           lib_blob_url lib_raw_url)
  in
  (match Driver.test_source ~mode:Driver.ModeParity prog_src with
   | Error ds ->
     cleanup ();
     failwith (String.concat "\n" (List.map Sigma_diagnostics.Diagnostic.render ds))
   | Ok res ->
     cleanup ();
     assert (res.passed_count = 2 && res.failed_count = 0));
  (match prev_cache with Some v -> Unix.putenv "SIGMA_CACHE_DIR" v | None -> Unix.putenv "SIGMA_CACHE_DIR" "");
  (match prev_offline with Some v -> Unix.putenv "SIGMA_OFFLINE" v | None -> Unix.putenv "SIGMA_OFFLINE" "")
