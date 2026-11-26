open Sigma_common
open Sigma_driver
open Sigma_pkg

let test_lib_project_and_pack_parity () =
  let root = Filename.temp_file "sigma_lib_proj_" "" in
  (if Sys.file_exists root then Sys.remove root);
  Sys.mkdir root 0o755;
  let cleanup () = ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote root))) in
  let m = Manifest.default ~kind:Manifest.KindLib ~name:"mylib" () in
  let toml_round = Manifest.parse_toml (Manifest.to_toml m) in
  (match toml_round with
   | Ok m2 -> assert (m2.kind = Manifest.KindLib && m2.entry = "src/lib.sgm")
   | Error e -> cleanup (); failwith e);
  (match Manifest.scaffold_project ~root_dir:root m with
   | Error e -> cleanup (); failwith e
   | Ok _ -> assert (Sys.file_exists (Filename.concat root "src/lib.sgm")));
  let entry_path = Filename.concat root m.entry in
  let src =
    match Source.load entry_path with
    | Ok s -> s
    | Error e -> cleanup (); failwith e
  in
  let bad_lib = Source.of_string ~filename:"bad_lib.sgm" ~content:"println(\"oops\");\npublic fun f(): int { return 1; }\n" in
  (match Driver.build_lib_source ~output_path:(Filename.concat root "bad.so") bad_lib with
   | Error (d :: _) -> assert (d.code = "E0135")
   | _ -> cleanup (); failwith "expected E0135 for top-level executable stmt in --lib");
  let lib_so = Filename.concat root ("lib" ^ m.name ^ Pkg_pack.shared_lib_ext ()) in
  let so, h, sgm =
    match Driver.build_lib_source ~output_path:lib_so src with
    | Ok res -> res
    | Error ds -> cleanup (); failwith (String.concat "\n" (List.map Sigma_diagnostics.Diagnostic.render ds))
  in
  assert (Sys.file_exists so && Sys.file_exists h && Sys.file_exists sgm);
  let pkg_out = Filename.concat root "mylib-0.1.0.sgmpkg" in
  (match Pkg_pack.pack_archive ~manifest_path:(Filename.concat root "sigma.toml") ~artifacts:[ so; h; sgm ] ~output_path:pkg_out with
   | Error e -> cleanup (); failwith e
   | Ok _ -> assert (Sys.file_exists pkg_out));
  let consumer =
    Source.of_string ~filename:"<pkg_consumer>"
      ~content:
        (Printf.sprintf
           "import %S as pkg_lib;\nimport %S as dir_lib;\nassert(pkg_lib.somar(7, 8), 15);\nassert(dir_lib.somar(20, 22), 42);\nassert_error(\"E0136\", \"import \\\"missing_123.sgmpkg\\\";\");\n"
           pkg_out root)
  in
  (match Driver.test_source ~mode:Driver.ModeParity consumer with
   | Error ds -> cleanup (); failwith (String.concat "\n" (List.map Sigma_diagnostics.Diagnostic.render ds))
   | Ok res ->
     cleanup ();
     assert (res.passed_count = 3 && res.failed_count = 0))
