open Sigma_common
open Sigma_driver
open Sigma_pkg

let test_semver_and_pkg_manager () =
  (match Pkg_semver.parse_range "^1.2.0, <1.5.0" with
   | Error e -> failwith e
   | Ok r ->
     let vs =
       List.filter_map
         (fun s -> match Pkg_semver.parse_version s with Ok v -> Some v | Error _ -> None)
         [ "1.1.0"; "1.2.0"; "1.4.2"; "1.5.0"; "2.0.0" ]
     in
     (match Pkg_semver.best_matching vs r with
      | Some v -> assert (Pkg_semver.to_string v = "1.4.2")
      | None -> failwith "expected 1.4.2"));
  (match Pkg_semver.parse_version "bad.ver" with
   | Error msg -> assert (String.starts_with ~prefix:"E0137:" msg)
   | Ok _ -> failwith "expected E0137");
  let tmp_root = Filename.temp_file "sigma_pm_" "" in
  (if Sys.file_exists tmp_root then Sys.remove tmp_root);
  Sys.mkdir tmp_root 0o755;
  let cleanup () = ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp_root))) in
  let prev_home = Sys.getenv_opt "SIGMA_HOME" in
  let sigma_home = Filename.concat tmp_root "sigma_home" in
  Unix.putenv "SIGMA_HOME" sigma_home;
  let mk_lib ver mult_val =
    let dir = Filename.concat tmp_root ("lib_" ^ ver) in
    let m = Manifest.default ~kind:Manifest.KindLib ~version:ver ~name:"math_pkg" () in
    ignore (Manifest.scaffold_project ~root_dir:dir m);
    let oc = open_out (Filename.concat dir "src/lib.sgm") in
    output_string oc (Printf.sprintf "public fun mult(x: int): int { return x * %d; }\n" mult_val);
    close_out oc;
    match Pkg_deps.publish_dir_to_store dir with
    | Ok _ -> ()
    | Error e -> cleanup (); failwith e
  in
  mk_lib "1.0.0" 2;
  mk_lib "1.2.3" 10;
  let app_dir = Filename.concat tmp_root "app" in
  let app_m = Manifest.default ~kind:Manifest.KindApp ~name:"my_app" () in
  ignore (Manifest.scaffold_project ~root_dir:app_dir app_m);
  (match Pkg_deps.add_dependency ~root_dir:app_dir ~target:"math_pkg" ~version_opt:(Some "^1.0.0") () with
   | Ok (n, v) -> assert (n = "math_pkg" && v = "1.2.3")
   | Error e -> cleanup (); failwith e);
  let loaded_app = match Manifest.load ~path:(Filename.concat app_dir "sigma.toml") () with Ok m -> m | Error e -> cleanup (); failwith e in
  (match Pkg_deps.install_all ~root_dir:app_dir ~frozen:true loaded_app with
   | Ok lock -> assert (List.length lock = 1)
   | Error e -> cleanup (); failwith e);
  (match Pkg_deps.add_dependency ~root_dir:app_dir ~target:"nonexistent_pkg_xyz" ~version_opt:None () with
   | Error msg -> assert (String.starts_with ~prefix:"E0138:" msg)
   | Ok _ -> cleanup (); failwith "expected E0138");
  let app_test_file = Filename.concat app_dir "src/main.sgm" in
  let consumer_src =
    Source.of_string ~filename:app_test_file
      ~content:"import math_pkg;\nfrom math_pkg import mult;\nassert(math_pkg.mult(3), 30);\nassert(mult(4), 40);\n"
  in
  (match Driver.test_source ~mode:Driver.ModeParity consumer_src with
   | Error ds -> cleanup (); failwith (String.concat "\n" (List.map Sigma_diagnostics.Diagnostic.render ds))
   | Ok res ->
     cleanup ();
     assert (res.passed_count = 2 && res.failed_count = 0));
  (match prev_home with Some v -> Unix.putenv "SIGMA_HOME" v | None -> Unix.putenv "SIGMA_HOME" "")
