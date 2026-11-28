open Sigma_common
open Sigma_driver
open Sigma_pkg

let rec mkdir_p dir =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let write_file path content =
  mkdir_p (Filename.dirname path);
  let oc = open_out path in
  output_string oc content;
  close_out oc

let test_mirrors_and_git_repos () =
  let tmp_root = Filename.temp_file "sigma_mg_" "" in
  (if Sys.file_exists tmp_root then Sys.remove tmp_root);
  Sys.mkdir tmp_root 0o755;
  let cleanup () = ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote tmp_root))) in
  let prev_home = Sys.getenv_opt "SIGMA_HOME" in
  let sigma_home = Filename.concat tmp_root "sigma_home" in
  Unix.putenv "SIGMA_HOME" sigma_home;
  (* 1. Criar mirror local file:// com pacote .sgmpkg *)
  let mlib_dir = Filename.concat tmp_root "mlib_src" in
  let mlib_m = Manifest.default ~kind:Manifest.KindLib ~version:"1.1.0" ~name:"mirror_lib" () in
  ignore (Manifest.scaffold_project ~root_dir:mlib_dir mlib_m);
  write_file (Filename.concat mlib_dir "src/lib.sgm") "public fun dobro(x: int): int { return x * 2; }\n";
  let mlib_so = Filename.concat mlib_dir ("libmirror_lib" ^ Pkg_pack.shared_lib_ext ()) in
  let mlib_src = match Source.load (Filename.concat mlib_dir "src/lib.sgm") with Ok s -> s | Error e -> cleanup (); failwith e in
  let so, h, sgm = match Driver.build_lib_source ~output_path:mlib_so mlib_src with Ok r -> r | Error _ -> cleanup (); failwith "build_lib failed" in
  let mirror_dir = Filename.concat tmp_root "mirror" in
  let pkg_file = Filename.concat mirror_dir "mirror_lib-1.1.0.sgmpkg" in
  ignore (Pkg_pack.pack_archive ~manifest_path:(Filename.concat mlib_dir "sigma.toml") ~artifacts:[ so; h; sgm ] ~output_path:pkg_file);
  write_file (Filename.concat mirror_dir "packages/mirror_lib/index.toml")
    (Printf.sprintf "[[version]]\nversion = \"1.1.0\"\nurl = \"file://%s\"\nchecksum = \"ok\"\n" pkg_file);
  (match Pkg_mirror.add_global_mirror ~name:"local_mirror" ~url:("file://" ^ mirror_dir) ~priority:1 with
   | Ok () -> ()
   | Error e -> cleanup (); failwith e);
  (* 2. Criar repositorio Git local com tag v1.0.0 *)
  let git_dir = Filename.concat tmp_root "git_lib" in
  let git_m = Manifest.default ~kind:Manifest.KindLib ~version:"1.0.0" ~name:"git_lib" () in
  ignore (Manifest.scaffold_project ~root_dir:git_dir git_m);
  write_file (Filename.concat git_dir "src/lib.sgm") "public fun quad(x: int): int { return x * 4; }\n";
  let git_cmd =
    Printf.sprintf
      "git -C %s init -q && git -C %s config user.email test@sigma.local && git -C %s config user.name Sigma && git -C %s add . && git -C %s commit -q -m init && git -C %s tag v1.0.0"
      (Filename.quote git_dir) (Filename.quote git_dir) (Filename.quote git_dir)
      (Filename.quote git_dir) (Filename.quote git_dir) (Filename.quote git_dir)
  in
  if Sys.command git_cmd <> 0 then (cleanup (); failwith "git init failed");
  (* 3. Projeto consumidor instala mirror_lib@^1.0.0 e git+file://...#v1.0.0 *)
  let app_dir = Filename.concat tmp_root "app" in
  ignore (Manifest.scaffold_project ~root_dir:app_dir (Manifest.default ~kind:Manifest.KindApp ~name:"app" ()));
  (match Pkg_deps.add_dependency ~root_dir:app_dir ~target:"mirror_lib" ~version_opt:(Some "^1.0.0") () with
   | Ok (n, v) -> assert (n = "mirror_lib" && v = "1.1.0")
   | Error e -> cleanup (); failwith e);
  (match Pkg_deps.add_dependency ~root_dir:app_dir ~target:("git+file://" ^ git_dir ^ "#v1.0.0") ~version_opt:None () with
   | Ok (n, v) -> assert (n = "git_lib" && v = "1.0.0")
   | Error e -> cleanup (); failwith e);
  (* 4. Testar E0140 e E0139 *)
  (match Pkg_deps.add_dependency ~root_dir:app_dir ~target:"nao_existe_mirror" ~version_opt:None () with
   | Error msg -> assert (String.starts_with ~prefix:"E0140:" msg)
   | Ok _ -> cleanup (); failwith "expected E0140");
  (match Pkg_git.clone_to_cache ~url:"file:///tmp/nao_existe_git_xyz" ~tag:(Some "v9") ~branch:None ~rev:None with
   | Error msg -> assert (String.starts_with ~prefix:"E0139:" msg)
   | Ok _ -> cleanup (); failwith "expected E0139");
  (* 5. Testar consumo em Sigma com paridade VM / C99 *)
  let consumer =
    Source.of_string ~filename:(Filename.concat app_dir "src/main.sgm")
      ~content:"import mirror_lib;\nimport git_lib;\nassert(mirror_lib.dobro(7), 14);\nassert(git_lib.quad(5), 20);\n"
  in
  (match Driver.test_source ~mode:Driver.ModeParity consumer with
   | Error ds -> cleanup (); failwith (String.concat "\n" (List.map Sigma_diagnostics.Diagnostic.render ds))
   | Ok res ->
     cleanup ();
     assert (res.passed_count = 2 && res.failed_count = 0));
  (match prev_home with Some v -> Unix.putenv "SIGMA_HOME" v | None -> Unix.putenv "SIGMA_HOME" "")
