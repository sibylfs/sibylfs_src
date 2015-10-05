(****************************************************************************)
(*  Copyright (c) 2013, 2014, 2015, Tom Ridge, David Sheets, Thomas Tuerk,  *)
(*  Andrea Giugliano (as part of the SibylFS project)                       *)
(*                                                                          *)
(*  Permission to use, copy, modify, and/or distribute this software for    *)
(*  any purpose with or without fee is hereby granted, provided that the    *)
(*  above copyright notice and this permission notice appear in all         *)
(*  copies.                                                                 *)
(*                                                                          *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL           *)
(*  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED           *)
(*  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE        *)
(*  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL    *)
(*  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR   *)
(*  PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER          *)
(*  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR        *)
(*  PERFORMANCE OF THIS SOFTWARE.                                           *)
(*                                                                          *)
(*  Meta:                                                                   *)
(*    - Headers maintained using headache.                                  *)
(*    - License source: http://opensource.org/licenses/ISC                  *)
(****************************************************************************)

open Cmdliner

let man_trailer = [
  `S "BUGS";
  `P "Report bugs on the web at <https://github.com/sibylfs/sibylfs_src>.";
]

let map f x = Term.(pure f $ x)

let out_dir default = Arg.(
  value & opt dir default & info ["o";"out-dir"]
    ~docv:"OUT_DIR"
    ~doc:("The directory in which to create our test run temporary directory.\n"
          ^"Defaults to '"^default^"'.")
)

let dir ~docv ~doc k = Arg.(
  required & pos k (some ~none:"<missing dir>" dir) None & info [] ~docv ~doc
)

let spec all_specs k = Arg.(
  let all_s = String.concat ", " all_specs in
  required & pos k (some ~none:"<missing spec>" string) None & info []
    ~docv:"SPEC"
    ~doc:("The spec which should be used (one of "^all_s^")")
)

let models all_models = map List.flatten Arg.(
    let all_s = String.concat ", " all_models in
    value & opt_all (list string) [all_models] & info ["model"]
      ~docv:"MODEL"
      ~doc:("The models which should be used (default "
            ^all_s^"). When running fs_test exec, the models
            linux_spec, posix_spec, mac_os_x_spec, and freebsd_spec allow
            you to run tests directly against the spec. The special model
            path=[path] allows you to use a file system path as the model
            to test against.")
  )

let params all_params = map List.flatten Arg.(
    let all_s = String.concat ", " all_params in
    value & opt_all (list string) [] & info ["param"]
      ~docv:"PARAM"
      ~doc:("The parameters which should be used during instantiation of
             a model (default none). Not all models support all parameters.
             Currently supported parameters include: "^all_s)
  )

let suite_path default =
  map (fun paths -> List.flatten (paths@[default])) Arg.(
    value & opt_all (list ~sep:':' string) [] & info ["suites"]
      ~docv:"SUITE_PATH"
      ~doc:("The paths to search for test suites (default "
            ^(String.concat ":" default)^")")
  )

let suite all_suites = map List.flatten Arg.(
  let all_s = match all_suites with
    | [] -> "all"
    | all_suites -> String.concat ", " all_suites
  in
  value & opt_all (list string) [all_suites] & info ["suite"]
    ~docv:"SUITE"
    ~doc:("The test suites to run (default "
          ^all_s^")")
)

let trace = map List.flatten Arg.(
  value & opt_all (list string) [] & info ["trace"]
    ~docv:"TRACE"
    ~doc:("The trace files to run (default all for selected suites)")
)

let skip_trace = map List.flatten Arg.(
  value & opt_all (list string) [] & info ["skip-trace"]
    ~docv:"TRACE"
    ~doc:("The trace files to skip")
)
