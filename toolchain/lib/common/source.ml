type t = {
  filename : string;
  content : string;
  lines : string array;
}

let of_string ~filename ~content =
  let lines = String.split_on_char '\n' content |> Array.of_list in
  { filename; content; lines }

let load filename =
  try
    let ic = open_in_bin filename in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    Ok (of_string ~filename ~content)
  with Sys_error msg -> Error msg

let get_line src line_num =
  let idx = line_num - 1 in
  if idx >= 0 && idx < Array.length src.lines then Some src.lines.(idx)
  else None
