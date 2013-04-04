open ErrorLocation
open FixEmacs

let rec fix_one_pattern loc =
  let file = loc.loc_file in
  let begin_pos = loc.loc_begin_pos in
  let end_pos = loc.loc_end_pos in

  Printf.fprintf stderr "begin_pos = %d/end_pos = %d/%d\n%!"
    begin_pos end_pos (String.length file.file_content);
  Printf.fprintf stderr "OPEN TO REMOVE: [%s]\n%!"
    (String.sub file.file_content begin_pos (end_pos - begin_pos + 1));
  FixUtils.(
    with_elisp
      [
        find_file loc.loc_file;
        delete_region loc.loc_file (begin_pos+1) (end_pos+2);
        next_error;
      ])

let rec fix loc dirname next_lines =
  fix_one_pattern loc;

  match next_lines with
    |  location_line :: error_line :: next_lines when
        OcpString.starts_with error_line
          "Warning 33: unused open"
        ->
      let loc = ErrorLocation.parse_location dirname location_line in
      fix loc dirname next_lines

    | _ ->
      FixEmacs.(
        with_elisp
          [
            save_current_buffer;
            print_message "Unused open removed, saved.";
          ])
