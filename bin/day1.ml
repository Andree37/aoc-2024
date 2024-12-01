let () =
  let input = Aoc.Input.read_file "inputs/day01/test.txt" in
  List.iter print_endline (List.iter Aoc.Input.split_string_by_char input);
