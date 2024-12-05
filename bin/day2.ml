let input = Aoc.Input.read_file "inputs/day02/input.txt"

let max_change = 3

let rec split_line_into_levels line_lst agg_lst =
  match line_lst with
  | [] -> agg_lst
  | a :: rest ->
    int_of_string a :: split_line_into_levels rest agg_lst

let rec make_list_of_diffs reports agg_lst =
  match reports with
  | [] -> agg_lst
  | a :: rest ->
    match rest with
    | [] -> []
    | b :: _ -> (a-b) :: (make_list_of_diffs rest agg_lst)

let all_positive_or_negative lst =
  List.for_all (fun x -> x > 0 && (abs x) <= max_change) lst || List.for_all (fun x -> x < 0 && (abs x) <= max_change) lst


let () =
  let reports = List.map (fun x -> split_line_into_levels (String.split_on_char ' ' x) []) input in
  let report_diffs = List.map (fun x -> make_list_of_diffs x []) reports in
  let reports_increase_or_decrease = List.map (fun x -> all_positive_or_negative x) report_diffs in
  let summed_reports = List.fold_left_map (fun acc x -> (acc + (if x then 1 else 0), x)) 0 reports_increase_or_decrease in
  print_endline ("day2 part 1: " ^ (string_of_int (fst summed_reports)));;
