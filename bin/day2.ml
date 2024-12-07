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

let rec remove_first_of_pred lst pred =
  match lst with
  | [] -> []
  | a :: rest when pred a -> rest
  | a :: rest -> a :: remove_first_of_pred rest pred

let all_positive_or_negative_or_one_off lst =
  let all_same = List.for_all (fun x -> x > 0 && (abs x) <= max_change) lst || List.for_all (fun x -> x < 0 && (abs x) <= max_change) lst in

  if (all_same) then true else
    (* remove one pos *)
    let lst_off_pos = remove_first_of_pred lst (fun x -> x >= 0 || x > max_change) in
    let all_same_pos_off = List.for_all (fun x -> x > 0 && (abs x) <= max_change) lst_off_pos || List.for_all (fun x -> x < 0 && (abs x) <= max_change) lst_off_pos in
    if (all_same_pos_off) then true else
      (* remove one negative *)
      let lst_off_neg = remove_first_of_pred lst (fun x -> x <= 0 || x > max_change) in
      List.for_all (fun x -> x > 0 && (abs x) <= max_change) lst_off_neg || List.for_all (fun x -> x < 0 && (abs x) <= max_change) lst_off_neg



let () =
  let reports = List.map (fun x -> split_line_into_levels (String.split_on_char ' ' x) []) input in
  let report_diffs = List.map (fun x -> make_list_of_diffs x []) reports in

  let reports_increase_or_decrease = List.map (fun x -> all_positive_or_negative x) report_diffs in
  let summed_reports = List.fold_left_map (fun acc x -> (acc + (if x then 1 else 0), x)) 0 reports_increase_or_decrease in
  print_endline ("day2 part 1: " ^ (string_of_int (fst summed_reports)));

  let reports_increase_or_decrease_one_off = List.map (fun x -> all_positive_or_negative_or_one_off x) report_diffs in
  let summed_reports_one_off = List.fold_left_map (fun acc x -> (acc + (if x then 1 else 0), x)) 0 reports_increase_or_decrease_one_off in
  print_endline ("day2 part 2: " ^ (string_of_int (fst summed_reports_one_off)));;
