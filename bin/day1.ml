let input = Aoc.Input.read_file "inputs/day01/input.txt"

let rec group acc1 acc2 input =
  match input with
  | [] -> (acc1, acc2)
  | line :: rest ->
    let nums = Scanf.sscanf line "%d %d" (fun a b -> (a,b)) in
    group (fst nums :: acc1) (snd nums :: acc2) rest

let rec sum_lists l1 l2 suml =
match l1, l2 with
| [], [] -> suml
| a :: rest1, b :: rest2 -> sum_lists rest1 rest2 (abs (a-b) :: suml)
| _, _ -> []


let compare_lists list1 list2 =
  let sorted1 = List.sort compare list1 in
  let sorted2 = List.sort compare list2 in

  let summed_lists = sum_lists sorted1 sorted2 [] in

  List.sort compare summed_lists

let rec sum_list list acl =
  match list with
  | [] -> acl
  | v :: rest -> sum_list rest acl+v

let () =
  let lines1, lines2 = group [] [] input in
  let compared_lists = compare_lists lines1 lines2 in
  print_endline (string_of_int (sum_list compared_lists 0))
