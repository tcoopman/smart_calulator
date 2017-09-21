type move =
    | Add of int
    | Divide of int
    | Replace of int * int

let apply start = function
    | Add i -> i + start
    | Divide i -> start / i
    | Replace (from, to_) -> 
        string_of_int start
        |> Js.String.replace (string_of_int from) (string_of_int to_)
        |> int_of_string

let print moves =
    let print = function
        | Add i -> {j|+ $(i)|j}
        | Divide i -> {j|/ $(i)|j}
        | Replace (from, to_) -> {j|$(from) => $(to_)|j}
    in
    List.map print moves
    |> String.concat ", "

(*
     after googling a lot, I found that we were looking for permutations with repetions
     https://en.wikipedia.org/wiki/Permutation
     https://rosettacode.org/wiki/Permutations_with_repetitions
*)
let rec permute nbOfMoves allowedMoves =
	match nbOfMoves with
    | 0 -> [[]]
    | _ -> 
        List.map (fun x -> 
        	List.map (fun move -> move :: x) allowedMoves |> List.rev
        ) (permute (nbOfMoves - 1) allowedMoves) 
        |> List.flatten
        
let solve start goal nbOfMoves allowedMoves =
	let permutations = permute nbOfMoves allowedMoves in
    match (start, goal, nbOfMoves) with
    | (x, y, 0) when x == y -> [[]]
    | _->
    	List.filter (fun possibleSolution ->
        	goal == List.fold_left apply start possibleSolution
        ) permutations
        

let assertResult start goal nbOfMoves allSolutions =
    match allSolutions with
    | [] -> false
    | moves :: _ -> 
        Js.log (print moves);
        (nbOfMoves == List.length moves) && (goal == List.fold_left apply start moves)

let test =
    Js.log (assertResult 0 0 0 (solve 0 0 0 []));
    Js.log (assertResult 0 1 1 (solve 0 1 1[Add 1]));
    Js.log (assertResult 0 9 3 (solve 0 9 3 [Add 2; Add 3;]));
    Js.log (assertResult 0 4 4 (solve 0 4 4 [Add 2; Add 3; Divide 2; Replace (3, 4)]));
    (* Js.log (assertResult 11 3100 9 (solve 11 3100 9 [Divide 2; Add 3; Replace (1, 2); Replace (2, 9); Add 2; Add 1; Replace (2, 10); Replace (10, 100)])) *)