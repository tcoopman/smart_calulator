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
     The permute 2 comes from: 
     http://developer-should-know.com/post/76525296527/permutation-with-repetition-generation-algorithm
*)
let permute nbOfMoves allowedMoves filter = 
    let permute2 nbOfMoves allowedMoves =
        let first = allowedMoves.(0) in
        let size = Array.length allowedMoves in
        let maximum = (float_of_int size) ** (float_of_int nbOfMoves) |> int_of_float in
        let result = [||] in
        for i = 0 to (maximum - 1) do
            let permutation = Array.make nbOfMoves first in
            let k = ref i in
            for j = 0 to (nbOfMoves - 1) do
                if j > 0 then k := !k /size;
                permutation.(j) <- allowedMoves.(!k mod size)
            done;
            let permutationResult = Array.to_list permutation in
            if filter permutationResult then Js.Array.push permutationResult result |> ignore;
        done;
        result |> Array.to_list
    in
    match (nbOfMoves, allowedMoves) with
        | (0, _) -> [[]]
        | (_, []) -> [[]]
        | _ ->
            permute2 nbOfMoves (Array.of_list allowedMoves)

let solve start goal nbOfMoves allowedMoves =
    let filter possibleSolution = goal == List.fold_left apply start possibleSolution in
	let permutations = permute nbOfMoves allowedMoves filter in
    match (start, goal, nbOfMoves) with
    | (x, y, 0) when x == y -> [[]]
    | _->
        permutations

let assertResult start goal nbOfMoves allSolutions =
    match allSolutions with
    | [] -> false
    | moves :: _ -> 
        Js.log "All solutions:";
        List.iter (fun x -> Js.log (print x)) allSolutions;
        Js.log "correct?";
        (nbOfMoves == List.length moves) && (goal == List.fold_left apply start moves)

let test =
    Js.log (assertResult 0 0 0 (solve 0 0 0 []));
    Js.log (assertResult 0 1 1 (solve 0 1 1[Add 1]));
    Js.log (assertResult 0 9 3 (solve 0 9 3 [Add 2; Add 3;]));
    Js.log (assertResult 0 4 4 (solve 0 4 4 [Add 2; Add 3; Divide 2; Replace (3, 4)]));
    (* This one now works, it takes a while to find all solutions, but it works :)*)
    Js.log (assertResult 11 3100 9 (solve 11 3100 9 [Divide 2; Add 3; Replace (1, 2); Replace (2, 9); Add 2; Add 1; Replace (2, 10); Replace (10, 100)]))