open Printf

type user =
    {id: int; name: string; mutable friends: int list}

let create_users names =
    List.mapi (fun id name -> {id; name; friends = []}) names
    |> List.fold_left (fun a x -> x :: a) []
    |> List.rev
    |> Array.of_list

let add_friends users =
    List.iter (fun (i, j) ->
        users.(i).friends <- j :: users.(i).friends;
        users.(j).friends <- i :: users.(j).friends
    )

let print_users users =
    for i = 0 to Array.length users - 1 do
        let {id; name; friends} = users.(i) in
        printf "id: %d, name: %s, friends: " id name;
        print_newline ();
        List.iter (fun i ->
            let {id; name} = users.(i) in
            printf "\tid: %d, name: %s" id name;
            print_newline ()
        ) friends
    done

let users_stats =
    Array.map (fun {id; friends} -> id, List.length friends)

(** friend of a friend function stub *)
let foaf users = ()
    
let () =
    let users = create_users ["Hero"; "Dunn"; "Sue"; "Chi"; "Thor"; "Clive"; "Hicks"; "Devin"; "Kate"; "Klein"] in
    add_friends users [ 0,1; 0,2; 1,2; 1,3; 2,3; 3,4; 4,5; 5,6; 5,7; 6,8; 7,8; 8,9 ];
    let total_links = Array.fold_left (fun a {friends} -> a + List.length friends) 0 users |> float in
    let total_users = Array.length users |> float in
    let avg_links = total_links /. total_users in    
    print_users users;
    let stats = users_stats users in
    Array.sort (fun (_, a) (_, b) ->
        match a, b with
        | _ when a > b -> -1
        | _ when a < b -> 1
        | _ -> 0
    ) stats;
    print_string "Users' statistics:";
    print_newline();    
    for i = 0 to Array.length stats - 1 do
        let id, count = stats.(i) in
        printf "id: %d, count: %d" id count;
        print_newline()
    done;
    printf "total links = %f, average links = %f" total_links avg_links;;