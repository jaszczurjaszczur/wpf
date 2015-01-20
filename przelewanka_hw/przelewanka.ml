exception SolutionFound

let przelewanka arr =
    let first_state = (Array.make (Array.length arr) 0)
    and limits = Array.map fst arr
    and end_state = Array.map snd arr
    in

    let bfs_queue = Queue.create () in
    Queue.add first_state bfs_queue;
    let dist_map = Hashtbl.create 17 (* 17 is just a intitial suggestion, so really whatever *) in
    Hashtbl.add dist_map first_state 0;
    
    let push_arr arr dist =
        if not (Hashtbl.mem dist_map arr) then
        begin
            Hashtbl.add dist_map arr dist;
            Queue.add arr bfs_queue;
            if arr = end_state then
                raise SolutionFound
        end
        ; ()
    in
    
    let fill_cups arr =
        let dist = Hashtbl.find dist_map arr in
        Array.iteri (
            fun ind x ->
                if x != limits.(ind) then
                begin
                    let new_arr = Array.copy arr in
                    new_arr.(ind) <- (limits.(ind));
                    push_arr new_arr (dist+1)
                end
        ) arr
    in
    
    let empty_cups arr =
        let dist = Hashtbl.find dist_map arr in
        Array.iteri (
            fun ind x ->
                if x != 0 then
                begin
                    let new_arr = Array.copy arr in
                    new_arr.(ind) <- 0;
                    push_arr new_arr (dist+1)
                end
        ) arr
    in
    
    let pour_cups arr =
        let dist = Hashtbl.find dist_map arr in
        Array.iteri (
            fun indout xout -> (* I can check something *)
            (
                if xout != 0 then
                Array.iteri (
                    fun indin xin ->
                        if indin != indout && xin != limits.(indin) then
                        begin
                            let new_arr = Array.copy arr in
                            let new_in = (min limits.(indin) (xout + xin) ) in
                            let new_out = xout + xin - new_in
                            in
                            new_arr.(indin) <- new_in;
                            new_arr.(indout) <- new_out;
                            push_arr new_arr (dist+1)
                        end
                ) arr
            )
        ) arr
    in
    
    let get_from_queue () =
        let arr = Queue.pop bfs_queue in
        pour_cups arr;
        fill_cups arr;
        empty_cups arr;
        
        ()
    in
    
    try
    begin
        if first_state = end_state then
            0
        else
        begin
            while not (Queue.is_empty bfs_queue) do
                get_from_queue ()
            done
            ;
            -1
        end
    end with 
        SolutionFound -> (Hashtbl.find dist_map end_state)
