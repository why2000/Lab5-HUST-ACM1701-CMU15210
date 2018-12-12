functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  (* 
   * vertex table and number of Edges
   *)
  type graph = (vertex seq table * int)
  (* 
   * root vertex and table of (target -> <path>)
   *)
  type asp = (vertex*vertex seq seq table)

  (* printer is great *)
  (* val pt = print ((Table.toString (fn vs => "<"^String.concatWith "," (Seq.toList (Seq.map (fn v => Key.toString v) vs))^">")) YourTable) *)
  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph = (Table.collect(E), length E)

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    let
      val (_, enumber) = G
    in
      enumber
    end

  fun numVertices (G : graph) : int =
    let
      val (vtable, enumber) = G
      val vtkeys = Table.domain vtable
      val vtvalues = Set.fromSeq (flatten (Table.range vtable))
    in 
      Set.size(Set.union (vtkeys, vtvalues))
    end

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    let
      val (vtable,_) = G
      val kvalues = Table.find vtable v
    in
      getOpt(kvalues, empty())
    end
  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    let
      val (vtable, enumber) = G
      val X = Table.empty()
      val F = Table.collect(Seq.singleton (v, singleton v))
      fun mkHistoryForward T Fro X f = 
        let
          val oldhistory = getOpt(Table.find Fro f, singleton(empty()))
          (* val history = Seq.map (fn onehis => Seq.append(onehis, Seq.singleton f)) oldhistory *)
          (* val ppztp = print(Int.toString (length (nth history 0))) *)
          val futurev = getOpt(Table.find T f, empty())
          fun addHistory (ver) = (ver, Seq.map (fn onehis => Seq.append(onehis, Seq.singleton ver)) oldhistory)
          val vhistory = Seq.map addHistory futurev
          val histab = Table.collect vhistory
          (* val ptss = print(Key.toString f) *)
        in
          Table.map (fn xseq => nth xseq 0) histab
        end
      fun BFSmkASP(vtable, X, F:key seq seq table) = 
        case size(F)
        of 0 => X
         | _ => 
          let
            val X' = Table.merge (fn(a, b) => Seq.append(a, b)) (X, F)
            val F' = Table.reduce (Table.merge (fn(a, b) => Seq.append(a, b))) (Table.empty()) (Table.tabulate (mkHistoryForward vtable F X') (Table.domain F))
            (* val prts = print(nth F' 0) *)
            (* val prts = print ("size:" ^ (Int.toString (size(F))) ^"") *)
            (* val re1 = print("<"^String.concatWith "," (Seq.toList (Seq.map (fn vmm => Int.toString (vmm)) Seq.fromList([1,2,3]) ))) *)
            (* val prtf = print ((Table.toString (fn vs => "<"^String.concatWith "," (Seq.toList (Seq.map (fn v0 => Key.toString (nth v0 0)) vs))^">")) F') *)
            (* val prtx = print ((Table.toString (fn vs => "<"^String.concatWith "," (Seq.toList ( Seq.map(fn v0 => Key.toString (nth v0 0)) vs))^">")) X') *)
            (* val xc = (Table.tabulate (mkHistoryForward vtable X') (Table.domain F))
            val te = if xc = 1 then 9 else 0 *)
            val F'' = Table.erase(F', Table.domain X')
            (* val reF'' = F'' *)
            (* val prtff = print ((Table.toString (fn vs => "<"^String.concatWith "," (Seq.toList (Seq.map (fn v0 => Key.toString (nth v0 0)) vs))^">")) F'') *)
          in
            BFSmkASP(vtable, X', F'')
          end
      val ASP = BFSmkASP(vtable, X, F)
      (* val prtz = print ("size:" ^ (Int.toString (size(ASP))) ^"")
      val prtzzz = print("sizezzz:" ^ (Int.toString (length(Table.range ASP))) ^"") *)
      (* val prtzz = print(Key.toString(nth (nth (nth (Table.range ASP) 0) 1) 1)) *)
    in
      (v, ASP)
    end

  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq =
    let
      val (root, pathtable) = A
    in
      getOpt(find pathtable v, empty())
    end

end
