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
   * number of Edges and vertex table
   *)
  type graph = (int*key seq table)
  (* 
   * root vertex and table of (target -> <path>)
   *)
  type asp = (vertex*vertex seq seq table)

  (* printer is great *)
  (* val pt = print ((Table.toString (fn vs => "<"^String.concatWith "," (Seq.toList (Seq.map (fn v => Key.toString v) vs))^">")) YourTable) *)
  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph = (length E, Table.collect(E))

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    let
      val (enumber, _) = G
    in
      enumber
    end

  fun numVertices (G : graph) : int =
    let
      val (enumber, vtable) = G
      val vtkeys = Table.domain vtable
      val vtvalues = Set.fromSeq (flatten (Table.range vtable))
    in 
      Set.size(Set.union (vtkeys, vtvalues))
    end

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    let
      val (_,vtable) = G
      val kvalues = Table.find vtable v
    in
      getOpt(kvalues, empty())
    end
  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    let
      val (enumber, vtable) = G
      val X = Table.empty()
      val F = Table.collect(Seq.singleton (v, singleton v))
      fun mkHistoryForward T Fro X f = 
        let
          val oldhistory = getOpt(Table.find Fro f, singleton(empty()))
          val futurev = outNeighbors (0,T) f
          fun addHistory (ver) = (ver, Seq.map (fn onehis => Seq.append(onehis, Seq.singleton ver)) oldhistory)
          val vhistory = Seq.map addHistory futurev
          val histab = Table.collect vhistory
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
            val F'' = Table.erase(F', Table.domain X')
          in
            BFSmkASP(vtable, X', F'')
          end
      val myASP = BFSmkASP(vtable, X, F)
    in
      (v, myASP)
    end

  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq =
    let
      val (root, pathtable) = A
    in
      getOpt(find pathtable v, empty())
    end

end
