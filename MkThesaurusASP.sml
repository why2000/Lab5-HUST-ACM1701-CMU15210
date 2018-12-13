functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP

  (* Remove the following two lines when you're done! *)
  exception NYI

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type thesaurus = graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
  (* raise NYI *)
    let
      fun toPairs(V1, E1) = Seq.tabulate (fn i => (V1, nth E1 i)) (length E1)
      val EG = Seq.flatten (Seq.map toPairs S)
      (* val EG = Seq.reduce toPairs (Seq.empty()) S *)
    in
      ASP.makeGraph EG
    end

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
  (* raise NYI *)
    ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
  (* raise NYI *)
    ASP.outNeighbors T w

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    let
      val TASP = ASP.makeASP T w1
    in
      report TASP w2
    end
end
