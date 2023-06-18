// Date: 11/06/2023
// Contributor(s): Roar

module TreeDesigner

open Config

type Position = float
type Tree<'a> = Node of 'a * (Tree<'a> list)
type Extent = (Position * Position) list

let moveTree (Node((label, x), subtrees), x') =
    Node((label, x+x'), subtrees)

let moveExtent ((e:Extent), x) :Extent =
    List.map (fun (p,q) -> (p+x, q+x)) e

let rec merge (ps:Extent) (qs:Extent) :Extent =
    match (ps,qs) with
        | ([], qs)               -> qs
        | (ps, [])               -> ps
        | ((p,_)::ps, (_,q)::qs) -> (p,q) :: merge ps qs

let mergeList (es: Extent list) : Extent =
    List.fold merge [] es

let rec fit (ps: Extent) (qs: Extent) (config: DesignConfig) =
    match (ps, qs) with
    | ((_,p)::ps, (q,_)::qs) -> max (fit ps qs config) (p - q + config.HorizontalSpacing)
    | _                      -> 0.0

let fitListLeft es config =
    let rec helper acc es =
        match es with
            | [] -> []
            | (e::ess) ->
                let x = fit acc e config
                in x :: helper (merge acc (moveExtent (e,x))) ess
    in es |> helper []

let fitListRight es config =
    let rec helper acc es =
        match es with
            | [] -> []
            | (e::ess) ->
                let x = -(fit e acc config)
                in x :: helper (merge (moveExtent (e,x)) acc) ess
    in List.rev es |> helper [] |> List.rev

let mean (x,y) :Position =
    (x + y)/2.0

let fitList es config =
    List.map mean (List.zip (fitListLeft es config) (fitListRight es config))

let design config tree =
    let rec design' (Node(label, subtrees)) =
        let (trees,extents) = List.unzip (List.map design' subtrees)
        let positions = fitList extents config
        let ptrees = List.map moveTree (List.zip trees positions)
        let pextents = List.map moveExtent (List.zip extents positions)
        let resultextent = (0.0, 0.0) :: mergeList pextents
        let resulttree = Node((label, 0.0), ptrees)
        in (resulttree, resultextent)
    in fst(design' tree)
