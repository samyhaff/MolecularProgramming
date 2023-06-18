// Date: 11/06/2023
// Contributor(s): Roar, Valeriu, Samy

module TreeDesignerChecks
type internal Marker = interface end
let internal moduleName = string typeof<Marker>.DeclaringType

open FsCheck
open TreeDesigner
open Config

type Distance = Dist of float

let floatTolerance = 1e-5

let floatsEquals (v1:float) (v2:float) =
    abs (v2-v1) < floatTolerance

let floatsHasMinDifference minDifference (v1:float, v2:float) =
    let diff = abs (v2-v1)
    diff + floatTolerance >= minDifference


let moveTreeMovesTree (originalPosition:Position) (Dist(move)) =
    let (Node((_, newPosition), _)) = moveTree (Node(("test", originalPosition), []), move)
    floatsEquals newPosition (originalPosition + move)

let moveExtentMovesAllPairs (ex:Extent) (Dist(move)) =
    moveExtent (ex,move)
        |> List.zip ex
        |> List.forall (fun ((p,q),(p',q')) -> (floatsEquals p' (p+move) && floatsEquals q' (q+move)))

let mergedExtentsHasMaxLength ex1 ex2 =
    merge ex1 ex2 |> List.length = max (List.length ex1) (List.length ex2)

let mergedExtentsCorrectPairMerge ex1 ex2 =
    let merged = merge ex1 ex2
    let leftCheck = Seq.zip ex1 merged |> Seq.forall (fun ((p,_), (m,_)) -> p.Equals(m))
    let rightCheck = Seq.zip ex2 merged |> Seq.forall (fun ((_,q), (_,m)) -> q.Equals(m))
    leftCheck && rightCheck


// Property 1
let nodesAtSameLevelShouldBeAtleastAGivenDistanceApart (Dist(spacing)) (tree:Tree<unit>) =
    let checkSpacing =
        floatsHasMinDifference spacing

    let childrenWithAbsolutePositionToParent (Node((_,p),c)) =
        List.map (fun (Node((l', p'), c')) -> Node((l', p+p'), c')) c

    let maxLevelSize tree =
        let rec maxLevelSize' level acc =
            let nextLevel = List.collect (fun (Node(_,c)) -> c) level
            let maxLevelSizeSoFar = max acc (List.length level)
            match List.length nextLevel with
            | 0 -> maxLevelSizeSoFar
            | _ -> maxLevelSize' nextLevel maxLevelSizeSoFar
        in maxLevelSize' [tree] 1

    let rec nodeDistanceCheck levelNodes =
        let validDistance =
            List.length levelNodes <= 1  ||
                let positions = List.map (fun (Node((_,p),_)) -> p) levelNodes |> List.sort
                Seq.zip positions (Seq.skip 1 positions) |> Seq.forall checkSpacing

        let nextLevel = List.collect childrenWithAbsolutePositionToParent levelNodes
        validDistance && (List.isEmpty nextLevel || nodeDistanceCheck nextLevel)

    let hasMultiNodeLevel = maxLevelSize tree > 1
    nodeDistanceCheck <| design {fst (getConfig()) with HorizontalSpacing = spacing} tree :: []
        |> Prop.classify (not hasMultiNodeLevel) "Trivial test"
        |> Prop.classify hasMultiNodeLevel "Contains multi node levels"

// Property 2
let parentIsCenteredOverOffsprings (Dist(spacing)) (tree: Tree<unit>) =
    let designedTree = design {fst (getConfig()) with HorizontalSpacing = spacing} tree
    let rec checkPositions (Node(_, children)) =
        let positions = List.map (fun (Node((_, p), _)) -> p) children
        let sum = if List.isEmpty positions then 0.0 else List.min positions + List.max positions
        floatsEquals sum 0.0 && List.forall checkPositions children

    let isTrivial (Node(_, children)) = List.isEmpty children

    checkPositions designedTree
    |> Prop.classify (isTrivial designedTree) "Trivial test"
    |> Prop.classify (not (isTrivial designedTree)) "Contains at least one parent node"

// Property 3
let treeHasReflectionalSymmetry (Dist(spacing)) (tree:Tree<unit>) =
    let rec mirrorTree (Node(v,c)) =
        Node(v, c |> List.map mirrorTree |> List.rev)

    let rec treeDegree (Node(_,c)) =
        max (List.length c) (List.map treeDegree c |> List.fold max 0)

    let positionedOriginalTree = design {fst (getConfig()) with HorizontalSpacing = spacing} tree
    let positionedMirroredTree = design {fst (getConfig()) with HorizontalSpacing = spacing} (mirrorTree tree)

    let areMirrored (Node((_,pOriginal),_), Node((_,pMirrored),_)) =
        floatsEquals pOriginal -pMirrored

    let rec hasReflectionalSymmetry nodeOriginal nodeMirrored =
        let (Node(_,originalChildren)) = nodeOriginal
        let (Node(_,mirroredChildren)) = nodeMirrored
        let mirroredPairs = List.zip originalChildren (List.rev mirroredChildren)
        areMirrored (nodeOriginal, nodeMirrored) && List.forall areMirrored mirroredPairs

    let degree = treeDegree tree
    in hasReflectionalSymmetry positionedOriginalTree positionedMirroredTree
        |> Prop.classify (degree <= 1) "Trivial"
        |> Prop.classify (degree > 1) "Branching tree"

type CustomGenerators =
    static member float() =
        {
            new Arbitrary<float>() with
            override x.Generator = Arb.generate<NormalFloat>
                                    |> Gen.map NormalFloat.op_Explicit
        }
    static member distance() =
        {
            new Arbitrary<Distance>() with
            override x.Generator = Arb.generate<NormalFloat>
                                    |> Gen.map NormalFloat.op_Explicit
                                    |> Gen.where ((<=) 0.0)
                                    |> Gen.map Dist
            override x.Shrinker f = match f with
                                    | Dist(x) when x = 0.0 -> seq [] // stop
                                    | Dist(x) when x <= 0.1 -> seq [Dist(0.0)] // try 0.0 when limit is reached
                                    | Dist(x) -> seq [Dist(x/2.0)] // half if above limit
        }


// Property 4
let identicalSubtreesAreRenderedIdentically (Dist(spacing)) (mainTree: Tree<unit>) (subTree: Tree<unit>) =
    let rec getRandomPath tree =
        let r = System.Random()
        match tree with
        | Node (_, []) -> []
        | Node (_, l) ->
            let randomNode = r.Next l.Length
            randomNode :: getRandomPath l.[randomNode]

    let rec insertAtPath subTree mainTree path =
        match mainTree, path with
        | _, [] -> subTree
        | Node(v, l), h::t -> Node(v, l |> List.mapi (fun i c -> if h=i then insertAtPath subTree c t else c))

    let insert subTree mainTree =
        let path = getRandomPath mainTree
        (insertAtPath subTree mainTree path, path)

    let (compositeTree, path) = insert subTree mainTree
    let designedCompositeTree = design {fst (getConfig()) with HorizontalSpacing = spacing} compositeTree
    let designedSubTree = design {fst (getConfig()) with HorizontalSpacing = spacing} subTree

    let rec findNodeByPath path tree =
        match tree, path with
        | node, [] -> node
        | Node (_, l), h::t -> findNodeByPath t l.[h]

    let identicalDesign (Node(_, l1)) (Node(_, l2)) =
        let rec equalPositions (Node((_,p1), l1), Node((_,p2), l2)) =
            floatsEquals p1 p2 && (List.zip l1 l2 |> List.forall equalPositions)
        List.zip l1 l2 |> List.forall equalPositions

    let designedIsomorphicSubTree = findNodeByPath path designedCompositeTree

    identicalDesign designedIsomorphicSubTree designedSubTree
        |> Prop.classify (path.Length = 0) "Trivial test"
        |> Prop.classify (path.Length > 0) "Nested subtree comparison"


let runAll () =
    printfn $"Running checks in {moduleName}..."

    let check prop = Check.One ({Config.QuickThrowOnFailure with QuietOnSuccess = true}, prop)
    let checkQuick lbl prop = Check.One ({Config.Quick with Name = lbl}, prop)

    Arb.register<CustomGenerators>() |> ignore
    check moveTreeMovesTree
    check moveExtentMovesAllPairs
    check mergedExtentsHasMaxLength
    check mergedExtentsCorrectPairMerge
    printfn "All basic checks are valid."

    checkQuick "Property 1 check: " nodesAtSameLevelShouldBeAtleastAGivenDistanceApart
    checkQuick "Property 2 check: " parentIsCenteredOverOffsprings
    checkQuick "Property 3 check: " treeHasReflectionalSymmetry
    checkQuick "Property 4 check: " identicalSubtreesAreRenderedIdentically
