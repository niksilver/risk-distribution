module Path exposing
    ( Path (Path)
    , Instruction (M, M', L, L', H, H', V, V')
    , map
    , d
    , fromPosList
    )

import Spline exposing (Pos)

-- Module for managing SVG path descriptions.


-- Each instruction is from the d element of <path d="...">
-- Relative intructions such as m, l, v and h are represented
-- by M', L', V, H', etc.

type Instruction
    = M  Float Float
    | M' Float Float
    | L  Float Float
    | L' Float Float
    | H  Float
    | H' Float
    | V  Float
    | V' Float

type Path = Path (List Instruction)


d : Path -> String
d (Path instrs) =
    case instrs of
        [] ->
            ""
        head :: tail ->
            dInstr head ++ (maybeSpace tail) ++ d (Path tail)

dInstr : Instruction -> String
dInstr instr =
    case instr of
        M  x y -> dTwoPart "M" x y
        M' x y -> dTwoPart "m" x y
        L  x y -> dTwoPart "L" x y
        L' x y -> dTwoPart "l" x y
        H  x -> dOnePart "H" x
        H' x -> dOnePart "h" x
        V  y -> dOnePart "V" y
        V' y -> dOnePart "v" y

maybeSpace : List Instruction -> String
maybeSpace instrs =
    if (List.isEmpty instrs) then "" else " "

dTwoPart : String -> Float -> Float -> String
dTwoPart i x y =
        i ++ " " ++ (toString x) ++ "," ++ (toString y)

dOnePart : String -> Float -> String
dOnePart i a =
        i ++ " " ++ (toString a)

-- Map a path.
-- Because the mapping may not be linear the instructions are all changed
-- to be absolute.
-- For example, Path [M 1 2, H' 3] draws a line from (1, 2) to (4, 2)
-- but if we map it with a function (\x y -> (x+y, x)) then we get
-- a line from (3, 1) to (6, 4). As a result the mapped path becomes
-- Path [M 3 1, L 6 4].

type alias PosFn = Float -> Float -> Pos

map : PosFn -> Path -> Path
map fn (Path instrs) =
    Path (map' (Pos 0 0) fn instrs)

-- Map the list of instructions, keeping track of the original
-- cursor position at each step

map' : Pos -> PosFn -> List Instruction -> List Instruction
map' pos fn instrs =
    case instrs of
        [] ->
            []
        head :: tail ->
            let
                (newPos, mappedInstr) = mapInstr pos fn head
            in
                mappedInstr :: map' newPos fn tail

mapInstr : Pos -> PosFn -> Instruction -> (Pos, Instruction)
mapInstr pos fn instr =
    case instr of
        M x y ->
            let
                pos = Pos x y
                pos' = fn x y
            in
                (pos, M pos'.x pos'.y)
        L x y ->
            let
                pos = Pos x y
                pos' = fn x y
            in
                (pos, L pos'.x pos'.y)
        H x ->
            mapInstr pos fn (L x pos.y)
        V y ->
            mapInstr pos fn (L pos.x y)
        M' dx dy ->
            mapInstr pos fn (M (pos.x + dx) (pos.y + dy))
        L' dx dy ->
            mapInstr pos fn (L (pos.x + dx) (pos.y + dy))
        H' dx ->
            mapInstr pos fn (L (pos.x + dx) pos.y)
        V' dy ->
            mapInstr pos fn (L pos.x (pos.y + dy))

-- Map [Pos 1 2, Pos 3 4, Pos 5 6, ...]
-- to [ M 1 2, L 3 4, L 5 6, ...]

fromPosList : List Pos -> Path
fromPosList ps =
    let
        -- Map [Pos a b, Pos c d,...] to [L a b, L c d,...]
        toLs tail =
            List.map (\pos -> L pos.x pos.y) tail
    in
        case ps of
            [] ->
                Path []
            head :: tail ->
                Path (M head.x head.y :: toLs tail)

