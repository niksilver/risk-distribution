module Path exposing
    ( Path (Path)
    , Instruction (M, M', L, L', H, H', V, V')
    , d
    )


-- Module for managing SVG path descriptions


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

