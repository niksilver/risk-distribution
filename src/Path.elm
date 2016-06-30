module Path exposing (Path(Path), Instruction(M, L), d)


-- Module for managing SVG path descriptions


type Instruction
    = M Float Float
    | L Float Float

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
        M x y ->
            "M " ++ (toString x) ++ "," ++ (toString y)
        L x y ->
            "L " ++ (toString x) ++ "," ++ (toString y)

maybeSpace : List Instruction -> String
maybeSpace instrs =
    if (List.isEmpty instrs) then "" else " "

