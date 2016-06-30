module Path exposing (Path(Path), M(M), d)

-- Module for managing SVG path descriptions

type M = M Float Float

type Path = Path (List M)

d : Path -> String
d (Path instrs) =
    case instrs of
        [] -> ""
        head :: tail -> dInstr head ++ d (Path tail)

dInstr : M -> String
dInstr (M x y) =
    "M " ++ (toString x) ++ "," ++ (toString y)
