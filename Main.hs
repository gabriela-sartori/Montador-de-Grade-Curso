{-
    Ideia do `Rodrigo Madera`, ele fez uma versão em C++ (https://github.com/madera/Schedulator/blob/master/schedulator.cxx)
    então resolvi fazer a minha versão em 
-}

import Control.Monad

data Dia        = SEG | TER | QUA | QUI | SEX | SAB | DOM deriving (Eq, Show)
data Hora       = H_0730_0900 | H_0905_1035 | H_1040_1210 | H_1830_2000 | H_2005_2135 | H_2140_2310 deriving (Eq, Show)
type Horario    = (Dia, Hora)
data Curso      = Curso { cursoNome :: String, cursoHorarios :: [Horario]  } deriving (Eq)
data CursoGrade = CursoGrade { cursoGradeNome :: String, cursoGradeClasses :: [[Horario]] }
data Grade      = Grade { gradeCursos :: [CursoGrade], gradeMeusHorariosDisponiveis :: [Horario] }

horarioToStr :: Horario -> String
horarioToStr (dia, hora) = "(" ++ dia' ++ " " ++ hora' ++ ")" where
    dia' = case dia of
                    SEG -> "Segunda"
                    TER -> "Terça  "
                    QUA -> "Quarta "
                    QUI -> "Quinta "
                    SEX -> "Sexta  "
                    SAB -> "Sábado "
                    DOM -> "Domingo"
    hora' = case hora of
                    H_0730_0900 -> " 7:30 -  9:00"
                    H_0905_1035 -> " 9:05 - 10:35"
                    H_1040_1210 -> "10:40 - 12:10"
                    H_1830_2000 -> "18:30 - 20:00"
                    H_2005_2135 -> "20:05 - 21:35"
                    H_2140_2310 -> "21:40 - 23:10"

(~>) :: a -> (a -> b) -> b ; (~>) a b = b a

todosOsDias :: [Dia]
todosOsDias = [SEG, TER, QUA, QUI, SEX, SAB, DOM]

todasAsHoras :: [Hora]
todasAsHoras  = [H_0730_0900, H_0905_1035, H_1040_1210, H_1830_2000, H_2005_2135, H_2140_2310]

-- Retorna todos os horários disponíveis
todosOsHorarios :: [Horario]
todosOsHorarios = [(dia, horario) | dia <- todosOsDias, horario <- todasAsHoras]

{- Gera turmas com todos os horarios daquela grade do curso -}
getCursosByCursoGrade :: CursoGrade -> [Curso]
getCursosByCursoGrade (CursoGrade { cursoGradeNome = nome, cursoGradeClasses = horarios }) = map (Curso nome) horarios

-- Pega uma Turma dentre varias Turmas de um curso da grade e retorna o curso
getCursoByCursoGrade :: CursoGrade -> Int -> Curso
getCursoByCursoGrade (CursoGrade { cursoGradeNome = nome, cursoGradeClasses = variosHorarios }) index = Curso nome (variosHorarios !! index)

-- Se dentre cada curso, nenhum curso tem o mesmo horario dentre os horarios dos outros cursos
nenhumCursoRepeteHorario :: [Curso] -> Bool
nenhumCursoRepeteHorario [] = True
nenhumCursoRepeteHorario (Curso { cursoHorarios = horarios}:cursos) = let
        horariosRepetidos = filter (\curso -> any (`elem` curso~>cursoHorarios) horarios) cursos
    in
        null horariosRepetidos && nenhumCursoRepeteHorario cursos

-- Dado um curso, todos os horarios precisam estar entre os horarios desejados a se ter aula
todosOsHorariosBatem :: [Horario] -> Curso -> Bool
todosOsHorariosBatem horarios curso = all (`elem` horarios) (curso~>cursoHorarios)

permute :: Eq a => [[a]] -> [[a]]
permute [] = [[]]
permute (list:lists) = [x : y | x <- list, y <- permute lists]

-- Retorna combinações de grade possíveis que não possuem conflito
getGradesDisponiveis :: Grade -> [[Curso]]
getGradesDisponiveis (Grade { gradeCursos = cursosDaGrade, gradeMeusHorariosDisponiveis = meusHorarios }) =
    let
        -- Permutacoes de [Curso], que seria a minha grade pessoal
        permutacoes :: [[Curso]]
        permutacoes = permute $ map getCursosByCursoGrade cursosDaGrade
    in
        filter (\cursos -> nenhumCursoRepeteHorario cursos && all (todosOsHorariosBatem meusHorarios) cursos) permutacoes

desenharGradeCursos :: Grade -> (Int, [Curso]) -> IO ()
desenharGradeCursos grade (solucao, cursos) = let
        nomes :: [String]
        nomes = map cursoGradeNome (grade~>gradeCursos)
    in  do
        putStrLn $ "Solução " ++ show solucao ++ ":"
        forM_ nomes (\nome -> do
            let horarios = concatMap (unwords . map horarioToStr . cursoHorarios) . filter ((== nome) . cursoNome) $ cursos
            putStr $ nome ++ ":\t" ++ horarios ++ "\n")
        putStrLn "\n"

main :: IO ()
main = do
    let gradesDisponiveis = getGradesDisponiveis minhaGrade
    putStrLn $ "Soluções encontradas: " ++ show (length gradesDisponiveis)
    void $ mapM_ (desenharGradeCursos minhaGrade) (zip [1..] gradesDisponiveis)


{- Configure aqui seus cursos e sua grade -}

_APD3 :: CursoGrade
_APD3 = CursoGrade "APD3" [ [(SEX, H_2005_2135)], [(SEG, H_2140_2310)], [(TER, H_2140_2310)]]

_IHM :: CursoGrade
_IHM = CursoGrade "IHM" [[(SEG, H_2005_2135)], [(QUI, H_1830_2000)], [(SEG, H_1830_2000)], [(SEG, H_1830_2000)]]

_BD1 :: CursoGrade
_BD1 = CursoGrade "BD1" [ [(QUA, H_2140_2310), (TER, H_2005_2135)]
                        , [(QUI, H_1830_2000), (TER, H_2005_2135)]
                        , [(QUA, H_1830_2000), (TER, H_2140_2310)]
                        , [(SEX, H_2140_2310), (TER, H_2140_2310)]
                        , [(TER, H_1830_2000), (QUI, H_2005_2135)]
                        ]
_ES2 :: CursoGrade
_ES2 = CursoGrade "ES2" [ [(QUI, H_1830_2000), (QUI, H_2140_2310)]
                        , [(QUA, H_2140_2310), (QUI, H_2140_2310)]
                        , [(SEG, H_1830_2000), (SEX, H_1830_2000)]
                        , [(TER, H_1830_2000), (SEX, H_1830_2000)]
                        ]
_SO :: CursoGrade
_SO = CursoGrade "SO"  [ [(QUA, H_2005_2135), (SEG, H_1830_2000)]
                        , [(TER, H_2140_2310), (SEG, H_1830_2000)]
                        , [(SEX, H_2005_2135), (SEX, H_2140_2310)]
                        , [(QUA, H_1830_2000), (SEX, H_2005_2135)]
                        ]
_LP3 :: CursoGrade
_LP3 = CursoGrade "LP3" [ [(TER, H_2140_2310), (SEG, H_2140_2310), (QUA, H_1830_2000)]
                        , [(QUA, H_1830_2000), (QUA, H_2005_2135), (SEG, H_2140_2310)]
                        , [(TER, H_1830_2000), (TER, H_2005_2135), (SEG, H_2005_2135)]
                        , [(SEG, H_1830_2000), (SEG, H_2005_2135), (TER, H_2005_2135)]
                        ]
_PARADIGMAS :: CursoGrade
_PARADIGMAS = CursoGrade "PLP" [[(SEX, H_2140_2310), (TER, H_1830_2000)]]

minhaGrade :: Grade
minhaGrade = Grade [_APD3, _IHM, _BD1, _ES2, _SO, _LP3, _PARADIGMAS] todosOsHorarios
