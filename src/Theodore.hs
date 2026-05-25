{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Theodore 
    ( Assumption ( Assumption )
    , Assumptions
    , Subgoal ( Subgoal )
    , Goal
    , Proof ( ToDo
            , Exact 
            , ImplI 
            , ConjI 
            , DisjlI
            , DisjrI
            , EqivI
            , NegI  
            , AllsI 
            , ExisI 
            , ImplE
            , ConjE 
            , DisjE 
            , EqivE
            , NegE  
            , AllsE 
            , ExisE )
    , mkGoal
    , apply
    , genLatexTree
    , parseFormula
    , parseAssumption
    , parseProof
    , trim
    , indentLevel
    , applyMacro
    , parseMacroLine
    , parseLemmaLine
    , applyLemma
    ) where

import FOL

import Debug.Trace (trace)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import qualified Data.List as List

data Assumption = Assumption { name     :: String 
                             , formula  :: Formula }

type Assumptions = [Assumption]

type MetaVars = [String]

data Subgoal = Subgoal { mvars :: MetaVars
                       , assms :: Assumptions 
                       , cncls :: Formula }

type Goal = [Subgoal]

data Proof = ToDo
           | Exact  { assmName  :: String }
           | ImplI  { assmName  :: String
                    , proof     :: Proof }
           | ConjI  { proofA    :: Proof 
                    , proofB    :: Proof }
           | DisjlI { proof     :: Proof }
           | DisjrI { proof     :: Proof }
           | EqivI  { assmName  :: String 
                    , proofA    :: Proof 
                    , proofB    :: Proof }
           | NegI   { assmName  :: String
                    , proof     :: Proof }
           | AllsI  { mvar      :: String
                    , proof     :: Proof }
           | ExisI  { mvar      :: String
                    , proof     :: Proof }
           | ImplE  { assmName  :: String
                    , proofA    :: Proof 
                    , proofB    :: Proof }
           | ConjE  { assmName  :: String
                    , proof     :: Proof }
           | DisjE  { assmName  :: String
                    , proofA    :: Proof
                    , proofB    :: Proof }
           | EqivE  { assmName  :: String
                    , proof     :: Proof }
           | NegE   { assmName  :: String
                    , proof     :: Proof }
           | AllsE  { mvar      :: String
                    , assmName  :: String
                    , proof     :: Proof }
           | ExisE  { mvar      :: String
                    , assmName  :: String
                    , proof     :: Proof }

data Token
    = TAll | TEx | TEquiv | TOr | TAnd | TNeg | TImpl
    | TTrue | TFalse
    | TLParen | TRParen | TColon | TComma
    | TIdent String
    deriving (Show, Eq)

type Parser a = [Token] -> Maybe (a, [Token])


instance Show Assumption where
    show assm = show (formula assm) ++ " (\ESC[32m" ++ (name assm) ++ "\ESC[0m)"

instance {-# OVERLAPS #-} Show Assumptions where
    show [] = ""
    show (a : as) = "\ESC[34m• \ESC[0m" ++ show a ++ "\n" ++ show as

instance {-# OVERLAPS #-} Show MetaVars where
    show [] = ""
    show (v : vs) = "\ESC[30m- \ESC[0m" ++ v ++ "\n" ++ show vs

instance {-# OVERLAPS #-} Show Subgoal where
    show subgoal = show (mvars subgoal) ++  show (assms subgoal) ++ "\ESC[34m⊢\ESC[0m " ++ show (cncls subgoal)

instance {-# OVERLAPS #-} Show Goal where
    show [] = "Nothing to prove!"
    show goals = "\ESC[1mGoal (" ++ show (length goals) ++ " subgoals):\ESC[0m" ++ show' 1 goals

instance Show Proof where
    show proof = showProof "" proof

showProof :: String -> Proof -> String
showProof append ToDo                       = append ++ "{! !}\n"
showProof append (Exact assm)               = append ++ "Exact (" ++ assm ++ ").\n"
showProof append (ImplI assm proof)         = append ++ "→I (" ++ assm ++ ").\n" ++ showProof append proof
showProof append (ConjI proofA proofB)      = append ++ "∧I:\n" ++ showProof (append ++ "  ")  proofA ++ showProof append proofB
showProof append (DisjlI proof)             = append ++ "∨lI.\n" ++ showProof append proof
showProof append (DisjrI proof)             = append ++ "∨rI.\n" ++ showProof append proof
showProof append (NegI assm proof)          = append ++ "¬I (" ++ assm ++ ").\n" ++ showProof append proof
showProof append (EqivI assm proofA proofB) = append ++ "↔I (" ++ assm ++ "):\n" ++ showProof (append ++ "  ") proofA ++ showProof append proofB
showProof append (AllsI mvar proof)         = append ++ "∀I (" ++ mvar ++ ").\n" ++ showProof append proof
showProof append (ExisI mvar proof)         = append ++ "∃I (" ++ mvar ++ ").\n" ++ showProof append proof
showProof append (ImplE assm proofA proofB) = append ++ "→E (" ++ assm ++ "):\n" ++ showProof (append ++ "  ") proofA ++ showProof append proofB
showProof append (ConjE assm proof)         = append ++ "∧E (" ++ assm ++ ").\n" ++ showProof append proof
showProof append (DisjE assm proofA proofB) = append ++ "∨E (" ++ assm ++ "):\n" ++ showProof (append ++ "  ") proofA ++ showProof append proofB
showProof append (NegE assm proof)          = append ++ "¬E (" ++ assm ++ ").\n" ++ showProof append proof
showProof append (EqivE assm proof)         = append ++ "↔E (" ++ assm ++ ").\n" ++ showProof append proof
showProof append (AllsE mvar assm proof)    = append ++ "∀E (" ++ mvar ++ ", " ++ assm ++ ").\n" ++ showProof append proof
showProof append (ExisE mvar assm proof)    = append ++ "∃E (" ++ mvar ++ ", " ++ assm ++ ").\n" ++ showProof append proof

show' :: Int -> Goal -> String
show' _ [] = ""
show' n (g : gs) = "\n\n\ESC[32m" ++ show n ++ ". subgoal\ESC[0m\n" ++ show g ++ show' (n + 1) gs

find :: String -> Assumptions -> Assumption
find assmName []        = error $ assmName ++ " not in assumptions!"
find assmName (a : as)  = 
    if (name a) == assmName then a else find assmName as

member :: String -> Assumptions -> Bool
member assmName assms   = any ((== assmName) . name) assms

lookup :: String -> Assumptions -> Maybe Assumption
lookup assmName []      = Nothing
lookup assmName (a : as)= 
    if (name a) == assmName then Just a else Theodore.lookup assmName as

delete :: String -> Assumptions -> Assumptions
delete assmName []      = error $ assmName ++ " not in assumptions!"
delete assmName (a : as)= 
    if (name a) == assmName then as else a : delete assmName as

mkGoal :: Assumptions -> Formula -> Goal
mkGoal assms cncls = [ Subgoal [] assms cncls ]

-- Apply assumption
exact :: String -> Goal -> Goal
exact assmName []       = error "Nothing to apply exact to!"
exact assmName (g : gs) = 
    if member assmName (assms g) then gs else error "Invalid rule!"

-- Apply implI
intro :: String -> Goal -> Goal
intro assmName []       = error "Nothing to apply intro to!"
intro assmName (g : gs) = case (cncls g) of
    Impl f1 f2  -> Subgoal (mvars g) (Assumption assmName f1 : assms g) f2 : gs
    _           -> error "Invalid rule!"

-- Apply conjI
tear :: Goal -> Goal
tear []         = error "Nothing to apply exact to!"
tear (g : gs)   = case (cncls g) of
    Conj f1 f2  -> Subgoal (mvars g) (assms g) f1 
                 : Subgoal (mvars g) (assms g) f2 
                 : gs
    _           -> error "Invalid rule!"

-- Apply disjI left
left :: Goal -> Goal
left []         = error "Nothing to apply exact to!"
left (g : gs)   = case (cncls g) of
    Disj f1 f2  -> Subgoal (mvars g) (assms g) f1 : gs
    _           -> error "Invalid rule!"

-- Apply disjI right
right :: Goal -> Goal
right []        = error "Nothing to apply exact to!"
right (g : gs)  = case (cncls g) of
    Disj f1 f2  -> Subgoal (mvars g) (assms g) f2 : gs
    _           -> error "Invalid rule!"

-- Apply eqivI
iff :: String -> Goal -> Goal
iff assmName []       = error "Nothing to apply iff to!"
iff assmName (g : gs) = case (cncls g) of
    Eqiv f1 f2  -> Subgoal (mvars g) (Assumption assmName f1 : assms g) f2 
                 : Subgoal (mvars g) (Assumption assmName f2 : assms g) f1 
                 : gs
    _           -> error "Invalid rule!"

-- Apply negI
false :: String -> Goal -> Goal
false assmName []       = error "Nothing to apply false to!"
false assmName (g : gs) = case (cncls g) of
    Neg f       -> Subgoal (mvars g) (Assumption assmName f  : assms g) Bot 
                 : gs
    _           -> error "Invalid rule!"

-- Apply allI
free :: String -> Goal -> Goal
free mvar [] = error "Nothing to apply free to!"
free mvar (g : gs) = case (cncls g) of
    Alls x f    -> if (notElem mvar (mvars g)) 
                   then Subgoal 
                            (mvar : mvars g) 
                            (assms g) 
                            (substVar x mvar f) 
                      : gs
                   else error "Invalid rule!"
    _           -> error "Invalid rule!"

-- Apply exI
set :: String -> Goal -> Goal
set mvar [] = error "Nothing to apply set to!"
set mvar (g : gs) = case (cncls g) of
    Exis x f    -> Subgoal
                       (mvars g)
                       (assms g)
                       (substVar x mvar f)
                   : gs
    _           -> error "Invalid rule!"

-- Apply conjE
split :: String -> Goal -> Goal
split assmName []       = error "Nothing to apply split to!"
split assmName (g : gs) = Subgoal (mvars g) (split' (assms g)) (cncls g) : gs
    where split' []         = error "Invalid rule!"
          split' (a : as)   = 
            if (name a) == assmName 
                then (split'' a) ++ as 
                else a : split' as
          split'' assm      = case (formula assm) of
            Conj f1 f2      -> [ Assumption (name assm ++ "1") f1
                               , Assumption (name assm ++ "2") f2 ]
            _               -> error "Invalid rule!"

-- Apply disjE
cases :: String -> Goal -> Goal
cases assmName []       = error "Nothing to apply cases to!"
cases assmName (g : gs) = Subgoal (mvars g) (left' (assms g)) (cncls g) 
                        : Subgoal (mvars g) (right'(assms g)) (cncls g) 
                        : gs
    where left' []          = error "Invalid rule!"
          left' (a : as)    = 
            if (name a) == assmName
                then left'' a : as
                else a : left' as
          left'' assm   = case (formula assm) of
            Disj f1 f2      -> Assumption assmName f1
            _               -> error "Invalid rule!"
          right' []         = error "Invalid rule!"
          right' (a : as)   =
            if (name a) == assmName
                then right'' a : as
                else a : right' as
          right'' assm      = case (formula assm) of
            Disj f1 f2      -> Assumption assmName f2
            _               -> error "Invalid rule!"

-- Apply impE
have :: String -> Goal -> Goal
have assmName []       = error "Nothing to apply have to!"
have assmName (g : gs) = Subgoal (mvars g) (delete assmName (assms g)) (left' (assms g))
                        : Subgoal (mvars g) (right' (assms g)) (cncls g)
                        : gs
    where left' as          =  
            let f = find assmName as
             in case (formula f) of
                Impl f1 f2  -> f1
                _           -> error "Invalid rule!"
          right' []         = error "Invalid rule!"
          right' (a : as)   = 
            if (name a) == assmName 
                then (right'' a) : as
                else a : right' as
          right'' assm      = case (formula assm) of
            Impl f1 f2      -> Assumption (name assm) f2
            _               -> error "Invalid rule!"

-- Apply eqivE
equiv :: String -> Goal -> Goal
equiv assmName []       = error "Nothing to apply equiv to!"
equiv assmName (g : gs) = Subgoal (mvars g) (split' (assms g)) (cncls g) : gs
    where split' []         = error "Invalid rule!"
          split' (a : as)   = 
            if (name a) == assmName
                then (split'' a) ++ as
                else a : split' as
          split'' assm      = case (formula assm) of
            Eqiv f1 f2      -> [ Assumption (name assm ++ "1") (Impl f1 f2)
                               , Assumption (name assm ++ "2") (Impl f2 f1) ]
            _               -> error "Invalid rule!"

-- Apply notE
turn :: String -> Goal -> Goal
turn assmName []        = error "Nothing to applt turn to!"
turn assmName (g : gs)  = Subgoal 
                            (mvars g) 
                            (delete assmName (assms g)) 
                            (subneg (assms g)) 
                        : gs
    where subneg as     = 
            let f = find assmName as 
             in case (formula f) of
                Neg f1  -> f1
                _       -> error "Invalid rule!"

-- Apply allE
fix :: String -> String -> Goal -> Goal
fix mvar assmName [] = error "Nothing to apply fix to!"
fix mvar assmName (g : gs) = Subgoal (mvars g) (fix' (assms g)) (cncls g) : gs
    where fix' [] = error "Invalid rule!"
          fix' (a : as) =
            if (name a) == assmName
                then case (formula a) of
                    Alls x f -> Assumption assmName (substVar x mvar f) : as
                    _        -> error "Invalid rule!"
                else a : fix' as

-- Apply exE
gen :: String -> String -> Goal -> Goal
gen mvar assmName [] = error "Nothing to apply gen to!"
gen mvar assmName (g : gs) = Subgoal 
                                (insert (assms g)) 
                                (gen' (assms g)) 
                                (cncls g)
                            : gs
    where gen' [] = error "Invalid rule!"
          gen' (a : as) =
            if (name a) == assmName
                then case (formula a) of
                    Exis x f -> if notElem mvar (mvars g)
                                then Assumption assmName (substVar x mvar f) : as
                                else error "Invalid rule!"
                    _        -> error "Invalid rule!"
                else a : gen' as
          insert as = 
            case Theodore.lookup assmName as of
                (Just a) -> case (formula a) of
                    Exis x f -> if notElem mvar (mvars g)
                                then mvar : (mvars g)
                                else error "Invalid rule!"
                    _        -> error "Invalid rule!"
                Nothing  -> error "Invalid rule!"

apply :: Proof -> Goal -> Goal
apply ToDo                          goal = goal
apply (Exact assm)                  goal = exact assm goal
apply (ImplI assm proof)            goal = apply proof (intro assm goal)
apply (ConjI proofA proofB)         goal = apply proofB (apply proofA (tear goal))
apply (DisjlI proof)                goal = apply proof (left goal)
apply (DisjrI proof)                goal = apply proof (right goal)
apply (EqivI assm proofA proofB)    goal = apply proofB (apply proofA (iff assm goal))
apply (NegI assm proof)             goal = apply proof (false assm goal)
apply (AllsI mvar proof)            goal = apply proof (free mvar goal)
apply (ExisI mvar proof)            goal = apply proof (set mvar goal)
apply (ImplE assm proofA proofB)    goal = apply proofB (apply proofA (have assm goal))
apply (ConjE assm proof)            goal = apply proof (split assm goal)
apply (DisjE assm proofA proofB)    goal = apply proofB (apply proofA (cases assm goal))
apply (EqivE assm proof)            goal = apply proof (equiv assm goal)
apply (NegE assm proof)             goal = apply proof (turn assm goal)
apply (AllsE mvar assm proof)       goal = apply proof (fix mvar assm goal)
apply (ExisE mvar assm proof)       goal = apply proof (gen mvar assm goal)

mathTexMetaVars :: MetaVars -> String
mathTexMetaVars mvars = List.intercalate ", " mvars

mathTexAssumption :: Assumption -> String
mathTexAssumption (Assumption _ f) = mathTexFormula f

mathTexAssumptions :: Assumptions -> String
mathTexAssumptions assms = (List.intercalate ", " . map mathTexAssumption) assms

mathTexSubgoal :: Subgoal -> String
mathTexSubgoal (Subgoal [] assms cncls)     = mathTexAssumptions assms 
                                           ++ " \\vdash " 
                                           ++ mathTexFormula cncls
mathTexSubgoal (Subgoal mvars assms cncls)  = mathTexMetaVars mvars 
                                           ++ "; " 
                                           ++ mathTexAssumptions assms 
                                           ++ " \\vdash " 
                                           ++ mathTexFormula cncls

latexTree :: Proof -> Goal -> String
latexTree ToDo                          _    = "%ToDo\n"
latexTree (Exact assm)                  goal = "\\AxiomC{}\n\\RightLabel{$\\mathsf{asm}$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (ImplI assm proof)            goal = latexTree proof (intro assm goal) 
                                            ++ "\\RightLabel{$\\implies_I$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (ConjI proofA proofB)         goal = latexTree proofB (apply proofA (tear goal))
                                            ++ latexTree proofA (tear goal) 
                                            ++ "\\RightLabel{$\\land_I$}\n\\BinaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (DisjlI proof)                goal = latexTree proof (left goal)
                                            ++ "\\RightLabel{$\\lor_{I_l}$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (DisjrI proof)                goal = latexTree proof (right goal)
                                            ++ "\\RightLabel{$\\lor_{I_r}$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (EqivI assm proofA proofB)    goal = latexTree proofB (apply proofA (iff assm goal))
                                            ++ latexTree proofA (iff assm goal) 
                                            ++ "\\RightLabel{$\\equiv_I$}\n\\BinaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (NegI assm proof)             goal = latexTree proof (false assm goal)
                                            ++ "\\RightLabel{$\\neg_I$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (AllsI mvar proof)            goal = latexTree proof (free mvar goal)
                                            ++ "\\RightLabel{$\\forall_I(" ++ mvar ++ ")$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (ExisI mvar proof)            goal = latexTree proof (set mvar goal)
                                            ++ "\\RightLabel{$\\exists_I(" ++ mvar ++ ")$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (ImplE assm proofA proofB)    goal = latexTree proofB (apply proofA (have assm goal))
                                            ++ latexTree proofA (have assm goal)
                                            ++ "\\RightLabel{$\\implies_E$}\n\\BinaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n" 
latexTree (ConjE assm proof)            goal = latexTree proof (split assm goal)
                                            ++ "\\RightLabel{$\\land_E$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (DisjE assm proofA proofB)    goal = latexTree proofB (apply proofA (cases assm goal))
                                            ++ latexTree proofA (cases assm goal) 
                                            ++ "\\RightLabel{$\\lor_E$}\n\\BinaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n" 
latexTree (EqivE assm proof)            goal = latexTree proof (equiv assm goal)
                                            ++ "\\RightLabel{$\\equiv_E$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (NegE assm proof)             goal = latexTree proof (turn assm goal)
                                            ++ "\\RightLabel{$\\neg_E$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (AllsE mvar assm proof)       goal = latexTree proof (fix mvar assm goal)
                                            ++ "\\RightLabel{$\\forall_E(" ++ mvar ++ ")$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTree (ExisE mvar assm proof)       goal = latexTree proof (gen mvar assm goal)
                                            ++ "\\RightLabel{$\\exists_E(" ++ mvar ++ ")$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"

genLatexTree :: Proof -> Goal -> IO ()
genLatexTree proof goal = 
    case apply proof goal of
    []  -> do
        putStr "\\begin{prooftree}\n"
        putStr (latexTree proof goal)
        putStr "\\end{prooftree}\n"
    _   -> error "Invalid proof!"

-- This part is added for parsing .thd files!

-- =========================================
-- User-friendly .thd assumption parser
-- =========================================

parseQuantVar :: String -> (String, String)
parseQuantVar s  =
    let ws = words s
    in case ws of
        (v:rest) -> (v, unwords rest)
        _        -> error "Invalid Quantifier"

splitAtOperator :: String -> String -> (String, String)
splitAtOperator op s = go 0 s ""
  where
    n = length op
    go _ "" acc = error $ "Operator " ++ op ++ " not found in " ++ s
    go depth rem@(c:cs) acc
        | take n rem == op && depth == 0 = (acc, drop n rem)
        | c == '(' = go (depth + 1) cs (acc ++ [c])
        | c == ')' = go (depth - 1) cs (acc ++ [c])
        | otherwise = go depth cs (acc ++ [c])

splitCommaTopLevel :: String -> [String]
splitCommaTopLevel s = go s 0 "" []
  where
    go [] _ acc res = res ++ [acc | not (null acc)]
    go (c:cs) depth acc res
        | c == ',' && depth == 0 = go cs depth "" (res ++ [acc])
        | c == '(' = go cs (depth + 1) (acc ++ [c]) res
        | c == ')' = go cs (depth - 1) (acc ++ [c]) res
        | otherwise = go cs depth (acc ++ [c]) res

indentLevel :: String -> Int
indentLevel = length . takeWhile isSpace

extractIndented :: [String] -> ([String], [String])
extractIndented = span (\l -> indentLevel l > 0)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = 
        -- Extract words that are alphanumeric + underscores
        let (word, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
        in case word of
            "All"   -> TAll   : lexer rest
            "Ex"    -> TEx    : lexer rest
            "True"  -> TTrue  : lexer rest
            "False" -> TFalse : lexer rest
            _       -> TIdent word : lexer rest
    | otherwise = case (c:cs) of
        ('<':'-':'>':rest) -> TEquiv : lexer rest
        ('-':'>':rest)     -> TImpl  : lexer rest
        ('&':rest)         -> TAnd   : lexer rest
        ('|':rest)         -> TOr    : lexer rest
        ('~':rest)         -> TNeg   : lexer rest
        (':':rest)         -> TColon : lexer rest
        ('(':rest)         -> TLParen : lexer rest
        (')':rest)         -> TRParen : lexer rest
        (',':rest)         -> TComma : lexer rest
        _                  -> error ("Lexer error: Unexpected character '" ++ [c] ++ "'")

expect :: Token -> [Token] -> [Token]
expect t (x:xs) | t == x = xs
expect t xs = error $ "Expected token: " ++ show t ++ ", got: " ++ show xs

-- Level 6: Equivalence (Lowest Precedence)
parseEquiv :: Parser Formula
parseEquiv tokens = do
    (f1, rest) <- parseImpl tokens
    case rest of
        (TEquiv : rest') -> do
            (f2, rest'') <- parseEquiv rest'
            return (Eqiv f1 f2, rest'')
        _ -> return (f1, rest)

-- Level 5: Implication
parseImpl :: Parser Formula
parseImpl tokens = do
    (f1, rest) <- parseDisj tokens
    case rest of
        (TImpl : rest') -> do
            (f2, rest'') <- parseImpl rest'
            return (Impl f1 f2, rest'')
        _ -> return (f1, rest)

-- Level 4: Disjunction
parseDisj :: Parser Formula
parseDisj tokens = do
    (f1, rest) <- parseConj tokens
    case rest of
        (TOr : rest') -> do
            (f2, rest'') <- parseDisj rest'
            return (Disj f1 f2, rest'')
        _ -> return (f1, rest)

-- Level 3: Conjunction
parseConj :: Parser Formula
parseConj tokens = do
    (f1, rest) <- parseUnary tokens
    case rest of
        (TAnd : rest') -> do
            (f2, rest'') <- parseConj rest'
            return (Conj f1 f2, rest'')
        _ -> return (f1, rest)

-- Level 2: Unary (Negation, Quantifiers)
parseUnary :: Parser Formula
parseUnary (TNeg : rest) = do
    (f, rest') <- parseUnary rest
    return (Neg f, rest')
parseUnary (TAll : TIdent x : TColon : rest) = do
    (f, rest') <- parseUnary rest
    return (Alls x f, rest')
parseUnary (TEx : TIdent x : TColon : rest) = do
    (f, rest') <- parseUnary rest
    return (Exis x f, rest')
parseUnary tokens = parseAtom tokens

-- Level 1: Atoms, Constants, and Parentheses (Highest Precedence)
parseAtom :: Parser Formula
parseAtom (TTrue : rest)  = Just (Top, rest)
parseAtom (TFalse : rest) = Just (Bot, rest)
parseAtom (TLParen : rest) = do
    (f, rest') <- parseEquiv rest  -- Loop back up for stuff inside parens
    case rest' of
        (TRParen : rest'') -> Just (f, rest'')
        _ -> Nothing
parseAtom (TIdent name : TLParen : rest) = do
    (args, rest') <- parseTerms rest
    case rest' of
        (TRParen : rest'') -> Just (Rel name args, rest'')
        _ -> Nothing
parseAtom (TIdent name : rest) = Just (Rel name [], rest)
parseAtom _ = Nothing

-- Term Parsers (for predicates like P(x, y))
parseTerms :: Parser [Term]
parseTerms tokens = do
    (t, rest) <- parseTerm tokens
    case rest of
        (TComma : rest') -> do
            (ts, rest'') <- parseTerms rest'
            return (t : ts, rest'')
        _ -> Just ([t], rest)

parseTerm :: Parser Term
parseTerm (TLParen : rest) = do
    (t, rest1) <- parseTerm rest
    case rest1 of
        (TRParen : rest2) -> Just (t, rest2)
        _ -> Nothing
parseTerm (TIdent fname : TLParen : rest) = do
    (args, rest') <- parseTerms rest
    case rest' of
        (TRParen : rest'') -> Just (Fun fname args, rest'')
        _ -> Nothing
parseTerm (TIdent x : rest) = Just (Var x, rest)
parseTerm _ = Nothing


-- The Main Entry Point for Formulas
parseFormula :: String -> Formula
parseFormula s = 
    case parseEquiv (lexer s) of
        Just (f, []) -> f
        Just (_, ts) -> error ("Parse error. Unconsumed tokens: " ++ show ts ++ " in formula: " ++ s)
        Nothing      -> error ("Failed to parse formula: " ++ s)

-- The Updated Assumption Parser
parseAssumption :: String -> Assumption
parseAssumption line = 
    let rest = drop (length "assumption ") line
        (namePart, formulaPart) = break (== ':') rest
        name = trim namePart
        formulaStr = drop 1 formulaPart -- No need to trim formulaStr, lexer ignores spaces
    in Assumption name (parseFormula formulaStr)

-- The internal Recursive DEscent parser for Proofs
parseE :: Parser Proof

-- ==========================================
-- 1. Base Cases (Leaves)
-- ==========================================

parseE (TIdent "Exact" : TIdent h : rest) = Just (Exact h, rest)
parseE (TIdent "ToDo" : rest)             = Just (ToDo, rest)

-- ==========================================
-- 2. Rules with 1 Sub-proof
-- Syntax: RuleName [AssmName] "(" E ")"
-- ==========================================

parseE (TIdent "ImplI" : TIdent h : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (ImplI h p, rest2); _ -> Nothing

parseE (TIdent "ConjE" : TIdent h : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (ConjE h p, rest2); _ -> Nothing

parseE (TIdent "DisjlI" : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (DisjlI p, rest2); _ -> Nothing

parseE (TIdent "DisjrI" : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (DisjrI p, rest2); _ -> Nothing

parseE (TIdent "NegI" : TIdent h : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (NegI h p, rest2); _ -> Nothing

parseE (TIdent "NegE" : TIdent h : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (NegE h p, rest2); _ -> Nothing

-- ==========================================
-- 3. Rules with 2 Sub-proofs
-- Syntax: RuleName [AssmName] "(" E1 "," E2 ")"
-- ==========================================

parseE (TIdent "ImplE" : TIdent h : TLParen : rest) = do
    (p1, rest1) <- parseE rest
    case rest1 of
        (TComma : rest2) -> do
            (p2, rest3) <- parseE rest2
            case rest3 of (TRParen : rest4) -> Just (ImplE h p1 p2, rest4); _ -> Nothing
        _ -> Nothing

parseE (TIdent "ConjI" : TLParen : rest) = do
    (p1, rest1) <- parseE rest
    case rest1 of
        (TComma : rest2) -> do
            (p2, rest3) <- parseE rest2
            case rest3 of (TRParen : rest4) -> Just (ConjI p1 p2, rest4); _ -> Nothing
        _ -> Nothing

parseE (TIdent "DisjE" : TIdent h : TLParen : rest) = do
    (p1, rest1) <- parseE rest
    case rest1 of
        (TComma : rest2) -> do
            (p2, rest3) <- parseE rest2
            case rest3 of (TRParen : rest4) -> Just (DisjE h p1 p2, rest4); _ -> Nothing
        _ -> Nothing

-- ==========================================
-- 4. Equivalence Rules
-- ==========================================
parseE (TIdent "EqivI" : TIdent h : TLParen : rest) = do
    (p1, rest1) <- parseE rest
    case rest1 of
        (TComma : rest2) -> do
            (p2, rest3) <- parseE rest2
            case rest3 of (TRParen : rest4) -> Just (EqivI h p1 p2, rest4); _ -> Nothing
        _ -> Nothing

parseE (TIdent "EqivE" : TIdent h : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (EqivE h p, rest2); _ -> Nothing

-- ==========================================
-- 5. Quantifier Rules
-- ==========================================
parseE (TIdent "AllsI" : TIdent x : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (AllsI x p, rest2); _ -> Nothing

-- Note: AllsE requires a term to substitute. It calls `parseTerm` from the formula parser.
parseE (TIdent "AllsE" : TIdent x : TIdent h : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (AllsE x h p, rest2); _ -> Nothing

-- ExisI requires the witness term
parseE (TIdent "ExisI" : TIdent x : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (ExisI x p, rest2); _ -> Nothing

-- ExisE introduces a new variable and assumption name for the subproof
parseE (TIdent "ExisE" : TIdent x : TIdent h_new : TLParen : rest) = do
    (p, rest1) <- parseE rest
    case rest1 of (TRParen : rest2) -> Just (ExisE x h_new p, rest2); _ -> Nothing

-- Catch-all for syntax errors
parseE _ = Nothing


parseProof :: String -> Proof
parseProof s = 
    case parseE (lexer s) of
        Just (p, []) -> p
        Just (_, ts) -> error $ "Parse error in proof. Unconsumed tokens starting at: " ++ show (take 5 ts) ++ "\nIn proof: " ++ s
        Nothing      -> error $ "Failed to parse proof block. Check your syntax and parentheses.\nIn proof: " ++ s

-- Parsing macros

parseMacroLine :: String -> (String, [String], String)
parseMacroLine line = 
    let rest = drop 6 line -- drop "macro "
        (lhs, rhs) = break (== '=') rest
        body = trim (drop 1 rhs)
        (namePart, argsPart) = break (== '(') (trim lhs)
        args = if null argsPart 
               then [] 
               else splitArgs (init (tail argsPart)) -- drop the ( and )
    in (trim namePart, map trim args, body)

applyMacro :: (String, [String], String) -> String -> String
applyMacro (name, params, body) input = go input
  where
    go [] = []
    go s@(c:cs)
        | name `List.isPrefixOf` s =
            let afterName = drop (length name) s
            in if not (null afterName) && head afterName == '('
               then let (argsStr, rest) = extractParenBlock (tail afterName) 0 ""
                        args = splitArgs argsStr
                        expandedBody = foldl (\b (p, a) -> replaceWord p a b) body (zip params args)
                    in expandedBody ++ go rest
               else if null params && (null afterName || not (isIdChar (head afterName)))
                    then "(" ++ body ++ ")" ++ go afterName
                    else name ++ go afterName
        | isIdChar c =
            let (idPart, rest) = span isIdChar s
            in idPart ++ go rest
        | otherwise = c : go cs

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_'

isIdStart :: Char -> Bool
isIdStart c = isAlpha c || c == '_'

replaceWord :: String -> String -> String -> String
replaceWord _ _ [] = []
replaceWord search replace str@(c:cs)
    | search `List.isPrefixOf` str = 
        let after = drop (length search) str
        in if null after || not (isAlphaNum (head after) || head after == '_')
           then replace ++ replaceWord search replace after
           else c : replaceWord search replace cs
    | isAlphaNum c || c == '_' = 
        let (word, rest) = span (\x -> isAlphaNum x || x == '_') str
        in word ++ replaceWord search replace rest
    | otherwise = c : replaceWord search replace cs

extractParenBlock :: String -> Int -> String -> (String, String)
extractParenBlock [] _ acc = (reverse acc, [])
extractParenBlock (')':cs) 0 acc = (reverse acc, cs)
extractParenBlock (')':cs) n acc = extractParenBlock cs (n-1) (')':acc)
extractParenBlock ('(':cs) n acc = extractParenBlock cs (n+1) ('(':acc)
extractParenBlock (c:cs) n acc = extractParenBlock cs n (c:acc)

splitArgs :: String -> [String]
splitArgs s = go 0 [] [] s
  where
    go _ acc [] cur = reverse (reverse cur : acc)   -- each cur is reversed, so we reverse later
    go depth acc cur (c:cs)
        | c == '(' && depth == 0 = go (depth+1) acc (c:cur) cs
        | c == ')' && depth == 1 = go (depth-1) acc (c:cur) cs
        | c == ',' && depth == 0 = go depth (reverse cur : acc) [] cs
        | otherwise = go depth acc (c:cur) cs


trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseLemmaLine :: String -> (String, [String], String)
parseLemmaLine line = 
    let rest = drop 6 line -- drop "lemma "
        (lhs, rhs) = break (== '=') rest
        body = trim (drop 1 rhs)
        (namePart, argsPart) = break (== '(') (trim lhs)
        args = if null argsPart 
               then [] 
               else splitArgs (init (tail argsPart))
    in (trim namePart, map trim args, body)

applyLemma :: (String, [String], String) -> String -> String
applyLemma (name, params, body) input = go input
  where
    go [] = []
    go s@(c:cs)
        | name `List.isPrefixOf` s =
            let afterName = drop (length name) s
            in if not (null afterName) && head afterName == '('
               then let (argsStr, rest) = extractParenBlock (tail afterName) 0 ""
                        args = splitArgs argsStr
                        expandedBody = foldl (\b (p, a) -> replaceWord p a b) body (zip params args)
                    in expandedBody ++ go rest
               else if null params && (null afterName || not (isIdChar (head afterName)))
                    then body ++ go afterName   -- no parentheses
                    else name ++ go afterName
        | isIdChar c =
            let (idPart, rest) = span isIdChar s
            in idPart ++ go rest
        | otherwise = c : go cs