{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Theodore 
    ( Assumption ( Assumption )
    , Assumptions
    , Lemma ( Lemma )
    , Lemmas
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
            , ExisE
            , Call )
    , mkGoal
    , apply
    , applyWithLemmas
    , genLatexTree
    , genLatexTreeWithLemmas
    , TheodoreError ( TheodoreError )
    , throwTheodoreError
    , parseFormulaIn
    , parseFormula
    , parseAssumptionIn
    , parseAssumption
    , parseProofIn
    , parseProof
    , trim
    , indentLevel
    , applyMacro
    , parseMacroLineIn
    , parseMacroLine
    , parseLemmaLineIn
    , parseLemmaLine
    ) where

import FOL

import Data.Char (isSpace, isAlpha, isAlphaNum)
import qualified Data.List as List

data Assumption = Assumption { name     :: String 
                             , formula  :: Formula }

type Assumptions = [Assumption]

data Lemma = Lemma { lemmaId     :: String
                   , lemmaParams :: [String]
                   , lemmaBody   :: Proof }

type Lemmas = [Lemma]

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
           | Call   { callName  :: String
                    , callArgs  :: [String] }

data Token
    = TAll | TEx | TEquiv | TOr | TAnd | TNeg | TImpl
    | TTrue | TFalse
    | TLParen | TRParen | TColon | TComma
    | TIdent String
    deriving (Show, Eq)

type Parser a = [Token] -> Maybe (a, [Token])

data TheodoreError =
    TheodoreError { errTitle   :: String
                  , errContext :: String
                  , errMessage :: String
                  , errSource  :: Maybe String
                  , errColumn  :: Maybe Int
                  , errHints   :: [String] }

formatTheodoreError :: TheodoreError -> String
formatTheodoreError err =
    errTitle err ++ contextLine ++ "\n"
    ++ errMessage err
    ++ sourceBlock
    ++ hintsBlock
  where
    contextLine =
        if null (errContext err) then "" else " in " ++ errContext err
    sourceBlock =
        case errSource err of
            Nothing -> ""
            Just source ->
                "\n\n" ++ source
                ++ case errColumn err of
                    Nothing -> ""
                    Just col -> "\n" ++ replicate (max 0 (col - 1)) ' ' ++ "^"
    hintsBlock =
        case errHints err of
            [] -> ""
            hints -> "\n\nHints:\n" ++ unlines (map ("- " ++) hints)

throwTheodoreError :: TheodoreError -> a
throwTheodoreError = error . formatTheodoreError

syntaxError :: String -> String -> Maybe String -> Maybe Int -> [String] -> a
syntaxError context message source col hints =
    throwTheodoreError (TheodoreError "Syntax error" context message source col hints)

lexicalError :: String -> String -> String -> Int -> [String] -> a
lexicalError context message source col hints =
    throwTheodoreError (TheodoreError "Lexical error" context message (Just source) (Just col) hints)

prettyTokens :: [Token] -> String
prettyTokens [] = "<end of input>"
prettyTokens tokens = List.intercalate " " (map prettyToken (take 8 tokens))

prettyToken :: Token -> String
prettyToken TAll = "All"
prettyToken TEx = "Ex"
prettyToken TEquiv = "<->"
prettyToken TOr = "|"
prettyToken TAnd = "&"
prettyToken TNeg = "~"
prettyToken TImpl = "->"
prettyToken TTrue = "True"
prettyToken TFalse = "False"
prettyToken TLParen = "("
prettyToken TRParen = ")"
prettyToken TColon = ":"
prettyToken TComma = ","
prettyToken (TIdent value) = value

formulaHints :: [String]
formulaHints =
    [ "Atoms look like A, P(x), or Rel(x, y)."
    , "Use ~, &, |, ->, <-> for logical operators."
    , "Use quantifiers as All x : formula or Ex x : formula."
    , "Wrap formulas in parentheses when precedence is unclear."
    ]

proofHints :: [String]
proofHints =
    [ "Examples: Exact h, ImplE h (Exact a, Exact h), ConjI (p1, p2)."
    , "Lemma calls look like name(arg1, arg2)."
    , "Rule names are case-sensitive."
    , "Every opening parenthesis must have a matching closing parenthesis."
    ]


instance Show Assumption where
    show assm = show (formula assm) ++ " (\ESC[32m" ++ (name assm) ++ "\ESC[0m)"

instance Show Lemma where
    show (Lemma lemmaName params body) =
        "lemma " ++ lemmaName ++ "(" ++ List.intercalate ", " params ++ ") =\n"
        ++ "-- params: " ++ show params ++ " (" ++ show (length params) ++ ")\n"
        ++ show body

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
showProof append (Call name args)           = append ++ name ++ "(" ++ List.intercalate ", " args ++ ").\n"

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
    case Theodore.lookup assmName (assms g) of
        Just assm | formula assm == cncls g -> gs
        _                                   -> error "Invalid rule!"

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
apply = applyWithLemmas []

applyWithLemmas :: Lemmas -> Proof -> Goal -> Goal
applyWithLemmas _      ToDo                          goal = goal
applyWithLemmas _      (Exact assm)                  goal = exact assm goal
applyWithLemmas lemmas (ImplI assm proof)            goal = applyWithLemmas lemmas proof (intro assm goal)
applyWithLemmas lemmas (ConjI proofA proofB)         goal = applyWithLemmas lemmas proofB (applyWithLemmas lemmas proofA (tear goal))
applyWithLemmas lemmas (DisjlI proof)                goal = applyWithLemmas lemmas proof (left goal)
applyWithLemmas lemmas (DisjrI proof)                goal = applyWithLemmas lemmas proof (right goal)
applyWithLemmas lemmas (EqivI assm proofA proofB)    goal = applyWithLemmas lemmas proofB (applyWithLemmas lemmas proofA (iff assm goal))
applyWithLemmas lemmas (NegI assm proof)             goal = applyWithLemmas lemmas proof (false assm goal)
applyWithLemmas lemmas (AllsI mvar proof)            goal = applyWithLemmas lemmas proof (free mvar goal)
applyWithLemmas lemmas (ExisI mvar proof)            goal = applyWithLemmas lemmas proof (set mvar goal)
applyWithLemmas lemmas (ImplE assm proofA proofB)    goal = applyWithLemmas lemmas proofB (applyWithLemmas lemmas proofA (have assm goal))
applyWithLemmas lemmas (ConjE assm proof)            goal = applyWithLemmas lemmas proof (split assm goal)
applyWithLemmas lemmas (DisjE assm proofA proofB)    goal = applyWithLemmas lemmas proofB (applyWithLemmas lemmas proofA (cases assm goal))
applyWithLemmas lemmas (EqivE assm proof)            goal = applyWithLemmas lemmas proof (equiv assm goal)
applyWithLemmas lemmas (NegE assm proof)             goal = applyWithLemmas lemmas proof (turn assm goal)
applyWithLemmas lemmas (AllsE mvar assm proof)       goal = applyWithLemmas lemmas proof (fix mvar assm goal)
applyWithLemmas lemmas (ExisE mvar assm proof)       goal = applyWithLemmas lemmas proof (gen mvar assm goal)
applyWithLemmas lemmas (Call name args)              goal = applyLemmaCall lemmas name args goal

findLemma :: String -> Lemmas -> Lemma
findLemma name [] = throwTheodoreError (TheodoreError
    "Lemma call error"
    ("lemma call " ++ name)
    ("Lemma " ++ show name ++ " was not found.")
    Nothing
    Nothing
    [ "Check the lemma name and make sure it is declared before the proof section."
    , "Lemma calls look like name(arg1, arg2)."
    ])
findLemma name (lemma : lemmas) =
    if lemmaId lemma == name then lemma else findLemma name lemmas

instantiateLemma :: Lemmas -> String -> [String] -> Subgoal -> (Proof, Goal)
instantiateLemma lemmas name args g =
    let lemma = findLemma name lemmas
        params = lemmaParams lemma
     in if length params == length args
        then let bindings = zip params args
                 localAssms = concatMap (localAssumption (assms g)) bindings
                 substitutions = filter (not . isAssumptionArg (assms g) . snd) bindings
                 localProof = substProofNames substitutions (lemmaBody lemma)
              in (localProof, [Subgoal (mvars g) localAssms (cncls g)])
        else throwTheodoreError (TheodoreError
            "Lemma call error"
            ("lemma call " ++ name)
            ("Wrong number of arguments for lemma " ++ name
                ++ ". Expected " ++ show (length params)
                ++ ", got " ++ show (length args) ++ ".")
            Nothing
            Nothing
            [ "Lemma parameters: " ++ show params
            , "Call arguments: " ++ show args
            , "Matched lemma:\n" ++ show lemma
            ])
  where
    localAssumption as (param, arg) =
        case Theodore.lookup arg as of
            Just assm -> [Assumption param (formula assm)]
            Nothing   -> []
    isAssumptionArg as arg =
        case Theodore.lookup arg as of
            Just _  -> True
            Nothing -> False

applyLemmaCall :: Lemmas -> String -> [String] -> Goal -> Goal
applyLemmaCall _      name _    []       = error $ "Nothing to apply lemma " ++ name ++ " to!"
applyLemmaCall lemmas name args (g : gs) =
    let (localProof, localGoal) = instantiateLemma lemmas name args g
     in case applyWithLemmas lemmas localProof localGoal of
        []        -> gs
        remaining -> error $
            "Lemma " ++ name ++ " did not prove the current goal. Remaining local subgoals:\n"
            ++ show remaining

substProofNames :: [(String, String)] -> Proof -> Proof
substProofNames env ToDo = ToDo
substProofNames env (Exact assm) = Exact (substName env assm)
substProofNames env (ImplI assm proof) = ImplI (substName env assm) (substProofNames env proof)
substProofNames env (ConjI proofA proofB) = ConjI (substProofNames env proofA) (substProofNames env proofB)
substProofNames env (DisjlI proof) = DisjlI (substProofNames env proof)
substProofNames env (DisjrI proof) = DisjrI (substProofNames env proof)
substProofNames env (EqivI assm proofA proofB) = EqivI (substName env assm) (substProofNames env proofA) (substProofNames env proofB)
substProofNames env (NegI assm proof) = NegI (substName env assm) (substProofNames env proof)
substProofNames env (AllsI mvar proof) = AllsI (substName env mvar) (substProofNames env proof)
substProofNames env (ExisI mvar proof) = ExisI (substName env mvar) (substProofNames env proof)
substProofNames env (ImplE assm proofA proofB) = ImplE (substName env assm) (substProofNames env proofA) (substProofNames env proofB)
substProofNames env (ConjE assm proof) = ConjE (substName env assm) (substProofNames env proof)
substProofNames env (DisjE assm proofA proofB) = DisjE (substName env assm) (substProofNames env proofA) (substProofNames env proofB)
substProofNames env (EqivE assm proof) = EqivE (substName env assm) (substProofNames env proof)
substProofNames env (NegE assm proof) = NegE (substName env assm) (substProofNames env proof)
substProofNames env (AllsE mvar assm proof) = AllsE (substName env mvar) (substName env assm) (substProofNames env proof)
substProofNames env (ExisE mvar assm proof) = ExisE (substName env mvar) (substName env assm) (substProofNames env proof)
substProofNames env (Call name args) = Call name (map (substName env) args)

substName :: [(String, String)] -> String -> String
substName env value =
    case List.lookup value env of
        Just value' -> value'
        Nothing     -> value

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
latexTree = latexTreeWithLemmas []

latexTreeWithLemmas :: Lemmas -> Proof -> Goal -> String
latexTreeWithLemmas _      ToDo                          _    = "%ToDo\n"
latexTreeWithLemmas _      (Exact assm)                  goal = "\\AxiomC{}\n\\RightLabel{$\\mathsf{asm}$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (ImplI assm proof)            goal = latexTreeWithLemmas lemmas proof (intro assm goal)
                                            ++ "\\RightLabel{$\\implies_I$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (ConjI proofA proofB)         goal = latexTreeWithLemmas lemmas proofB (applyWithLemmas lemmas proofA (tear goal))
                                            ++ latexTreeWithLemmas lemmas proofA (tear goal)
                                            ++ "\\RightLabel{$\\land_I$}\n\\BinaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (DisjlI proof)                goal = latexTreeWithLemmas lemmas proof (left goal)
                                            ++ "\\RightLabel{$\\lor_{I_l}$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (DisjrI proof)                goal = latexTreeWithLemmas lemmas proof (right goal)
                                            ++ "\\RightLabel{$\\lor_{I_r}$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (EqivI assm proofA proofB)    goal = latexTreeWithLemmas lemmas proofB (applyWithLemmas lemmas proofA (iff assm goal))
                                            ++ latexTreeWithLemmas lemmas proofA (iff assm goal)
                                            ++ "\\RightLabel{$\\equiv_I$}\n\\BinaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (NegI assm proof)             goal = latexTreeWithLemmas lemmas proof (false assm goal)
                                            ++ "\\RightLabel{$\\neg_I$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (AllsI mvar proof)            goal = latexTreeWithLemmas lemmas proof (free mvar goal)
                                            ++ "\\RightLabel{$\\forall_I(" ++ mvar ++ ")$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (ExisI mvar proof)            goal = latexTreeWithLemmas lemmas proof (set mvar goal)
                                            ++ "\\RightLabel{$\\exists_I(" ++ mvar ++ ")$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (ImplE assm proofA proofB)    goal = latexTreeWithLemmas lemmas proofB (applyWithLemmas lemmas proofA (have assm goal))
                                            ++ latexTreeWithLemmas lemmas proofA (have assm goal)
                                            ++ "\\RightLabel{$\\implies_E$}\n\\BinaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n" 
latexTreeWithLemmas lemmas (ConjE assm proof)            goal = latexTreeWithLemmas lemmas proof (split assm goal)
                                            ++ "\\RightLabel{$\\land_E$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (DisjE assm proofA proofB)    goal = latexTreeWithLemmas lemmas proofB (applyWithLemmas lemmas proofA (cases assm goal))
                                            ++ latexTreeWithLemmas lemmas proofA (cases assm goal)
                                            ++ "\\RightLabel{$\\lor_E$}\n\\BinaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n" 
latexTreeWithLemmas lemmas (EqivE assm proof)            goal = latexTreeWithLemmas lemmas proof (equiv assm goal)
                                            ++ "\\RightLabel{$\\equiv_E$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (NegE assm proof)             goal = latexTreeWithLemmas lemmas proof (turn assm goal)
                                            ++ "\\RightLabel{$\\neg_E$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (AllsE mvar assm proof)       goal = latexTreeWithLemmas lemmas proof (fix mvar assm goal)
                                            ++ "\\RightLabel{$\\forall_E(" ++ mvar ++ ")$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (ExisE mvar assm proof)       goal = latexTreeWithLemmas lemmas proof (gen mvar assm goal)
                                            ++ "\\RightLabel{$\\exists_E(" ++ mvar ++ ")$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas lemmas (Call name args)              goal@(g : _) =
    let (localProof, localGoal) = instantiateLemma lemmas name args g
     in latexTreeWithLemmas lemmas localProof localGoal
        ++ "\\RightLabel{$\\mathsf{lemma}\\ " ++ name ++ "$}\n\\UnaryInfC{$" ++ mathTexSubgoal (head goal) ++ "$}\n"
latexTreeWithLemmas _      (Call name _)                 [] = error $ "Nothing to apply lemma " ++ name ++ " to!"

genLatexTree :: Proof -> Goal -> IO ()
genLatexTree = genLatexTreeWithLemmas []

genLatexTreeWithLemmas :: Lemmas -> Proof -> Goal -> IO ()
genLatexTreeWithLemmas lemmas proof goal =
    case applyWithLemmas lemmas proof goal of
    []  -> do
        putStr "\\begin{prooftree}\n"
        putStr (latexTreeWithLemmas lemmas proof goal)
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
lexer = lexerIn "input"

lexerIn :: String -> String -> [Token]
lexerIn context source = go 1 source
  where
    go _ [] = []
    go col (c:cs)
        | isSpace c = go (col + 1) cs
        | isAlpha c =
            let (word, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
                token = case word of
                    "All"   -> TAll
                    "Ex"    -> TEx
                    "True"  -> TTrue
                    "False" -> TFalse
                    _       -> TIdent word
             in token : go (col + length word) rest
        | otherwise = case (c:cs) of
            ('<':'-':'>':rest) -> TEquiv  : go (col + 3) rest
            ('-':'>':rest)     -> TImpl   : go (col + 2) rest
            ('&':rest)         -> TAnd    : go (col + 1) rest
            ('|':rest)         -> TOr     : go (col + 1) rest
            ('~':rest)         -> TNeg    : go (col + 1) rest
            (':':rest)         -> TColon  : go (col + 1) rest
            ('(':rest)         -> TLParen : go (col + 1) rest
            (')':rest)         -> TRParen : go (col + 1) rest
            (',':rest)         -> TComma  : go (col + 1) rest
            _                  -> lexicalError context
                                    ("Unexpected character " ++ show [c] ++ ".")
                                    source
                                    col
                                    [ "Identifiers must start with a letter."
                                    , "Supported symbols are (, ), :, ,, ~, &, |, ->, and <->."
                                    ]

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
    (f, rest') <- parseEquiv rest
    return (Alls x f, rest')
parseUnary (TEx : TIdent x : TColon : rest) = do
    (f, rest') <- parseEquiv rest
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
parseFormula = parseFormulaIn "formula"

parseFormulaIn :: String -> String -> Formula
parseFormulaIn context s =
    case parseEquiv (lexerIn context s) of
        Just (f, []) -> f
        Just (_, ts) -> syntaxError
            context
            ("Unexpected tokens after a complete formula: " ++ prettyTokens ts)
            (Just s)
            Nothing
            formulaHints
        Nothing      -> syntaxError
            context
            "Could not parse this formula."
            (Just s)
            Nothing
            formulaHints

-- The Updated Assumption Parser
parseAssumption :: String -> Assumption
parseAssumption = parseAssumptionIn "assumption"

parseAssumptionIn :: String -> String -> Assumption
parseAssumptionIn context line =
    let line' = trim line
        rest = drop (length "assumption ") line'
        (namePart, formulaPart) = break (== ':') rest
        name = trim namePart
     in case formulaPart of
        [] -> syntaxError
            context
            "Assumption declarations must contain ':' between the name and formula."
            (Just line)
            Nothing
            [ "Example: assumption h1 : A -> B" ]
        (_:formulaStr)
            | null name -> syntaxError
                context
                "Assumption name is empty."
                (Just line)
                Nothing
                [ "Example: assumption h1 : A -> B" ]
            | null (trim formulaStr) -> syntaxError
                context
                ("Assumption " ++ show name ++ " is missing a formula.")
                (Just line)
                Nothing
                [ "Example: assumption h1 : A -> B" ]
            | otherwise -> Assumption name (parseFormulaIn (context ++ " formula") formulaStr)

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

-- Lemma calls
parseE (TIdent name : TLParen : rest) = do
    (args, rest1) <- parseCallArgList rest
    return (Call name args, rest1)

-- Catch-all for syntax errors
parseE _ = Nothing

parseCallArgList :: Parser [String]
parseCallArgList (TRParen : rest) = Just ([], rest)
parseCallArgList (TIdent arg : TComma : rest) = do
    (args, rest1) <- parseCallArgList rest
    return (arg : args, rest1)
parseCallArgList (TIdent arg : TRParen : rest) = Just ([arg], rest)
parseCallArgList _ = Nothing


parseProof :: String -> Proof
parseProof = parseProofIn "proof"

parseProofIn :: String -> String -> Proof
parseProofIn context s =
    case parseE (lexerIn context s) of
        Just (p, []) -> p
        Just (_, ts) -> syntaxError
            context
            ("Unexpected tokens after a complete proof expression: " ++ prettyTokens ts)
            (Just s)
            Nothing
            proofHints
        Nothing      -> syntaxError
            context
            "Could not parse this proof expression."
            (Just s)
            Nothing
            proofHints

-- Parsing macros

parseMacroLine :: String -> (String, [String], String)
parseMacroLine = parseMacroLineIn "macro"

parseMacroLineIn :: String -> String -> (String, [String], String)
parseMacroLineIn context line =
    let line' = trim line
        rest = drop 6 line' -- drop "macro "
        (lhs, rhs) = break (== '=') rest
        (namePart, argsPart) = break (== '(') (trim lhs)
        name = trim namePart
     in case rhs of
        [] -> syntaxError
            context
            "Macro declarations must contain '=' between the header and body."
            (Just line)
            Nothing
            [ "Example: macro RULE(X) = P(X) -> Q(X)" ]
        (_:bodyText)
            | null name -> syntaxError
                context
                "Macro name is empty."
                (Just line)
                Nothing
                [ "Example: macro RULE(X) = P(X) -> Q(X)" ]
            | null (trim bodyText) -> syntaxError
                context
                ("Macro " ++ show name ++ " is missing a body.")
                (Just line)
                Nothing
                [ "Example: macro RULE(X) = P(X) -> Q(X)" ]
            | otherwise ->
                let args = parseDefinitionArgsIn context line argsPart
                 in (name, map trim args, trim bodyText)

parseDefinitionArgsIn :: String -> String -> String -> [String]
parseDefinitionArgsIn context source argsPart =
    if null argsPart
    then []
    else let (rawArgs, rest) = extractParenBlockIn context source (tail argsPart)
             args = parseCallArgs rawArgs
          in if not (null (trim rest))
             then syntaxError
                context
                ("Unexpected text after argument list: " ++ show (trim rest))
                (Just source)
                Nothing
                [ "The definition header should look like name(arg1, arg2)." ]
             else if any (null . trim) args
             then syntaxError
                context
                "Argument list contains an empty argument."
                (Just source)
                Nothing
                [ "Example: lemma name(arg1, arg2) = proof" ]
             else args

parseCallArgs :: String -> [String]
parseCallArgs s =
    if null (trim s)
    then []
    else map trim (splitArgs s)

substituteTemplate :: String -> [String] -> [String] -> String -> String
substituteTemplate name params args body =
    if length params == length args
    then replaceWords (zip params args) body
    else throwTheodoreError (TheodoreError
        "Macro expansion error"
        ("macro call " ++ name)
        ("Wrong number of arguments for " ++ name
            ++ ". Expected " ++ show (length params)
            ++ ", got " ++ show (length args) ++ ".")
        Nothing
        Nothing
        [ "Macro parameters: " ++ show params
        , "Call arguments: " ++ show args
        ])

replaceWords :: [(String, String)] -> String -> String
replaceWords _ [] = []
replaceWords replacements input@(c:_)
    | isIdChar c =
        let (word, rest) = span isIdChar input
            replacement = case List.lookup word replacements of
                Just value -> value
                Nothing    -> word
        in replacement ++ replaceWords replacements rest
    | otherwise = c : replaceWords replacements (tail input)

applyMacro :: (String, [String], String) -> String -> String
applyMacro (name, params, body) input = go input
  where
    go [] = []
    go s@(c:cs)
        | name `List.isPrefixOf` s =
            let afterName = drop (length name) s
            in if not (null afterName) && head afterName == '('
               then let (argsStr, rest) = extractParenBlock (tail afterName) 0 ""
                        args = parseCallArgs argsStr
                        expandedBody = substituteTemplate name params args body
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

extractParenBlock :: String -> Int -> String -> (String, String)
extractParenBlock [] _ acc = (reverse acc, [])
extractParenBlock (')':cs) 0 acc = (reverse acc, cs)
extractParenBlock (')':cs) n acc = extractParenBlock cs (n-1) (')':acc)
extractParenBlock ('(':cs) n acc = extractParenBlock cs (n+1) ('(':acc)
extractParenBlock (c:cs) n acc = extractParenBlock cs n (c:acc)

extractParenBlockIn :: String -> String -> String -> (String, String)
extractParenBlockIn context source = go 0 ""
  where
    go _ acc [] = syntaxError
        context
        "Missing closing ')' in argument list."
        (Just source)
        Nothing
        [ "Example: lemma name(arg1, arg2) = proof" ]
    go 0 acc (')':cs) = (reverse acc, cs)
    go depth acc (')':cs) = go (depth - 1) (')':acc) cs
    go depth acc ('(':cs) = go (depth + 1) ('(':acc) cs
    go depth acc (c:cs) = go depth (c:acc) cs

splitArgs :: String -> [String]
splitArgs s = go 0 [] [] s
  where
    go _ acc cur [] = reverse (reverse cur : acc)
    go depth acc cur (c:cs)
        | c == '(' = go (depth + 1) acc (c:cur) cs
        | c == ')' && depth > 0 = go (depth - 1) acc (c:cur) cs
        | c == ',' && depth == 0 = go depth (reverse cur : acc) [] cs
        | otherwise = go depth acc (c:cur) cs


trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseLemmaLine :: String -> Lemma
parseLemmaLine = parseLemmaLineIn "lemma"

parseLemmaLineIn :: String -> String -> Lemma
parseLemmaLineIn context line =
    let line' = trim line
        rest = drop 6 line' -- drop "lemma "
        (lhs, rhs) = break (== '=') rest
        (namePart, argsPart) = break (== '(') (trim lhs)
        lemmaName = trim namePart
     in case rhs of
        [] -> syntaxError
            context
            "Lemma declarations must contain '=' between the header and proof body."
            (Just line)
            Nothing
            [ "Example: lemma mp(hImp, hBase) = ImplE hImp (Exact hBase, Exact hImp)" ]
        (_:bodyText)
            | null lemmaName -> syntaxError
                context
                "Lemma name is empty."
                (Just line)
                Nothing
                [ "Example: lemma mp(hImp, hBase) = ImplE hImp (Exact hBase, Exact hImp)" ]
            | null (trim bodyText) -> syntaxError
                context
                ("Lemma " ++ show lemmaName ++ " is missing a proof body.")
                (Just line)
                Nothing
                [ "Example: lemma mp(hImp, hBase) = ImplE hImp (Exact hBase, Exact hImp)" ]
            | otherwise ->
                let params = map trim (parseDefinitionArgsIn context line argsPart)
                    body = trim bodyText
                 in Lemma lemmaName params (parseProofIn (context ++ " body") body)
