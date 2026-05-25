module Lib
    ( readTheodoreFile
    ) where

import FOL
import Theodore
import qualified Data.List as List

data SourceLine = SourceLine { sourceLineNo :: Int
                             , sourceLineText :: String }

lineContext :: FilePath -> String -> SourceLine -> String
lineContext path label line =
    path ++ ":" ++ show (sourceLineNo line) ++ " (" ++ label ++ ")"

mergeDefinitions :: [SourceLine] -> [SourceLine]
mergeDefinitions [] = []
mergeDefinitions (l:ls)
    | startsWith "macro " l || startsWith "lemma " l =
        let isKeyword line = any (\keyword -> startsWith keyword line) ["macro ", "lemma ", "goal :", "assumption ", "proof"]
            (bodyLines, rest) = span (not . isKeyword) ls
            cleanBody = filter (not . null . trim) (map sourceLineText bodyLines)
            mergedLine = sourceLineText l ++ " " ++ unwords (map trim cleanBody)
        in SourceLine (sourceLineNo l) mergedLine : mergeDefinitions rest
    | otherwise = l : mergeDefinitions ls
  where
    startsWith prefix line = prefix `List.isPrefixOf` trim (sourceLineText line)

expandAllMacros :: [(String, [String], String)] -> String -> String
expandAllMacros macros s = 
    let s' = foldl (flip applyMacro) s macros
    in if s' == s then s' else expandAllMacros macros s'

readTheodoreFile :: FilePath -> IO (Assumptions, Lemmas, Proof, Goal)
readTheodoreFile path = do
    rawContent <- zipWith SourceLine [1..] . lines <$> readFile path
    let mergedContent = mergeDefinitions rawContent
    
    let startsWith prefix line = prefix `List.isPrefixOf` trim (sourceLineText line)
        macroLines = [l | l <- mergedContent, startsWith "macro " l]
        macros = map (\line -> parseMacroLineIn (lineContext path "macro" line) (sourceLineText line)) macroLines
        lemmaLines = [l | l <- mergedContent, startsWith "lemma " l]
        lemmas = map (\line -> parseLemmaLineIn (lineContext path "lemma" line) (expandAllMacros macros (sourceLineText line))) lemmaLines
    
    -- Split into pre-proof and proof section
    let isProof l = startsWith "proof" l
        (preProof, proofSection) = span (not . isProof) mergedContent
    if null proofSection
    then throwTheodoreError (TheodoreError
        "Syntax error"
        path
        "Missing proof section. Expected a line starting with: proof"
        Nothing
        Nothing
        [ "Add a proof section after assumptions and the goal."
        , "Example:\nproof\nExact h"
        ])
    else return ()
    
    -- Expand macros on pre-proof (assumptions, goal, etc.)
    let content = map (\line -> line { sourceLineText = expandAllMacros macros (sourceLineText line) }) preProof
        isKnownTopLevel line =
            null (trim (sourceLineText line))
            || startsWith "macro " line
            || startsWith "lemma " line
            || startsWith "goal :" line
            || startsWith "assumption " line
        unknownLines = filter (not . isKnownTopLevel) content
    if null unknownLines
    then return ()
    else let badLine = head unknownLines
          in throwTheodoreError (TheodoreError
            "Syntax error"
            (lineContext path "top-level declaration" badLine)
            "Unknown top-level declaration."
            (Just (sourceLineText badLine))
            Nothing
            [ "Expected one of: macro, lemma, assumption, goal :, or proof."
            , "Example: assumption h1 : A -> B"
            ])
    
    -- Extract proof body, then expand macros inside it. Lemmas remain as calls.
    let proofLine = head proofSection
        proofBody = unwords (map (trim . sourceLineText) (drop 1 proofSection))
        finalProof = expandAllMacros macros proofBody
    
    -- Parse goal and assumptions from expanded content
    let goalLines = [l | l <- content, startsWith "goal :" l]
    goalLine <- case goalLines of
        [l] -> return l
        []  -> throwTheodoreError (TheodoreError
            "Syntax error"
            path
            "Missing goal declaration. Expected exactly one line starting with: goal :"
            Nothing
            Nothing
            [ "Example: goal : B & B" ])
        _   -> throwTheodoreError (TheodoreError
            "Syntax error"
            path
            ("Multiple goal declarations found at lines: " ++ show (map sourceLineNo goalLines))
            Nothing
            Nothing
            [ "Keep exactly one goal : line in each .thd file." ])
    let goalFormula = parseFormulaIn (lineContext path "goal" goalLine) (drop 6 (trim (sourceLineText goalLine)))
        assumptionLines = [l | l <- content, startsWith "assumption " l]
        assumptions = map (\line -> parseAssumptionIn (lineContext path "assumption" line) (sourceLineText line)) assumptionLines

    return (assumptions, lemmas, parseProofIn (lineContext path "proof" proofLine) finalProof, mkGoal assumptions goalFormula)
