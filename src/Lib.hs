module Lib
    ( readTheodoreFile
    ) where

import FOL
import Theodore
import qualified Data.List as List

mergeDefinitions :: [String] -> [String]
mergeDefinitions [] = []
mergeDefinitions (l:ls)
    | "macro " `List.isPrefixOf` l || "lemma " `List.isPrefixOf` l =
        let isKeyword line = any (`List.isPrefixOf` line) ["macro ", "lemma ", "goal :", "assumption ", "proof"]
            (bodyLines, rest) = span (not . isKeyword) ls
            cleanBody = filter (not . null . trim) bodyLines
            mergedLine = l ++ " " ++ unwords (map trim cleanBody)
        in mergedLine : mergeDefinitions rest
    | otherwise = l : mergeDefinitions ls

expandAllMacros :: [(String, [String], String)] -> String -> String
expandAllMacros macros s = 
    let s' = foldl (flip applyMacro) s macros
    in if s' == s then s' else expandAllMacros macros s'

expandAllLemmas :: [(String, [String], String)] -> String -> String
expandAllLemmas lemmas s = 
    let s' = foldl (flip applyLemma) s lemmas
    in if s' == s then s' else expandAllLemmas lemmas s'


readTheodoreFile :: FilePath -> IO (Assumptions, Proof, Goal)
readTheodoreFile path = do
    rawContent <- lines <$> readFile path
    let mergedContent = mergeDefinitions rawContent
    
    let macroLines = [l | l <- mergedContent, "macro " `List.isPrefixOf` l]
        macros = map parseMacroLine macroLines
        lemmaLines = [l | l <- mergedContent, "lemma " `List.isPrefixOf` l]
        lemmas = map parseLemmaLine lemmaLines
    
    -- Split into pre-proof and proof section
    let isProof l = "proof" `List.isPrefixOf` l
        (preProof, proofSection) = span (not . isProof) mergedContent
    
    -- Expand macros on pre-proof (assumptions, goal, etc.)
    let content = map (expandAllMacros macros) preProof
    
    -- Extract proof body, expand lemmas, then expand macros again inside it
    let proofBody = unwords (map trim (drop 1 proofSection))
        expandedProof = expandAllLemmas lemmas proofBody
        finalProof = expandAllMacros macros expandedProof
    
    -- Parse goal and assumptions from expanded content
    let goalLine = head [l | l <- content, "goal :" `List.isPrefixOf` l]
        goalFormula = parseFormula (drop 6 (trim goalLine))
        assumptionLines = [l | l <- content, "assumption " `List.isPrefixOf` l]
        assumptions = map parseAssumption assumptionLines
    
    return (assumptions, parseProof finalProof, mkGoal assumptions goalFormula)




