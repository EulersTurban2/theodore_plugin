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


readTheodoreFile :: FilePath -> IO (Assumptions, Proof, Goal)
readTheodoreFile path = do
    rawContent <- lines <$> readFile path

    let mergedContent = mergeDefinitions rawContent

    -- 1. EXTRACT MACROS
    let macroLines = [l | l <- mergedContent, "macro " `List.isPrefixOf` l]
        macros = map parseMacroLine macroLines

    let lemmaLines = [l | l <- mergedContent, "lemma " `List.isPrefixOf` l]
        lemmas = map parseLemmaLine lemmaLines

    -- 2. APPLY MACROS (The Preprocessor)
    let pureLines = filter (\l -> not ("macro " `List.isPrefixOf` l) && not ("lemma " `List.isPrefixOf` l)) mergedContent
        contentWithLemmas = map (\line -> foldl (flip applyLemma) line lemmas) pureLines
        content = map (\line -> foldl (flip applyMacro) line macros) contentWithLemmas

    -- 3. STANDARD EXTRACTION
    let goalLine = head [l | l <- content, "goal :" `List.isPrefixOf` l]
        goalFormula = parseFormula (drop 6 (trim goalLine)) 

    let assumptionLines = [l | l <- content, "assumption " `List.isPrefixOf` l]
        assumptions = map parseAssumption assumptionLines

    let proofLines = dropWhile (\l -> not ("proof" `List.isPrefixOf` l)) content
        firstProofLine = drop 5 (head proofLines) 
        proofStr   = firstProofLine ++ "\n" ++ unlines (tail proofLines)
        proof      = parseProof proofStr

    let goal = mkGoal assumptions goalFormula

    return (assumptions, proof, goal)




