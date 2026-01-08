module Lib
    ( readTheodoreFile
    ) where

import FOL
import Theodore
import qualified Data.List as List

readTheodoreFile :: FilePath -> IO (Assumptions, Proof, Goal)
readTheodoreFile path = do
    content <- lines <$> readFile path
    let goalLine = head [l | l <- content, "goal :" `List.isPrefixOf` l]
        goalFormula = parseFormula (drop 6 goalLine)
        assumptionLines = [l | l <- content, "assumption " `List.isPrefixOf` l]
        assumptions = map parseAssumption assumptionLines
        proofLines = dropWhile (\l -> not ("proof" `List.isPrefixOf` l)) content
        proof = fst $ parseProof (tail proofLines)  -- tail skips "proof" line
    return (assumptions, proof, mkGoal assumptions goalFormula)





