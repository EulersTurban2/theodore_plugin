module Lib
    ( readTheodoreFile
    ) where

import FOL
import Theodore
import qualified Data.List as List

readTheodoreFile :: FilePath -> IO (Assumptions, Proof, Goal)
readTheodoreFile path = do
    content <- lines <$> readFile path

    -- Extract goal
    let goalLine = head [l | l <- content, "goal :" `List.isPrefixOf` l]
        goalFormula = parseFormula (drop 6 goalLine)  -- drop "goal :"

    -- Extract assumptions
    let assumptionLines = [l | l <- content, "assumption " `List.isPrefixOf` l]
        assumptions = map parseAssumption assumptionLines

    -- Extract proof
    let proofLines = dropWhile (\l -> not ("proof" `List.isPrefixOf` l)) content
        proofStr   = unlines (tail proofLines)  -- drop "proof" line
        proof      = parseProof proofStr

    -- Construct the goal
    let goal = mkGoal assumptions goalFormula

    return (assumptions, proof, goal)




