module Main where

import Theodore
import Lib

main :: IO ()
main = do
    putStrLn "Enter the path to your .thd file:"
    filePath <- getLine
    (assumptions, lemmas, proof, goal) <- readTheodoreFile filePath

    putStrLn "\nGoals:"
    print goal

    putStrLn "\nAssumptions:"
    print assumptions

    print proof

    putStrLn "\nApplying proof..."
    let result = applyWithLemmas lemmas proof goal
    if null result
    then putStrLn "Proof successful! Nothing left to prove."
    else putStrLn $ "Remaining subgoals:\n" ++ show result

    genLatexTreeWithLemmas lemmas proof goal
