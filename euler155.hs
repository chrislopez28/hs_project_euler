-- n capacitors
-- D(1) = 1; D(2) = 3; D(3) = 7

capacitorSequence :: Int -> Int 
capacitorSequence n | n == 1 = 1
                    | n > 1 = (capacitorSequence(n-1) * 2)