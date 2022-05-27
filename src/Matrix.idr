import Data.Vect

addMatrix : Num numType =>
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys


multMatrix : Num numType =>
             Vect n (Vect m numType) -> Vect m (Vect p numType) ->
             Vect n (Vect p numType)

createEmpties : {n : _} -> Vect n (Vect 0 elem)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties

transposeHelper : Vect n elem_0 -> Vect n (Vect len elem_0) -> Vect n (Vect (S len) elem_0)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : {n : _} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         transposeHelper x xsTrans


ziptransposeMat : {n : _} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
ziptransposeMat [] = createEmpties
ziptransposeMat (x :: xs) = let xsTrans = ziptransposeMat xs in
                            zipWith (\y, ys => y :: ys) x xsTrans

  
