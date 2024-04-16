data Bin a = Hoja | Nodo (Bin a) a (Bin a)

min_bst :: Bin a -> a
min_bst (Nodo Hoja a _) = a
min_bst (Nodo l a _) = min_bst l

maximun :: Bin a -> a
maximun (Nodo _ a Hoja) = a
maximun (Nodo _ a r) = maximun r

checkBST :: Ord a => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja a Hoja) = True
checkBST (Nodo Hoja a r) = (a < (min_bst r)) && (checkBST r)
checkBST (Nodo l a Hoja) = ((maximun l) <= a) && (checkBST l)
checkBST (Nodo l a r) = ((maximun l) <= a) && (a < (min_bst r)) && (checkBST l) && (checkBST r)

