# Merge Algorithm

## Edit distance: Calculates the edit distance between ancestor and the forked version 
(Here E stands for empty tree)
- Base case: E, E => []
- E, T2 => Insert (min T2) :: Edit-distance (E, T2-min)
- T1, E => Delete (min T1) :: Edit-distance (T1-min, E) 
- T1, T2 => 
      a1 = min T1 and a2 = min T2
      case 0 : a1 = a2 => (Edit-distance (T1-a1) (T2-a2)) (In this case we just care about the rest of the trees except the head
                                                           Why? Because if a value smaller was inserted to the forked version it
                                                           would have moved to the head. And if it has moved to head then a1 cannot
                                                           be equal to a2)
      case 1: a1 < a2 => Delete a1 :: 
                         (Edit-distance (T1-a1) T2)       (In this case we know a1 < a2 that means a1 was deleted in T2. If a smaller
                                                           element was inserted then that value must have moved to the head that means 
                                                           that will lead to a1 > a2, so this case will not be valid. We will deal that
                                                           case in case 2 described below.)
                                                           
      case 2: a1 > a2 => Insert a2 ::
                         (Edit-distance T1 (T2-a2))       (In this case we know a1 > a2 that means we need to insert a2 and it will take 
                                                           the head position as in the new version from ancestor we have a2 as the head. Then 
                                                           we calulcate the edit distance between T1 and the rest of the tree of T2(leaving head))
                                                           
 ## Operation Transform: 
    We get (p,q) from the above algorithm and now we calulate (p',q'). P and q are list of edits where edits are of this form
    * Insert a
    * Delete a
    (Here [] stands for empty list)         (Think in diagonal diamond structure)
- Base case: [], [] => [], []               (nothing changed)
- p, [] => p, []                            (here p' is equal to p and q is empty)
- [], q => [], q                            (here q' is equal to q and p is empty)
- p' :: ps, q' :: qs => 
  case 0 : p' = Insert x and q' = Insert y
           case 01: x = y                   (here both the forked branch inserted same element, we have to only take care of 
           Op-transform ps qs                the tail of p and q in that case)
           case 02: x < y or x > y 
                      
      
