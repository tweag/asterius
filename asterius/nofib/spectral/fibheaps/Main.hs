
-- partain

 if x <= y then Node x (b:as) else Node y (a:bs)





 where flatten EmptyBag xs = xs
       flatten (ConsBag x b) xs = flatten b (x:xs)
       flatten (UnionBags b1 b2) xs = flatten b1 (flatten b2 xs)










 if root' tt1 <= root' tt2 then
     FH (n1+n2) tt1 (ConsBag tt2 (UnionBags f1 f2))
 else
     FH (n1+n2) tt2 (ConsBag tt1 (UnionBags f1 f2))

 let
   d = log2 (n-1) -- maximum possible degree

   ins :: Ord a => STArray s Int (MyMaybe (Tree a)) -> (Int,Tree a) -> ST s ()
   ins a (i, t) =
       readArray a i >>= \e ->
       case e of
         Zero   -> writeArray a i (One t)
         One t2 -> writeArray a i Zero >>
                   ins a (i+1, link t t2)

   getMin a =
       readArray a d >>= \e ->
       case e of
         Zero  -> error "must be One" -- since array is filled as bits of n-1
         One t -> getMin' a d t EmptyBag 0
   getMin' a mini mint b i =
       if i >= d then
         return ((mini, mint),b)
       else
         readArray a i >>= \e ->
         case e of
           Zero  -> getMin' a mini mint b (i+1)
           One t -> if root mint <= root t then
                      getMin' a mini mint (ConsBag (i, t) b) (i+1)
                    else
                      getMin' a i t (ConsBag (mini, mint) b) (i+1)

 in
   runST (newArray (0,d) Zero >>= \a ->
          applyToAll (ins a) f >>
          sequence (map (ins a) (getChildren tt)) >>
          getMin a >>= \ (tt,f) ->
          return (FH (n-1) tt f))

 let
   d = log2 (n-1) -- maximum possible degree

   a = accumArray (flip (:)) [] (0,d) (getChildren tt ++ bagToList f)

   doLinks (ts:rest) = startup 0 ts rest
     where startup i [] [] = []
           startup i [] (ts:rest) = startup (i+1) ts rest
           startup i ts [] = combine i ts [] []
           startup i ts (next:rest) = combine i ts next rest

           combine i [] next rest = startup (i+1) next rest
           combine i [t] next rest = (i, t) : startup (i+1) next rest
           combine i (t1:t2:ts) next rest =
               combine i ts (link t1 t2 : next) rest

   getMin (tt:rest) = foldl chooseMin (tt,EmptyBag) rest
     where chooseMin (tt1,b) tt2 =
               if root' tt1 <= root' tt2 then
                   (tt1,ConsBag tt2 b)
               else
                   (tt2,ConsBag tt1 b)

   (new_tt,new_f) = getMin (doLinks (elems a))
 in
   FH (n-1) new_tt new_f

              else minFH xs : fibToList (deleteMinFH xs)


               else minFH xs : fibToList' (deleteMinFH' xs)










  [n] <- getArgs
  test (read n) `seq` return ()
