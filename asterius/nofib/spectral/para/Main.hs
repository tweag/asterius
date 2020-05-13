
           where g' a   = [g a]
                 f' a s = f a (head s) : s

           where choice a b | f a <  f b = a
                            | otherwise  = b

         where last_word w = [ [[w]] ]
               next_word w ps = map (new w) ps ++ map (glue w) ps

       where plus w n = length w + 1 + n

      where plus l n = linc l + n
            linc l = (optw - width l)^2

= minWith cost . fold1 step start
  where
    step w ps = filter fitH (new w (minWith cost ps):map (glue w) ps)
    start w   = filter fitH [ [[w]] ]

= the . minWith cost . fold1 step start
  where
    step w ps = filter fitH (new w (minWith cost ps):map (glue w) ps)
    start w   = filter fitH [([[w]], length w,0)]
    new w ([l],n,0)   = ([w]:[l], length w, 0)
    new w p           = ([w]:ls , length w, cost p) where (ls,n,m) = p
    glue w (l:ls,n,m) = ((w:l):ls, length w + 1 + n, m)
    the (ls,n,m)      = ls
    width_hd (ls,n,m) = n
    cost_tl (ls,n,m)  = m
    linc_hd p         = (optw - width_hd p)^2
    cost ([_],_,_)    = 0
    cost p            = linc_hd p + cost_tl p
    fitH p            = width_hd p <= maxw

= minWith cost . fold1 step start
  where
    step w ps = trim (filter fitH (new w (minWith cost ps):map (glue w) ps))
    start w   = filter fitH [ [[w]] ]
    trim []   = []
    trim [p]  = [p]
    trim pspq
      | cost p <= cost q = trim psp
      | otherwise        = trim psp ++ [q]
      where q   = last pspq
            psp = init pspq
            p   = last psp
            ps  = init psp

= last . fold1 step start
  where
    step w ps = trim(filter fitH (new w (last ps) `add` map (glue w) ps))
    start w   = filter fitH [ [[w]] ]
    add p []                          = [p]
    add p [q]                         = [p,q]
    add p (q:r:rs) | bf p q <= bf q r = add p (r:rs)
                   | otherwise        = p:q:r:rs
    bf p q
      | single q && cost pt == 0
                  = (optw - wph) `min` rqh
      | single q  = rqh
      | otherwise = ceildiv (cost p - cost q) (2*(wqh-wph)) `min` rqh
        where ph:pt = p
              qh:qt = q
              wph   = width ph
              wqh   = width qh
              rqh   = maxw - wqh + 1
              ceildiv n m = (n+m-1) `div` m
    trim []                      = []
    trim [p]                     = [p]
    trim pspq | cost p <= cost q = trim psp
              | otherwise        = pspq
      where q   = last pspq
            psp = init pspq
            p   = last psp
            ps  = init psp

= last . fold1 step start
  where
    step w ps = trim(droptail (not.fitH) (new w (last ps) `add` map (glue w) ps))
    start w   = droptail (not.fitH) [ [[w]] ]
    droptail p []              = []
    droptail p xsx | p x       = droptail p xs
                   | otherwise = xsx
      where x  = last xsx
            xs = init xsx
    add p []                          = [p]
    add p [q]                         = [p,q]
    add p (q:r:rs) | bf p q <= bf q r = add p (r:rs)
                   | otherwise        = p:q:r:rs
    bf p q
      | single q && cost pt == 0
                  = (optw - wph) `min` rqh
      | single q  = rqh
      | otherwise = ceildiv (cost p - cost q) (2*(wqh-wph)) `min` rqh
        where ph:pt = p
              qh:qt = q
              wph   = width ph
              wqh   = width qh
              rqh   = maxw - wqh + 1
              ceildiv n m = (n+m-1) `div` m
    trim []                      = []
    trim [p]                     = [p]
    trim pspq | cost p <= cost q = trim psp
              | otherwise        = pspq
      where q   = last pspq
            psp = init pspq
            p   = last psp
            ps  = init psp

                  where l = n - m
                        (ws1,ws2) = splitAt l ws

= tile ws (map (len_tl.last'.fst3) zs, thd3 (head zs))
  where zs = scan1 stepr startr (map length ws)

       (SymList Par, Cost, Length) ->
       (SymList Par, Cost, Length)

= (trim (drop_nofit (new (last' ps) `add` ps)), tot_width, tot_len)
  where
    single p      = len_tl p == 0
    cost p
      | single p  = 0
      | otherwise = cost_tl p + (optw - width_hd p)^2
    width_hd p
      | single p  = tot_width
      | otherwise = tot_width - width_tl p - 1
    tot_width     = w + 1 + tw
    tot_len       = 1 + tl

    new p | single p  = (tw,0,tl)
          | otherwise = (tw,cost_tl p + (optw-old_width_hd p)^2,tl)
    old_width_hd p | single p  = tw
                   | otherwise = tw - width_tl p - 1

    trim ps_pq | null' ps_pq      = ps_pq
               | single' ps_pq    = ps_pq
               | cost p <= cost q = trim ps_p
               | otherwise        = ps_pq
                 where ps_p = init' ps_pq
                       q    = last' ps_pq
                       p    = last' ps_p

    drop_nofit ps_p | null' ps_p        = ps_p
                    | width_hd p > maxw = drop_nofit ps
                    | otherwise         = ps_p
                      where ps = init' ps_p
                            p  = last' ps_p

    add p qr_rs | single' qr_rs || null' qr_rs = cons' p qr_rs
                | bf p q <= bf q r             = add p r_rs
                | otherwise                    = cons' p qr_rs
                  where r_rs = tail' qr_rs
                        q  = head' qr_rs
                        r  = head' r_rs

    bf p q
      | single q && cost_tl p == 0 = (optw - wph) `min` rqh
      | single q                   = rqh
      | otherwise                  = ceildiv (cost p-cost q)
                                             (2*(wqh-wph)) `min` rqh
         where
           wph = width_hd p
           wqh = width_hd q
           rqh = maxw - wqh + 1

           | otherwise = head y

           | otherwise = head y

             | otherwise = ([a],x)

             | otherwise = (x,[a])

           | single x  = (reverse y1, y0)
           | otherwise = (tail x, y)
             where (y0,y1) = splitAt (length y `div` 2) y

           | single x  = (y0, reverse y1)
           | otherwise = (y, tail x)
             where (y0,y1) = splitAt (length y `div` 2) y

        where insert xs ys = xs ++ [a] ++ ys

      where break a b xs | a == b    = []:xs
                         | otherwise = (b:head xs):tail xs
            start a b = break a b [[]]

 where
   nextword p w | fits (last p++[w]) = init p ++ [last p ++ [w]]
                | otherwise = p ++ [[w]]

 "In the constructive programming community it is commonplace to see " ++
 "formal developments of textbook algorithms. In the algorithm design " ++
 "community, on the other hand, it may be well known that the textbook " ++
 "solution to a problem is not the most efficient possible. However, in " ++
 "presenting the more efficient solution, the algorithm designer will " ++
 "usually omit some of the implementation details, this creating an " ++
 "algorithm gap between the abstract algorithm and its concrete " ++
 "implementation. This is in contrast to the formal development, which " ++
 "usually presents the complete concrete implementation of the less " ++
 "efficient solution. \n \n"

   [n,_] <- getArgs
   replicateM_ (read n) $ do
       [_,f] <- getArgs
       h <- openFile f ReadMode
       ws <- hGetContents h
       length (if null ws then [] else (fmt ws)) `seq` return ()
