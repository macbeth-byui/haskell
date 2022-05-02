-- Fractional was needed since we are doing real division
midpoint :: (Fractional a) => (a,a) -> (a,a) -> (a,a)
midpoint (x1, y1) (x2, y2) = ((x2 - x1) / 2 , (y2 - y1) / 2)

-- RealFrac is needed since we need Float or Double with Ord capability.  This is
-- why we didn't select Fractional.
-- floor and ceiling return Integral.  We can go back to Num by using fromIntegral
-- which will preserve our RealFrac.
roundNum :: (RealFrac a) => a -> a
roundNum x 
    | (x - f) < 0.5 = f
    | otherwise     = c
    where
        f = fromIntegral (floor x)
        c = fromIntegral (ceiling x)

-- RealFrac because we need to do floor
-- Whole part could be Integer or Integral ... couldn't be Num because no way to get Float
splitReal :: RealFrac a => a -> (Integer, a)
splitReal x = 
    let f = floor x
    in (f, x - fromIntegral f)

-- Note that String is a type for [Char]
prefix :: Integer -> Char -> String
prefix 0 c = [c,'\n']
prefix x c= ' ':prefix (x-1) c


-- Need RealFrac due to floor
star :: RealFrac a => a -> String
star x = 
    let xrel = floor (x * 50.0)
    in prefix xrel '*'


-- Star needs the result of the funciton to be RealFrac
-- I need start and end to be Ord and everything else to be Num
-- To support trig functions, I will need to require my start to 
-- be Fractional.  However, since I need Ord as well, I will
-- require RealFrac.  If the user wants to send in an Integer, they
-- will need to convert with fromIntegral.
plot :: (Real a, RealFrac b) => (a -> b) -> a -> a -> a -> String
plot f start end step 
    | start > end = []
    | otherwise   = star (f start) ++ plot f (start+step) end step
