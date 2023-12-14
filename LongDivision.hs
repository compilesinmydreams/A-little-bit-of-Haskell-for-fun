--Thara Radha Palaniswamy
longDivisionEH :: Int -> Int -> (Int, Int)
longDivisionEH n d
  | d == 0 = error "You're trying to divide by zero"
  | n < d = (0, n)
  | otherwise = (q + 1, r)
    where
      (q, r) = longDivisionEH (n - d) d

longDivisionMaybe :: Int -> Int -> Maybe (Int, Int)
longDivisionMaybe n d
  | d == 0 = Nothing
  | n < d = Just (0, n)
  | otherwise= Nothing

quo :: Maybe (Int, Int) -> Maybe Int
quo (Just (q, _)) = Just q


rem :: Maybe (Int, Int) -> Maybe Int
rem (Just (_, r)) = Just r


longDivisionTC :: Int -> Int -> (Int, Int)
longDivisionTC n d = longDivisionTCHelper n d 0

longDivisionTCHelper :: Int -> Int -> Int -> (Int, Int)
longDivisionTCHelper n d q
  | n < d = (q, n)
  | otherwise = longDivisionTCHelper (n - d) d (q + 1)