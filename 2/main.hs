main :: IO ()
main = do
    let res = checkerRec [1, 2, 3, 5] Unset
    print res

data LevelDir = Asc | Des | Unset
    deriving (Eq, Show)

checkerRec :: [Int] -> LevelDir -> Bool
checkerRec [_] _ = True
checkerRec [] _ = True
checkerRec (lv1 : lv2 : lvls) dir
    | abs (lv1 - lv2) > 1       = False 
    | lv1 > lv2 && dir == Asc   = False
    | lv1 < lv2 && dir == Des   = False
    | lv1 > lv2                 = checkerRec (lv2:lvls) Des
    | lv1 < lv2                 = checkerRec (lv2:lvls) Asc
    | otherwise                 = checkerRec (lv2:lvls) Unset


