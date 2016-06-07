module Main where
{
    import Data.Countable;
    import Data.Searchable;
    import Data.Empty;
    import Data.Word;
    import Control.Monad;
    import Prelude;

    type Property a = a -> Bool;

    testProperty :: (Show a) => String -> Property a -> a -> IO ();
    testProperty s prop a = do
    {
        putStrLn (s ++ "(" ++ (show a) ++ "): " ++ (if prop a then "PASS" else "FAIL"));
    };

    maybeNextDifferent :: (Countable a) => Property a;
    maybeNextDifferent a = countMaybeNext (Just a) /= Just a;

    prevDifferent :: (Countable a) => Property a;
    prevDifferent a = countPrevious a /= Just a;

    maybeNextPrev :: (Countable a) => Property a;
    maybeNextPrev a = countMaybeNext (countPrevious a) == Just a;

    prevMaybeNext :: (Countable a) => Property (Maybe a);
    prevMaybeNext ma = case countMaybeNext ma of
    {
        Just a' -> countPrevious a' == ma;
        Nothing -> True;
    };

    testAllCountable' :: (Show a,Countable a) => a -> IO ();
    testAllCountable' a = do
    {
        putStrLn "---";
        testProperty "maybeNextDifferent" maybeNextDifferent a;
        testProperty "prevDifferent" prevDifferent a;
        testProperty "maybeNextPrev" maybeNextPrev a;
        testProperty "prevMaybeNext" prevMaybeNext (Just a);
    };

    testAllCountable :: (Show a,Countable a) => a -> IO ();
    testAllCountable a = do
    {
        testAllCountable' a;
        testProperty "findInNext" (findInNext 1000) a;
    };

    testCountableValue :: (Show a, Countable a) => a -> IO ();
    testCountableValue a = let
    {
        ma1 = countPrevious a;
        ma' = countMaybeNext ma1;
    } in do
    {
        putStrLn (show (ma' == Just a,a,ma1,ma'));
    };

    nextIsMaybeNext :: (InfiniteCountable a) => Property (Maybe a);
    nextIsMaybeNext ma = countMaybeNext ma == Just (countNext ma);

    testAllInfiniteCountable :: (Show a,InfiniteCountable a) => a -> IO ();
    testAllInfiniteCountable a = do
    {
        testAllCountable a;
        testProperty "nextIsMaybeNext" nextIsMaybeNext (Just a);
    };

    findInNext :: (Countable a) => Int -> Property a;
    findInNext n a = findInNext' n Nothing where
    {
        findInNext' 0 _ = False;
        findInNext' _ (Just x) | x == a = True;
        findInNext' n' mx = case countMaybeNext mx of
        {
            Nothing -> False;
            mx' -> findInNext' (n' - 1) mx';
        };
    };

    checkN :: (Show a,Countable a) => Int -> Maybe a -> IO ();
    checkN 0 _ = return ();
    checkN n ma = let
    {
        ma' = countMaybeNext ma;
    } in do
    {
        testProperty "prevMaybeNext" prevMaybeNext ma;
        case ma' of
        {
            Nothing -> return ();
            _ -> checkN (n - 1) ma';
        };
    };

    data Three = T1 | T2 | T3 deriving (Eq,Show);

    instance Searchable Three where
    {
        search = finiteSearch;
    };

    instance Countable Three where
    {
        countPrevious = finiteCountPrevious;
        countMaybeNext = finiteCountMaybeNext;
    };

    instance AtLeastOneCountable Three where
    {
        countFirst = T1;
    };

    instance Finite Three where
    {
        allValues = [T1,T2,T3];
    };

    main :: IO ();
    main = do
    {
        mapM_ testAllCountable (allValues :: [()]);
        mapM_ testAllCountable (allValues :: [Bool]);
        mapM_ testAllCountable ([0,3,255] :: [Word8]);
        mapM_ testAllCountable (allValues :: [Maybe ()]);
        mapM_ testAllCountable (allValues :: [Maybe Bool]);
        mapM_ testAllCountable (allValues :: [Maybe (Maybe Bool)]);
        mapM_ testAllCountable ([[],[0],[2],[-1,1],[0,0,0]] :: [[Integer]]);
        mapM_ testAllCountable' ([[1,2,1],[-5,17,112]] :: [[Integer]]);
        mapM_ testAllCountable ([[],[True,True]] :: [[Bool]]);
        mapM_ testAllInfiniteCountable ([0,1,-1,3,-7] :: [Integer]);
        mapM_ testAllCountable (allValues :: [Three -> Three]);
        mapM_ testAllCountable (allValues :: [None]);
        mapM_ testAllCountable ([[] :: [None]]);
        putStrLn "---";
        checkN 40 (Nothing :: Maybe [Bool]);
        putStrLn "---";
        checkN 40 (Nothing :: Maybe [Word8]);
        putStrLn "---";
        checkN 40 (Nothing :: Maybe [Integer]);
    };
}
