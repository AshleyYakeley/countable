module Main where
{
    import Prelude;
    import Data.Word;
    import Test.Tasty;
    import Test.Tasty.HUnit;

    import Data.Empty;
    import Data.Searchable;
    import Data.Countable;

    import TypeName;
    import Three;
    import Golden;

    (@/=?) :: (Eq a,Show a) => a -> a -> Assertion;
    unexpected @/=? actual | unexpected /= actual = return ();
    _unexpected @/=? actual = assertFailure $ "got unexpected " ++ show actual;

    prevMaybeNext :: (Countable a,Show a) => Maybe a -> Assertion;
    prevMaybeNext ma = case countMaybeNext ma of
    {
        Just a' -> countPrevious a' @=? ma;
        Nothing -> return ();
    };

    countableTests' :: (Show a,Countable a) => a -> [TestTree];
    countableTests' a =
    [
        testCase "maybeNextDifferent" $ (Just a) @/=? countMaybeNext (Just a),
        testCase "prevDifferent" $ (Just a) @/=? (countPrevious a),
        testCase "maybeNextPrev" $ (Just a) @=? countMaybeNext (countPrevious a),
        testCase "prevMaybeNext" $ prevMaybeNext (Just a)
    ];

    findInNext :: (Countable a) => Int -> a -> TestTree;
    findInNext n a = testCase "findInNext" $ findInNext' n Nothing where
    {
        findInNext' 0 _ = assertFailure "failed";
        findInNext' _ (Just x) | x == a = return ();
        findInNext' n' mx = case countMaybeNext mx of
        {
            Nothing -> assertFailure "failed";
            mx' -> findInNext' (n' - 1) mx';
        };
    };

    countableTests :: (Show a,Countable a) => a -> [TestTree];
    countableTests a = (countableTests' a) ++ [findInNext 1000 a];

    nextIsMaybeNext :: (Show a,InfiniteCountable a) => Maybe a -> TestTree;
    nextIsMaybeNext ma = testCase "nextIsMaybeNext" $ (Just (countNext ma)) @=? (countMaybeNext ma);

    infiniteCountableTests :: (Show a,InfiniteCountable a) => a -> [TestTree];
    infiniteCountableTests a = (countableTests a) ++ [nextIsMaybeNext (Just a)];
    checkN :: (Show a,Countable a) => (String -> IO ()) -> Int -> Maybe a -> IO ();
    checkN _ 0 _ = return ();
    checkN write n ma = let
    {
        ma' = countMaybeNext ma;
    } in do
    {
        prevMaybeNext ma;
        write (show ma ++ "\n");
        case ma' of
        {
            Nothing -> return ();
            _ -> checkN write (n - 1) ma';
        };
    };

    testType :: forall a. (TypeName a,Show a) => (a -> [TestTree]) -> [a] -> TestTree;
    testType tests vals = testGroup (typeName vals) $ fmap (\a -> testGroup (show a) (tests a)) vals;

    allTests :: TestTree;
    allTests = testGroup "countable"
        [
        testType countableTests (allValues :: [()]),
        testType countableTests (allValues :: [Bool]),
        testType countableTests ([0,3,255] :: [Word8]),
        testType countableTests (allValues :: [Maybe ()]),
        testType countableTests (allValues :: [Maybe Bool]),
        testType countableTests (allValues :: [Maybe (Maybe Bool)]),
        testType countableTests ([[],[0],[2],[-1,1],[0,0,0]] :: [[Integer]]),
        testType countableTests' ([[1,2,1],[-5,17,112]] :: [[Integer]]),
        testType countableTests ([[],[True,True]] :: [[Bool]]),
        testType infiniteCountableTests ([0,1,-1,3,-7] :: [Integer]),
        testType countableTests (allValues :: [Three -> Three]),
        testType countableTests (allValues :: [None]),
        testType countableTests ([[] :: [None]]),
        testGroup "list"
            [
            goldenVsWriteString "Bool"    "test/count.Bool.ref"    $ \write -> checkN write 40 (Nothing :: Maybe [Bool   ]),
            goldenVsWriteString "Word8"   "test/count.Word8.ref"   $ \write -> checkN write 40 (Nothing :: Maybe [Word8  ]),
            goldenVsWriteString "Integer" "test/count.Integer.ref" $ \write -> checkN write 40 (Nothing :: Maybe [Integer])
            ]
        ];

    main :: IO ();
    main = defaultMain allTests;
}
