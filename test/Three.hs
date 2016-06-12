module Three where
{
    import Data.Searchable;
    import Data.Countable;
    import TypeName;

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

    instance TypeName Three where
    {
        typeName _ = "Three";
    };
}