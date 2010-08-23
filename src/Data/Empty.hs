module Data.Empty where
{
    import Data.Countable;
    import Data.Searchable;

    class (Finite n) => Empty n where
    {
        never :: n -> a;
        never n = seq n undefined;
    };

    instance (Empty a,Empty b) => Empty (Either a b) where
    {
        never (Left a) = never a;
        never (Right a) = never a;
    };

    instance (Empty a,Finite b) => Empty (a,b) where
    {
        never (a,_) = never a;
    };

    instance (AtLeastOneCountable a,Finite a,Empty b) => Empty (a -> b) where
    {
        never ab = never (ab countFirst);
    };

    data Nothing;

    instance Countable Nothing where
    {
        countPrevious = never;
        countMaybeNext Nothing = Nothing;
        countMaybeNext (Just n) = never n;
    };

    instance Searchable Nothing where
    {
        search = finiteSearch;
    };

    instance Finite Nothing where
    {
        allValues = [];
    };

    instance Empty Nothing;

    instance Eq Nothing where
    {
        a == _b = never a;
    };

    instance Ord Nothing where
    {
        a <= _b = never a;
    };

    instance Show Nothing where
    {
        show a = never a;
    };
}
