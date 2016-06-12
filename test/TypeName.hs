module TypeName where
{
    import Data.Proxy;
    import Data.Word;
    import Data.Empty;

    class TypeName a where
    {
        typeName :: forall proxy. proxy a -> String;
    };

    instance TypeName None where
    {
        typeName _ = "None";
    };

    instance TypeName () where
    {
        typeName _ = "()";
    };

    instance TypeName Bool where
    {
        typeName _ = "Bool";
    };

    instance TypeName Word8 where
    {
        typeName _ = "Word8";
    };

    instance TypeName Integer where
    {
        typeName _ = "Integer";
    };

    instance TypeName a => TypeName (Maybe a) where
    {
        typeName _ = "Maybe " ++ (typeName (Proxy :: Proxy a));
    };

    instance TypeName a => TypeName [a] where
    {
        typeName _ = "[" ++ (typeName (Proxy :: Proxy a)) ++ "]";
    };

    instance (TypeName a,TypeName b) => TypeName (a -> b) where
    {
        typeName _ = (typeName (Proxy :: Proxy a)) ++ "->" ++ (typeName (Proxy :: Proxy b));
    };
}
