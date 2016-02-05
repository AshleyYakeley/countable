module Data.Expression (Expression(..),expressionSym,runExpression) where
{
    data Expression a b f r = ClosedExpression (f r) | OpenExpression a (Expression a b f (b -> r));

    instance (Functor f) => Functor (Expression a b f) where
    {
        fmap pq (ClosedExpression fp) = ClosedExpression (fmap pq fp);
        fmap pq (OpenExpression a ebp) = OpenExpression a (fmap (\bp -> pq . bp) ebp);
    };

    ffmap :: (Applicative f) => f (p -> q) -> Expression a b f p -> Expression a b f q;
    ffmap fpq (ClosedExpression fp) = ClosedExpression (fpq <*> fp);
    ffmap fpq (OpenExpression a ebp) = OpenExpression a (ffmap (fmap (\pq bp -> pq . bp) fpq) ebp);

    instance (Applicative f) => Applicative (Expression a b f) where
    {
        pure t = ClosedExpression (pure t);
        (ClosedExpression fpq) <*> ep = ffmap fpq ep;
        (OpenExpression a ebpq) <*> ep = OpenExpression a ((fmap (\bpq p b -> bpq b p) ebpq) <*> ep);
    };

    expressionSym :: a -> f (b -> r) -> Expression a b f r;
    expressionSym a fbr = OpenExpression a (ClosedExpression fbr);

    runExpression :: (Functor f) => Expression a b f r -> f ((a -> b) -> r);
    runExpression (ClosedExpression fr) = fmap (\r _ab -> r) fr;
    runExpression (OpenExpression a0 ebr) = fmap (\abbr ab -> abbr ab (ab a0)) (runExpression ebr);
}
