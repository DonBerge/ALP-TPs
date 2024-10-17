type Name = String
data Estado a = Inicial | Define Name a (Estado a)

update :: Name -> a -> Estado a -> Estado a
update n v Inicial = Define n v Inicial
update n v (Define n' v' e)
    | n == n' = Define n v e
    | otherwise = Define n' v' $ update n v e

lookfor :: Name -> Estado a -> Maybe a
lookfor n Inicial = Nothing
lookfor n (Define n' v e)
    | n == n' = Just v
    | otherwise = lookfor n e

free :: Name -> Estado a -> Estado a
free n Inicial = Inicial
free n (Define n' v' e)
    | n == n' = free n e
    | otherwise = Define n' v' $ free n e

