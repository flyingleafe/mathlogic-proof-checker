module Kripke where

import LogicType
import Utils
import Data.List

data KTree = KTree { forced :: [String]
                   , restricted :: [String]
                   , children :: [KTree]
                   } deriving (Eq, Ord)

instance Show KTree where
    show = showIndented 0

forAllWorlds f t = f t && all (forAllWorlds f) (children t)
forAnyWorld f t = f t || any (forAnyWorld f) (children t)

showIndented :: Int -> KTree -> String
showIndented n (KTree f _ []) = take n (repeat ' ') ++ "(" ++ intercalate "," f ++ ")" ++ "\n"
showIndented n (KTree f _ (c:ch)) = take n (repeat ' ') ++ headParens ++
                                    drop n' (showIndented n' c) ++
                                    concatMap (showIndented n') ch
                                        where headParens = "(" ++ intercalate "," f ++ ")"
                                              n' = n + length headParens

force, restrict :: String -> KTree -> KTree
force v (KTree f r ch) = KTree (v:f) r $ map (force v) ch
restrict v (KTree f r ch) = KTree f (v:r) $ map (restrict v) ch

actAll f = foldl (\g v -> g . f v) id

forceAll = actAll force
restrictAll = actAll restrict

pushRestricted :: KTree -> KTree
pushRestricted (KTree f r ch) = KTree f rs' ch
    where rs' = fastNub $ r ++
                [r' | c <- ch, r' <- restricted $ pushRestricted c]

addChild :: KTree -> KTree -> KTree
addChild (KTree f r chs) ch = (KTree f (union r $ restricted ch) (ch:chs))

chTree = addChild (KTree [] [] [])

buildModel :: Logic -> [KTree]
buildModel (Pred (VarID s _)) = [KTree [s] [] []]
buildModel (Not l) = fmap chTree $ buildUnmodel l
buildModel (a :| b) = buildModel a ++ buildModel b
buildModel (a :& b) = buildModel a `mergeMbTrees` buildModel b
buildModel (a :-> b) = fmap chTree $
                       buildModel a ++ buildUnmodel b

buildUnmodel :: Logic -> [KTree]
buildUnmodel (Pred (VarID s _)) = [KTree [] [s] []]
buildUnmodel (Not l) = fmap chTree $ buildModel l
buildUnmodel (a :| b) = buildUnmodel a `mergeMbTrees` buildUnmodel b
buildUnmodel (a :& b) = buildUnmodel a ++ buildUnmodel b
buildUnmodel (a :-> b) = fmap chTree $
                         buildModel a `mergeMbTrees` buildUnmodel b

validateWorld :: Logic -> KTree -> Bool
validateWorld (Pred (VarID s _)) w = s `elem` forced w
validateWorld (Not l) w = forAllWorlds (not . validateWorld l) w
validateWorld (a :| b) w = validateWorld a w || validateWorld b w
validateWorld (a :& b) w = validateWorld a w && validateWorld b w
validateWorld (a :-> b) w = forAllWorlds (\t -> if validateWorld a t
                                                then validateWorld b t
                                                else True) w

unvalidateWorld :: Logic -> KTree -> Bool
unvalidateWorld (Pred (VarID s _)) w = not $ s `elem` forced w
unvalidateWorld (Not l) w = forAnyWorld (validateWorld l) w
unvalidateWorld (a :| b) w = unvalidateWorld a w && unvalidateWorld b w
unvalidateWorld (a :& b) w = unvalidateWorld a w || unvalidateWorld b w
unvalidateWorld (a :-> b) w = forAnyWorld (\t -> validateWorld a t &&
                                                 unvalidateWorld b t) w

validateModel = forAllWorlds . validateWorld
validateUnmodel = forAnyWorld . unvalidateWorld

unProve :: Logic -> Maybe KTree
unProve l = findOk $ buildUnmodel l
    where findOk [] = Nothing
          findOk (t:ts)
              | validateUnmodel l t = Just t
              | otherwise = findOk ts

mergeTrees' :: KTree -> KTree -> [KTree]
mergeTrees' (KTree f r ch) (KTree f' r' ch')
    | intersect f r' /= [] = []
    | intersect f' r /= [] = []
    | otherwise = [KTree (union f f') (union r r') (ch ++ ch')]

mergeTrees a b = pushRestricted a `mergeTrees'` pushRestricted b

mergeMbTrees a b = pairM (a, b) >>= (uncurry mergeTrees)
