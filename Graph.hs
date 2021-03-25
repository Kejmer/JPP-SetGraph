module Graph where
import Set(Set)
import qualified Set

class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)


bipartie :: [a] -> [a] -> Set (a, a)
bipartie xs ys = foldl (\acc x -> Set.union (bipartieAux x ys) acc) Set.empty xs

bipartieAux :: a -> [a] -> Set (a, a)
bipartieAux x = foldl (\acc y -> Set.union acc (Set.singleton(x, y))) Set.empty

instance Graph Relation where
  empty = Relation { domain = Set.empty, relation = Set.empty}
  vertex v = Relation { domain = Set.singleton v, relation = Set.empty}
  union (Relation d1 r1) (Relation d2 r2) = Relation {
    domain = Set.union d1 d2,
    relation = Set.union r1 r2
  }
  connect (Relation d1 r1) (Relation d2 r2) = Relation {
    domain = Set.union d1 d2,
    relation = Set.union (Set.union r1 r2) (bipartie (Set.toList d1) (Set.toList d2))
  }


instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id


instance Graph Basic where
    empty = Empty
    vertex = Vertex
    union = Union
    connect = Connect

removeRepetition :: Ord a => Set a -> Set a
removeRepetition set = Set.fromList $ Set.toAscList set

basicDomain :: Ord a => Basic a -> Set a
basicDomain g = removeRepetition $ basicDomainAux g
basicDomainAux :: Ord a => Basic a -> Set a
basicDomainAux Empty = Set.empty
basicDomainAux (Vertex x) = Set.singleton x
basicDomainAux (Union x y) = basicDomainAux x `Set.union` basicDomainAux y
basicDomainAux (Connect x y) = basicDomainAux x `Set.union` basicDomainAux y

basicRelation :: Ord a => Basic a -> Set (a,a)
basicRelation g = removeRepetition $ basicRelationAux g
basicRelationAux :: Ord a => Basic a -> Set (a,a)
basicRelationAux Empty = Set.empty
basicRelationAux (Vertex x) = Set.empty
basicRelationAux (Union x y) = basicRelationAux x `Set.union` basicRelationAux y
basicRelationAux (Connect x y) = basicRelationAux x `Set.union` basicRelationAux y `Set.union` bipartie (Set.toList $ basicDomain x) (Set.toList $ basicDomain y)

instance Ord a => Eq (Basic a) where
    (==) fst snd = basicRelation fst == basicRelation snd && basicDomain fst == basicDomain snd

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex x) = vertex x
fromBasic (Union x y) = fromBasic x `union` fromBasic y
fromBasic (Connect x y) = fromBasic x `connect` fromBasic y

unusedVerticies :: Ord a => Basic a -> [a]
unusedVerticies g = diffrence (flatten $ basicRelation g) (basicDomain g)

diffrence :: Ord a => Set a -> Set a -> [a]
diffrence rel dom = Set.toAscList $ foldl (diffrenceAux rel) Set.empty (Set.toList dom)
diffrenceAux :: Ord a => Set a -> Set a -> a -> Set a
diffrenceAux rel acc el
  | Set.member el acc = acc
  | not (Set.member el rel) = acc `Set.union` Set.singleton el
  | otherwise = acc
flatten :: Ord a => Set (a,a) -> Set a
flatten set = Set.fromList $ Set.toAscList $ foldl flattenAux Set.empty (Set.toList set)
flattenAux :: Ord a => Set a -> (a,a) -> Set a
flattenAux set (x,y) = set `Set.union` Set.singleton x `Set.union` Set.singleton y
  -- let set' = if Set.member x set
  --     then set
  --     else set `Set.union` Set.singleton x in
  -- if Set.member y set'
  --   then set'
  --   else set' `Set.union` Set.singleton y


instance (Ord a, Show a) => Show (Basic a) where
    show g = let first = "edges " ++ show (Set.toAscList (basicRelation g)) in
      let second = " + verticies " ++ show (unusedVerticies g) in
      first ++ second
-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g = "digraph {\n" ++ relationAux g ++ verticiesAux g ++ "}\n"
  where
    relationAux :: (Ord a, Show a) => Basic a -> String
    relationAux g = foldl arrow "" (Set.toAscList $ basicRelation g)
    verticiesAux :: (Ord a, Show a) => Basic a -> String
    verticiesAux g = foldl (\acc x -> acc ++ show x ++ "\n") "" (unusedVerticies g)
    arrow :: Show a => String -> (a,a) -> String
    arrow acc (x,y) = acc ++ show x ++ " -> " ++ show y ++ ";\n"

instance Functor Basic where
    fmap f Empty = empty
    fmap f (Vertex x) = vertex $ f x
    fmap f (Union x y) = fmap f x `union` fmap f y
    fmap f (Connect x y) = fmap f x `connect` fmap f y

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV x y z = fmap (mergeVAux x y z)
mergeVAux :: Eq a => a -> a -> a -> a -> a
mergeVAux x y z v = if (v == x) || (v == y) then z else v

instance Applicative Basic where
  pure x = vertex x
  Empty <*> _ = empty
  (Vertex f) <*> g = fmap f g
  (Union f f') <*> g = (f <*> g) `union` (f' <*> g)
  (Connect f f') <*> g = (f <*> g) `connect` (f' <*> g)

instance Monad Basic where
  return = vertex
  Empty >>= f = empty 
  (Vertex v) >>= f = f v 
  (Union a b) >>= f = (a >>= f) `union` (b >>= f) 
  (Connect a b) >>= f = (a >>= f) `connect` (b >>= f) 
-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV x y z g = g >>= (\v -> if v == x 
                                then vertex y `union` vertex z 
                                else vertex x)

