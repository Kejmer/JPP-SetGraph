module Graph where
import Set(Set)
import qualified Set
import Data.List (sort)

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

-- źródło https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem?fbclid=IwAR1yPj39c2CR6hCKuf4sk93d7NHfILimuh0yhfmNEt1tDeqbJLPvQzrvQWY
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

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

basicDomain :: Ord a => Basic a -> [a]
basicDomain g = Data.List.sort $ removeDuplicates $ Set.toList $ basicDomainAux g
basicDomainAux :: Ord a => Basic a -> Set a
basicDomainAux Empty = Set.empty
basicDomainAux (Vertex x) = Set.singleton x
basicDomainAux (Union x y) = basicDomainAux x `Set.union` basicDomainAux y
basicDomainAux (Connect x y) = basicDomainAux x `Set.union` basicDomainAux y

basicRelation :: Ord a => Basic a -> [(a,a)]
basicRelation g = Data.List.sort $ removeDuplicates $ Set.toList $ basicRelationAux g
basicRelationAux :: Ord a => Basic a -> Set (a,a)
basicRelationAux Empty = Set.empty
basicRelationAux (Vertex x) = Set.empty
basicRelationAux (Union x y) = basicRelationAux x `Set.union` basicRelationAux y
basicRelationAux (Connect x y) = basicRelationAux x
                      `Set.union` basicRelationAux y
                      `Set.union` bipartie (basicDomain x) (basicDomain y)

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

diffrence :: Ord a => [a] -> [a] -> [a]
diffrence rel dom = Data.List.sort $ foldl (diffrenceAux rel) [] dom
diffrenceAux :: Ord a => [a] -> [a] -> a -> [a]
diffrenceAux rel acc el
  | el `elem` acc = acc
  | el `notElem` rel = el:acc
  | otherwise = acc
flatten :: Ord a => [(a,a)] -> [a]
flatten list = Data.List.sort $ removeDuplicates $ foldl (\acc (x,y) -> x:y:acc) [] list

instance (Ord a, Show a) => Show (Basic a) where
    show g = let first = "edges " ++ show (basicRelation g) in
      let second = " + vertices " ++ show (unusedVerticies g) in
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
    relationAux g = foldl arrow "" (basicRelation g)
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
mergeV x y z = fmap (\v -> if (v == x) || (v == y) then z else v)

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
                                else vertex v)