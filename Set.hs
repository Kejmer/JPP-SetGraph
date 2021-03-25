module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)
import Data.List (sort)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

-- Returns empty set O(1)
empty :: Set a
empty = Empty

-- Check if set is Empty O(1)
null :: Set a -> Bool
null Empty = True
null set = False

-- Check if elem is a member of set O(set size)
member :: Eq a => a -> Set a -> Bool
member elem Empty = False
member elem (Singleton x) = x == elem
member elem (Union x y) = member elem x || member elem y

-- Returns a singleton of a given element O(1)
singleton :: a -> Set a
singleton = Singleton

-- Returns a set made from elements of list O(n) 
fromList :: [a] -> Set a
fromList = foldl (flip insert) Empty

-- Returns a list with elements of set's O(n)
toList :: Set a -> [a]
toList = toListAux []
  where
    toListAux :: [a] -> Set a -> [a]
    toListAux acc Empty = acc
    toListAux acc (Singleton x) = x:acc
    toListAux acc (Union x y) = toListAux (toListAux acc x) y

-- źródło https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem?fbclid=IwAR1yPj39c2CR6hCKuf4sk93d7NHfILimuh0yhfmNEt1tDeqbJLPvQzrvQWY
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []
                                      
-- Returns a sorted lists of set's elements O(n*logn)
toAscList :: Ord a => Set a -> [a]
toAscList set = Data.List.sort $ removeDuplicates $ toList set

-- alias for toList
elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union fst Empty = fst
union Empty snd = snd
union fst snd = Union fst snd

insert :: a -> Set a -> Set a
insert elem = union (Singleton elem)

instance Ord a => Eq (Set a) where
  (==) fst snd = toAscList fst == toAscList snd

instance Semigroup (Set a) where
  (<>) = union

instance Monoid (Set a) where
  mempty = Empty

instance Show a => Show (Set a) where
  show set = show (toList set)

instance Functor Set where
    fmap f s = fromList $ map f (toList s)