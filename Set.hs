module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)

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
member elem (Union x y) = member elem x && member elem y

-- Returns a singleton of a given element O(1)
singleton :: a -> Set a
singleton = Singleton

-- Returns a set made from elements of list O(n) 
fromList :: [a] -> Set a
fromList = foldl (flip insert) Empty

-- Returns a list with elements of set's O(n)
toList :: Set a -> [a]
toList Empty = []
toList (Singleton x) = [x]
toList (Union x y) = toList x ++ toList y

-- Returns a sorted lists of set's elements O(n*logn)
toAscList :: Ord a => Set a -> [a]
-- toAscList set = sort $ toList set 
toAscList set = undefined

-- alias for toList
elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union fst (Union x y) = union (fst `union` x) y
union fst Empty = fst
union fst (Singleton x) = Union fst (Singleton x)

insert :: a -> Set a -> Set a
insert elem = Union (Singleton elem)

instance Ord a => Eq (Set a) where
  (==) fst snd = foldl (\acc x -> acc && member x fst) False (toList snd)

instance Semigroup (Set a) where
  (<>) = union

instance Monoid (Set a) where
  mempty = Empty

instance Show a => Show (Set a) where
  show set = show (toList set)

instance Functor Set where
    fmap f s = fromList $ map f (toList s)