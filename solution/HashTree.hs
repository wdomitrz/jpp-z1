module HashTree where
import           Hashable32
import           Utils

-- A. Drzewa skrótów
data Tree a = Leaf {value :: a,     treeHash :: Hash}
            | Node {left :: Tree a, treeHash :: Hash, right :: Tree a }
            | Twig {left :: Tree a, treeHash :: Hash}

leaf :: Hashable a => a -> Tree a
leaf v = Leaf { value = v, treeHash = hash v }
node :: Hashable a => Tree a -> Tree a -> Tree a
node l r =
    Node { left = l, treeHash = combine (treeHash l) (treeHash r), right = r }
twig :: Hashable a => Tree a -> Tree a
twig l = Twig { left = l, treeHash = combine (treeHash l) (treeHash l) }

instance Hashable (Tree a) where
    hash = treeHash

buildTree :: Hashable a => [a] -> Tree a
buildTree = go . map leaf
  where
    go :: Hashable a => [Tree a] -> Tree a
    go [x] = x
    go []  = error "buildTree: empty list"
    go xs  = go $ go' xs
    go' :: Hashable a => [Tree a] -> [Tree a]
    go' []           = []
    go' [x         ] = [twig x]
    go' (x : y : ys) = node x y : go' ys

instance Show a => Show (Tree a) where
    show = unlines . go 0
      where
        go :: Show a => Int -> Tree a -> [String]
        go n t@Leaf{} = [pad n $ showBase t]
        go n t@Node { left = l, right = r } =
            pad n (showBase t) : go (n + 1) l ++ go (n + 1) r
        go n t@Twig { left = l } = pad n (showBase t) : go (n + 1) l
        pad :: Int -> String -> String
        pad = (++) . (`replicate` ' ') . (2 *)
        showBase :: Show a => Tree a -> String
        showBase t = showHash (hash t) ++ " " ++ symblicValue t
        symblicValue :: Show a => Tree a -> String
        symblicValue Leaf { value = v } = show v
        symblicValue Node{}             = "-"
        symblicValue Twig{}             = "+"

{- | Tree drawing
>>> putStr $ drawTree $ buildTree "fubar"
0x2e1cc0e4 -
  0xfbfe18ac -
    0x6600a107 -
      0x00000066 'f'
      0x00000075 'u'
    0x62009aa7 -
      0x00000062 'b'
      0x00000061 'a'
  0xd11bea20 +
    0x7200b3e8 +
      0x00000072 'r'
-}
drawTree :: Show a => Tree a -> String
drawTree = show


-- B. Dowody
type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

{- | buildProof and instance Show (MerkleProof a)
>>> buildProof 'i' $ buildTree "bitcoin"
Just (MerkleProof 'i' <0x5214666a<0x7400b6ff>0x00000062)
>>> buildProof 'e' $ buildTree "bitcoin"
Nothing
-}
buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x = (MerkleProof x <$>) . maybeHead . merklePaths x

{- | merklePaths - assuming that we traverse the tree left-to-right
>>> merklePaths 'i' $ buildTree "bitcoin"
[[Left 1377068650,Left 1946203903,Right 98],[Right 1777612924,Left 1845538200,Right 111]]
-}
merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x = go
  where
    hx :: Hash
    hx = hash x
    go :: Tree a -> [MerklePath]
    go Leaf { treeHash = h } | hx == h   = [[]]
                             | otherwise = []
    go Node { left = l, right = r } = map (Left (treeHash r) :) (go l)
        ++ map (Right (treeHash l) :) (go r)
    go Twig { left = l } = map (Left (treeHash l) :) $ go l


instance Show a => Show (MerkleProof a) where
    showsPrec d (MerkleProof x ps) = showParen
        (d > 0)
        (showString "MerkleProof " . showsPrec 11 x . showString
            (" " ++ showMerklePath ps)
        )

{- | showMerklePath
>>> mapM_ print $ map showMerklePath  $ merklePaths 'i' $ buildTree "bitcoin"
"<0x5214666a<0x7400b6ff>0x00000062"
">0x69f4387c<0x6e00ad98>0x0000006f"
-}
showMerklePath :: MerklePath -> String
showMerklePath = foldr go ""
  where
    go :: Either Hash Hash -> String -> String
    go = (++) . either (('<' :) . showHash) (('>' :) . showHash)

{- | verifyProof
>>> let t = buildTree "bitcoin"
>>> let proof = buildProof 'i' t
>>> verifyProof (treeHash t) <$> proof
Just True
>>> verifyProof 0xbada55bb <$> proof
Just False
-}
verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof x ps) = h == foldr go (hash x) ps
  where
    go :: Either Hash Hash -> Hash -> Hash
    go p h' = either (h' `combine`) (`combine` h') p
