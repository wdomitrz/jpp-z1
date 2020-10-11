-- Author: Konrad Staniszewski
module CustomTests where
import Blockchain
import HashTree
import Data.Word
import Hashable32
import PPrint
--import ProblemStatementTests

{- | Tree constructors
>>> let x = 'f'
>>> treeHash (leaf x) == hash x
True
>>> treeHash (twig (leaf x)) == combine (hash x) (hash x)
True
>>> treeHash (node (leaf x) (leaf x)) == treeHash (twig (leaf x))
True
>>> treeHash (buildTree "f") == hash 'f'
True
-}

{- | Tree drawing
>>> let x = leaf 'f'
>>> putStr $ drawTree x
0x00000066 'f'

>>> putStr $ drawTree (twig x)
0x6600a0f8 +
  0x00000066 'f'
-}

{- | Merkle Paths
>>> let t1 = buildTree "f"
>>> let t2 = buildTree "ABC"
>>> mapM_ print [buildProof 'f' t1]
Just (MerkleProof 'f' )
>>> mapM_ print $ map showMerklePath  $ merklePaths 'f' $ t1
""
>>> mapM_ print $ map showMerklePath  $ merklePaths 'C' $ t2
">0x41006695<0x00000043"
-}

{- | Transactions
>>> let john = hash "John"
>>> let (block, []) = mineTransactions john (hash block1) []
>>> block
BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 553764705, txAmount = 50000}, txroot = 1456316675, nonce = 14}
<BLANKLINE>
-}

{- | Chains
>>> verifyChain [block2, block1]
Nothing
>>> VH <$> verifyChain [block0,block1,block2]
Nothing
>>> VH <$> verifyChain [block2,block1,block0]
Just 0x0dbea380
-}

-- slack
{- | Benke example of multi trans
>>> let makeTx f t a = Tx (hash f) (hash t) (a*coin)
>>> let tx1 = makeTx "Satoshi" "Alice" 10
>>> let tx2 = makeTx "Alice" "Bob" 1
>>> let tx3 = makeTx "Alice" "Charlie" 1
>>> mineTransactions (hash "Charlie") (hash block1) [tx1,tx2,tx3]
(BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2996394280, nonce = 26}
Tx {txFrom = 1912855007, txTo = 2030195168, txAmount = 10000}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}
Tx {txFrom = 2030195168, txTo = 1392748814, txAmount = 1000}
,[TxReceipt {txrBlock = 3725795968, txrProof = MerkleProof (Tx {txFrom = 1912855007, txTo = 2030195168, txAmount = 10000}) <0xae9d56b7>0xbcc3e45a},TxReceipt {txrBlock = 3725795968, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0x3c177e6b<0x1b6a0892},TxReceipt {txrBlock = 3725795968, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 1392748814, txAmount = 1000}) >0x3c177e6b>0x085e2467}])
-}

{- | Benke example of merkleproof
>>> buildProof 'a' $ buildTree "a"
Just (MerkleProof 'a' )
>>> buildProof 'i' $ buildTree "bitcoin"
Just (MerkleProof 'i' <0x5214666a<0x7400b6ff>0x00000062)
-}

{- | Benke print
>>> print $ drawTree $ buildTree "a"
"0x00000061 'a'\n"
-}

{- | Benke sample pprBlock
>>> runShows $ pprBlock block0
hash: 0x70b432e0
parent: 0000000000
miner: 0x7203d9df
root: 0x5b10bd5d
nonce: 18
Tx# 0x5b10bd5d from: 0000000000 to: 0x7203d9df amount: 50000
>>> runShows $ pprBlock block1
hash: 0x2f83ae40
parent: 0x70b432e0
miner: 0x790251e0
root: 0x5ea7a6f0
nonce: 0
Tx# 0x5ea7a6f0 from: 0000000000 to: 0x790251e0 amount: 50000
-}

{- | Benke paths
>>> merklePaths 'i' $ buildTree "bitcoin"
[[Left 1377068650,Left 1946203903,Right 98],[Right 1777612924,Left 1845538200,Right 111]]
-}

{- | Benke pprListWith
>>> runShows $ pprListWith showString ["asd", "zxc"]
asd
zxc
-}


-- WARNING implementation specific
{- | Lazy Test
>>> let t = Node (Leaf '1' (hash '1')) (hash 'f') (Twig undefined (hash 'f'))
>>> buildProof '1' t
Just (MerkleProof '1' <0x00000066)
-}
