-- |
-- Module: Text.Dot
-- Copyright: Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: portable
--
-- This module provides a simple interface for building .dot graph files, for input into the dot and graphviz tools. 
-- It includes a monadic interface for building graphs.

module Text.Dot 
	( 
	  -- * Dot
	  Dot		-- abstract
	  -- * Nodes
	, node
	, NodeId	-- abstract
	, userNodeId
	, userNode
	  -- * Edges
	, edge
        , edge'
	, (.->.)
	  -- * Showing a graph
	, showDot
	  -- * Other combinators
	, scope
	, attribute
	, share
	, same
	, cluster
	  -- * Simple netlist generation
	, netlistGraph
	) where

import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

data DotGraph = DotGraph [GraphElement]

data NodeId = NodeId String
	    | UserNodeId Int

instance Show NodeId where
  show (NodeId str) = str
  show (UserNodeId i) 
	| i < 0     = "u_" ++ show (negate i)
	| otherwise = "u" ++ show i

data GraphElement = GraphAttribute String String
		  | GraphNode NodeId        [(String,String)]
		  | GraphEdge NodeId NodeId [(String,String)]
		  | GraphEdge' NodeId (Maybe String) NodeId (Maybe String) [(String,String)]
		  | Scope           [GraphElement]
		  | SubGraph NodeId [GraphElement]

data Dot a = Dot { unDot :: Int -> ([GraphElement],Int,a) }

instance Monad Dot where
  return a = Dot $ \ uq -> ([],uq,a)
  m >>= k  = Dot $ \ uq -> case unDot m uq of
			   (g1,uq',r) -> case unDot (k r) uq' of
					   (g2,uq2,r2) -> (g1 ++ g2,uq2,r2)

-- | 'node' takes a list of attributes, generates a new node, and gives a 'NodeId'.
node      :: [(String,String)] -> Dot NodeId
node attrs = Dot $ \ uq -> let nid = NodeId $ "n" ++ show uq 
			  in ( [ GraphNode nid attrs ],succ uq,nid)


-- | 'userNodeId' allows a user to use their own (Int-based) node id's, without needing to remap them.
userNodeId :: Int -> NodeId
userNodeId i = UserNodeId i

-- | 'userNode' takes a NodeId, and adds some attributes to that node. 
userNode :: NodeId -> [(String,String)] -> Dot ()
userNode nId attrs = Dot $ \ uq -> ( [GraphNode nId attrs ],uq,())

-- | 'edge' generates an edge between two 'NodeId's, with attributes.
edge      :: NodeId -> NodeId -> [(String,String)] -> Dot ()
edge  from to attrs = Dot (\ uq -> ( [ GraphEdge from to attrs ],uq,()))

-- | 'edge' generates an edge between two 'NodeId's, with optional node sub-labels, and attributes.
edge'      :: NodeId -> Maybe String -> NodeId -> Maybe String -> [(String,String)] -> Dot ()
edge'  from optF to optT attrs = Dot (\ uq -> ( [ GraphEdge' from optF to optT attrs ],uq,()))

-- | '.->.' generates an edge between two 'NodeId's.
(.->.) from to = edge from to []

-- | 'scope' groups a subgraph together; in dot these are the subgraphs inside "{" and "}".
scope     :: Dot a -> Dot a
scope (Dot fn) = Dot (\ uq -> case fn uq of
			      ( elems,uq',a) -> ([Scope elems],uq',a))

-- | 'share' is when a set of nodes share specific attributes. Usually used for layout tweaking.
share :: [(String,String)] -> [NodeId] -> Dot ()
share attrs nodeids = Dot $ \ uq -> 
      ( [ Scope ( [ GraphAttribute name val | (name,val) <- attrs]
	       ++ [ GraphNode nodeid [] | nodeid <- nodeids ]
	       ) 
        ], uq, ())

-- | 'same' provides a combinator for a common pattern; a set of 'NodeId's with the same rank.
same :: [NodeId] -> Dot ()
same = share [("rank","same")]


-- | 'cluster' builds an explicit, internally named subgraph (called cluster). 
cluster :: Dot a -> Dot (NodeId,a)
cluster (Dot fn) = Dot (\ uq -> 
		let cid = NodeId $ "cluster_" ++ show uq 
		in case fn (succ uq) of
		    (elems,uq',a) -> ([SubGraph cid elems],uq',(cid,a)))

-- | 'attribute' gives a attribute to the current scope.
attribute :: (String,String) -> Dot ()
attribute (name,val) = Dot (\ uq -> ( [  GraphAttribute name val ],uq,()))

-- 'showDot' renders a dot graph as a 'String'.
showDot :: Dot a -> String
showDot (Dot dm) = case dm 0 of
		    (elems,_,_) -> "digraph G {\n" ++ unlines (map showGraphElement elems) ++ "\n}\n"

showGraphElement (GraphAttribute name val) = showAttr (name,val) ++ ";"
showGraphElement (GraphNode nid attrs)           = show nid ++ showAttrs attrs ++ ";"
showGraphElement (GraphEdge from to attrs) = show from ++ " -> " ++ show to ++  showAttrs attrs ++ ";"
showGraphElement (GraphEdge' from optF to optT attrs) = showName from optF ++ " -> " ++ showName to optT ++  showAttrs attrs ++ ";"
    where showName n Nothing = show n
          showName n (Just t) = show n ++ ":" ++ t
showGraphElement (Scope elems) = "{\n" ++ unlines (map showGraphElement elems) ++ "\n}"
showGraphElement (SubGraph nid elems) = "subgraph " ++ show nid ++ " {\n" ++ unlines (map showGraphElement elems) ++ "\n}"

showAttrs [] = ""
showAttrs xs = "[" ++ showAttrs' xs ++ "]"
    where
	-- never empty list
	showAttrs' [a]    = showAttr a
	showAttrs' (a:as) = showAttr a ++ "," ++ showAttrs' as

showAttr (name,val) = name ++ "=\""   ++ foldr showsDotChar "" val ++ "\""

showsDotChar '"'  = ("\\\"" ++)
showsDotChar '\\' = ("\\\\" ++)
showsDotChar x    = showLitChar x


-- | 'netlistGraph' generates a simple graph from a netlist.
netlistGraph :: (Ord a) 
          => (b -> [(String,String)])   -- ^ Attributes for each node
          -> (b -> [a])                 -- ^ Out edges leaving each node
          -> [(a,b)] 			-- ^ The netlist
	  -> Dot ()
netlistGraph attrFn outFn assocs = do
    let nodes = S.fromList $ [ a | (a,_) <- assocs ]
    let outs  = S.fromList $ [ o | (_,b) <- assocs
				 , o <- outFn b 
			     ]
    nodeTab <- sequence [ do nd <- node (attrFn b)
                             return (a,nd)
                        | (a,b) <- assocs ]
    otherTab <- sequence [ do nd <- node []
                              return (o,nd)
                         | o <- S.toList outs
                         , o `S.notMember` nodes
                         ]
    let fm = M.fromList (nodeTab ++ otherTab)
    sequence_ [ (fm M.! src) .->. (fm M.! dst)
              | (dst,b) <- assocs
              , src     <- outFn b
              ]
    return ()

