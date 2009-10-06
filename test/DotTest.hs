module Main where

import Text.Dot

data Animation = Start

src label = node $ [ ("shape","none"),("label",label) ]
box label = node $ [ ("shape","box"),("style","rounded"),("label",label) ]

diamond label = node $ [("shape","diamond"),("label",label),("fontsize","10")]


main = putStrLn $ showDot $ do
	attribute ("size","40,15")
	attribute ("rankdir","LR")
	refSpec <- src "S"
	tarSpec <- src "T"
	same [refSpec,tarSpec]

	c1 <- box "S"
	c2 <- box "C"
	c3 <- box "F"
	same [c1,c2,c3]

	refSpec .->. c1	
	tarSpec .->. c2	
	tarSpec .->. c3

	m1 <- box "x"
	m2 <- box "y"
	ntm <- box "z"

	same [m1,m2,ntm] 
	c1 .->. m1
	c2 .->. m2

	xilinxSynthesis <- box "x"
	c3 .->. xilinxSynthesis

	gns <- box "G"
	xilinxSynthesis .->. gns

	gns .->. ntm

	ecs <- sequence
		[ diamond "E"
		, diamond "E"
		, diamond "Eq"
		]
	same ecs	

	m1 .->. (ecs !! 0)
	m1 .->. (ecs !! 1)
	m2 .->. (ecs !! 0)
	m2 .->. (ecs !! 2)
	ntm .->. (ecs !! 1)
	ntm .->. (ecs !! 2)

	sequence [ do evidence <- src "EE"
		      n .->. evidence
		 | n <- ecs 
		 ]


	edge refSpec tarSpec [("label","Engineering\nEffort"),("style","dotted")]

	() <- scope $ do v1 <- box "Hello"
		         v2 <- box "World"
		         v1 .->. v2

	(x,()) <- cluster $
		do v1 <- box "Hello"
		   v2 <- box "World"
		   v1 .->. v2
			
--	x .->. m2
	-- for hpc
	() <- same [x,x]
	v <- box "XYZ"
	v .->. v
	() <- attribute ("rankdir","LR")

	let n1 = userNodeId 1
	let n2 = userNodeId (-1)

	() <- n1 `userNode` [ ("shape","box")]
	n1 .->. n2
	
	v'' <- box "XYZ"

        box "(\n\\n)\"(/\\)"

	netlistGraph (\ a -> [("label","X" ++ show a)])
	             (\ a -> [succ a `mod` 10,pred a `mod` 10])
	             [ (n,n) | n <- [0..9]]
	             
	
	return ()

