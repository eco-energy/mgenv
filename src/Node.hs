module Node where


{---

I want a spacetime graph

Graph Construction:
Use algebraic-graphs to assemble a test graph
1. Node Features
2. Edge Features

Have functions that give you:
1) outgoing :: Node -> [Node]
2) incoming :: Node -> [Node]

---}


data NodeType = Storage { batteryVoltage :: Double }
              | Generation { produced :: Double }
              | Consumption { consumed :: Double
                            , demand :: Double }
              deriving (Eq, Ord)

data EdgeType = Send | Recieve deriving (Eq, Ord)
