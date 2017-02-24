data Index :: *1 where
 Inil  :: Index
 Icons :: Tag ~> Index ~> Index
  deriving List(i)

data Count :: Tag ~> Index ~> *0 where
 Czero :: Count t [t;xs]i
 Csucc :: Count t xs -> Count t [x;xs]i
  deriving Nat(c)

data Subset :: Index ~> Index ~> *0 where
 Snil :: Subset []i xs
 Scons :: Count t xs -> Label t -> Subset ts xs -> Subset [t;ts]i xs
  deriving Record(s)

t0 = {2c=`eat,0c=`shop}s :: Subset [`eat,`shop]i [`shop,a,`eat; b]i

data Step :: Tag ~> Index ~> Index ~> *0 where
 Step :: String
      -> Label t
      -> Subset is xs
      -> Step t is xs

data Plan :: Index ~> *0 where
 Pnil  :: Plan []i
 Pcons :: Step t is xs -> Plan xs -> Plan [t;xs]i
  deriving List(p)

t1 = [step4,step3,step2,step1]p
  where
    step4 = Step "s4" `read {1c=`shop}s
    step3 = Step "s3" `cook {0c=`shop}s
    step2 = Step "s2" `shop {0c=`wake}s
    step1 = Step "s1" `wake {}s

-- addTask :: Plan xs -> Plan [x;xs]
-- addTask = Pcons (Step with empty prereqs)

-- mkPreReq :: Label i -> Label j -> Plan xs -> exists ys. Plan ys
-- t2 = mkPreReq `cook `shop myplan
