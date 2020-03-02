module Shonkier.Examples where

import Data.Map (singleton)
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2

import Data.Bwd
import Shonkier.Syntax
import Shonkier.Semantics

appendEnv :: Env
appendEnv = singleton "append" $ VFun [] appendEnv []
    [ ( PValue <$> [ PCell (PBind "x") (PBind "xs")
                   , PBind "ys"
                   ]
      , Cell (Var "x") (App (Var "append") [Var "xs", Var "ys"])
      )
    , ( PValue <$> [ PAtom ""
                   , PBind "ys"
                   ]
      , Var "ys"
      )
    ]

onetwo :: Term
onetwo = Cell (Atom "1") (Cell (Atom "2") (Atom ""))

threefour :: Term
threefour = Cell (Atom "3") (Cell (Atom "4") (Atom ""))

onetwothreefour :: Term
onetwothreefour = App (Var "append") [onetwo, threefour]

appendTest :: Computation
appendTest = eval Nil (appendEnv, onetwothreefour)


readerEnv :: Env
readerEnv = singleton "runReader" $ VFun [] readerEnv [[],["ask"]]
     [ ( PValue <$> [PBind "_", PBind "val"]
       , Var "val"
       )
     , ( [PValue (PBind "r"), PRequest ("ask", []) "k"]
       , App (Var "runReader") [ Var "r"
                               , App (Var "k") [Var "r"]
                               ]
       )
     ]

onetwoSquared :: Term
onetwoSquared = App (Var "runReader")
  [ onetwo
  , App (Var "append") [ask, ask]
  ] where ask = App (Atom "ask") []


askTest :: Computation
askTest = eval Nil (appendEnv <> readerEnv, onetwoSquared)


stateEnv :: Env
stateEnv = singleton "runState" $ VFun [] stateEnv [[],["get", "put"]]
  [ ( PValue <$> [PBind "_", PBind "val"]
    , Var "val"
    )
  , ( [ PValue (PBind "s"), PRequest ("get", []) "k"]
    , App (Var "runState") [ Var "s"
                           , App (Var "k") [Var "s"]
                           ]
    )
  , ( [ PValue (PBind "_"), PRequest ("put", [PBind "s"]) "k"]
    , App (Var "runState") [ Var "s"
                           , App (Var "k") [Atom ""]
                           ]
    )
  ]

mapEnv :: Env
mapEnv = singleton "map" $ VFun [] mapEnv []
  [ ( PValue <$> [ PBind "f", PAtom "" ]
    , Atom ""
    )
  , ( PValue <$> [ PBind "f", PCell (PBind "x") (PBind "xs") ]
    , Cell (App (Var "f") [Var "x"]) (App (Var "map") (Var <$> ["f", "xs"]))
    )
  ]

lam :: Variable -> (Term -> Term) -> Term
lam x b = Fun [] [ ( [ PValue (PBind x)]
                   , b (Var x)
                   )
                 ]

inc :: Term
inc = App f [ App (Atom "get") []
            , App (Atom "put") [ Cell (Atom "bip") (App (Atom "get") []) ]
            ] where

  f = Fun [] [ ( PValue . PBind <$> ["v", "_"]
               , Var "v"
               )
             ]

bipping :: Term
bipping = App (Var "runState")
  [ Atom ""
  , App (Var "map") [ lam "_" (\ _ -> inc)
                    , onetwothreefour
                    ]
  ]

stateTest :: Computation
stateTest = eval Nil (mapEnv <> stateEnv <> appendEnv, bipping)

primEnv :: Env
primEnv = foldMap (\ str -> singleton str $ VPrim str [])
        [ "primStringConcat"
        , "primNumAdd"
        ]

mkPrim :: String -> [Literal] -> Computation
mkPrim p ls = eval Nil (primEnv, App (Var p) (Lit <$> ls))

strConcat :: [Literal] -> Computation
strConcat = mkPrim "primStringConcat"

helloworld :: Computation
helloworld = strConcat $ String "foo" <$> ["hello ", "world", "!"]

helloworld' :: Computation
helloworld' = strConcat $ String "" <$> ["hello ", "world", "!"]

foogoo :: Computation
foogoo = strConcat [String "foo" "fo", String "goo" "\"foo", String "" " oof!"]

listConcat :: Computation
listConcat = eval Nil (primEnv, App (Var "primStringConcat") [str]) where
  str = Cell (Cell (TString "" "hello")
                   (Cell (TString "" " ") (TString "" "world")))
             (Cell (TString "" "!") (TString "" "\n"))

numAdd :: [Literal] -> Computation
numAdd = mkPrim "primNumAdd"

three :: Computation
three = numAdd (Num <$> [1, 2])
