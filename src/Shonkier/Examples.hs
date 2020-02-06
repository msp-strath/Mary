module Shonkier.Examples where

import Data.Bwd
import Shonkier.Syntax

env :: Env
env = singleton "append" $ VFun [] env
    [ ( [PCell (PBind "x") (PBind "xs"), PBind "ys"]
      , Cell (Var "x") (App (Var "append") [Var "xs", Var "ys"])
      )
    , ( [PAtom "", PBind "ys"]
      , Var "ys"
      )
    ]

onetwo :: Term
onetwo = Cell (Atom "1") (Cell (Atom "2") (Atom ""))

threefour :: Term
threefour = Cell (Atom "3") (Cell (Atom "4") (Atom ""))

onetwothreefour :: Term
onetwothreefour = App (Var "append") [onetwo, threefour]

test :: Computation
test = eval Nil (env, onetwothreefour)
