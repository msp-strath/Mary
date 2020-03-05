{-# LANGUAGE OverloadedStrings #-}

module Shonkier.Parser.Examples where

import Shonkier.Parser
import Shonkier.Syntax
import Shonkier.Semantics

import Data.Foldable

mapT :: Term
mapT = getMeATerm
   "{ f, []     -> []                   \
  \ | f, [x|xs] -> [f(x)|map(f, xs)]   \
  \ }"

runReaderT :: Term
runReaderT = getMeATerm
   "{ r, v              -> v                    \
  \ | r, {'ask() -> k}  -> runReaderT(r, k(r))  \
  \ }"

appendP :: Program
appendP = getMeAProgram
  "append([],     ys) -> ys                \
  \append([x|xs], ys) -> [x|append(xs, ys)]"

mapP :: Program
mapP = getMeAProgram
  "map(f,[]) -> []                  \
  \map(f,[x|xs]) -> [f(x)|map(f,xs)]"

runReaderP :: Program
runReaderP = getMeAProgram
  "runReader(,'ask):                              \
  \runReader(r,v) -> v                            \
  \runReader(r,{'ask() -> k}) -> runReader(r,k(r))"

pipeP :: Program
pipeP = getMeAProgram
  "pipe('send,'recv):                                            \
  \pipe({'send(x) -> ks},{'recv() -> kr}) -> pipe(ks([]),kr(x))  \
  \pipe({s},v) -> v                                              \
  \pipe(v,{r}) -> 'abort()                                       "


test0 :: Computation
test0 = shonkier (mkGlobalEnv . fold $ [runReaderP, appendP]) $ getMeATerm
  "runReader(['1 '2],append('ask(),'ask()))"

runStateP :: Program
runStateP = getMeAProgram
  "runState(, 'get 'put):                                         \
  \runState(s, v) -> v                                            \
  \runState(s, {'get() -> k})  -> runState(s, k(s))               \
  \runState(x, {'put(s) -> k}) -> runState(s, k([]))              "

semiP :: Program
semiP = getMeAProgram
  "semi(x,y) -> y  \
  \imes(x,y) -> x  "

bipperP :: Program
bipperP = getMeAProgram
  "bipper() -> semi('send('get()),semi('put(['bip|'get()]),bipper()))"

test1 :: Computation
test1 = shonkier (mkGlobalEnv . fold $ [runStateP, pipeP, semiP, bipperP, mapP]) $ getMeATerm
  "runState([],pipe(bipper(),map({x -> 'recv()},[[] [] [] []])))"

string :: Term
string = getMeATerm "f(foo\"oulala\"foo, g(\"oula\", goo\"ou\"la\"la\"goo))"

string2 :: Term
string2 = getMeATerm "\" \
  \hallo   \n\
  \ wolrd\n\""

num :: Term
num = getMeATerm "foo(3.4,6.75,8.25,2/3,1/4,18.000,6/4,3.400,3)"
