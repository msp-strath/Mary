module Shonkier.Lexer.Examples where

import Shonkier.Lexer

string :: [Token]
string = lexing "foo\"example\"foo -> 013 -> (-0\"another\"-0, \"final\")"
