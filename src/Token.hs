module Token where

import MyPrelude

data Keyword = K_break | K_let | K_forever | K_if | K_return | K_var | K_while deriving (Eq, Show)

data BracketKind = Round | Curly | Square deriving (Eq, Show)

data BracketDirection = Open | Close deriving (Eq, Show)

data Bracket = Bracket { bracketKind :: !BracketKind, bracketDirection :: !BracketDirection } deriving (Eq, Show)

data Token = Keyword !Keyword | Name !Text | BinaryOperator !BinaryOperator | UnaryOperator !UnaryOperator | Bracket' !Bracket | Number !Integer | EqualsSign | Comma | Semicolon | Newline
             deriving (Eq, Show)

tokenize :: Text -> [Token]
tokenize = todo
