module TL1.Parse
       (TL1_Variable(..),
        TL1_Expr(..),
        TL1_Step(..),
        TL1_FormatSpecifier(..),
        TL1_Statement(..),
        TL1_Def(..),
        TL1_Program(..),
        parseTL1)
where

import TL1.Lex

import Data.Char
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Pos
import Control.Monad
import Data.Maybe

data TL1_Variable
  = TL1_GVar String
  | TL1_LVar String
  | TL1_GArr String TL1_Expr
  | TL1_LArr String TL1_Expr
  | TL1_Mem TL1_Expr TL1_Expr
  deriving Show

data TL1_Expr
  = TL1_Plus TL1_Expr TL1_Expr
  | TL1_PlusWithCarry TL1_Expr TL1_Expr
  | TL1_Minus TL1_Expr TL1_Expr
  | TL1_MinusWithBorrow TL1_Expr TL1_Expr
  | TL1_Mul TL1_Expr TL1_Expr
  | TL1_Div TL1_Expr TL1_Expr
  | TL1_LessThan TL1_Expr TL1_Expr
  | TL1_GreaterThan TL1_Expr TL1_Expr
  | TL1_NotEqual TL1_Expr TL1_Expr
  | TL1_Equal TL1_Expr TL1_Expr
  | TL1_LT TL1_Expr TL1_Expr
  | TL1_GT TL1_Expr TL1_Expr
  | TL1_And TL1_Expr TL1_Expr
  | TL1_Or TL1_Expr TL1_Expr
  | TL1_Eor TL1_Expr TL1_Expr
  | TL1_ADC TL1_Expr TL1_Expr
  | TL1_SBC TL1_Expr TL1_Expr
  | TL1_FuncCall String [TL1_Expr]
  | TL1_VariableRef TL1_Variable
  | TL1_Const Integer
  | TL1_True
  | TL1_False
  deriving Show

data TL1_Step = TL1_Up | TL1_Down deriving Show

data TL1_FormatSpecifier
  = TL1_LeftDec TL1_Expr
  | TL1_RightHex TL1_Expr TL1_Expr
  | TL1_String String
  | TL1_Ascii TL1_Expr
  | TL1_Space TL1_Expr
  | TL1_CRLF TL1_Expr
  | TL1_Hex TL1_Expr
  deriving Show

type TL1_Array = (String, Integer)

data TL1_Statement
  = TL1_Return
  | TL1_ReturnWithValue TL1_Expr
  | TL1_ProcCall String [TL1_Expr]
  | TL1_If TL1_Expr TL1_Statement TL1_Statement
  | TL1_Block [TL1_Statement]
  | TL1_For TL1_Variable TL1_Expr TL1_Step TL1_Expr TL1_Statement
  | TL1_Repeat TL1_Statement TL1_Expr
  | TL1_While TL1_Expr TL1_Statement
  | TL1_Case TL1_Expr [(TL1_Expr, TL1_Statement)] TL1_Statement
  | TL1_Write TL1_Expr [TL1_FormatSpecifier]
  | TL1_Assignment [TL1_Variable] TL1_Expr
  deriving Show

data TL1_Def
  = TL1_DefFunc String [String] [String] [TL1_Array] TL1_Statement
  | TL1_DefProc String [String] [String] [TL1_Array] TL1_Statement
  deriving Show

data TL1_Program
  = TL1_Program [String] [String] [String] [TL1_Array] TL1_Statement [TL1_Def]
  deriving Show

data TL1_Scope = GlobalScope | FunctionLocalScope | ProcedureLocalScope
               deriving Eq

data Environment =
  Environment { scope::TL1_Scope,
                procedureNames::[String],
                functionNames::[String],
                globalVariableNames::[String],
                globalArrayNames::[(String,Integer)],
                localVariableNames::[String],
                localArrayNames::[(String,Integer)],
                entryNames::[String]}

defaultEnvironment =
  Environment
  GlobalScope
  ["CALL"]
  ["MHIGH",
   "MOD",
   "RND",
   "GET",
   "READ",
   "NOT",
   "NEG",
   "COM",
   "LSR",
   "ASR",
   "ASL",
   "ROR",
   "ROL",
   "USR",
   "RDHEX",
   "RRC",
   "RLC"]
  [] [] [] [] []

type TL1_Parser a = Parsec [(SourcePos,Token)] Environment a

p_comma = p_symbol ','

x <!> y = try x <|> y

putScope :: TL1_Scope -> TL1_Parser ()
putScope scope =
  do st <- getState
     putState st{scope=scope}

getScope :: TL1_Parser TL1_Scope
getScope =
  do st <- getState
     return $ scope st

p_keyword :: String -> TL1_Parser ()
p_keyword s =
  do st <- getState
     tokenPrim (showToken . snd) incPos (test st)
  where
    test st (pos, (IdentifierToken name))
      | isNothing (lookup name (localArrayNames st))
        && name `notElem` localVariableNames st
        && isNothing (lookup name (globalArrayNames st))
        && name `notElem` globalVariableNames st
        && name `notElem` functionNames st
        && name `notElem` procedureNames st
        && name==s = Just ()
      | otherwise = Nothing
    test st _ = Nothing

-- reversed words
k_begin  = p_keyword "BEGIN"
k_end    = p_keyword "END"
k_write  = p_keyword "WRITE"
k_crlf   = p_keyword "CRLF"
k_ascii  = p_keyword "ASCII"
k_space  = p_keyword "SPACE"
k_hex    = p_keyword "HEX"
k_array  = p_keyword "ARRAY"
k_for    = p_keyword "FOR"
k_do     = p_keyword "DO"
k_if     = p_keyword "IF"
k_then   = p_keyword "THEN"
k_else   = p_keyword "ELSE"
k_case   = p_keyword "CASE"
k_repeat = p_keyword "REPEAT"
k_until  = p_keyword "UNTIL"
k_of     = p_keyword "OF"
k_while  = p_keyword "WHILE"
k_return = p_keyword "RETURN"
k_mem    = p_keyword "MEM"
k_to     = p_keyword "TO"     >> return  TL1_Up
k_downto = p_keyword "DOWNTO" >> return TL1_Down
k_var    = p_keyword "VAR"
k_proc   = p_keyword "PROC"
k_func   = p_keyword "FUNC"
k_gt     = p_keyword "GT"     >> return TL1_GT
k_lt     = p_keyword "LT"     >> return TL1_LT
k_and    = p_keyword "AND"    >> return TL1_And
k_or     = p_keyword "OR"     >> return TL1_Or
k_eor    = p_keyword "EOR"    >> return TL1_Eor
k_adc    = p_keyword "ADC"    >> return TL1_ADC
k_sbc    = p_keyword "SBC"    >> return TL1_SBC
k_true   = p_keyword "TRUE"   >> return TL1_True
k_false  = p_keyword "FALSE"  >> return TL1_False

between_paren = between (p_symbol '(') (p_symbol ')')

incPos cpos (pos, x) xs = pos

showToken (IdentifierToken ident) = show ident
showToken (NumericToken n) = show n
showToken (SymbolicToken s) = show s

p_symbol :: Char -> TL1_Parser ()
p_symbol c = tokenPrim (showToken . snd) incPos test
  where
    test (pos, (SymbolicToken k))
      | c == k = Just ()
      | otherwise = Nothing
    test _ = Nothing

p_ident :: TL1_Parser String
p_ident = tokenPrim (showToken . snd) incPos test
  where
    test (pos, (IdentifierToken k)) = Just k
    test _ = Nothing

p_integer :: TL1_Parser Integer
p_integer = tokenPrim (showToken . snd) incPos test
  where
    test (pos, (NumericToken k)) = Just k
    test _ = Nothing

p_string :: TL1_Parser String
p_string = tokenPrim (showToken . snd) incPos test
  where
    test (pos, (StringToken k)) = Just k
    test _ = Nothing

p_function :: TL1_Parser String
p_function =
  do st <- getState
     tokenPrim (showToken . snd) incPos (test st)
  where
    test st (_, (IdentifierToken name)) =
      if isNothing (lookup name (localArrayNames st))
         && name `notElem` localVariableNames st
         && isNothing (lookup name (globalArrayNames st))
         && name `notElem` globalVariableNames st
         && elem name (functionNames st)
      then Just name
      else Nothing
    test _ _ = Nothing

p_procedure :: TL1_Parser String
p_procedure =
  do st <- getState
     tokenPrim (showToken . snd) incPos (test st)
  where
    test st (_, (IdentifierToken name)) =
      if isNothing (lookup name (localArrayNames st))
         && name `notElem` localVariableNames st
         && isNothing (lookup name (globalArrayNames st))
         && name `notElem` globalVariableNames st
         && name `notElem` functionNames st
         && name `elem` procedureNames st
      then Just name
      else Nothing
    test _ _ = Nothing

p_var :: TL1_Parser TL1_Variable
p_var =
  do st <- getState
     tokenPrim (showToken . snd) incPos (test st)
  where
    test st (_, (IdentifierToken name))
      | isNothing (lookup name (localArrayNames st))
        && name `elem` localVariableNames st = Just $ TL1_LVar name
      | isNothing (lookup name (localArrayNames st))
        && name `notElem` localVariableNames st
        && isNothing (lookup name (globalArrayNames st))
        && name `elem` globalVariableNames st = Just $ TL1_GVar name
      | otherwise = Nothing
    test _ _ = Nothing

p_array :: TL1_Parser TL1_Variable
p_array =
  do st <- getState
     arr <- tokenPrim (showToken . snd) incPos (test st)
     ind <- between (p_symbol '[') (p_symbol ']') (p_expr <?> "expression")
     return $ arr ind
  where
    test st (_, (IdentifierToken name))
      | isJust $ lookup name (localArrayNames st) = Just $ TL1_LArr name
      | name `notElem` localVariableNames st
        && isJust (lookup name (globalArrayNames st)) = Just $ TL1_GArr name
      | otherwise = Nothing
    test _ _ = Nothing

p_number :: TL1_Parser Integer
p_number = tokenPrim (showToken . snd) incPos test
  where
    test (_, (NumericToken k)) = Just k
    test _ = Nothing

p_mem :: TL1_Parser TL1_Variable
p_mem =
  do k_mem
     p_symbol '('
     expr1 <- p_expr <?> "<expression>"
     p_symbol ','
     expr2 <- p_expr <?> "<expression>"
     p_symbol ')'
     return $ TL1_Mem expr1 expr2

p_factor :: TL1_Parser TL1_Expr
p_factor = between_paren p_expr
           <!> between (p_symbol '[') (p_symbol ']') p_expr
           <!> between (p_symbol '{') (p_symbol '}') p_expr
           <!> liftM TL1_VariableRef p_var
           <!> p_constant
           <!> p_functionCall
           <!> liftM TL1_VariableRef p_mem
           <!> liftM TL1_VariableRef p_array
           <!> liftM TL1_VariableRef p_var

p_functionCall :: TL1_Parser TL1_Expr
p_functionCall =
  do f <- p_function
     args <- option [] $ between_paren $ sepBy p_expr p_comma
     return $ TL1_FuncCall f args

p_constant :: TL1_Parser TL1_Expr
p_constant = k_true <!> k_false <!> (liftM TL1_Const p_integer)

p_expr :: TL1_Parser TL1_Expr
p_expr = p_additiveWithCurry

p_multitive :: TL1_Parser TL1_Expr
p_multitive = chainl1 p_factor (op_mul <!> op_div)
  where
    op_mul = p_symbol '*' >> return TL1_Mul
    op_div = p_symbol '/' >> return TL1_Div

p_additive :: TL1_Parser TL1_Expr
p_additive = chainl1 p_multitive (op_plus <!> op_minus)
  where
    op_plus  = p_symbol '+' >> return TL1_Plus
    op_minus = p_symbol '-' >> return TL1_Minus

p_relation :: TL1_Parser TL1_Expr
p_relation = chainl1 p_additive op
  where
    op_lt  = p_symbol '<' >> return TL1_LessThan
    op_gt  = p_symbol '>' >> return TL1_GreaterThan
    op_neq = p_symbol '#' >> return TL1_NotEqual
    op_eq  = p_symbol '=' >> return TL1_Equal
    op     = op_lt <!> op_gt <!> op_neq <!> op_eq <!> k_gt <!> k_lt

p_logical :: TL1_Parser TL1_Expr
p_logical = chainl1 p_relation (k_and <!> k_or <!> k_eor) 

p_additiveWithCurry :: TL1_Parser TL1_Expr
p_additiveWithCurry = chainl1 p_logical (k_adc <!> k_sbc)

p_statement :: TL1_Parser TL1_Statement
p_statement =
  p_return
  <!> p_assignment
  <!> p_procCall
  <!> p_case
  <!> p_if
  <!> p_block
  <!> p_for
  <!> p_repeat
  <!> p_while
  <!> p_write

p_write_specifier :: TL1_Parser TL1_FormatSpecifier
p_write_specifier =
  liftM TL1_LeftDec p_expr
  <!> (do { p_symbol '#';
            p_symbol '(';
            expr1 <- p_expr;
            p_comma;
            expr2 <- p_expr;
            p_symbol ')';
            return $ TL1_RightHex expr1 expr2
          })
  <!> liftM TL1_String p_string
  <!> (k_ascii >> liftM TL1_Ascii arg1)
  <!> (k_space >> liftM TL1_Space arg1)
  <!> (k_crlf >> (liftM TL1_CRLF $ option (TL1_Const 0) arg1))
  <!> (k_hex >> liftM TL1_Hex arg1)
  <?> "<format specifier>"
  where
    arg1 = between_paren p_expr

p_write :: TL1_Parser TL1_Statement
p_write =
  do k_write
     p_symbol '('
     device <- p_expr <?> "<expression of device number>"
     p_symbol ':' <?> "':'"
     specifiers <- sepBy p_write_specifier p_comma
     p_symbol ')'
     return $ TL1_Write device specifiers

p_return :: TL1_Parser TL1_Statement
p_return =
  do k_return
     scope <- getScope
     expr <- if scope==FunctionLocalScope
             then (liftM TL1_ReturnWithValue p_expr)
             else return TL1_Return
     return expr

p_arguments :: TL1_Parser [TL1_Expr]
p_arguments = between_paren $ sepBy p_expr p_comma

p_procCall  :: TL1_Parser TL1_Statement
p_procCall =
  do f <- p_procedure
     args <- option [] $ between_paren $ sepBy p_expr p_comma
     return $ TL1_ProcCall f args

p_if :: TL1_Parser TL1_Statement
p_if =
  do k_if
     cond_clause <- p_expr <?> "<expression>"
     k_then <?> "THEN"
     then_clause <- p_statement <?> "<statement>"
     else_clause <- option (TL1_Block [])
                           (k_else >> p_statement <?> "<statement>")
     return $ TL1_If cond_clause then_clause else_clause

p_block :: TL1_Parser TL1_Statement
p_block = liftM TL1_Block
          (between k_begin k_end (many p_statement)
          <!> between (p_symbol '{') (p_symbol '}') (many p_statement)
          <!> between (p_symbol '[') (p_symbol ']') (many p_statement)
          <!> between_paren (many p_statement))

p_for :: TL1_Parser TL1_Statement
p_for =
  do k_for
     var <- p_var <?> "<simple variable>"
     p_symbol ':'
     p_symbol '='
     init <- p_expr <?> "<expression>"
     dir <- k_to <!> k_downto <?> "TO or DOWNTO"
     end <- p_expr <?> "<expression>"
     k_do <?> "DO"
     stat <- p_statement <?> "<statement>"
     return $ TL1_For var init dir end stat

p_repeat :: TL1_Parser TL1_Statement
p_repeat =
  do k_repeat
     statements <- many p_statement <?> "<statement>"
     k_until <?> "UNTIL"
     expr <- p_expr <?> "<expression>"
     return $ TL1_Repeat (TL1_Block statements) expr

p_while =
  do k_while
     expr <- p_expr <?> "<expression>"
     k_do <?> "DO"
     stat <- p_statement <?> "<statement>"
     return $ TL1_While expr stat

p_case :: TL1_Parser TL1_Statement
p_case =
  do k_case
     expr <- p_expr <?> "<expression>"
     k_of <?> "OF"
     clauses <-  many $ liftM2 (,) p_expr (p_statement <?> "<statement>")
     k_else <?> "ELSE"
     else_clause <- p_statement <?> "<statement>"
     return $ TL1_Case expr clauses else_clause

p_assignment :: TL1_Parser TL1_Statement
p_assignment =
  do v <- sepBy (p_var <!> p_array <!> p_mem) p_comma
     p_symbol ':'
     p_symbol '='
     e <- p_expr <?> "<expression>"
     return $ TL1_Assignment v e

p_vdec :: TL1_Parser [String]
p_vdec = k_var >> sepBy p_ident p_comma

p_pdec :: TL1_Parser [String]
p_pdec = k_proc >> sepBy p_ident p_comma

p_fdec :: TL1_Parser [String]
p_fdec = k_func >> sepBy p_ident p_comma

p_index :: TL1_Parser Integer
p_index = between (p_symbol '[') (p_symbol ']') (p_number <?> "<number>")

p_adec :: TL1_Parser [TL1_Array]
p_adec = k_array >> sepBy (liftM2 (,) p_ident p_index) p_comma

p_defineFunc :: TL1_Parser TL1_Def
p_defineFunc =
  do name <- p_function
     params <- option [] $ between_paren $ sepBy p_ident p_comma
     vars <- option [] p_vdec
     arrs <- option [] p_adec
     env <- getState
     putState env { localVariableNames=vars++params,
                    localArrayNames=arrs }
     k_begin
     putScope FunctionLocalScope
     statements <- many p_statement <?> "<statement>"
     k_end <?> "END"
     putScope GlobalScope
     putState env
     return $ TL1_DefFunc name params vars arrs (TL1_Block statements)

p_defineProc :: TL1_Parser TL1_Def
p_defineProc =
  do name <- p_procedure
     params <- option [] $ between_paren $ sepBy p_ident p_comma
     vars <- option [] p_vdec
     arrs <- option [] p_adec
     env <- getState
     putState env { localVariableNames=vars++params,
                    localArrayNames=arrs }
     k_begin
     putScope ProcedureLocalScope
     statements <- many p_statement <?> "<statement>"
     k_end <?> "END"
     putScope GlobalScope
     putState env
     return $ TL1_DefProc name params vars arrs (TL1_Block statements)

p_program :: TL1_Parser TL1_Program
p_program =
  do procs <- option [] p_pdec
     funcs <- option [] p_fdec
     vars  <- option [] p_vdec
     arrs  <- option [] p_adec
     k_begin
     env <- getState
     putState env { procedureNames=(procedureNames env)++procs,
                    functionNames=(functionNames env)++funcs,
                    globalVariableNames=vars,
                    globalArrayNames=arrs}
     main_prog <- many p_statement <?> "<statement>"
     k_end <?> "END"
     sub_progs <- many (p_defineProc <!> p_defineFunc)
     eof
     return $ TL1_Program procs funcs vars arrs (TL1_Block main_prog) sub_progs

parseTL1 source name =
  tokenizeTL1 source name >>= runParser p_program defaultEnvironment name
