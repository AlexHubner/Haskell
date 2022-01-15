module AbsLI where

import Data.Maybe
import Data.List
import PrintLI

type Nome = String

data Tipo = TipoClasse Nome | TipoInt | TipoBool | Pont {param :: [Tipo], res :: Tipo} | TipoUn deriving (Eq)

instance Show Tipo where
    show (TipoClasse c) = c
    show (TipoInt) = "int"
    show (Pont ps s) = "(" ++ virg ps ++ ")" ++ show p
    show (TipoUn) = "unidade"

newtype Programa = Programa [DefClasse] deriving (Show)

data DefClasse = DefClasse {nclasse :: Nome, campos :: [DefCampos], metodos :: [DefMetodos]}

instance Show DefClasse where
    show DefClasse {nclasse, campos, metodos} = 
        "classe " ++ nclasse ++ concatMap show campos ++ concatMap show metodos "fim"

data DefCampos = DefCampos {nomecampo :: Nome, tipocampo :: Tipo}

data Parametro = Parametro {nomepar :: Nome, tipopar :: Tipo}

instance Show Parametro where 
    show Parametro{nomepar, tipopar} = nomepar ++ " : " ++ show tipopar

data DefMetodos = DefMetodos {nomemet :: Nome, parmetodos :: [Parametro], tipomet :: Tipo, corpomet :: Exp}

instance Show DefMetodos where
    show DefMetodos{nomemet, parmetodos, tipomet, corpomet} =
        "def " ++ nomemet ++ "(" ++ virg parmetodos ++ ") : " ++ show tipomet ++ show corpomet
    
-- Operações com inteiros
data Op = Adi | Sub | Mul | Div deriving  (Eq)

instance Show Op where
    show Adi = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

data Exp = LitBool {tipoexp :: Maybe Tipo, valorbool :: Bool}
        |  LitInt {tipoexp :: Maybe Tipo, valorint :: Int}
        |  Nulo {tipoexp :: Maybe Tipo}
        |  Nulo'{tipoexp :: Maybe Tipo, parame :: [Parametro], corpo :: Exp}
        |  AVariarvel {tipoexp :: Maybe Tipo, nome :: Nome}
        |  ACampo {tipoexp :: Maybe Tipo, objet :: Exp, nome :: Nome}
        |  Atribuir {tipoexp :: Maybe Tipo, at1 :: Exp, at2 :: Exp}
        |  CMetodo {tipoexp :: Maybe Tipo, objet :: Exp, nome :: Nome, arg :: [Exp]}
        |  CFun {tipoexp :: Maybe Tipo, objet :: Exp, arg :: [Exp]}
        |  If {tipoexp :: Maybe Tipo, cnd :: Exp, thn :: Exp, els :: Exp}
        |  Let {tipoexp :: Maybe Tipo, nome :: Nome, valor :: Exp, corpo :: Exp}
        |  Binar {tipoexp :: Maybe Tipo, op :: OP, at1 :: Exp, at2 :: Exp}
        |  New {tipoexp :: Maybe Tipo, tp :: Tipo, arg :: [Exp]}
        | Relac {tipoexp :: Maybe Tipo, corpo :: Exp, tp :: Tipo}



-- Checar se um nome é um construtor:
eNomeConstrutor = (=="init")

-- Criar vírgula
virg :: Show v => [v] -> String
virg = intercalate ", " . map show