-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module Exp.Abs where

import Data.Aeson as JSON
import GHC.Generics (Generic)

newtype AIdent = AIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

newtype CIdent = CIdent String
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

newtype HoleIdent = HoleIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Module = Module AIdent [Imp] [Decl]
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Imp = Import AIdent
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Decl
    = DeclDef AIdent [Tele] Exp ExpWhere
    | DeclData AIdent [Tele] [Label]
    | DeclHData AIdent [Tele] [Label]
    | DeclSplit AIdent [Tele] Exp [Branch]
    | DeclUndef AIdent [Tele] Exp
    | DeclMutual [Decl]
    | DeclOpaque AIdent
    | DeclTransparent AIdent
    | DeclTransparentAll
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data ExpWhere = Where Exp [Decl] | NoWhere Exp
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Exp
    = Let [Decl] Exp
    | Lam [PTele] Exp
    | PLam [AIdent] Exp
    | Split Exp [Branch]
    | Fun Exp Exp
    | Pi [PTele] Exp
    | Sigma [PTele] Exp
    | AppFormula Exp Formula
    | App Exp Exp
    | PathP Exp Exp Exp
    | Comp Exp Exp System
    | HComp Exp Exp System
    | Trans Exp Exp
    | Fill Exp Exp System
    | Glue Exp System
    | GlueElem Exp System
    | UnGlueElem Exp System
    | Id Exp Exp Exp
    | IdPair Exp System
    | IdJ Exp Exp Exp Exp Exp Exp
    | Fst Exp
    | Snd Exp
    | Pair Exp [Exp]
    | Var AIdent
    | PCon AIdent Exp
    | U
    | Hole HoleIdent
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Dir = Dir0 | Dir1
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data System = System [Side]
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Face = Face AIdent Dir
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Side = Side [Face] Exp
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Formula
    = Disj Formula Formula
    | Conj Formula CIdent Formula
    | Neg Formula
    | Atom AIdent
    | Dir Dir
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Branch
    = OBranch AIdent [AIdent] ExpWhere
    | PBranch AIdent [AIdent] [AIdent] ExpWhere
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Label
    = OLabel AIdent [Tele] | PLabel AIdent [Tele] [AIdent] System
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data Tele = Tele AIdent [AIdent] Exp
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

data PTele = PTele Exp Exp
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

