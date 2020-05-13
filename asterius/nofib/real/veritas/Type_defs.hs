
module Type_defs where

import Core_datatype

import Kernel

data MayBe a b = Ok a | Bad b deriving ( Eq )

data Token = Rvd String | Clr String | Bdr Binder_conn |

  -- partain:
instance Show Token

data Flagged_ITrm = Opr Operator Oprtype Int |

data Operand = Itrm ITrm | Idec IDec | Isgn ISgn   -- normal operands

data Operator = OpItrm ITrm | OpBdr Binder_conn

type Itrm_fn = Flagged_ITrm -> ITrm

type Trm_fn = Flagged_ITrm -> Trm

type Deriv_fn = Flagged_ITrm -> Thm

type Tag = ( String , [Tag_Arg_type] , Itrm_fn , Trm_fn , Deriv_fn )

data Tag_Arg_type = Term_Arg | Deriv_Arg | Int_Arg deriving ( Eq )
-}
