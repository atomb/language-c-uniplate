module Language.C.Syntax.UniplateInstances where

import Data.Generics.PlateDirect
import Language.C.Syntax.AST

-----------------------------------------------------------------------------
-- Uniplate instances
-----------------------------------------------------------------------------

instance Uniplate (CExpression a) where
  uniplate (CComma es ni)         = plate CComma ||* es |- ni
  uniplate (CAssign op l r ni)    = plate CAssign |- op |* l |* r |- ni
  uniplate (CCond c t e ni)       = plate CCond |* c |+ t |* e |- ni
  uniplate (CBinary op l r ni)    = plate CBinary |- op |* l |* r |- ni
  uniplate (CCast d e ni)         = plate CCast |- d |* e |- ni
  uniplate (CUnary op e ni)       = plate CUnary |- op |* e |- ni
  uniplate (CSizeofExpr e ni)     = plate CSizeofExpr |* e |- ni
  uniplate (CSizeofType d ni)     = plate CSizeofType |- d |- ni
  uniplate (CAlignofExpr e ni)    = plate CAlignofExpr |* e |- ni
  uniplate (CAlignofType d ni)    = plate CAlignofType |- d |- ni
  uniplate (CComplexReal e ni)    = plate CComplexReal |* e |- ni
  uniplate (CComplexImag e ni)    = plate CComplexImag |* e |- ni
  uniplate (CIndex b i ni)        = plate CIndex |* b |* i |- ni
  uniplate (CCall f args ni)      = plate CCall |* f ||* args |- ni
  uniplate (CMember e m deref ni) = plate CMember |* e |- m |- deref |- ni
  uniplate (CVar x ni)            = plate CVar |- x |- ni
  uniplate (CConst c)             = plate CConst |- c
  uniplate (CCompoundLit d il ni) = plate CCompoundLit |- d ||+ il |- ni
  uniplate (CStatExpr s ni)       = plate CStatExpr |+ s |- ni
  uniplate (CLabAddrExpr l ni)    = plate CLabAddrExpr |- l |- ni
  uniplate (CBuiltinExpr b)       = plate CBuiltinExpr |- b

instance Uniplate (CStatement a) where
  uniplate (CLabel l s as ni)             = plate CLabel |- l |* s |- as |- ni
  uniplate (CCase e s ni)                 = plate CCase |+ e |* s |- ni
  uniplate (CCases e1 e2 s ni)            = plate CCases |+ e1 |+ e2 |* s |- ni
  uniplate (CDefault s ni)                = plate CDefault |* s |- ni
  uniplate (CExpr me ni)                  = plate CExpr |+ me |- ni
  uniplate (CCompound ls ss ni)           = plate CCompound |- ls ||+ ss |- ni
  uniplate (CIf e thn els ni)             = plate CIf |+ e |* thn |+ els |- ni
  uniplate (CSwitch e s ni)               = plate CSwitch |+ e |* s |- ni
  uniplate (CWhile e s isdo ni)           = plate CWhile |+ e |* s |- isdo |- ni
  uniplate (CFor i c s body ni)           =
    plate CFor |+ i |+ c |+ s |* body |- ni
  uniplate (CGoto l ni)                   = plate CGoto |- l |- ni
  uniplate (CGotoPtr e ni)                = plate CGotoPtr |+ e |- ni
  uniplate (CCont ni)                     = plate CCont |- ni
  uniplate (CBreak ni)                    = plate CBreak |- ni
  uniplate (CReturn me ni)                = plate CReturn |+ me |- ni
  uniplate (CAsm asmStmt ni)              = plate CAsm |- asmStmt |- ni

-----------------------------------------------------------------------------
-- Self Biplate instances
-----------------------------------------------------------------------------

instance Biplate (CExpression a) (CExpression a) where
  biplate = plateSelf

instance Biplate (CStatement a) (CStatement a) where
  biplate = plateSelf

-----------------------------------------------------------------------------
-- Generic Biplate instances (included to avoid undecidable instances)
-----------------------------------------------------------------------------

instance Biplate (Maybe (CExpression a)) (CExpression a) where
  biplate (Just e) = plate Just |* e
  biplate Nothing  = plate Nothing

instance Biplate (Maybe (CDeclarator a)) (CExpression a) where
  biplate (Just e) = plate Just |+ e
  biplate Nothing  = plate Nothing

instance Biplate (Maybe (CInitializer a)) (CExpression a) where
  biplate (Just e) = plate Just |+ e
  biplate Nothing  = plate Nothing

instance Biplate (Maybe (CExpression a)) (CStatement a) where
  biplate (Just e) = plate Just |+ e
  biplate Nothing  = plate Nothing

instance Biplate (Maybe (CStatement a)) (CStatement a) where
  biplate (Just e) = plate Just |* e
  biplate Nothing  = plate Nothing

instance Biplate (Maybe (CStatement a)) (CExpression a) where
  biplate (Just e) = plate Just |+ e
  biplate Nothing  = plate Nothing

instance Biplate (Either (Maybe (CExpression a)) (CDeclaration a))
                 (CExpression a) where
  biplate (Left me) = plate Left |+ me
  biplate (Right d) = plate Right |+ d

instance Biplate (Either (Maybe (CExpression a)) (CDeclaration a))
                 (CStatement a) where
  biplate (Left me) = plate Left |+ me
  biplate (Right d) = plate Right |+ d

instance Biplate (Maybe (CDeclarator a),
                  Maybe (CInitializer a),
                  Maybe (CExpression a))
                 (CExpression a) where
  biplate (md, mi, me) = plate (,,) |+ md |+ mi |+ me

-----------------------------------------------------------------------------
-- General Biplate instances
-----------------------------------------------------------------------------

instance Biplate (CDeclaration a) (CExpression a) where
  biplate (CDecl specs inits ni) = plate CDecl |- specs ||+ inits |- ni

instance Biplate (CDeclaration a) (CStatement a) where
  biplate = plate

instance Biplate (CInitializer a) (CExpression a) where
  biplate (CInitExpr e ni) = plate CInitExpr |* e |- ni
  biplate (CInitList il ni) = plate CInitList ||+ il |- ni

instance Biplate (CInitializer a) (CStatement a) where
  biplate (CInitExpr e ni) = plate CInitExpr |+ e |- ni
  biplate (CInitList il ni) = plate CInitList ||+ il |- ni

-- XXX: a DerivedDeclr may contain an expression in an array size
instance Biplate (CDeclarator a) (CExpression a) where
  biplate = plate

-- XXX: a CDesignator may contain an expression
instance Biplate ([CPartDesignator a], CInitializer a) (CExpression a) where
  biplate (ds, i) = plate (,) |- ds |+ i

instance Biplate ([CPartDesignator a], CInitializer a) (CStatement a) where
  biplate (ds, i) = plate (,) |- ds |+ i

instance Biplate (CStatement a) (CExpression a) where
  biplate (CLabel l s attrs ni)          = plate CLabel |- l |+ s |- attrs |- ni
  biplate (CCase e s ni)                 = plate CCase |* e |+ s |- ni
  biplate (CCases e1 e2 s ni)            = plate CCases |* e1 |* e2 |+ s |- ni
  biplate (CDefault s ni)                = plate CDefault |+ s |- ni
  biplate (CExpr me ni)                  = plate CExpr |+ me |- ni
  biplate (CCompound ls ss ni)           = plate CCompound |- ls ||+ ss |- ni
  biplate (CIf e thn els ni)             = plate CIf |* e |+ thn |+ els |- ni
  biplate (CSwitch e s ni)               = plate CSwitch |* e |+ s |- ni
  biplate (CWhile e s isdo ni)           = plate CWhile |* e |+ s |- isdo |- ni
  biplate (CFor for_init cond step s ni) =
    plate CFor |+ for_init |+ cond |+ step |+ s |- ni
  biplate (CGoto l ni)                   = plate CGoto |- l |- ni
  biplate (CGotoPtr e ni)                = plate CGotoPtr |* e |- ni
  biplate (CCont ni)                     = plate CCont |- ni
  biplate (CBreak ni)                    = plate CBreak |- ni
  biplate (CReturn me ni)                = plate CReturn |+ me |- ni
  biplate (CAsm asmStmt ni)              = plate CAsm |- asmStmt |- ni

instance Biplate (CExpression a) (CStatement a) where
  biplate (CComma es ni)         = plate CComma ||+ es |- ni
  biplate (CAssign op l r ni)    = plate CAssign |- op |+ l |+ r |- ni
  biplate (CCond c t e ni)       = plate CCond |+ c |+ t |+ e |- ni
  biplate (CBinary op l r ni)    = plate CBinary |- op |+ l |+ r |- ni
  biplate (CCast d e ni)         = plate CCast |- d |+ e |- ni
  biplate (CUnary op e ni)       = plate CUnary |- op |+ e |- ni
  biplate (CSizeofExpr e ni)     = plate CSizeofExpr |+ e |- ni
  biplate (CSizeofType d ni)     = plate CSizeofType |- d |- ni
  biplate (CAlignofExpr e ni)    = plate CAlignofExpr |+ e |- ni
  biplate (CAlignofType d ni)    = plate CAlignofType |- d |- ni
  biplate (CComplexReal e ni)    = plate CComplexReal |+ e |- ni
  biplate (CComplexImag e ni)    = plate CComplexImag |+ e |- ni
  biplate (CIndex b i ni)        = plate CIndex |+ b |+ i |- ni
  biplate (CCall f args ni)      = plate CCall |+ f ||+ args |- ni
  biplate (CMember e m deref ni) = plate CMember |+ e |- m |- deref |- ni
  biplate (CVar x ni)            = plate CVar |- x |- ni
  biplate (CConst c)             = plate CConst |- c
  biplate (CCompoundLit d il ni) = plate CCompoundLit |- d ||+ il |- ni
  biplate (CStatExpr s ni)       = plate CStatExpr |* s |- ni
  biplate (CLabAddrExpr l ni)    = plate CLabAddrExpr |- l |- ni
  biplate (CBuiltinExpr b)       = plate CBuiltinExpr |- b

instance Biplate (CCompoundBlockItem a) (CExpression a) where
  biplate (CBlockStmt s) = plate CBlockStmt |+ s
  biplate (CBlockDecl d) = plate CBlockDecl |+ d
  biplate bi = plate bi

instance Biplate (CCompoundBlockItem a) (CStatement a) where
  biplate (CBlockStmt s) = plate CBlockStmt |* s
  -- XXX: is a statement expression possible in a decl?
  biplate (CBlockDecl d) = plate CBlockDecl |- d
  biplate bi = plate bi

