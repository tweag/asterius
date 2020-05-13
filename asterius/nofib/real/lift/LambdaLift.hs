
module LambdaLift where


import Utilities

type Name = [Char]

data Constant = CNum Integer | CBool Bool | CFun Name

type IsRec = Bool
recursive = True
nonRecursive = False

data Expr binder


type Defn binder = (binder, Expr binder)

type Expression = Expr Name

type AnnExpr binder annot = (annot, AnnExpr' binder annot)

data AnnExpr' binder annot


type AnnDefn binder annot = (binder, AnnExpr binder annot)

bindersOf       :: [(binder,rhs)] -> [binder]
bindersOf defns =  [name | (name, rhs) <- defns]

rhssOf        :: [(binder,rhs)] -> [rhs]
rhssOf defns  =  [rhs | (name,rhs) <- defns]

lambdaLift :: Expression -> [SCDefn]

type SCDefn = (Name, [Name], Expression)

freeVars :: Expression -> AnnExpr Name (Set Name)

abstract :: AnnExpr Name (Set Name) -> Expression

collectSCs :: Expression -> [SCDefn]

lambdaLift = collectSCs . abstract . freeVars

freeVars (EConst k) = (setEmpty, AConst k)
freeVars (EVar v)   = (setSingleton v, AVar v)

freeVars (EAp e1 e2) =
  (setUnion (freeVarsOf e1') (freeVarsOf e2'), AAp e1' e2')
  where
  e1' = freeVars e1
  e2' = freeVars e2

freeVars (ELam args body) =
  (setDifference (freeVarsOf body') (setFromList args), ALam args body')
  where
  body' = freeVars body

freeVars (ELet isRec defns body) =
  (setUnion defnsFree bodyFree, ALet isRec (zip binders rhss') body')
  where
  binders = bindersOf defns
  binderSet = setFromList binders
  rhss' = map freeVars (rhssOf defns)
  freeInRhss = setUnionList (map freeVarsOf rhss')
  defnsFree | isRec     = setDifference freeInRhss binderSet

  body' = freeVars body
  bodyFree = setDifference (freeVarsOf body') binderSet

freeVarsOf :: AnnExpr Name (Set Name) -> Set Name
freeVarsOf (free_vars, expr) = free_vars

collectSCs_e :: NameSupply -> Expression

collectSCs e = [("$main", [], e')] ++ bagToList scs

collectSCs_e ns (EConst k) = (ns, bagEmpty, EConst k)
collectSCs_e ns (EVar v)   = (ns, bagEmpty, EVar v)
collectSCs_e ns (EAp e1 e2) =
  (ns2, bagUnion scs1 scs2, EAp e1' e2')
  where
  (ns1, scs1, e1') = collectSCs_e ns  e1
  (ns2, scs2, e2') = collectSCs_e ns1 e2

collectSCs_e ns (ELam args body) =
  (ns2, bagInsert (name, args, body') bodySCs, EConst (CFun name))
  where
  (ns1, bodySCs, body') = collectSCs_e ns body
  (ns2, name) = newName ns1 "SC"

collectSCs_e ns (ELet isRec defns body) =
  (ns2, scs, ELet isRec defns' body')
  where
  (ns1, bodySCs, body') = collectSCs_e ns body
  ((ns2, scs), defns') = mapAccuml collectSCs_d (ns1, bodySCs) defns

  collectSCs_d (ns, scs) (name, rhs) =
    ((ns1, bagUnion scs scs'), (name, rhs'))
    where
    (ns1, scs', rhs') = collectSCs_e ns rhs

separateLams :: Expression -> Expression

type Level = Int
addLevels :: Expression -> AnnExpr (Name, Level) Level

identifyMFEs :: AnnExpr (Name, Level) Level -> Expr (Name, Level)

rename :: Expr (Name, a) -> Expr (Name, a)

float :: Expr (Name, Level) -> Expression

fullyLazyLift = lambdaLift . float . rename .

freeSetToLevel :: Set Name -> Assn Name Level -> Level
freeSetToLevel free_vars env =

addLevels = freeToLevel . freeVars

freeToLevel_e :: Level			-- Level of context

freeToLevel e = freeToLevel_e 0 [] e

freeToLevel_e level env (_, AConst k) = (0, AConst k)
freeToLevel_e level env (_, AVar v) = (assLookup env v, AVar v)
freeToLevel_e level env (_, AAp e1 e2) =
  (max (levelOf e1') (levelOf e2'), AAp e1' e2')
   where
   e1' = freeToLevel_e level env e1
   e2' = freeToLevel_e level env e2

freeToLevel_e level env (free, ALam args body) =
  (freeSetToLevel free env, ALam args' body')
  where
  body' = freeToLevel_e (level + 1) (args' ++ env) body
  args' = zip args (repeat (level+1))

freeToLevel_e level env (free, ALet isRec defns body) =
  (levelOf body', ALet isRec defns' body')
  where
  binders = bindersOf defns
  freeRhsVars = setUnionList [free | (free, _) <- rhssOf defns]
  maxRhsLevel = freeSetToLevel freeRhsVars

  defns' = map freeToLevel_d defns
  body' = freeToLevel_e level (bindersOf defns' ++ env) body

  freeToLevel_d (name, rhs) = ((name, levelOf rhs'), rhs')

  envRhs | isRec     = [(name,maxRhsLevel) | name <- binders] ++ env
         | not isRec = env

levelOf :: AnnExpr a Level -> Level
levelOf (level, e) = level

identifyMFEs_e :: Level -> AnnExpr (Name, Level) Level -> Expr (Name, Level)

identifyMFEs e = identifyMFEs_e 0 e

notMFECandidate (AConst k) = True
notMFECandidate (AVar v) = True
notMFECandidate _ = False	-- For now, everything else is a candidate

identifyMFEs_e cxt (level, e) =
  if (level == cxt || notMFECandidate e)
  then e'
  else transformMFE level e'
  where
  e' = identifyMFEs_e1 level e

transformMFE level e = ELet nonRecursive [(("v",level), e)] (EVar "v")

abstract (_, AConst k) = EConst k
abstract (_, AVar v) = EVar v
abstract (_, AAp e1 e2) = EAp (abstract e1) (abstract e2)

abstract (free, ALam args body) =
  foldl EAp sc (map EVar fvList)
  where
  fvList = setToList free
  sc = ELam (fvList ++ args) (abstract body)

abstract (_, ALet isRec defns body) =
  ELet isRec [(name, abstract body) | (name, body) <- defns] (abstract body)

separateLams (EConst k) = EConst k
separateLams (EVar v) = EVar v
separateLams (EAp e1 e2) = EAp (separateLams e1) (separateLams e2)
separateLams (ELam args body) = foldr mkLam (separateLams body) args
  				  where

separateLams (ELet isRec defns body) =
  ELet isRec [(name, separateLams rhs) | (name,rhs) <- defns]

identifyMFEs_e1 :: Level -> AnnExpr' (Name, Level) Level -> Expr (Name, Level)
identifyMFEs_e1 level (AConst k) = EConst k
identifyMFEs_e1 level (AVar v)   = EVar v
identifyMFEs_e1 level (AAp e1 e2) =
  EAp (identifyMFEs_e level e1) (identifyMFEs_e level e2)

identifyMFEs_e1 level (ALam args body) =
  ELam args (identifyMFEs_e argLevel body)
  where
  (_, argLevel) = head args

identifyMFEs_e1 level (ALet isRec defns body) =
  ELet isRec defns' body'
  where
  body' = identifyMFEs_e level body
  defns' = [ ((name,rhsLevel),identifyMFEs_e rhsLevel rhs)

rename e = e' where (_, e') = rename_e [] initialNameSupply e

rename_e :: Assn Name Name -> NameSupply -> Expr (Name,a)

rename_e env ns (EConst k) = (ns, EConst k)
rename_e env ns (EVar v) = (ns, EVar (assLookup env v))
rename_e env ns (EAp e1 e2) =
  (ns2, EAp e1' e2')
  where
  (ns1, e1') = rename_e env ns e1
  (ns2, e2') = rename_e env ns1 e2
rename_e env ns (ELam args body) =
  (ns1, ELam args' body')
  where
  (ns1, args') = mapAccuml newBinder ns args
  (ns2, body') = rename_e (assocBinders args args' ++ env) ns1 body

rename_e env ns (ELet isRec defns body) =
  (ns3, ELet isRec (zip binders' rhss') body')
  where
  (ns1, body') = rename_e env' ns body
  binders = bindersOf defns
  (ns2, binders') = mapAccuml newBinder ns1 binders
  env' = assocBinders binders binders' ++ env
  (ns3, rhss') = mapAccuml (rename_e rhsEnv) ns2 (rhssOf defns)
  rhsEnv |     isRec = env'

newBinder ns (name, info) =
  (ns1, (name', info)) where (ns1, name') = newName ns name

assocBinders :: [(Name,a)] -> [(Name,a)] -> Assn Name Name
assocBinders binders binders' = zip (map fst binders) (map fst binders')

float_e :: Expr (Name, Level) -> (FloatedDefns, Expression)

float e = install floatedDefns e'  where  (floatedDefns, e') = float_e e

type FloatedDefns = [(Level, IsRec, [Defn Name])]

install :: FloatedDefns -> Expression -> Expression
install defnGroups e =
  foldr installGroup e defnGroups
  where
  installGroup (level, isRec, defns) e = ELet isRec defns e

float_e (EConst k) = ([], EConst k)
float_e (EVar v) = ([], EVar v)
float_e (EAp e1 e2) = (fd1 ++ fd2, EAp e1' e2')

float_e (ELam args body) =
  (outerLevelDefns, ELam args' (install thisLevelDefns body'))
  where
  args' = [arg | (arg,level) <- args]
  (_,thisLevel) = head args		-- Extract level of abstraction
  (floatedDefns, body') = float_e body
  thisLevelDefns  = filter groupIsThisLevel       floatedDefns
  outerLevelDefns = filter (not.groupIsThisLevel) floatedDefns
  groupIsThisLevel (level,isRec,defns) = level >= thisLevel

float_e (ELet isRec defns body) =
  (rhsFloatDefns ++ [thisGroup] ++ bodyFloatDefns, body')
  where
  (bodyFloatDefns, body') = float_e body
  (rhsFloatDefns, defns') = mapAccuml float_defn [] defns
  thisGroup = (thisLevel, isRec, defns')
  (_,thisLevel) = head (bindersOf defns)

float_defn floatedDefns ((name,level), rhs) =
  (rhsFloatDefns ++ floatedDefns, (name, rhs'))
  where
  (rhsFloatDefns, rhs') = float_e rhs
