{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE InstanceSigs              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Flatten (
  FlatDiagram
 ,FlatCon
 ,flatten
) where
import Datatype (UMLStateDiagram
                ,StateDiagram'(..)
                ,StateDiagram
                ,globalise
                --,localise
                ,Connection'(..)
                ,Connection
                )
import Data.List(groupBy
                ,sortBy
                , sort)
import Data.Bifunctor(bimap
                     ,Bifunctor(second, first))

type FlatDiagram = StateDiagram' Int [FlatCon]

type FlatCon = Connection' [[Int]] [[Int]]

type Flattening = ([FlatDiagram],[Relabeled])

type Relabeled = ([[Int]],FlatDiagram)

class Flattable a b where
  inFlatForm :: a -> b
  inCompositeForm :: b -> a

instance Flattable UMLStateDiagram FlatDiagram where
  inFlatForm :: UMLStateDiagram -> FlatDiagram
  inFlatForm diagram
    = case diagram of
        (StateDiagram {substate, label, name, connection, startState})
          -> StateDiagram { substate = map inFlatForm substate
                         , label = label
                         , name = name
                         , connection = map inFlatForm connection
                         , startState = startState }
        (InnerMostState {label, name, operations})
          -> InnerMostState { label = label
                            , name = name
                            , operations = operations }
        (CombineDiagram {substate, label})
          -> CombineDiagram { substate = map inFlatForm substate
                            , label = label }
        (EndState {label})
          -> EndState { label = label }
        (Joint {label})
          -> Joint { label = label }
        (History {label, historyType})
          -> History { label = label
                     , historyType = historyType }
  inCompositeForm :: FlatDiagram -> UMLStateDiagram
  inCompositeForm diagram
    = case diagram of
        (StateDiagram {substate, label, name, connection, startState})
          -> StateDiagram { substate = map inCompositeForm substate
                         , label = label
                         , name = name
                         , connection = map inCompositeForm connection
                         , startState = startState }
        (InnerMostState {label, name, operations})
          -> InnerMostState { label = label
                            , name = name
                            , operations = operations }
        (CombineDiagram {substate, label})
          -> CombineDiagram { substate = map inCompositeForm substate
                            , label = label }
        (EndState {label})
          -> EndState { label = label }
        (Joint {label})
          -> Joint { label = label }
        (History {label, historyType})
          -> History { label = label
                     , historyType = historyType }

instance Flattable Connection FlatCon where
  inFlatForm :: Connection -> FlatCon
  inFlatForm (Connection {pointFrom, pointTo, transition })
    = Connection { pointFrom = [pointFrom]
                 , pointTo = [pointTo]
                 , transition = transition}
  inCompositeForm :: FlatCon -> Connection
  inCompositeForm (Connection {pointFrom, pointTo, transition })
    = Connection { pointFrom = head pointFrom
                 , pointTo = head pointTo
                 , transition = transition}

{- end FlattenUtil.hs -}

newtype State a b = State (a -> (a, b))

runState :: State a b -> a -> (a, b)
runState (State f) = f

execState :: State a b -> a -> b
execState (State f) = snd . f

instance Functor (State a) where
    fmap f state = State $ \s -> let (ns, res) = runState state s in (ns, f res)

instance Applicative (State a) where
    sf <*> sarg = State $ \s -> let
        (ns, f) = runState sf s
        (nns, arg) = runState sarg ns
        in (nns, f arg)
    pure x = State $ \s -> (s, x)

instance Monad (State a) where
    (State sfunc) >>= f
      = State $ \state -> let (state', x) = sfunc state in runState (f x) state'
    return = pure

get :: State a a
get = State $ \state -> (state, state)

put :: a -> State a ()
put newstate = State $ const (newstate, ())

toRelabel :: [FlatDiagram] -> [Relabeled]
toRelabel
  = map ([],)

toPlain' :: [Relabeled] -> [FlatDiagram]
toPlain'
  = map snd

inheritName' :: [FlatDiagram] -> FlatDiagram -> [FlatDiagram]
inheritName' substates superState
  = map (\case
          i@(InnerMostState{name = innerName})
            -> case superState of
                  (StateDiagram {name = outerName})
                    -> i {name = outerName ++ ", " ++ innerName }
                  _ -> i
          _ -> error "cannot inherit super state name to other state types than inner most"
    ) substates

relabel :: [Relabeled] -> [Int] -> [Relabeled]
relabel [] _ = []
relabel (x:xs) (y:ys)
  = case x of
      (_,s@(InnerMostState{label})) -> ([[label]],s{label=y}) : relabel xs ys
      _ -> error "only InnerMostState can be relabeled"
relabel _ [] = error "run out of labels"

getLabel :: StateDiagram a -> Int
getLabel = \case
              (StateDiagram {label}) -> label
              (CombineDiagram {label}) -> label
              (InnerMostState {label}) -> label
              (Joint {label}) -> label
              (History {label}) -> label
              (EndState {label}) -> label

obtainFreeLabels :: FlatDiagram -> Int -> [Int]
obtainFreeLabels = obtainFreeLabels' 1

obtainFreeLabels' :: Int -> FlatDiagram -> Int -> [Int]
obtainFreeLabels' x sd@(StateDiagram{substate}) n
  = if isUsedIn substate x
    then obtainFreeLabels' (x+1) sd n
    else x : obtainFreeLabels' (x+1) sd (n-1)
obtainFreeLabels' _ _ 0 = []
obtainFreeLabels' _ _ _ = error "only StateDiagram are allowed to be queried"

isUsedIn :: [FlatDiagram] -> Int -> Bool
isUsedIn sublist l
  = not $ foldr ((&&) . (\x -> getLabel x /= l )) True sublist

isFlat :: FlatDiagram -> Bool
isFlat
  = \case
      (StateDiagram {substate})
        -> foldr ((&&) .
           (\case
               (InnerMostState {}) -> True
               _ -> False))
           True substate
      _ -> error "only applicable to StateDiagram"

hasFlatSDs :: FlatDiagram -> Bool
hasFlatSDs
  = \case
      (CombineDiagram {substate})
        -> (foldr ((&&) . (\case
                    s@(StateDiagram{}) -> isFlat s
                    _ -> False)) True substate)
      _ -> error "only applicable to CombineDiagram"

getFreeLabelsIn :: FlatDiagram -> Int -> State a [Int]
getFreeLabelsIn x y = return (obtainFreeLabels x y)

relabelSubstates :: [FlatDiagram] -> [Int] -> State a [Relabeled]
relabelSubstates x y = return (relabel (toRelabel x) y)

stripOldLabels :: [Relabeled] -> State a [FlatDiagram]
stripOldLabels x = return (toPlain' x)

toRelabelMemoizingParent :: [FlatDiagram] -> FlatDiagram -> [Relabeled]
toRelabelMemoizingParent [] _ = []
toRelabelMemoizingParent (i@(InnerMostState{label=cLabel}):xs) s@(StateDiagram{label=pLabel})
  = ([[pLabel,cLabel]],i):toRelabelMemoizingParent xs s
toRelabelMemoizingParent _ _ = error "invalid content within CD <>-- SD [...]"


crossSDs :: State [FlatDiagram] [Relabeled]
crossSDs
  = do
    trav <- get
    case trav of
      ((CombineDiagram{substate}):_)
        -> return (
           crossProduct
           (map (\case
                  s@(StateDiagram{substate=sdSubs})
                    -> (toRelabelMemoizingParent sdSubs s)
                  _ -> error "invalid content in SD")
            substate))
      _ -> error "invalid op"

crossStates :: Relabeled -> Relabeled -> Relabeled
crossStates (leftLabel,InnerMostState { name = leftName
                                       , operations = leftOperations })
            (rightLabel,InnerMostState { name = rightName
                                        , operations = rightOperations })
  = (leftLabel ++ rightLabel,
     InnerMostState { label = 0
                    , name = leftName ++ ", " ++ rightName
                    , operations = leftOperations ++ "\n" ++ rightOperations })
crossStates _ _
  = error "only states that decayed into an InnerMostState can be crossed"

crossProduct' :: [Relabeled] -> [Relabeled] -> [Relabeled]
crossProduct' xs ys
  = [ crossStates x y | x <- xs, y <-ys ]

crossProduct :: [[Relabeled]] -> [Relabeled]
crossProduct [] = []
crossProduct [x] = x
crossProduct (x:y:xs) = crossProduct (crossProduct' x y : xs)

relabelSDsSubs :: [Relabeled] -> [Int] -> State [FlatDiagram] [Relabeled]
relabelSDsSubs x y = return ( zipWith (curry (\case
                ((old,i@InnerMostState{}),new)
                   -> (old,i{label=new})
                _ -> error "invalid content")) x y)

inheritName :: [FlatDiagram] -> FlatDiagram -> State a [FlatDiagram]
inheritName x y = return (inheritName' x y)

rewireLocalSDConnections :: [FlatCon] -> [Relabeled] -> State a [FlatCon]
rewireLocalSDConnections x y = return [ c{ pointFrom
                                             = replaceIfMatches pointFrom (labelUpdatesOf y)
                                         , pointTo
                                             = replaceIfMatches pointTo (labelUpdatesOf y) }
                                      |c@(Connection{pointFrom,pointTo})<-x]


labelUpdatesOf :: [Relabeled] -> [([[Int]],Int)]
labelUpdatesOf
  = map labelUpdateOf

labelUpdateOf :: Relabeled -> ([[Int]],Int)
labelUpdateOf
  = \case
       (x,InnerMostState{label}) -> (x,label)
       _ -> error "only InnerMostState can be relabeled"

replaceIfMatches :: [[Int]] -> [([[Int]],Int)] -> [[Int]]
replaceIfMatches x [] = x
replaceIfMatches x ((o,n):ys)
  = if sort x == sort o
    then [[n]]
    else replaceIfMatches x ys

labelPrefix :: [FlatDiagram] -> [Int]
labelPrefix
  = foldr (\case
             (StateDiagram {label}) -> (label:)
             (CombineDiagram {label}) -> (label:)
             _ -> error "unsupported container type")
    []

replaceMatching :: [[Int]] -> [([[Int]],[Int])] -> [[Int]]
replaceMatching [] _ = []
replaceMatching (x:xs) ys = if null possibleTarget then x : replaceMatching xs ys
                            else possibleTarget ++ replaceMatching xs ys
                            where
                            possibleTarget = [ y'| (y,y') <- ys, head y == x ]

{- TODO: Bezeichner aufräumen für Parent und Child, Anordnung stimmt vom Namen her nicht immer -}
globalUpdateFromSD :: FlatDiagram -> [FlatDiagram] -> [FlatDiagram] -> [Relabeled] -> State [FlatDiagram] ()
globalUpdateFromSD w@(StateDiagram{label = pLabel, connection = pCons, startState = pStart}) [] ys zs
  = put (w{connection=updCons}:reverse ys)
    where
    updateSet
      = [(map ((labelPrefix ys ++ [pLabel]) ++) rs,labelPrefix ys ++ [label])|(rs,InnerMostState{label})<-zs]
        ++
        [([labelPrefix ys ++ [pLabel]],labelPrefix ys ++ initialStateOfSD)]
    updCons
      = [ c { pointFrom = replaceMatching pointFrom updateSet
            , pointTo = replaceMatching pointTo updateSet }
        | c@(Connection{pointFrom,pointTo})<-pCons ]
    initialStateOfSD = [label|i@(rs,InnerMostState{label})<-zs,pStart == head rs]
globalUpdateFromSD w@(StateDiagram{label = pLabel, startState = pStart}) (x@(StateDiagram{connection = pCons, startState = cStart}):xs) ys zs
  = globalUpdateFromSD w xs (x{connection = updCns, startState = updCStart}:ys) zs
    where
    updateSet
      = [(map ((labelPrefix ys ++ [pLabel]) ++) rs, labelPrefix ys ++ [label])|(rs,InnerMostState{label})<-zs]
        ++
        [([labelPrefix ys ++ [pLabel]],labelPrefix ys ++ initialStateOfSD)]
    updCns
      = [ c { pointFrom = replaceMatching pointFrom updateSet
            , pointTo = replaceMatching pointTo updateSet }
        | c@(Connection{pointFrom,pointTo}) <- pCons ]
    initialStateOfSD = [label|i@(rs,InnerMostState{label})<-zs,pStart == head rs]
    updCStart = head $ replaceMatching [cStart] updateSet
globalUpdateFromSD w@(StateDiagram{label=pLabel}) (x@(CombineDiagram{label=cLabel}):xs) ys zs
  = globalUpdateFromSD w xs (x:ys) zs
globalUpdateFromSD _ _ _ _ = error "not defined"

pushToParent :: [FlatDiagram] -> [FlatCon] -> State [FlatDiagram] ()
pushToParent x y = do
            trav <- get
            case trav of
              ((StateDiagram{}):p@(StateDiagram{substate = pSubs, connection = pCons}):xs)
                -> put ((p{substate = pSubs ++ x, connection = pCons ++ y}):xs)
              ((CombineDiagram{}):p@(StateDiagram{substate = pSubs, connection = pCons}):xs)
                -> put ((p{substate = pSubs ++ x, connection = pCons ++ y}):xs)
              _ -> error "nicht definiert"
-- globalUpdateFromCD :: ([Int],[Int]) ->

updateGlobalConnections :: [Relabeled] -> State [FlatDiagram] ()
updateGlobalConnections y = do
          trav <- get
          case trav of
            (x@(StateDiagram{}):xs) -> globalUpdateFromSD x xs [] y
            _ -> error "not possible"
          return ()

-- we need to know the sd initial states
-- (old region, old initial) TODO: simplify return only [[Int]]
initialStatesOfSDs :: State [FlatDiagram] [[Int]]
initialStatesOfSDs
  = do
    trav <- get
    case trav of
      ((CombineDiagram{substate}):_)
        -> return ( map (\case
                            (StateDiagram{label = pLabel, startState = pStart})
                              -> (pLabel : pStart)
                         ) substate
             )
      _ -> error "invalid container type"

oldLabelsToNewLabels :: [Relabeled] -> [([[Int]],Int)]
oldLabelsToNewLabels
  = map (\(old,InnerMostState{label}) -> (old,label))


isSDInitial :: [Int] -> [Int] -> [[Int]] -> Bool
isSDInitial x z ys
  = x `elem` map (z ++) ys

{- maybe callwrap and strip duplicates  -}
replaceMulti :: [[Int]] -> FlatDiagram -> [FlatDiagram] -> [Relabeled] -> [[Int]] -> [[Int]]
replaceMulti [] w ys rs is = []
replaceMulti (x:xs) w@(CombineDiagram{label=wLabel}) ys rs is
  = if not (null finalTarget)
    then
    case snd (head finalTarget) of
      InnerMostState {label}
        -> [label] : replaceMulti xs w ys rs is
      _ -> error "invalid constructor"
    else x : replaceMulti xs w ys rs is
    where
    possibleTarget = [(map ((labelPrefix ys ++ [wLabel]) ++) oldLabels, ims)| (oldLabels,ims) <- rs]
    possibleTargets' = filter (\(crossTargets,_) -> x `elem` crossTargets) possibleTarget
    withoutX = map (Data.Bifunctor.first (filter (/= x)) ) possibleTargets'

    finalTarget = filter (\(k,l) -> all (\k' -> isSDInitial k' (labelPrefix ys ++ [wLabel]) is) k) withoutX

globalUpdateFromCD :: FlatDiagram -> [FlatDiagram] -> [FlatDiagram] -> [[Int]] -> [Relabeled] -> State [FlatDiagram] ()
globalUpdateFromCD w@(CombineDiagram{label=cLabel}) [] ys is rs
  = do
    put (w:reverse ys)
    return ()
globalUpdateFromCD w@(CombineDiagram{label=wLabel}) (x@(StateDiagram{label=pLabel,startState=pStart,connection=pCons}):xs) ys is rs
  = globalUpdateFromCD w xs (x{connection=updCons,startState= head (replaceMulti [pStart] w ys rs is)}:ys) is rs
    where
    updCons
      = [ c { pointFrom = replaceMulti oldFrom w ys rs is
            , pointTo = replaceMulti oldTo  w ys rs is
            }| c@(Connection{pointFrom=oldFrom,pointTo=oldTo}) <- pCons]
globalUpdateFromCD w@(CombineDiagram{label=cLabel}) (x@(CombineDiagram{label = pLabel}):xs) ys is rs
  = globalUpdateFromCD w xs (x:ys) is rs {- passthrough -}

stomp :: State [FlatDiagram] ()
stomp
  = do
    node <- get
    case node of
      [x@(StateDiagram{substate=sub:substates})]
        -> if isFlat x
           then put [x]
           else do
                put [sub,x{substate=substates}]
                stomp
      (c@(StateDiagram{substate=sub:substates,connection,startState}):p@(StateDiagram{substate=psubs,connection=pcons}):xs)
        -> if isFlat c
           then do {- SD into SD -}
                   newLabels <- getFreeLabelsIn p (length (sub:substates))
                   relabeledSubs <- relabelSubstates (sub:substates) newLabels
                   rewiredLocalCons <- rewireLocalSDConnections connection relabeledSubs
                   updateGlobalConnections relabeledSubs
                   rSubsInPlainForm <- stripOldLabels relabeledSubs
                   rSubsWithPName <- inheritName rSubsInPlainForm p
                   pushToParent rSubsWithPName rewiredLocalCons
                   stomp
           else do
                put [sub,c{substate=substates},p]
                stomp
      (c@(CombineDiagram{substate=flats@(sub:substates)}):p@(StateDiagram{substate=psubs}):xs)
        -> if hasFlatSDs c
           then {- CD into SD -}
                do
                {- cross regions, relabel, rewire, updateGlobal -}
                crsSdSubs <- crossSDs
                newLabels <- getFreeLabelsIn p (length crsSdSubs)
                relabeledSDSubs <- relabelSDsSubs crsSdSubs newLabels
                adjCons <- getCDConnections
                rewiredLocalCons <- restoreConnections relabeledSDSubs adjCons
                rSubsInPlainForm <- stripOldLabels relabeledSDSubs
                oldInitials <- initialStatesOfSDs
                globalUpdateFromCD c (p:xs) [] oldInitials relabeledSDSubs
                pushToParent rSubsInPlainForm rewiredLocalCons
                stomp
           else do {- if the SDs ain't flat, then stomp on their children -}
                put (sub:c{substate=substates}:p:xs)
                stomp
      (c@(StateDiagram{substate=sub:substates}):p@(CombineDiagram{substate=psubs}):xs)
        -> if isFlat c
           then {- SD into CD -}
                do {- a flat SD is perfect for CD, just hand it back -}
                put (p{substate=psubs++[c]}:xs)
                stomp
           else do {- if the SD isn't flat, then make its contents flat -}
                put (sub:c{substate=substates}:p:xs)
                stomp
      (i@(InnerMostState{}):p@(StateDiagram{substate=psubs}):xs)
        -> do
           {- InnerMost is good already -}
           put (p{substate=psubs++[i]}:xs)
           stomp
      ((Joint{label=cLabel}):xs)
        -> do
           -- error "lets crash"
           convertConnection
           removeNode
           stomp
      _ -> error "unknown constructor traversed"
    return () -- we just change the state, which is our pre-order traversal stack

stomp' :: State [FlatDiagram] [FlatDiagram]
stomp' = do stomp
            get


getCDConnections :: State [FlatDiagram] [FlatCon]
getCDConnections
  = do
    trav <- get
    return ( case trav of
              ((CombineDiagram{substate}):xs)
                -> (concatMap (\case
                                 (StateDiagram{ connection
                                              , label = pLabel })
                                   -> (map (\case
                                              i@(Connection{ pointFrom = oldFrom
                                                           , pointTo = oldTo })
                                                -> (i{ pointFrom = map (pLabel :) (oldFrom::[[Int]])
                                                     , pointTo = map (pLabel :) (oldTo::[[Int]]) })
                                           ) connection )
                                   )
                   substate::[FlatCon])
              _ -> error ""
            )

removeNode :: State [FlatDiagram] ()
removeNode
  = do
    trav <- get
    case trav of
      (x:xs)
        -> do
           put (xs)
           return ()
      _ -> error "you likely dont want to do this and want to keep the head"
    return ()

extractTransitionName :: [FlatCon] -> String
extractTransitionName [] = "should never happen"
extractTransitionName ((Connection {transition}):_) = transition

asSourceToTarget :: [FlatCon] -> [([[Int]],[[Int]])]
asSourceToTarget [] = []
asSourceToTarget ((Connection {pointTo,pointFrom}):xs) = (pointFrom,pointTo):asSourceToTarget xs

asSingleSourceToTarget :: [FlatCon] -> [([[Int]],[Int])]
asSingleSourceToTarget [] = []
asSingleSourceToTarget ((Connection {pointTo,pointFrom}):xs) = (pointFrom,head pointTo):asSingleSourceToTarget xs

getConnections' :: FlatDiagram -> [FlatCon]
getConnections' (StateDiagram {connection}) = connection
getConnections' _ = []

transitionName :: FlatCon -> FlatCon -> Ordering
transitionName (Connection{transition}) (Connection{transition=transition'})
    | transition >= transition' = GT
    | otherwise = LT

transitionName' :: FlatCon -> FlatCon -> Bool
transitionName' (Connection{transition}) (Connection{transition=transition'})
    = transition == transition'

groupSameConnections :: [FlatCon] -> [[FlatCon]]
groupSameConnections = groupBy transitionName' . sortBy transitionName

restoreConnections :: [Relabeled] -> [FlatCon] -> State [FlatDiagram] [FlatCon]
restoreConnections flat connections
    = return (
  [ Connection { pointFrom = [[newSource]]
               , pointTo = [[newTarget]]
               , transition = extractTransitionName transitionGroup }
  | (srcLabels, InnerMostState {label = newSource}) <- flat
  , transitionGroup <- groupSameConnections connections
  , (_,InnerMostState {label = newTarget}) <- [tgt | tgt@(tgtLabels,_) <- flat
  , sort tgtLabels == sort (replaceMatching srcLabels (asSingleSourceToTarget transitionGroup))
  , replaceMatching srcLabels (asSingleSourceToTarget transitionGroup) /= srcLabels ] ])

flatten :: UMLStateDiagram -> UMLStateDiagram
flatten x =  inCompositeForm (head (execState stomp' [inFlatForm x]))::UMLStateDiagram

buildCon :: ([FlatCon],[FlatCon]) -> FlatCon
buildCon
  = (\(x,y@(Connection{ transition = t }):ys)
       -> let
          source
            = foldr (\case
                       (Connection {pointFrom})
                         -> (pointFrom ++)
                    )
              [] x
          target
            = foldr (\case
                       (Connection {pointTo})
                         -> (pointTo ++)
                    )
              [] (y:ys)
          in
          (Connection { pointFrom = source
                      , pointTo = target
                      , transition = t })
    )


convertConnection :: State [FlatDiagram] ()
convertConnection
  = do
    trav <- get
    case trav of
      (j@(Joint{label}):xs)
        -> do
           rs <- extractTargeting j xs [] ([],[])
           case rs of
             ([],[])
               -> do
                  return ()
             _ -> do
                  trav' <- get
                  case (reverse trav') of
                    (x@(StateDiagram{connection = pCons}):xs')
                      -> do
                         put (j:(reverse ((x{connection = (buildCon rs) : pCons}) : xs')))
                         return ()
                    _ -> error "SD must be diagram root"
      _ -> error "invalid input"
    return ()

extractTargeting :: FlatDiagram -> [FlatDiagram] -> [FlatDiagram] -> ([FlatCon],[FlatCon]) -> State [FlatDiagram] ([FlatCon],[FlatCon])
extractTargeting w@(Joint{label}) [] ys zs
  = do
    put (w : (reverse ys))
    return zs
extractTargeting w@(Joint{label=jLabel}) (x@(StateDiagram{label=pLabel}):xs) ys zs
  = do
    extractTargeting w xs ((stripOut $ stripInc x):ys) ((\(k,l) -> ( (collectInc x) ++ k, (collectOut x) ++ l ) ) (pullUp zs))
  where
  jointLabel = (jLabel : (labelPrefix ys))
  stripInc {- vielleicht als Applikativ umsetzen -}
    = (\case
         n@(StateDiagram{connection = pCons})
           -> n{connection
                 = filter (\case
                             Connection{ pointTo }
                               -> (not (((labelPrefix ys) ++ jointLabel) `elem` pointTo)) )
                   pCons}
         _ -> error "node must contain connections"
      )
  stripOut
    = (\case
         n@(StateDiagram{connection = pCons})
           -> n{connection
                 = filter (\case
                             Connection{pointFrom}
                               -> (not (((labelPrefix ys) ++ jointLabel) `elem` pointFrom)) )
                   pCons}
         _ -> error "node must contain connections"
      )
  (collectInc::(FlatDiagram->[FlatCon]))
    = (\case
         n@(StateDiagram{connection})
           -> (filter (\case
                         Connection{pointTo}
                           -> (((labelPrefix ys) ++ jointLabel) `elem` pointTo)
                      )
              connection)
         _ -> error "node must contain connections"
      )
  (collectOut::(FlatDiagram->[FlatCon]))
    = (\case
         n@(StateDiagram{connection})
           -> (filter (\case
                         (Connection{pointFrom})
                           -> (((labelPrefix ys) ++ jointLabel) `elem` pointFrom)
                      )
              connection)
         _ -> error "node must contain connections"
      )
  (addLabel::(FlatCon->FlatCon))
    = \case
        c@(Connection{ pointFrom = pF
                     , pointTo = pT } )
          -> (c { pointFrom = (map (pLabel :) pF)
                , pointTo = (map (pLabel :) pT) } )
  (pullUp::(([FlatCon],[FlatCon])->([FlatCon],[FlatCon])))
    = (\(k,l)
          -> (map addLabel k, map addLabel l)
      )
extractTargeting w@(Joint{label}) (x@(CombineDiagram{label=pLabel}):xs) ys zs
  = do
    extractTargeting w xs (x:ys) (pullUp zs)
    where
    (addLabel::(FlatCon->FlatCon))
      = \case
          c@(Connection{ pointFrom = pF
                       , pointTo = pT } )
            -> (c { pointFrom = (map (pLabel :) pF)
                  , pointTo = (map (pLabel :) pT) } )
    (pullUp::(([FlatCon],[FlatCon])->([FlatCon],[FlatCon])))
      = (\(k,l)
            -> (map addLabel k, map addLabel l)
        )
