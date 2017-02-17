module Language.Ava.Flow where

data Flow a = FlowState ![a] (Maybe a) ![a]
    deriving ( Eq, Show )

-- | Constructs a new flow state
--
newFlow [] = FlowState [] Nothing []
newFlow (x:xs) = FlowState [] (Just x) xs

focus (FlowState _ f _) = f

left (FlowState l (Just v) []) = FlowState (v:l) Nothing []
left (FlowState l (Just v) (x:xs)) = FlowState (v:l) (Just x) xs
left (FlowState l Nothing []) = FlowState

