
import CLaSH.Prelude

data OP = Add | Sub

topEntity' :: OP -> Signed 9 -> Signed 9 -> Signed 9
topEntity' Add a1 a2 = a1 + a2
topEntity' Sub a1 a2 = a1 - a2


topEntity :: Signal OP -> Signal (Signed 9) -> Signal (Signed 9) -> Signal (Signed 9)
--topEntity op a1 a2 = topEntity' <$> op <*> a1 <*> a2
topEntity = liftA3 topEntity'
