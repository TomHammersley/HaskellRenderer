import Control.Monad.State
import Data.Maybe

stateTest :: Bool -> State (Maybe Int) Bool
stateTest consume = do return False
                       x <- get
                       case x of 
                         Just value -> if consume
                                       then do put Nothing
                                               return True
                                       else do return False
                         _ -> do return False

