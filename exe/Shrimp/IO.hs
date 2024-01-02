module Shrimp.IO where

import qualified Shrimp.IO.Input as Input
import qualified Shrimp.IO.Output as Output

data Queue = Queue
    { inputRequests :: [Input.Request]
    , outputRequests :: [Output.Request]
    }

enqueueInput :: Input.Request -> Queue -> Queue
enqueueInput = undefined

dequeueInput :: Queue -> (Queue, Maybe Input.Request)
dequeueInput = undefined

enqueueOutput :: Output.Request -> Queue -> Queue
enqueueOutput = undefined

dequeueOutput :: Queue -> (Queue, Maybe Output.Request)
dequeueOutput = undefined

forOutput' :: (Monad m) => [Output.Request] -> (Output.Request -> m ()) -> m ()
forOutput' [] _ = return ()
forOutput' (y : ys) handler = do
    handler y
    forOutput' ys handler

forOutput :: (Monad m) => Queue -> (Output.Request -> m ()) -> m ()
forOutput queue handler = forOutput' (outputRequests queue) handler
