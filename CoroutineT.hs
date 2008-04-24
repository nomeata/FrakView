{-
Copyright © 2008 Joachim Breitner

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECT
-}

module CoroutineT
	( CoroutineT
	, runCoroutineT
	, pause )
where 

import Control.Monad.Trans
import Control.Monad

data CoroutineT m a = CoroutineT {unCoroutineT :: (m (Either (CoroutineT m a) a)) }

instance (Monad m) => Monad (CoroutineT m) where
    return v = CoroutineT (return (Right v))
    a >>= b = CoroutineT $ do
    	r <- unCoroutineT a
	case r of
	   Left  paused   -> return $ Left (paused >>= b)
	   Right unpaused -> unCoroutineT (b unpaused)

instance (Monad m) => Functor (CoroutineT m) where
    fmap = liftM

instance MonadTrans (CoroutineT) where
    lift m = CoroutineT (Right `liftM` m)

instance (MonadIO m) => MonadIO (CoroutineT m) where
    liftIO = lift . liftIO

runCoroutineT :: Monad m => CoroutineT m () -> m (Maybe (CoroutineT m ()))
runCoroutineT a = either (Just) (const Nothing) `liftM` unCoroutineT a

pause :: Monad m => CoroutineT m ()
pause = CoroutineT (return (Left (CoroutineT (return (Right ())))))

example n = keepGoingFor n $ do
 		liftIO $ putStrLn "This is the coroutine"
		forM_ [1..10] $ \i -> do
			liftIO $ putStrLn $ "Counting to "++ show i ++" while you keep calling it"
			pause
	
  where --keepGoing :: Monad m => CoroutineT m () -> m ()
  	keepGoingFor 0 _   = putStrLn "Here I just abort the run"
  	keepGoingFor n cor = do
		resume <- runCoroutineT cor
		case resume of
			Just runAgain -> keepGoingFor (n-1) runAgain
			Nothing       -> putStrLn "Finally stopped"

{- output of the example:

*CouroutineT> example 5
This is the coroutine
Counting to 1 while you keep calling it
Counting to 2 while you keep calling it
Counting to 3 while you keep calling it
Counting to 4 while you keep calling it
Counting to 5 while you keep calling it
Here I just abort the run
*CouroutineT> example 14
This is the coroutine
Counting to 1 while you keep calling it
Counting to 2 while you keep calling it
Counting to 3 while you keep calling it
Counting to 4 while you keep calling it
Counting to 5 while you keep calling it
Counting to 6 while you keep calling it
Counting to 7 while you keep calling it
Counting to 8 while you keep calling it
Counting to 9 while you keep calling it
Counting to 10 while you keep calling it
Finally stopped


-}
