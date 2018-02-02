module Parser.Test where

import Control.Monad.Free

data Directive
  = Open
  | Transact
  | Close

data CommandF t a =
  Execute Directive
          t
          a

instance Functor (CommandF t) where
  fmap f (Execute d t a) = Execute d t (f a)

type Command t = Free (CommandF t)

execute' :: Directive -> t -> Command t ()
execute' d t = liftF $ Execute d t ()

run :: (Show t) => Command t r -> IO r
run (Pure r) = return r
run (Free (Execute Open t a)) = putStrLn ("Open" `mappend` show t) >> run a
run (Free (Execute Close t a)) = putStrLn ("Close" `mappend` show t) >> run a
run (Free (Execute Transact t a)) =
  putStrLn ("Transact" `mappend` show t) >> run a
