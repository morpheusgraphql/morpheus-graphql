module Boo (main) where

data Ev = Ev
--data Va = Va
--data Vb = Vb

newtype M m a = M { unM :: m (Ev -> m a) }

instance Functor (M m )
instance Applicative (M m )
instance (Monad m) => Monad (M m) where
  -- M e a            ->  a -> M e b                ->  M e b
  --  M { e -> m a }  ->  ( a -> M { e -> M b } )   ->  M { e -> M b }
  (M m_e_to_ma) >>= a_to_m1_Meb = M $ wow <$> m_e_to_ma
         where
           wow e_to_ma e = do
             a <- e_to_ma e
             unM (a_to_m1_Meb a) >>=  (\x -> x e)

nextMonad :: Int -> M IO Int
nextMonad x = M { unM = pure $ \e -> pure (x + 10) }

checkLoop = (M { unM = pure $  \x -> pure 1 } :: M IO Int) >>= nextMonad



main = unM (checkLoop) >>= \x -> (x Ev) >>= print