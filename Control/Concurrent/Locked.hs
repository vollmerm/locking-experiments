{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Concurrent.Locked where

import Data.Proxy
import GHC.TypeLits
import Control.Concurrent.MVar
import Data.Constraint
import Data.Proxy
import Data.IORef

data ProxyLock (l :: Symbol) = PL

data LockState = LS [Symbol] [Symbol]

type family Contains (l :: Symbol) (ls :: [Symbol]) :: Bool where
  Contains l (l ': ls) = 'True
  Contains l (z ': ls) = Contains l ls
  Contains l '[] = 'False

type family Absent (l :: Symbol) (ls :: [Symbol]) :: Bool where
  Absent l (l ': ls) = 'False
  Absent l (z ': ls) = Absent l ls
  Absent l '[] = 'True

type family ListAllocated (s :: LockState) :: [Symbol] where
  ListAllocated ('LS ls' (l ': ls)) =
    l ': (ListAllocated ('LS ls' ls))
  ListAllocated ('LS ls' '[]) = '[]

type family ListHeld (s :: LockState) :: [Symbol] where
  ListHeld ('LS (l ': ls) ls') =
    l ': (ListAllocated ('LS ls ls'))
  ListHeld ('LS '[] ls') = '[]  

type family Allocate (s :: LockState) (l :: Symbol) :: LockState where
  Allocate ('LS ls' ls) l = 'LS ls' (l ': ls)

type family Acquire (s :: LockState) (l :: Symbol) :: LockState where
  Acquire ('LS ls ls') l = 'LS (l ': ls) ls'

type family NotPresent (s :: LockState) (l :: Symbol) :: Constraint where
  NotPresent s l = Absent l (ListAllocated s) ~ 'True

type family NotHeld (s :: LockState) (l :: Symbol) :: Constraint where
  NotHeld s l = Absent l (ListHeld s) ~ 'True

type family IsPresent (s :: LockState) (l :: Symbol) :: Constraint where
  IsPresent s l = Contains l (ListAllocated s) ~ 'True

data IxLocked :: LockState -> LockState -> * -> * where
  
  LReturn :: a -> IxLocked i i a
  
  LNewLock :: NotPresent s l
           => ProxyLock l
           -> IxLocked (Allocate s l) k a
           -> IxLocked s k a

  LGetLock :: (IsPresent s l, NotHeld s l)
           => ProxyLock l
           -> IxLocked (Acquire s l) k a
           -> IxLocked s k a

class IxMonad (m :: LockState -> LockState -> * -> *) where
  ibind :: m i j a -> (a -> m j k b)-> m i k b
  ireturn :: a -> m i i a

instance IxMonad IxLocked where
  ireturn = LReturn
  ibind (LReturn a) k = k a
  ibind (LNewLock lock j) k = LNewLock lock (ibind j k)
  ibind (LGetLock lock j) k = LGetLock lock (ibind j k)

