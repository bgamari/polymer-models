{-# LANGUAGE TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module WormLikeChain where

import Control.Applicative
import Control.Monad

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import Data.Vector.Unboxed.Deriving

import Data.VectorSpace
import Data.Cross
import Data.AffineSpace
import Data.AffineSpace.Point
import Control.Newtype

import Data.Random
import Data.Random.Distribution.Bernoulli
import Data.Number.LogFloat hiding (realToFrac)

derivingUnbox "Point"
              [t| (V.Unbox a) => Point (a,a,a) -> (a,a,a) |]
              [| \(P x)->x |]
              [| P |]

data WormLikeChain = WLC { lp :: Double
                         , links :: Int
                         }

type Angle = Double -- ^ Angle in radians
type Dist = Double
type Energy = Double
type Mass = Double
type Charge = Double -- ^ Electric charge
type PTrans = Double -- ^ Momentum transfer
type Intensity = Double -- ^ Scattering Amplitude

type R3 = (Double, Double, Double)
type P3 = Point R3

-- | Chain parametrized by bend angles
newtype ChainConfig = ChainC (V.Vector Angle)
                    deriving (Show)
instance Newtype ChainConfig (V.Vector Angle) where
    pack = ChainC
    unpack (ChainC a) = a

-- | Embedding of chain parametrized by dihedral angles
newtype ChainEmbedding = ChainE (V.Vector (Angle,Angle))
                       deriving (Show)
instance Newtype ChainEmbedding (V.Vector (Angle,Angle)) where
    pack = ChainE
    unpack (ChainE a) = a
     
-- | Position of links parametrized in Cartesian 3-space
newtype ChainPos = ChainP (V.Vector P3)
                 deriving (Show)
instance Newtype ChainPos (V.Vector P3) where
    pack = ChainP       
    unpack (ChainP a) = a

-- | The bend angles of a Euler-angle parametrized embedding
embeddingToConfig :: ChainEmbedding -> ChainConfig
embeddingToConfig (ChainE v) =
    ChainC $ V.map (\(α,β)->undefined) v

-- | Dihedral angles to Cartesian embedding given link length
embeddingToPositions :: Dist -> ChainEmbedding -> ChainPos
embeddingToPositions d (ChainE e) =
    ChainP $ V.fromList $ reverse $ go [] (V.toList e)
    where go :: [P3] -> [(Angle,Angle)] -> [P3]
          go ps [] = ps
          go [] ((_,_):rest) = go [origin] rest
          go ps@(p1:[]) ((_,_):rest) = let p0 = p1 .+^ (1,0,0)
                                       in go (p0:ps) rest
          go ps@(p1:p2:[]) ((α,_):rest) = let p0 = p1 .+^ d *^ (cos α, sin α, 0)
                                          in go (p0:ps) rest
          go ps@(p1:p2:p3:_) ((α,β):rest) =
              let p0 = p1 .+^ d *^ dihedralDir p3 p2 p1 (α,β)
              in go (p0:ps) rest
                     
-- | Normalized vector in direction specified by dihedral angles
-- relative to the three points given
-- 
-- ...--p3--p2   dir
--           \  /
--            p1
dihedralDir :: P3 -> P3 -> P3 -> (Angle,Angle) -> R3
dihedralDir p3 p2 p1 (α,β) =
   let x = normalized $ p1 .-. p2
       y = x `cross3` z
       z = case (p2 .-. p1) `cross3` (p3 .-. p1) of
                a | magnitude a < 1e-4  -> (0,0,1)
                a                       -> a
   in x ^* cos α ^* cos β ^+^ y ^* sin α ^* cos β ^+^ sin β *^ z

-- | A straight chain
straightChain :: Int -> ChainEmbedding
straightChain n = ChainE $ V.replicate n (0,0)
              
-- | Bending energy of given configuration under worm-like chain model
bendEnergy :: WormLikeChain -> ChainConfig -> Energy
bendEnergy (WLC lp links) (ChainC config) =
    V.sum $ V.map energy config
    where energy θ = undefined

-- | Electrostatic self-energy
selfEnergy :: Charge -> Dist -> ChainPos -> Energy
selfEnergy chainQ debyeL (ChainP v) =
    sum $ map pairEnergy $ pairsWith distance v
    where pairEnergy :: Dist -> Energy
          pairEnergy r = 2*chainQ / r * exp (-r / debyeL)
    
-- | Zip together all combinations (not permutations) of distinct
-- elements with function f
pairsWith :: V.Unbox a => (a -> a -> b) -> V.Vector a -> [b]
pairsWith f v =
    case V.toList v of
        x:xs -> map (f x) xs ++ pairsWith f (V.tail v)
        [] -> []

-- | Generate a random chain
randomChain :: Int -> RVar ChainEmbedding
randomChain n =
    (ChainE . V.fromList) <$> replicateM n randomLink
    where randomLink = do
              α <- uniform 0 (2*pi)
              β <- uniform 0 pi
              return (α, β)

--- Importance sampling
-- | Propose a new embedding
proposal :: ChainEmbedding -> RVar ChainEmbedding
proposal (ChainE e) = do
    n <- uniform 0 (V.length e - 1)
    α <- uniform 0 (2*pi)
    β <- uniform 0 pi
    return $ ChainE $ e V.// [(n,(α,β))]
    
-- | Metropolis acceptance
accept :: (a -> LogFloat) -> a -> a -> RVar a
accept prob x x'
    | p' > p    = return x'
    | otherwise = do a <- bernoulli $ (realToFrac $ p' / p :: Double)
                     return $ if a then x' else x
    where (p, p') = (prob x, prob x')

-- | Monte Carlo sampling of embedding space
-- 'evolve n energy beta e0' produces 'n' configurations evolved from
-- initial chain configuration 'e0' 'under energy function 'energy' at
-- temperature 'T = 1 / beta / k_B'
evolve :: Int -> (ChainEmbedding -> Energy) -> Energy -> ChainEmbedding -> RVar [ChainEmbedding]
evolve n energy beta = iterateM n go
    where go e = proposal e >>= accept prob e
          prob x = logToLogFloat $ -energy x * beta

-- | Scattering amplitude for given chain configuration
scattering :: V.Vector P3 -> R3 -> Intensity
scattering v q =
    n + 1 + 2*sum (map (\d->cos $ 2*pi * (d <.> q)) $ pairsWith (.-.) v)
    where n = realToFrac $ V.length v

--- Observables
-- | End to end distance
endToEndDist :: ChainPos -> Double
endToEndDist (ChainP p) = V.last p `distance` V.head p

-- | Squared radius of gyration
gyrationRad :: V.Vector Mass -> ChainPos -> Double
gyrationRad masses (ChainP e) =
    weight * V.sum (V.zipWith (\m p->m * p `distanceSq` origin) masses e) - magnitudeSq cm
    where weight = V.sum masses
          cm = V.foldl1 (^+^) $ V.zipWith (\m p->m *^ (p .-. origin)) masses e

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM 0 _ _ = return []
iterateM n f x = do
    x' <- f x
    xs <- iterateM (n-1) f x'
    return $ x':xs
    
