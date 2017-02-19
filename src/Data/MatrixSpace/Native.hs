module Data.MatrixSpace.Native where

#include <haskell>

newtype Mat (r :: Nat) (c :: Nat) a = Mat (Matrix a) deriving (Functor, Foldable, Traversable)

deriving instance (KnownNat r, KnownNat c, Eq a) => Eq (Mat r c a)
deriving instance (KnownNat r, KnownNat c, Show a) => Show (Mat r c a)

instance (KnownNat r, KnownNat c, AdditiveGroup a) => AdditiveGroup (Mat r c a) where
  zeroV = Mat $ matrix (fromInteger $ natVal (Proxy :: Proxy r)) (fromInteger $ natVal (Proxy :: Proxy c)) (const zeroV)
  (Mat a) ^+^ (Mat b) = Mat $ elementwise (^+^) a b
  negateV (Mat a) = Mat $ negateV <$> a

instance (KnownNat r, KnownNat c, VectorSpace a) => VectorSpace (Mat r c a) where
  type Scalar (Mat r c a) = Scalar a
  k *^ (Mat a) = Mat $ (k *^) <$> a

type Vec (n :: Nat) a = Mat n 1 a
type VecT (n :: Nat) a = Mat 1 n a

vecX :: Vec 3 a -> a
vecX (Mat a) = unsafeGet 1 1 a

vecY :: Vec 3 a -> a
vecY (Mat a) = unsafeGet 2 1 a

vecZ :: Vec 3 a -> a
vecZ (Mat a) = unsafeGet 3 1 a

vec3 :: a -> a -> a -> Vec 3 a
vec3 x y z =
  Mat $ matrix 3 1 $ \n ->
    case n of
      (1, 1) -> x
      (2, 1) -> y
      (3, 1) -> z
      _ -> error "vec3"

instance (KnownNat n, VectorSpace a, Num a, a ~ Scalar a) => InnerSpace (Vec n a) where
  Mat a <.> Mat b = trace $ transpose a `multStd` b

instance Num a => HasCross3 (Vec 3 a) where
  cross3 a b =
    let (x, y, z) = cross3 (vecX a, vecY a, vecZ a) (vecX b, vecY b, vecZ b) in
    vec3 x y z

infix 7 ^*^
(^*^) :: HasCross3 v => v -> v -> v
(^*^) = cross3

instance (VectorSpace a, Num a, a ~ Scalar a) => HasCross3 (a, Vec 3 a) where
  cross3 (as, av) (bs, bv) = (as * bs - av <.> bv, as *^ bv ^+^ bs *^ av ^+^ av ^*^ bv)

qrotation :: (VectorSpace a, Floating a, a ~ Scalar a) => a -> Vec 3 a -> (a, Vec 3 a)
qrotation phi ort = (cos (phi / 2.0), sin(phi / 2.0) *^ ort)

qconjugate :: AdditiveGroup a => (a, Vec 3 a) -> (a, Vec 3 a)
qconjugate (s, v) = (s, negateV v)

qrotate :: (VectorSpace a, Floating a, a ~ Scalar a) => (a, Vec 3 a) -> Vec 3 a -> Vec 3 a
qrotate q v = snd $ (q ^*^ (0.0, v)) ^*^ (qconjugate q)
