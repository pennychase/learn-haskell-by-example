module Data.Sliceable 
    ( Sliceable(..),
    )
where


class Sliceable a where
    slice :: Int -> Int -> a -> a
    slice idx1 idx2 xs =
        let (_, s, _) = slicePartition idx1 idx2 xs
        in s

    slicePartition :: Int -> Int -> a -> (a, a, a)


instance Sliceable [a] where
    slicePartition idx1 idx2 xs =
        ( take idx1 xs,
          take (idx2 - idx1) $ drop idx1 xs,
          drop idx2 xs
        )

instance Sliceable a => Sliceable (Maybe a) where
    slicePartition idx1 idx2 Nothing =
        (Nothing, Nothing, Nothing)
    slicePartition idx1 idx2 (Just xs) =
        let (hd, s, tl) = slicePartition idx1 idx2 xs
        in (Just hd, Just s, Just tl)


