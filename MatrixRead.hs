{-# LANGUAGE StandaloneDeriving #-}
module MatrixRead where
import Graphics.Rendering.Cairo.Matrix
deriving instance Read (Matrix)
