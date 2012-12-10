import WormLikeChain

import Control.Applicative
import Control.Monad

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Data.Random
import qualified System.Random.MWC as MWC

import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Plot.Histogram

main = do
    rng <- MWC.create
    chain0 <- runRVar (randomChain 10) rng
    chains <- runRVar (evolve 500000 10 chain0) rng
    let pos = map (embeddingToPositions 1) chains
    --let c = map endToEndDist pos
    --let c = map (selfEnergy 1 0.1) pos
    let c = map (\(ChainP p)->gyrationRad (VU.map (const 1) p) (ChainP p)) pos
    renderableToWindow (toRenderable $ histogram $ V.fromList c) 640 480
    
histogram values = layout
    where hist = plot_hist_values  ^= values
               $ plot_hist_bins  ^= 100
               $ plot_hist_drop_lines ^= True
               $ defaultPlotHist
          layout :: Layout1 Double Int
          layout = layout1_title ^= "Hello World"
                 $ layout1_plots ^= [ Left (histToPlot hist)
                                    ]
                 $ defaultLayout1


