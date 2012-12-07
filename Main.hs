import WormLikeChain
import Control.Applicative
import Control.Monad

import Data.Random
import qualified System.Random.MWC as MWC

import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Plot.Histogram

main = do
    rng <- MWC.create
    let chainEnergy = selfEnergy 1 0.1 . embeddingToPositions 1
    --chains <- runRVar (replicateM 50000 $ chainEnergy <$> randomChain 10) rng
    chain0 <- runRVar (randomChain 10) rng
    chains <- map chainEnergy <$> runRVar (evolve 500000 10 chain0) rng
    renderableToWindow (toRenderable $ histogram chains) 640 480
    
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


