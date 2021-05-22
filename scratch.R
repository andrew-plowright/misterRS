tileName <- "R11C11"

library(raster)

source("D:/Pv/R/Pv_1_variables.R")
source("D:/Pv/R/Pv_2_paths.R")
source("D:/Pv/R/Pv_3_data.R")

tileNames <- c("R8C6", "R8C7", "R9C6", "R9C7")

setwd("C:/Users/Andrew/Dropbox/Scripts/Libraries/misterRS/R")
devtools::load_all()
setwd("D:/Pv")


trainingPts  = tp$rufclass[iv$rufclass[[it$rufclass]]$tp][[2]]
segPoly_RSDS = rsds$rufsegpoly
metrics      = rsds[c("rufsegmettex", "rufsegmetlas", "rufsegmetspc")]
segID        = var$rufsegID
overwrite    = FALSE
