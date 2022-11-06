
setwd("C:/Users/andre/Dropbox/Scripts/libraries/misterRS")
devtools::load_all()

source("D:/Temp/CNV/R/cnv2021/cnv2021_3_data.R")


segClassRas_RSDS = rsds$rufclassras
canopyMask_RSDS  = rsds$canopymask
canopyClasses    = var$canopyClasses
canopyEdits      = p$canopy$edits
openings         = 2
openingRadius    = 0.5
tileNames        = tileNames
overwrite        = TRUE
