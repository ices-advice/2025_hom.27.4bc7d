## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(FLSAM)
library(ggplot2)

mkdir("output")

source("output_diagnostics.R")


#- Load the model output
load(file="model/model_base.RData")

#- Generate standard set of diagnostics
run 	<- "run_with_2024ibts"

FLSAM.diagnostics("output/",run=run,NSHOM.sam,NSHOM.tun,residuals=T,retro=NSHOM.retro,loo=NSHOM.loo[-2], refyy = 2024)

load(file="model/model_sens.RData")

#- Generate standard set of diagnostics
run 	<- "run_wh_2024ibts"

FLSAM.diagnostics("output/",run=run,NSHOM.sam,NSHOM.tun,residuals=T,retro=NSHOM.retro,loo=NSHOM.loo[-2], refyy = 2024)

load(file="model/model_naplusgroup.RData")

#- Generate standard set of diagnostics
run 	<- "run_na_plusgrp"

FLSAM.diagnostics("output/",run=run,NSHOM.sam,NSHOM.tun,residuals=T,retro=NSHOM.retro,loo=NSHOM.loo[-2], refyy = 2024)

 
# load(file="model/model_plusgrp.RData")
# 
# #- Generate standard set of diagnostics
# run 	<- "run_plusgrp_states"
# 
# FLSAM.diagnostics("output/",run=run,NSHOM.sam,NSHOM.tun,residuals=T,retro=NSHOM.retro,loo=NSHOM.loo[-2])
# 
# # - table of 
# 
# load(file="model/model_plusgrp_1.RData")
# 
# #- Generate standard set of diagnostics
# run 	<- "run_plusgrp_1"
# 
# FLSAM.diagnostics("output/",run=run,NSHOM.sam,NSHOM.tun,residuals=T,retro=NSHOM.retro,loo=NSHOM.loo[-2])
