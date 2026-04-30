## Run analysis, write model results

## Before:
## After:

 .libPaths(c(file.path(getwd(),"boot/library"),.libPaths()))
 .libPaths()

library(icesTAF)
library(FLSAM)

mkdir("model")

startYr 	<- 2002 # set start of the time-series to use
rage  		<- 1 	# set recruitment age
pg 			<- 14 	# set plusgroup

#- Load input data
load(file="data/input.RData", verbose = TRUE)

#- dimensions
minyr       <- startYr
maxyr		<- max(as.numeric(rownames(CAA)))

#- Construct FLR object
dmns 		<- list(age=colnames(CAA),year=startYr:max(rownames(CAA)),unit="unique",season="all",area="unique",iter=1)
NSHOM 		<- FLStock(m=FLQuant(NA,dimnames=dmns))

#- Fill the objects
NSHOM@name 			<- "North Sea horse mackerel"
NSHOM@range[c("minfbar","maxfbar")] <- c(2,8)
units(NSHOM)[1:17]  <- as.list(c(rep(c("tonnes","thousands","kg"),4), 
	                             rep("NA",2),"f",rep("NA",2)))

NSHOM@catch.n[] 	<- t(CAA[ac(dmns$year),])
NSHOM@landings.n[] 	<- NSHOM@catch.n
NSHOM@discards.n[] 	<- 0
	
NSHOM@catch.wt[] 	<- t(WAA[ac(dmns$year),])
NSHOM@landings.wt[] <- NSHOM@catch.wt
NSHOM@stock.wt[] 	<- NSHOM@catch.wt

NSHOM@catch  		<- computeCatch(NSHOM)
NSHOM@landings  	<- computeLandings(NSHOM)
NSHOM@discards  	<- computeDiscards(NSHOM)
	
NSHOM@m[] 			<- natmort
NSHOM@mat[] 		<- mat
NSHOM@m.spwn[] 		<- 0.25
NSHOM@harvest.spwn[]<- 0.25

#- Select recruitment age & plusgroup
# NSHOM				<- NSHOM[ac(rage:max(as.numeric(colnames(CAA)))),]
NSHOM <- trim(NSHOM, age = rage:max(as.numeric(colnames(CAA))))
NSHOM 				<- setPlusGroup(NSHOM,pg,na.rm=T)

#- remove the 2024 survey data from the assessment
load(taf.data.path("delta-LN_SSB.Rda"), verbose = TRUE)
    dmns 			<- list(age="all",year=rownames(SI.dln_SSB$idx)[-length(SI.dln_SSB$idx)],unit="unique",season="all",area="unique",iter=1)
    IBTS 			<- FLIndex(index=FLQuant(NA,dimnames=dmns))
    IBTS@index[] 	<- c(SI.dln_SSB$idx[-length(SI.dln_SSB$idx),1])
    IBTS@range[c("startf","endf")] <- c(0.6,0.8)
    IBTS@name 		<- "IBTS Q3_Q4"
    IBTS@type 		<-"biomass"

#Load indices
NSHOM.tun 			<- FLIndices(window(IBTS,start=startYr),CPUE)


#- Setup control file
NSHOM.ctrl 								<- FLSAM.control(NSHOM,NSHOM.tun)
NSHOM.ctrl@states["catch unique",] 		<- c(1,2,3,4,5,6,7,8,9,10,11,11,11,11)
NSHOM.ctrl@f.vars["catch unique",] 		<- c(0,0,rep(1,12))
NSHOM.ctrl@obs.vars[1,] 				<- c(1,rep(2,6),rep(3,3),rep(4,4)) +101
NSHOM.ctrl@cor.F 						<- 2
NSHOM.ctrl@biomassTreat[c(2,3)] 		<- c(0,2) # ssb and exploitable biomass
NSHOM.ctrl 								<- update(NSHOM.ctrl)
NSHOM.ctrl 								<- update(NSHOM.ctrl)

#- Downweighting of historic catch data
catch.vars 								<- list(residual=NSHOM@catch.n)
catch.vars[[1]][] 						<- 1
catch.vars[[1]][,ac(startYr:2002)] 		<- 0.5

NSHOM.ctrl@residuals 					<- T

NSHOM.sam 								<- FLSAM(NSHOM,NSHOM.tun,NSHOM.ctrl,
												 catch.vars=catch.vars)
NSHOM@stock.n 							<- NSHOM.sam@stock.n
NSHOM@harvest 							<- NSHOM.sam@harvest

#- Run a retrospective	
NSHOM.retro 							<- FLSAMs()
NSHOM.ctrl@residuals 					<- T
NSHOM.retro[[ac(maxyr)]] 				<- NSHOM.sam
for(iRetro in (maxyr-1):(maxyr-4)){
	print(iRetro)
	NSHOM.ctrlRetro 					<- NSHOM.ctrl
	NSHOM.ctrlRetro@range["maxyear"]	<- iRetro
	NSHOM.retro[[ac(iRetro)]] 			<- FLSAM(window(NSHOM,end=iRetro),
											 	 window(NSHOM.tun,end=iRetro),
											 	 NSHOM.ctrlRetro,
											 	 catch.vars=list(residual=window(catch.vars[[1]],end=iRetro)))
}

NSHOM.retro
#- Run a leave-one-out model
NSHOM.loo 								<- FLSAMs()
NSHOM.ctrl@residuals 					<- T
NSHOM.loo[["all fleets"]] 				<- NSHOM.sam

for(iloo in names(NSHOM.ctrl@fleets)[2:3]){
	NSHOM.ctrlloo 					<- NSHOM.ctrl
	NSHOM.ctrlloo 					<- drop.from.control(NSHOM.ctrlloo,fleets=iloo)
	NSHOM.ctrlloo@plus.group 		<- NSHOM.ctrlloo@plus.group[-which(names(NSHOM.tun)==iloo)]
	NSHOM.tunloo 					<- NSHOM.tun[-which(names(NSHOM.tun)==iloo)]
	NSHOM.loo[[paste("without",iloo)]]	<- FLSAM(NSHOM,NSHOM.tunloo,NSHOM.ctrlloo,
												 catch.vars=catch.vars)
}

save(NSHOM,NSHOM.tun,NSHOM.sam,NSHOM.ctrl,NSHOM.retro,NSHOM.loo,file="model/model_sens.RData")
