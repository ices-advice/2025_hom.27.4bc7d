rm(list=ls())
library(FLCore)
library(FLAssess)
library(stockassessment) 
library(FLash)
library(ggplot2)

# load("model/model_base.RData")
load("model/model_base.Rdata")

hom <- NSHOM

ImY <- 2025
AdY <- 2026
CtY <- 2027

# define the recruitment for the short term forecast (starting at ImY )
rec.years <- (2014:2024)
hom.srr <- as.FLSR(trim(hom,year=rec.years))
model(hom.srr)<- "geomean"
hom.srr <- fmle(hom.srr)
rgm <- exp(mean(log(rec(NSHOM)[,ac(2014:2024)])))

params(hom.srr)["a", ]  <- rgm
hom.srr





srr <- as.data.frame(hom.srr@rec)
ggplot(srr, aes(as.factor(year), data)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = rgm), color = "blue")

# replace unknown W@age for + group in recent years
stock.wt(hom)[14,ac(2021:2023)] <-  stock.wt(hom)[14,ac(2020)]
catch.wt(hom)[14,ac(2021:2023)] <-  catch.wt(hom)[14,ac(2020)]


#Expand stock object
#NEA.hom.proj <- stf(NEA.hom,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE)

# What they used in the previous forecast
# hom.proj <- stf(hom,nyears=3,wts.nyears=5,arith.mean=TRUE,na.rm=TRUE,fbar.nyears=1) # to use object saved before rounding
hom.proj <- stf(hom,
                nyears=3, 
                wts.nyears=5, # mean weight at age average 
                fbar.nyears=1, # selectivity 
                arith.mean=TRUE,
                na.rm=TRUE, 
                f.rescale = FALSE)


# GENERATE targets from reference points
# refpts=FLPar(c(0, 0.034, 0.042, 236228,170000,236228,NA),
#              dimnames=list(params=c("F0", "Fmsy","Flim","Btrigger","Blim","Bpa","Fpa"),year=iy,iter=1),
#              units=c(NA,NA,NA,"tonnes","tonnes","tonnes",NA))
# targets <- Matrix::expand(as(refpts, 'FLQuant'), year=ay:fy)


# define the option for the advice sheet
options.l <- list( 

  "F = Fmsy"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch","f","f"),
                          val=c(970,0.034,0.034))),
  
  

  "F = 0"=
    fwdControl(data.frame(year=c(ImY,AdY,CtY),
                          quantity=c("catch", "f", "f"),
                          val=c(970, 0.0, 0.0))),
  
  
  "Flim" = fwdControl(data.frame(year = c(ImY, AdY, CtY),
                                 quantity = c("catch", "f", "f"),
                                 val = c(970, 0.0385, 0.0385))),
  
  "F = F2025" = fwdControl(data.frame(year = c(ImY, AdY, CtY),
                                 quantity = c("catch", "f", "f"),
                                 rel.year = c(NA, 2025, 2026),
                                 val = c(970, 1, 1))), # ??,
  
  "Catch2026 = Catch2025 - 20%" = fwdControl(data.frame(year = c(ImY, AdY, CtY),
                                 quantity = c("catch", "catch", "f"),
                                 rel.year = c(NA, NA, 2026),
                                 val = c(970, 970*0.80, 1))), # bycatch quota -20%
  
  "Catch2026 = Catch2025" = fwdControl(data.frame(year = c(ImY, AdY, CtY),
                                 quantity = c("catch", "catch", "f"),
                                 rel.year = c(NA, NA, 2026),
                                 val = c(970, 970, 1))),
  
  "Catch2026 = Catch2025 + 25%" = fwdControl(data.frame(year = c(ImY, AdY, CtY),
                                                        quantity = c("catch", "catch", "f"),
                                                        rel.year = c(NA, NA, 2026),
                                                        val = c(970, 970*1.25, 1))) # bycatch quota + 25%
  

) #End options list



#Calculate options for two option tables
hom.options   <- lapply(options.l,function(ctr_op) {fwd(hom.proj,ctrl=ctr_op,sr = hom.srr)})
names(hom.options)<-names(options.l)
plot(hom.options[[2]])
plot(hom.options[[3]])
plot(hom.options[[4]])



                 
#Multi-options table - standard one to show wider range of options for the report
# F multipliers from 0 to 2 *roll over F
fmult.targs  <- seq(0,1,by=0.002)
mult.opts.l <- lapply(as.list(fmult.targs),function(fmult) {
                          fwdControl(data.frame(year=c(ImY,AdY,CtY),
                                          quantity=c("catch", "f","f"),
                                          # rel=c(NA, NA, 2026),
                                          val=c(970, fmult, fmult)))
                  })
names(mult.opts.l) <- sprintf("Fmult(2025) = %4.3f",fmult.targs)

#Calculate options for two option tables
hom.options   <- lapply(options.l,function(ctr_op) {fwd(hom.proj,ctrl=ctr_op,sr = hom.srr)})
names(hom.options)<-names(options.l)
hom.mult.opts <- lapply(mult.opts.l,function(ctr_op) {fwd(hom.proj,ctrl=ctr_op,sr = hom.srr)})
names(hom.mult.opts)<-names(mult.opts.l)


### ======================================================================================================
### Write Options Tables
### ======================================================================================================

output.base <- paste0(getwd(), "/forecast")
#Document input settings
input.tbl.file <-paste(output.base,"/options - input.csv",sep=".")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY)){
    col.dat <- sapply(input.tbl.list,function(slt) slot(hom.proj,slt)[,as.character(yr),drop=TRUE])
    write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
    write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

tbl.yrs <- c(ac(2025:2027))
#Detailed options table
options.file <-paste(output.base,"/options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(hom.options)) {
    opt <- names(hom.options)[i]
    stk <- hom.options[[opt]]
    #Now the F and N by age
    nums.by.age <- stk@stock.n[,tbl.yrs,drop=TRUE]
    colnames(nums.by.age) <- sprintf("N(%s)",tbl.yrs)
    f.by.age    <- stk@harvest[,tbl.yrs,drop=TRUE]
    colnames(f.by.age) <- sprintf("F(%s)",tbl.yrs)
    age.tbl     <- cbind(Age=rownames(f.by.age),N=nums.by.age,F=f.by.age)
    #And now the summary tbl
    sum.tbl     <- cbind(Year=tbl.yrs,SSB=ssb(stk)[,tbl.yrs],
                        F.bar=fbar(stk)[,tbl.yrs],Yield=computeCatch(stk)[,tbl.yrs])
    #Now, bind it all together
    sum.tbl.padding <- matrix("",nrow=nrow(age.tbl)-nrow(sum.tbl),ncol=ncol(sum.tbl))
    comb.tbl    <- cbind(age.tbl," ",rbind(sum.tbl,sum.tbl.padding))
    #And write it - hdr first, then the rest
    write.table(sprintf("%s). %s",letters[i],opt),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(t(colnames(comb.tbl)),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(comb.tbl,options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(c(""),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
}

#Options summary table
opt.sum.tbl <- function(stcks,fname) {
  options.sum.tbl <- sapply(as.list(1:length(stcks)),function(i) {
    opt <- names(stcks)[i]
    stk <- stcks[[opt]]
    #Build up the summary
    sum.tbl <- data.frame(Rationale=ac(names(stcks)[i]),
                          F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
                          Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
                          SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],
                          
                          TSB.ImY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(ImY),drop=TRUE],
                          
                          F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
                          Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
                          SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],
                          
                          TSB.AdY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(AdY),drop=TRUE] ,
                          
                          SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE],
                          TSB.CtY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(CtY),drop=TRUE] )
    
  })
  options.sum.tbl <- t(options.sum.tbl)
  options.sum.tbl[,1]<-names(stcks)
  colnames(options.sum.tbl) <- c("Rationale",
                                 sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),sprintf("TSB (%i)",ImY),
                                 sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),sprintf("TSB (%i)",AdY),
                                 sprintf("SSB (%i)",CtY),sprintf("TSB (%i)",CtY) )
  write.csv(options.sum.tbl,file=fname,row.names=FALSE)
}
opt.sum.tbl(stcks=hom.options,fname=paste(output.base,"/options - summary.csv",sep="."))
opt.sum.tbl(stcks=hom.mult.opts,fname=paste(output.base,"/multi-options - summary.csv",sep="."))

stk <- hom.mult.opts
 sum.tbl <- data.frame(Rationale=ac(names(stcks)[i]),
                          F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
                          Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
                          SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],
                          
                          TSB.ImY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(ImY),drop=TRUE],
                          
                          F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
                          Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
                          SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],
                          
                          TSB.AdY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(AdY),drop=TRUE] ,
                          
                          SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE],
                          TSB.CtY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(CtY),drop=TRUE] )


### ======================================================================================================
### apply the ICES MSY advice rule
# ============================================================================


 #  if projections indicate that SSB advice is below MSY Btrigger, then find the F that would lead an SSB on the MSY HCR
 

# load function that measures the difference between the Ftarget you put in and the F msy hcr that corresponds to the resulting SSB
# this function needs to be minimised 
 source('FindFtarget.r')
 
# reference point for the MSY HCCR
msyref <- c(Ftarg=0.26,Btrig=2.58e6)

#Find the Fmultiplier to apply relative to ImY
Ft<-optimise(FindFtarget,c(0.1,1.5),stk=hom.proj,srr=hom.srr,ady=AdY,cty=CtY,op=msyref,tol=0.0000000001)$minimum  

# uupdate F advice
FAdY  <- c(Ft*fbar((hom.proj)[,ac(ImY)]))


# do the forecast for this option
ctrl_MSY<-fwdControl(data.frame(year=c(AdY,CtY),
                                          quantity=c("f","f"),
                                          val=c(FAdY,FAdY)))

hom.options<-(fwd(hom.proj,ctrl = ctrl_MSY,sr =hom.srr))
names(hom.options)<-names("MSY AR")



### Write Option Table


output.base <- output.dir
opt <- names(hom.options)
stk <- hom.options
#Build up the summary
options.sum.tbl <- data.frame(Rationale="MSY AR",
F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],

TSB.ImY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(ImY),drop=TRUE],

F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],

TSB.AdY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(AdY),drop=TRUE] ,

SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE],
TSB.CtY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(CtY),drop=TRUE] )


#options.sum.tbl <- t(options.sum.tbl)
options.sum.tbl[,1]<-"MSY AR"
colnames(options.sum.tbl) <- c("Rationale",
sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),sprintf("TSB (%i)",ImY),
sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),sprintf("TSB (%i)",AdY),
sprintf("SSB (%i)",CtY),sprintf("TSB (%i)",CtY) )
write.csv(options.sum.tbl,file=paste(output.base,"/options - summary applying Btrigger.csv",sep="."),row.names=FALSE)




#### make a plot to check if the MSY AR is computed fine

opt <- read.csv("./STFresults/multi-options - summary.csv" )

plot(0,0,xlim=c(0,5e6),ylim=c(0,0.4),cex=0,xlab = "SSB in 2025" , ylab = "Fbar in 2025",main="ICES MSY Advice Rule")
points(opt$SSB..2025. , opt$Fbar..2025. , pch=19,cex=0.6)
segments(0,0,2.58e6,0.26)
segments(5.5e6,0.26,2.58e6,0.26)
points(options.sum.tbl[8],options.sum.tbl[6],pch=20,col="red",cex=2)







### ======================================================================================================
### compute the STF for all precautionary options from the 2020 MSE
# ============================================================================


#
#
#
## read in the Ftarget/Btrigger values for these options
#scen <- read.csv("MSE scenarios.csv")
#scen <- tidyr::gather(scen, "Ftarget","value", - "Btrigger") 
#scen <- subset(scen , !is.na(value)) 
#scen$Ftarget <- as.numeric(gsub("X","", scen$Ftarget))
#
## create results table
#
#namesRes <- c ("Ftarget"  ,
#                   "Btrigger" ,
#                   sprintf("Fbar (%i)",ImY),
#                   sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),sprintf("TSB (%i)",ImY),
#sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),sprintf("TSB (%i)",AdY),
#sprintf("SSB (%i)",CtY),sprintf("TSB (%i)",CtY) )
#res<-data.frame(matrix(rep(NA,dim(scen)[1]*length(namesRes)),ncol = length(namesRes))) 
#names(res) <- namesRes
#
#
#
## loop through the 70+ scenarios
#for (sc in 1:dim(scen)[1])
#{
#Ftarget   <- scen$Ftarget[sc]
#Btrigger  <- scen$Btrigger[sc]
#
## reference point for the MSY HCCR
#msyref <- c(Ftarg=Ftarget,Btrig=Btrigger)
#
##Find the Fmultiplier to apply relative to ImY
#Ft<-optimise(FindFtarget,c(0.1,2),stk=hom.proj,srr=hom.srr,ady=AdY,cty=CtY,op=msyref,tol=0.0000000001)$minimum  
#
## uupdate F advice
#FAdY  <- c(Ft*fbar((hom.proj)[,ac(ImY)]))
#
#
## do the forecast for this option
#ctrl_MSY<-fwdControl(data.frame(year=c(AdY,CtY),
#                                          quantity=c("f","f"),
#                                          val=c(FAdY,FAdY)))
#
#hom.options<-(fwd(hom.proj,ctrl = ctrl_MSY,sr =hom.srr))
#names(hom.options)<-names(paste("Btrg=",Btrigger,"Ftrg=",Ftarget))
#
#
#
#### Write Option Table
#output.base <- output.dir
#opt <- names(hom.options)
#stk <- hom.options
##Build up the summary
#resSc <- c(Ftarget,Btrigger,
#fbar(stk)[,as.character(ImY),drop=TRUE],
#computeCatch(stk)[,as.character(ImY),drop=TRUE],
#ssb(stk)[,as.character(ImY),drop=TRUE],
#quantSums(stock.wt(stk)*stock.n(stk))[,as.character(ImY),drop=TRUE],
#fbar(stk)[,as.character(AdY),drop=TRUE],
#computeCatch(stk)[,as.character(AdY),drop=TRUE],
#ssb(stk)[,as.character(AdY),drop=TRUE],
#quantSums(stock.wt(stk)*stock.n(stk))[,as.character(AdY),drop=TRUE] ,
#ssb(stk)[,as.character(CtY),drop=TRUE],
#quantSums(stock.wt(stk)*stock.n(stk))[,as.character(CtY),drop=TRUE] )
#
#res[sc,] <- resSc
#rm(resSc)
#}
#
#
#write.csv(res,file=paste(output.base,"/options - summary MSE scenarios.csv",sep="."),row.names=FALSE)
#
#



 #### Make the plots for the contribution of the assumption in R on the cathc and SSB
 

msy<-hom.options

save(msy,file="mac_MSYproj2024.RData")


load("mac_MSYproj2024.RData")

C25 <- catch.n(msy)*catch.wt(msy)
C25 <- C25[,"2025"]
C25 <- as.data.frame(C25,drop=T)
C25$quant<-"catch 2025"

bgen25 <-  mat(msy) * stock.wt(msy) * stock.n(msy) * exp(- harvest.spwn(msy) * harvest(msy) - m.spwn(msy) * m(msy))
bgen25 <- bgen25[,"2025"]
bgen25 <- as.data.frame(bgen25,drop=T)
bgen25$quant <- "spawning biomass 2025"


adv<-rbind(C25,bgen25)
adv$"Year Class" <- 2025-adv$age
adv$basis <-"SAM estimate"
adv$basis[adv$"Year Class"==2023] <- "RCT3"
adv$basis[adv$"Year Class"==2024] <- "GM"
adv$basis[adv$"Year Class"==2025] <- "GM"

dd<-aggregate(data~quant,adv,sum)
names(dd)[2] <- "tot"
adv<-merge(adv,dd,all.x=T)
adv$data<-adv$data/adv$tot

adv$"Year Class"<-gsub("20","",adv$"Year Class")
adv$"Year Class"[adv$"Year Class"==""]  <- "20"

ggplot(adv , aes(x="",y=data,fill=basis)) + 
    geom_bar(width=1,stat="identity",col="black") + 
    facet_grid(.~quant)  +
    xlab("")  +  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
   coord_polar(theta = "y")
   
   