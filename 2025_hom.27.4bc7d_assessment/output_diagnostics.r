#-------------------------------------------------------------------------------
# Diagnostic plots
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Do the plotting
#-------------------------------------------------------------------------------
FLSAM.diagnostics <- function(path,run,sam,tun,residuals=T,retro=NULL,loo=NULL,loi=NULL, refyy = NA){
  pdf(file.path(path,paste0("plots_diagnostics_",run,".pdf"),sep=""))
    print(plot(sam,futureYrs=T))
    if(residuals){
      residual.diagnostics(sam)
    
      resids <- subset(residuals(sam))
      resids$std.res[which(is.na(resids$std.res))] <- 0
      resids[resids$fleet == "IBTS Q3_Q4",]
      print(xyplot(age ~ year | fleet,data=resids,main="Residuals by fleet",group=resids$fleet,cex=resids$std.res,
             panel=function(...){
               lst <- list(...)
               panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex[lst$subscript]>0,1,19),col="black",cex=1*abs(lst$cex[lst$subscript]))
             }))

      # print(xyplot(age ~ fleet | as.factor(year),data=resids,main="Residuals by year",group=resids$fleet,cex=resids$std.res,scales=list(x=list(rot=90)),
      #        panel=function(...){
      #          lst <- list(...)
      #          panel.xyplot(lst$x,lst$y,pch=ifelse(lst$cex[lst$subscript]>0,1,19),col="black",cex=1*abs(lst$cex[lst$subscript]))
      #        }))
    }
    # figure - catchabilities at age from HERAS
    catch <- catchabilities(sam)
    if(any(table(catch$fleet)>1)){
    print(xyplot(value+ubnd+lbnd ~ age | fleet,catch,
           scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
           type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
           subset=fleet %in% names(tun),
           main="Survey catchability parameters",ylab="Catchability",xlab="Age"))
    }

    # figure - f.vars
    fvar <- f.var(sam)
    print(xyplot(value+ubnd+lbnd ~ age | fleet,data=fvar,
           scale=list(alternating=FALSE,y=list(relation="free")),as.table=TRUE,
           type="l",lwd=c(2,1,1),col=c("black","grey","grey"),
           main="F random walk parameters",ylab="Variance",xlab="Age"))

    # figure - variance by data source
    obv <- obs.var(sam)
    obv$str <- paste(obv$fleet,ifelse(is.na(obv$age),"",obv$age))
    obv <- obv[order(obv$value),]
    bp <- barplot(obv$value,ylab="Observation Variance",
                  main="Observation variances by data source",col=factor(obv$fleet))
    axis(1,at=bp,labels=obv$str,las=3,lty=0,mgp=c(0,0,0))
    legend("topleft",levels(obv$fleet),pch=15,col=1:nlevels(obv$fleet),pt.cex=1.5)

    # figure - variance vs uncertainty for each data source
    plot(obv$value,obv$CV,xlab="Observation variance",ylab="CV of estimate",log="x",
         pch=16,col=obv$fleet,main="Observation variance vs uncertainty")
    text(obv$value,obv$CV,obv$str,pos=4,cex=0.75,xpd=NA)

    # figure - fishing age selectivity per year
    sel.pat <- merge(f(sam),fbar(sam),
                     by="year",suffixes=c(".f",".fbar"))
    sel.pat$sel <- sel.pat$value.f/sel.pat$value.fbar
    sel.pat$age <- as.numeric(as.character(sel.pat$age))

    custom_panel <- function(x, y, groups, subscripts, ...) {
      # cat("new panel \n")
      # cat("all groups: \n")
      # print(groups)
      # Plot all lines for each group
      unique_groups <- unique(groups[subscripts])
      # cat("unique_groups, \n")
      # print(unique_groups)
      # cat("y:\n", y, "\n")
      
      for (grp in unique_groups) {
        # cat(grp, "\n")
        # Subset data for the current group
        # cat("subscripts: \n",subscripts, "\n")
        # cat("subscripts length:", length(subscripts), "  groups length",length(groups[subscripts]), "\n")
        # cat("length x:", length(x), "\n")
        indices <- subscripts[groups[subscripts] == grp] - min(subscripts)+1
        # cat("indices: \n",indices, "\n")
        # cat("y:", (y[(indices)]), "\n")
        # cat("x:", (x[(indices)]), "\n")

        panel.lines(x[indices], y[indices], ...)
      
        #print(data.frame(x = x, y = y, subscripts = subscripts, groups = groups))
        # Aggregate sel values for each group
        
        aggregated_data <- aggregate(y[indices], by = list(groups[indices+min(subscripts)-1]), FUN = function(x){return(x[length(x)])})
        
        # cat("y: \n")
        # print(y[indices])
        #print(list(groups[indices]), "\n")

        # cat("aggregated_data: \n")
        # print(aggregated_data)
                
        # cat("\n")
        # print(grp)
        
              
        #Add labels to the end of each line
        for (i in 1:nrow(aggregated_data)) {
          group <- aggregated_data$Group.1[i]
          x_end <- 14
          y_end <- aggregated_data$x[i]
  
          # Extract the year from the group
          year <- strsplit(as.character(group), " ")[[1]][1]
          print(year)
          cat("x end",x_end, "\n")
          cat("y end",y_end, "\n")
  
          # Add the text label for the year
          panel.text(x_end+0.2, y_end+0.05, labels = year, cex = 0.7)
        }
      }
       
    }
    
    print(
      xyplot(
        sel ~ age | sprintf("%i's", floor(year / 5) * 5), data = sel.pat,
        groups = paste(year, fleet), type = "l", as.table = TRUE,
        scale = list(alternating = FALSE),
        main = "Selectivity of the Fishery by Pentad", 
        xlab = "Age", 
        ylab = "F/Fbar",
        panel = custom_panel
      )
    )

    print(
      xyplot(
        sel ~ age | sprintf("%i's", floor(year / 5) * 5), data = sel.pat,
        groups = paste(year, fleet), type = "l", as.table = TRUE,
        scale = list(alternating = FALSE),
        main = "Selectivity of the Fishery by Pentad", 
        xlab = "Age", 
        ylab = "F/Fbar",
      )
    )       

    print(retroParams(list('2022' = sam)))
    # figure - correlation matrix of model parameters
    print(cor.plot(sam))

    #Plot uncertainties as a function of time
    CV.yrs <- ssb(sam)$year
    CV.dat <- cbind(SSB=ssb(sam)$CV,
                       Fbar=fbar(sam)$CV,Rec=rec(sam)$CV)
    print(matplot(CV.yrs,CV.dat,type="l",ylim=range(pretty(c(0,CV.dat))),yaxs="i",
        xlab="Year",ylab="CV of estimate",main="Uncertainties of key parameters"))
    legend("topleft",legend=colnames(CV.dat),lty=1:5,col=1:6,bty="n")

    if(!is.null(retro))
      print(plot(retro))

    # Calculate Mohn's rho for SSB, Fbar, and Recruitment
    ssb_mean <- mean(mohns.rho(retro, ref.year=refyy, span=3, type="ssb")[1:3,1])
    fbar_mean <- mean(mohns.rho(retro, ref.year=refyy, span=3, type="fbar")[1:3,1])
    rec_mean <- mean(mohns.rho(retro, ref.year=refyy, span=3, type="rec")[1:3,1])

    #benchmark run
    # ssb_mean  <- -13.30,
    # fbar_mean <- 24.93
    # rec_mean <- 13.93


    # Combine the results into a table
    results_table <- data.frame(
      Metric = c("SSB", "Fbar", "Recruitment"),
      Mean_Rho = c(ssb_mean, fbar_mean, rec_mean)
    )

    # Print the results table
    print(results_table)
    
    if(!is.null(retro))
      retroParams(retro)

    if(!is.null(loi))
      print(plot(loi,main="Leave one in"))
    if(!is.null(loo))
      print(plot(loo,main="Leave one out"))

    retroParams_bind_ggplt <- function (x){
        
        ask_respo <- function(){
            user_input <- readline(prompt = "Continue? Type 'Y' to proceed: ")
        
            if(toupper(user_input) != "Y") {
              stop("Execution stopped by user.")
            }
        }

        retroPars <- lapply(x, params)
        lapply(retroPars, function(y) {return(y$name)})
        subretroPars <- lapply(retroPars, function(y) {
            return(subset(y, name %in% c("logFpar", "lowQpow", "logSdLogFsta", 
                "logSdLogN", "logSdLogObs", "rec_loga", "rec_logb", 
                "rho", "logScale", "logScaleSSB", "logPowSSB", "logSdSSB")))
        })

        subretroPars <- lapply(as.list(1:length(subretroPars)), function(y) {
            return(cbind(year = names(subretroPars)[y], subretroPars[[y]]))
        })

        subretroPars <- lapply(subretroPars, function(y) {
            lapply(as.list(names(table(ac(y$name)))), function(z) {
                tmpy <- subset(y, name == z)
                tmpy$nameOrig <- tmpy$name
                if (nrow(tmpy) > 1) 
                    tmpy$name_bound <- paste(tmpy$name, 1:nrow(tmpy))
                return(tmpy)
            })
        })
        
        subretroPars <- do.call(rbind, lapply(subretroPars, function(y) {
            do.call(rbind, y)
        }))
        
        if(length(lapply(x, params)) == 1){
            subretroPars$bindings <- as.numeric(gsub("[^0-9]", "", subretroPars$name_bound))

            subretroPars_exp <- subretroPars
            subretroPars_exp$lo_log <- subretroPars_exp$value - 1.96 * subretroPars_exp$std.dev
            subretroPars_exp$hi_log <- subretroPars_exp$value + 1.96 * subretroPars_exp$std.dev
            
            #subretroPars_exp$sd_exp <- sqrt((exp((subretroPars$std.dev)^2)-1) * exp((2 * subretroPars$value + (subretroPars$std.dev)^2)) )
            subretroPars_exp$val_exp <- exp(subretroPars_exp$value)
            subretroPars_exp$hi_exp <- exp(subretroPars_exp$hi_log)
            subretroPars_exp$lo_exp <- exp(subretroPars_exp$lo_log)

            p <- ggplot(subretroPars_exp, aes(x = factor(bindings), y = val_exp)) + 
                geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
                geom_errorbar(aes(ymin = lo_exp, ymax = hi_exp), 
                              width = 0.2, color = "black") +
                facet_wrap(~ name, scales = "free_y") +
                labs(x = "Bindings", y = "Value") +
                theme_minimal()

            print(p)    
        }
    }

    print(retroParams_bind_ggplt(list('2023' = NSHOM.sam)))


    retroResiduals_ggplt <- function (x, fleet, yrs) {
          res <- lapply(x, residuals)
          res <- lapply(res, function(y) {
              y[which(y$fleet == fleet & y$year %in% yrs), ]
          })
          res <- lapply(res, function(y) {
              cbind(y, retro = max(y$year))
          })
          res <- do.call(rbind, res)

          p <- ggplot(data = res, aes(x = year, y= std.res, group = retro, col = as.factor(retro)))+
          geom_line()+
          geom_point(shape = 1, size = 3)+
          xlab("Year")+
          theme_bw()

          if(! -1 %in% unique(res$age)) p <- p + facet_wrap(~age)
          print(p)



    }

  print(retroResiduals_ggplt(NSHOM.retro, fleet = "catch unique", yrs = c((refyy-4):refyy)))
  print(retroResiduals_ggplt(NSHOM.retro, fleet = "IBTS Q3_Q4", yrs = c((refyy-4):refyy)))
  print(retroResiduals_ggplt(NSHOM.retro, fleet = "PFA standardized CPUE", yrs = c((refyy-4):refyy)))

  dev.off()

}
