## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)

library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(FLCore)


mkdir("report")

load(file="model/model_base.RData", verbose = TRUE)

NSHOM@catch.n[]
NSHOM@catch.wt[]


load(file="data/input.RData", verbose = TRUE)

# - setup FLStock object for the reporting
    startYr 	<- 2002 # set start of the time-series to use
    year        <- 2024
    uniqueAreas     <- c('27.4.b','27.4.c','27.7.d')
    dmns 		<- list(age=colnames(CAA),
                        year=startYr:max(rownames(CAA)),
                        unit="unique",
                        season="all",
                        area="unique",
                        iter=1)

    NSHOM.rep 		<- FLStock(m=FLQuant(NA,dimnames=dmns))
    NSHOM.rep@name	<- "North Sea horse mackerel"
    NSHOM.rep@desc  <- "hom.27.4bc7d"
    NSHOM.rep@range[c("minfbar","maxfbar")] <- c(2,8)

    units(NSHOM.rep)[1:17]  <- as.list(c(rep(c("tonnes","thousands","kg"),4), 
    	                             rep("NA",2),"f",rep("NA",2)))
    NSHOM.rep@catch.n[] <- t(CAA[ac(dmns$year),])
    NSHOM.rep@catch.wt[] <- t(WAA[ac(dmns$year),])

colSums(NSHOM.rep@catch.n*NSHOM.rep@catch.wt, na.rm = TRUE)

    
# - check if indeed the input is the same in the eventual object
    NSHOM.rep@catch.wt[ac(1:14)] == NSHOM@catch.wt[] #> indeed the same, but 14 is used as the plusgroup in the model

# - plot the CAA numbers
    windows()
    ggplot(as.data.frame(catch.n(NSHOM.rep)), aes(year, as.factor(age), size=data)) + geom_point(shape=21) + 
      scale_size(range = c(1, 14)) + ylab("age") + theme(legend.position = "none")+theme_bw()

# - convert CAA and WAA to report tables
    CAA_tab <- as.data.frame(NSHOM.rep@catch.n[])[,c(1,2,7)] %>% mutate(data = round(data/1000, 1)) %>% tidyr::spread(key = "year", value = "data")
    write.csv(CAA_tab, "report/CAA_tab.csv")
    WAA_tab <- as.data.frame(NSHOM.rep@catch.wt[])[,c(1,2,7)] %>% mutate(data = round(data, 3)) %>% tidyr::spread(key = "year", value = "data")
    write.csv(WAA_tab, "report/WAA_tab.csv")

##############################################################
#################### Plot commercial data ####################
##############################################################


# - create caton. 
    new_caton <- as.data.frame(WAA * CAA)
    new_caton$years <- rownames(new_caton)
    new_caton <- tidyr::gather(new_caton, key = "age", value = "caton",-years)  %>%  group_by(years)  %>% summarise(caton = sum(caton, na.rm= TRUE))

    write.csv(new_caton, "report/caton_tab.csv")

    #from the official caton.txt file by Jens 
    {
      caton<-"caton.txt"
      file_sel <- file.path( taf.data.path(), list.files(taf.data.path())[grepl("caton",list.files(taf.data.path()))])
      read_the_text <- scan(file =   file_sel, # if your data is in a text file, use the file argument
                        what = character(),
                        sep = "\n")
      caton_new<-data.frame(year=year, catch=read_the_text[length(read_the_text)])
      caton_new$catch<-round(as.numeric(gsub(",", "",caton_new$catch)),0)
      
      #Caton_orig<-read.csv(paste0(getwd(),"./data/commercial/caton.csv"), sep =";")
      #Caton_new<-rbind(Caton_orig, caton_new)
    } 
    write.csv2(caton_new, "report/caton.csv",row.names=FALSE)

#################### Catch at age in numbers (milions)  all areas ##############################

  CAA_df      <- as.data.frame(CAA)
  CAA_df$year <- rownames(CAA_df)
  CAA_df_long <- reshape::melt(CAA_df,id=c("year"))
  colnames(CAA_df_long) <- c("year","age",'catch')
  #lapply(CAA_df_long, class)
  #unique(CAA_df_long$year)
  CAA_df_long$year <-as.numeric(CAA_df_long$year)

  png("catch_N_at_age.png",, width=1700, heigh= 1300, units="px", pointsize=7, bg="white", res=300)
  p = ggplot(data=CAA_df_long, aes(x=year, y=age)) 
  p = p + geom_point(aes(size=sqrt(catch)/max(catch, na.rm =TRUE)),alpha=0.5)
  p = p + scale_shape(solid = FALSE) 
  p = p + scale_size_continuous(range=c(0,8))
  p = p + scale_x_continuous(breaks=seq(min(CAA_df_long$year), max(CAA_df_long$year), by = 5))
  p = p +  theme(legend.position = "none")
  p = p  + ggtitle("NSHM: catch at age (N; observed) all areas") + theme(plot.title = element_text(color="grey40", size=7, face="bold.italic", hjust=0.5))
  p = p + theme(plot.title = element_text(size = 15, face = "bold"))
  p = p + theme(text = element_text(size=15))
  print(p)
  dev.off()
#################### Internal consistency of CAA #########################
  library(latticeExtra)
  FLQcaa <- FLIndex(index=NSHOM@catch.n)
  plot(FLQcaa, type = "internal")




#################### Mean weight at age   #########################

  WAA_df      <- as.data.frame(WAA)
  WAA_df$year <- rownames(WAA_df)
  WAA_df_long <- reshape::melt(WAA_df,id=c("year"))
  colnames(WAA_df_long) <- c("year","age",'wgt')
  #lapply(WAA_df_long, class)
  #unique(WAA_df_long$year)
  WAA_df_long$year <-as.numeric(WAA_df_long$year)
  head(WAA_df_long)
  
  WAA_df_long_cut <- WAA_df_long[WAA_df_long$year %in% c(2003:2024),]

  png("report/mean_weight_at_age.png", width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

  p <- ggplot(  WAA_df_long_cut, aes(x = year, y = wgt, colour=age, group = age)) + geom_point() + geom_line()  
  p = p + theme(text = element_text(size=10))+labs(x="Year",y="Weight (kg)")  
  p = p  + ggtitle("Mean weight at age (kg)") + theme(plot.title = element_text(color="grey40", size=5, face="bold.italic", hjust=0.5))
  p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8), legend.key = element_rect(fill = "white")) 
  p = p + theme(plot.title = element_text(size = 12, face = "bold"))
  p = p + theme(panel.background = element_rect(fill = "white")) 
  p = p + theme(axis.line = element_line(color="grey40", size = 0.3))

  print(p)

  dev.off()

####################  Commercial CPUE  ###################
  load(taf.data.path("/commercial_CPUE.Rda"), verbose = TRUE)

  ggplot(com_CPUE, aes(x= Year, y= Estimate))+
  geom_ribbon(aes(ymin = Lower, ymax = Upper ), fill = "grey70", col = "darkgrey", alpha = 0.5) +
  geom_line( lty =2)+
  theme_bw()

####################  Catch Curves and Total mortality (Z) by cohort  ####################

  endYearCohort   <- 2016
  startYearCohort <- 2002
  stkCohort <- FLCohort(log(NSHOM@catch.n))

  png("report/Catch_curves_by_cohort.png", width=2500, heigh= 1500, units="px", pointsize=7, bg="white", res=300)
  FLCore::plot(stkCohort[,as.character(2003:endYearCohort)])
  dev.off()

  coh       <- startYearCohort:endYearCohort
  cont      <- length(coh)
  par_a     <- vector(length=cont)
  par_b     <- vector(length=cont)
  pvalue_b  <- vector(length=cont)


  data<-as.data.frame(stkCohort)

  for(i in 1:cont){
    datsel=data[data$cohort==coh[i],]
    # Elimino los a?os sin capturas (NA) y los que se apuntaron como 0 (problemas con logaritmos)
    datsel <- datsel[!is.na(datsel$data),]
    datsel <- datsel[datsel$data>0,]
    model=lm(log(data)~age,data=datsel)
    par_a[i]=model$coefficients[1]
    par_b[i]=model$coefficients[2]
    pvalue_b[i]=summary(model)$coef[8]
    base::plot(datsel$age,log(datsel$data),type="b",main=coh[i])
  }

  result=data.frame(par_a,par_b,pvalue_b)

  png("report/Catch_curves_by_cohort_par_slope.png", width=2500, heigh= 1500, units="px", pointsize=10, bg="white", res=300)
  base::plot(coh,-par_b, type="b", pch=19, xlab="Cohort", ylab="Total mortality Z", main="Total mortality by cohort", 
      cex.main=1.8, cex.axis=1.3, cex.lab=1.5, ylim=c(0,max(-par_b)), xaxt="none")
  axis(1, seq(startYearCohort,endYearCohort,1))
  dev.off()

  write.csv(drop(stkCohort),"report/catch_at_age_cohort.csv", row.names=F)

  #- test alternative catch curve plot
   stkCohort <- FLCohort(NSHOM@catch.n*1000)
   cohort_df<- as.data.frame(stkCohort)
   cohort_df$data<- round(cohort_df$data,0)
   cohort_df$data[cohort_df$data == 0] <- NA
   cohort_df$data <- log(cohort_df$data)
   range(cohort_df$data, na.rm = TRUE)
   cohort_df$year <- cohort_df$cohort+cohort_df$age
   cohort_df <- cohort_df[!is.na(cohort_df$data),]
   
   
   ggplot(cohort_df, aes(x=year, y=data, col=as.factor(cohort), group = cohort))+
   geom_line()+
   geom_point(size = 3, col = "white")+
   labs(color = "cohort")+
   ylab("log(abundance)")+
   geom_text(aes(label = age), size = 3 , show.legend = FALSE)+
   theme_bw()

  cohort_df  %>%  
    mutate(freq_age = exp(data) * age)  %>% 
    group_by(year)  %>%  summarize(mean_age_c = sum(freq_age)/sum(exp(data)))  %>%  
    as.data.frame()
  

#################### Mean length at age  ##############################
  #- create lena_area.csv
    #::manually from the catch table::#
    {#Lena<-aggregate(LECA~Year+AgeOrLength,  NSHM_2021_catch_age, FUN=sum)
      #names(Lena)<-c("Year","AgeOrLength","value")
      #spread(Lena, AgeOrLength, value)
    }
        #From the Intercatch output.txt file by Jens  ##used this one
    {
      file_sel <- file.path( taf.data.path(), list.files(taf.data.path())[grepl("Catch",list.files(taf.data.path()))])
      read_the_text <- scan(file = file_sel, # if your data is in a text file, use the file argument
                            what = character(),
                            sep = "\n")  #looking for these lines in the txt file to start 

    #looking for these lines in the txt file to start 
      line<-"Mean Length at Age by Area "
      line2<-"For Period\tAll periods"
      #GET lena per area
      {lena_area<- read_the_text[c(grep(line, read_the_text)[5]:(grep(line, read_the_text)[5]+(13+4)))][-c(1:3)] #this year the readings go to age 13 

        #here is the header
        lena_header<-lena_area[1]
        lena_header <- strsplit(x = lena_header,
                                split = "\t")


        split_each_line_by_spaces <- strsplit(x = lena_area[-1],
                                              split = "\t")


        ages <- lapply(X = split_each_line_by_spaces,
                                FUN = `[[`,
                                i = 1)
        get_element_values <- lapply(X = split_each_line_by_spaces,
                                     FUN = `[`,
                                     i = (-1))
        required_result_as_character <- setNames(object = get_element_values,
                                                 nm = ages)

        df <- do.call("rbind", required_result_as_character)
        df<-as.data.frame(df)
        names(df) <-lena_header[[1]][-1]

        #make new len to correct length 
        max_age <- 15
        ages_orig <- 0:max_age
        ages_miss<-as.numeric(setdiff(ac(ages_orig),ac(ages)))

        # Data for ages 14 and 15
        miss_data <- matrix(NA, nrow = length(ages_miss), ncol = length(names(df)))
        miss_data <- as.data.frame(miss_data)
        names(miss_data) <- names(df)
        rownames(miss_data) <- ages_miss

        # Append the new data to the existing data frame
        lena_area <- rbind(df, miss_data)
        lena_area[is.na(lena_area)] <- 0
        lena_area$age <- ages_orig
        lena_area$year <- 2023
        lena_area<-lena_area[,c("age", "year", "27.4.b","27.4.c","27.7.d", "Total")]
        lena_area$Total <- round(as.numeric(lena_area$Total),1)
        lena_area[is.na(lena_area)] <- 0

      }
      #MERGE (to be done in 2025, becuase we have no previous record of these since the benchmark of 2024, would have to be compiled after the re-raising procedure, but currently unavailable)
        #   Lena_orig<-read.csv(paste0(getwd(),"./data/commercial/lena.csv"), sep =",")
        #   Lena_orig<-Lena_orig[,1:which(colnames(Lena_orig) == "X2021")] #till 2021
        #   Lena_new<-dplyr::left_join(Lena_orig,lena_area)
        #   names(Lena_new)<-c("Age", c(as.character(1995:year)))
        #   cols = c(1:length(Lena_new))
        #   Lena_new[,cols] = apply(Lena_new[,cols], 2, function(x) as.numeric(gsub( ",", ".",x)))
        #   Lena_new
    }
    write.csv(lena_area, "report/lena_area.csv", row.names=FALSE)

#- create lena_area.csv
# 
#     startYear <- 2000
#     endYear <- 2024
# 
#     tempVar <- as.data.frame(stk@m[as.character(1:10),as.character(startYear:endYear),,,'all'])
#     tempVar$age <- factor(tempVar$age, levels=c("1","2","3","4","5","6","7","8","9","10"))
# 
# 
#     png(paste(figPath, "mean_length_at_age.png", sep=""), width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)
# 
#     p=ggplot(tempVar, aes(x = year, y = data, color = age, group = age)) + geom_point() + geom_line() 
#     p = p + theme(text = element_text(size=10))+labs(x="Year",y="Length (cm)")  
#     p = p  + ggtitle("Mean length at age (cm)") + theme(plot.title = element_text(color="grey40", size=5, face="bold.italic", hjust=0.5))
#     p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8), legend.key = element_rect(fill = "white")) 
#     p = p + theme(plot.title = element_text(size = 12, face = "bold"))
#     p = p + theme(panel.background = element_rect(fill = "white")) 
#     p = p + theme(axis.line = element_line(color="grey40", size = 0.3))
#     print(p)
# 
#     dev.off()


# - create canum_area.csv
    {
      #Canum_area<-aggregate(round(CANUM, 1)~Year+AgeOrLength+Area,  NSHM_2021_catch_age, FUN=sum)
      #names(Canum_area)<-c("Year","AgeOrLength","Area","value")
      #tidyr::spread(Canum_area, Area, value)
      #canum_area
    }
    #From the Intercatch output.txt file by Jens  ##used this one
    {
      file_sel <- file.path( taf.data.path(), list.files(taf.data.path())[grepl("Catch",list.files(taf.data.path()))])
      read_the_text <- scan(file = file_sel, # if your data is in a text file, use the file argument
                            what = character(),
                            sep = "\n")
    
      #looking for these lines in the txt file to start 
      line<-"Catch Numbers at Age by Area "
      line2<-"For Period\tAll periods"
    
      #GET canum per area
      { canum_area<- read_the_text[c(grep(line, read_the_text)[5]:(grep(line, read_the_text)[5]+(13+4)))][-c(1:3)] #readings need to go up to age 15 but go to age 13 in year 2023
    
        #here is the header
        canum_header<-canum_area[1]
        canum_header <- strsplit(x = canum_header,
                                 split = "\t")


        split_each_line_by_spaces <- strsplit(x = canum_area[-1],
                                              split = "\t")

        ages <- lapply(X = split_each_line_by_spaces,
                                    FUN = `[[`,
                                    i = 1)

        get_element_values <- lapply(X = split_each_line_by_spaces,
                                     FUN = `[`,
                                     i = (-1))
        required_result_as_character <- setNames(object = get_element_values,
                                                 nm = ages)


        df <- do.call("rbind", required_result_as_character)
        df<-as.data.frame(df)
        names(df) <-canum_header[[1]][-1]

        #make new canum to correct length 
        max_age <- 15
        ages_orig <- 0:max_age
        ages_miss<-as.numeric(setdiff(ac(ages_orig),ac(ages)))

        # Data for ages 14 and 15
        miss_data <- matrix(NA, nrow = length(ages_miss), ncol = length(names(df)))
        miss_data <- as.data.frame(miss_data)
        names(miss_data) <- names(df)
        rownames(miss_data) <- ages_miss

        # Append the new data to the existing data frame
        canum_area <- rbind(df, miss_data)
        canum_area[is.na(canum_area)] <- 0
        canum_area$age <- ages_orig
        canum_area$year <- 2024
        canum_area<-canum_area[,c("age", "year", "27.4.b","27.4.c","27.7.d")]
      }
    
      #MERGE (to be done in 2025, becuase we have no previous record of these since the benchmark of 2024, would have to be compiled after the re-raising procedure, but currently unavailable)
        # canum_area_orig<-read.csv("./data/commercial/canum_area.csv", sep =";")
        # canum_area_orig<-canum_area_orig[1:length(canum_area_orig[(canum_area_orig$year%in% c(1998:year)),1]),]
        # canum_area_new<-dplyr::left_join(na.omit(canum_area_orig[canum_area_orig$year==1998,]),canum_area, by="age")
        # canum_area_new<-canum_area_new[,-c(2:7)]
        # #change column classes to numeric
        # canum_area_new[,c(1:7)]<- apply(canum_area_new[,c(1:7)], 2, function(x) as.numeric(x))
        # names(canum_area_new)[2]<-'year'
        # canum_area_new$year<-year
        # canum_area_new[is.na(canum_area_new[,])]<-0
        # names(canum_area_orig)<-names(canum_area_new)
        # canum_area_fin<-rbind(canum_area_orig, canum_area_new)
    
    }
    write.csv(canum_area, "report/canum_area.csv", row.names=FALSE)


# - create weca_area.csv
    #::manually from the catch table::#
    {#weca<-aggregate(LECA~Year+AgeOrLength,  NSHM_2021_catch_age, FUN=sum)
    #names(weca)<-c("Year","AgeOrLength","value")
    #spread(weca, AgeOrLength, value)
    }
        #From the Intercatch output.txt file by Jens  ##used this one
    {
    file_sel <- file.path( taf.data.path(), list.files(taf.data.path())[grepl("Catch",list.files(taf.data.path()))])
    read_the_text <- scan(file = file_sel, # if your data is in a text file, use the file argument
                            what = character(),
                            sep = "\n")  #looking for these lines in the txt file to start 
                        
    #looking for these lines in the txt file to start 
    line<-"Mean Weight at Age by Area "
    line2<-"For Period\tAll periods"
    #GET weca per area
    {weca_area<- read_the_text[c(grep(line, read_the_text)[5]:(grep(line, read_the_text)[5]+(13+4)))][-c(1:3)] #this year the readings go to age 13 
        
        #here is the header
        weca_header<-weca_area[1]
        weca_header <- strsplit(x = weca_header,
                                split = "\t")
        
        
        split_each_line_by_spaces <- strsplit(x = weca_area[-1],
                                            split = "\t")
        
        
        ages <- lapply(X = split_each_line_by_spaces,
                                FUN = `[[`,
                                i = 1)
        get_element_values <- lapply(X = split_each_line_by_spaces,
                                    FUN = `[`,
                                    i = (-1))
        required_result_as_character <- setNames(object = get_element_values,
                                                nm = ages)
        
        df <- do.call("rbind", required_result_as_character)
        df<-as.data.frame(df)
        names(df) <-weca_header[[1]][-1]
        
        #make new len to correct length 
        max_age <- 15
        ages_orig <- 0:max_age
        ages_miss<-as.numeric(setdiff(ac(ages_orig),ac(ages)))

        # Data for ages 14 and 15
        miss_data <- matrix(NA, nrow = length(ages_miss), ncol = length(names(df)))
        miss_data <- as.data.frame(miss_data)
        names(miss_data) <- names(df)
        rownames(miss_data) <- ages_miss

        # Append the new data to the existing data frame
        weca_area <- rbind(df, miss_data)
        weca_area[is.na(weca_area)] <- 0
        weca_area$age <- ages_orig
        weca_area$year <- 2024
        weca_area<-weca_area[,c("age", "year", "27.4.b","27.4.c","27.7.d", "Total")]
        weca_area$Total <- round(as.numeric(weca_area$Total),1)
        weca_area[is.na(weca_area)] <- 0
        
    }
    #MERGE (to be done in 2025, becuase we have no previous record of these since the benchmark of 2024, would have to be compiled after the re-raising procedure, but currently unavailable)
        #   weca_orig<-read.csv(paste0(getwd(),"./data/commercial/weca.csv"), sep =",")
        #   weca_orig<-weca_orig[,1:which(colnames(weca_orig) == "X2021")] #till 2021
        #   weca_new<-dplyr::left_join(weca_orig,weca_area)
        #   names(weca_new)<-c("Age", c(as.character(1995:year)))
        #   cols = c(1:length(weca_new))
        #   weca_new[,cols] = apply(weca_new[,cols], 2, function(x) as.numeric(gsub( ",", ".",x)))
        #   weca_new
    }
    write.csv(weca_area, "report/weca_area.csv", row.names=FALSE)

as.data.frame(NSHOM.tun[[2]])$data


# - create canum_area_quarter.csv

    year <- 2024

    file_sel <- file.path( taf.data.path(), list.files(taf.data.path())[grepl("Catch",list.files(taf.data.path()))])
    read_the_text <- scan(file = file_sel, # if your data is in a text file, use the file argument
                                what = character(),
                                sep = "\n")  #looking for these lines in the txt file to start 
        

    #check where the canum per quarter are 
    read_the_text[grep("Catch Numbers at Age by Area", read_the_text)[5]]

    read_the_text[grep("Catch Numbers at Age by Area", read_the_text)[5]:(grep("Catch Numbers at Age by Area", read_the_text)[5]+(13+4))] #up till age 13 this year

    quarters=c(1:5) #uptill 5 because there is another table that has combined quarters 

    ##canum @ age  per quarter
    for(iQuart in quarters){
    print(iQuart)
    
    Quart<- read_the_text[grep("Catch Numbers at Age by Area", read_the_text)[iQuart]:(grep("Catch Numbers at Age by Area", read_the_text)[iQuart]+(13+4))] #up till age 13 this year
    header<-Quart[c(1:4)]
    values<-Quart[-c(1:4)]
    
    table1_header<-header[4]
    table1_header <- strsplit(x = table1_header[[1]],
                                split = "\t")
    
    split_each_line_by_spaces <- strsplit(x = values,
                                            split = "\t")
    
    
    ages <- lapply(X = split_each_line_by_spaces,
                            FUN = `[[`,
                            i = 1)
    
    get_element_values <- lapply(X = split_each_line_by_spaces,
                                FUN = `[`,
                                i = (-1))
    
    required_result_as_character <- setNames(object = get_element_values,
                                            nm = ages)
    
    df <- do.call("rbind", required_result_as_character)
    df<-as.data.frame(df)
    df$age<-rownames(df)
    df<-cbind(df$age, df[,-length(df)])
    names(df)<-table1_header[[1]]

    df$Stock<-"hom.27.4bc7d"
    #turn to numeric
    cols = c(1:(length(df)-1))
    df[,cols] = apply(df[,cols], 2, function(x) as.numeric(as.character(x)))
    #devide by 1 000 to turn into per tonne
    cols = c(2:(length(df)-1))
    df[,cols] = apply(df[,cols], 2, function(x) round(x/(1000),2))
    #sum the rows
    df[length(df[,1])+1,cols] = apply(df[,cols], 2, function(x) sum(x))
    rownames(df)[length(df[,1])]<-"Sum"
    rownames(df)[length(df[,1])]<-"Sum"
    
    
    assign(value = df, x = paste0("NSHM_",year,"_canum_q", iQuart,"_table")) #for inspection when updating the graphs in the report
    
    write.csv(x = df, file = paste0("report/NSHM_",year,"_canum_q", iQuart,"_table.csv")  ,row.names=TRUE)
    }

# Calculate Historic TAC data
  hist_TAC        <- read.csv(paste0(taf.data.path(),"/hist_TAC.csv"), sep =",")
  hist_TAC        <- hist_TAC[hist_TAC$year <= year,]

  TAC_2024 <-
  c("Belgium",7,
  "Denmark",3141,
  "Germany",277,
  "Spain",58,
  "France",261,
  "Ireland",198,
  "Netherlands",1891,
  "Portugal",8,
  "Sweden",75,
  "Norway",0,
  "United Kingdom", 669,
  "all",9730)

  # Convert to a matrix with two columns
  TAC_2024_matrix <- matrix(TAC_2024, ncol = 2, byrow = TRUE)

  # Assign column names
  colnames(TAC_2024_matrix) <- c("country", "TAC")
  TAC_2024_matrix <- as.data.frame(TAC_2024_matrix)
  TAC_2024_matrix$year <-2024

  TAC_2024_matrix$advice_TAC <- NA
  TAC_2024_matrix$advice_TAC[dim(TAC_2024_matrix)[1]]<- 9730

  #Was the change more than the buffer and was the cap therefore reached?
  TAC_2024_matrix$buffer <- NA
  TAC_2024_matrix$buffer[dim(TAC_2024_matrix)[1]]<- 0

  #In which direction did the advice change 
  TAC_2024_matrix$cap <- NA
  TAC_2024_matrix$cap[dim(TAC_2024_matrix)[1]]<- "Up"

  df_TAC <- rbind(hist_TAC, TAC_2024_matrix[,names(hist_TAC)])

# - Calculate total catches and catches by category and area 
 {
  file_sel <- file.path( taf.data.path(), list.files(taf.data.path())[grepl("Catch",list.files(taf.data.path()))])
  read_the_text <- scan(file = file_sel, # if your data is in a text file, use the file argument
                        what = character(),
                        sep = "\n")


  read_the_text[(grep("TABLE", read_the_text)[1]-10): grep("TABLE", read_the_text)[1] ]
  
  read_the_text[grep("TABLE", read_the_text)[1]]
  read_the_text[grep("TABLE", read_the_text)[2]]

  ##Table 1
  table1<- read_the_text[c(grep("TABLE", read_the_text)[1]:grep("TABLE", read_the_text)[2]-1)]
    table1.1<-table1[grep("hom.27.4bc7d",table1)]

    #here is the header
    table1_header<-table1[5]
    table1_header <- strsplit(x = table1_header[[1]],
                              split = "\t")


    split_each_line_by_spaces <- strsplit(x = table1.1,
                                          split = "\t")

    ages <- lapply(X = split_each_line_by_spaces,
                                FUN = `[[`,
                                i = 1)
    get_element_values <- lapply(X = split_each_line_by_spaces,
                                 FUN = `[`,
                                 i = (-1))
    required_result_as_character <- setNames(object = get_element_values,
                                             nm = ages)

    df <- do.call("rbind", required_result_as_character)
    df<-as.data.frame(df)
    rownames(df) <- NULL
    df$Stock<-"hom.27.4bc7d"
    df<-cbind(df$Stock, df[,-length(df)])
    names(df)<-table1_header[[1]]
  }
  head(df)
  names(df)

  catch_raw_tab <- df 
  catch_raw_tab[catch_raw_tab$Fleet == "TrawlPlus",]

  # Group some categories or change names
  df$CatchCategory=plyr::revalue(df$CatchCategory, c("BMS landing"="Landings", "Logbook Registered Discard"="Discards"))
  
  # Select data of interest
  choice=c("Country","Year","CatchCategory","Area","Season","CATON")
  select_df=df[,names(df) %in% choice]
  colnames(select_df)=c("country","year","category","area","season","catch")
  select_df$catch <- as.numeric(select_df$catch)
  ss <- select_df %>%
    group_by(year, category) %>%
    summarise(catch = sum(catch, na.rm = TRUE)/1000)
  #lapply(select_df,class)

  #sum up UK catches and replace main data where UK data is split up
  UKcatches         <- aggregate(catch ~ category + area + season, data=select_df[select_df$country %in% c("UK (England)","UK(Northern Ireland)","UK(Scotland)"),], FUN=sum)
  UKcatches$country <- "UK"
  UKcatches$year    <- 2024
  UKcatches$season[UKcatches$season == 2024] <- "all"
  UKcatches         <- UKcatches[,c("country","year","category","area","season","catch")]
  select_df              <- subset(select_df, !country %in% c("UK (England)","UK(Northern Ireland)","UK(Scotland)"))
  select_df              <- rbind(select_df,UKcatches)

  #catch in tonnes
  select_df$catch=select_df$catch/1000
  
  #sum of product comparison
  an(computeCatch(NSHOM)[,ac(2024)])/sum(select_df$catch)


  ####################  catch against TAC for current year  #########################
    endYear <-2024

    cl        <- 1.5
    ca        <- 1.2
    fam       <- ""
    fonts     <- 2
    parmar    <- rep(0.4,4)
    paroma    <- (c(4,4,1,1)+0.1)
    mtextline <- 4
    ltextcex  <- 1.2
    figtype   <- "png"

    historical_TAC  <-   df_TAC
    historical_TAC$country[historical_TAC$country == 'UK'] <- "United Kingdom"
    uniqueCountries <- as.character(unique(historical_TAC$country))
    historical_TAC$TAC <- an(historical_TAC$TAC)

    outArray            <- array(NA,dim=c(2,length(uniqueCountries)))
    rownames(outArray)  <- c('TAC','TAC_use')
    colnames(outArray)  <- uniqueCountries

    for(i in uniqueCountries){
      if(i == 'United Kingdom'){
        iUK <- c('UK (England)','UK(Scotland)', 'UK')
        outArray['TAC',i]     <- subset(historical_TAC, year == endYear & country == i)$TAC
        outArray['TAC_use',i] <- sum(subset(select_df, country %in% c(iUK[1], iUK[2], iUK[3]))$catch)
      } else if((i == 'all')){
        outArray['TAC',i]     <- subset(historical_TAC, year == endYear & country == i)$TAC
        outArray['TAC_use',i] <- sum(select_df$catch)
        
      } else if(is.na(match(i,select_df$country))){
        outArray['TAC',i]     <- subset(historical_TAC, year == endYear & country == i)$TAC
        outArray['TAC_use',i] <- 0
      } else{
        outArray['TAC',i]     <- subset(historical_TAC, year == endYear & country == i)$TAC
        outArray['TAC_use',i] <- sum(subset(select_df, country == i)$catch)
      }
    }

    colnames(outArray)[colnames(outArray) == "Netherlands"] <- "NL"
    colnames(outArray)[colnames(outArray) == "United Kingdom"] <- "UK" 

    windows()

    barplot(outArray,beside=T,col = c('blue','red'),las=2,main=paste0(endYear,' TAC utilisation'))
    legend("topright", 
          legend = c("TAC", "TAC use"), 
          fill = c("blue", "red"))

    savePlot("report/Catch_TAC_country.png",type=figtype)

    #tac use in percentages for 2023
    outArray[,colnames(outArray) == "all"][2]/outArray[,colnames(outArray) == "all"][1] *100  


  ####################  historical TAC and utilisation (since 2000)  #########################
    hist_TAC[hist_TAC$country == 'all',]
    file_sel <- file.path( taf.data.path(), "all_serie_NSHOM_2023.csv")
    all_catches <- read.table(file_sel,sep=',', header = TRUE, check.names=FALSE)
    all_catches$rel_d<-all_catches$`7.d`/all_catches$total*100
    all_catches$rel_bc<-all_catches$`4.bc`/all_catches$total*100

    TAC_util_df<- left_join(all_catches, hist_TAC[hist_TAC$country == 'all',],by = "year")
    

    xrange <- c(2000,endYear)
    yrange <- range(pretty(c(0,max(TAC_util_df$total,na.rm=T))))

    TAC_util_df <- TAC_util_df  %>%  filter(year %in% xrange[1]:xrange[2])
    TAC_util_df$util_perc <- TAC_util_df$total/TAC_util_df$TAC*100
    
    #uptake from 2016 onward
    range(TAC_util_df[TAC_util_df$year %in% c(2016:2024),"util_perc"])
    mean(TAC_util_df[TAC_util_df$year %in% c(2016:2024),"util_perc"])

    #catch range 
    range(TAC_util_df[TAC_util_df$year %in% c(2002:2010),"total"])
    

    windows()

      par(oma=paroma,yaxs="i")
      plot(0,0,col="white",xlim=xrange,ylim=yrange,xlab="Years",ylab="",
          cex.lab=cl,cex.axis=ca,font.lab=fonts,font=fonts,las=1)
      mtext(side=2,line=mtextline,text="Catch / TAC (tonnes)",cex=cl,font=fonts)
      rect((2000:(endYear))-0.5,0,(2000:(endYear))+0.5,TAC_util_df[,'total'],col="grey",lwd=2)
      lines(x=rep(2000:(endYear),each=2)+c(-0.5,0.5),y=rep(TAC_util_df[,'TAC'],each=2),lwd=4)
      legend("bottom",legend=c("Catch","TAC"),lwd=c(1,5),lty=c(NA,1),pch=c(22,NA),col="black",bg="white",pt.cex=c(2),box.lty=0)
      title(main="Catch and TAC")

    savePlot("report/historic_Catch_TAC.png",type=figtype)    
  
  ###################### Distribution of catches by season and area  ###################################

    png("report/catch_quarter_area.png", width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

      p=ggplot(select_df,aes(season, fill=area, weight=catch))
      p= p + geom_bar(position="stack", width=0.75)
      p= p + scale_fill_brewer(palette="Set1")
      p = p + theme(text = element_text(size=10))+labs(x="Quarter",y="Catch (tons)")  
      p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
      p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
      p = p + theme(panel.background = element_rect(fill = "white")) 
      p = p  + ggtitle("Catch by quarter and area") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
      p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
      print(p)

    dev.off()
  ###################### Distribution of catches by category and area  ###################################

    png("report/catch_area_category.png", width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

      p=ggplot(select_df,aes(area, fill=category, weight=catch))
      p= p + geom_bar(position="stack", width=0.75)
      p= p + scale_fill_brewer(palette="Set1")
      p = p + theme(text = element_text(size=10))+labs(x="Area",y="Catch (tons)")  
      p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
      p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
      p = p + theme(panel.background = element_rect(fill = "white")) 
      p = p  + ggtitle("Catch by season and category") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
      p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
      print(p)

    dev.off()

  #################### Distribution of catches country  ###################################

    png("report/catch_country_quarter.png", width=1500, heigh= 1500, units="px", pointsize=7, bg="white", res=300)

      p=ggplot(select_df,aes(country, fill=season, weight=catch))
      p= p + geom_bar(position="stack", width=0.75)
      p= p + scale_fill_brewer(palette="Set1", name="Quarter")
      p = p + theme(text = element_text(size=10))+labs(x="Country",y="Catch (tons)")  
      p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
      p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8,angle = 90)) + 
        theme(axis.text.y = element_text(size = 6.5)) 
      p = p + theme(panel.background = element_rect(fill = "white")) 
      p = p  + ggtitle("Catch by country and quarter") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
      p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
      print(p)

    dev.off()

  ###################### Different figures for distribution of catches by area, season, category and area  #################################

    png("report/catch_category_country.png", width=1500, heigh= 1150, units="px", pointsize=7, bg="white", res=300)

      p=ggplot(select_df,aes(category, fill=country, weight=catch))
      p= p + geom_bar(position="stack", width=0.75)
      p= p + scale_fill_brewer(palette="Set3")
      p = p + theme(text = element_text(size=10))+labs(x="Category",y="Catch (tons)")  
      p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
      p = p + theme(axis.text.x = element_text(hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
      p = p + theme(panel.background = element_rect(fill = "white")) 
      p = p  + ggtitle("Catch category per country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
      p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
      print(p)

    dev.off()

    png("report/catch_area_quarter_category_country.png", width=2500, heigh= 1150, units="px", pointsize=5, bg="white", res=300)

      p=ggplot(select_df,aes(category, fill=country, weight=catch))
      p= p + geom_bar(position="stack", width=0.75) + facet_wrap(~ area + season, nrow=1)
      p= p + scale_fill_brewer(palette="Set3")
      p = p + theme(text = element_text(size=10))+labs(x="Category",y="Catch (tons)")  
      p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
      p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
      p = p + theme(panel.background = element_rect(fill = "white")) 
      p = p  + ggtitle("Catch by area, quarter, category and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
      p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
      print(p)

    dev.off()

    png("report/catch_area_category_country.png", width=2500, heigh= 1150, units="px", pointsize=5, bg="white", res=300)

      p=ggplot(select_df,aes(category, fill=country, weight=catch))
      p= p + geom_bar(position="stack", width=0.75) + facet_wrap(~ area, nrow=1)
      p= p + scale_fill_brewer(palette="Set3")
      p = p + theme(text = element_text(size=10))+labs(x="Category",y="Catch (tons)")  
      p = p + theme(legend.title=element_blank(), legend.text = element_text(size = 8)) 
      p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 8)) + theme(axis.text.y = element_text(size = 6.5)) 
      p = p + theme(panel.background = element_rect(fill = "white")) 
      p = p  + ggtitle("Catch by area, category and country") + theme(plot.title = element_text(color="grey40", size=12, face="bold.italic", hjust=0.5))
      p = p + theme(axis.line.x = element_line(size = 0.5, colour = "grey30"),axis.line.y = element_line(size = 0.5, colour = "grey30"))
      print(p)

    dev.off()

  # - create caton_area_cat_2023
    # Get all areas and catch categories and fleets
    areaUnique             <- as.character(unique(catch_raw_tab$Area))
    catchCategoryUniqueAll <- as.character(unique(catch_raw_tab$CatchCategory))
    CountryUnique                <- as.character(unique(catch_raw_tab$Country))
    FleetUnique                <- as.character(unique(catch_raw_tab$Fleet))

    # Create array to store the data in
    outArray           <- array(NA, dim=c(length(areaUnique),length(catchCategoryUniqueAll)))
    colnames(outArray) <- catchCategoryUniqueAll
    rownames(outArray) <- areaUnique
    outArray

    # Loop through the areas to calculate the total catch per category and per area
    for(area in areaUnique){
      idxArea             <- match(area,areaUnique)
      sub_catch_raw_tab   <- subset(catch_raw_tab,Area == area)
      catchCategoryUnique <- as.character(unique(sub_catch_raw_tab$CatchCategory))
      for(catchCategory in catchCategoryUnique){
        idxCatchCategory <- match(catchCategory,catchCategoryUniqueAll)
        outTemp          <- subset(sub_catch_raw_tab,CatchCategory == catchCategory)
        outTemp          <- subset(outTemp,CatchCategory == catchCategory)
        outArray[idxArea,idxCatchCategory] <- sum(as.numeric(outTemp$CATON))
      }
    }
    caton_area_cat_2023<-outArray/1000 #convert from kg to tonnes
    write.csv(caton_area_cat_2023, file=paste0("report/caton_area_cat_",year,".csv")) 
    
    caton_area_cat_2023[is.na(caton_area_cat_2023)] <- 0
    caton_area_cat_2023 <- as.data.frame(caton_area_cat_2023)
    caton_area_cat_2023 <- caton_area_cat_2023 %>%
      rowwise() %>%
      mutate(catch = sum(c_across(Landings:`BMS landing`)))
    caton_area_cat_2023 <- as.data.frame(caton_area_cat_2023)
    rownames(caton_area_cat_2023) <- rownames(outArray)
  
  # - create catch_area_country

  table=as.data.frame(tapply(round(select_df$catch,digits=0), list(select_df$area, select_df$country), sum))
  table$area=rownames(table)
  table=table[,c(dim(table)[2],1:dim(table)[2]-1)]
  table[is.na(table)]=0

  write.table(table,'report/catch_area_country.csv',sep=',',col.names=TRUE, row.names = FALSE)

  ### PDF
  pdf(file ="report/table_catch_area_category_country.pdf")

  grid.table(table, rows=NULL, cols=colnames(table), theme=ttheme_default(base_size = 5))

  dev.off() 


  ##########################
  ###EXTRA for catchcategories per country
  ##########################
  # Create array to store the data in
  outArray           <- array(NA, dim=c(length(CountryUnique),length(catchCategoryUniqueAll)))
  colnames(outArray) <- catchCategoryUniqueAll
  rownames(outArray) <- CountryUnique
  outArray
  
  # Loop through the countries to calculate the total catch per category and per country
  for(iCountry in CountryUnique){
    idxiCountry         <- match(iCountry,CountryUnique)
    sub_catch_raw_tab   <- subset(catch_raw_tab,Country == iCountry)
    catchCategoryUnique <- as.character(unique(sub_catch_raw_tab$CatchCategory))
    for(catchCategory in catchCategoryUnique){
      idxCatchCategory <- match(catchCategory,catchCategoryUniqueAll)
      outTemp          <- subset(sub_catch_raw_tab,CatchCategory == catchCategory)
      outTemp          <- subset(outTemp,CatchCategory == catchCategory)
      outArray[idxiCountry,idxCatchCategory] <- sum(as.numeric(outTemp$CATON), na.rm =TRUE)
    }
  }
  outArray
  
  # - discard ratio current year
  

  ##########################
  ###EXTRA for catchcategories per fleet 
  ##########################
  
  outArray           <- array(NA, dim=c(length(FleetUnique),length(catchCategoryUniqueAll)))
  colnames(outArray) <- catchCategoryUniqueAll
  rownames(outArray) <- FleetUnique
  outArray
  
  # Loop through the countries to calculate the total catch per category and per country
  for(iFleet in FleetUnique){
    idxiFleet         <- match(iFleet,FleetUnique)
    sub_catch_raw_tab   <- subset(catch_raw_tab,Fleet == iFleet)
    catchCategoryUnique <- as.character(unique(sub_catch_raw_tab$CatchCategory))
    for(catchCategory in catchCategoryUnique){
      idxCatchCategory <- match(catchCategory,catchCategoryUniqueAll)
      outTemp          <- subset(sub_catch_raw_tab,CatchCategory == catchCategory)
      outTemp          <- subset(outTemp,CatchCategory == catchCategory)
      outArray[idxiFleet,idxCatchCategory] <- sum(as.numeric(outTemp$CATON), na.rm =TRUE)
    }
  }
  outArray<-as.data.frame(outArray)
  
  outArray$Fleet<-rownames(outArray)
  outArray <- outArray %>%
    mutate(combined_landings = rowSums(select(., Landings, `BMS landing`), na.rm = TRUE))

  #split the fleet defs
  outArray <- outArray %>%
    tidyr::separate(Fleet, into = c("Gear", "Species"), sep = "_", extra = "drop")
  
  #now the metiers have to be assigned to a particular category of fishery: 
  # either "Bottom trawl", "Pelagic trawl", "Seine", or "Undefined gears"
  
  #now have a look of what we can base the assignment on
  unique(outArray$Gear)
    
  #GEAR TYPES
  #############
  #OTM = Midwater otter trawl
  #OTB = Bottom otter trawl
  #GTR = trammel nets
  #GNS = set gillnet
  #SSC = flyshoot seine scottish
  #TBB = beam trawl
  #FPO = Pots and traps
  #LHP = Handline and pole lines (hand operated)
  #LLS = Set long lines
  #TrawlPus == ??
  
  unique(outArray$Species)
  #SPECIES TYPES
  #############
  
  #CRU = crustacians 
  #DEF = Demersal fish
  #SPF = Small pelagics
  #MIS = miscellaneous
  #FIF = Finfish
  #MCD = 
  
  outArray_backup<-outArray 
  #do te actual assignment::
  unique(paste0(outArray$Gear, "_", outArray$Species))
  
  outArray<- outArray %>%  
    mutate(
      assignment = case_when(
        Gear %in% c("OTB", "TBB")  ~ "Bottom",
        Gear %in% c("OTM")  ~ "Pelagic",
        Gear %in% c("SSC")  ~ "Seine",
        #Gear %in% c("GNS", "MIS", "FPO", "GTR" )  ~ "Other",
        TRUE  ~ "Undefined"  # Default case if none of the above conditions match
      )
    )
  
  unique(outArray$Gear[outArray$assignment=="Undefined"])
  
  #landings
  relative_landings_by_fleets<-aggregate(combined_landings~ assignment, data= outArray,FUN=sum)
  relative_landings_by_fleets$combined_landings_t <- relative_landings_by_fleets$combined_landings/1000
  relative_landings_by_fleets$portion<-round(relative_landings_by_fleets$combined_landings/sum(relative_landings_by_fleets$combined_landings)*100,1)
  relative_landings_by_fleets
  sum(relative_landings_by_fleets$combined_landings_t)

  #discards 
  outArray <- outArray  %>% rowwise()  %>% 
                              mutate(combined_disc = sum(`Logbook Registered Discard`,
                                          Discards,`BMS landing`, na.rm = TRUE)
                                    )
  as.data.frame(outArray)                                  
  relative_discards_by_fleets <-aggregate(combined_disc ~ assignment, data= outArray,FUN=sum)
  relative_discards_by_fleets$combined_disc_t <- relative_discards_by_fleets$combined_disc/1000
  relative_discards_by_fleets$portion<-round(relative_discards_by_fleets$combined_disc/sum(relative_discards_by_fleets$combined_disc)*100,1)
  relative_discards_by_fleets
  sum(relative_discards_by_fleets$combined_disc_t)




  
#Table 2
  {table2<- read_the_text[c(grep("TABLE", read_the_text)[2]:length(read_the_text))]
    table2.1<-table2[grep("hom.27.4bc7d",table2)]

    #here is the header
    table2_header<-table2[4]
    table2_header <- strsplit(x = table2_header,
                              split = "\t")


    split_each_line_by_spaces <- strsplit(x = table2.1,
                                          split = "\t")

    ages <- lapply(X = split_each_line_by_spaces,
                                FUN = `[[`,
                                i = 1)
    get_element_values <- lapply(X = split_each_line_by_spaces,
                                 FUN = `[`,
                                 i = (-1))
    required_result_as_character <- setNames(object = get_element_values,
                                             nm = ages)

    df <- do.call("rbind", required_result_as_character)
    df<-as.data.frame(df)
    rownames(df) <- NULL
    df$Stock<-"hom.27.4bc7d"
    df<-cbind(df$Stock, df[,-length(df)])
    names(df)<-table2_header[[1]]
  }
  head(df)
  names(df)





