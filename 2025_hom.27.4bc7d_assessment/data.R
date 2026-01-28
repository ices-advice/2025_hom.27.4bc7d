#- Data NSHOM
rm(list=ls())

#library(PFA)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(xlsx)
library(readxl)
library(FLCore)
library(icesTAF)


mkdir("data")

#- read in benchmark series of CAA and WAA
load("boot/initial/data/input_2024_assessment.Rdata", verbose = TRUE)
CAAo <- CAA
WAAo <- WAA

#converyt new WGWIDE CAA and WAA txt files to 
{ #canum<-"canum.txt" 
  file_sel <- taf.data.path("canum.txt")
  
  read_the_text <- scan(file = file_sel, # if your data is in a text file, use the file argument
                        what = character(),
                        sep = "\n")
  #extract year
  year_of_int <- (strsplit(x = read_the_text[4],
                                        split = " ")[[1]][1])
  
  #extract CAA
  split_each_line_by_spaces <- strsplit(x = read_the_text[8],
                                        split = " ")
  CAA_vals<-split_each_line_by_spaces[[1]][grep("[0-9]",split_each_line_by_spaces[[1]])]
  
  #extract age range
  age_in_file <- strsplit(x = read_the_text[5],
                                        split = " ")
  age_vals <- age_in_file[[1]][grep("[0-9]",age_in_file[[1]])]
  age_vals <- c(as.numeric(age_vals[1]):as.numeric(age_vals[2]))
                                                                             
  canum_new<-as.data.frame(as.numeric(gsub(",", "",CAA_vals)))
  canum_new<-cbind(age_vals,canum_new)
  names(canum_new)<-c("Age",year_of_int)

  canum_new[,2]<-round(canum_new[,2]/1000, 2) #in thousands!
  
  
  #grab original file and append the data there
  Canum_orig<-CAAo
  #make new canum to correct length 
  ages_orig <- 0:(dim(Canum_orig)[2]-1)
  ages_miss<-as.numeric(setdiff(ac(ages_orig),ac(age_vals)))

  # Data for ages 14 and 15
  miss_data <- data.frame(
      Age = ages_miss,
      newyear = c(rep(NA, length(ages_miss)))
    )

  names(miss_data) <- names(canum_new)

  # Append the new data to the existing data frame
  canum_new_full <- rbind(canum_new, miss_data)
   
  #subset for year_of_int-1 jsut to be sure 
  Canum_orig<-Canum_orig[1:which(rownames(Canum_orig) == an(year_of_int)-1),] #till previous year
  
  # now reshape canum_new_full so it can be appended to Canum_orig
  # Reshape canum_new_full to wide format
  new_row <- t(canum_new_full[,2])
  colnames(new_row) <- canum_new_full$Age
  rownames(new_row) <- year_of_int
  
  #do the bind
  Canum_new <- rbind(Canum_orig, new_row)
  CAA <- Canum_new
}


{ #weca<-"weca.txt"  
 file_sel <- file.path( taf.data.path(), list.files(taf.data.path())[grepl("weca.txt",list.files(taf.data.path()))])
  
  read_the_text <- scan(file = file_sel, # if your data is in a text file, use the file argument
                        what = character(),
                        sep = "\n")

  #extract year
  year_of_int <- (strsplit(x = read_the_text[4],
                                        split = " ")[[1]][1])
  
  #extract WAA
  split_each_line_by_spaces <- strsplit(x = read_the_text[8],
                                        split = " ")
  WAA_vals<-split_each_line_by_spaces[[1]][grep("[0-9]",split_each_line_by_spaces[[1]])]
  
  #extract age range
  age_in_file <- strsplit(x = read_the_text[5],
                                        split = " ")
  age_vals <- age_in_file[[1]][grep("[0-9]",age_in_file[[1]])]
  age_vals <- c(as.numeric(age_vals[1]):as.numeric(age_vals[2]))

 
  weca_new<-as.data.frame(as.numeric(gsub(",", "",WAA_vals)))
  weca_new<-cbind(age_vals,weca_new)
  names(weca_new)<-c("Age",year_of_int)

  weca_new[,2]<-round(weca_new[,2]/1000, 3) #Convert to kg
  weca_new[weca_new[,2]==0,2] <- NA
  
  #grab original file and append the data there
  Weca_orig<-WAAo
  #make new canum to correct length 
  ages_orig <- 0:(dim(Weca_orig)[2]-1)
  ages_miss<-as.numeric(setdiff(ac(ages_orig),ac(age_vals)))

  # Data for ages 14 and 15
  miss_data <- data.frame(
      Age = ages_miss,
      newyear = c(rep(NA, length(ages_miss)))
    )

  names(miss_data) <- names(weca_new)

  # Append the new data to the existing data frame
  weca_new_full <- rbind(weca_new, miss_data)

  #subset for year_of_int-1 jsut to be sure 
  Weca_orig<-Weca_orig[1:which(rownames(Weca_orig) == an(year_of_int)-1),] #till previous year
  
  # now reshape weca so it can be appended to Weca_orig
  # Reshape weca_new_full to wide format
  new_row <- t(weca_new_full[,2])
  colnames(new_row) <- weca_new_full$Age
  rownames(new_row) <- year_of_int
  
  #do the bind
  Weca_new <- rbind(Weca_orig, new_row)

  #- Fill in missing data in weight at age
  for(iAge in colnames(Weca_new)){
    for(idx in which(is.na(Weca_new[,ac(iAge)])))
  		Weca_new[idx,ac(iAge)] <- mean(Weca_new[idx+c(-1,1),ac(iAge)],na.rm=T)
  	if(length(which(is.na(Weca_new[,ac(iAge)])))>0)
  		Weca_new[which(is.na(Weca_new[,ac(iAge)])),ac(iAge)] <- mean(Weca_new[,ac(iAge)],na.rm=T)
  }
  WAA <- Weca_new
}


#- Load the survey index data
  load(taf.data.path("delta-LN_SSB.Rda"), verbose = TRUE)
  # load(taf.data.path("commercial_CPUE.Rda"), verbose = TRUE)
  
  com_CPUE <- data.frame(
    Year = 2017:2024,
    Estimate = c(494.2, 493.3, 448.2, 407.0, 325.1, 258.9, 219.7, 205.9),
    Upper = c(660.2, 635.3, 585.6, 512.9, 401.8, 326.2, 273.1, 264.7),
    Lower = c(369.9, 383.0, 343.0, 322.9, 263.0, 205.5, 176.7, 160.2)
  )
  
  save(com_CPUE, file = paste0(taf.data.path(), "/commercial_CPUE.rda"))
    
#- Convert data to FLR objects
  dmns 			<- list(age="all",year=rownames(SI.dln_SSB$idx),unit="unique",season="all",area="unique",iter=1)
  IBTS 			<- FLIndex(index=FLQuant(NA,dimnames=dmns))
  IBTS@index[] 	<- c(SI.dln_SSB$idx[,1])
  IBTS@range[c("startf","endf")] <- c(0.6,0.8)
  IBTS@name 		<- "IBTS Q3_Q4"
  IBTS@type 		<-"biomass"
  
  dmns 			<- list(age="all",year=2017:2024,unit="unique",season="all",area="unique",iter=1)
  CPUE 			<- FLIndex(index=FLQuant(NA,dimnames=dmns))
  CPUE@index[] 	<- com_CPUE$Estimate
  CPUE@range[c("startf","endf")] <- c(1/12)
  CPUE@name 		<- "PFA standardized CPUE"
  CPUE@type 		<-"biomass"

#- Get the M vector and Mat vector
    natmort_WHOM 		<- matrix(c(1.07,0.66,0.44,0.37,0.32,0.29,0.27,0.25,0.24,0.23,0.22,0.21,0.2,0.2,0.19,0.18),
     						    nrow=1,ncol=length(colnames(CAA)),dimnames=list(year="fixed",age=colnames(CAA)))

    # natmort_NSHOM 	<- matrix(c(0.41,0.33,0.28,0.25,0.23,0.21,0.19,0.18,0.17,0.17,0.16,0.15,0.15,0.15,0.14,0.14),
    #                     nrow=1,ncol=length(0:15),dimnames=list(year="fixed",age=0:15))

    natmort <-  natmort_WHOM 

    mat 			<- matrix(c(0.013137249,0.075033566,0.356599205,0.623250486,0.795379904,0.885876324,0.932668201,0.955832881,0.969666782,0.978211714,0.985787247,0.989482214,0.992577436,0.99443712,0.996233947,0.997640622),
    							nrow=1,ncol=length(colnames(CAA)),dimnames=list(year="fixed",age=colnames(CAA)))


    save(CAA,WAA,IBTS,CPUE,natmort,mat, 
         file="data/input.RData")#
