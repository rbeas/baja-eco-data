########################################################################
### Output SAS structured csv file for ANOVA (Reef Check fish data)

### Written by C. Boch; May 20, 2014;
### F. Micheli Laboratory,  Hopkins Marine Station, Stanford University

########################################################################
rm(list=ls())

### Relevant packages and scripts
require(reshape)
require(plyr)
require(lattice)
require(doBy)
require(gsubfn)

source("~/Desktop/NSF_CHN/Ecology/ReefCheck/R_scripts/0002_All_Baja_Reef_Check_Invertebrate_Data_Raw_Clean_Up.R") # This script reads in all the Reef Check Fish data and outputs a single dataframe called AllBajaFishes

source("~/Desktop/R_Scripts/StatSum.R") #  This script calls the summarySE function which calculates the mean, number of samples, sd, se, and ci

########################################################################

NDatos <- subset(AllBajaInvertebrates, Site == "IslaNatividad")

########################################################################

########################################################################

GS.df <- split(NDatos, f=NDatos$Genusspecies) ### Split the data by species

Totalcountsbyspecies <- lapply(GS.df, function(k){sum(k$TotalCounts)})

print(str(Totalcountsbyspecies)) # a table of the total counts over the 9 yrs by species

########################################################################
### Summary statistics by species and by year ###

SumGSbyYear.df <- lapply(GS.df, function(i){aggregate(i$TotalCounts, list(Year = i$Year, Transect=i$TransectNumber, Location=i$Location, Zone=i$Zone), sum)})  #

SumGSbyTransect.df <- lapply(SumGSbyYear.df, function(f){aggregate(f$x, list(Transect=f$Transect, Year=f$Year, Location=f$Location, Zone=f$Zone), sum)})
 
FinalSum <- do.call(rbind, SumGSbyTransect.df) # when this is called, all the separate df's are binded by rows with the sub df's name as the temporary row names--in this case, the Genusspecies

FinalSum$Genusspecies <-row.names(FinalSum) # make the Genusspecies the row names or a variable

FinalSum <- FinalSum[, c(6, 2, 3, 1, 4, 5)]

names(FinalSum)[6]<-"TotalAbundance"

row.names(FinalSum) <- NULL

FinalSum$Genusspecies <-gsub("\\.0", "", FinalSum$Genusspecies)# remove the tags after Genusspecies; a result of row names
FinalSum$Genusspecies <-gsub("\\.1", "", FinalSum$Genusspecies) 
FinalSum$Genusspecies <-gsub("\\.2", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("\\.3", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("\\.4", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("\\.5", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("\\.6", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("\\.7", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("\\.8", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("\\.9", "", FinalSum$Genusspecies)

FinalSum$Genusspecies <-gsub("0", "", FinalSum$Genusspecies)# remove the tags after Genusspecies
FinalSum$Genusspecies <-gsub("1", "", FinalSum$Genusspecies) 
FinalSum$Genusspecies <-gsub("2", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("3", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("4", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("5", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("6", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("7", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("8", "", FinalSum$Genusspecies)
FinalSum$Genusspecies <-gsub("9", "", FinalSum$Genusspecies)

###################################

### Remove data poor observations #####
### data poor ####
### output to reflect species on the winners and losers figure ####


FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Cancersp" | is.na(Genusspecies)), ]) # remove rows with the data Cancersp

FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Cypraeaspadicea" | is.na(Genusspecies)), ]) # remove rows with Cypraeaspadicea

FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Gorgoniansp" | is.na(Genusspecies)), ]) # remove rows with Gorgoniansp

FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Haliotiscracherodii" | is.na(Genusspecies)), ]) # remove rows with Haliotiscracherodii

FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Haliotisrufescens" | is.na(Genusspecies)), ]) # remove rows with Haliotisrufescens

FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Haliotissorenseni" | is.na(Genusspecies)), ]) # remove rows with Haliotissorenseni

FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Loxorhyncusgrandis" | is.na(Genusspecies)), ]) # remove rows with Loxorhyncusgrandis

FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Octopussp" | is.na(Genusspecies)), ]) # remove rows with Octopussp

FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Panulirusinterruptus" | is.na(Genusspecies)), ]) # remove rows with Panulirusinterruptus; this species is analyzed separately

FinalSum <- with(FinalSum, FinalSum[!(Genusspecies == "Sunstarsp" | is.na(Genusspecies)), ]) # remove rows with Mycteropercajordani


###################################################################################################
### split the Year, Site, TransectNumber, Zone subset

YSTZ <- subset(FinalSum, Genusspecies == "Anemonesp") # any species will do; we just want the YSTZ columns

YSTZ <- YSTZ[, c("Year", "Location", "Transect", "Zone")] # these are the desired columns

Period<-vector(length=length(YSTZ$Year))

for (i in 1:length(YSTZ$Year)){
	Period[i] <- if (YSTZ$Year[i] <= 2008) "Before" else if (YSTZ$Year[i] >= 2009) "After"}

Datasplit.df <- split(FinalSum, f= FinalSum$Genusspecies) # split the data by GS

TotAbun <- lapply(Datasplit.df, function(i){i[[6]]}) # retrieve the Totalabundance count for each species from the separate dataframes

TotAbunbySpecies <- do.call(cbind, TotAbun) # put all the abundance data in column format

SASoutput <- cbind(YSTZ, Period, TotAbunbySpecies)

print(SASoutput)

write.table(SASoutput, "/Users/boch1976/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/Natividad_Reef_Check_Invertebrates_data_2006_2014_SASoutput_winners_losers.csv", sep=",", col.names=T, row.names=F)




