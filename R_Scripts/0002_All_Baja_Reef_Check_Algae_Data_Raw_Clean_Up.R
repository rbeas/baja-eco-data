########### Reef Check Algae Data Clean Up Process ###########
# Written by: C. A. Boch, May 29, 2014
# Micheli Laboratory, Hopkins Marine Station, Stanford University

# Note: the raw data was changed by the following to keep the data structure and format in its most basic form and structure
# make sure column names and order are all the same as previous files

############## Processing in Excel ##############
# Done in Excel
# 1. Made a dulicate copy of original xls file (keeps dates the same)
# 2. Deleted other worksheets (Hojas) in this duplicate file
# 3. saved the file from xls --> csv (this also gets rid of any pulldowns and formulas)
# 4. cleared the contents of all columns and rows outside of dataframe.

##  Now, the file/s are structure ready to be imported into R

## Road Map: Bring in all data from all sites --> retrieve columns with desired data --> rename columns with english names --> remove all unecessary characters --> change spanish to latin Genusspecies --> add Zone, Country, Site data --> output a single file with all the data

############## Processing in R ##############

rm(list=ls())
library(zoo)
library(plyr)
library(gsubfn)

######## Read in all raw Algae data from All Sites (2006- )

system("ls -lt ~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Raw/*algaeraw* > ~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/All_Reef_Check_algaeraw_filelist_latest.txt")
# load the list of files into a table
currentTable = read.table("~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/All_Reef_Check_algaeraw_filelist_latest.txt",as.is=TRUE)
#  read all tables in the list
tables = list()
##read in data and attach dataframe to each file in list
for (i in 1:nrow(currentTable)){
tables[[i]] = read.csv(currentTable$V9[i], header=T, stringsAsFactors=F, fileEncoding="latin1")
}

################################################################################################################################################################################
#### Remove unecessary columns of data

Algaecolumnnameslist.df <- lapply(tables, function(x){colnames(x)}) # call on Algaecolumnnameslist.df at the R Console to see the column names of all the files
Algae.df <- lapply(tables, function(x){x[, c("observador", "fecha", "a.o", "tiempo.inicio", "tiempo.final", "no..transecto", "sitio", "tipo.de.sitio", "prof.inicial..m.", "prof.final..m.", "prof.max..m.", "prof.X..m.", "latitud..N.", "longitud..W.", "temperatura...C.", "visibilidad..m.", "especie", "abundancia", "X..guias", "Distancia")]})# Extracting columns from each dataframe only works if the column names are the same--including case!!!

AllLocationsAlgaeRaw <- do.call(rbind, Algae.df)

################################################################################################################################################################################
#### Change the name of column headings

names(AllLocationsAlgaeRaw)[1]<-"Observer"
names(AllLocationsAlgaeRaw)[2]<-"Date"
names(AllLocationsAlgaeRaw)[3]<-"Year"
names(AllLocationsAlgaeRaw)[4]<-"TimeInitial"
names(AllLocationsAlgaeRaw)[5]<-"TimeFinal"
names(AllLocationsAlgaeRaw)[6]<-"TransectNumber"
names(AllLocationsAlgaeRaw)[7]<-"Location"
names(AllLocationsAlgaeRaw)[8]<-"Zone"
names(AllLocationsAlgaeRaw)[9]<-"DepthInitial_m"
names(AllLocationsAlgaeRaw)[10]<-"DepthFinal_m"
names(AllLocationsAlgaeRaw)[11]<-"DepthMax_m"
names(AllLocationsAlgaeRaw)[12]<-"MidTransDepth_m"
names(AllLocationsAlgaeRaw)[13]<-"Latitude"
names(AllLocationsAlgaeRaw)[14]<-"Longitude"
names(AllLocationsAlgaeRaw)[15]<-"Temperature_C"
names(AllLocationsAlgaeRaw)[16]<-"Visibility_m"
names(AllLocationsAlgaeRaw)[17]<-"Genusspecies"
names(AllLocationsAlgaeRaw)[18]<-"TotalAbundance"
names(AllLocationsAlgaeRaw)[19]<-"NumberStipes"
names(AllLocationsAlgaeRaw)[20]<-"Distance"

################################################################################################################################################################################
#### Add column with Site and Country and a searchable date format

AllLocationsAlgaeRaw <- as.data.frame(sapply(AllLocationsAlgaeRaw, function(x) gsub("\u0096", "n", x, ignore.case=TRUE))) # replace the spanish n
AllLocationsAlgaeRaw <- as.data.frame(sapply(AllLocationsAlgaeRaw, function(x) gsub("n/d", "", x, ignore.case=TRUE))) # replace n/d
AllLocationsAlgaeRaw <- as.data.frame(sapply(AllLocationsAlgaeRaw, function(x) gsub("n/a", "", x, ignore.case=TRUE))) # replace n/a
AllLocationsAlgaeRaw <- as.data.frame(sapply(AllLocationsAlgaeRaw, function(x) gsub(" ", "", x, ignore.case=TRUE))) # replace spaces

################################################################################################################################################################################
#### Replace unneccessary location designations or dual or / names of locations

AllLocationsAlgaeRaw <- as.data.frame(sapply(AllLocationsAlgaeRaw, function(x) gsub("profundo", "", x, ignore.case=TRUE))) # replace depth information
AllLocationsAlgaeRaw <- as.data.frame(sapply(AllLocationsAlgaeRaw, function(x) gsub("somero", "", x, ignore.case=TRUE))) # replace depth information
AllLocationsAlgaeRaw <- as.data.frame(sapply(AllLocationsAlgaeRaw, function(x) gsub("LaPlana/LasCuevas", "LaPlana", x, ignore.case=TRUE))) # replace any dual named data

################################################################################################################################################################################
#### Replace Zone designation from 1 and 2's to Reserve and Fished

AllLocationsAlgaeRaw$Zone <- ifelse(AllLocationsAlgaeRaw$Location %in% c("PuntaBlanca", "Garropas", "ElProgresista", "PuntaPrieta", "LaPlana"), "Reserve", ifelse(AllLocationsAlgaeRaw$Location %in% c("ElAbolladero", "LosCabitos", "LaDulce", "Babencho", "LaBarrita", "MorroPrieto", "LaGuanera", "LaBarrita", "ElNido", "ElTibo", "ElTriangulo", "LaVela", "LomaLinda", "PiedrasAltas"), "Fished", ""))

################################################################################################################################################################################
#### Replace spanish entries with Latin names but need to split the dataframe because the species list changes in 2013

### Replace some questionable species names with Genusspp in 2006-2012 data
### Replace any spanish names with Genusspecies in 2006-2012 data

AllLocationsAlgaeRaw$Genusspecies <- sapply(AllLocationsAlgaeRaw$Genusspecies, function(x) gsub("Coliflor", "Eiseniaarborea", x, ignore.case=TRUE)) #
AllLocationsAlgaeRaw$Genusspecies <- sapply(AllLocationsAlgaeRaw$Genusspecies, function(x) gsub("Pterygophora", "Pterygophoracalifornica", x, ignore.case=TRUE)) #
AllLocationsAlgaeRaw$Genusspecies <- sapply(AllLocationsAlgaeRaw$Genusspecies, function(x) gsub("sargassummuticum", "Sargassummuticum", x)) #
AllLocationsAlgaeRaw$Genusspecies <- sapply(AllLocationsAlgaeRaw$Genusspecies, function(x) gsub("sargassumfilicinum", "Sargassumfilicinum", x)) #
AllLocationsAlgaeRaw$Genusspecies <- sapply(AllLocationsAlgaeRaw$Genusspecies, function(x) gsub("Undaria", "Undariapinnatifida", x)) # 
AllLocationsAlgaeRaw$Genusspecies <- sapply(AllLocationsAlgaeRaw$Genusspecies, function(x) gsub("Caulerpa", "Caulerpataxifolia", x, ignore.case=TRUE))
AllLocationsAlgaeRaw$Genusspecies <- sapply(AllLocationsAlgaeRaw$Genusspecies, function(x) gsub("sargazogigante", "Macrocystispyrifera", x, ignore.case=TRUE))
AllLocationsAlgaeRaw$Genusspecies <- sapply(AllLocationsAlgaeRaw$Genusspecies, function(x) gsub("Laminaria", "Laminariaspp", x, ignore.case=TRUE))
AllLocationsAlgaeRaw$Genusspecies <- sapply(AllLocationsAlgaeRaw$Genusspecies, function(x) gsub("Cistoceyrasp.", "Cystoceiraspp", x, ignore.case=TRUE))

################################################################################################################################################################################
#### Add Country and Site data

AllLocationsAlgaeRaw["Country"] <- "Mexico"

AllLocationsAlgaeRaw$Site <- ifelse(AllLocationsAlgaeRaw$Location %in% c("PuntaBlanca", "Garropas", "ElProgresista", "LosCabitos", "ElAbolladero"), "IslaMagdalena", ifelse(AllLocationsAlgaeRaw$Location %in% c("LaPlana", "LaDulce", "PuntaPrieta", "Babencho", "LaGuanera", "LaBarrita"), "IslaNatividad", ""))

AllLocationsAlgaeRaw <- AllLocationsAlgaeRaw[, c(1, 2, 3, 4, 5, 6, 7, 22, 21, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)]

############################################################################################################################################################################
#### Fill in zero data for all of RC Algae data
#### At this point, the data needs to be split by Sites because the sites have a different species list.

Mag <- subset(AllLocationsAlgaeRaw, Site == "IslaMagdalena")

Mr<-rle(c(Mag$TransectNumber))$lengths # index the dataframe by TransectNumber --> TransectNumber were entered in clusters; essentially, this represents each page of the dataset entered!

Mag.df <- split(Mag, rep(seq_along(Mr), Mr)) ## this is the bread and butter!!!  double checked that it works!

# Now, we need to fill in each subdataframe by the full RCGS list to fill in zeroes

### Below is the full list of Reef Check Algae species from I. Magdalena

MagRCGS <- c("Eiseniaarborea", "Pterygophoracalifornica", "Laminariaspp", "Macrocystispyrifera") 
MagRCGS <-as.data.frame(MagRCGS)
names(MagRCGS)[1] <- "Genusspecies"

###############################################################################################################################################################################

# merge function to match the MagRCGS list with the raw data

MagCompleteAlgaeRaw.df <- lapply(Mag.df, function(x, y) {merge(x, y, by.x=names(x)[19], by.y=names(y)[1], all=T)}, MagRCGS)

# fill in the NA with the corresponding transect metadata -- i.e., location, site, latitude, etc...
FillNA <- function(z){
	AbunDis = z[,20:22]
	YY = na.locf(z[,1:19], na.rm=TRUE, fromLast=TRUE) # fill in
	YYY = na.locf(YY, na.rm=TRUE) # fill in
	NewFill = cbind(YYY, AbunDis) # combine all the columns back again
}

MagCompleteAlgae.df<-lapply(MagCompleteAlgaeRaw.df, FillNA)

MagCompleteAlgae<-do.call(rbind, MagCompleteAlgae.df)

MagAlgae <- with(MagCompleteAlgae, MagCompleteAlgae[!(Genusspecies == "" | is.na(Genusspecies)), ]) # delete all rows with no Genusspecies in the dataframe.  When the raw data is expanded to include zero observations for a trasect, the merging function creates an additional row that is unnecessary.  So this function looks for cells with blank values in the Genusspecies column and deletes these blank rows.

#### Reorder the column order -- because the other R analysis/scripts has this order  -- author's learning mistake

MagCompleteAlgaeRaw <- MagAlgae[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 1, 20, 21, 22)]

MagCompleteAlgaeRaw$TotalAbundance[is.na(MagCompleteAlgaeRaw$TotalAbundance)] <- "0" # replace the NA's with 0

#############################################################################################################################################################################################################################################################################################################################################################

Nat <- subset(AllLocationsAlgaeRaw, Site == "IslaNatividad")

Nr<-rle(c(Nat$TransectNumber))$lengths # index the dataframe by TransectNumber --> TransectNumber were entered in clusters; essentially, this represents each page of the dataset entered!

Nat.df <- split(Nat, rep(seq_along(Nr), Nr))

# Now, we need to fill in each subdataframe by the full RCGS list to fill in zeroes

### Below is the full list of Reef Check Algae species from I. Magdalena

NatRCGS <- c("Eiseniaarborea", "Pterygophoracalifornica", "Laminariaspp", "Macrocystispyrifera", "Sargassummuticum", "Sargassumfilicinum", "Undariaspp", "Caulerpataxifolia") 
NatRCGS <-as.data.frame(NatRCGS)
names(NatRCGS)[1] <- "Genusspecies"

###############################################################################################################################################################################

# merge function to match the MagRCGS list with the raw data

NatCompleteAlgaeRaw.df <- lapply(Nat.df, function(x, y) {merge(x, y, by.x=names(x)[19], by.y=names(y)[1], all=T)}, NatRCGS)

# fill in the NA with the corresponding transect metadata -- i.e., location, site, latitude, etc...
FillNA <- function(z){
	AbunDis = z[,20:22]
	YY = na.locf(z[,1:19], na.rm=TRUE, fromLast=TRUE) # fill in
	YYY = na.locf(YY, na.rm=TRUE) # fill in
	NewFill = cbind(YYY, AbunDis) # combine all the columns back again
}

NatCompleteAlgae.df<-lapply(NatCompleteAlgaeRaw.df, FillNA)

NatCompleteAlgae<-do.call(rbind, NatCompleteAlgae.df)

NatAlgae <- with(NatCompleteAlgae, NatCompleteAlgae[!(Genusspecies == "" | is.na(Genusspecies)), ]) # delete all rows with no Genusspecies in the dataframe.  When the raw data is expanded to include zero observations for a trasect, the merging function creates an additional row that is unnecessary.  So this function looks for cells with blank values in the Genusspecies column and deletes these blank rows.

#### Reorder the column order -- because the other R analysis/scripts has this order  -- author's learning mistake

NatCompleteAlgaeRaw <- NatAlgae[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 1, 20, 21, 22)]

NatCompleteAlgaeRaw$TotalAbundance[is.na(NatCompleteAlgaeRaw$TotalAbundance)] <- "0" # replace the NA's with 0

AllAlgaeClean <- rbind(MagCompleteAlgaeRaw, NatCompleteAlgaeRaw)

write.table(AllAlgaeClean, "~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/All_locations_Reef_Check_algae_data_latest.csv", sep=",", col.names=T, row.names=F)




