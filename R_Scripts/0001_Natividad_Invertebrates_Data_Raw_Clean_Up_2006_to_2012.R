########### Reef Check Invertebrate Data Clean Up Process ###########
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
# 5. Manually changed all date formats to m/dd/yy (easier in Excel)

##  Now, the file/s are structure ready to be imported into R

############## Processing in R ##############

rm(list=ls())
library(zoo)
library(plyr)
library(gsubfn)

######## Read in all raw Invertebrates data from Isla Natividad Site (2006-2012)

system("ls -lt ~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Raw/*invertebratesraw_2006_2012* > ~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/Natividad_2006_2012_Reef_Check_Invertebratesraw_filelist_latest.txt")
# load the list of files into a table
currentTable = read.table("~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/Natividad_2006_2012_Reef_Check_Invertebratesraw_filelist_latest.txt",as.is=TRUE)
#  read all tables in the list
tables = list()
##read in data and attach dataframe to each file in list
for (i in 1:nrow(currentTable)){
tables[[i]] = read.csv(currentTable$V9[i], header=T, stringsAsFactors=F, fileEncoding="latin1")
}

NatividadInvertebratesRaw <- do.call(rbind, tables)

################################################################################################################################################################################
#### Remove unecessary columns of data

NatividadInvertebratesRaw$codigo <- NULL
NatividadInvertebratesRaw$tiempo.total <- NULL
NatividadInvertebratesRaw$no..buceo <- NULL
NatividadInvertebratesRaw$epoca <- NULL
NatividadInvertebratesRaw$no..replica <- NULL
NatividadInvertebratesRaw$sitio.en.extenso <- NULL
NatividadInvertebratesRaw$prof.inicial..ft. <- NULL
NatividadInvertebratesRaw$prof.final..ft. <- NULL
NatividadInvertebratesRaw$prof.max..ft. <- NULL
NatividadInvertebratesRaw$prof.X..ft. <- NULL
NatividadInvertebratesRaw$temperatura...F. <- NULL
NatividadInvertebratesRaw$Direccion <- NULL
NatividadInvertebratesRaw$Observaciones <- NULL

################################################################################################################################################################################
#### Change the name of column headings

names(NatividadInvertebratesRaw)[1]<-"Observer"
names(NatividadInvertebratesRaw)[2]<-"Date"
names(NatividadInvertebratesRaw)[3]<-"Year"
names(NatividadInvertebratesRaw)[4]<-"TimeInitial"
names(NatividadInvertebratesRaw)[5]<-"TimeFinal"
names(NatividadInvertebratesRaw)[6]<-"TransectNumber"
names(NatividadInvertebratesRaw)[7]<-"Location"
names(NatividadInvertebratesRaw)[8]<-"Zone"
names(NatividadInvertebratesRaw)[9]<-"DepthInitial_m"
names(NatividadInvertebratesRaw)[10]<-"DepthFinal_m"
names(NatividadInvertebratesRaw)[11]<-"DepthMax_m"
names(NatividadInvertebratesRaw)[12]<-"MidTransDepth_m"
names(NatividadInvertebratesRaw)[13]<-"Latitude"
names(NatividadInvertebratesRaw)[14]<-"Longitude"
names(NatividadInvertebratesRaw)[15]<-"Temperature_C"
names(NatividadInvertebratesRaw)[16]<-"Visibility_m"
names(NatividadInvertebratesRaw)[17]<-"Genusspecies"
names(NatividadInvertebratesRaw)[18]<-"TotalCounts"
names(NatividadInvertebratesRaw)[19]<-"Distance"
names(NatividadInvertebratesRaw)[20]<-"ExtrapolationValue"

################################################################################################################################################################################
#### Add column with Site and Country and a searchable date format

NatividadInvertebratesRaw["Country"] <- "Mexico"
NatividadInvertebratesRaw["Site"] <- "IslaNatividad"

NatividadInvertebratesRaw$Observer <- sapply(NatividadInvertebratesRaw$Observer, function(x) gsub("\u0096", "n", x, ignore.case=TRUE)) # replace the spanish n
NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub("\u008e", "e", x, ignore.case=TRUE)) # replace the spanish e
NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub("\u0096", "n", x, ignore.case=TRUE)) # replace the spanish n

################################################################################################################################################################################
#### Reorder the column order

NatividadInvertebratesRaw <- NatividadInvertebratesRaw[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Site", "Location", "Country", "Zone", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "TotalCounts", "Distance", "ExtrapolationValue")]

### Use unique() function to check for all unique entries column by column and change/replace if necessary
################################################################################################################################################################################
#### Replace old Location (sitioenextenso) data with new Location names

NatividadInvertebratesRaw$Location <- sapply(NatividadInvertebratesRaw$Location, function(x) gsub(" ", "", x)) # replace spaces

NatividadInvertebratesRaw$Location <- sapply(NatividadInvertebratesRaw$Location, function(x) gsub("LaPlana/LasCuevas", "LaPlana", x))

################################################################################################################################################################################
#### Replace Zone designation from location information to Reserve or Fished (easier to remember) --> future designation should be based on GPS coordinates

NatividadInvertebratesRaw$Zone <- ifelse(NatividadInvertebratesRaw$Location %in% c("PuntaPrieta", "LaPlana"), "Reserve", ifelse(NatividadInvertebratesRaw$Location %in% c("LaDulce", "Babencho", "LaBarrita", "MorroPrieto", "LaGuanera", "LaBarrita", "ElNido", "ElTibo", "ElTriangulo", "LaVela", "LomaLinda", "PiedrasAltas"), "Fished", ""))


###############################################################################################################################################################################
#### Replace n/a, n/d from data -- not consistenetly entered

NatividadInvertebratesRaw$Distance <- sapply(NatividadInvertebratesRaw$Distance, function(x) gsub("n/a", "", x, ignore.case=TRUE))

###############################################################################################################################################################################
#### Replace spanish Genusspecies with Latin names but need to split the dataframe because the species list changes in 2013

### Replace some questionable species names with Genusspp in 2006-2012 data
### Replace any spanish names with Genusspecies in 2006-2012 data

NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub(" ", "", x)) # replace all spaces

NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub("0", "", x)) # replace all zeros

NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub("spp.", "sp", x)) # replace all

NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub("Anthopleuraspp", "Anemonesp", x, ignore.case=TRUE))

NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub("Muriceacalifornica", "Muriceasp", x, ignore.case=TRUE))

NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub("pulpo", "Octopussp", x, ignore.case=TRUE))

NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub("Pycnopodiahelianthoides", "Sunstarsp", x, ignore.case=TRUE))

NatividadInvertebratesRaw$Genusspecies <- sapply(NatividadInvertebratesRaw$Genusspecies, function(x) gsub("Strongylocentrotusfranciscanus", "Mesocentrotusfranciscanus", x, ignore.case=TRUE))

################################################################################################################################################################################
#### Fill in data for zeros for all of RC Invertebrates data
#### A bit complicated for the whole dataframe but worth it

r<-rle(c(NatividadInvertebratesRaw$TransectNumber))$lengths # index the dataframe by TransectNumber --> TransectNumber were entered in clusters; essentially, this represents each page of the dataset entered!

NatividadInvertebratesRaw.df <- split(NatividadInvertebratesRaw, rep(seq_along(r), r)) ## this is the bread and butter!!!  double checked that it works!

## Now, we need to fill in each subdataframe by the full RCGS list to fill in zeroes

### Below is the full list of Reef Check invertebrate species from 2006 - 2012 in I. Natividad; list changed in 2013

RCGS <- c("Haliotiscorrugata", "Haliotisfulgens", "Haliotissorenseni", "Haliotisrufescens", "Haliotiscracherodii", "Panulirusinterruptus", "Parastichopusparvimensis", "Pisastergiganteus", "Sunstarsp", "Megastraeaundosa", "Megastraeaturbanica", "Megathuracrenulata", "Kelletiakelletii", "Crassedomagiganteum", "Anemonesp", "Gorgoniansp", "Lophogorgiachilensis", "Mesocentrotusfranciscanus", "Strongylocentrotuspurpuratus", "Centrostephanuscoronatus", "Patiriaminiata", "Cancersp", "Loxorhyncusgrandis", "Octopussp", "Cypraeaspadicea") 
RCGS <-as.data.frame(RCGS)
names(RCGS)[1] <- "Genusspecies"

################################################################################################################################################################################

# merge function to match the RCGS list with the raw data

CompleteInvertebratesRaw.df <- lapply(NatividadInvertebratesRaw.df, function(x, y) {merge(x, y, by.x=names(x)[19], by.y=names(y)[1], all=T)}, RCGS)

# fill in the NA with the corresponding transect metadata -- i.e., location, site, latitude, etc...
FillNA <- function(z){
	AbunDis = z[,20:22]
	YY = na.locf(z[,1:19], na.rm=TRUE, fromLast=TRUE) # fill in
	YYY = na.locf(YY, na.rm=TRUE) # fill in
	NewFill = cbind(YYY, AbunDis) # combine all the columns back again
}

CompleteInvertebrates.df<-lapply(CompleteInvertebratesRaw.df, FillNA)

CompleteInvertebrates<-do.call(rbind, CompleteInvertebrates.df)

NatividadInvertebrates <- with(CompleteInvertebrates, CompleteInvertebrates[!(Genusspecies == "" | is.na(Genusspecies)), ]) # delete all rows with no Genusspecies in the dataframe.  When the raw data is expanded to include zero observations for a trasect, the merging function creates an additional row that is unnecessary.  So this function looks for cells with blank values in the Genusspecies column and deletes these blank rows.

#### Reorder the column order, change NAs in TotalCounts to 0's, and add Concession/NoConcession data

NatividadInvertebrates <- NatividadInvertebrates[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Site", "Location", "Country", "Zone", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "TotalCounts", "Distance", "ExtrapolationValue")]

NatividadInvertebrates[, "TotalCounts"] <- round(as.numeric(as.character(NatividadInvertebrates$TotalCounts))) # round up to nearest integer

NatividadInvertebrates$TotalCounts[is.na(NatividadInvertebrates$TotalCounts)] <- 0 # replace NA in Total Abundance column with zeros: reflects transects with zero counts

NatividadInvertebrates$Status <- ifelse(NatividadInvertebrates$Genusspecies %in% c("Haliotiscorrugata", "Haliotisfulgens", "Haliotissorenseni", "Haliotiscracherodii", "Haliotisrufescens", "Haliotisassimilis", "Mesocentrotusfranciscanus", "Parastichopusparvimensis", "Octopusbimaculatus", "Octopusrubescens", "Octopussp", "Panulirusinterruptus", "Panulirusinflatus", "Megastraeaundosa", "Megastraeaturbanica"), "Concession", "NoConcession") # note, concession refers to recent <10 years it has been fished

NatividadInvertebrates0612 <- NatividadInvertebrates

write.table(NatividadInvertebrates0612, "~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/Natividad_Invertebrates_clean_data_2006_to_2012.csv", sep=",", col.names=T, row.names=F)




