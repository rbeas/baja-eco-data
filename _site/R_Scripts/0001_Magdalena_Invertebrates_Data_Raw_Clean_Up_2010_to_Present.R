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

##  Now, the file/s are structure ready to be imported into R

############## Processing in R ##############

rm(list=ls())
library(zoo)
library(plyr)
library(gsubfn)

######## Read in all raw Invertebrates data from Isla Magdalena Site (2006-2012)

system("ls -lt ~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Raw/*Magdalena_invertebratesraw* > ~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/Magdalena_2012_to_Present_Reef_Check_Invertebratesraw_filelist_latest.txt")
# load the list of files into a table
currentTable = read.table("~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/Magdalena_2012_to_Present_Reef_Check_Invertebratesraw_filelist_latest.txt",as.is=TRUE)
#  read all tables in the list
tables = list()
##read in data and attach dataframe to each file in list
for (i in 1:nrow(currentTable)){
tables[[i]] = read.csv(currentTable$V9[i], header=T, stringsAsFactors=F, fileEncoding="latin1")
}

# Invertebratescolumnnameslist.df <- lapply(tables, function(x){colnames(x)}) # check the colnames of all the files

# print(unique(sort(unlist(unique(Invertebratescolumnnameslist.df))))) # this function puts the list of column names in alphabetical order and lists the unique column names

###############################################################################################################################################################################
#### Rename desired columns

tables <- lapply(tables, function(x) {colnames(x) <- tolower(colnames(x)); x}) # change all text to lower case
tables <- lapply(tables, function(x) {colnames(x) <- gsub("no..transecto", "TransectNumber", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("fecha", "Date", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("especie", "Genusspecies", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("latitud..n.", "Latitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("longitud..w.", "Longitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("observador", "Observer", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("tiempo.inicio", "TimeInitial", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("tiempo.final", "TimeFinal", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("prof.inicial..m.", "DepthInitial_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("prof.final..m.", "DepthFinal_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("prof.max..m.", "DepthMax_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("prof.x..m.", "MidTransDepth_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("temperatura...c.", "Temperature_C", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("sitio", "Location", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("visibilidad..m.", "Visibility_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("extrapolacion", "ExtrapolationValue", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("abundancia", "TotalCounts", colnames(x), fixed=TRUE); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("distancia", "Distance", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("a.o", "Year", colnames(x), fixed=TRUE); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("tipo.de.Location", "Zone", colnames(x), fixed=TRUE); x})

#########
#Add any columns that need to be added if they are missing from a particular table

# MagInvertebratescolumnnameslist.df <- lapply(tables, function(x){colnames(x)}) # check the colnames of all the files

# print(unique(sort(unlist(unique(MagInvertebratescolumnnameslist.df))))) # this function puts the list of column names in alphabetical order and lists the unique column names

############################
##### Select the columns with data that we want from all the files and create a single raw data table
# use the function --> currentTable to check all the files have been imported
# use the function --> ls(tables[[1]]) to check all the column headings

# Date
# DepthFinal_m
# DepthInitial_m
# DepthMax_m
# Distance
# ExtrapolationValue
# Genusspecies
# Latitude
# Location
# Longitude
# MidTransDepth_m
# Observer
# Temperature_C
# TimeFinal
# TimeInitial
# TotalCounts
# TransectNumber
# Visibility_m
# Year

#########
# Add any columns that need to be added if they are missing from a sub table
# if Error in `[.data.frame`(x, , c("Observer", "Date", "Year", "TimeInitial",  : undefined columns selected <-- usually due to a new file added --> need to recheck all the column headers again

# use currentTable to check that all files are imported; tables numbers (file) are ordered on the list; table numbers will change when new yearly monitoring data are available

tables[[3]]["ExtrapolationValue"] <- "NA" # this Magdalena 2011 file does not have the Extrapolation column
tables[[4]]["ExtrapolationValue"] <- "NA" # this Magdalena 2010 file does not have the Extrapolation column

MagdalenaInvertebratesRaw.df <- lapply(tables, function(x){x[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Location", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "TotalCounts", "Distance", "ExtrapolationValue")]})# Extracting columns from each dataframe only works if the column names are the same--including case!!!

MagdalenaInvertebratesRaw <- do.call(rbind, MagdalenaInvertebratesRaw.df) # all the tables from different files are now in a single dataframe

###############################################################################################################################################################################
#### Add column with Site and Country

MagdalenaInvertebratesRaw["Country"] <- "Mexico"
MagdalenaInvertebratesRaw["Site"] <- "IslaMagdalena"

MagdalenaInvertebratesRaw["Longitude"] <- -1 * abs(MagdalenaInvertebratesRaw$Longitude) # change values to correct longitude designation for Magdalena locations

MagdalenaInvertebratesRaw$Observer <- sapply(MagdalenaInvertebratesRaw$Observer, function(x) gsub("\u0096", "n", x, ignore.case=TRUE)) # replace the spanish n
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("\u008e", "e", x, ignore.case=TRUE)) # replace the spanish n
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("\u0096", "n", x, ignore.case=TRUE)) # replace the spanish n

#############################################################################################################################################################################
### Reorder the column order -- author's preference

MagdalenaInvertebratesRaw <- MagdalenaInvertebratesRaw[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Site", "Location", "Country", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "TotalCounts", "Distance", "ExtrapolationValue")]

# Use unique() function to check for all unique entries column by column and change/replace if necessary
#############################################################################################################################################################################
#### Replace old Location data with new Location names

MagdalenaInvertebratesRaw$Location <- sapply(MagdalenaInvertebratesRaw$Location, function(x) gsub(" ", "", x))
MagdalenaInvertebratesRaw$Location <- sapply(MagdalenaInvertebratesRaw$Location, function(x) gsub("profundo", "", x))
MagdalenaInvertebratesRaw$Location <- sapply(MagdalenaInvertebratesRaw$Location, function(x) gsub("Profundo", "", x))
MagdalenaInvertebratesRaw$Location <- sapply(MagdalenaInvertebratesRaw$Location, function(x) gsub("somero", "", x))
MagdalenaInvertebratesRaw$Location <- sapply(MagdalenaInvertebratesRaw$Location, function(x) gsub("Somero", "", x))

MagdalenaInvertebratesRaw <- as.data.frame(sapply(MagdalenaInvertebratesRaw, function(x) gsub("PuntaBlancaTepetate", "PuntaBlanca", x, ignore.case=TRUE))) # change two site names; this needs to be discussed
MagdalenaInvertebratesRaw <- as.data.frame(sapply(MagdalenaInvertebratesRaw, function(x) gsub("PuntaBlancaGarropas", "PuntaBlanca", x, ignore.case=TRUE))) # change two site names; this needs to be discussed

##############################################################################################################################################################################
#### Create Zone designation (Reserve or Fished) from location information

MagdalenaInvertebratesRaw$Zone <- ifelse(MagdalenaInvertebratesRaw$Location %in% c("PuntaBlanca", "Garropas"), "Reserve", ifelse(MagdalenaInvertebratesRaw$Location %in% c("ElAbolladero",  "ElProgresista", "LosCabitos"), "Fished", "")) # both Tepetate and Garropas are in the Reserve Zones

##############################################################################################################################################################################
#### Replace spanish Genusspecies with Latin names but need to split the dataframe because the species list changes in 2013

### Replace some questionable species names with Genusspp in 2006-2012 data
### Replace any spanish names with Genusspecies in 2006-2012 data

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub(" ", "", x)) # replace all spaces
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("n/d", "", x)) # replace all N/D
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("N/D", "", x)) # replace all N/D

MagdalenaInvertebratesRaw$Genusspecies <- (sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("\u009c", "u", x, ignore.case=TRUE))) # replace the spanish u

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("0", "", x)) # replace all zeros
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("\\.", "", x)) # replace all . after spp.
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("\\(>1cm)", "", x)) # replace all . after spp.

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("spp", "", x, ignore.case=TRUE))



MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("abulonamarillo", "Haliotiscorrugata", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("abulonazul", "Haliotisfulgens", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("Abulonchino", "Haliotissorenseni", x, ignore.case=TRUE))

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("arbolitos", "Gorgoniansp", x, ignore.case=TRUE))

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("Muricea", "Muriceacalifornica", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("muriceacalifornica", "Muriceacalifornica", x, ignore.case=FALSE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("Arbolitocafe", "Muriceacalifornica", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("Arbolitorojo", "Lophogorgiachilensis", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("Arboilitorojo", "Lophogorgiachilensis", x, ignore.case=TRUE))



MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("caracolchino", "Hexaplexprinceps", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("caracolturbanico", "Megastraeaturbanica", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("caracolundosa", "Megastraeaundosa", x, ignore.case=TRUE))

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("Erizopuntadelapiz", "Eucidaristhouarsii", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("erizoespinudo", "Centrostephanuscoronatus", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("erizonegro", "Centrostephanuscoronatus", x, ignore.case=TRUE)) # need to double check that erizo negro is Centrostephanus coronatus
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("erizopuntasrotas", "Echinometravanbrunti", x, ignore.case=TRUE))

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("estrellacomun", "Phatariaunifascialis", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("estrellagirasol", "Sunstarsp", x, ignore.case=TRUE))

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("langostaroja", "Panulirusinterruptus", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("langostaazul", "Panulirusinflatus", x, ignore.case=TRUE))

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("pepinocafe", "Isostichopusfuscus", x, ignore.case=TRUE))

MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("pulporojo", "Octopusrubescens", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("pulpodospuntos", "Octopusbimaculatus", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("Pulodospuntos", "Octopusbimaculatus", x, ignore.case=TRUE))
MagdalenaInvertebratesRaw$Genusspecies <- sapply(MagdalenaInvertebratesRaw$Genusspecies, function(x) gsub("pulpo", "Octopussp", x, ignore.case=TRUE)) # no species identification

##############################################################################################################################################################################
#### Fill in data for zeros for all of 2010 - 2012 RC Invertebrates data

MagdalenaInvertebratesRaw[, "Year"] <- as.numeric(as.character(MagdalenaInvertebratesRaw$Year))

r<-rle(c(MagdalenaInvertebratesRaw$TransectNumber))$lengths # index the dataframe by TransectNumber --> TransectNumber were entered in clusters; essentially, this represents each page of the dataset entered!

MagdalenaInvertebratesRaw.df <- split(MagdalenaInvertebratesRaw, rep(seq_along(r), r)) ## this is the bread and butter!!!  double checked that it works!

## Now, we need to fill in each subdataframe by the full RCGS list to fill in zeroes
### Below is the full list of Reef Check invertebrate species from 2010-2014 in I. Magdalena; list changed in 2013; 2013 is missing data due to hurricanes; any new species can be added to this list; dataframe can be subsequently split and GS not on list for a particular year removed

RCGS <- c("Haliotiscorrugata", "Haliotisfulgens", "Haliotissorenseni", "Haliotisrufescens", "Haliotiscracherodii", "Panulirusinterruptus", "Panulirusinflatus", "Isostichopusfuscus", "Sunstarsp", "Megastraeaundosa", "Megastraeaturbanica", "Hexaplexprinceps", "Eucidaristhouarsii", "Centrostephanuscoronatus", "Echinometravanbrunti", "Phatariaunifascialis", "Muriceacalifornica", "Octopusrubescens", "Octopusbimaculatus", "Octopussp", "Isostichopusfuscus", "Haliotissp") 
RCGS <-as.data.frame(RCGS)
names(RCGS)[1] <- "Genusspecies"

##############################################################################################################################################################################

# merge function to match the RCGS list with the raw data

CompleteInvertebratesRaw.df <- lapply(MagdalenaInvertebratesRaw.df, function(x, y) {merge(x, y, by.x=names(x)[18], by.y=names(y)[1], all=T)}, RCGS)

# fill in the NA with the corresponding transect metadata -- i.e., location, site, latitude, etc...
FillNA <- function(z){
	AbunDis = z[,19:22]
	YY = na.locf(z[,1:18], na.rm=TRUE, fromLast=TRUE) # fill in
	YYY = na.locf(YY, na.rm=TRUE) # fill in
	NewFill = cbind(YYY, AbunDis) # combine all the columns back again
}

CompleteInvertebrates.df <- lapply(CompleteInvertebratesRaw.df, FillNA)

CompleteInvertebrates <- do.call(rbind, CompleteInvertebrates.df)

MagdalenaInvertebrates <- with(CompleteInvertebrates, CompleteInvertebrates[!(Genusspecies == "" | is.na(Genusspecies)), ]) # delete all rows with no Genusspecies in the dataframe.  When the raw data is expanded to include zero observations for a trasect, the merging function creates an additional row that is unnecessary.  So this function looks for cells with blank values in the Genusspecies column and deletes these blank rows.

MagdalenaInvertebrates[, "TotalCounts"] <- round(as.numeric(as.character(MagdalenaInvertebrates$TotalCounts))) # round up to nearest integer

MagdalenaInvertebrates$TotalCounts[is.na(MagdalenaInvertebrates$TotalCounts)] <- 0 # replace NA in Total Abundance column with zeros: reflects transects with zero counts

MagdalenaInvertebrates$Zone <- ifelse(MagdalenaInvertebrates$Location %in% c("PuntaBlanca", "Garropas"), "Reserve", ifelse(MagdalenaInvertebrates$Location %in% c("ElAbolladero",  "ElProgresista", "LosCabitos"), "Fished", "")) # both Tepetate and Garropas are in the Reserve Zones


MagdalenaInvertebrates$Status <- ifelse(MagdalenaInvertebrates$Genusspecies %in% c("Haliotiscorrugata", "Haliotisfulgens", "Haliotissorenseni", "Haliotiscracherodii", "Haliotisrufescens", "Haliotisassimilis", "Mesocentrotusfranciscanus", "Parastichopusparvimensis", "Octopusbimaculatus", "Octopusrubescens", "Octopussp", "Panulirusinterruptus", "Panulirusinflatus", "Megastraeaundosa", "Megastraeaturbanica"), "Concession", "NoConcession") # note, concession refers to recent <10 years it has been fished

##*******************************************************
# ### Below is the full list of Reef Check invertebrate species from 2010-2012 in I. Magdalena; list changed in 2013

# RCGS <- c("Haliotiscorrugata", "Haliotisfulgens", "Haliotissorenseni", "Haliotisrufescens", "Haliotiscracherodii", "Panulirusinterruptus", "Panulirusinflatus", "Isostichopusfuscus", "Sunstarsp", "Megastraeaundosa", "Megastraeaturbanica", "Hexaplexprinceps", "Eucidaristhouarsii", "Centrostephanuscoronatus", "Echinometravanbrunti", "Phatariaunifascialis", "Muriceacalifornica", "Octopusrubescens", "Octopusbimaculatus", "Isostichopusfuscus") 

##############################################################################################################################################################################
# ### Below is the full list of Reef Check invertebrate species from 2013 to

# RCGS2 <- c("Haliotiscorrugata", "Haliotisfulgens", "Haliotissorenseni", "Haliotisrufescens", "Haliotiscracherodii", "Panulirusinterruptus", "Panulirusinflatus", "Isostichopusfuscus", "Megastraeaundosa", "Megastraeaturbanica", "Hexaplexprinceps", "Haliotissp", "Centrostephanuscoronatus", "Echinometravanbrunti", "Phatariaunifascialis", "Muriceacalifornica", "Octopusrubescens", "Octopusbimaculatus", "Isostichopusfuscus", "Octopussp") # removed Eucidaristhouarsii and Picnopodiasp/Heliasterkubinji; and added Haliotissp to indicate hybrid species

##############################################################################################################################################################################
# split the dataframe by 2010 - 2012 and 2014 to present and make species list appropriate for each time frame

MagdalenaInvertebrates1012 <- subset(MagdalenaInvertebrates, MagdalenaInvertebrates$Year <= 2012)

MagdalenaInvertebrates1012 <- with(MagdalenaInvertebrates1012, MagdalenaInvertebrates1012[!(Genusspecies == "Haliotissp" | is.na(Genusspecies)), ]) # remove rows with the data Halitoissp <-- no hybrid species data entry

##############################################################
MagdalenaInvertebrates14to <- subset(MagdalenaInvertebrates, MagdalenaInvertebrates$Year >= 2012)

MagdalenaInvertebrates14to <- with(MagdalenaInvertebrates14to, MagdalenaInvertebrates14to[!(Genusspecies == "Eucidaristhouarsii" | is.na(Genusspecies)), ]) # remove rows with the data Eucidaristhouarsii <- removed from monitoring
MagdalenaInvertebrates14to <- with(MagdalenaInvertebrates14to, MagdalenaInvertebrates14to[!(Genusspecies == "Sunstarsp" | is.na(Genusspecies)), ]) # remove rows with the data Sunstarsp <- removed from monitoring


#### Combine the 2 dataframes back into one
MagdalenaInvertebratesClean <- rbind(MagdalenaInvertebrates1012, MagdalenaInvertebrates14to)

################################
#### Reorder the column order, change NAs in TotalCounts to 0's, and add Concession/NoConcession data
MagdalenaInvertebratesClean <- MagdalenaInvertebratesClean[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Site", "Location", "Country", "Zone", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "TotalCounts", "Distance", "ExtrapolationValue", "Status")]

write.table(MagdalenaInvertebratesClean, "~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/Magdalena_Invertebrates_clean_data_2010_to_Present.csv", sep=",", col.names=T, row.names=F)




