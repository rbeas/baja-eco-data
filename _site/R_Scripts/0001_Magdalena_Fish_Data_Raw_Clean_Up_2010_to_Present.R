########### Reef Check Fish Data Clean Up Process ###########
# Written by: C. A. Boch, May 29, 2014
# Micheli Laboratory, Hopkins Marine Station, Stanford University

# Note: the raw data was changed by the following to keep the data structure and format in its most basic form and structure

############## Processing in Excel ##############
# Done in Excel
# 1. Made a dulicate copy of original xls file (keeps dates the same)
# 2. Deleted other worksheets (Hojas) in this duplicate file
# 3. saved the file from xls --> csv (this also gets rid of any pulldowns and formulas)
# 4. cleared the contents of all columns and rows outside of dataframe.

##  Now, the files are structure ready to be imported into R

############## Processing in R ##############

rm(list=ls())
library(zoo)
library(plyr)
library(gsubfn)

#1. read in all fish data
#2. split the data by site name because each site has a different species list
#3. split the site data into years because the RC full list changed slightly in 2013
#4. split each year data by Location -- after this, each location and year now has unique transect numbers we can split by
#5. merge the full list with each TransectNumber observations --> this only fills in the GS information
#6a. fill in the rest of the column data information for each TransectNumber -- ie location, site, latitude, etc
#6b. keep SizeClass and SexClass information as the same -- don't want to "fill in"
#7. recombine all the split data
#8. export as a table --> this is now the cleaned up processed data
### Note, although this fills in all the missing Genusspecies info, we still need to account for zeros.  This can be done by turning the SizeClass information into binary and sum the 1's (total abundance count).  O's will then represent not observed


######## Read in all raw fish data from Isla Magdalena Site (2012-)

system("ls -lt ~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Raw/*Magdalena_fishesraw* > ~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/Magdalena_2010_Reef_Check_fishesraw_filelist_latest.txt")
# load the list of files into a table
currentTable = read.table("~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/Magdalena_2010_Reef_Check_fishesraw_filelist_latest.txt",as.is=TRUE)
#  read all tables in the list
tables = list()
##read in data and attach dataframe to each file in list
for (i in 1:nrow(currentTable)){
tables[[i]] = read.csv(currentTable$V9[i], header=T, stringsAsFactors=F, fileEncoding="latin1")
}

# Fishescolumnnameslist.df <- lapply(tables, function(x){colnames(x)}) # check the colnames of all the files

# print(unique(sort(unlist(unique(Fishescolumnnameslist.df))))) # this function puts the list of column names in alphabetical order and lists the unique column names
################################################################################################################################################################################
# #### Reanmes desired columns 

tables <- lapply(tables, function(x) {colnames(x) <- tolower(colnames(x)); x}) # change all text to lower case
tables <- lapply(tables, function(x) {colnames(x) <- gsub("no..transecto", "TransectNumber", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("categoria", "MatureClass", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("fecha", "Date", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("especie", "Genusspecies", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("latitud..N.", "Latitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("latitud..n.", "Latitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("longitud..w.", "Longitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("Longitud..w.", "Longitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("observador", "Observer", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("tiempo.inicio", "TimeInitial", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("tiempo.final", "TimeFinal", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("prof.inicio..m.", "DepthInitial_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("prof.inicial..m.", "DepthInitial_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("prof.final..m.", "DepthFinal_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("prof.max..m.", "DepthMax_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("prof.x..m.", "MidTransDepth_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("temperatura...c.", "Temperature_C", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("sitio", "Location", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("visibilidad..m.", "Visibility_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("extrapolacion", "Extrapolation", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("Abundancia..NO.extrapolada", "Extrapolation", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("Abundancia.NO.extrapolada", "Extrapolation", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("abundancia.total", "Counts", colnames(x), fixed=TRUE); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("abundancia", "Counts", colnames(x), fixed=TRUE); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("distancia", "Distance", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("a.o", "Year", colnames(x), fixed=TRUE); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("Counts..no.extrapolada", "Extrapolation", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("Counts.no.extrapolada", "Extrapolation", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("observaciones", "Notes", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("Obervaciones", "Notes", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("comentarios", "Notes", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("sexo", "Sex", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("talla", "SizeClass", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("presence", "SpeciesPresOutTrans", colnames(x)); x})
################################################################################################################################################################################
### Change the names of column headings

#########
#Add any columns that need to be added if they are missing from a particular table

# MagFishescolumnnameslist.df <- lapply(tables, function(x){colnames(x)}) # check the colnames of all the files

# print(unique(sort(unlist(unique(MagFishescolumnnameslist.df))))) # this function puts the list of column names in alphabetical order and lists the unique column names

############################
##### Select the columns with data that we want from all the files and create a single raw data table
# use the function --> currentTable to check all the files have been imported
# use the function --> ls(tables[[1]]) to check all the column headings

# Date
# DepthFinal_m
# DepthInitial_m
# DepthMax_m
# Genusspecies
# Latitude
# Location
# Longitude
# MatureClass <- COBI separated Sex class and Mature Class in this dataset
# MidTransDepth_m
# Observer
# Sex
# SizeClass
# SpeciesPresOutTrans <-- species present outside the transect
# Temperature_C
# TimeFinal
# TimeInitial
# TransectNumber
# Visibility_m
# Year

#########
# Add any columns that need to be added if they are missing from a sub table
# if Error in `[.data.frame`(x, , c("Observer", "Date", "Year", "TimeInitial",  : undefined columns selected <-- usually due to a new file added --> need to recheck all the column headers again

# use currentTable to check that all files are imported; tables numbers (file) are ordered on the list; table numbers will change when new yearly monitoring data are available

# all data files have same column headings


MagdalenaFishesRaw.df <- lapply(tables, function(x){x[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Location", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "SizeClass", "Sex", "MatureClass", "SpeciesPresOutTrans")]})# Extracting columns from each dataframe only works if the column names are the same--including case!!!

MagdalenaFishesRaw <- do.call(rbind, MagdalenaFishesRaw.df) # all the tables from different files are now in a single dataframe

# need to merge the SexClass and MatureClass into one

MagdalenaFishesRaw <- sapply(MagdalenaFishesRaw, function(x) gsub("n/a", "", x, ignore.case=TRUE))
MagdalenaFishesRaw <- as.data.frame(MagdalenaFishesRaw)

MagdalenaFishesRaw <- sapply(MagdalenaFishesRaw, function(x) gsub("n/d", "", x, ignore.case=TRUE))
MagdalenaFishesRaw <- as.data.frame(MagdalenaFishesRaw)

MagdalenaFishesRaw <- sapply(MagdalenaFishesRaw, function(x) gsub(" cm", "", x, ignore.case=TRUE))
MagdalenaFishesRaw <- as.data.frame(MagdalenaFishesRaw)

x <- as.data.frame(MagdalenaFishesRaw$MatureClass)
names(x)[1]<-"Class"
y <- as.data.frame(MagdalenaFishesRaw$Sex)
names(y)[1]<-"Class"

################################################################################################################################################################################
#### Add columns with Site and Country and SexClass entries (and remove MatureClass and SexClass)
#### Convert any positive Longitude values to negative

MagdalenaFishesRaw["Country"] <- "Mexico"
MagdalenaFishesRaw["Site"] <- "IslaMagdalena"
MagdalenaFishesRaw["SexClass"] <- as.data.frame(paste(x$Class, y$Class))
MagdalenaFishesRaw["Longitude"] <- -1 * abs(as.numeric(levels(MagdalenaFishesRaw$Longitude))[MagdalenaFishesRaw$Longitude]) # NAs introduced by coercion message but script ok
MagdalenaFishesRaw["Observer"] <- sapply(MagdalenaFishesRaw$Observer, function(x) gsub("\u0096", "n", x, ignore.case=TRUE)) # replace the spanish n

MagdalenaFishesRaw$MatureClass <- NULL
MagdalenaFishesRaw$Sex <- NULL

##############################################################################################################################################################################


### Use unique() function to check for all unique entries column by column and change/replace if necessary
################################################################################################################################################################################
#### Replace old Location data with new Location names

MagdalenaFishesRaw$Location <- sapply(MagdalenaFishesRaw$Location, function(x) gsub(" ", "", x))
MagdalenaFishesRaw$Location <- sapply(MagdalenaFishesRaw$Location, function(x) gsub("profundo", "", x))
MagdalenaFishesRaw$Location <- sapply(MagdalenaFishesRaw$Location, function(x) gsub("somero", "", x))

MagdalenaFishesRaw <- as.data.frame(sapply(MagdalenaFishesRaw, function(x) gsub("PuntaBlancaTepetate", "PuntaBlanca", x, ignore.case=TRUE))) # change two site names
MagdalenaFishesRaw <- as.data.frame(sapply(MagdalenaFishesRaw, function(x) gsub("PuntaBlancaGarropas", "PuntaBlanca", x, ignore.case=TRUE))) # change two site names

###############################################################################################################################################################################
#### Replace Zone designation from location information to Reserve or Fished (easier to remember) --> future designation should be based on GPS coordinates

MagdalenaFishesRaw$Zone <- ifelse(MagdalenaFishesRaw$Location %in% c("PuntaBlanca", "Garropas"), "Reserve", ifelse(MagdalenaFishesRaw$Location %in% c("ElProgresista", "ElAbolladero", "LosCabitos"), "Fished", ""))

################################################################################################################################################################################
#### Replace spanish Genusspecies with Latin names 

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("\u0096", "n", x, ignore.case=TRUE)) # replace the spanish n
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("\u0097", "o", x, ignore.case=TRUE)) # replace the spanish a
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub(" ", "", x)) # replace any spaces
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("0", "", x)) # replace any spaces

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("AngelRey", "Holacanthuspasser", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("AngeldeCortez", "Pomacanthuszonipectus", x, ignore.case=TRUE)) 
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("AngelCortez", "Pomacanthuszonipectus", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Baya", "Mycteropercasp", x, ignore.case=TRUE)) 

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Castanetacoladetijera", "Chromisatrilobata", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("ChivoBarbon", "Mulloidichthysdentatus", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Cotorrodienteflojo", "Nicholsinadenticulata", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("CabrillaSardinera", "Mycteropercarosacea", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("cabrillasargacera", "Paralabraxclathratus", x, ignore.case=TRUE))
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("cabrillasargasera", "Paralabraxclathratus", x, ignore.case=TRUE)) 

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Cabrillapinta", "Epinephelusanalogus", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("CabrillacoladeEscoba", "Mycteropercaprionura", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Curricata", "Paralabraxmaculatofasciatus", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("IdoloMoro", "Zancluscornutus", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Meluda", "Calamusbrachysomus", x, ignore.case=TRUE)) 

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) 
 gsub("Mariposabarbera", "Johnrandallianigrirostris", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Mariposamuneca", "Chaetodonhumeralis", x, ignore.case=TRUE))
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Maiposamuneca", "Chaetodonhumeralis", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("mojarramueluda", "Calamusbrachysomus", x, ignore.case=FALSE))
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("mojarron", "Calamusbrachysomus", x, ignore.case=FALSE))
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Mueluda", "Calamusbrachysomus", x, ignore.case=FALSE))
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("mueluda", "Calamusbrachysomus", x, ignore.case=FALSE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Naranjito", "Hypsypopsrubicundus", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Sargorayado", "Anisotremusdavidsoni", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Tiburonperro", "Heterodontusfrancisci", x, ignore.case=TRUE))

MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Verdillo", "Paralabraxnebulifer", x, ignore.case=TRUE))


MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Viejacalifornica", "Semicossyphuspulcher", x, ignore.case=FALSE)) 
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Viejacalifornia", "Semicossyphuspulcher", x, ignore.case=FALSE))
MagdalenaFishesRaw$Genusspecies <- sapply(MagdalenaFishesRaw$Genusspecies, function(x) gsub("Vieja", "Semicossyphuspulcher", x, ignore.case=FALSE))

###############################################################################################################################################################################
#### Replace unique entries of SexClass so all entries are either M (M), F (Female) or J (Juvenile), A (Adult)

MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub(" ", "", x))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("JHembra", "J", x))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("hembra", "F", x, ignore.case=TRUE))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("H", "F", x))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("Amacho", "M", x, ignore.case=TRUE))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("macho", "M", x, ignore.case=TRUE))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("a", "A", x, ignore.case=TRUE))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("j", "J", x))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("AM", "M", x, ignore.case=TRUE))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("AA", "A", x))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("Ah", "A", x))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("JJ", "J", x))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("JF", "J", x))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("FF", "F", x, ignore.case=TRUE))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("hh", "F", x))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("Mm", "M", x, ignore.case=TRUE))
MagdalenaFishesRaw$SexClass <- sapply(MagdalenaFishesRaw$SexClass, function(x) gsub("m", "M", x, ignore.case=FALSE))
# data is now in either lower or upper case of SexClass: M (M), F (Female) or J (Juvenile), A (Adult)

###############################################################################################################################################################################
#### Remove lobos and focas from dataframe--not on the RC list

MagdalenaFishesRaw <- with(MagdalenaFishesRaw, MagdalenaFishesRaw[!(Genusspecies == "LoboMarino" | is.na(Genusspecies)), ]) # remove rows with the data lobo because these are not on the RC species list but was entered in data

###############################################################################################################################################################################

#### Reorder the column order because Zone column was addedd previously

MagdalenaFishesRaw <- MagdalenaFishesRaw[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Location", "Site", "Zone", "Country", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "SizeClass", "SexClass", "SpeciesPresOutTrans")]

###############################################################################################################################################################################
### Fill in data for zeros for all of RC Fish data
### A bit complicated for the whole dataframe but worth it
### Note, 2011 data file has some missing transect numbers in the original raw file; author manually entered unique transect numbers for each location and dive; if NAs in TransectNumber, the routine below does not work

r<-rle(c(MagdalenaFishesRaw$TransectNumber))$lengths # index the dataframe by TransectNumber --> TransectNumber were entered in clusters; essentially, this represents each page of the dataset entered!

MagdalenaFishesRaw.df <- split(MagdalenaFishesRaw, rep(seq_along(r), r)) ## this is the bread and butter!!!  double checked that it works!

## Now, we need to fill in each subdataframe by the full RCGS list to fill in zeroes

### Below is the full list of Reef Check species for I. Magdalena; list changed in 2013

RCGS <- c("Semicossyphuspulcher", "Calamusbrachysomus", "Hypsypopsrubicundus", "Chaetodonhumeralis", "Paralabraxnebulifer", "Holocanthuspasser", "Johnrandallianigrirostris", "Pomacanthuszonipectus", "Chromisatrilobata", "Mycteropercaprionura", "Nicholsinadenticulata", "Mulloidichthysdentatus", "Mycteropercarosacea", "Anisotremusdavidsonii", "Paralabraxclathratus", "Mycteropercaxenarcha", "Mycteropercajordani", "Epinephelusanalogus", "Epinephelusitajara", "Zancluscornutus", "Paralabraxmaculatofasciatus", "Stereolepisgigas")
RCGS <-as.data.frame(RCGS)
names(RCGS)[1] <- "Genusspecies"

###############################################################################################################################################################################

# merge function to match the RCGS list with the raw data

CompleteFishRaw.df <- lapply(MagdalenaFishesRaw.df, function(x, y) {merge(x, y, by.x=names(x)[19], by.y=names(y)[1], all=T)}, RCGS)

# fill in the NA with the corresponding transect metadata -- i.e., location, site, latitude, etc...
FillNA <- function(z){
	SizeSex = z[,20:22]
	# SizeSex[is.na(SizeSex)] <- "" # remove the NAs created by the above splitting
	YY = na.locf(z[,1:19], na.rm=TRUE, fromLast=TRUE) # fill in
	YYY = na.locf(YY, na.rm=TRUE) # fill in
	NewFill = cbind(YYY, SizeSex) # combine all the columns back again
}

CompleteFish.df<-lapply(CompleteFishRaw.df, FillNA)

CompleteFish<-do.call(rbind, CompleteFish.df)

MagdalenaFishes <- with(CompleteFish, CompleteFish[!(Genusspecies == "" | is.na(Genusspecies)), ]) # delete all rows with no Genusspecies in the dataframe.  When the raw data is expanded to include zero observations for a trasect, the merging function creates an additional row that is unnecessary.  So this function looks for cells with blank values in the Genusspecies column and deletes these blank rows.

### Reorder the column order

MagdalenaFishes <- MagdalenaFishes[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Location", "Site", "Zone", "Country", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "SizeClass", "SexClass", "SpeciesPresOutTrans")]

MagdalenaFishes$Present <- ifelse(MagdalenaFishes$SizeClass == "NA", 0, 1) # add a column called Present and fill with binary data: if a fish species was present in the transect (as indicated by SizeClass entry), then place 1 in the corresponding column.  0 would then indicate NAs or absence of fish in each row 
MagdalenaFishes$Present[is.na(MagdalenaFishes$Present)] <- "0" # but since R doesn't place the NAs and replace it with 0's in the above function, this function does it after the fact
MagdalenaFishes[, "Present"] <- as.numeric(as.character(MagdalenaFishes[, "Present"])) # R also interprets the binary data as characters but we need it as numbers so we can count the 1's or 0's; this function changes the binary data from characters to numeric values

### ********** Final Column Headings (same as Natividad data)

# Observer	
# Date <-- Local Date
# Year	
# TimeInitial <-- Local Time
# TimeFinal <- Local Time
# TransectNumber
# Location
# Site <-- Baja site
# Zone <-- Reserve or Fished
# Country	
# DepthInitial_m
# DepthFinal_m
# DepthMax_m
# MidTransDepth_m
# Latitude
# Longitude	
# Temperature_C
# Visibility_m
# Genusspecies
# SizeClass <-- not modfied in any way
# SexClass <- Adult, Female, Male, Juvenile
# SpeciesPresOutTrans <- 1 is for present, 0 absent, NA
# Present <-- 1 is for present in transect, 0 absent in transect

write.table(MagdalenaFishes, "~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/Magdalena_fishes_clean_data_2010_to_Present.csv", sep=",", col.names=T, row.names=F)




