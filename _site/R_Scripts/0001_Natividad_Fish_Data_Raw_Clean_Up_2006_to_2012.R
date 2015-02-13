########### Reef Check Fish Data Clean Up Process ###########
# Written by: C. A. Boch, May 29, 2014
# Micheli Laboratory, Hopkins Marine Station, Stanford University

# Note: the raw data was changed by the following to keep the data structure and format in its most basic form and structure
# Process the data to make sure the number of columns, column names and column order are all the same as previous files --> much easier to analyzie data in a single structure

############## Processing in Excel ##############
# Done in Excel
# 1. Make a dulicate copy of original xls file (keeps dates the same)
# 2. Deleted other worksheets (Hojas) in this duplicate file
# 3. saved the file from xls --> csv (this also gets rid of any pulldowns and formulas)
# 4. cleared the contents of all columns and rows outside of dataframe.
# 5. Manually changed all date formats to m/dd/yy (easier in Excel)

##  Now, the file is structure ready to be imported into R

############## Processing in R ##############

rm(list=ls())
library(zoo)
library(plyr)
library(gsubfn)

#1. read in the fish data file
#2. split the data by site name because each site has a different species list
#3. split the site data into years because the RC full list changed slightly in 2013
#4. split each year data by Location -- after this, each location and year now has unique transect numbers we can split by
#5. merge the full list with each TransectNumber observations --> this only fills in the GS information
#6a. fill in the rest of the column data information for each TransectNumber -- ie location, site, latitude, etc
#6b. keep SizeClass and SexClass information as the same -- don't want to "fill in"
#7. recombine all the split data
#8. export as a table --> this is now the cleaned up processed data
### Note, although this fills in all the missing Genusspecies info, we still need to account for zeros.  This can be done by turning the SizeClass information into binary and sum the 1's (total abundance count).  O's will then represent not observed


######## Read in all raw fish data from Isla Natividad Site (2006-2012)

NatividadFishesRaw <- read.csv("~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Raw/Natividad_fishesraw_2006_2012.csv", header=T, stringsAsFactors=F, fileEncoding="latin1")

################################################################################################################################################################################
#### Remove unecessary columns of data

NatividadFishesRaw$codigo <- NULL
NatividadFishesRaw$tiempo.total <- NULL
NatividadFishesRaw$no..buceo <- NULL
NatividadFishesRaw$epoca <- NULL
NatividadFishesRaw$no..replica <- NULL
NatividadFishesRaw$sitio.en.extenso <- NULL
NatividadFishesRaw$prof.inicial..ft. <- NULL
NatividadFishesRaw$prof.final..ft. <- NULL
NatividadFishesRaw$prof.max..ft. <- NULL
NatividadFishesRaw$prof.X..ft. <- NULL
NatividadFishesRaw$temperatura...F. <- NULL
NatividadFishesRaw$Direccion <- NULL
NatividadFishesRaw$Observaciones <- NULL
NatividadFishesRaw$X <- NULL

# ################################################################################################################################################################################
#### Change the name of column headings

names(NatividadFishesRaw)[1]<-"Observer"
names(NatividadFishesRaw)[2]<-"Date"
names(NatividadFishesRaw)[3]<-"Year"
names(NatividadFishesRaw)[4]<-"TimeInitial"
names(NatividadFishesRaw)[5]<-"TimeFinal"
names(NatividadFishesRaw)[6]<-"TransectNumber"
names(NatividadFishesRaw)[7]<-"Location"
names(NatividadFishesRaw)[8]<-"Zone"
names(NatividadFishesRaw)[9]<-"DepthInitial_m"
names(NatividadFishesRaw)[10]<-"DepthFinal_m"
names(NatividadFishesRaw)[11]<-"DepthMax_m"
names(NatividadFishesRaw)[12]<-"MidTransDepth_m"
names(NatividadFishesRaw)[13]<-"Latitude"
names(NatividadFishesRaw)[14]<-"Longitude"
names(NatividadFishesRaw)[15]<-"Temperature_C"
names(NatividadFishesRaw)[16]<-"Visibility_m"
names(NatividadFishesRaw)[17]<-"Genusspecies"
names(NatividadFishesRaw)[18]<-"SizeClass"
names(NatividadFishesRaw)[19]<-"SpeciesPresOutTrans"
names(NatividadFishesRaw)[20]<-"SexClass"

################################################################################################################################################################################
#### Add column with Site and Country and a searchable date format

NatividadFishesRaw["Country"] <- "Mexico"
NatividadFishesRaw["Site"] <- "IslaNatividad"

NatividadFishesRaw$Observer <- sapply(NatividadFishesRaw$Observer, function(x) gsub("\u0096", "n", x, ignore.case=TRUE)) # replace the spanish n

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("\u0096", "n", x, ignore.case=TRUE)) # replace the spanish n

# ################################################################################################################################################################################
# #### Reorder the column order

NatividadFishesRaw <- NatividadFishesRaw[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Location", "Site", "Zone", "Country", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "SizeClass", "SexClass", "SpeciesPresOutTrans")]

#### Use unique() function to check for all unique entries column by column and change/replace if necessary
################################################################################################################################################################################
#### Replace old Location (sitioenextenso) data with new Location names

NatividadFishesRaw$Location <- sapply(NatividadFishesRaw$Location, function(x) gsub(" ", "", x))
NatividadFishesRaw$Location <- sapply(NatividadFishesRaw$Location, function(x) gsub("LaPlana/LasCuevas", "LaPlana", x))
NatividadFishesRaw$Location <- sapply(NatividadFishesRaw$Location, function(x) gsub("Puntaprieta", "PuntaPrieta", x)) #w/o comma between Natividad and Baja

################################################################################################################################################################################
#### Replace Zone designation from location information to Reserve or Fished (easier to remember) --> future designation should be based on GPS coordinates

NatividadFishesRaw$Zone <- ifelse(NatividadFishesRaw$Location %in% c("PuntaPrieta", "LaPlana"), "Reserve", ifelse(NatividadFishesRaw$Location %in% c("LaDulce", "Babencho", "LaBarrita", "MorroPrieto", "LaGuanera", "LaBarrita"), "Fished", ""))

###############################################################################################################################################################################
#### Replace n/a, n/d from data -- not consistenetly entered

NatividadFishesRaw$SizeClass <- sapply(NatividadFishesRaw$SizeClass, function(x) gsub("n/a", "", x, ignore.case=TRUE))
NatividadFishesRaw$SizeClass <- sapply(NatividadFishesRaw$SizeClass, function(x) gsub("n/d", "", x, ignore.case=TRUE))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("n/a", "", x, ignore.case=TRUE))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("n/d", "", x, ignore.case=TRUE))
NatividadFishesRaw$DepthFinal_m <- sapply(NatividadFishesRaw$DepthFinal_m, function(x) gsub("n/d", "", x, ignore.case=TRUE))
NatividadFishesRaw$DepthMax_m <- sapply(NatividadFishesRaw$DepthFinal_m, function(x) gsub("n/d", "", x, ignore.case=TRUE))
NatividadFishesRaw$Temperature_C <- sapply(NatividadFishesRaw$Temperature_C, function(x) gsub("n/d", "", x, ignore.case=TRUE))

################################################################################################################################################################################
#### Replace spanish Genusspecies with Latin names but need to split the dataframe because the species list changes in 2013

### Replace Spanish names with Latin Genusspecies names in 2006-2012 data

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub(" ", "", x)) # replace any spaces

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Blanco", "Caulolatilusprinceps", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Cabezon", "Scorpaenichthysmarmoratus", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Cabrillaamarilla", "Paralabraxclathratus", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("chopaverde", "Girellanigricans", x, ignore.case=TRUE)) # need to place the space in front of verde to get rid it
NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Chopa", "Girellanigricans", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Chromis", "Chromispunctipinnis", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Guitarra", "Rhinobatosproductus", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Meronegro", "Stereolepisgigas", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Mojarranegra", "Embioticajacksoni", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Molva", "Ophiodonelongatus", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("naranjito", "Hypsypopsrubicundus", x, ignore.case=TRUE)) # different spelling found using unique()

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Perro", "Heterodontusfrancisci", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Rocote", "Sebastessp", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Roncador", "Anisotremusdavidsoni", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Sargacerito", "Halichoeressemicintus", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Senorita", "Oxyjuliscalifornica", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Verdillo", "Paralabraxnebulifer", x, ignore.case=TRUE))

NatividadFishesRaw$Genusspecies <- sapply(NatividadFishesRaw$Genusspecies, function(x) gsub("Vieja", "Semicossyphuspulcher", x, ignore.case=TRUE))

################################################################################################################################################################################
#### Categorize SizeClass into bins 

# ###############################################################################################################################################################################
#### Replace unique entries of SexClass so all entries are either M (M), F (Female) or J (Juvenile), A (Adult)

NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("macho", "M", x, ignore.case=TRUE))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("hembra", "F", x, ignore.case=TRUE))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("adulto", "A", x, ignore.case=TRUE))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("juvenil", "J", x, ignore.case=TRUE))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("j", "J", x))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("m", "M", x))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("a", "A", x))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("H", "F", x, ignore.case=TRUE))
NatividadFishesRaw$SexClass <- sapply(NatividadFishesRaw$SexClass, function(x) gsub("0", "", x))

# data is now in either lower or upper case of SexClass
# print(unique(NatividadFishesRaw$SexClass)) #to check for any outliers

###############################################################################################################################################################################
#### Remove lobos and focas from dataframe--not on the RC list

NatividadFishesRaw <- with(NatividadFishesRaw, NatividadFishesRaw[!(Genusspecies == "Lobo" | is.na(Genusspecies)), ]) # remove rows with the data lobo because these are not on the RC species list but was entered in data

NatividadFishesRaw <- with(NatividadFishesRaw, NatividadFishesRaw[!(Genusspecies == "Foca" | is.na(Genusspecies)), ]) # remove rows with the data foca because these are not on the RC species list but was entered in data

# could do the same with Rhinobatosproductus but it can be taken out later in the analyses

################################################################################################################################################################################
#### Fill in data for zeros for all of RC Fish data
#### A bit complicated for the whole dataframe but worth it

r<-rle(c(NatividadFishesRaw$TransectNumber))$lengths # index the dataframe by TransectNumber --> TransectNumber were entered in clusters; essentially, this represents each page of the dataset entered!

NatividadFishesRaw.df <- split(NatividadFishesRaw, rep(seq_along(r), r)) ## this is the bread and butter!!!  double checked that it works!

## Now, we need to fill in each subdataframe by the full RCGS list to fill in zeroes

### Below is the full list of Reef Check species from 2006 - 2012 in I. Natividad; list changed in 2013

RCGS <- c("Paralabraxclathratus", "Paralabraxnebulifer", "Hypsypopsrubicundus", "Chromispunctipinnis", "Girellanigricans", "Anisotremusdavidsoni", "Embioticajacksoni", "Semicossyphuspulcher", "Sebastessp", "Oxyjuliscalifornica", "Halichoeressemicintus", "Caulolatilusprinceps", "Heterodontusfrancisci", "Stereolepisgigas", "Mycteropercajordani", "Mycteropercaxenarcha", "Scorpaenichthysmarmoratus", "Ophiodonelongatus", "Squatinacalifornica", "Rhacochilusvacca") 
RCGS <-as.data.frame(RCGS)
names(RCGS)[1] <- "Genusspecies"

################################################################################################################################################################################

# merge function to match the RCGS list with the raw data

CompleteFishRaw.df <- lapply(NatividadFishesRaw.df, function(x, y) {merge(x, y, by.x=names(x)[19], by.y=names(y)[1], all=T)}, RCGS)

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

NatividadFishes <- with(CompleteFish, CompleteFish[!(Genusspecies == "" | is.na(Genusspecies)), ]) # delete all rows with no Genusspecies in the dataframe.  When the raw data is expanded to include zero observations for a trasect, the merging function creates an additional row that is unnecessary.  So this function looks for cells with blank values in the Genusspecies column and deletes these blank rows.

#### Reorder the column order 

NatividadFishes <- NatividadFishes[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "TransectNumber", "Location", "Site", "Zone", "Country", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "MidTransDepth_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "SizeClass", "SexClass", "SpeciesPresOutTrans")]

NatividadFishes$Present <- ifelse(NatividadFishes$SizeClass == "NA", 0, 1) # add a column called Present and fill with binary data: if a fish species was present in the transect (as indicated by SizeClass entry), then place 1 in the corresponding column.  0 would then indicate NAs or absence of fish in each row 
NatividadFishes$Present[is.na(NatividadFishes$Present)] <- "0" # but since R doesn't place the NAs and replace it with 0's in the above function, this function does it after the fact
NatividadFishes[, "Present"] <- as.numeric(as.character(NatividadFishes[, "Present"])) # R also interprets the binary data as characters but we need it as numbers so we can count the 1's or 0's; this function changes the binary data from characters to numeric values

NatividadFishes0612 <- NatividadFishes

write.table(NatividadFishes0612, "~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/Natividad_fishes_clean_data_2006_to_2012.csv", sep=",", col.names=T, row.names=F)




