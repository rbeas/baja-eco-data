########### Reef Check Abalone Timed Searches Raw Data Clean Up ###########
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
# 5. changed the date formatting to m/dd/yy (to keep all of them same)

##  Now, the file/s are structure ready to be imported into R

## Road Map: Bring in all data from all sites --> retrieve columns with desired data --> rename columns with english names --> remove all unecessary characters --> change spanish to latin Genusspecies --> add Zone, Country, Site data --> output a single file with all the data

############## Processing in R ##############

rm(list=ls())
library(compare) # output common items in a list
library(zoo)
library(plyr)
library(gsubfn)
library(reshape) # for melt function

######## Read in all raw abalone timed searches data from All Sites (2006- )

system("ls -lt ~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Raw/*abalonetimedsearchesraw* > ~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/All_Reef_Check_abalonetimedsearchesraw_filelist_latest.txt")
# load the list of files into a table
currentTable = read.table("~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/All_Reef_Check_abalonetimedsearchesraw_filelist_latest.txt",as.is=TRUE)
#  read all tables in the list
tables = list()
##read in data and attach dataframe to each file in list
for (i in 1:nrow(currentTable)){
tables[[i]] = read.csv(currentTable$V9[i], header=T, stringsAsFactors=F, fileEncoding="latin1")
}

################################################################################################################################################################################
#### Rename desired columns

tables <- lapply(tables, function(x) {colnames(x) <- tolower(colnames(x)); x}) # change all text to lower case
tables <- lapply(tables, function(x) {colnames(x) <- gsub("\\.", "", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("notransecto", "DiveNumber_Transect", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("busqueda", "DiveNumber_Transect", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("condition", "Condition", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("condicin", "Condition", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("fecha", "Date", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("especie", "Genusspecies", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("latituden", "Latitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("latitudn", "Latitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("longitudw", "Longitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("longitudew", "Longitude", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("observador", "Observer", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("oservador", "Observer", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("parche", "Patch", colnames(x), ignore.case=F); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("Parche", "Patch", colnames(x), ignore.case=F); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("tiempoinicialti", "TimeInitial", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("tiempoinicio", "TimeInitial", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("tiempofinal", "TimeFinal", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("profiniciom", "DepthInitial_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("profinicialm", "DepthInitial_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("proffinm", "DepthFinal_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("proffinalm", "DepthFinal_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("profmaxm", "DepthMax_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("tempc", "Temperature_C", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("temc", "Temperature_C", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("temperaturac", "Temperature_C", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("sitio", "Location", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("visibilidadm", "Visibility_m", colnames(x), ignore.case=F); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("visibilidad", "Visibility_m", colnames(x), ignore.case=F); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("vism", "Visibility_m", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("abundancia", "Abundance", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("talla", "Diameter_cm", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("ao", "Year", colnames(x)); x})
tables <- lapply(tables, function(x) {colnames(x) <- gsub("year", "Year", colnames(x)); x})

AbTimedSearchescolumnnameslist.df <- lapply(tables, function(x){colnames(x)}) # check the colnames of all the files

print(unique(sort(unlist(unique(AbTimedSearchescolumnnameslist.df))))) # this function puts the list of column names in alphabetical order and lists the unique column names


############################
##### Select the columns with data that we want from all the files and create a single raw data table

# use the function --> ls(tables[[1]]) to check all the column headings

# Abundance <-- sum of abalone in the same size across patches during one dive search; calculated value; keep in data frame but these values are not total abundance
# Condition
# Date
# DepthFinal_m
# DepthInitial_m
# DepthMax_m
# Diameter_cm
# DiveNumber_Transect
# Genusspecies
# Latitude
# Location
# Longitude
# Observer
# Patch <- sum aggregate as a function of patch and dive number will give the sum of abundance in the patch; note: when Patch was left blank, this means there were no patches of abalone found during the dive; NA's indicate missing data
# Temperature_C
# TimeFinal
# TimeInitial
# Visibility_m
# Year
# Diameter_cm_Corrected <- add this column later in the script
#########
#Add any columns that need to be added if they are missing from a sub table
# if Error in `[.data.frame`(x, , c("Observer", "Date", "Year", "TimeInitial",  : undefined columns selected <-- usually due to a new file added --> need to recheck all the column headers again

# use currentTable to chjeck that all files are imported; tables numbers (file) are oredered on the list

tables[[10]]["Abundance"] <- NA # this Magdalena 2011 fulgens file has no abundance column
tables[[10]]["Condition"] <- ""# this Magdalena 2011 fulgens file has no condition column
tables[[10]]["Year"] <- 2011 # this Magdalena 2011 fulgens file has no Year column
tables[[11]]["Condition"] <- "" # this Magdalena 2010 fulgens file has no condition column
tables[[14]]["Condition"] <- "" # this Magdalena 2011 corrugata file has no condition column
tables[[14]]["Year"] <- 2011 # this Magdalena 2011 corrugata file has no year column
tables[[15]]["Condition"] <- "" # this Magdalena 2010 corrugata file has no condition column

AbTimedSearches.df <- lapply(tables, function(x){x[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "DiveNumber_Transect", "Location", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "Diameter_cm", "Patch", "Abundance", "Condition")]})# Extracting columns from each dataframe only works if the column names are the same--including case!!!

AllLocationsAbTimedSearchesRaw <- do.call(rbind, AbTimedSearches.df)

###############################################################################################################################################################################
#### Replace "funky" characters

AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("\u0096", "n", x, ignore.case=TRUE))) # replace the spanish n
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("\u009c", "u", x, ignore.case=TRUE))) # replace the spanish u
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("n/d", "", x, ignore.case=TRUE))) # replace n/d
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("n/a", "", x, ignore.case=TRUE))) # replace n/a
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub(" ", "", x, ignore.case=TRUE))) # replace spaces
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("-", "", x, ignore.case=TRUE))) # replace dashes
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("profundo", "", x, ignore.case=TRUE))) # remove depth information from Location
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("somero", "", x, ignore.case=TRUE))) # remove depth information from Location
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("Somero", "", x, ignore.case=TRUE))) # remove depth information from Location
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("PuntaBlancaGarropas", "PuntaBlanca", x, ignore.case=TRUE))) # change two site names
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("PuntaBlancaTepetate", "PuntaBlanca", x, ignore.case=TRUE))) # change two site names
AllLocationsAbTimedSearchesRaw <- as.data.frame(sapply(AllLocationsAbTimedSearchesRaw, function(x) gsub("LaPlana/LasCuevas", "LaPlana", x, ignore.case=TRUE))) # replace any dual named data

###############################################################################################################################################################################
#### Add column with Site and Country and a searchable date format

AllLocationsAbTimedSearchesRaw["Site"] <- ifelse(AllLocationsAbTimedSearchesRaw$Location %in% c("ElProgresista", "ElAbolladero", "Garropas", "LosCabitos", "PuntaBlanca"), "IslaMagdalena", ifelse(AllLocationsAbTimedSearchesRaw$Location %in% c("Anegados", "Babencho", "CaboPruneda", "ElNido", "ElTivo", "ElTriangulo", "LaBarrita", "LaDulce", "LaGuanera", "LaPlana", "LaVela", "LomaLinda", "MorroPrieto", "PiedrasAltas", "PuntaPrieta"), "IslaNatividad", "")) # add new locations as necessary (eg when Rosario data is added)

AllLocationsAbTimedSearchesRaw["Country"] <- "Mexico"

################################################################################################################################################################################
#### Make all values for Longitude negative to denote W

AllLocationsAbTimedSearchesRaw["Longitude"] <- -1 * abs(as.numeric(paste(AllLocationsAbTimedSearchesRaw$Longitude))) # need to convert the factor values to numeric first

################################################################################################################################################################################
#### Designate "Fished" or "Reserve" according to location (in the future, this should be done by Latitude and Longitude)

AllLocationsAbTimedSearchesRaw$Zone <- ifelse(AllLocationsAbTimedSearchesRaw$Location %in% c("PuntaBlanca", "Garropas", "ElProgresista", "PuntaPrieta", "LaPlana"), "Reserve", ifelse(AllLocationsAbTimedSearchesRaw$Location %in% c("Anegados", "Babencho", "CaboPruneda", "ElAbolladero", "ElNido", "ElTivo", "ElTriangulo", "LaDulce", "LaBarrita", "LaGuanera", "LaBarrita", "LaVela", "LomaLinda", "LosCabitos", "PiedrasAltas", "MorroPrieto"), "Fished", "")) # add new locations as necessary (eg when Rosario data is added)

###############################################################################################################################################################################
### Replace any spanish names with Genusspecies in all data

AllLocationsAbTimedSearchesRaw$Genusspecies <- sapply(AllLocationsAbTimedSearchesRaw$Genusspecies, function(x) gsub("Azul", "Haliotisfulgens", x, ignore.case=TRUE)) #
AllLocationsAbTimedSearchesRaw$Genusspecies <- sapply(AllLocationsAbTimedSearchesRaw$Genusspecies, function(x) gsub("Amarillo", "Haliotiscorrugata", x, ignore.case=TRUE)) #

################################################################################################################################################################################
#### Replace Spanish with English in Condition column

AllLocationsAbTimedSearchesRaw$Condition <- sapply(AllLocationsAbTimedSearchesRaw$Condition, function(x) gsub("Enfermo", "Unhealthy", x, ignore.case=TRUE)) #
AllLocationsAbTimedSearchesRaw$Condition <- sapply(AllLocationsAbTimedSearchesRaw$Condition, function(x) gsub("Sano", "Healthy", x, ignore.case=TRUE)) #
AllLocationsAbTimedSearchesRaw$Condition <- sapply(AllLocationsAbTimedSearchesRaw$Condition, function(x) gsub("Muerto", "Dead", x, ignore.case=TRUE)) #

################################################################################################################################################################################
#### Round integer values and make numeric

AllLocationsAbTimedSearchesRaw[, "DepthInitial_m"] <- round(as.numeric(as.character(AllLocationsAbTimedSearchesRaw$DepthInitial_m))) # round up to nearest integer
AllLocationsAbTimedSearchesRaw[, "DepthFinal_m"] <- round(as.numeric(as.character(AllLocationsAbTimedSearchesRaw$DepthFinal_m))) # round up to nearest integer
AllLocationsAbTimedSearchesRaw[, "DepthMax_m"] <- round(as.numeric(as.character(AllLocationsAbTimedSearchesRaw$DepthMax_m))) # round up to nearest integer
AllLocationsAbTimedSearchesRaw[, "Visibility_m"] <- round(as.numeric(as.character(AllLocationsAbTimedSearchesRaw$Visibility_m))) # round up to nearest integer
AllLocationsAbTimedSearchesRaw[, "Diameter_cm"] <- round(as.numeric(as.character(AllLocationsAbTimedSearchesRaw$Diameter_cm))) # round up to nearest integer

################################################################################################################################################################################
#### Other random clean up -- sequence of functions matter

AllLocationsAbTimedSearchesRaw$DepthInitial_m <- as.numeric(as.character(sapply(AllLocationsAbTimedSearchesRaw$DepthInitial_m, function(x) gsub("41", "14", x, ignore.case=TRUE)))) # wrong depth entry

AllLocationsAbTimedSearchesRaw$Diameter_cm <- ifelse(AllLocationsAbTimedSearchesRaw$Diameter_cm < 1, "", AllLocationsAbTimedSearchesRaw$Diameter_cm) # change 0 entries to NAs

AllLocationsAbTimedSearchesRaw[, "Diameter_cm"] <- as.numeric(as.character(AllLocationsAbTimedSearchesRaw$Diameter_cm)) # change back to numeric because the above function turns objects into factors

AllLocationsAbTimedSearchesRaw$Patch[ AllLocationsAbTimedSearchesRaw$Patch == ""] <- 0 # when Patch was left blank, this means there were no patches of abalone found during the dive; NA's indicate missing data

AllLocationsAbTimedSearchesRaw$Diameter_cm_Corrected <-  (0.709 * AllLocationsAbTimedSearchesRaw$Diameter_cm) + 1.157 # this formula needs to be double vetted; sourced from COBI raw data files

AllLocationsAbTimedSearchesRaw[, "Diameter_cm_Corrected"] <- round(as.numeric(as.character(AllLocationsAbTimedSearchesRaw$Diameter_cm_Corrected))) # round up to nearest integer

################################################################################################################################################################################
#### Final Cleanup, Column Order, and write to a single overall file but missing values (NAs and double checking data entry could still be done); otherwise, pretty clean

AllAbTimedSearchesClean <- AllLocationsAbTimedSearchesRaw[, c("Observer", "Date", "Year", "TimeInitial", "TimeFinal", "DiveNumber_Transect", "Country", "Site", "Location", "Zone", "DepthInitial_m", "DepthFinal_m", "DepthMax_m", "Latitude", "Longitude", "Temperature_C", "Visibility_m", "Genusspecies", "Patch", "Diameter_cm", "Abundance", "Condition", "Diameter_cm_Corrected")]

write.table(AllAbTimedSearchesClean, "~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/All_locations_Reef_Check_abaloneTimedSearches_data_latest.csv", sep=",", col.names=T, row.names=F)


