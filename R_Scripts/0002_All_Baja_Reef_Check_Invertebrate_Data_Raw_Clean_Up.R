######### Run all the scripts files to clean up the invertebrates raw data
### By C. Boch Ph.D June 2, 2014
### Micheli Laboratory, Hopkins Marine Station, Stanford Univerisity
#Import all Reef Check data and bind the multiple files into a single file to work from.

rm(list=ls()) # Remove all variables from R

### Read the list of data processing scripts
file.sources = list.files(pattern="*Invertebrates_Data_Raw_Clean_Up") 

### Run those processing scripts
sapply(file.sources, source)

#####Access the folder containing the files and create table containing the list of files. Then redirect the output to as a text file (tab delimited) and store in a known place.

## In this case, we want all the clean data that contains the Reef Check invertebrate data.

system("ls -lt ~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/*Invertebrates_clean_data* > ~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/ReefCheckInvertebratefile.txt") # create a table containing the file names containing "invertebrates" in the file name

# now you can load the list table --> this tells R that these files have the information you want

currentTable = read.table("~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/ReefCheckInvertebratefile.txt",as.is=TRUE) # read in this table of file name information. as.is true says the file contains characters and numerics.

#  read all tables in the list

tables = list() #this creates the overall data.frame space and the for routine below fills in the data.frame space with data from each row of the currentTable table

for (i in 1:nrow(currentTable)){
tables[[i]] = read.csv(currentTable$V9[i],as.is=TRUE)
}

########Now that all the tables are in a singel space, we can use rbind to create a single data.frame with a single column name heading

AllBajaInvertebrates = do.call(rbind,tables) ### ReefCheckInverts now contains all the data from all the Reef Check sites.

write.csv(AllBajaInvertebrates, "~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/All_locations_Reef_Check_invertebrates_data_latest.csv", row.names=FALSE)