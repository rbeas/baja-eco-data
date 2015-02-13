######### Run all the scripts files to clean up the raw data for fish raw data
### By C. Boch Ph.D June 2, 2014
### Micheli Laboratory, Hopkins Marine Station, Stanford Univerisity

rm(list=ls())

### Read the list of data processing scripts
file.sources = list.files(pattern="*Fish_Data_Raw_Clean_Up") 

### Run those processing scripts
sapply(file.sources, source)

### the above returns cleaned up .csv files for each dataset entry but does not creat a single dataframe of AllBajaFishes to work from
### Thus, run a rbind command

# access the dataset folder and create table containing the list of files and then redirect the output to a text file

system("ls -lt ~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/*fishes_clean_data* > ~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/All_Reef_Check_fishes_filelist_latest.txt")

# load the list of files into a table
currentTable = read.table("~/Desktop/NSF_CHN/Ecology/ReefCheck/Sandbox/All_Reef_Check_fishes_filelist_latest.txt",as.is=TRUE)

#  read all tables in the list
tables = list()

##read in data and attach dataframe to each file in list
for (i in 1:nrow(currentTable)){
tables[[i]] = read.csv(currentTable$V9[i],as.is=TRUE)
}

AllBajaFishes = do.call(rbind,tables) # a new master dataframe containing all the data from different files in one place

write.table(AllBajaFishes, "~/Desktop/NSF_CHN/Ecology/ReefCheck/Data_Clean/All_locations_Reef_Check_fishes_data_latest.csv", sep=",", col.names=T, row.names=F)


