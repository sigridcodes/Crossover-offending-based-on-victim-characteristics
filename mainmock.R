# Set variables
BaseDirectory = "newmockdata/"
CrimeAndVictimDataFile = "./data/raw_data/mock_dataset_T.csv"
SuspectDataFile = "./data/raw_data/mock_dataset_P.csv"
OutputFileName = "./output/results.txt"
OutputFileRawData = "./output/rawdata.csv"
WithIndex = FALSE
#---------------------------------------------------------------

# Load functions
source("~/Documents/statistics/newmockdata/functions/loadandtranslate.R")
source("~/Documents/statistics/newmockdata/functions/dataprocessing.R")
source("~/Documents/statistics/newmockdata/functions/presentresults.R")

# Install packages and create directories
InstallAndLoadPackages()
CreateDirectories(BaseDirectory)

# Load data from files

#load file Sigrid
#CrimeAndVictimData <- read.csv2("~/Documents/statistics/newmockdata/data/raw_data/mock_dataset_T.csv")
#SuspectData <- read.csv2("~/Documents/statistics/newmockdata/data/raw_data/mock_dataset_P.csv")

CrimeAndVictimData <- read.csv2(paste0(BaseDirectory, CrimeAndVictimDataFile)) 
SuspectData <- read.csv2(paste0(BaseDirectory, SuspectDataFile))

# Translate data to English

SuspectData = RecodeSuspectData(SuspectData, WithIndex = WithIndex)
CrimeAndVictimData = RecodeCrimeAndVictimData(CrimeAndVictimData)

# Process data

SuspectPersonalNumbers = unique(CrimeAndVictimData$P_Nummer_Personengrunddaten)
CrimeAndVictimData$MappedRelationship <- lapply(CrimeAndVictimData$victim_relationship_formal, RelationshipMapper)
CrimeAndVictimData$MappedAge <- lapply(CrimeAndVictimData$victim_age, AgeMapper)
SuspectStatistics = GenerateSuspectStatistics(SuspectPersonalNumbers, CrimeAndVictimData, SuspectData)
SerialSuspectStatistics = SuspectStatistics[SuspectStatistics$NumberOfVictims > 1 & !is.na(SuspectStatistics$AgeAtFirstCrime) & !is.na(SuspectStatistics$AgeAtLastCrime) & SuspectStatistics$AgeAtFirstCrime > 17 & SuspectStatistics$AgeAtLastCrime < 70,]

# Compute statistical values and present results

PresentResults(SerialSuspectStatistics, SuspectStatistics, paste0(BaseDirectory, OutputFileName), paste0(BaseDirectory, OutputFileRawData))
