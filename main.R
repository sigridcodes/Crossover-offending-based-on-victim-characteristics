# Set variables
BaseDirectory = "C:/Users/jwillmes/Documents/RealData/"
CrimeAndVictimDataFile = "./data/raw_data/hashed_DHPOL_T-Gruppe_1 - identifizierte Taeter.csv"
SuspectDataFile = "./data/raw_data/hashed_DHPOL_P-Gruppe_1 - identifizierte Taeter.csv"
OutputFileName = "./output/results.txt"
OutputFileRawData = "./output/rawdata.csv"
WithIndex = FALSE
#---------------------------------------------------------------

# Load functions
source("~/RealData/functions/dataprocessing.R")
source("~/RealData/functions/loadandtranslate.R")
source("~/RealData/functions/presentresults.R")

# Install packages and create directories
InstallAndLoadPackages()
CreateDirectories(BaseDirectory)

# Load data from files

CrimeAndVictimData <- read.csv2("C:/Users/jwillmes/Documents/RealData/data/raw_data/hashed_DHPOL_T-Gruppe_1 - identifizierte Taeter.csv") 
SuspectData <- read.csv2("C:/Users/jwillmes/Documents/RealData/data/raw_data/hashed_DHPOL_P-Gruppe_1 - identifizierte Taeter.csv")

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
