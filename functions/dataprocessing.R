# Data processing

# Define categories and function
RelationshipMapper <- function(Relationship){
  Stranger = list("No Past Relationship", "No Relationship", "Fellow Countryman")
  Acquaintance = list("Casual Past Relationship", "Acquaintance/Friendship", "Casual Acquaintance", "Formal Social Relationships", "Acquaintance","Former Spouse/Partner", "Close Friendship", "Partners in Cohabiting Relationships")
  Family = list("Parents/Foster Parents", "Kinship/Relative", "Grandchild", "Other Relatives", "Grandparents", "Children/Foster Children", "Siblings", "In-Laws", "Spouse")
  if (Relationship %in% Stranger) {
    return ("Stranger")
  }
  else if (Relationship %in% Acquaintance){
    return ("Acquaintance")
  }
  else if (Relationship %in% Family) {
    return ("Family")
  }
  else {
    return(NA_character_)
  }
}


ViolenceMapper <- function(Violence){
  Violent = list("MINOR INJURY", "SERIOUS INJURY", "FATAL INJURY")
  NotViolent = list("NOT INJURED")
  if (Violence %in% Violent) {
    return ("Violent")
  }
  else if (Violence %in% NotViolent){
    return ("NotViolent")
  }
  else {
    return (NA_character_)
  }
}



AgeMapper <- function(Age){
  if (is.na(Age)) {
    return (NA_character_)
  }
  if (Age <= 10) {
    return ("Pre-pubescent")
  }
  else if (Age >= 11 && Age <= 13){
    return ("Pubescent")
  }
  else if (Age >= 14 && Age <= 17) {
    return ("Post-pubescent")
  }
  else if (Age >=18) {
    return ("Adult")
  }
  else {
    return (NA_character_)
  }
}

DefineHasVictim <- function(HasVictimType, HasNa) {
  if (HasVictimType) {
    return(TRUE)
  }
  else if ( HasNa ){
    return(NA)
  }
  else {
    return(FALSE)
  }
}

DefineHasCrossover <- function(VictimProfile) {
  VictimVector <- unlist(VictimProfile)  
  NumberOfVictimTypes <- sum(VictimVector == TRUE, na.rm = TRUE)
  ContainsNA <- any(is.na(VictimVector) | VictimVector == "UNKNOWN")  
  if (NumberOfVictimTypes > 1) {
    return(TRUE)
  } else if (ContainsNA) {
    return(NA_character_)  
  } else {
    return(FALSE)
  }
}


DefineHasNonAdjacentCrossover <- function(
    HasPrePubescentVictim, HasPubescentVictim, HasPostPubescentVictim, HasAdultVictim, HasNaAgeVictim
) {
  NonAdjacentCombination1 <- HasPrePubescentVictim && HasPostPubescentVictim
  NonAdjacentCombination2 <- HasPrePubescentVictim && HasAdultVictim
  NonAdjacentCombination3 <- HasPubescentVictim && HasAdultVictim
  
  if (NonAdjacentCombination1 | NonAdjacentCombination2 | NonAdjacentCombination3) {
    return(TRUE)
  } else if (HasNaAgeVictim) {
    return(NA)  
  } else {
    return(FALSE)
  }
}

GenerateSuspectProfile <- function(SuspectPersonalNumber, OffenceHistory, SuspectInitialProfile) {
  SuspectProfile = SuspectInitialProfile
  SuspectProfile$SuspectPersonalNumber = SuspectPersonalNumber
  SuspectProfile$NumberOfVictims = dim(OffenceHistory)[1]
  
  # Convert birth year to date
  SuspectBirthdateString <- paste("31.12.", toString(SuspectProfile$Birth_Year)) 
  SuspectBirthdate <- as.Date(SuspectBirthdateString, format = "%d.%m.%Y")
  
  # Find age at first crime
  SuspectStartCrimeDates <- as.Date(OffenceHistory$Tatzeit_von, format = "%d.%m.%Y")
  if(length(SuspectStartCrimeDates) > 0 && any(!is.na(SuspectStartCrimeDates))) {
    SuspectFirstCrimeDate <- min(SuspectStartCrimeDates, na.rm = TRUE)
    AgeAtFirstCrimeDays <- difftime(SuspectFirstCrimeDate, SuspectBirthdate, units = "days")
    AgeAtFirstCrime <- floor(as.numeric(AgeAtFirstCrimeDays)/365.25)
  } else {
    AgeAtFirstCrime <- NA
  }
  SuspectProfile$AgeAtFirstCrime <- AgeAtFirstCrime
  
  # Find age at last crime
  SuspectEndCrimeDates <- as.Date(OffenceHistory$Tatzeit_bis, format = "%d.%m.%Y")
  if(length(SuspectEndCrimeDates) > 0 && any(!is.na(SuspectEndCrimeDates))) {
    SuspectLastCrimeDate <- max(SuspectEndCrimeDates, na.rm = TRUE)
    AgeAtLastCrimeDays <- difftime(SuspectLastCrimeDate, SuspectBirthdate, units = "days")
    AgeAtLastCrime <- floor(as.numeric(AgeAtLastCrimeDays)/365.25)
  } else {
    AgeAtLastCrime <- NA
  }
  SuspectProfile$AgeAtLastCrime <- AgeAtLastCrime

  # Gender
  HasAtLeastOneFemaleVictim = "FEMALE" %in% OffenceHistory$victim_gender
  HasAtLeastOneMaleVictim = "MALE"   %in% OffenceHistory$victim_gender
  HasNaGenderVictim = sum(is.na(OffenceHistory$victim_gender)) > 0
  HasFemaleVictim = DefineHasVictim(HasAtLeastOneFemaleVictim, HasNaGenderVictim)
  HasMaleVictim = DefineHasVictim(HasAtLeastOneMaleVictim, HasNaGenderVictim)
  SuspectProfile$HasFemaleVictim = HasFemaleVictim
  SuspectProfile$HasMaleVictim = HasMaleVictim
  SuspectProfile$GenderCrossover = DefineHasCrossover(list(HasFemaleVictim, HasMaleVictim))
  
  # Age
  HasAtLeastOneChildUnderSixVictim = sum(OffenceHistory$victim_age <= 6, na.rm=TRUE) > 0
  HasAtLeastOnePrePubescentVictim = "Pre-pubescent" %in% OffenceHistory$MappedAge
  HasAtLeastOnePubescentVictim = "Pubescent" %in% OffenceHistory$MappedAge
  HasAtLeastOnePostPubescentVictim = "Post-pubescent" %in% OffenceHistory$MappedAge
  HasAtLeastOneAdultVictim = "Adult" %in% OffenceHistory$MappedAge
  HasNaAgeVictim = any(is.na(OffenceHistory$MappedAge))  
  HasChildUnderSixVictim = DefineHasVictim(HasAtLeastOneChildUnderSixVictim, HasNaAgeVictim)
  HasPrePubescentVictim = DefineHasVictim(HasAtLeastOnePrePubescentVictim, HasNaAgeVictim)
  HasPubescentVictim = DefineHasVictim(HasAtLeastOnePubescentVictim, HasNaAgeVictim)
  HasPostPubescentVictim = DefineHasVictim(HasAtLeastOnePostPubescentVictim, HasNaAgeVictim)
  HasAdultVictim = DefineHasVictim(HasAtLeastOneAdultVictim, HasNaAgeVictim)
  SuspectProfile$HasChildUnderSixVictim = HasChildUnderSixVictim
  SuspectProfile$HasPrePubescentVictim = HasPrePubescentVictim
  SuspectProfile$HasPubescentVictim = HasPubescentVictim
  SuspectProfile$HasPostPubescentVictim = HasPostPubescentVictim
  SuspectProfile$HasAdultVictim = HasAdultVictim
  SuspectProfile$AgeCrossover = DefineHasCrossover(list(HasPrePubescentVictim, HasPubescentVictim, HasPostPubescentVictim, HasAdultVictim))
  
  SuspectProfile$NonAdjacentAgeCrossover = DefineHasNonAdjacentCrossover(HasPrePubescentVictim, HasPubescentVictim, HasPostPubescentVictim, HasAdultVictim, HasNaAgeVictim)
  
  SuspectProfile$YoungestVictim <- ifelse(
    SuspectProfile$HasPrePubescentVictim, "Prepubescent",
    ifelse(SuspectProfile$HasPubescentVictim, "Pubescent",
           ifelse(SuspectProfile$HasPostPubescentVictim, "Postpubescent",
                  ifelse(SuspectProfile$HasAdultVictim, "Adult", NA)
           )
    )
  )

  #Violence
  HasInjuredAtLeastOneVictim = "FATAL INJURY" %in% OffenceHistory$injury | "SERIOUS INJURY" %in% OffenceHistory$injury
  SuspectProfile$HasInjuredVictim = HasInjuredAtLeastOneVictim
  
  # Relationship
  HasAtLeastOneFamilyVictim = "Family" %in% OffenceHistory$MappedRelationship
  HasAtLeastOneAcquaintanceVictim = "Acquaintance" %in% OffenceHistory$MappedRelationship
  HasAtLeastOneStrangerVictim = "Stranger" %in% OffenceHistory$MappedRelationship
  HasNaRelationshipVictim = "Undefined" %in% OffenceHistory$MappedRelationship
  HasFamilyVictim = DefineHasVictim(HasAtLeastOneFamilyVictim, HasNaRelationshipVictim)
  HasAcquaintanceVictim = DefineHasVictim(HasAtLeastOneAcquaintanceVictim, HasNaRelationshipVictim)
  HasStrangerVictim = DefineHasVictim(HasAtLeastOneStrangerVictim, HasNaRelationshipVictim)
  SuspectProfile$HasFamilyVictim = HasFamilyVictim
  SuspectProfile$HasAcquaintanceVictim = HasAcquaintanceVictim
  SuspectProfile$HasStrangerVictim = HasStrangerVictim
  SuspectProfile$RelationshipCrossover = DefineHasCrossover(list(HasFamilyVictim, HasAcquaintanceVictim, HasStrangerVictim))
  return (SuspectProfile)
}

GenerateSuspectStatistics <- function(SuspectPersonalNumbers, CrimeAndVictimData, SuspectData){
  SuspectStatistics = data.frame()
  for (SuspectPersonalNumber in SuspectPersonalNumbers) {
    OffenceHistory = CrimeAndVictimData[CrimeAndVictimData$P_Nummer_Personengrunddaten %in% SuspectPersonalNumber,]
    SuspectInitialProfile = SuspectData[SuspectData$P_Nummer_Personengrunddaten %in% SuspectPersonalNumber,][1,]
    SuspectProfile = GenerateSuspectProfile(SuspectPersonalNumber, OffenceHistory, SuspectInitialProfile)
    SuspectStatistics <- rbind(SuspectStatistics, SuspectProfile)
  }
  return(SuspectStatistics)
}

