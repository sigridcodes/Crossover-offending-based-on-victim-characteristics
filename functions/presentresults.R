# Present results

# Compute probability that there are more suspects with gender crossover among suspects with child under six victims

PresentResults <- function(SerialSuspectStatistics, SuspectStatistics, OutputFileName, OutputFileRawData){
  # -------------------------------------
  # Compute statistical values
  # -------------------------------------
  
  
  SerialSuspectPersonalNumbers = SerialSuspectStatistics$SuspectPersonalNumber
  SerialSuspectGender <- vector( length = length(SerialSuspectPersonalNumbers))
  
  for (SerialSuspectIndex in 1:length(SerialSuspectPersonalNumbers)) {
    SerialSuspectInformation = SuspectData[SuspectData$P_Nummer_Personengrunddaten %in% SerialSuspectPersonalNumbers[SerialSuspectIndex],]
    SerialSuspectGender[SerialSuspectIndex] = SerialSuspectInformation$Gender[1]
  }
  
  # Information about under six and over six suspects
  NumberOfTotalSuspects = dim(SerialSuspectStatistics)[1]
  NumberOfOverSixSuspects = sum(SerialSuspectStatistics$HasChildUnderSixVictim == FALSE, na.rm = TRUE)
  NumberOfOverSixSuspectsWithGenderCrossover =  sum(SerialSuspectStatistics$GenderCrossover == TRUE & SerialSuspectStatistics$HasChildUnderSixVictim == FALSE, na.rm = TRUE)
  NumberOfUnderSixSuspects = sum(SerialSuspectStatistics$HasChildUnderSixVictim == TRUE, na.rm = TRUE)
  NumberOfUnderSixSuspectWithGenderCrossover = sum(SerialSuspectStatistics$HasChildUnderSixVictim == TRUE & SerialSuspectStatistics$GenderCrossover == TRUE, na.rm = TRUE)
  NumberOfUnknownAgeSuspects <- sum(is.na(SerialSuspectStatistics$HasChildUnderSixVictim))
  
  
  # Information about serial suspects
  NumberOfMaleSerialSuspects = sum(SerialSuspectGender == "MALE", na.rm = TRUE)
  ProportionOfMaleSerialSuspects = NumberOfMaleSerialSuspects/NumberOfTotalSuspects
  NumberOfFemaleSerialSuspects = sum(SerialSuspectGender == "FEMALE", na.rm = TRUE)
  ProportionOfFemaleSerialSuspects = NumberOfFemaleSerialSuspects/NumberOfTotalSuspects
  NumberOfUnknownGenderSerialSuspects <- sum(is.na(SerialSuspectGender))
  ProportionOfUnknownGenderSerialSuspects <- NumberOfUnknownGenderSerialSuspects / NumberOfTotalSuspects
  MeanAgeAtFirstCrime = mean(SerialSuspectStatistics$AgeAtFirstCrime, na.rm = TRUE)
  MeanAgeAtLastCrime = mean(SerialSuspectStatistics$AgeAtLastCrime, na.rm = TRUE)
  
  file.create(OutputFileName)
  write("Under six suspect statistics", OutputFileName, append = FALSE)
  write("-----------------------------", OutputFileName, append = TRUE)
  write(paste("Number of total (serial) suspects: ", toString(NumberOfTotalSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of over six suspects: ", toString(NumberOfOverSixSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of under six suspects: ", toString(NumberOfUnderSixSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of under six suspects with gender crossover: ", toString(NumberOfUnderSixSuspectWithGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Number of unknown age suspects: ", toString(NumberOfUnknownAgeSuspects)), OutputFileName, append = TRUE)
  write("", OutputFileName, append = TRUE)
  
  
  # Compute proportion of gender crossover for suspects that only has victims above 6 years and write to file
  ProportionGenderCrossoverOverSixSuspects = NumberOfOverSixSuspectsWithGenderCrossover / NumberOfOverSixSuspects
  print(ProportionGenderCrossoverOverSixSuspects)
  
  write("Testing gender crossover for over and under six suspects", OutputFileName, append = TRUE)
  write("-----------------------------", OutputFileName, append = TRUE)
  write(paste("Proportion of gender crossover for over-six-suspects: ", toString(ProportionGenderCrossoverOverSixSuspects)), OutputFileName, append = TRUE)
  
  # Testing hypothesis1: "Among people who have committed repeated (at least two) sexual abuse, those who had any victim below age 6 are more likely to have victims of both sexes compared to those who only had victims above age 6"
  # Get statistics for binom test https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/binom.test
  # save to file
  BinomtestUnder6vsOver6 = binom.test(x = NumberOfUnderSixSuspectWithGenderCrossover, n = NumberOfUnderSixSuspects, p = ProportionGenderCrossoverOverSixSuspects, alternative = c("two.sided"), conf.level = 0.95)

   # Calculate Cohens' h
    p_observedBinomtestUnder6vsOver6 <- NumberOfUnderSixSuspectWithGenderCrossover / NumberOfUnderSixSuspects
    p_expectedBinomtestUnder6vsOver6 <- ProportionGenderCrossoverOverSixSuspects
    cohen_hBinomtestUnder6vsOver6 <- 2 * (asin(sqrt(p_observedBinomtestUnder6vsOver6)) - asin(sqrt(p_expectedBinomtestUnder6vsOver6)))
  
   # save to file
   write(paste("BinomtestUnder6vsOver6: ", toString(BinomtestUnder6vsOver6)), OutputFileName, append = TRUE)
   write(paste("Cohens H, BinomtestUnder6vsOver6: ", toString(cohen_hBinomtestUnder6vsOver6)), OutputFileName, append = TRUE)
  
   
  # Create diagram for overlap between age categories
  
  groups <- c('HasPrePubescentVictim', 'HasPubescentVictim', 'HasPostPubescentVictim', 'HasAdultVictim')
  
  overlapdata = matrix(c(1:16), ncol=4, byrow=TRUE)
  
  for (colIndex in 1:length(groups)) {
    for (rowIndex in 1:length(groups)) {
      #overlapdata[colIndex, rowIndex] = sum(SerialSuspectStatistics[[groups[colIndex]]] == TRUE & SerialSuspectStatistics[[groups[rowIndex]]] == TRUE)
      if (colIndex <= rowIndex){
        overlapdata[colIndex, rowIndex] = sum(SerialSuspectStatistics[[groups[colIndex]]] == TRUE & SerialSuspectStatistics[[groups[rowIndex]]] == TRUE, na.rm = TRUE)
      }
      else {
        overlapdata[colIndex, rowIndex] = ''
      }
    }
  }
  colnames(overlapdata) = groups
  rownames(overlapdata) = groups
  
  overlapdatatable = as.table(overlapdata)
  
  # Create list for different types of crossover
  NumberOfGenderCrossoverSuspects = sum(SerialSuspectStatistics$GenderCrossover == TRUE, na.rm = TRUE)
  NumberOfGenderSpecializedSuspects = sum(SerialSuspectStatistics$GenderCrossover == FALSE, na.rm = TRUE)
  NumberOfAgeCrossoverSuspects = sum(SerialSuspectStatistics$AgeCrossover == TRUE, na.rm = TRUE)
  NumberOfAgeSpecializedSuspects = sum(SerialSuspectStatistics$AgeCrossover == FALSE, na.rm = TRUE)
  NumberOfRelationshipCrossoverSuspects = sum(SerialSuspectStatistics$RelationshipCrossover == TRUE, na.rm = TRUE)
  NumberOfRelationshipSpecializedSuspects = sum(SerialSuspectStatistics$RelationshipCrossover == FALSE, na.rm = TRUE)
  NumberOfGenderAndAgeCrossoverSuspects = sum(SerialSuspectStatistics$GenderCrossover == TRUE & SerialSuspectStatistics$AgeCrossover == TRUE, na.rm = TRUE)
  NumberOfGenderAndRelationshipCrossoverSuspects = sum(SerialSuspectStatistics$GenderCrossover == TRUE & SerialSuspectStatistics$RelationshipCrossover == TRUE, na.rm = TRUE)
  NumberOfAgeAndRelationshipCrossoverSuspects = sum(SerialSuspectStatistics$AgeCrossover == TRUE & SerialSuspectStatistics$RelationshipCrossover == TRUE, na.rm = TRUE)
  NumberOfGenderAgeAndRelationshipCrossoverSuspects = sum(SerialSuspectStatistics$GenderCrossover == TRUE & SerialSuspectStatistics$AgeCrossover == TRUE & SerialSuspectStatistics$RelationshipCrossover == TRUE, na.rm = TRUE)

  # Find proportions for different types of crossover
  ProportionOfGenderCrossoverSuspects = NumberOfGenderCrossoverSuspects/NumberOfTotalSuspects
  ProportionOfAgeCrossoverSuspects = NumberOfAgeCrossoverSuspects/NumberOfTotalSuspects
  ProportionOfRelationshipCrossoverSuspects = NumberOfRelationshipCrossoverSuspects/NumberOfTotalSuspects
  ProportionOfGenderAndAgeCrossoverSuspects = NumberOfGenderAndAgeCrossoverSuspects/NumberOfTotalSuspects
  ProportionOfGenderAndRelationshipCrossoverSuspects = NumberOfGenderAndRelationshipCrossoverSuspects/NumberOfTotalSuspects
  ProportionOfAgeAndRelationshipCrossoverSuspects = NumberOfAgeAndRelationshipCrossoverSuspects/NumberOfTotalSuspects
  ProportionOfGenderAgeAndRelationshipCrossoverSuspects = NumberOfGenderAgeAndRelationshipCrossoverSuspects/NumberOfTotalSuspects
  
# Find number of specialized suspects and suspects with different crossover combinations and save to file
  
  # Specialized gender suspects 
  SuspectsWithOnlyMaleVictims = sum(SerialSuspectStatistics$HasMaleVictim == TRUE & SerialSuspectStatistics$HasFemaleVictim == FALSE, na.rm = TRUE)
  SuspectsWithOnlyFemaleVictims = sum(SerialSuspectStatistics$HasMaleVictim == FALSE & SerialSuspectStatistics$HasFemaleVictim == TRUE, na.rm = TRUE)
  
  # Specialized relationship suspects 
  SuspectsWithOnlyFamilyVictims = sum(SerialSuspectStatistics$HasFamilyVictim == TRUE & SerialSuspectStatistics$HasStrangerVictim == FALSE & SerialSuspectStatistics$HasAcquaintanceVictim == FALSE, na.rm = TRUE)
  SuspectsWithOnlyStrangerVictims = sum(SerialSuspectStatistics$HasFamilyVictim == FALSE & SerialSuspectStatistics$HasStrangerVictim == TRUE & SerialSuspectStatistics$HasAcquaintanceVictim == FALSE, na.rm = TRUE)
  SuspectsWithOnlyAcquaintanceVictims = sum(SerialSuspectStatistics$HasFamilyVictim == FALSE & SerialSuspectStatistics$HasStrangerVictim == FALSE & SerialSuspectStatistics$HasAcquaintanceVictim == TRUE, na.rm = TRUE)
  
  #Pairwise relationship crossover
  SuspectsWithFamilyAndStrangerVictims = sum(SerialSuspectStatistics$HasFamilyVictim == TRUE & SerialSuspectStatistics$HasStrangerVictim == TRUE & SerialSuspectStatistics$HasAcquaintanceVictim == FALSE, na.rm = TRUE)    
  SuspectsWithFamilyAndAcquaintanceVictims = sum(SerialSuspectStatistics$HasFamilyVictim == TRUE & SerialSuspectStatistics$HasStrangerVictim == FALSE & SerialSuspectStatistics$HasAcquaintanceVictim == TRUE, na.rm = TRUE)
  SuspectsWithAcquaintanceAndStrangerVictims = sum(SerialSuspectStatistics$HasFamilyVictim == FALSE & SerialSuspectStatistics$HasStrangerVictim == TRUE & SerialSuspectStatistics$HasAcquaintanceVictim == TRUE, na.rm = TRUE)
  
  #Triple relationship crossover
  SuspectsWithFamilyAcquaintanceAndStrangerVictims = sum(SerialSuspectStatistics$HasFamilyVictim == TRUE & SerialSuspectStatistics$HasStrangerVictim == TRUE & SerialSuspectStatistics$HasAcquaintanceVictim == TRUE, na.rm = TRUE)
  
  #Specialized age suspects 
  SuspectsWithOnlyPrePubescentVictims = sum(SerialSuspectStatistics$HasPrePubescentVictim == TRUE & SerialSuspectStatistics$HasPubescentVictim == FALSE & SerialSuspectStatistics$HasPostPubescentVictim == FALSE, na.rm = TRUE)
  SuspectsWithOnlyPubescentVictims = sum(SerialSuspectStatistics$HasPrePubescentVictim == FALSE & SerialSuspectStatistics$HasPubescentVictim == TRUE & SerialSuspectStatistics$HasPostPubescentVictim == FALSE, na.rm = TRUE)
  SuspectsWithOnlyPostPubescentVictims = sum(SerialSuspectStatistics$HasPrePubescentVictim == FALSE & SerialSuspectStatistics$HasPubescentVictim == FALSE & SerialSuspectStatistics$HasPostPubescentVictim == TRUE, na.rm = TRUE)
  
  #Pairwise age crossover
  SuspectsWithPrepubescentAndPubescentVictims = sum(SerialSuspectStatistics$HasPrePubescentVictim == TRUE & SerialSuspectStatistics$HasPubescentVictim == TRUE & SerialSuspectStatistics$HasPostPubescentVictim == FALSE, na.rm = TRUE)
  SuspectsWithPrepubescentAndPostpubescentVictims = sum(SerialSuspectStatistics$HasPrePubescentVictim == TRUE & SerialSuspectStatistics$HasPubescentVictim == FALSE & SerialSuspectStatistics$HasPostPubescentVictim == TRUE, na.rm = TRUE)
  SuspectsWithPubescentAndPostpubescentVictims = sum(SerialSuspectStatistics$HasPrePubescentVictim == FALSE & SerialSuspectStatistics$HasPubescentVictim == TRUE & SerialSuspectStatistics$HasPostPubescentVictim == TRUE, na.rm = TRUE)
  
  #Triple age crossover
  SuspectsWithPrepubescentPubescentAndPostpubescentVictims = sum(SerialSuspectStatistics$HasPrePubescentVictim == TRUE & SerialSuspectStatistics$HasPubescentVictim == TRUE & SerialSuspectStatistics$HasPostPubescentVictim == TRUE, na.rm = TRUE)
  
  #save to file
  write("Information about crossover combinations", OutputFileName, append = TRUE)
  write("------------------------------", OutputFileName, append = TRUE)
  write("Gender:", OutputFileName, append = TRUE)
  write(paste("Suspects with only male victims: ", toString(SuspectsWithOnlyMaleVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with only female victims: ", toString(SuspectsWithOnlyFemaleVictims)), OutputFileName, append = TRUE)
  write("Relationship:", OutputFileName, append = TRUE)
  write(paste("Suspects with only family victims: ", toString(SuspectsWithOnlyFamilyVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with only stranger victims: ", toString(SuspectsWithOnlyStrangerVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with only acquaintance victims: ", toString(SuspectsWithOnlyAcquaintanceVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with family and stranger victims: ", toString(SuspectsWithFamilyAndStrangerVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with family and acquaintance victims: ", toString(SuspectsWithFamilyAndAcquaintanceVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with stranger and aquaintance victims: ", toString(SuspectsWithAcquaintanceAndStrangerVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with triple relationship crossover (family, stranger and acquaintance victims): ", toString(SuspectsWithFamilyAcquaintanceAndStrangerVictims)), OutputFileName, append = TRUE)
  write("Age:", OutputFileName, append = TRUE)
  write(paste("Suspects with only prepubescent victims: ", toString(SuspectsWithOnlyPrePubescentVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with only pubescent victims: ", toString(SuspectsWithOnlyPubescentVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with only postpubescent victims: ", toString(SuspectsWithOnlyPostPubescentVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with pre- and pubescent victims: ", toString(SuspectsWithPrepubescentAndPubescentVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with pre- and postpubescent victims: ", toString(SuspectsWithPrepubescentAndPostpubescentVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with pubescent and postpubescent victims: ", toString(SuspectsWithPubescentAndPostpubescentVictims)), OutputFileName, append = TRUE)
  write(paste("Suspects with triple age crossover (pre-, post- and pubescent): ", toString(SuspectsWithPrepubescentPubescentAndPostpubescentVictims)), OutputFileName, append = TRUE)
  
  
  
#Find average number of victims for non-crossover suspects and save to file
  NonCrossoverSuspects = SerialSuspectStatistics[SerialSuspectStatistics$GenderCrossover == FALSE & SerialSuspectStatistics$AgeCrossover == FALSE & SerialSuspectStatistics$RelationshipCrossover == FALSE,]
  NumberOfNonCrossoverSuspects = dim(NonCrossoverSuspects)[1]
  NonCrossoverSuspectsAverageNumberOfVictims = mean(NonCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
  NonGenderCrossoverSuspects = SerialSuspectStatistics[SerialSuspectStatistics$GenderCrossover == FALSE,]
  NonRelationshipCrossoverSuspects = SerialSuspectStatistics[SerialSuspectStatistics$RelationshipCrossover == FALSE,]
  NonAgeCrossoverSuspects = SerialSuspectStatistics[SerialSuspectStatistics$AgeCrossover == FALSE,]
  
  write("", OutputFileName, append = TRUE)
  write("Testing if number of victim differs between crossover and specialized suspects", OutputFileName, append = TRUE)
  write("-----------------------", OutputFileName, append = TRUE)
  write(paste("Average number of victims for non-crossover suspects: ", toString(NonCrossoverSuspectsAverageNumberOfVictims)), OutputFileName, append = TRUE)
  write(paste("Number of non-crossover suspects: ", toString(NumberOfNonCrossoverSuspects)), OutputFileName, append = TRUE)
  
  #Find average number of victims for crossover suspects and save to file
  AnyTypeCrossoverSuspects = SerialSuspectStatistics[SerialSuspectStatistics$GenderCrossover == TRUE | SerialSuspectStatistics$AgeCrossover == TRUE | SerialSuspectStatistics$RelationshipCrossover == TRUE,]
  NumberOfAnyTypeCrossoverSuspects = dim(AnyTypeCrossoverSuspects)[1]
  AnyTypeCrossoverSuspectsAverageNumberOfVictims = mean(AnyTypeCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
  GenderCrossoverSuspects = SerialSuspectStatistics[SerialSuspectStatistics$GenderCrossover == TRUE,]
  RelationshipCrossoverSuspects = SerialSuspectStatistics[SerialSuspectStatistics$RelationshipCrossover == TRUE,]
  AgeCrossoverSuspects = SerialSuspectStatistics[SerialSuspectStatistics$AgeCrossover == TRUE,]
  
  
  # T-tests comparing number of victims differs for crossover suspects compared to specialized suspects
    # T-test comparing number of victims differs for suspects with any type of crossover with any type of crossover vs those that do not have any crossover
    TTestAnyTypeCrossoverNumberOfVictims = t.test(x = AnyTypeCrossoverSuspects$NumberOfVictims, y = NonCrossoverSuspects$NumberOfVictims, alternative = c("two.sided"), paired = FALSE)
    CohensDTTestAnyTypeCrossoverNumberOfVictims <- cohens_d(x = AnyTypeCrossoverSuspects$NumberOfVictims, y = NonCrossoverSuspects$NumberOfVictims, pooled_sd = TRUE, standardized = "d")
    mean(AnyTypeCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
    mean(NonCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
  
  
    # T-test comparing number of victims differs for gender crossover suspects vs those that do not have gender crossover
    TTestGenderCrossoverNumberOfVictims = t.test(x = GenderCrossoverSuspects$NumberOfVictims, y = NonGenderCrossoverSuspects$NumberOfVictims, alternative = c("two.sided"), paired = FALSE)
    CohensDTTestGenderCrossoverNumberOfVictims <- cohens_d(x = GenderCrossoverSuspects$NumberOfVictims, y = NonGenderCrossoverSuspects$NumberOfVictims, pooled_sd = TRUE, standardized = "d")
    mean(GenderCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
    mean(NonGenderCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
  
    # T-test comparing number of victims differs for age crossover suspects vs those that do not have age crossover
    TTestAgeCrossoverNumberOfVictims = t.test(x = AgeCrossoverSuspects$NumberOfVictims, y = NonAgeCrossoverSuspects$NumberOfVictims, alternative = c("two.sided"), paired = FALSE)
    effectsizeTTestAgeCrossoverNumberOfVictims <- cohens_d(x = AgeCrossoverSuspects$NumberOfVictims, y = NonAgeCrossoverSuspects$NumberOfVictims,  pooled_sd = TRUE, standardized = "d")
    mean(AgeCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
    mean(NonAgeCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
  
    # T-test comparing number of victims differs for relationship crossover suspects vs those that do not have relationship crossover
    TTestRelationshipCrossoverNumberOfVictims = t.test(x = RelationshipCrossoverSuspects$NumberOfVictims, y = NonRelationshipCrossoverSuspects$NumberOfVictims, alternative = c("two.sided"), paired = FALSE)
    CohensDTTestRelationshipCrossoverNumberOfVictims <- cohens_d(x = RelationshipCrossoverSuspects$NumberOfVictims, y = NonRelationshipCrossoverSuspects$NumberOfVictims, pooled_sd = TRUE, standardized = "d")
    mean(RelationshipCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
    mean(NonRelationshipCrossoverSuspects$NumberOfVictims, na.rm = TRUE)
  
    #P-values from the three tests
    p_valNumberOfVictimsGENDER <- TTestGenderCrossoverNumberOfVictims$p.value
    p_valNumberOfVictimsRELATIONSHIP <- TTestRelationshipCrossoverNumberOfVictims$p.value
    p_valNumberOfVictimsAGE <- TTestAgeCrossoverNumberOfVictims$p.value
    p_valuesNumberOfVictims <- c(p_valNumberOfVictimsGENDER, p_valNumberOfVictimsRELATIONSHIP, p_valNumberOfVictimsAGE)
  
  
    # Apply Holm-Bonferroni correction
    NumberofVictimsAdjusted_p_values <- p.adjust(p_valuesNumberOfVictims, method = "holm")
  
    # Write to file
    write(paste("Average number of victims for suspects with any type of crossover: ", toString(AnyTypeCrossoverSuspectsAverageNumberOfVictims)), OutputFileName, append = TRUE)
    write(paste("Number of suspects with any type of crossover: ", toString(NumberOfAnyTypeCrossoverSuspects)), OutputFileName, append = TRUE)
    write(paste("T-test comparing number of victims  for suspects with any type of crossover vs those that do not have any crossover:", toString(TTestAnyTypeCrossoverNumberOfVictims)), OutputFileName, append = TRUE)
    write(paste("^Effectsize, cohens d, any type", toString(CohensDTTestAnyTypeCrossoverNumberOfVictims)), OutputFileName, append = TRUE)
    write(paste("T-test comparing number of victims differs for gender crossover suspects vs those that do not have gender crossover:", toString(TTestGenderCrossoverNumberOfVictims)), OutputFileName, append = TRUE)
    write(paste("^Effectsize, cohens d, gender", toString(CohensDTTestGenderCrossoverNumberOfVictims)), OutputFileName, append = TRUE)
    write(paste("T-test comparing number of victims differs for age crossover suspects vs those that do not have age crossover:", toString(TTestAgeCrossoverNumberOfVictims)), OutputFileName, append = TRUE)
    write(paste("^Effectsize, cohens d", toString(effectsizeTTestAgeCrossoverNumberOfVictims)), OutputFileName, append = TRUE)
    write(paste("T-test comparing number of victims differs for relationship crossover suspects vs those that do not have relationship crossover:", toString(TTestRelationshipCrossoverNumberOfVictims)), OutputFileName, append = TRUE)
    write(paste("^Effectsize, cohens d, relationship", toString(CohensDTTestRelationshipCrossoverNumberOfVictims)), OutputFileName, append = TRUE)
    write(paste("Number of victims differs: Holm-Bonferroni adjusted p-values for gender, relationship and age crossover: ", toString(NumberofVictimsAdjusted_p_values)), OutputFileName, append = TRUE)
  
  #Gender crossover for different victim age groups 
  HasChildUnderSixVictimAndGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == TRUE & SerialSuspectStatistics$HasChildUnderSixVictim == TRUE, na.rm = TRUE)
  YoungestVictimPrepubescentAndGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == TRUE & SerialSuspectStatistics$YoungestVictim == "Prepubescent", na.rm = TRUE)
  YoungestVictimPubescentAndGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == TRUE & SerialSuspectStatistics$YoungestVictim == "Pubescent", na.rm = TRUE)
  YoungestVictimPostpubescentAndGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == TRUE & SerialSuspectStatistics$YoungestVictim == "Postpubescent", na.rm = TRUE)
  YoungestVictimAdultAndGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == TRUE & SerialSuspectStatistics$YoungestVictim == "Adult", na.rm = TRUE)
  HasChildUnderSixVictimAndNoGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == FALSE & SerialSuspectStatistics$HasChildUnderSixVictim == TRUE, na.rm = TRUE)
  YoungestVictimPrepubescentAndNoGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == FALSE & SerialSuspectStatistics$YoungestVictim == "Prepubescent", na.rm = TRUE)
  YoungestVictimPubescentAndNoGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == FALSE & SerialSuspectStatistics$YoungestVictim == "Pubescent", na.rm = TRUE)
  YoungestVictimPostpubescentAndNoGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == FALSE & SerialSuspectStatistics$YoungestVictim == "Postpubescent", na.rm = TRUE)
  YoungestVictimAdultAndNoGenderCrossover = sum(SerialSuspectStatistics$GenderCrossover == FALSE & SerialSuspectStatistics$YoungestVictim == "Adult", na.rm = TRUE)
  
  TotalSuspectsWithPrepubescentVictim = sum(SerialSuspectStatistics$HasPrePubescentVictim == TRUE, na.rm = TRUE)
  TotalSuspectsWithPubescentVictim = sum(SerialSuspectStatistics$HasPubescentVictim == TRUE, na.rm = TRUE)
  TotalSuspectsWithPostpubescentVictim = sum(SerialSuspectStatistics$HasPostPubescentVictim == TRUE, na.rm = TRUE)
  TotalSuspectsWithAdultVictim = sum(SerialSuspectStatistics$HasAdultVictim == TRUE, na.rm = TRUE)
  
  
  Rownamesgendercrossovertable <- c('Prepubescent victim', 'Pubescent victim', 'Postpubescent victim', 'Adult victim')
  Colnamesgendercrossovertable <- c('One Gender', 'Both Gender', 'Total')
  
  Gendercrossovertable = matrix(c(1:12), ncol=3, byrow=TRUE)
  Gendercrossovertable[1, 1] = YoungestVictimPrepubescentAndNoGenderCrossover
  Gendercrossovertable[2, 1] = YoungestVictimPubescentAndNoGenderCrossover
  Gendercrossovertable[3, 1] = YoungestVictimPostpubescentAndNoGenderCrossover
  Gendercrossovertable[4, 1] = YoungestVictimAdultAndNoGenderCrossover
  Gendercrossovertable[1, 2] = YoungestVictimPrepubescentAndGenderCrossover
  Gendercrossovertable[2, 2] = YoungestVictimPubescentAndGenderCrossover
  Gendercrossovertable[3, 2] = YoungestVictimPostpubescentAndGenderCrossover
  Gendercrossovertable[4, 2] = YoungestVictimAdultAndGenderCrossover
  Gendercrossovertable[1, 3] = YoungestVictimPrepubescentAndGenderCrossover + YoungestVictimPrepubescentAndNoGenderCrossover
  Gendercrossovertable[2, 3] = YoungestVictimPubescentAndGenderCrossover + YoungestVictimPubescentAndNoGenderCrossover
  Gendercrossovertable[3, 3] = YoungestVictimPostpubescentAndGenderCrossover + YoungestVictimPostpubescentAndNoGenderCrossover
  Gendercrossovertable[4, 3] = YoungestVictimAdultAndGenderCrossover + YoungestVictimAdultAndNoGenderCrossover
  
  colnames(Gendercrossovertable) = Colnamesgendercrossovertable
  rownames(Gendercrossovertable) = Rownamesgendercrossovertable
  
  write("", OutputFileName, append = TRUE)
  write("Victim age: overlaptable", OutputFileName, append = TRUE)
  write("----------------------", OutputFileName, append = TRUE)
  write.table(overlapdatatable, OutputFileName, append = TRUE, sep = ",", dec = ".", row.names = TRUE, col.names = TRUE)
  
  GendercrossovertableWithoutTotal = Gendercrossovertable[1:3,1:2]
  resultschitestGENDERCROSSOVER = chisq.test(GendercrossovertableWithoutTotal)
  print(resultschitestGENDERCROSSOVER)
  
  write("", OutputFileName, append = TRUE)
  write("Chi test gender crossover", OutputFileName, append = TRUE)
  write("-------------------------", OutputFileName, append = TRUE)
  write(paste("Result chi test for gender crossover with different age groups: ", toString(resultschitestGENDERCROSSOVER)), OutputFileName, append = TRUE)
  
  #Compare values pairwise
  GendercrossoverPairwise = pairwise.prop.test(GendercrossovertableWithoutTotal)
  
  # Number of suspects with gender crossover in each victim age group
  AgeAndGenderCrossover <- c(YoungestVictimPrepubescentAndGenderCrossover, YoungestVictimPubescentAndGenderCrossover, YoungestVictimPostpubescentAndGenderCrossover)  
  
  # Total suspects in each group
  TotalAge <- c((YoungestVictimPrepubescentAndGenderCrossover + YoungestVictimPrepubescentAndNoGenderCrossover), (YoungestVictimPubescentAndGenderCrossover + YoungestVictimPubescentAndNoGenderCrossover), (YoungestVictimPostpubescentAndGenderCrossover + YoungestVictimPostpubescentAndNoGenderCrossover))
  
  # Compute proportions
  ProportionsAgeAndGenderCrossover <- AgeAndGenderCrossover / TotalAge
  
  cohen_h <- function(p1, p2) {
    2 * (asin(sqrt(p1)) - asin(sqrt(p2)))
  }
  
  # Prepubescent vs Pubescent
  cohenshPrevsPub <- cohen_h(ProportionsAgeAndGenderCrossover[1], ProportionsAgeAndGenderCrossover[2])
  
  # Prepubescent vs Postpubescent
  cohenshPrevsPost <- cohen_h(ProportionsAgeAndGenderCrossover[1], ProportionsAgeAndGenderCrossover[3])
  
  # Pubescent vs Postpubescent
  cohenshPubvsPost <- cohen_h(ProportionsAgeAndGenderCrossover[2], ProportionsAgeAndGenderCrossover[3])
  
  write("", OutputFileName, append = TRUE)
  write("Cohens h for pairwise comparisons between gender crossover and victim age groups", OutputFileName, append = TRUE)
  write("---------------------------------------------------------------------------------", OutputFileName, append = TRUE)
  write(paste("Cohens h prepubescent vs pubescent: ", toString(cohenshPrevsPub)), OutputFileName, append = TRUE)
  write(paste("Cohens h prepubescent vs postpubescent: ", toString(cohenshPrevsPost)), OutputFileName, append = TRUE)
  write(paste("Cohens h pubescent vs postpubescent: ", toString(cohenshPubvsPost)), OutputFileName, append = TRUE)
  
  #Gender crossover for different age groups in proportions and save to file
  NumberOfSuspectsWithUnknownGenderCrossover <- sum(is.na(SerialSuspectStatistics$GenderCrossover))
  ProportionHasChildUnderSixVictimAndGenderCrossover = HasChildUnderSixVictimAndGenderCrossover / (HasChildUnderSixVictimAndGenderCrossover + HasChildUnderSixVictimAndNoGenderCrossover)
  ProportionYoungestVictimPrepubescentAndGenderCrossover =  YoungestVictimPrepubescentAndGenderCrossover / (YoungestVictimPrepubescentAndGenderCrossover + YoungestVictimPrepubescentAndNoGenderCrossover)
  ProportionYoungestVictimPubescentAndGenderCrossover =  YoungestVictimPubescentAndGenderCrossover / (YoungestVictimPubescentAndGenderCrossover + YoungestVictimPubescentAndNoGenderCrossover)
  ProportionYoungestVictimPostpubescentAndGenderCrossover =  YoungestVictimPostpubescentAndGenderCrossover / (YoungestVictimPostpubescentAndGenderCrossover + YoungestVictimPostpubescentAndNoGenderCrossover)
  ProportionYoungestVictimAdultAndGenderCrossover = YoungestVictimAdultAndGenderCrossover / (YoungestVictimAdultAndGenderCrossover + YoungestVictimAdultAndNoGenderCrossover)
  ProportionHasChildUnderSixVictimAndNoGenderCrossover = HasChildUnderSixVictimAndNoGenderCrossover / (HasChildUnderSixVictimAndGenderCrossover + HasChildUnderSixVictimAndNoGenderCrossover)
  ProportionYoungestVictimPrepubescentAndNoGenderCrossover =  YoungestVictimPrepubescentAndNoGenderCrossover / (YoungestVictimPrepubescentAndGenderCrossover + YoungestVictimPrepubescentAndNoGenderCrossover)
  ProportionYoungestVictimPubescentAndNoGenderCrossover =  YoungestVictimPubescentAndNoGenderCrossover / (YoungestVictimPubescentAndGenderCrossover + YoungestVictimPubescentAndNoGenderCrossover)
  ProportionYoungestVictimPostpubescentAndNoGenderCrossover =  YoungestVictimPostpubescentAndNoGenderCrossover / (YoungestVictimPostpubescentAndGenderCrossover + YoungestVictimPostpubescentAndNoGenderCrossover)
  ProportionYoungestVictimAdultAndNoGenderCrossover = YoungestVictimAdultAndNoGenderCrossover / (YoungestVictimAdultAndGenderCrossover + YoungestVictimAdultAndNoGenderCrossover)
  ProportionSuspectsWithUnknownGenderCrossover = NumberOfSuspectsWithUnknownGenderCrossover / NumberOfTotalSuspects
  
  write("", OutputFileName, append = TRUE)
  write("Gender crossover for different age groups", OutputFileName, append = TRUE)
  write("-----------------------------------------", OutputFileName, append = TRUE)
  write(paste("Has child under six victim and gender crossover: ", toString(HasChildUnderSixVictimAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion has child under six victim and gender crossover: ", toString(ProportionHasChildUnderSixVictimAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("YoungestVictim prepubescent victim and gender crossover: ", toString(YoungestVictimPrepubescentAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion YoungestVictim prepubescent victim and gender crossover: ", toString(ProportionYoungestVictimPrepubescentAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("YoungestVictim pubescent victim and gender crossover: ", toString(YoungestVictimPubescentAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion YoungestVictim pubescent victim and gender crossover: ", toString(ProportionYoungestVictimPubescentAndGenderCrossover)), OutputFileName, append = TRUE)     
  write(paste("YoungestVictim postpubescent victim and gender crossover: ", toString(YoungestVictimPostpubescentAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion YoungestVictim postpubescent victim and gender crossover: ", toString(ProportionYoungestVictimPostpubescentAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("YoungestVictim adult victim and gender crossover: ", toString(YoungestVictimAdultAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion YoungestVictim adult victim and gender crossover: ", toString(ProportionYoungestVictimAdultAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Has child under six victim and no gender crossover: ", toString(HasChildUnderSixVictimAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion YoungestVictim child under six victim and no gender crossover: ", toString(ProportionHasChildUnderSixVictimAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("YoungestVictim prepubescent victim and no gender crossover: ", toString(YoungestVictimPrepubescentAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion YoungestVictim prepubescent victim and no gender crossover: ", toString(ProportionYoungestVictimPrepubescentAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("YoungestVictim pubescent victim and no gender crossover: ", toString(YoungestVictimPubescentAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion YoungestVictim pubescent victim and no gender crossover: ", toString(ProportionYoungestVictimPubescentAndNoGenderCrossover)), OutputFileName, append = TRUE)     
  write(paste("YoungestVictim postpubescent victim and no gender crossover: ", toString(YoungestVictimPostpubescentAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion YoungestVictim postpubescent victim and no gender crossover: ", toString(ProportionYoungestVictimPostpubescentAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("YoungestVictim adult victim and no gender crossover: ", toString(YoungestVictimAdultAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion YoungestVictim adult victim and no gender crossover: ", toString(ProportionYoungestVictimAdultAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with unknown gender crossover: ", toString(NumberOfSuspectsWithUnknownGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion of suspects with unknown gender crossover: ", toString(ProportionSuspectsWithUnknownGenderCrossover)), OutputFileName, append = TRUE)
  
  #Violence for different victim age groups
  YoungestVictimPrepubescentAndHasInjuredVictim = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$YoungestVictim == "Prepubescent", na.rm = TRUE)
  YoungestVictimPubescentAndHasInjuredVictim = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$YoungestVictim == "Pubescent", na.rm = TRUE)
  YoungestVictimPostpubescentAndHasInjuredVictim = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$YoungestVictim == "Postpubescent", na.rm = TRUE)
  YoungestVictimAdultAndHasInjuredVictim = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$YoungestVictim == "Adult", na.rm = TRUE)
  YoungestVictimPrepubescentAndNoHasInjuredVictim = sum(SerialSuspectStatistics$HasInjuredVictim == FALSE & SerialSuspectStatistics$YoungestVictim == "Prepubescent", na.rm = TRUE)
  YoungestVictimPubescentAndNoHasInjuredVictim = sum(SerialSuspectStatistics$HasInjuredVictim == FALSE & SerialSuspectStatistics$YoungestVictim == "Pubescent", na.rm = TRUE)
  YoungestVictimPostpubescentAndNoHasInjuredVictim = sum(SerialSuspectStatistics$HasInjuredVictim == FALSE & SerialSuspectStatistics$YoungestVictim == "Postpubescent", na.rm = TRUE)
  YoungestVictimAdultAndNoHasInjuredVictim = sum(SerialSuspectStatistics$HasInjuredVictim == FALSE & SerialSuspectStatistics$YoungestVictim == "Adult", na.rm = TRUE)
  SuspectsWithUnknownViolence <- sum(is.na(SerialSuspectStatistics$HasInjuredVictim))
  
  RownamesHasInjuredVictimtable <- c('Prepubescent victim', 'Pubescent victim', 'Postpubescent victim', 'Adult victim')
  ColnamesHasInjuredVictimtable <- c('No violence', 'Violence', 'Total')

  HasInjuredVictimtable = matrix(c(1:12), ncol=3, byrow=TRUE)
  HasInjuredVictimtable[1, 1] = YoungestVictimPrepubescentAndNoHasInjuredVictim
  HasInjuredVictimtable[2, 1] = YoungestVictimPubescentAndNoHasInjuredVictim
  HasInjuredVictimtable[3, 1] = YoungestVictimPostpubescentAndNoHasInjuredVictim
  HasInjuredVictimtable[4, 1] = YoungestVictimAdultAndNoHasInjuredVictim
  HasInjuredVictimtable[1, 2] = YoungestVictimPrepubescentAndHasInjuredVictim
  HasInjuredVictimtable[2, 2] = YoungestVictimPubescentAndHasInjuredVictim
  HasInjuredVictimtable[3, 2] = YoungestVictimPostpubescentAndHasInjuredVictim
  HasInjuredVictimtable[4, 2] = YoungestVictimAdultAndHasInjuredVictim
  HasInjuredVictimtable[1, 3] = YoungestVictimPrepubescentAndHasInjuredVictim + YoungestVictimPrepubescentAndNoHasInjuredVictim
  HasInjuredVictimtable[2, 3] = YoungestVictimPubescentAndHasInjuredVictim + YoungestVictimPubescentAndNoHasInjuredVictim
  HasInjuredVictimtable[3, 3] = YoungestVictimPostpubescentAndHasInjuredVictim + YoungestVictimPostpubescentAndNoHasInjuredVictim
  HasInjuredVictimtable[4, 3] = YoungestVictimAdultAndHasInjuredVictim + YoungestVictimAdultAndNoHasInjuredVictim
  
  colnames(HasInjuredVictimtable) = ColnamesHasInjuredVictimtable
  rownames(HasInjuredVictimtable) = RownamesHasInjuredVictimtable
  
  write("", OutputFileName, append = TRUE)
  write("Has injured victim table", OutputFileName, append = TRUE)
  write("------------------------", OutputFileName, append = TRUE)
 
  write.table(HasInjuredVictimtable, OutputFileName, append = TRUE, sep = ",", dec = ".", row.names = TRUE, col.names = TRUE)
  
  HasInjuredVictimtableWithoutTotal = HasInjuredVictimtable[1:3,1:2]
  resultschitestINJURY = chisq.test(HasInjuredVictimtableWithoutTotal)
  print(resultschitestINJURY)
  
  write("", OutputFileName, append = TRUE)
  write("Chi test injured victim", OutputFileName, append = TRUE)
  write("-----------------------", OutputFileName, append = TRUE)
  write(paste("Result chi test for injury with different age groups: ", toString(resultschitestINJURY)), OutputFileName, append = TRUE)
  
  # Pairwise proportion tests, injury
  InjuryPairwise <- pairwise.prop.test(HasInjuredVictimtableWithoutTotal)
  
  # Counts used for Cohen's h 
  InjuryCounts <- c(
    YoungestVictimPrepubescentAndHasInjuredVictim,
    YoungestVictimPubescentAndHasInjuredVictim,
    YoungestVictimPostpubescentAndHasInjuredVictim
  )
  
  TotalCountsInjury <- HasInjuredVictimtable[1:3,3]
  
  ProportionsInjury <- InjuryCounts / TotalCountsInjury
  
  # Cohen's h comparisons
  h_Injury_Pre_vs_Pub  <- cohen_h(ProportionsInjury[1], ProportionsInjury[2])
  h_Injury_Pre_vs_Post <- cohen_h(ProportionsInjury[1], ProportionsInjury[3])
  h_Injury_Pub_vs_Post <- cohen_h(ProportionsInjury[2], ProportionsInjury[3])
  
  write("", OutputFileName, append = TRUE)
  write("Cohen's h for pairwise comparisons between injury and victim age groups", OutputFileName, append = TRUE)
  write("----------------------------------------------------------------------------", OutputFileName, append = TRUE)
  write(paste("Cohen's h prepubescent vs pubescent: ", h_Injury_Pre_vs_Pub), OutputFileName, append = TRUE)
  write(paste("Cohen's h prepubescent vs postpubescent: ", h_Injury_Pre_vs_Post), OutputFileName, append = TRUE)
  write(paste("Cohen's h pubescent vs postpubescent: ", h_Injury_Pub_vs_Post), OutputFileName, append = TRUE)
  
  #Testing violence hypothesis
  NumberOfSuspectsWithViolenceAndGenderCrossover = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$GenderCrossover == TRUE, na.rm = TRUE)
  NumberOfSuspectsWithViolenceAndRelationshipCrossover = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$RelationshipCrossover == TRUE, na.rm = TRUE)
  NumberOfSuspectsWithViolenceAndAgeCrossover = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$AgeCrossover == TRUE, na.rm = TRUE)
  NumberOfSuspectsWithViolenceAndNoGenderCrossover = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$GenderCrossover == FALSE, na.rm = TRUE)
  NumberOfSuspectsWithViolenceAndNoRelationshipCrossover = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$RelationshipCrossover == FALSE, na.rm = TRUE)
  NumberOfSuspectsWithViolenceAndNoAgeCrossover = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE & SerialSuspectStatistics$AgeCrossover == FALSE, na.rm = TRUE)
  NumberOfSuspectsWithViolence = sum(SerialSuspectStatistics$HasInjuredVictim == TRUE, na.rm = TRUE)
  ProportionSuspectsWithViolenceAndNoGenderCrossover = NumberOfSuspectsWithViolenceAndNoGenderCrossover / NumberOfGenderSpecializedSuspects
  ProportionSuspectsWithViolenceAndNoRelationshipCrossover = NumberOfSuspectsWithViolenceAndNoRelationshipCrossover / NumberOfRelationshipSpecializedSuspects
  ProportionSuspectsWithViolenceAndNoAgeCrossover = NumberOfSuspectsWithViolenceAndNoAgeCrossover / NumberOfAgeSpecializedSuspects
  NumberOfSuspectsWithUnknownAgeCrossover <- sum(is.na(SerialSuspectStatistics$AgeCrossover))
  NumberOfSuspectsWithUnknownRelationshipCrossover <- sum(is.na(SerialSuspectStatistics$RelationshipCrossover))
  
  write(paste("Suspects with violence and gender crossover: ", toString(NumberOfSuspectsWithViolenceAndGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with violence and relationship crossover: ", toString(NumberOfSuspectsWithViolenceAndRelationshipCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with violence and age crossover: ", toString(NumberOfSuspectsWithViolenceAndAgeCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with violence and no gender crossover: ", toString(NumberOfSuspectsWithViolenceAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with violence and no relationship crossover: ", toString(NumberOfSuspectsWithViolenceAndNoRelationshipCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with violence and no age crossover: ", toString(NumberOfSuspectsWithViolenceAndNoAgeCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with violence: ", toString(NumberOfSuspectsWithViolence)), OutputFileName, append = TRUE)
  write(paste("Proportion of suspects with violence and no gender crossover: ", toString(ProportionSuspectsWithViolenceAndNoGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion of suspects with violence and no relationship crossover: ", toString(ProportionSuspectsWithViolenceAndNoRelationshipCrossover)), OutputFileName, append = TRUE)
  write(paste("Proportion of suspects with violence and no age crossover: ", toString(ProportionSuspectsWithViolenceAndNoAgeCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with unknown gender crossover: ", toString(NumberOfSuspectsWithUnknownGenderCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with unknown age crossover: ", toString(NumberOfSuspectsWithUnknownAgeCrossover)), OutputFileName, append = TRUE)
  write(paste("Suspects with unknown relationship crossover: ", toString(NumberOfSuspectsWithUnknownRelationshipCrossover)), OutputFileName, append = TRUE)
  
  # Testing if number of suspects who have injured at least one victim differs between crossover and specialized suspects
  BinomtestviolenceGENDER = binom.test(x = NumberOfSuspectsWithViolenceAndGenderCrossover, n = NumberOfGenderCrossoverSuspects, p = ProportionSuspectsWithViolenceAndNoGenderCrossover, alternative = c("two.sided"), conf.level = 0.95)
  BinomtestviolenceRELATIONSHIP = binom.test(x = NumberOfSuspectsWithViolenceAndRelationshipCrossover, n = NumberOfRelationshipCrossoverSuspects, p = ProportionSuspectsWithViolenceAndNoRelationshipCrossover, alternative = c("two.sided"), conf.level = 0.95)
  BinomtestviolenceAGE = binom.test(x = NumberOfSuspectsWithViolenceAndAgeCrossover, n = NumberOfAgeCrossoverSuspects, p = ProportionSuspectsWithViolenceAndNoAgeCrossover, alternative = c("two.sided"), conf.level = 0.95)
  
  #Find p-values from the tests
  p_valviolenceGENDER <- BinomtestviolenceGENDER$p.value
  p_valviolenceRELATIONSHIP <- BinomtestviolenceRELATIONSHIP$p.value
  p_valviolenceAGE <- BinomtestviolenceAGE$p.value
  p_valuesviolence <- c(p_valviolenceGENDER, p_valviolenceRELATIONSHIP, p_valviolenceAGE)
  
  # Apply Holm-Bonferroni correction
  ViolenceAdjusted_p_values <- p.adjust(p_valuesviolence, method = "holm")
  
  # Finding effect size for violence hypothesis
  cohen_h_binom <- function(successes, trials, p_expected){
    # Calculate observed proportion
    p_obs <- successes / trials
    # Compute Cohen's h
   2 * (asin(sqrt(p_obs)) - asin(sqrt(p_expected)))
  }
  # Gender
  h_gender <- cohen_h_binom(
    successes = NumberOfSuspectsWithViolenceAndGenderCrossover,
    trials    = NumberOfGenderCrossoverSuspects,
   p_expected = ProportionSuspectsWithViolenceAndNoGenderCrossover
  )
  
  # Relationship
  h_relationship <- cohen_h_binom(
    successes = NumberOfSuspectsWithViolenceAndRelationshipCrossover,
    trials    = NumberOfRelationshipCrossoverSuspects,
    p_expected = ProportionSuspectsWithViolenceAndNoRelationshipCrossover
  )
  
  # Age
  h_age <- cohen_h_binom(
    successes = NumberOfSuspectsWithViolenceAndAgeCrossover,
    trials    = NumberOfAgeCrossoverSuspects,
    p_expected = ProportionSuspectsWithViolenceAndNoAgeCrossover
  )
  
  # Print results
  h_gender
  h_relationship
  h_age

  
  # Save output to file
  
  write(paste("BinomtestviolenceGENDER: ", toString(BinomtestviolenceGENDER)), OutputFileName, append = TRUE)
  write(paste("Cohens h, GENDER: ", toString(h_gender)), OutputFileName, append = TRUE)
  write(paste("BinomtestviolenceRELATIONSHIP: ", toString(BinomtestviolenceRELATIONSHIP)), OutputFileName, append = TRUE)
  write(paste("Cohens h, GENDER: ", toString(h_relationship)), OutputFileName, append = TRUE)
  write(paste("BinomtestviolenceAGE: ", toString(BinomtestviolenceAGE)), OutputFileName, append = TRUE)
  write(paste("Cohens h, GENDER: ", toString(h_age)), OutputFileName, append = TRUE)
  write(paste("Violence : Holm-Bonferroni adjusted p-values for gender, relationship and age crossover: ", toString(ViolenceAdjusted_p_values)), OutputFileName, append = TRUE)
  
  #Save output to file: crossover statistics
  write("", OutputFileName, append = TRUE)
  write("Suspect and crossover numbers", OutputFileName, append = TRUE)
  write("------------------------------", OutputFileName, append = TRUE)
  write(paste("Total number of serial suspects: ", toString(NumberOfTotalSuspects)), OutputFileName, append = TRUE)
  write(paste("Mean age at first offence: ", toString(MeanAgeAtFirstCrime)), OutputFileName, append = TRUE)
  write(paste("Mean age at last offence: ", toString(MeanAgeAtLastCrime)), OutputFileName, append = TRUE)
  write(paste("Number of female serial suspects: ", toString(NumberOfFemaleSerialSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of female serial suspects: ", toString(ProportionOfFemaleSerialSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of male serial suspects: ", toString(NumberOfMaleSerialSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of male serial suspects: ", toString(ProportionOfMaleSerialSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of unknown gender serial suspects: ", toString(NumberOfUnknownGenderSerialSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of unknown gender serial suspects: ", toString(ProportionOfUnknownGenderSerialSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of gender crossover suspects: ", toString(NumberOfGenderCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of gender crossover suspects: ", toString(ProportionOfGenderCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of age crossover suspects: ", toString(NumberOfAgeCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of age crossover suspects: ", toString(ProportionOfAgeCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of relationship crossover suspects: ", toString(NumberOfRelationshipCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of relationship crossover suspects: ", toString(ProportionOfRelationshipCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of gender and age crossover suspects: ", toString(NumberOfGenderAndAgeCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of gender and age crossover suspects: ", toString(ProportionOfGenderAndAgeCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of gender and relationship crossover suspects: ", toString(NumberOfGenderAndRelationshipCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of gender and relationship crossover suspects: ", toString(ProportionOfGenderAndRelationshipCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of age and relationship crossover suspects: ", toString(NumberOfAgeAndRelationshipCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of age and relationship crossover suspects: ", toString(ProportionOfAgeAndRelationshipCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Number of gender, age and relationship crossover suspects: ", toString(NumberOfGenderAgeAndRelationshipCrossoverSuspects)), OutputFileName, append = TRUE)
  write(paste("Proportion of gender, age and relationship crossover suspects: ", toString(ProportionOfGenderAgeAndRelationshipCrossoverSuspects)), OutputFileName, append = TRUE)
  
  write("", OutputFileName, append = TRUE)
  write("Overlapdatatable", OutputFileName, append = TRUE)
  write("----------------", OutputFileName, append = TRUE)
  write.table(overlapdatatable, OutputFileName, append = TRUE, sep = ",", dec = ".", row.names = TRUE, col.names = TRUE)
  
  SerialSuspectStatisticsNoPersonalNumber = subset(SerialSuspectStatistics, select = -c(1:23, 26))
  file.create(OutputFileRawData)
  write.table(SerialSuspectStatisticsNoPersonalNumber, OutputFileRawData, append = FALSE, sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)
}

