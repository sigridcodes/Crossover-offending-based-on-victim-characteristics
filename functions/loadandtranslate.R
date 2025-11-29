# Helper functions

InstallAndLoadPackages <- function() {
  # Define required packages for project and install missing packages
  list.of.packages <- c("dplyr", "stringr", "effectsize")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(dplyr)
  library(stringr)
  library(effectsize)
}

CreateDirectories <- function(BaseDirectory) {
  # Create folders for data and outputs
  dir.create(BaseDirectory, showWarnings = FALSE)
  dir.create(paste0(BaseDirectory, 'data'), showWarnings = FALSE)
  dir.create(paste0(BaseDirectory, 'data/raw_data'), showWarnings = FALSE)
  dir.create(paste0(BaseDirectory, 'data/processed_data'), showWarnings = FALSE)
  dir.create(paste0(BaseDirectory, 'data/metadata'), showWarnings = FALSE)
  dir.create(paste0(BaseDirectory, 'output'), showWarnings = FALSE) 
  dir.create(paste0(BaseDirectory, 'R'), showWarnings = FALSE)
  dir.create(paste0(BaseDirectory, 'scripts'), showWarnings = FALSE)
  dir.create(paste0(BaseDirectory, 'Rmd'), showWarnings = FALSE)
}

RecodeSuspectData <- function(SuspectData, WithIndex) {
  SuspectData = TranslateSuspectDataLabels(SuspectData)
  SuspectData$School_Education = RecodeSchoolEducation(SuspectData$School_Education, WithIndex = WithIndex)
  SuspectData$Gender = RecodeGender(SuspectData$Gender)
  SuspectData = FilterOutNonMaleSuspects(SuspectData)
  return(SuspectData)
}

TranslateSuspectDataLabels <- function(SuspectData) {
  SuspectData <- SuspectData %>%
  mutate(
    School_Education = Schulbildung,
    Gender = Geschlecht,
    Birth_Year = Geburtsjahr
  )
  return (SuspectData)
}
  
RecodeSchoolEducation <- function(School_Education, WithIndex = FALSE) {
  if (WithIndex) {
    School_Education <- recode(School_Education,
                               "01 OHNE SCHULBILDUNG" = "NO SCHOOLING",
                               "02 SONDERSCHULE" = "SPECIAL NEEDS SCHOOL",
                               "03 GRUNDSCHULE / HAUPTSCHULE" = "PRIMARY SCHOOL / GENERAL SCHOOL (until 9th or 10th grade)",
                               "04 MITTLERE REIFE / FACHOBERSCHULE" = "SCHOOL LEAVING CERTIFICATE (after 10 yrs of school) / VOCATIONAL SCHOOL (age 16 to 18)",
                               "05 ABITUR / FACHHOCHSCHULE" = "SCHOOL LEAVING CERTIFICATE (certificate of general qualification for university entrance after 12 or 13 yrs of schooling) / UNIVERSITY OF APPLIED SCIENCES",
                               "06 STUDIENABSCHLUSS" = "UNIVERSITY DEGREE",
                               "07 LEHRABSCHLUSS" = "APPRENTICESHIP",
                               "08 SCHULABSCHLUSS UNBEKANNT" = "SCHOOL COMPLETION UNKNOWN"
    )
  } else {
    School_Education <- recode(School_Education,
                               "OHNE SCHULBILDUNG" = "NO SCHOOLING",
                               "SONDERSCHULE" = "SPECIAL NEEDS SCHOOL",
                               "GRUNDSCHULE / HAUPTSCHULE" = "PRIMARY SCHOOL / GENERAL SCHOOL (until 9th or 10th grade)",
                               "MITTLERE REIFE / FACHOBERSCHULE" = "SCHOOL LEAVING CERTIFICATE (after 10 yrs of school) / VOCATIONAL SCHOOL (age 16 to 18)",
                               "ABITUR / FACHHOCHSCHULE" = "SCHOOL LEAVING CERTIFICATE (certificate of general qualification for university entrance after 12 or 13 yrs of schooling) / UNIVERSITY OF APPLIED SCIENCES",
                               "STUDIENABSCHLUSS" = "UNIVERSITY DEGREE",
                               "LEHRABSCHLUSS" = "APPRENTICESHIP",
                               "SCHULABSCHLUSS UNBEKANNT" = "SCHOOL COMPLETION UNKNOWN"
    )
  }
  return(School_Education)
}

RecodeGender <- function(Gender) {
  Gender <- recode(Gender,
                                "MAENNLICH" = "MALE",
                                "UNBEKANNT" = "UNKNOWN",
                                "WEIBLICH" = "FEMALE"
  )
  return(Gender)
}

FilterOutNonMaleSuspects <- function(SuspectData){
  print(paste0("number of unknown gender suspects: ",  sum(SuspectData$Gender == "UNKNOWN", na.rm=TRUE)))
  print(paste0("number of female gender suspects: ",  sum(SuspectData$Gender == "FEMALE", na.rm=TRUE)))
  MaleSuspects = subset(SuspectData, Gender=="MALE")
  return(MaleSuspects) 
}


FilterSpecialAgeSuspects <- function(SuspectData, lower_age_cap, higher_age_cap){
  print(paste0("Filtered out victims under lower_age_cap: ",  sum(SuspectData$Gender == "UNKNOWN", na.rm=TRUE)))
  print(paste0("number of female gender suspects: ",  sum(SuspectData$Gender == "FEMALE", na.rm=TRUE)))
}

RecodeCrimeAndVictimData <- function(CrimeAndVictimData) {
  CrimeAndVictimData = TranslateCrimeAndVictimDataLabels(CrimeAndVictimData)
  CrimeAndVictimData$victim_gender = TranslateVictimGender(CrimeAndVictimData$victim_gender)
  CrimeAndVictimData$victim_relationship_spatial_social = TranslateSocialRelationship(CrimeAndVictimData$victim_relationship_spatial_social)
  CrimeAndVictimData$victim_relationship_formal = TranslateFormalRelationship(CrimeAndVictimData$victim_relationship_formal)
  CrimeAndVictimData$injury = TranslateInjury(CrimeAndVictimData$injury)
  CrimeAndVictimData$crime = TranslateCrime(CrimeAndVictimData$crime)
  CrimeAndVictimData = FilterOutAdultVictims(CrimeAndVictimData)
  CrimeAndVictimData = FilterOutCSEM(CrimeAndVictimData)
  return(CrimeAndVictimData)
}

FilterOutCSEM <- function(CrimeAndVictimData){
  print(paste0("number of kipo victims: ",  sum(CrimeAndVictimData$Deliktsart == "kipo", na.rm=TRUE)))
  print(paste0("number of jupo victims: ",  sum(CrimeAndVictimData$Deliktsart == "jupo", na.rm=TRUE)))
  CSASuspects = subset(CrimeAndVictimData, CrimeAndVictimData$Deliktsart == "smvj" | CrimeAndVictimData$Deliktsart == "smvk")
  return(CSASuspects) 
}

TranslateCrimeAndVictimDataLabels <- function(CrimeAndVictimData) {
  CrimeAndVictimData <- CrimeAndVictimData %>%
    mutate(
       victim_gender  =  Opfergeschlecht,
       victim_age  =  Opfer_Alter,
       victim_relationship_spatial_social  =  Opfer_TV_Beziehung_raeumlich_sozial,
       victim_relationship_formal  =  Opfer_TV_Beziehung_formal,
       injury  =  Verletzung,
       crime  =  Delikt
    )
  return(CrimeAndVictimData)
}

TranslateVictimGender <- function(victim_gender) {
  victim_gender <- recode(victim_gender,
                                             "DIVERS" = "DIVERSE",
                                             "MAENNLICH" = "MALE",
                                             "UNBEKANNT" = "UNKNOWN",
                                             "WEIBLICH" = "FEMALE")
  return(victim_gender)
}

TranslateSocialRelationship <- function(victim_relationship_spatial_social) {
  victim_relationship_spatial_social <- recode(victim_relationship_spatial_social,
                                                                  "211 BETREUUNGSVERHAELTNIS IM KRANKENHAUS" = "Care Relationship in Hospital",
                                                                  "212 BETREUUNGSVERHAELTNIS IM SENIOREN-/PFLEGEHEIM" = "Care Relationship in Nursing Home",
                                                                  "219 BETREUUNGSVERHAELTNIS IM SONSTIGEN GESUNDHEITSWESEN" = "Care Relationship in Other Health Care",
                                                                  "213 BETREUUNGSVERHAELTNIS IN DER HAEUSLICHEN PFLEGE" = "Care Relationship in Home Care",
                                                                  "220 ERZIEHUNGS-/BETREUUNGSVERH. IM BILDUNGSWESEN" = "Educational Care Relationship in Education",
                                                                  "290 ERZIEHUNGS-/BETREUUNGSVERH. IM SONSTIGEN BEREICH" = "Educational Care Relationship in Other Field",
                                                                  "110 GEMEINSAMER HAUSHALT - ERZIEHUNGS-/BETREUUNGSVERH." = "Shared Household Educational Care Relationship",
                                                                  "190 GEMEINSAMER HAUSHALT - SONSTIGES VERHAELTNIS" = "Shared Household Other Relationship",
                                                                  "730 GESCHAEFTLICHE BEZIEHUNG" = "Business Relationship",
                                                                  "800 KEINE BEZIEHUNG" = "No Relationship",
                                                                  "710 NACHBARSCHAFT" = "Neighbourhood",
                                                                  "799 SONSTIGE RAEUMLICHE UND/ODER SOZIALE NAEHE" = "Other Spatial Social Proximity",
                                                                  "900 UNBEKANNT" = "UNKNOWN",
                                                                  "720 ZUGEHOERIGKEIT ZUM GLEICHEN BETRIEB" = "Affiliation to Same Company"
  )
  return(victim_relationship_spatial_social)
}

TranslateFormalRelationship <- function(victim_relationship_formal) {
  victim_relationship_formal <- recode(victim_relationship_formal,
                                                          "200 BEKANNTSCHAFT" = "Acquaintance",
                                                          "620 BEKANNTSCHAFT/FREUNDSCHAFT" = "Acquaintance/Friendship",
                                                          "114 EHEMALIGER EHEPARTNER / LEBENSPARTNER" = "Former Spouse/Partner",
                                                          "111 EHEPARTNER" = "Spouse",
                                                          "125 ELTERN / PFLEGEELTERN" = "Parents/Foster Parents",
                                                          "610 ENGE FREUNDSCHAFT" = "Close Friendship",
                                                          "112 ENKEL" = "Grandchild",
                                                          "630 FLUECHTIGE BEKANNTSCHAFT" = "Casual Acquaintance",
                                                          "400 FLUECHTIGE VORBEZIEHUNG" = "Casual Past Relationship",
                                                          "700 FORMELLE SOZIALE BEZIEHUNGEN" = "Formal Social Relationships",
                                                          "127 GESCHWISTER" = "Siblings",
                                                          "126 GROSSELTERN" = "Grandparents",
                                                          "800 KEINE BEZIEHUNG" = "No Relationship",
                                                          "500 KEINE VORBEZIEHUNG" = "No Past Relationship",
                                                          "121 KINDER / PFLEGEKINDER" = "Children/Foster Children",
                                                          "300 LANDSMANN" = "Fellow Countryman",
                                                          "113 PARTNER NICHTEHELICHER LEBENSGEMEINSCHAFTEN" = "Partners in Cohabiting Relationships",
                                                          "128 SCHWIEGERELTERN / -SOHN / -TOCHTER" = "In-Laws",
                                                          "190 SONSTIGE ANGEHOERIGE" = "Other Relatives",
                                                          "900 UNGEKLAERT" = "Unresolved",
                                                          "100 VERWANDTSCHAFT/ANGEHOERIGER" = "Kinship/Relative"
  )
  return(victim_relationship_formal)
}

TranslateInjury <- function(injury) {
  injury <- recode(injury,
                                      "2 LEICHT VERLETZT" = "MINOR INJURY",
                                      "1 NICHT VERLETZT" = "NOT INJURED",
                                      "3 SCHWER VERLETZT" = "SERIOUS INJURY",
                                      "4 TOEDLICH VERLETZT" = "FATAL INJURY",
                                      "0 UNBEKANNT" = "UNKNOWN")
  return(injury)
}

TranslateCrime <- function(crime) {
  crime <- recode(crime,
                                     "11110000 VERGEWALTIGUNG (ALTBESTAND)" = "11110000 RAPE (OLD REGULATION)",
                                     "11110000 VERGEWALTIGUNG UEBERFALLARTIG (EINZELTAETER) GEMAESS par. 177 ABS. 6 NR. 1, ABS. 7 UND 8 STGB" = "11110000 ROBBERY RAPE (SINGLE PERPETRATOR) ACCORDING TO § 177 PARA. 6 NO. 1, ABS. 7 AND 8 PENAL CODE",
                                     "11120000 VERGEWALTIGUNG UEBERFALLARTIG (DURCH GRUPPEN) GEMAESS par. 177 ABS. 6 NR. 2, ABS. 7 UND 8 STGB" = "11120000 ROBBERY RAPE (BY GROUPS) ACCORDING TO § 177 PARA. 6 NO. 2, ABS. 7 AND 8 PENAL CODE",
                                     "11130000 VERGEWALTIGUNG DURCH GRUPPEN GEMAESS par. 177 ABS. 6 NR. 2, ABS. 7 UND 8 STGB" = "11130000 RAPE BY GROUPS ACCORDING TO § 177 PARA. 6 NO. 2, ABS. 7 AND 8 PENAL CODE",
                                     "11140000 SONSTIGE STRAFTATEN GEMAESS par. 177 ABS. 6 NR. 1, ABS. 7 UND 8 STGB" = "11140000 OTHER OFFENSES ACCORDING TO § 177 PARA. 6 NO. 1, ABS. 7 AND 8 PENAL CODE",
                                     "11150000 SEXUELLER UEBERGRIFF, SEXUELLE NOETIGUNG UND VERGEWALTIGUNG MIT TODESFOLGE GEMAESS par. 178 STGB" = "11150000 SEXUAL ASSAULT, SEXUAL COERCION AND RAPE RESULTING IN DEATH ACCORDING TO § 178 PENAL CODE",
                                      "11160000 SEXUELLER UEBERGRIFF GEMAESS par. 177 ABS. 1, 2, 3, 4, 7, 8 UND 9 STGB" = "11160000 SEXUAL ASSAULT ACCORDING TO § 177 PARA. 1, 2, 3, 4, 7, 8 AND 9 PENAL CODE",
                                     "11171000 VERGEWALTIGUNG par. 177 ABS. 6 NR. 1, 2 STGB (OHNE SCHL. 111730)" = "11171000 RAPE § 177 PARA. 6 NO. 1, 2 PENAL CODE (EXCLUDING 111730)",
                                     "11172000 VERGEWALTIGUNG IM BESONDERS SCHWEREN FALL par. 177 ABS. 6 NR. 1, 2 I. V. M. ABS. 7, 8 STGB" = "11172000 RAPE IN PARTICULARLY SERIOUS CASES § 177 PARA. 6 NO. 1, 2 IN CONJUNCTION WITH ABS. 7, 8 PENAL CODE",
                                     "11173000 VERGEWALTIGUNG VON WIDERSTANDSUNFAEHIGEN PERSONEN (par. 177 ABS. 2 NR. 1, ABS. 4) GEM. par. 177 ABS. 6 NR. 1, 2 STGB" = "11173000 RAPE OF PERSONS UNABLE TO RESIST (§ 177 PARA. 2 NO. 1, ABS. 4) ACC. § 177 ABS. 6 NO. 1, 2 PENAL CODE",
                                     "11181000 SEXUELLER UEBERGRIFF IM BESONDERS SCHWEREN FALL par. 177 ABS. 1, 2 (OHNE NR. 1) I. V. M. ABS. 6 NR. 2, ABS. 7, 8 STGB" = "11181000 SEXUAL ASSAULT IN PARTICULARLY SERIOUS CASES § 177 PARA. 1, 2 (EXCLUDING NO. 1) IN CONJUNCTION WITH ABS. 6 NO. 2, ABS. 7, 8 PENAL CODE",
                                     "11182000 SEXUELLE NOETIGUNG IM BESONDERS SCHWEREN FALL par. 177 ABS. 5 I. V. M. ABS. 6 NR. 2, ABS. 7, 8 STGB" = "11182000 SEXUAL COERCION IN PARTICULARLY SERIOUS CASES § 177 PARA. 5 IN CONJUNCTION WITH ABS. 6 NO. 2, ABS. 7, 8 PENAL CODE",
                                     "11183000 SEXUELLER UEBERGRIFF AN WIDERSTANDSUNFAEHIGEN PERSONEN IM BESONDERS SCHWEREN FALL par. 177 ABS 2 NR. 1, ABS. 4 I. V. M. ABS. 6 NR. 2, ABS. 7, 8 NR. 2 STGB" = "11183000 SEXUAL ASSAULT ON PERSONS UNABLE TO RESIST IN PARTICULARLY SERIOUS CASES § 177 ABS. 2 NO. 1, ABS. 4 IN CONJUNCTION WITH ABS. 6 NO. 2, ABS. 7, 8 NO. 2 PENAL CODE",
                                     "11301000 SEXUELLER MISSBRAUCH VON SCHUTZBEFOHLENEN" = "11301000 SEXUAL ABUSE OF DEPENDENT PERSONS",
                                     "11303000 SEXUELLER MISSBRAUCH - AUSNUTZUNG EINER AMTSSTELLUNG" = "11303000 SEXUAL ABUSE - MISUSE OF OFFICE",
                                     "11304000 SEXUELLER MISSBRAUCH - AUSNUTZUNG EINES BERATUNGS- / BEHANDLUNGS- / BETREUUNGSVERHAELTNISSES" = "11304000 SEXUAL ABUSE - MISUSE OF A COUNSELING/ TREATMENT/ SUPERVISION RELATIONSHIP",
                                     "11311000 SEXUELLER MISSBRAUCH VON SCHUTZBEFOHLENEN (KIND)" = "11311000 SEXUAL ABUSE OF DEPENDENT PERSONS (CHILD)",
                                     "11312000 SEXUELLER MISSBRAUCH VON GEFANGENEN / VERWAHRTEN (KIND)" = "11312000 SEXUAL ABUSE OF PRISONERS / DETAINED (CHILD)",
                                     "11312000 SEXUELLER MISSBRAUCH VON GEFANGNENEN / VERWAHRTEN UNTER AUSNUTZUNG DER STELLUNG Z. N. VON KINDERN" = "11312000 SEXUAL ABUSE OF PRISONERS / DETAINED BY EXPLOITING THEIR POSITION RELATING TO CHILDREN",
                                     "11313000 SEXUELLER MISSBRAUCH AUSNUTZUNG EINER AMTSSTELLUNG ZUM NACHTEIL VON KINDERN" = "11313000 SEXUAL ABUSE MISUSE OF OFFICE TO THE DETRIMENT OF CHILDREN",
                                     "11314000 SEXUELLER MISSBRAUCH UNTER AUSNUTZUNG EINES BERATUNGS- / BEHANDLUNG- / BETREUUNGSVERHAELTNISSES Z. N. VON KINDERN" = "11314000 SEXUAL ABUSE UNDER MISUSE OF A COUNSELING/ TREATMENT/ SUPERVISION RELATIONSHIP TO THE DETRIMENT OF CHILDREN",
                                     "13101000 HANDLUNGEN NACH par. 176 ABS. 5 STGB" = "13101000 ACTIONS UNDER § 176 ABS. 5 PENAL CODE",
                                     "13101100 SEXUELLER MISSBRAUCH VON KINDERN - KINDER FUER SEX. HANDLUNGEN ANBIETET, NACHWEIS VERSPRICHT par. 176 ABS. 1 NR. 3 STGB" = "13101100 SEXUAL ABUSE OF CHILDREN - OFFERING OR PROMISING CHILDREN FOR SEXUAL ACTS par. 176(1) No. 3 StGB",
                                     "13101200 SEXUELLER MISSBRAUCH VON KINDERN - KINDER FUER SEX. MISSBRAUCH OHNE KOERPERKONTAKT ANBIETET, NACHWEIS VERSPRICHT, ZUR TAT VERABREDET par. 176A ABS. 2 STGB" =  "13101200 SEXUAL ABUSE OF CHILDREN - OFFERING CHILDREN FOR SEXUAL ABUSE WITHOUT PHYSICAL CONTACT par. 176A(2) StGB",
                                     "13101300 SEXUELLER MISSBRAUCH VON KINDERN - KIND ZUM VORBEREITENDEN EINWIRKEN ANBIETET, NACHWEIS VERSPRICHT, ZUR TAT VERABREDET par. 176B ABS. 2 STGB" = "13101300 SEXUAL ABUSE OF CHILDREN - OFFERING CHILD FOR PREPARATORY ACTIONS par. 176B(2) StGB",
                                     "13110000 SEXUALDELIKTE Z. N. VON KINDERN - ALTBESTAND" = "13110000 SEXUAL OFFENSES AGAINST CHILDREN - LEGACY CASES",
                                     "13110000 SEXUELLE HANDLUNGEN NACH par. 176 (1) UND (2) STGB (KIND)" = "13110000 SEXUAL ACTS UNDER par. 176(1) AND (2) StGB (CHILD)",
                                     "13110000 SEXUELLER MISSBRAUCH VON KINDERN - SEXUELLE HANDLUNGEN AN KIND/DURCH KIND VORNEHMEN LAESST par. 176 ABS 1 NR. 1 UND 2 STGB" = "13110000 SEXUAL ABUSE OF CHILDREN - COMMITTING SEXUAL ACTS ON/WITH CHILD par. 176(1) No. 1 AND 2 StGB",
                                     "13120000 EXHIBITIONISTISCHE / SEXUELLE HANDLUNGEN VOR KINDERN par. 176 ABS. 4 NR. 1 STGB" = "13120000 EXHIBITIONISTIC/SEXUAL ACTS IN FRONT OF CHILDREN par. 176(4) No. 1 StGB",
                                     "13120000 EXHIBITIONISTISCHE/SEXUELLE HANDLUNGEN VOR KINDERN par. 176A ABS. 1 NR. 1 STGB" = "13120000 EXHIBITIONISTIC/SEXUAL ACTS IN FRONT OF CHILDREN par. 176A(1) No. 1 StGB",
                                     "13120000 EXHIBTION. / SEXUELLE HANDLUNGEN VOR KINDERN (ALTBESTAND)" = "13120000 EXHIBITIONISTIC/SEXUAL ACTS IN FRONT OF CHILDREN - LEGACY CASES",
                                     "13130000 SEXUELLE HANDLUNGEN NACH par. 176 ABS. 4 NR. 2 STGB" = "13130000 SEXUAL ACTS UNDER par. 176(4) No. 2 StGB",
                                     "13130000 SEXUELLER MISSBRAUCH VON KINDERN - TAETER BESTIMMT KIND, SEXUELLE HANDLUNGEN AN SICH SELBST VORZUNEHMEN par. 176A ABS 1 NR. 2 STGB" = "13130000 SEXUAL ABUSE OF CHILDREN - OFFENDER INSTRUCTS CHILD TO PERFORM SEXUAL ACTS ON THEMSELVES par. 176A(1) No. 2 StGB",
                                     "13140000 EINWIRKEN AUF KINDER NACH par. 176 ABS. 4 NR. 3 UND 4 STGB" = "13140000 ACTING ON CHILDREN UNDER par. 176(4) Nos. 3 AND 4 StGB",
                                     "13141100 SEXUELLER MISSBRAUCH VON KINDERN - EINWIRKEN AUF KIND DURCH PORNOGRAPHISCHEN INHALT ODER ENTSPRECHENDE REDEN par. 176A ABS. 1 NR. 3 STGB" = "13141100 SEXUAL ABUSE OF CHILDREN - ACTING ON CHILD THROUGH PORNOGRAPHIC CONTENT OR SPEECH par. 176A(1) No. 3 StGB",
                                     "13142100 SEXUELLER MISSBRAUCH VON KINDERN - EINWIRKEN AUF KIND ZUR VORBEREITUNG SEX. MISSBRAUCHS par. 176B ABS. 1 STGB" = "13142100 SEXUAL ABUSE OF CHILDREN - ACTING ON CHILD FOR PREPARATORY SEXUAL ABUSE par. 176B(1) StGB",
                                     "13150000 SCHWERER SEXUELLER MISSBRAUCH VON KINDERN - VOLLZUG DES BEISCHLAFS MIT EINEM KIND ODER VORNAHME EINER AEHNLICHEN SEXUELLEN HANDLUNG NACH par. 176C ABS. 1 NR. 2 STGB" = "13150000 SEVERE SEXUAL ABUSE OF CHILDREN - FULL SEXUAL INTERCOURSE OR SIMILAR ACTS WITH A CHILD par. 176C(1) No. 2 StGB",
                                     "13160000 SCHWERER SEXUELLER MISSBRAUCH VON KINDERN - HERSTELLUNG UND VERBREITUNG PORNOGRAPHISCHER SCHRIFTEN par. 176C ABS. 2 STGB" = "13160000 SEVERE SEXUAL ABUSE OF CHILDREN - PRODUCTION AND DISTRIBUTION OF PORNOGRAPHIC MATERIAL par. 176C(2) StGB",
                                     "13170000 SONSTIGER SCHWERER SEXUELLER MISSBRAUCH VON KINDERN NACH par. 176A STGB" = "13170000 OTHER SEVERE SEXUAL ABUSE OF CHILDREN par. 176A StGB",
                                     "13180000 SEXUELLER MISSBRAUCH VON KINDERN MIT TODESFOLGE par. 176B STGB" = "13180000 SEXUAL ABUSE OF CHILDREN RESULTING IN DEATH par. 176B StGB",
                                     "13300000 SEXUELLER MISSBRAUCH VON JUGENDLICHEN" = "13300000 SEXUAL ABUSE OF ADOLESCENTS",
                                     "13310000 SEXUELLER MISSBRAUCH VON JUGENDLICHEN GEGEN ENTGELT par. 182 ABS. 2 STGB" = "13310000 SEXUAL ABUSE OF ADOLESCENTS FOR PAYMENT par. 182(2) StGB",
                                     "13370000 SONSTIGER SEXUELLER MISSBRAUCH VON JUGENDLICHEN par. 182 STGB" = "13370000 OTHER SEXUAL ABUSE OF ADOLESCENTS par. 182 StGB",
                                     "14110000 FOERDERUNG SEXUELLER HANDLUNGEN MINDERJAEHRIGER" = "14110000 PROMOTING SEXUAL ACTS INVOLVING MINORS",
                                     "14111000 FOERDERUNG SEXUELLER HANDLUNGEN MINDERJAEHRIGER DURCH VERMITTLUNG ODER GEGEN ENTGELT par. 180 STGB, ABS. 1, NR. 1, ODER ABS. 2" = "14111000 PROMOTING SEXUAL ACTS INVOLVING MINORS THROUGH ARRANGEMENTS OR FOR PAYMENT par. 180(1) No. 1 OR 180(2) StGB",
                                     "14117900 SONSTIGE FOERDERUNG SEXUELLER HANDLUNGEN MINDERJAEHRIGER par. 180 STGB" = "14117900 OTHER PROMOTING SEXUAL ACTS INVOLVING MINORS par. 180 StGB",
                                     "14120000 AUSBEUTUNG VON PROSTITUIERTEN" = "14120000 EXPLOITATION OF PROSTITUTES"
  )
  return(crime)
}

FilterOutAdultVictims <- function(CrimeAndVictimData) {
  ChildVictims = subset(CrimeAndVictimData, victim_age < 18)
  return(ChildVictims)
}
  
