###############################################################################
# FINAL PROPOSAL - ANALYSIS
############################

#Set Up ----------------------------------------------------------------------

library(rlang)
library(dplyr)
library(skimr)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(Hmisc)
library(tigris)
library(sp)
library(rgdal)
library(tmap)



data <- read.csv(file = 'LA_Crime_Data_from_2020_to_Present.csv', stringsAsFactors = F)
View(data)
str(data)

# delete blank columns 
data <- subset(data, select = -Crm.Cd.2)
data <- subset(data, select = -Crm.Cd.3)
data <- subset(data, select = -Crm.Cd.4)
data <- subset(data, select = -DR_NO)
data <- subset(data, select = -Crm.Cd) #same as Crm.Cd.1
data <- subset(data, select = -Part.1.2)
data <- subset(data, select = -Status)
data <- subset(data, select = -Cross.Street)
data <- subset(data, select = -Date.Rptd)

View(data)
nrow(data)
ncol(data)
str(data)


#Check how many NA's for each variable
sapply(data, function(x) sum(is.na(x)))
skim(data)


#Extract year ----
date_time_format <- as.POSIXct(data$DATE.OCC, format = "%m/%d/%Y %H:%M:%S")
date_time_format
class(date_time_format)
year <- format(date_time_format, format = "%Y"); table(year)
sum(is.na(year))
length(year)

data <- data %>%
  mutate(hour = hour, month = month, year = year)
str(data)

#Remove points with missing values OR (lon, lat) = (0,0) ----
class(data$LAT)
class(data$LON)
class(data$LOCATION)
factor(data$LON) #We see 0!
factor(data$LAT) #We see 0!
matrix(c(data$LON, data$LAT), ncol = 2, byrow = F)

#Remove (0,0)
sum(data$LON == 0) #2279
sum(data$LAT == 0) #2279 -- same! Possible 2279 (0,0)
sum(data$LON == 0) / nrow(data) #0.004918305
sum(data$LAT == 0) / nrow(data) #0.004918305 -- same! Let's remove since < 1%

newData <- data[!data$LON == 0, ]
sum(newData$LON == 0) #No zero longitude
sum(newData$LAT == 0) #No zero latitude
factor(newData$LAT) #No 0 - confirm
factor(newData$LON) #No 0 - confirm

#Location is set, so let's look at criminal activity type ----
sort(unique(newData$Crm.Cd.Desc))
crm.desc <- newData$Crm.Cd.Desc
class(crm.desc)
sort(table(crm.desc), decreasing = T)


#[1] "ARSON"                                                   
#[2] "ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER"            
#[3] "ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT"          
#[4] "ATTEMPTED ROBBERY"                                       
#[5] "BATTERY - SIMPLE ASSAULT"                                
#[6] "BATTERY ON A FIREFIGHTER"                                
#[7] "BATTERY POLICE (SIMPLE)"                                 
#[8] "BATTERY WITH SEXUAL CONTACT"                             
#[9] "BEASTIALITY, CRIME AGAINST NATURE SEXUAL ASSLT WITH ANIM"
#[10] "BIGAMY"                                                  
#[11] "BIKE - ATTEMPTED STOLEN"                                 
#[12] "BIKE - STOLEN"                                           
#[13] "BOAT - STOLEN"                                           
#[14] "BOMB SCARE"                                              
#[15] "BRANDISH WEAPON"                                         
#[16] "BRIBERY"                                                 
#[17] "BUNCO, ATTEMPT"                                          
#[18] "BUNCO, GRAND THEFT"                                      
#[19] "BUNCO, PETTY THEFT"                                      
#[20] "BURGLARY"                                                
#[21] "BURGLARY FROM VEHICLE"                                   
#[22] "BURGLARY FROM VEHICLE, ATTEMPTED"                        
#[23] "BURGLARY, ATTEMPTED"                                     
#[24] "CHILD ABANDONMENT"                                       
#[25] "CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT"             
#[26] "CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT"                 
#[27] "CHILD ANNOYING (17YRS & UNDER)"                          
#[28] "CHILD NEGLECT (SEE 300 W.I.C.)"                          
#[29] "CHILD PORNOGRAPHY"                                       
#[30] "CHILD STEALING"                                          
#[31] "CONSPIRACY"                                              
#[32] "CONTEMPT OF COURT"                                       
#[33] "CONTRIBUTING"                                            
#[34] "COUNTERFEIT"                                             
#[35] "CREDIT CARDS, FRAUD USE ($950 & UNDER"                   
#[36] "CREDIT CARDS, FRAUD USE ($950.01 & OVER)"                
#[37] "CRIMINAL HOMICIDE"                                       
#[38] "CRIMINAL THREATS - NO WEAPON DISPLAYED"                  
#[39] "CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)"
#[40] "CRUELTY TO ANIMALS"                                      
#[41] "DEFRAUDING INNKEEPER/THEFT OF SERVICES, $950 & UNDER"    
#[42] "DEFRAUDING INNKEEPER/THEFT OF SERVICES, OVER $950.01"    
#[43] "DISCHARGE FIREARMS/SHOTS FIRED"                          
#[44] "DISHONEST EMPLOYEE - GRAND THEFT"                        
#[45] "DISHONEST EMPLOYEE - PETTY THEFT"                        
#[46] "DISRUPT SCHOOL"                                          
#[47] "DISTURBING THE PEACE"                                    
#[48] "DOCUMENT FORGERY / STOLEN FELONY"                        
#[49] "DOCUMENT WORTHLESS ($200 & UNDER)"                       
#[50] "DOCUMENT WORTHLESS ($200.01 & OVER)"                     
#[51] "DRIVING WITHOUT OWNER CONSENT (DWOC)"                    
#[52] "DRUGS, TO A MINOR"                                       
#[53] "DRUNK ROLL"                                              
#[54] "EMBEZZLEMENT, GRAND THEFT ($950.01 & OVER)"              
#[55] "EMBEZZLEMENT, PETTY THEFT ($950 & UNDER)"                
#[56] "EXTORTION"                                               
#[57] "FAILURE TO DISPERSE"                                     
#[58] "FAILURE TO YIELD"                                        
#[59] "FALSE IMPRISONMENT"                                      
#[60] "FALSE POLICE REPORT"                                     
#[61] "FIREARMS EMERGENCY PROTECTIVE ORDER (FIREARMS EPO)"      
#[62] "FIREARMS RESTRAINING ORDER (FIREARMS RO)"                
#[63] "GRAND THEFT / AUTO REPAIR"                               
#[64] "GRAND THEFT / INSURANCE FRAUD"                           
#[65] "HUMAN TRAFFICKING - COMMERCIAL SEX ACTS"                 
#[66] "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE"               
#[67] "ILLEGAL DUMPING"                                         
#[68] "INCEST (SEXUAL ACTS BETWEEN BLOOD RELATIVES)"            
#[69] "INDECENT EXPOSURE"                                       
#[70] "INTIMATE PARTNER - AGGRAVATED ASSAULT"                   
#[71] "INTIMATE PARTNER - SIMPLE ASSAULT"                       
#[72] "KIDNAPPING"                                              
#[73] "KIDNAPPING - GRAND ATTEMPT"                              
#[74] "LETTERS, LEWD  -  TELEPHONE CALLS, LEWD"                 
#[75] "LEWD CONDUCT"                                            
#[76] "LEWD/LASCIVIOUS ACTS WITH CHILD"                         
#[77] "LYNCHING"                                                
#[78] "LYNCHING - ATTEMPTED"                                    
#[79] "MANSLAUGHTER, NEGLIGENT"                                 
#[80] "ORAL COPULATION"                                         
#[81] "OTHER ASSAULT"                                           
#[82] "OTHER MISCELLANEOUS CRIME"                               
#[83] "PANDERING"                                               
#[84] "PEEPING TOM"                                             
#[85] "PETTY THEFT - AUTO REPAIR"                               
#[86] "PICKPOCKET"                                              
#[87] "PICKPOCKET, ATTEMPT"                                     
#[88] "PIMPING"                                                 
#[89] "PROWLER"                                                 
#[90] "PURSE SNATCHING"                                         
#[91] "PURSE SNATCHING - ATTEMPT"                               
#[92] "RAPE, ATTEMPTED"                                         
#[93] "RAPE, FORCIBLE"                                          
#[94] "RECKLESS DRIVING"                                        
#[95] "REPLICA FIREARMS(SALE,DISPLAY,MANUFACTURE OR DISTRIBUTE)"
#[96] "RESISTING ARREST"                                        
#[97] "ROBBERY"                                                 
#[98] "SEX OFFENDER REGISTRANT OUT OF COMPLIANCE"               
#[99] "SEX,UNLAWFUL(INC MUTUAL CONSENT, PENETRATION W/ FRGN OBJ"
#[100] "SEXUAL PENETRATION W/FOREIGN OBJECT"                     
#[101] "SHOPLIFTING-GRAND THEFT ($950.01 & OVER)"                
#[102] "SHOPLIFTING - ATTEMPT"                                   
#[103] "SHOPLIFTING - PETTY THEFT ($950 & UNDER)"                
#[104] "SHOTS FIRED AT INHABITED DWELLING"                       
#[105] "SHOTS FIRED AT MOVING VEHICLE, TRAIN OR AIRCRAFT"        
#[106] "SODOMY/SEXUAL CONTACT B/W PENIS OF ONE PERS TO ANUS OTH" 
#[107] "STALKING"                                                
#[108] "TELEPHONE PROPERTY - DAMAGE"                             
#[109] "THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD"
#[110] "THEFT FROM MOTOR VEHICLE - ATTEMPT"                      
#[111] "THEFT FROM MOTOR VEHICLE - GRAND ($950.01 AND OVER)"     
#[112] "THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)"         
#[113] "THEFT FROM PERSON - ATTEMPT"                             
#[114] "THEFT OF IDENTITY"                                       
#[115] "THEFT PLAIN - ATTEMPT"                                   
#[116] "THEFT PLAIN - PETTY ($950 & UNDER)"                      
#[117] "THEFT, COIN MACHINE - ATTEMPT"                           
#[118] "THEFT, COIN MACHINE - GRAND ($950.01 & OVER)"            
#[119] "THEFT, COIN MACHINE - PETTY ($950 & UNDER)"              
#[120] "THEFT, PERSON"                                           
#[121] "THREATENING PHONE CALLS/LETTERS"                         
#[122] "THROWING OBJECT AT MOVING VEHICLE"                       
#[123] "TILL TAP - GRAND THEFT ($950.01 & OVER)"                 
#[124] "TILL TAP - PETTY ($950 & UNDER)"                         
#[125] "TRESPASSING"                                             
#[126] "UNAUTHORIZED COMPUTER ACCESS"                            
#[127] "VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)" 
#[128] "VANDALISM - MISDEAMEANOR ($399 OR UNDER)"                
#[129] "VEHICLE - ATTEMPT STOLEN"                                
#[130] "VEHICLE - MOTORIZED SCOOTERS, BICYCLES, AND WHEELCHAIRS" 
#[131] "VEHICLE - STOLEN"                                        
#[132] "VIOLATION OF COURT ORDER"                                
#[133] "VIOLATION OF RESTRAINING ORDER"                          
#[134] "VIOLATION OF TEMPORARY RESTRAINING ORDER"                
#[135] "WEAPONS POSSESSION/BOMBING"                              



library(stringr)

#Creating a lexicon - Loop every criminal activity and categorize by type ----
for (i in 1:length(crm.desc)) {
  
  #Arson
  if (str_detect(crm.desc[i], "ARSON")) {
    newData$crm.type[i] <- "Arson"

  #Theft/Auto Repair
  } else if ( (str_detect(crm.desc[i], "BIKE") ||
               str_detect(crm.desc[i], "AUTO") ||
               str_detect(crm.desc[i], "BURGLARY FROM VEHICLE") ||
               str_detect(crm.desc[i], "THEFT FROM MOTOR VEHICLE") ||
               str_detect(crm.desc[i], "VEHICLE -")) ) {
    newData$crm.type[i] <- "Theft/Auto Repair"
    
  #Aggravated Assault
  } else if ( str_detect(crm.desc[i], "ASSAULT WITH DEADLY WEAPON") ||
             str_detect(crm.desc[i], "SHOTS") ) {
    newData$crm.type[i] <- "Aggravated Assault"
   
  #Simple Assualt
  } else if ( (str_detect(crm.desc[i], "SIMPLE") ||
               str_detect(crm.desc[i], "OTHER ASSAULT") ||
               str_detect(crm.desc[i], "BATTERY POLICE") ||
               str_detect(crm.desc[i], "BATTERY -") ||
               str_detect(crm.desc[i], "BATTERY ON")) &&
              (!str_detect(crm.desc[i], "CHILD") ||
               !str_detect(crm.desc[i], "PARTNER")) ) {
    newData$crm.type[i] <- "Simple Assault"
    
  #Sexual Assault
  } else if ( str_detect(crm.desc[i], "ORAL") ||
             str_detect(crm.desc[i], "RAPE") ||
             str_detect(crm.desc[i], "SEX OFFENDER") ||
             str_detect(crm.desc[i], "PEN") ||
             str_detect(crm.desc[i], "CONTACT") ||
             str_detect(crm.desc[i], "INDECENT") ||
             crm.desc[i] == "LETTERS, LEWD  -  TELEPHONE CALLS, LEWD" ||
             str_detect(crm.desc[i], "CONDUCT") ) {
    newData$crm.type[i] <- "Sexual Assault"
    
  #Animal Cruelty
  } else if ( str_detect(crm.desc[i], "ANIM") ) {
    newData$crm.type[i] <- "Animal Cruelty"
    
  #Property Theft
  } else if ( str_detect(crm.desc[i], "BOAT") ||
             str_detect(crm.desc[i], "ROBBERY") ||
             str_detect(crm.desc[i], "PERSON") ||
             str_detect(crm.desc[i], "PLAIN") ||
             str_detect(crm.desc[i], "TAP") ||
             str_detect(crm.desc[i], "PICKPOCKET") ||
             str_detect(crm.desc[i], "SHOPLIFT") ||
             str_detect(crm.desc[i], "PURSE") ||
             str_detect(crm.desc[i], "DRUNK") ||
             str_detect(crm.desc[i], "THEFT-GRAND")) {
    newData$crm.type[i] <- "Property Theft"
    
  #Weapon Possession
  } else if ( str_detect(crm.desc[i], "BRANDISH") ||
             str_detect(crm.desc[i], "POSSESSION") ||
             str_detect(crm.desc[i], "EMERGENCY") ||
             str_detect(crm.desc[i], "FIREARMS RESTRAINING ORDER") ) {
    newData$crm.type[i] <- "Weapon Possession"
  
  #Child Abuse
  } else if ( str_detect(crm.desc[i], "NEGLECT") ||
             str_detect(crm.desc[i], "ABUSE") ||
             str_detect(crm.desc[i], "ANNOYING") ||
             str_detect(crm.desc[i], "PORNOGRAPHY") ||
             str_detect(crm.desc[i], "ACTS WITH CHILD") ||
             str_detect(crm.desc[i], "CRM AGNST") ) {
    newData$crm.type[i] <- "Child Abuse"
  
  #Abduction  
  } else if ( str_detect(crm.desc[i], "STEAL") ||
             str_detect(crm.desc[i], "KIDNAP") ) {
    newData$crm.type[i] <- "Abduction"
    
  #Fraud  
  } else if ( str_detect(crm.desc[i], "CREDIT") ||
             str_detect(crm.desc[i], "EMBEZZLEMENT") ||
             str_detect(crm.desc[i], "INSURANCE") ||
             str_detect(crm.desc[i], "DISHONEST") ||
             str_detect(crm.desc[i], "IDENTITY") ||
             str_detect(crm.desc[i], "DEFRAUD") ||
             str_detect(crm.desc[i], "BUNCO") ||
             str_detect(crm.desc[i], "DOCUMENT") ||
             str_detect(crm.desc[i], "COUNTERFEIT") ||
             str_detect(crm.desc[i], "REPLICA")) {
    newData$crm.type[i] <- "Fraud"
    
  #Threats
  } else if ( str_detect(crm.desc[i], "SCARE") ||
             str_detect(crm.desc[i], "THREAT") ||
             str_detect(crm.desc[i], "STALKING") ||
             str_detect(crm.desc[i], "EXTORTION") ) {
    newData$crm.type[i] <- "Threats"
  
  #Burglary  
  } else if ( crm.desc[i] == "BURGLARY" ||
              str_detect(crm.desc[i], "BURGLARY, ATTEMPTED") ||
             str_detect(crm.desc[i], "COIN") ) {
    newData$crm.type[i] <- "Burglary"
  
  #Violations of Order/Law
  } else if ( str_detect(crm.desc[i], "COURT") || 
             str_detect(crm.desc[i], "VIOLATION") ||
             str_detect(crm.desc[i], "RESIST") ||
             str_detect(crm.desc[i], "FALSE") ) {
    newData$crm.type[i] <- "Violations of Order"
  
  #Homicide    
  } else if ( str_detect(crm.desc[i], "HOMICIDE") ||
             str_detect(crm.desc[i], "MANSLAUGHTER") ||
             str_detect(crm.desc[i], "LYNCHING") ) {
    newData$crm.type[i] <- "Homicide"

  #Damage/Disruption
  } else if ( str_detect(crm.desc[i], "DISRUPT") ||
             str_detect(crm.desc[i], "DISTURB") ||
             str_detect(crm.desc[i], "DISPERSE") ||
             str_detect(crm.desc[i], "THROW") ||
             str_detect(crm.desc[i], "DAMAGE") ||
             str_detect(crm.desc[i], "VANDALISM") ||
             str_detect(crm.desc[i], "DUMPING") ) {
    newData$crm.type[i] <- "Damage/Disruption"
  
  #Driving  
  } else if ( str_detect(crm.desc[i], "YIELD") ||
             str_detect(crm.desc[i], "DRIVING") ) {
    newData$crm.type[i] <- "Driving"
  
  #Property Trespass  
  } else if ( str_detect(crm.desc[i], "COMPUTER") ||
             str_detect(crm.desc[i], "PROPERTY") ||
             str_detect(crm.desc[i], "TRESPASSING") ||
             str_detect(crm.desc[i], "PROWLER") ||
             str_detect(crm.desc[i], "PEEPING") ) {
    newData$crm.type[i] <- "Property Trespass"
  
  #Human Trafficking/Prostitution 
  } else if ( str_detect(crm.desc[i], "TRAFFICKING") ||
             str_detect(crm.desc[i], "PANDER") ||
             str_detect(crm.desc[i], "PIMP") ) {
    newData$crm.type[i] <- "Human Trafficking/Prostitution"
  
  #Domestic Violence  
  } else if ( str_detect(crm.desc[i], "PARTNER") ) {
    newData$crm.type[i] <- "Domestic Violence"
  
  #Others
  } else {
    newData$crm.type[i] <- "Others"
  }
}


sort(unique(newData$crm.type))
sort(table(newData$crm.type), decreasing = T)


#Decide which criminal activity types to exclude or categorize more ----
sum(newData$crm.type == "Others") #3537
sum(newData$crm.type == "Others") / nrow(data) #0.007680671 < 0.01
newData_map <- newData[!newData$crm.type == "Others", ]
nrow(newData_map) / nrow(newData) #0.9922814
sort(unique(newData_map$crm.type))
sort(table(newData_map$crm.type), decreasing = T)
table(newData_map$crm.type, newData_map$year)


#Now, let's proceed with mapping and graphing ----
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")
library(ggmap)
library(tmap)

register_google(key = "API_key")

la_map <- get_map(location = c(-118.3436849, 34.0522342),
                  zoom = 10, scale = 4, maptype = "toner")
ggmap(la_map)



#Mapping of all redefined crimes as violent, financial, or others  ----
newData_map$crm.type.cat <- rep(0, nrow(newData_map))
for (i in 1:nrow(newData_map)) {
  
  if (str_detect(newData_map$crm.type[i], "Abduction") ||
      str_detect(newData_map$crm.type[i], "Assault") ||
      str_detect(newData_map$crm.type[i], "Animal") ||
      str_detect(newData_map$crm.type[i], "Arson") ||
      str_detect(newData_map$crm.type[i], "Child Abuse") ||
      str_detect(newData_map$crm.type[i], "Domestic Violence") ||
      str_detect(newData_map$crm.type[i], "Homicide") ||
      str_detect(newData_map$crm.type[i], "Damage") ||
      str_detect(newData_map$crm.type[i], "Human") ||
      str_detect(newData_map$crm.type[i], "Driving") ||
      str_detect(newData_map$crm.type[i], "Threats")) {
    newData_map$crm.type.cat[i] <- "Violent Crime"
  } else if (str_detect(newData_map$crm.type[i], "Burglary") ||
             str_detect(newData_map$crm.type[i], "Fraud") ||
             str_detect(newData_map$crm.type[i], "Theft")) {
    newData_map$crm.type.cat[i] <- "Financial Crime"
  } else {
    newData_map$crm.type.cat[i] <- "Others"
  }
}


sort(unique(newData_map$crm.type.cat))
levels(factor(newData_map$crm.type.cat))
newData_map$crm.type.cat <- factor(newData_map$crm.type.cat,
                                   levels = c("Violent Crime", "Financial Crime", "Others"))
levels(factor(newData_map$crm.type.cat))
table(newData_map$crm.type, newData_map$crm.type.cat == "Others")
table(newData_map$crm.type.cat)
table(newData_map$crm.type.cat)/nrow(data)



ggmap(la_map) +
  geom_point(data = newData_map, 
             mapping = aes(x = LON, 
                           y = LAT, 
                           color = crm.type.cat),
             size = 0.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)


#Let's look by the year ----

#Showing graph of crime rates increasing each year
year1 <- newData_map[newData_map$year == "2020", ]
year2 <- newData_map[newData_map$year == "2021", ]
year3 <- newData_map[newData_map$year == "2022", ]

year1$crm.type.cat <- factor(year1$crm.type.cat,
                             levels = c("Violent Crime", "Financial Crime", "Others"))
year2$crm.type.cat <- factor(year2$crm.type.cat,
                             levels = c("Violent Crime", "Financial Crime", "Others"))
year3$crm.type.cat <- factor(year3$crm.type.cat,
                             levels = c("Violent Crime", "Financial Crime", "Others"))


#2020 ----
ggmap(la_map) +
  geom_point(data = year1, 
             mapping = aes(x = LON, 
                           y = LAT, 
                           color = crm.type.cat),
             size = 0.1) +
  xlab("Longitude") +
  ylab("Latitude")  +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)


#2021 ----
ggmap(la_map) +
  geom_point(data = year2, 
             mapping = aes(x = LON, 
                           y = LAT, 
                           color = crm.type.cat),
             size = 0.1) +
  xlab("Longitude") +
  ylab("Latitude")  +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)



#2022 ----
ggmap(la_map) +
  geom_point(data = year3, 
             mapping = aes(x = LON, 
                           y = LAT, 
                           color = crm.type.cat),
             size = 0.1) +
  xlab("Longitude") +
  ylab("Latitude")  +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)



#Understand criminal activities by the police patrolling community areas ----
str(newData)
table(newData$AREA)
table(newData$AREA.NAME)
unique(matrix(c(newData$AREA, newData$AREA.NAME), ncol = 2, byrow = F)) 


#1 = Central Area ----
central_map <- get_map(location = c(-118.25555, 34.039755),
                       zoom = 13, scale = 8, maptype = "toner")
central <- newData_map[newData_map$AREA.NAME == "Central", ]
ggmap(central_map, darken = c(0.2, "white")) +
  geom_point(data = central, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(central$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(central$crm.type)/nrow(data)*100


#2 = Rampart Area ----
rampart_map <- get_map(location = c(-118.267199, 34.056599),
                       zoom = 13, scale = 8, maptype = "toner")
rampart <- newData_map[newData_map$AREA.NAME == "Rampart", ]
ggmap(rampart_map, darken = c(0.2, "white")) +
  geom_point(data = rampart, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(rampart$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(rampart$crm.type)/nrow(data)*100


#3 = Southwest Area ----
sw_map <- get_map(location = c(-118.323900, 34.015000),
                       zoom = 12, scale = 21, maptype = "toner")
sw <- newData_map[newData_map$AREA.NAME == "Southwest", ]
ggmap(sw_map, darken = c(0.2, "white")) +
  geom_point(data = sw,
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 0.7) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(sw$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(sw$crm.type)/nrow(data)*100


#4 = Hollenbeck Area ----
hollenbeck_map <- get_map(location = c(-118.185500, 34.0503000), 
                          zoom = 12, scale = 8, maptype = "toner")
hollenbeck <- newData_map[newData_map$AREA.NAME == "Hollenbeck", ]
ggmap(hollenbeck_map, darken = c(0.2, "white")) +
  geom_point(data = hollenbeck, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 0.7) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(Hollenbeck$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(Hollenbeck$crm.type)/nrow(data)*100


#5 = Harbor Area ----
harbor_map <- get_map(location = c(-118.296500, 33.850700), 
                      zoom = 11, scale = 8, maptype = "toner")
harbor <- newData_map[newData_map$AREA.NAME == "Harbor", ]
ggmap(harbor_map, darken = c(0.2, "white")) +
  geom_point(data = harbor, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 0.7) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(harbor$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(harbor$crm.type)/nrow(data)*100


#6 = Hollywood Area ----
hw_map <- get_map(location = c(-118.350000, 34.121800), 
                      zoom = 13, scale = 8, maptype = "toner")
hw <- newData_map[newData_map$AREA.NAME == "Hollywood", ]
ggmap(hw_map, darken = c(0.2, "white")) +
  geom_point(data = hw, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(hw$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(hw$crm.type)/nrow(data)*100


#7 = Wilshire Area ----
wilshire_map <- get_map(location = c(-118.345900, 34.057200), 
                        zoom = 13, scale = 8, maptype = "toner")
wilshire <- newData_map[newData_map$AREA.NAME == "Wilshire", ]
ggmap(wilshire_map, darken = c(0.2, "white")) +
  geom_point(data = wilshire, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(wilshire$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(wilshire$crm.type)/nrow(data)*100


#8 = West LA Area ----
wla_map <- get_map(location = c(-118.465200, 34.071200), 
                        zoom = 12, scale = 8, maptype = "toner")
wla <- newData_map[newData_map$AREA.NAME == "West LA", ]
ggmap(wla_map, darken = c(0.2, "white")) +
  geom_point(data = wla, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(wla$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(wla$crm.type)/nrow(data)*100


#9 = Van Nuys Area ----
van_map <- get_map(location = c(-118.449400, 34.170900), 
                   zoom = 13, scale = 8, maptype = "toner")
van <- newData_map[newData_map$AREA.NAME == "Van Nuys", ]
ggmap(van_map, darken = c(0.2, "white")) +
  geom_point(data = van, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(van$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(van$crm.type)/nrow(data)*100


#10 = West Valley ----
wval_map <- get_map(location = c(-118.525388, 34.184616), 
                   zoom = 12, scale = 8, maptype = "toner")
wval <- newData_map[newData_map$AREA.NAME == "West Valley", ]
ggmap(wval_map, darken = c(0.2, "white")) +
  geom_point(data = wval, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.25) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(wval$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(wval$crm.type)/nrow(data)*100


#11 = Northeast ----
ne_map <- get_map(location = c(-118.222355, 34.105000), 
                    zoom = 12, scale = 8, maptype = "toner")
ne <- newData_map[newData_map$AREA.NAME == "Northeast", ]
ggmap(ne_map, darken = c(0.2, "white")) +
  geom_point(data = ne, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.25) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(ne$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(ne$crm.type)/nrow(data)*100


#12 = 77th Street ----
st77_map <- get_map(location = c(-118.307500, 33.970300), 
                  zoom = 13, scale = 8, maptype = "toner")
st77 <- newData_map[newData_map$AREA.NAME == "77th Street", ]
ggmap(st77_map, darken = c(0.2, "white")) +
  geom_point(data = st77, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(st77$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(st77$crm.type)/nrow(data)*100


#13 = Newton ----
newt_map <- get_map(location = c(-118.256200, 34.012300), 
                    zoom = 13, scale = 8, maptype = "toner")
newt <- newData_map[newData_map$AREA.NAME == "Newton", ]
ggmap(newt_map, darken = c(0.2, "white")) +
  geom_point(data = newt, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(newt$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(newt$crm.type)/nrow(data)*100


#14 = Pacific ----
pac_map <- get_map(location = c(-118.426200, 33.962300), 
                    zoom = 12, scale = 8, maptype = "toner")
pac <- newData_map[newData_map$AREA.NAME == "Pacific", ]
ggmap(pac_map, darken = c(0.2, "white")) +
  geom_point(data = pac, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(pac$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(pac$crm.type)/nrow(data)*100


#15 = N Hollywood ----
nhw_map <- get_map(location = c(-118.381300, 34.187000), 
                   zoom = 12, scale = 8, maptype = "toner")
nhw <- newData_map[newData_map$AREA.NAME == "N Hollywood", ]
ggmap(nhw_map, darken = c(0.2, "white")) +
  geom_point(data = nhw, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(nhw$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(nhw$crm.type)/nrow(data)*100


#16 = Foothill ----
fh_map <- get_map(location = c(-118.362302, 34.262765), 
                   zoom = 12, scale = 8, maptype = "toner")
fh <- newData_map[newData_map$AREA.NAME == "Foothill", ]
ggmap(fh_map, darken = c(0.2, "white")) +
  geom_point(data = fh, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(fh$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(fh$crm.type)/nrow(data)*100


#17 = Devonshire ----
dev_map <- get_map(location = c(-118.531002, 34.2667), 
                  zoom = 12, scale = 8, maptype = "toner")
dev <- newData_map[newData_map$AREA.NAME == "Devonshire", ]
ggmap(dev_map, darken = c(0.2, "white")) +
  geom_point(data = dev, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(dev$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(dev$crm.type)/nrow(data)*100


#18 = Southeast ----
se_map <- get_map(location = c(-118.243683, 33.922235), 
                   zoom = 12, scale = 8, maptype = "toner")
se <- newData_map[newData_map$AREA.NAME == "Southeast", ]
ggmap(se_map, darken = c(0.2, "white")) +
  geom_point(data = se, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.35) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(se$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(se$crm.type)/nrow(data)*100


#19 = Mission ----
msn_map <- get_map(location = c(-118.458900, 34.272300), 
                  zoom = 12, scale = 8, maptype = "toner")
msn <- newData_map[newData_map$AREA.NAME == "Mission", ]
ggmap(msn_map, darken = c(0.2, "white")) +
  geom_point(data = msn, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(msn$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(msn$crm.type)/nrow(data)*100


#20 = Olympic ----
olym_map <- get_map(location = c(-118.305500, 34.059800), 
                   zoom = 13, scale = 8, maptype = "toner")
olym <- newData_map[newData_map$AREA.NAME == "Olympic", ]
ggmap(olym_map, darken = c(0.2, "white")) +
  geom_point(data = olym, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(olym$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(olym$crm.type)/nrow(data)*100


#21 = Topanga ----
top_map <- get_map(location = c(-118.602100, 34.191900), 
                    zoom = 12, scale = 8, maptype = "toner")
top <- newData_map[newData_map$AREA.NAME == "Topanga", ]
ggmap(top_map, darken = c(0.2, "white")) +
  geom_point(data = top, 
             mapping = aes(x = LON,y = LAT, color = crm.type.cat), size = 1.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)

sort(round(table(top$crm.type)/nrow(data)*100, digits = 4), decreasing = T)
table(top$crm.type)/nrow(data)*100


#Compare proportions of areas by crm.type ----
CrimeArea_Prop <- round(prop.table(table(newData_map$AREA.NAME, newData_map$crm.type.cat), 
                                   1), digits = 4)*100
CrimeArea_Prop <- as.data.frame.matrix(CrimeArea_Prop)
class(CrimeArea_Prop)

#Top 6 and Bottom 6 Locations with high violent crime rate
CrimeArea_Prop_Violent_high <- CrimeArea_Prop %>%
  arrange(desc(`Violent Crime`)) %>%
  head() 
CrimeArea_Prop_Violent_high

CrimeArea_Prop_Violent_low <- CrimeArea_Prop %>%
  arrange(desc(`Violent Crime`)) %>%
  tail() 
CrimeArea_Prop_Violent_low


#Top 6 and Bottom 6 Locations with high financial crime rate
CrimeArea_Prop_Financial_high <- CrimeArea_Prop %>%
  arrange(desc(`Financial Crime`)) %>%
  head()
CrimeArea_Prop_Financial_high

CrimeArea_Prop_Financial_low <- CrimeArea_Prop %>%
  arrange(desc(`Financial Crime`)) %>%
  head()
CrimeArea_Prop_Financial_low



#Police locations on LA map and crm.type proportions ----
pol_lon <- c(-118.277978135, -118.24951873058, -118.531371, -118.410486587,
             -118.289208833, -118.21287140481, -118.3306698, -118.468117012,
             -118.381256, -118.256085266, -118.1878501, -118.29115, 
             -118.41986, -118.266973, -118.27544, -118.3049806,
             -118.599583812, -118.451355, -118.26685, -118.54761, 
             -118.243683)
pol_lat <- c(33.970057517, 34.009038908691, 34.256869, 34.2531425605, 
             33.7575360638, 34.044723832702, 34.095818, 34.2731517133,
             34.187042, 34.0125249685, 34.1180642, 34.05023, 
             33.99164, 34.0567094, 33.93858, 34.0106033,
             34.2211926286, 34.189857, 34.05686, 34.19336,
             34.052235)

pol_loc <- as.data.frame(matrix(c(pol_lon, pol_lat), ncol = 2, byrow = F))


ggmap(la_map) +
  geom_point(data = newData_map, 
             mapping = aes(x = LON, 
                           y = LAT, 
                           color = crm.type.cat),
             size = 0.5) +
  geom_point(data = pol_loc, mapping = aes(V1, V2), size = 2, color = "black")+
  geom_label(data = pol_loc, mapping = aes(V1, V2), 
             label = c("77th Street", "Central", "Devonshire",
                       "Foothill", "Harbor", "Hollenbeck",
                       "Hollywood", "Mission", "N Hollywood",
                       "Newton", "Northeast", "Olympic", 
                       "Pacific", "Rampart", "Southeast",
                       "Southwest", "Topanga", "Van Nuys", 
                       "West LA", "West Valley", "Wilshire"),
             nudge_y = 0.002, size = 2) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(type = "seq", palette = "YlOrRd", direction = -1)


#Most Violent Crimes          
sort(table(se$crm.type), decreasing = T)
3989/sum(table(se$crm.type))
table(se$crm.type, se$year)["Simple Assault", ]

sort(table(st77$crm.type), decreasing = T)
4766 / sum(table(st77$crm.type))
table(st77$crm.type, st77$year)["Simple Assault", ]


#Most Financial Crimes
sort(table(wla$crm.type), decreasing = T)
6216/sum(table(wla$crm.type))
table(wla$crm.type, wla$year)["Theft/Auto Repair", ]

sort(table(pac$crm.type), decreasing = T)
8928 / sum(table(pac$crm.type))
table(pac$crm.type, pac$year)["Theft/Auto Repair", ]


