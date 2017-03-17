#R - File for Propensity Score Matching and finding the most probable matches

#Path where the Data is stored.
DataDirectory <- "/Volumes/SocialWork/CCWB/TITLE_IV_E_WAIVER_DCS/OUTCOME_EVALUATION/DATA/2016/Raw Data/"
#Path when using CCWB system
DataDirectory2 <- "Z:/TITLE_IV_E_WAIVER_DCS/OUTCOME_EVALUATION/DATA/2016/Raw Data" 
DataDirectory_User <- "/Volumes/SocialWork/CCWB/TITLE_IV_E_WAIVER_DCS/OUTCOME_EVALUATION/DATA/2016/Raw Data"
setwd(DataDirectory)

#Reading the csv data
Base_Pop <- read.csv("ASU AFCARS Base-Pop.csv", header = T)
Participant_File <- read.csv("RPT187_PARTICIPANT_FILE_20170106_09231498.csv", header = T)
Person_File <- read.csv("RPT187_PERSON_FILE_20170106_09231498.csv", header = T)
Placement_File <- read.csv("RPT187_PLACEMENTS_FILE_20170106_09231498.csv", header = T)
Removal_File <- read.csv("RPT187_REMOVAL_FILE_20170106_09231498.csv", header = T)
Report_File <- read.csv("RPT187_REPORT_FILE_20170106_09231498.csv", header = T)
Services_File <- read.csv("RPT187_SERVICES_FILE_20170106_09231498.csv", header = T)

#Setting dates as dates in the tables
#Base_Pop$FC.21.LATEST.REMOVAL.DATE <- as.character(Base_Pop$FC.21.LATEST.REMOVAL.DATE)
Base_Pop$FC.21.LATEST.REMOVAL.DATE <- as.Date(Base_Pop$FC.21.LATEST.REMOVAL.DATE, format = "%m/%d/%y")
Base_Pop$FC.56.DATE.OF.DISCHARGE.FROM.CARE <- as.Date(Base_Pop$FC.56.DATE.OF.DISCHARGE.FROM.CARE, format = "%m/%d/%Y")
Base_Pop$FC.06.DATE.OF.BIRTH <- as.Date(Base_Pop$FC.06.DATE.OF.BIRTH, format = "%m/%d/%y")
Base_Pop$FC.23.CURRENT.PLACEMENT.DATE <- as.Date(Base_Pop$FC.23.CURRENT.PLACEMENT.DATE, format = "%m/%d/%Y")

Participant_File$Part.Run.Date <- as.Date(Participant_File$Part.Run.Date, format = "%m/%d/%Y")

Person_File$Part.Run.Date <- as.Date(Person_File$Part.Run.Date, format = "%m/%d/%Y")
Person_File$PERS.DOB <- as.Date(Person_File$PERS.DOB, format = "%m/%d/%Y")

Placement_File$Placem.Eff.Date <- as.Date(Placement_File$Placem.Eff.Date, format = "%m/%d/%Y")
Placement_File$Placem.End.Date <- as.Date(Placement_File$Placem.End.Date, format = "%m/%d/%Y")
Placement_File$Placem.Run.Date <- as.Date(Placement_File$Placem.Run.Date, format = "%m/%d/%Y")

Removal_File$REMVL.DATE <- as.Date(Removal_File$REMVL.DATE, format = "%m/%d/%Y")
Removal_File$REMVL.END.DATE <- as.Date(Removal_File$REMVL.END.DATE, format = "%m/%d/%Y")
Removal_File$REMVL.RUN.DT <- as.Date(Removal_File$REMVL.RUN.DT, format = "%m/%d/%Y")

Report_File$REPORT.Rprt.Date <- as.Date(Report_File$REPORT.Rprt.Date, format = "%m/%d/%Y")
Report_File$REPORT.Run.Date <- as.Date(Report_File$REPORT.Run.Date, format = "%m/%d/%Y")

Services_File$SVC.Eff.Date <- as.Date(Services_File$SVC.Eff.Date, format = "%m/%d/%Y")
Services_File$SVC.End.Date <- as.Date(Services_File$SVC.End.Date, format = "%m/%d/%Y")
Services_File$SVC.Run.Date <- as.Date(Services_File$SVC.Run.Date, format = "%m/%d/%Y")


#Subset Group home and sheter as congregate care type
Base_Pop2 <- Base_Pop[Base_Pop$CONGREGATE.CARE.TYPE == "SHELTER" | Base_Pop$CONGREGATE.CARE.TYPE=="GROUP HOME",]

#Subset the 1131 cases on which we have the information
common_cases <- intersect(Base_Pop2$FC.CASE.IDX, Removal_File$REMVL.CASE.ID)
Base_Pop3 <- Base_Pop2[Base_Pop2$FC.CASE.IDX %in% common_cases,]

#unit numbers that belong to avondale and tempe
Tempe_Unit_Numbers <- c('0010 00 07 000','0010 00 07 070','0010 00 07 071','0010 00 07 072', '0010 00 07 073',
                        '0010 00 07 074','0010 00 07 075','0010 00 07 076','0010 00 07 077','0010 00 07 078')

Avondale_Unit_Numbers <- c('0050 00 07 000','0050 00 07 070','0050 00 07 071','0050 00 07 072','0050 00 07 073',
                           '0050 00 07 074','0050 00 07 075','0050 00 07 076','0050 00 07 077')

#subset the records from Tempe and Avondale
Base_Pop4 <- Base_Pop3[Base_Pop3$FC.UNIT %in% Tempe_Unit_Numbers | Base_Pop3$FC.UNIT %in% Avondale_Unit_Numbers,]

#unit numbers that belong to regions in Maricopa other than Tempe and Avondale
Peoria_Unit_Numbers <- c('0050 00 05 000','0050 00 05 050','0050 00 05 051','0050 00 05 052','0050 00 05 053',
                         '0050 00 05 054','0050 00 05 055','0050 00 05 056')
Glendale_Unit_Numbers <- c('0050 00 06 000','0050 00 06 060','0050 00 06 061','0050 00 06 062','0050 00 06 063',
                           '0050 00 06 064','0050 00 06 065','0050 00 06 066','0050 00 06 067','0050 00 06 068',
                           '0050 00 06 069')
Thund_Mesa_Distone <- c('0050 00 08 000','0050 00 08 080','0050 00 08 081','0050 00 08 082','0050 00 08 083',
                        '0050 00 08 084','0050 00 08 085','0050 00 08 086','0050 00 08 087')
Thunderbird_Unit_Numbers <- c('0050 00 04 000','0050 00 04 040','0050 00 04 041','0050 00 04 042','0050 00 04 043',
                              '0050 00 04 044','0050 00 04 045','0050 00 04 046','0050 00 04 047','0050 00 04 048')
Phoenix_unit_Numbers <- c('0010 00 03 000','0010 00 03 030','0010 00 03 031','0010 00 03 032','0010 00 03 033',
                          '0010 00 03 034','0010 00 03 035','0010 00 03 036')
Mesa_Mcdonald_Unit_Numbers <- c('0010 00 05 000','0010 00 05 050','0010 00 05 051','0010 00 05 052','0010 00 05 053',
                                '0010 00 05 054','0010 00 05 055','0010 00 05 056','0010 00 05 057','0010 00 05 058')
South_Mountain <- c('0010 00 08 000','0010 00 08 080','0010 00 08 081','0010 00 08 082','0010 00 08 083',
                    '0010 00 08 084','0010 00 08 085','0010 00 08 086','0010 00 08 087','0010 00 08 088',
                    '0010 00 08 089')
University_Unit_Numbers <- c('0010 00 10 000','0010 00 10 100','0010 00 10 101','0010 00 10 102','0010 00 10 103',
                             '0010 00 10 104','0010 00 10 105','0010 00 10 106','0010 00 10 107')
South_Mesa_Unit_Numbers <- c('0010 00 06 000','0010 00 06 060','0010 00 06 061','0010 00 06 062','0010 00 06 063',
                             '0010 00 06 064','0010 00 06 065','0010 00 06 066','0010 00 06 067','0010 00 06 068')
Twntieth_Street <- c('0010 00 09 000','0010 00 09 090','0010 00 09 091','0010 00 09 092','0010 00 09 093',
                     '0010 00 09 094','0010 00 09 095','0010 00 09 096','0010 00 09 097','0010 00 09 098',
                     '0010 00 09 099')
#subset the records from regions other than Tempe and Avondale but those that belong to Maricopa
Base_Pop5 <- Base_Pop3[Base_Pop3$FC.UNIT %in% Peoria_Unit_Numbers | Base_Pop3$FC.UNIT %in% Glendale_Unit_Numbers
                       | Base_Pop3$FC.UNIT %in% Thund_Mesa_Distone | Base_Pop3$FC.UNIT %in% Thunderbird_Unit_Numbers
                       | Base_Pop3$FC.UNIT %in% Phoenix_unit_Numbers | Base_Pop3$FC.UNIT %in% Mesa_Mcdonald_Unit_Numbers
                       | Base_Pop3$FC.UNIT %in% South_Mountain | Base_Pop3$FC.UNIT %in% University_Unit_Numbers
                       | Base_Pop3$FC.UNIT %in% South_Mesa_Unit_Numbers | Base_Pop3$FC.UNIT %in% Twntieth_Street,]

#Base_Pop4 is split one - cases that have recieved treatment
#Base_Pop5 is split two - cases that have not received treatment
#We need to build variables for the two files - 
#1) Age at First Removal
#2) Race
#3) Gender
#4) Primary Language of Caretaker
#5) Number of Removals
#6) Hispanic (Yes/No)
#7) Marital STatus of Caretaker

#Merging Removal File with Person FIle
#Consider columnns for Removal and Person necessary for table construction
Removal_Filter <- Removal_File[,c(1, 2, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 37, 38)]
Person_Filter <- Person_File[,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
#Merge tables Removal_File and Person Detail Record
Removal_Person_merger <-  merge(x = Removal_Filter, by.x = "REMVL.CHILD.ID", y = Person_Filter, 
                                by.y="PERS.ID", all.x = TRUE)

Removal_Person_merger$PERS.GENDER <- as.character(Removal_Person_merger$PERS.GENDER)
Removal_Person_merger$PERS.GENDER[Removal_Person_merger$PERS.GENDER == 'M'] <- 1
Removal_Person_merger$PERS.GENDER[Removal_Person_merger$PERS.GENDER == 'F'] <- 2
Removal_Person_merger$PERS.GENDER <- as.numeric(Removal_Person_merger$PERS.GENDER)
#Merging Base_Pop4 with the Removal File - First Population
Base_Pop4_Removal <- merge(x = Base_Pop4, 
                           y = Removal_Person_merger, 
                           by.x = c("FC.CASE.IDX","FC.21.LATEST.REMOVAL.DATE", "FC.06.DATE.OF.BIRTH","FC.07.SEX"),
                           by.y = c("REMVL.CASE.ID","REMVL.DATE","PERS.DOB","PERS.GENDER"))

#Merging Base_Pop5 with the Removal File - Second Population
Base_Pop5_Removal <- merge(x = Base_Pop5, 
                           y = Removal_Person_merger, 
                           by.x = c("FC.CASE.IDX","FC.21.LATEST.REMOVAL.DATE", "FC.06.DATE.OF.BIRTH","FC.07.SEX"),
                           by.y = c("REMVL.CASE.ID","REMVL.DATE","PERS.DOB","PERS.GENDER"))

#We do an inner join here cause we dont want the table to be filled with NA's

#Function for checking the Child'Ids with the number of removal in removal file to that of the Base Pop File

CheckRemovals <- function(df,rem){
  n <- nrow(df)
  num_removals <- 0
  for(i in 1:n){
   ch_id <- df$REMVL.CHILD.ID[i]
   temp <- rem[rem$REMVL.CHILD.ID == ch_id,]
   num_removals[i] <- nrow(temp)
  }
  
  flag <- 0
  for(i in 1:n){
    if(df$FC.19.TIMES.REMOVED.FROM.FOME[i] == num_removals[i]){
      flag = flag
    }
    else{
      flag = 1
    }
  }
  
  if(flag == 1){
    print("The number of removals mentioned for the child's are incorrect and have been replaced with the correct data")
    return(num_removals)
  }
  
  else{
    print("Number of removals menitoned are right. No changes have been made")
    return(df$REMVL.CHILD.ID)
  }
  
}

#Calling the function statements for BasePop4_Removal and BasPop4_Removal
Base_Pop4_Removal$RemCheck <- CheckRemovals(Base_Pop4_Removal,Removal_File)
Base_Pop5_Removal$RemCheck <- CheckRemovals(Base_Pop5_Removal,Removal_File)

Base_Pop4_Removal$NumOfRemovals <- 0
Base_Pop5_Removal$NumOfRemovals <- 0

#As we are getting two values for the number of removals, as the data is not correct. We take the higher value between the two.

for(i in 1:nrow(Base_Pop4_Removal)){
  Base_Pop4_Removal$NumOfRemovals[i] <- max(Base_Pop4_Removal$FC.19.TIMES.REMOVED.FROM.FOME[i], Base_Pop4_Removal$RemCheck[i])
}

for(i in 1:nrow(Base_Pop5_Removal)){
  Base_Pop5_Removal$NumOfRemovals[i] <- max(Base_Pop5_Removal$FC.19.TIMES.REMOVED.FROM.FOME[i], Base_Pop5_Removal$RemCheck[i])
}

count <- 0
for(i in 1:nrow(Base_Pop4_Removal)){
  if(Base_Pop4_Removal$FC.19.TIMES.REMOVED.FROM.FOME[i] == Base_Pop4_Removal$RemCheck[i]){
    count <- count + 1
  }
}
print("The number of Children from INTERVENTION who have different Removal Number are : ")
nrow(Base_Pop4_Removal) - count

count <- 0
for(i in 1:nrow(Base_Pop5_Removal)){
  if(Base_Pop5_Removal$FC.19.TIMES.REMOVED.FROM.FOME[i] == Base_Pop5_Removal$RemCheck[i]){
    count <- count + 1
  }
}
print("The number of Children from CONTROL who have different Removal Number are : ")
nrow(Base_Pop5_Removal) - count

#Creating the two populations for the Matching by choosing required columns.
#Out of the 144 records in Population1, only 122 match for Maricopa county.
#Included only variables that could be potentially useful for matching
#Care needs to be taken to exclude missing variables
#Variables chosen in a way that no variables repeat
#FC.REMOVAL.COUNTY represents county of treatment and
#REMVL.COUNTY represents county of removal - assumption
#Age in days at removal in years given - which should suffice, age in days will make it more granular


Population1 <- Base_Pop4_Removal[,c(1,43,2,3,8,10,11,12,13,14,15,16,17,18,19,20,22,23,25,26,27,28,29,30,31,38,39,40,41,42,45,46,47,48,
                                    52,53,54,58,59,60,70)]
#we add a new column Sample_Type and declare population1 as treated.
Population1$Sample_Type <- as.factor('Treated')

Population2 <- Base_Pop5_Removal[,c(1,43,2,3,8,10,11,12,13,14,15,16,17,18,19,20,22,23,25,26,27,28,29,30,31,38,39,40,41,42,45,46,47,48,
                                    52,53,54,58,59,60,70)]
#we add a new column Sample_Type and declare population2 as non-treated.
Population2$Sample_Type <- as.factor('Non-Treated')

#Before we match the samples, we need to merge the two dataframes. 
#We create a new variable GROUP - of type logic - based on the variable Sample_Type
Population <- rbind(Population1,Population2)
Population$GROUP<- as.logical(Population$Sample_Type == 'Treated')

#check for any missing values
#FC.11.MENTAL.RETARDATION, FC.12.VISUAL.HEARING.IMPAIRED, FC.13.PHYS.DISABLED have 6 NAs each
#Family Structure Description has 53 blank strings, thus we will not consider these 4 covariates 
#in matching. We now have 122 treated and 651 non-treated cases

Test_Set <- Population[,c("REMVL.CHILD.ID","Gender","FC.08A.RACE.AM.IND","FC.08B.RACE.ASIAN","FC.08C.RACE.BLK","FC.08D.RACE.PAC.ISLE","FC.08E.RACE.WHITE","FC.8F.RACE.UNDET",
                                   "FC.09.HISPANIC.ORIGIN","INDIVIDUAL.AGE","Age.At.Removal", 
                                   "CONGREGATE.CARE.TYPE","FC.24.PLACE.COUNT.CURRENT.REMOVAL", "NumOfRemovals","Sample_Type")]

colnames(Test_Set)[1] <- "Child_Id"
colnames(Test_Set)[2] <- "Sex"
colnames(Test_Set)[3] <- "Race_Indian"
colnames(Test_Set)[4] <- "Race_Asian"
colnames(Test_Set)[5] <- "Race_BLK"
colnames(Test_Set)[6] <- "Race_Pac_ISL"
colnames(Test_Set)[7] <- "Race_White"
colnames(Test_Set)[8] <- "Race_Undetermined"
colnames(Test_Set)[9] <- "Hispanic_Origin"
colnames(Test_Set)[12] <- "Congregate_Care_Type"
colnames(Test_Set)[13] <- "Num_Of_Placements"

#Data column PCT Marital Status has 44 unknown values, so we cannot use that column for marking the match
#Data column Primary Language has 44 unknown values, so we cannot use that column for marking the match
#Hispanic has unknown and it is not advisable to take a variable that has unknown and use it for matching
#Cant use Race_Unknown as matching a kid with unknown race to another unknown race does not make sense

lapply(Test_Set, class)

#Changing Sex to a numeric variable
Test_Set$Sex <- as.character(Test_Set$Sex)
Test_Set$Sex[Test_Set$Sex == "Male"] <- 1 #Male is defined as 1
Test_Set$Sex[Test_Set$Sex == "Female"] <- 0 #Female is defined as 0
Test_Set$Sex <- as.integer(Test_Set$Sex)

#Changing Congregrate Care Type to Integer
Test_Set$Congregate_Care_Type <- as.character(Test_Set$Congregate_Care_Type)
Test_Set$Congregate_Care_Type[Test_Set$Congregate_Care_Type == "GROUP HOME"] <- 1
Test_Set$Congregate_Care_Type[Test_Set$Congregate_Care_Type == "SHELTER"] <- 0
Test_Set$Congregate_Care_Type <- as.integer(Test_Set$Congregate_Care_Type)

#Changing Sample_Type to Binary
Test_Set$Sample_Type <- as.character(Test_Set$Sample_Type)
Test_Set$Sample_Type[Test_Set$Sample_Type == "Treated"] <- 1
Test_Set$Sample_Type[Test_Set$Sample_Type == "Non-Treated"] <- 0
Test_Set$Sample_Type <- as.integer(Test_Set$Sample_Type)

lapply(Test_Set, class)

#Normalizing Age variable as we are using Eucledian Distance to calculate the Propensity Score
Test_Set$INDIVIDUAL.AGE_Normalized <- (Test_Set$INDIVIDUAL.AGE - min(Test_Set$INDIVIDUAL.AGE)) / (max(Test_Set$INDIVIDUAL.AGE) - min(Test_Set$INDIVIDUAL.AGE))
Test_Set$Age.At.Removal_Normalized <- (Test_Set$Age.At.Removal - min(Test_Set$Age.At.Removal)) / (max(Test_Set$Age.At.Removal) - min(Test_Set$Age.At.Removal))
Test_Set$NumOfRemovals_Normalized <- (Test_Set$NumOfRemovals - min(Test_Set$NumOfRemovals)) / (max(Test_Set$NumOfRemovals) - min(Test_Set$NumOfRemovals))
Test_Set$Num_Of_Placements_Normalized <- (Test_Set$Num_Of_Placements - min(Test_Set$Num_Of_Placements)) / (max(Test_Set$Num_Of_Placements) - min(Test_Set$Num_Of_Placements))

#Install and Load MatchIt Package
library(MASS)
library(MatchIt)

#Summary after normalizing age
m_n.out <- matchit(Sample_Type ~ INDIVIDUAL.AGE_Normalized + Age.At.Removal_Normalized + Race_Indian + Race_Asian + Race_BLK + Race_Pac_ISL + Race_White
                   + Congregate_Care_Type + Num_Of_Placements_Normalized + NumOfRemovals_Normalized , data = Test_Set, method = "nearest", ratio = 1, exact = c("Sex"))
summary(m_n.out)
m_n.out$match.matrix
#plot(m_n.out, type = "jitter")
plot(m_n.out, type = "hist")
m.data1 <- match.data(m_n.out)
#write.csv(m.data1, file = "Output_Only_Sex_Exact.csv")


#Summary exact matches for race parameters
m_n1.out <- matchit(Sample_Type ~ INDIVIDUAL.AGE_Normalized + Age.At.Removal_Normalized
                   + Congregate_Care_Type + Num_Of_Placements_Normalized + NumOfRemovals_Normalized , data = Test_Set, method = "nearest", ratio = 2, exact = c("Sex", "Race_Indian", "Race_Asian","Race_BLK", "Race_Pac_ISL", "Race_White"))
summary(m_n1.out)
m_n1.out$match.matrix
#plot(m_n1.out, type = "jitter")
plot(m_n1.out, type = "hist")
m.data2 <- match.data(m_n1.out)
#write.csv(m.data2, file = "Output_Race_Sex_Exact.csv")

#Summary exact matches for race parameters and using Optimal Matching Technique

## Optimal Mathcing Technique - This method tried to achieve a global optimal in terms of the kids that are matched to the treatment group. (More Information available in package details)

m_n2.out <- matchit(Sample_Type ~ INDIVIDUAL.AGE_Normalized + Age.At.Removal_Normalized
                    + Congregate_Care_Type + Num_Of_Placements_Normalized + NumOfRemovals_Normalized , data = Test_Set, method = "optimal", ratio = 1, exact = c("Sex", "Race_Indian", "Race_Asian","Race_BLK", "Race_Pac_ISL", "Race_White"))
summary(m_n2.out)
m_n2.out$match.matrix
#plot(m_n2.out, type = "jitter")
plot(m_n2.out, type = "hist")
m.data3 <- match.data(m_n2.out)
colnames(m.data1)[1] <- "ID"
#write.csv(m.data3, file = "Output_Race_Sex_Optimal.csv")

#Saving the Matches
Matches <- as.data.frame(m_n.out$match.matrix)
colnames(Matches)[1] <- "Only_Sex_Exact"

temp <- m_n1.out$match.matrix[,1]
Matches$Sex_Race_Exact <- temp

temp <- m_n1.out$match.matrix[,2] #Second Degree match for Sex_Race_Exact
Matches$Sex_Race_Exact2 <- temp       

temp <- m_n2.out$match.matrix[,1]
Matches$Sex_Race_Optimal <- temp

#Validating the matches with a check on the exactness conditions are met by the matching methods
validation <- function(output, mat){
  
  mat <- as.character(mat)
  mat <- as.integer(mat)
  Treatment <- output[output$Sample_Type == 1,]
  Control <- output[output$Sample_Type == 0,]
  Treatment$rownames <- rownames(Treatment)
  Treatment$rownames <- as.integer(Treatment$rownames)
  Control$rownames <- rownames(Control)
  Control$rownames <- as.integer(Control$rownames)
  Control <- Control[match(mat,Control$rownames),]
  
  #Checking for same sex
  Flag <- 0
  if(all(Treatment$Sex == Control$Sex)){
    Flag <- Flag + 1
  }
  
  if(Flag == 0){
    print("Sex is not the same betweem treatment and control")
    print(paste0( "Number different are : ", length(which(Treatment$Sex != Control$Sex))))
  }
  Flag <- 0
  
  #Checking for same Race_Indian
  Flag <- 0

  if(all(Treatment$Race_Indian == Control$Race_Indian)){
    Flag <- Flag + 1
  }
 
  if(Flag == 0){
    print("Race Indian is not the same betweem treatment and control")
    print(paste0( "Number different are : ", length(which(Treatment$Race_Indian != Control$Race_Indian))))
  }
  Flag <- 0
  
  #Checking for same Race Asian
  Flag <- 0
  
  if(all(Treatment$Race_Asian == Control$Race_Asian)){
      Flag <- Flag + 1
  }
  
  if(Flag == 0){
    print("Race_Asian is not the same betweem treatment and control")
    print(paste0( "Number different are : ", length(which(Treatment$Race_Asian != Control$Race_Asian))))
  }
  Flag <- 0
  
  
  #Checking for same Race_BLK
  Flag <- 0
  
  if(all(Treatment$Race_BLK == Control$Race_BLK)){
      Flag <- Flag + 1
  }
  
  if(Flag == 0){
    print("Race_BLK is not the same betweem treatment and control")
    print(paste0( "Number different are : ", length(which(Treatment$Race_BLK != Control$Race_BLK))))
  }
  Flag <- 0
  
  #Checking for same Race_Pac_ISL
  Flag <- 0
  
  if(all(Treatment$Race_Pac_ISL == Control$Race_Pac_ISL)){
      Flag <- Flag + 1
  }
  
  if(Flag == 0){
    print("Race_PAC_ISL is not the same betweem treatment and control")
    print(paste0( "Number different are : ", length(which(Treatment$Race_Pac_ISL != Control$Race_Pac_ISL))))
  }
  Flag <- 0
  
  #Checking for same Race_White
  Flag <- 0
  
  if(all(Treatment$Race_White == Control$Race_White)){
      Flag <- Flag + 1
  }
  
  if(Flag == 0){
    print("Race_WHITE is not the same betweem treatment and control")
    print(paste0( "Number different are : ", length(which(Treatment$Race_White != Control$Race_White))))
  }
  Flag <- 0
  
  #Checking for same Individual Age
  Flag <- 0
  
  if(all(Treatment$INDIVIDUAL.AGE == Control$INDIVIDUAL.AGE)){
    Flag <- Flag + 1
  }
  
  if(Flag == 0){
    print("INDIVIDUAL.AGE is not the same betweem treatment and control")
    print(paste0( "Number different are : ", length(which(Treatment$INDIVIDUAL.AGE != Control$INDIVIDUAL.AGE))))
    print(paste0( "Average difference in Individual Age of kids (Treatment - Control) = ", (sum(Treatment$INDIVIDUAL.AGE - Control$INDIVIDUAL.AGE)/144) ))
  }
  Flag <- 0
  
  #Checking for same Num_Of_Placements
  Flag <- 0
  
  if(all(Treatment$Num_Of_Placements == Control$Num_Of_Placements)){
    Flag <- Flag + 1
  }
  
  if(Flag == 0){
    print("Num_Placements is not the same betweem treatment and control")
    print(paste0( "Number different are : ", length(which(Treatment$Num_Of_Placements != Control$Num_Of_Placements))))
    print(paste0( "Average difference in number of placements (Treatment - Control) = ", (sum(Treatment$Num_Of_Placements == Control$Num_Of_Placements)/144) ))
  }
  Flag <- 0
  
  #Checking for same Num_Removals
  Flag <- 0
  
  if(all(Treatment$NumOfRemovals == Control$NumOfRemovals)){
    Flag <- Flag + 1
  }
  
  if(Flag == 0){
    print("Number Removals is not the same betweem treatment and control")
    print(paste0( "Number different are : ", length(which(Treatment$NumOfRemovals != Control$NumOfRemovals))))
    print(paste0( "Average difference in number of removals (Treatment - Control) = ", (sum(Treatment$NumOfRemovals - Control$NumOfRemovals)/144) ))
  }
  Flag <- 0
  
}

#Running Validation Function to see what all columns have exact matches for the 3 models made
validation(m.data1,Matches$Only_Sex_Exact)
validation(m.data2,Matches$Sex_Race_Exact)
validation(m.data2,Matches$Sex_Race_Exact2)
validation(m.data3,Matches$Sex_Race_Optimal)

#Now the model we need to select is the Sex_Race_Exact using nearest neighbours and we need to get the two results.

#The output file containing the names of the children are as follows:

final_output <- m.data2
final_output$rownames <- rownames(final_output)
final_output <- merge(x = final_output, y = Person_File[,c(1,2,3)], by.x = "Child_Id", by.y = "PERS.ID" , all.x = TRUE)
final_output <- final_output[,c("Child_Id", "PERS.LAST.NAME", "PERS.FIRST.NAME","rownames")]
colnames(final_output) <- c("Child_ID","Last_Name","First_Name","rownumber")
final_output$rownumber <- as.integer(final_output$rownumber)

match1 <- m_n1.out$match.matrix[,1]
match1 <- as.character(match1)
match1 <- as.integer(match1)

match2 <- m_n1.out$match.matrix[,2]
match2 <- as.character(match2)
match2 <- as.integer(match2)

Treatment_Kids <- final_output[1:144,]

control_match1 <- as.data.frame(match1)
colnames(control_match1) <- c("rownumber")
control_match1 <- merge(x = control_match1, y = final_output, by.x = "rownumber", by.y = "rownumber", all.x = TRUE)
control_match1 <- control_match1[match(match1,control_match1$rownumber),]

control_match2 <- as.data.frame(match2)
colnames(control_match2) <- c("rownumber")
control_match2 <- merge(x = control_match2, y = final_output, by.x = "rownumber", by.y = "rownumber", all.x = TRUE)
control_match2 <- control_match2[match(match2,control_match2$rownumber),]

#Final Output Data Frame is 

output_match <- cbind(Treatment_Kids,control_match1,control_match2)
output_match2 <- output_match[,c(1,2,3,6,7,8,10,11,12)] 
colnames(output_match2) <- c("Treatment_Child_ID","Treatment_Child_LastName","Treatment_Child_FirstName","ControlMatch1_Child_ID","ControlMatch1_Child_LastName"
                            ,"ControlMatch1_Child_FirstName","ControlMatch2_Child_ID","ControlMatch2_Child_LastName","ControlMatch2_Child_FirstName")



































