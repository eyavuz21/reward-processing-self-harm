#Aims:
##To load all data files for FID task for EDG
##To merge and create data.frame for columns to go into scored data file
##To exclude RT values <0.1
##To sort columns 

# empty workspace 
rm(list=ls());

# install relevant libraries install.libraries("library")[once]
#install.packages("openxlsx");
#install.packages("readxl");
#install.packages("data.table");
#install.packages("tidyr");
#install.packages("tidyverse");
#install.packages("zoo");
#install.packages("purrr")

# load relevant libraries 
library(openxlsx); library(readxl); library(data.table); library(tidyr); library(tidyverse); library(zoo); library(dplyr);library(purrr)

# declare in which location on your computer you are working (this can also be through Box or Dropbox)
setwd("~/Desktop/Other academic work/Imagine Data/NARRrawinput"); 
getwd(); 

home <- "~/Desktop/Other academic work/Imagine Data/NARRrawinput";

data <- list.files(path = home,
                   pattern="*.csv") 
data1 <- list.files(path = home,
                   pattern="*.csv", 
                   full.names = T) %>% 
  map_df(function(data) read_csv(data, col_types = cols(.default = "c")))  

##having trouble with removal of practice trials
# remove all the rows that contain a value in column x (trial data)
#data2 <- data1[!(data1$trials_prac_1.thisRepN ==1 ),] 

#coding for block 1 or 2 
data1$Block<-(ifelse(is.na(data1$target_resp_So.rt)==F | is.na (data1$target_resp_M.rt)== F|is.na(data1$target_resp_SH.rt)== F, "Block1", ifelse(is.na(data1$target_resp_So2.rt)==F|is.na(data1$target_resp_M2.rt)==F|is.na(data1$target_resp_SH2.rt)== F, "Block2"," " )))

#merging columns for rt 
data1$RT_new <- ifelse(is.na(data1$target_resp_So.rt) == F, data1$target_resp_So.rt, ifelse(is.na(data1$target_resp_So2.rt) == F, data1$target_resp_So2.rt, ifelse(is.na(data1$target_resp_M.rt) == F, data1$target_resp_M.rt, ifelse(is.na(data1$target_resp_M2.rt) == F, data1$target_resp_M2.rt, ifelse(is.na(data1$target_resp_SH.rt) == F, data1$target_resp_SH.rt, ifelse(is.na(data1$target_resp_SH2.rt) == F, data1$target_resp_SH2.rt," " )))))) 

#Select wanted columns 
selData <- data.frame(cbind(data1$participant, data1$Block, data1$TrialNum, data1$TrialType, data1$RT_new));
colnames(selData) <- c("Participant", "Block", "TrialNumber", "Condition", "RT");

# make RT as numeric
selData$RT <- as.numeric(selData$RT); 
# remove all the rows that don't contain a value in RT 
selData <- selData[!(selData$RT == "NA" ),]; 
# remove the incorrect conditions (can use this to check if it worked: sum(selData$Correct==0)) )
selData$ExcludedTrials <-ifelse(selData$RT<0.1,selData$RT, " " ) 
selData$RT <- ifelse(selData$RT<0.1," ",selData$RT ) 
selData$RT <- as.numeric(selData$RT); 
#sort data
selData<-selData[with(selData, order(Participant,Condition,Block,TrialNumber)), ]
# Calculate the means per group
selData$RT <- as.numeric(selData$RT); 
mnselData <- selData %>%
  group_by(Participant,Condition) %>%      # these are your grouping variables. I have added ID as well in case you'll have bigger datasets later with multiple IDs
  summarise_at(vars(RT),              # this is the value for which you want to calculate the average (Number)
               list(mnData = mean),na.rm=T) 
# now you have a new variable called 'mndataFilt' that contains the calculated means. You can write this to an excelsheet, 
# or save this in a bigger frame if you want to write everything away at once 

#Total money won 
data1$total_money<-as.numeric(data1$total_money)
data1$participant<-as.character(data1$participant)
totalmoney<-aggregate( total_money ~ participant, data = data1, max)
totalmoney<- data.frame(totalmoney);
colnames(totalmoney) <- c("Participant", "TotalMoney")



##########################Modified outcomes####################
###Outcomes for blocks
# Calculate the means per group per block 
selData$RT <- as.numeric(selData$RT); 
mnselData_block <- selData %>%
  group_by(Participant,Condition,Block) %>%      # these are your grouping variables. I have added ID as well in case you'll have bigger datasets later with multiple IDs
  summarise_at(vars(RT),              # this is the value for which you want to calculate the average (Number)
               list(mnData = mean),na.rm=T) 
# now you have a new variable called 'mndataFilt' that contains the calculated means. You can write this to an excelsheet, 
# or save this in a bigger frame if you want to write everything away at once 

###Total mean + SD for mnData 
#Total mean 
mnselData$mnData <- as.numeric(mnselData$mnData); 
Mean_mnData <- mnselData %>%
  group_by(Condition) %>%      # these are your grouping variables. I have added ID as well in case you'll have bigger datasets later with multiple IDs
  summarise_at(vars(mnData),              # this is the value for which you want to calculate the average (Number)
               list(Mean =mean),na.rm=T)
Mean_mnData$Mean<-Mean_mnData$Mean*1000
#SD
mnselData$mnData <- as.numeric(mnselData$mnData); 
SD_mnData <- mnselData %>%
  group_by(Condition) %>%      # these are your grouping variables. I have added ID as well in case you'll have bigger datasets later with multiple IDs
  summarise_at(vars(mnData),              # this is the value for which you want to calculate the average (Number)
               list(SD =sd),na.rm=T)
SD_mnData$SD<-SD_mnData$SD*1000

Mean_SD_mnData<-merge(Mean_mnData, SD_mnData, by= c("Condition"))
Mean_SD_mnData<-na.omit(Mean_SD_mnData)

###Total mean+SD for mnData_block
#Total mean
mnselData_block$mnData <- as.numeric(mnselData_block$mnData); 
Mean_mnData_block <- mnselData_block %>%
  group_by(Condition,Block) %>%      # these are your grouping variables. I have added ID as well in case you'll have bigger datasets later with multiple IDs
  summarise_at(vars(mnData),              # this is the value for which you want to calculate the average (Number)
               list(Mean =mean),na.rm=T)
Mean_mnData_block$Mean<-Mean_mnData_block$Mean*1000
#SD
mnselData_block$mnData <- as.numeric(mnselData_block$mnData); 
SD_mnData_block <- mnselData_block %>%
  group_by(Condition,Block) %>%      # these are your grouping variables. I have added ID as well in case you'll have bigger datasets later with multiple IDs
  summarise_at(vars(mnData),              # this is the value for which you want to calculate the average (Number)
               list(SD =sd),na.rm=T)
SD_mnData_block$SD<-SD_mnData_block$SD*1000

Mean_SD_mnData_block<-merge(Mean_mnData_block, SD_mnData_block, by= c("Condition", "Block"))
Mean_SD_mnData_block<-na.omit(Mean_SD_mnData_block)

###Calculate accuracy 
#Premature response = excluded + cue response 
data1$Cue_Response <- coalesce(data1$cue_resp_SH.rt, data1$cue_resp_SH2.rt, data1$cue_resp_M.rt, data1$cue_resp_M2.rt, data1$cue_resp_So.rt, data1$cue_resp_So2.rt)
Cue_response<-data.frame(cbind(data1$participant, data1$Cue_Response))
colnames(Cue_response)<-c("Participant", "Cue_response")
Cue_response$Cue_response<-as.numeric(Cue_response$Cue_response)
Cue_Response<-aggregate(Cue_response~Participant, data = Cue_response, length)
Cue_Response<-Cue_Response[with(Cue_Response, order(Participant)), ]
selData$ExcludedTrials<-as.numeric(selData$ExcludedTrials)
Excluded_Trials<-aggregate(ExcludedTrials~Participant, data = selData, length)
Excluded_Trials<-Excluded_Trials[with(Excluded_Trials, order(Participant)), ]
Premature_response<-merge(Cue_Response, Excluded_Trials, by=c("Participant", "Participant"), all=T)
Premature_response[is.na(Premature_response)]=0
Premature_response$Premature_response <-cbind(Premature_response=rowSums(Premature_response[,c('Cue_response','ExcludedTrials')]))
Premature_Response<-data.frame(cbind(Premature_response$Participant, Premature_response$Premature_response))
colnames(Premature_Response)<-c("Participant", "Premature_response")

#Late response= (y=Late response) - (x=premature response + normal response)
data1$x<- coalesce(data1$target_resp_SH.rt, data1$target_resp_SH2.rt, data1$target_resp_M.rt, data1$target_resp_M2.rt, data1$target_resp_So.rt, data1$target_resp_So2.rt,
                   data1$cue_resp_SH.rt, data1$cue_resp_SH2.rt, data1$cue_resp_M.rt, data1$cue_resp_M2.rt, data1$cue_resp_So.rt, data1$cue_resp_So2.rt)

data1$y<- coalesce (data1$too_late_resp_SH.rt, data1$too_late_resp_SH2.rt, data1$too_late_resp_M.rt, data1$too_late_resp_M2.rt, data1$too_late_resp_So.rt, data1$too_late_resp_So2.rt)            

data1$Late_response<-ifelse( is.na(data1$x)==T, data1$y, "")
data1$Late_response<-as.numeric(data1$Late_response)
Late_response<-aggregate(Late_response~participant, data = data1, length)
Late_Response<- data.frame(cbind(Late_response$participant, Late_response$Late_response))
colnames(Late_Response)<-c("Participant", "Late_response")
Accuracy<-merge(Premature_Response,Late_Response , by=c("Participant", "Participant"), all=T)
selData$TrialNumber <- as.numeric(selData$TrialNumber)
total_trials <- selData %>%
  group_by(Participant) %>%
  summarize(Total_Trials = max(TrialNumber), .groups = 'drop')
selData <- selData %>%
  left_join(total_trials, by = "Participant")

###Calculate accuracy by condition 

#Premature response = excluded + cue response 
data1$Cue_Response <- coalesce(data1$cue_resp_SH.rt, data1$cue_resp_SH2.rt, data1$cue_resp_M.rt, data1$cue_resp_M2.rt, data1$cue_resp_So.rt, data1$cue_resp_So2.rt)
Cue_response_condition<-data.frame(cbind(data1$participant, data1$TrialType, data1$Cue_Response))
colnames(Cue_response_condition)<-c("Participant", "Condition","Cue_response")
Cue_response_condition$Cue_response<-as.numeric(Cue_response_condition$Cue_response)
Cue_Response_condition<-aggregate(Cue_response~Participant+Condition, data = Cue_response_condition, length)
Cue_Response_condition<-Cue_Response_condition[with(Cue_Response_condition, order(Participant, Condition)), ]
selData$ExcludedTrials<-as.numeric(selData$ExcludedTrials)
Excluded_Trials_condition<-aggregate(ExcludedTrials~Participant+Condition, data = selData, length)
Excluded_Trials_condition<-Excluded_Trials_condition[with(Excluded_Trials_condition, order(Participant, Condition)), ]
Premature_response_condition<-merge(Cue_Response_condition, Excluded_Trials_condition, by=c("Participant", "Condition"), all=T)
Premature_response_condition[is.na(Premature_response_condition)]=0
Premature_response_condition$Premature_response <-cbind(Premature_response_condition=rowSums(Premature_response_condition[,c('Cue_response','ExcludedTrials')]))
Premature_Response_condition<-data.frame(cbind(Premature_response_condition$Participant, Premature_response_condition$Condition,Premature_response_condition$Premature_response))
colnames(Premature_Response_condition)<-c("Participant", "Condition","Premature_response")

#Late response= (y=Late response) - (x=premature response + normal response)
data1$x_condition<- coalesce(data1$target_resp_SH.rt, data1$target_resp_SH2.rt, data1$target_resp_M.rt, data1$target_resp_M2.rt, data1$target_resp_So.rt, data1$target_resp_So2.rt,
                   data1$cue_resp_SH.rt, data1$cue_resp_SH2.rt, data1$cue_resp_M.rt, data1$cue_resp_M2.rt, data1$cue_resp_So.rt, data1$cue_resp_So2.rt)

data1$y_condition<- coalesce (data1$too_late_resp_SH.rt, data1$too_late_resp_SH2.rt, data1$too_late_resp_M.rt, data1$too_late_resp_M2.rt, data1$too_late_resp_So.rt, data1$too_late_resp_So2.rt)            

data1$Late_response_condition<-ifelse( is.na(data1$x_condition)==T, data1$y_condition, "")
data1$Late_response_condition<-as.numeric(data1$Late_response_condition)
Late_response_condition<-aggregate(Late_response_condition~participant+TrialType, data = data1, length)
Late_Response_condition<- data.frame(cbind(Late_response_condition$participant,Late_response_condition$TrialType ,Late_response_condition$Late_response_condition))
colnames(Late_Response_condition)<-c("Participant", "Condition","Late_response")
Accuracy_condition<-merge(Premature_Response_condition,Late_Response_condition, by=c("Participant", "Condition"), all=T)
Accuracy_condition <- Accuracy_condition %>%
  left_join(total_trials, by = "Participant")
Accuracy_condition$Premature_response <- as.numeric(Accuracy_condition$Premature_response)
Accuracy_condition$Late_response <- as.numeric(Accuracy_condition$Late_response)
Accuracy_condition$Error <- with(Accuracy_condition,Premature_response + Late_response)
Accuracy_condition$Correct_Trials <- with(Accuracy_condition, Total_Trials - (Premature_response + Late_response))
Accuracy_condition[is.na(Accuracy_condition)] <- 0
file_path <- "~/Desktop/selData.csv"
write.csv(selData, file = file_path, row.names = FALSE)

#Accuracy by block 
#coding for block 1 or 2 
data1$Blocking<-ifelse(is.na(data1$too_late_resp_So.rt)==F, "Social_block1", ifelse(is.na (data1$too_late_resp_M.rt)== F, "Money_block1", ifelse(is.na(data1$too_late_resp_SH.rt)== F, "SH_block1",
                                                                                                                                                 ifelse(is.na(data1$too_late_resp_So2.rt)==F, "Social_block2", ifelse(is.na(data1$too_late_resp_M2.rt)==F, "Money_block2", ifelse(is.na(data1$too_late_resp_SH2.rt)== F, "SH_block2", NA))))))
#Premature response = excluded + cue response 
data1$Cue_Response <- coalesce(data1$cue_resp_SH.rt, data1$cue_resp_SH2.rt, data1$cue_resp_M.rt, data1$cue_resp_M2.rt, data1$cue_resp_So.rt, data1$cue_resp_So2.rt)
Cue_response_block<-data.frame(cbind(data1$participant, data1$Blocking,data1$Cue_Response))
colnames(Cue_response_block)<-c("Participant", "Block","Cue_response")
Cue_response_block$Cue_response<-as.numeric(Cue_response_block$Cue_response)
Cue_Response_block<-aggregate(Cue_response~Participant+Block, data = Cue_response_block, length)
Cue_Response_block<-Cue_Response_block[with(Cue_Response_block, order(Participant, Block)), ]

#Select wanted columns 
selData_blocking <- data.frame(cbind(data1$participant, data1$Blocking, data1$RT_new));
colnames(selData_blocking) <- c("Participant", "Block", "RT");

# make RT as numeric
selData_blocking$RT <- as.numeric(selData_blocking$RT); 
# remove all the rows that don't contain a value in RT 
selData_blocking <- selData_blocking[!(selData_blocking$RT == "NA" ),]; 
# remove the incorrect conditions (can use this to check if it worked: sum(selData$Correct==0)) )
selData_blocking$ExcludedTrials <-ifelse(selData_blocking$RT<0.1,selData_blocking$RT, " " ) 
selData_blocking$RT <- ifelse(selData_blocking$RT<0.1," ",selData_blocking$RT ) 
selData_blocking$RT <- as.numeric(selData_blocking$RT); 
#sort data
selData_blocking<-selData_blocking[with(selData_blocking, order(Participant,Block)), ]
selData_blocking$ExcludedTrials<-as.numeric(selData_blocking$ExcludedTrials)
Excluded_Trials_block<-aggregate(ExcludedTrials~Participant+Block, data = selData_blocking, length)
Excluded_Trials_block<-Excluded_Trials_block[with(Excluded_Trials_block, order(Participant, Block)), ]
Premature_response_block<-merge(Cue_Response_block, Excluded_Trials_block, by=c("Participant", "Block"), all=T)
Premature_response_block[is.na(Premature_response_block)]=0
Premature_response_block$Premature_response <-cbind(Premature_response_block=rowSums(Premature_response_block[,c('Cue_response','ExcludedTrials')]))
Premature_Response_block<-data.frame(cbind(Premature_response_block$Participant,Premature_response_block$Block ,Premature_response_block$Premature_response))
colnames(Premature_Response_block)<-c("Participant", "Block","Premature_response")

#Late response= (y=Late response) - (x=premature response + normal response)
data1$x_block<- coalesce(data1$target_resp_SH.rt, data1$target_resp_SH2.rt, data1$target_resp_M.rt, data1$target_resp_M2.rt, data1$target_resp_So.rt, data1$target_resp_So2.rt,
                             data1$cue_resp_SH.rt, data1$cue_resp_SH2.rt, data1$cue_resp_M.rt, data1$cue_resp_M2.rt, data1$cue_resp_So.rt, data1$cue_resp_So2.rt)

data1$y_block<- coalesce(data1$too_late_resp_SH.rt, data1$too_late_resp_SH2.rt, data1$too_late_resp_M.rt, data1$too_late_resp_M2.rt, data1$too_late_resp_So.rt, data1$too_late_resp_So2.rt)            

data1$Late_response_block<-ifelse( is.na(data1$x_block)==T, data1$y_block, "")
data1$Late_response_block<-as.numeric(data1$Late_response_block)
Late_response_block<-aggregate(Late_response_block~participant+Blocking, data = data1, length)
Late_Response_block<- data.frame(cbind(Late_response_block$participant, Late_response_block$Blocking,Late_response_block$Late_response_block))
colnames(Late_Response_block)<-c("Participant", "Block","Late_response")
Accuracy_block<-merge(Premature_Response_block,Late_Response_block , by=c("Participant", "Block"), all=T)
Accuracy_block[is.na(Accuracy_block)]=0









#export this as excel file ready for copying 
setwd("~/Desktop/Other academic work/Imagine Data/IDtaskscriptoutputNARR"); 
################Scored data#################
write.csv(selData, "selDataR.csv")
#################Means##########################
write.csv(mnselData, "mnDataR.csv")
write.csv(Mean_SD_mnData, "Mean_SD_mnDataR.csv")
#################Means by block########################
write.csv(mnselData_block, "mnselData_blockR.csv")
write.csv(Mean_SD_mnData_block, "Mean_SD_mnData_blockR.csv")
##################Total money won and accuracy################
write.csv(totalmoney, "totalmoneyR.csv")
write.csv(Accuracy, "AccuracyR.csv")
write.csv(Accuracy_condition, "Accuracy_conditionR.csv")
write.csv(Accuracy_block, "Accuracy_blockR.csv")