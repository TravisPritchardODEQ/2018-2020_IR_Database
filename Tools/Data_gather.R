library(tidyverse)
library(openxlsx)

options(scipen = 999999999)
bacteria_coast_contact <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Bacteria_Coast_Contact_IR_Data_ALLDATA.csv", 
                                  stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-validation, -Conclusion)

bacteria_fresh_contact <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Bacteria_Fresh_Contact_IR_data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)%>%
  select(-validation, -Conclusion)


bacteria_Shell_harvest <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Bacteria_Shell_harvest_IR_data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)%>%
  select(-validation, -Conclusion)


chl <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Chla_IR_data_ALLDATA.csv",
                stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)


DO_cont_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_Continuous_Spawn_IR_data_ALLDATA.csv",
                          stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-crit_7Mi, -crit_Min, -crit_Instant) %>%
  mutate(crit_30D = 13) %>%
  rename(crit_spawn = crit_30D)%>%
  select(-validation, -Conclusion)


DO_cont_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_YearRound_continuous_IR_data_ALLDATA.csv",
                              stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)%>%
  select(-validation, -Conclusion)


DO_instant_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_Instant_Spawn_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-crit_7Mi, -crit_Min, -crit_Instant) %>%
  mutate(crit_30D = 13) %>%
  rename(crit_spawn = crit_30D)

DO_inst_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_YearRound_instant_IR_data_ALLDATA.csv",
                              stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)%>%
  select(-validation, -Conclusion)


DO_estuary_spawn <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_estuary_instant_Spawn_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)


DO_estuary_yearround <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/DO_Estuary_Yearround_IR_data_ALLDATA.csv",
                                 stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  select(-validation, -Conclusion)


pH <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/pH_IR_data_ALLDATA.csv",
               stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)%>%
  select(-validation, -Conclusion)


temp <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Temperature_IR_data_ALLDATA.csv",
                 stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) %>%
  mutate(Spawn_criteria = ifelse(Spawn_type == "Spawn", 13, "" ) ) 

temp <- temp[,c(1:12, 46, 13:49)] %>%
  select(-validation)

Tox_AL_Ammonia <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Ammonia_IR_Data_ALLDATA.csv",
                           stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate) 

Tox_AL_CU <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Cu_IR_Data_ALLDATA.csv",
                      stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_Hardness_Metals <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Hardness_Metals_IR_Data_ALLDATA.csv",
                                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_Others <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Others_IR_Data_ALLDATA.csv",
                          stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_AL_Penta <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/TOX_AL_Pentachlorophenol_IR_data_ALLDATA.csv",
                         stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_HH <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Tox_HH_IR_data_ALLDATA.csv",
                   stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)

Tox_HH_Hg_tissue <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Data/Tox_HH_hg_tissue_IR_data_ALLDATA.csv",
                             stringsAsFactors = FALSE) %>%
  arrange(AU_ID, MLocID, SampleStartDate)




# 


bacteria_coast_contact_basin <-  bacteria_coast_contact




bacteria_fresh_contact_basin <-   bacteria_fresh_contact 

bacteria_Shell_harvest_basin <-   bacteria_Shell_harvest 


save(bacteria_coast_contact,bacteria_fresh_contact,bacteria_Shell_harvest, chl, DO_cont_spawn, DO_cont_yearround, 
     DO_instant_spawn, DO_inst_yearround, DO_estuary_spawn,  DO_estuary_yearround, pH, temp,  Tox_AL_Ammonia,Tox_AL_CU,Tox_AL_Hardness_Metals,
     Tox_AL_Others, Tox_AL_Penta, Tox_HH, Tox_HH_Hg_tissue, file = "data/IR_data.Rdata" )



# -------------------------------------------------------------------------

load('data/IR_data.Rdata')
load("data/assessed_AUs.Rdata")

