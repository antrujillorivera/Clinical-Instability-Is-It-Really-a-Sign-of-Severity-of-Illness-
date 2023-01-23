library(tidyverse)
library(doSNOW)
library(readxl)

d1 <- read_xlsx("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-Meds-1-18_1-19 (updated 6-3-21).xlsx",
               sheet = 1,n_max = Inf,col_names = TRUE,
               col_types = c("skip" , # `Empi ID`
                             "text", #  `Encounter Alias`  
                             'skip' , #  `Nurse Unit` 
                             "text", #  `Drug Code` ,
                             "text", # `MDrug DIsplay` 
                             "text" , # ``Drug Primary Display
                             "skip" , #  Dose 
                             "skip" ,# `Dose Unit` 
                             "skip" , # Frequency  
                             "text",#  Route 
                             "date", # `Start Dt Tm`  
                             "date" ))  # `Start Dt Tm`
d2 <- read_xlsx("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-Meds-1-19_1-20 (update 6-3-21).xlsx",
                sheet = 1,n_max = Inf,col_names = TRUE,
                col_types = c("skip" , # `Empi ID`
                              "text", #  `Encounter Alias`  
                              'skip' , #  `Nurse Unit` 
                              "text", #  `Drug Code` ,
                              "text", # `MDrug DIsplay` 
                              "text" , # ``Drug Primary Display
                              "skip" , #  Dose 
                              "skip" ,# `Dose Unit` 
                              "skip" , # Frequency  
                              "text",#  Route 
                              "date", # `Start Dt Tm`  
                              "date" ))  # `Start Dt Tm`
d3 <- read_xlsx("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-Meds-1-20_1-21 (update 6-3-21).xlsx",
                sheet = 1,n_max = Inf,col_names = TRUE,
                col_types = c("skip" , # `Empi ID`
                              "text", #  `Encounter Alias`  
                              'skip' , #  `Nurse Unit` 
                              "text", #  `Drug Code` ,
                              "text", # `MDrug DIsplay` 
                              "text" , # ``Drug Primary Display
                              "skip" , #  Dose 
                              "skip" ,# `Dose Unit` 
                              "skip" , # Frequency  
                              "text",#  Route 
                              "date", # `Start Dt Tm`  
                              "date" ))  # `Start Dt Tm`
d <- read_xlsx("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-Meds-1-21_3-21 (update 6-3-21).xlsx",
                sheet = 1,n_max = Inf,col_names = TRUE,
                col_types = c("skip" , # `Empi ID`
                              "text", #  `Encounter Alias`  
                              'skip' , #  `Nurse Unit` 
                              "text", #  `Drug Code` ,
                              "text", # `MDrug DIsplay` 
                              "text" , # ``Drug Primary Display
                              "skip" , #  Dose 
                              "skip" ,# `Dose Unit` 
                              "skip" , # Frequency  
                              "text",#  Route 
                              "date", # `Start Dt Tm`  
                              "date" ))  # `Start Dt Tm`

d <- rbind(d,d1,d2,d3)
rm(d1,d2,d3)

d %>%  distinct(`Encounter Alias`) %>% nrow() #  8,374

d <- d %>% rename(encounter_id = `Encounter Alias`,
                  d2 = `Drug Primary Display`,
                  d1 = `Drug Display`,
                  drug_code = `Drug Code`,
                  route = Route,
                  start_dt_tm = `Start Dt Tm`,
                  stop_dt_tm = `Stop Dt Tm` ) %>%
  filter( !is.na(start_dt_tm) & !is.na(stop_dt_tm) & 
            (!is.na(d2) | !is.na(d1) ) ) %>%
  mutate(d1 = tolower(d1), d2 = tolower(d2),
         d1 = trimws(d1) , d2 = trimws(d2),
         route = tolower(route))  %>%
  distinct_all()

#######################################################################
#############################3 classes to be extrated from here
b <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Copy of Merge Multum Tables with 6 categories 2017_08_24-AKP.csv")
b  <- b %>% select(d3 = `generic drug name`,d4 = `Drug Name` , 
                   first_tier_1 = `First Tier Category Name` , 
                   second_tier_1 = `Second Tier Category Name`,
                   third_tier_1 = `Third Tier Category Name`,
                   first_tier_2 = `first tier category_name_2`,
                   second_tier_2 = `second tier category_name_2`,
                   third_tier_2 = `third tier category_name_2`) %>%
  mutate(d3 = tolower(d3),  d3 = trimws(d3),
         d4 = tolower(d4),  d4 = trimws(d4)) %>%
  distinct(d3,d4 , .keep_all = T) %>% mutate(joined = T)
#############################################################
#############################################
## first join by "d2" and d3
## then d1 and d3
a1 <- d %>% left_join(b %>% select(-d4) , by = c("d2" = "d3") )

a2 <- a1 %>% filter(is.na(joined)) %>%  
  select( names(d)   ) %>%
  left_join(b %>% select(-d4) , by = c("d1" = "d3"))
a1 <- a1 %>%  filter(!is.na(joined))

a3 <- a2 %>% filter(is.na(joined))  %>% select(-joined)
a2 <- a2 %>% filter(!is.na(joined))

a2 # done
a1 #  done
# a3 is not done

########################################################
## joining d2 and d4

## a3 are the cases I can not  classify yet
a3 <- a3 %>% select(names(d))
b1 <- a3 %>% left_join(b %>% select(-d3) %>% distinct(d4,.keep_all = T) , by = c("d2" = "d4"))

b2 <- b1 %>%
  filter(is.na(joined)) %>% 
  select(names(d)) %>%
  left_join(b %>% select(-d3) , 
            by = c("d1" = "d4"))
b1 <- b1 %>%  filter(!is.na(joined)) ##  THIS HAVE MOST OF THE CASES

b3 <- b2 %>% filter(is.na(joined))  
b2 <- b2 %>% filter(!is.na(joined))
b1 #is done
b2 # is done
b3 # is not done
#################################################################
# in b3 I have the  cases not able to join.
# ill check if there are some already classified with the same drug_code that has not been classified
b3 <- b3 %>% 
  filter(!is.na(drug_code)) %>% # this erase 8 rows
  select(names(d))  %>%
  distinct_all()
b3 %>% group_by(drug_code) %>%
  summarise(ns = n()) %>%
  group_by(ns) %>%
  summarise(ns2 = n()) %>%
  arrange(ns)

dready <- rbind(a1,a2,b1,b2) %>%
  distinct_all()
rm(a1,a2,a3,b1,b2)

dready %>% group_by(drug_code) %>%
  summarise(ns = n()) %>%
  group_by(ns) %>%
  summarise(ns2 = n()) %>%
  arrange(ns)

a4 <- b3 %>% distinct(drug_code) %>% ## II CAN NOT DO THIS FOR THE DATASET
  inner_join( dready ) %>%
  select(drug_code,contains("tier")) %>%
  distinct_all()
a4  <- b3 %>% inner_join(a4)
a4 

b4 <- b3 %>%  anti_join( a4 )

dready <- dready  %>% select(-joined) %>% rbind( a4)#### is ready
rm(a4)

b4# I have the cases not  VBLE TO JOIN
###########################################################################################
### I will try to join by d2, with d3 byt first  I eliminate numbers, .... in b
a4 <- b4 %>% distinct(d2)%>%
  filter(!is.na(d2)) %>%
  mutate(original = d2,
         d2 =  if_else(str_detect(d2 ,"hydroxytryptophan"),"hydroxytryptophan",d2),
         d2 = gsub("\\(.*","",d2), ##eliminita everting at and after '('
         d2 = gsub("\\,.*","",d2),  ##eliminita everting at and after ','
         d2 = gsub("\\+.*","",d2),
         d2 = gsub("\\_.*","",d2),
         d2 = gsub("1.*","",d2),
         d2 = gsub("2.*","",d2),
         d2 = gsub("3.*","",d2),
         d2 = gsub("4.*","",d2),
         d2 = gsub("5.*","",d2),
         d2 = gsub("6.*","",d2),
         d2 = gsub("7.*","",d2),
         d2 = gsub("8.*","",d2),
         d2 = gsub("9.*","",d2),
         d2 = gsub("0.*","",d2),
         d2 = gsub("two.*","",d2),
         d2 = gsub("oral t.*","",d2),
         d2 = gsub("junior.*","",d2),
         d2 = str_replace_all(d2,"-"," "),
         d2 = str_remove(d2 , "butt paste"),
         d2 = str_trim(d2)
  ) %>%
  arrange(d2)
z4 <- b %>% rename(original_b = d3) %>%
  mutate( d2 =  if_else(str_detect(original_b ,"hydroxytryptophan"),"hydroxytryptophan",original_b),
          d2 = gsub("\\(.*","",d2), ##eliminita everting at and after '('
          d2 = gsub("\\,.*","",d2),  ##eliminita everting at and after ','
          d2 = gsub("\\+.*","",d2),
          d2 = gsub("\\_.*","",d2),
          d2 = gsub("1.*","",d2),
          d2 = gsub("2.*","",d2),
          d2 = gsub("3.*","",d2),
          d2 = gsub("4.*","",d2),
          d2 = gsub("5.*","",d2),
          d2 = gsub("6.*","",d2),
          d2 = gsub("7.*","",d2),
          d2 = gsub("8.*","",d2),
          d2 = gsub("9.*","",d2),
          d2 = gsub("0.*","",d2),
          d2 = gsub("two.*","",d2),
          d2 = gsub("oral t.*","",d2),
          d2 = gsub("junior.*","",d2),
          d2 = str_replace_all(d2,"-"," "),
          d2 = str_remove(d2 , "butt paste"),
          d2 = str_trim(d2)
  ) %>%
  arrange(d2)  %>%
  distinct(d2,.keep_all = T)

a5 <- a4 %>% left_join(z4,by = "d2")  ## joining d2 with d3
a6 <- a5 %>% filter(is.na(joined)) %>% distinct(original) %>% rename(d2=original)
a5  <- a5 %>% filter(!is.na(joined)) %>%
  select(d2 = original , contains("tier"))
a5  <- b4 %>% inner_join(a5)
a6 <- b4 %>% inner_join(a6)
rm(b4)

a5 # is ready
a6 #is  not redy
###########################################
dready <- rbind(dready,a5)
rm(a5)
####################################################
# a6 contains the cases I still can not join
### I will try to join by d1, with d3
# a6 <- a6 %>% select(d2 = original)
a4 <- a6 %>% 
  filter(!is.na(d1)) %>%
  distinct(d1)%>%
  mutate(original = d1,
         d1 =  if_else(str_detect(original ,"hydroxytryptophan"),"hydroxytryptophan",original),
         d1 = gsub("\\(.*","",d1), ##eliminita everting at and after '('
         d1 = gsub("\\,.*","",d1),  ##eliminita everting at and after ','
         d1 = gsub("\\+.*","",d1),
         d1 = gsub("\\_.*","",d1),
         d1 = gsub("1.*","",d1),
         d1 = gsub("2.*","",d1),
         d1 = gsub("3.*","",d1),
         d1 = gsub("4.*","",d1),
         d1 = gsub("5.*","",d1),
         d1 = gsub("6.*","",d1),
         d1 = gsub("7.*","",d1),
         d1 = gsub("8.*","",d1),
         d1 = gsub("9.*","",d1),
         d1 = gsub("0.*","",d1),
         d1 = gsub("two.*","",d1),
         d1 = gsub("oral t.*","",d1),
         d1 = gsub("junior.*","",d1),
         d1 = str_replace_all(d1,"-"," "),
         d1 = str_remove(d1 , "butt paste"),
         d1 = str_trim(d1)
  ) %>%
  arrange(d1)

#
# a5 <- a4 %>% left_join(z4,by = "d2")  ## joining d2 with d3
# a6 <- a5 %>% filter(is.na(joined)) %>% distinct(original) %>% rename(d2=original)
# a5  <- a5 %>% filter(!is.na(joined)) %>%
#   select(d2 = original , contains("tier"))
# a5  <- b4 %>% inner_join(a5)
# a6 <- b4 %>% inner_join(a6)
# #
a7 <- a4 %>% left_join(z4  , by = c("d1" = "d2"))  ## joining d1 with d3
a8 <- a7 %>% filter(is.na(joined)) %>% distinct(original) %>% rename(d1=original)

a7  <- a7 %>% filter(!is.na(joined))%>% select(d1 = original , contains("tier"))
a7 <- a6 %>%  inner_join(a7)
a8 <- a6 %>% inner_join(a8)
a7 # is ready
a8 # is not classified
#####################################
dready <- dready %>% rbind(a7)
a8 <- a8 %>% left_join(z4,by = c("d1" = "d2")) %>% select(names(a7))
###############
a8 ## is not ready
a8 %>% distinct(d2) %>% nrow()

dready  <- dready %>% mutate(classified = T) %>%
  rbind(a8 %>% mutate(classified = F))


d %>% distinct_all() %>% nrow() # 657692
dready %>% distinct_all() %>% nrow() # 657691

d <- dready
rm(dready)
save(d,file = "meds_with_joined_multum.RData")
