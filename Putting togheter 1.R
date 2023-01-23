library(tidyverse)
library(lubridate)
library(doSNOW)
library(readxl)
## picu encounters
#picu <- read_csv("~/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/HF 2018 General Tables/picu_encounters.csv")

# total pediatric inpatients from hospitals that had at least one patient with med, lab and evennts and the patient has events
#h <- read_csv("ped_inpatients_for_ml_HF2018.csv")

# times of ICU admission 
#t <- read_csv("~/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/HF 2018 General Tables/tiempos_10_icu_28_floor_dec_23_2019.csv")
#t <- t %>% filter(times_icu >= 31*24) %>%
#  distinct(encounter_id)

### Extracting age  and sex
encs <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-ICU_DEMOGRAPHICS-4-23.csv")
encs <- encs %>%
  select(id = `Empi ID` ,  encounter_id = `Encounter Alias` , gender = Gender , birth = `Birth Date` , 
         admission =`Hospital Admission/Arrival Date-Time`) 

# a <- encs %>%  group_by(id) %>%
#   summarise( n_genders = n_distinct(gender),
#              n_birt = n_distinct(birth) ,
#              n_admis = n_distinct(admission)) %>%
#   ungroup()
# #
# View(a)
# 
# encs %>%  filter(id == "e41d6709-b474-43e7-8db3-70e26bcc2afa")  %>% View()
# 
# encs %>%  distinct(encounter_id ) %>% nrow()
# encs %>%  distinct( id) %>% nrow()
####################################
encs <- encs %>%  select(encounter_id,gender , birth , admission) %>% distinct(encounter_id , .keep_all = T)
encs <- encs %>%
  filter( str_starts(gender,"F|M")) %>%
  mutate(birth2 = as.POSIXct(birth , format = "%m/%d/%Y %H:%M"),
         admission2 = as.POSIXct(admission, format = "%m/%d/%Y %H:%M"),
         admission2 = if_else(admission2 < birth2 , admission2 +years(100) , admission2 ),
         ages_days = as.numeric(difftime(time1 = admission2  , time2 = birth2 , units = "days" ))) #%>%
# select(encounter_id , gender , ages_days  )
View(encs %>% filter(ages_days < 0))

summary(encs$ages_days) / 365

encs %>%  filter(ages_days / 365 >= 22) %>% nrow() # 192 patients

encs <- encs %>% filter(ages_days < 22 * 365 ) %>%
  select(encounter_id , gender , ages_days  )
gc()

## 28,220 e3ncounters

# Medication gathered, Notice it is only ICU patients: file d
load("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Medications/ICU_gathered_hours_med_presence_for_ML_CN.RData")
d <- encs  %>% distinct(encounter_id) %>%  inner_join(d, by = "encounter_id")
m <- d
rm(d)
gc()
m <- m %>%
  select(encounter_id,begin_hour,
         super_group = ranked_tier_name,
         n_distinct_generic_names_this_period)

# meds <- m %>% distinct(super_group) %>%
#   mutate(groups = row_number())
# save(meds,file  = "meds_clases.RData")
## meds:
load("~/Dropbox/Documents Mac Work/Creating Data for Neural Network HF 2018/meds_clases.RData")

m <- m %>% left_join( meds ) 

##################
# m %>%  filter(is.na(groups)) %>%
#   group_by(super_group) %>% summarise(n=n(),n_encs = n_distinct(encounter_id)) %>%
#   ungroup()
# super_group              n n_encs
# cftr modulators       2320      7
# cholinergic agonists    68      3
# herbal products       1534      4
# thrombolytics         3023     55
# vmat2 inhibitors       790      4
##################
## non of the super groups abbove were used previously
## 7735 records with the above medications: 0.03847906 % of the data
## I will erase such records
m  <- m %>% filter(!is.na(groups)) %>% select(-super_group)

#############################################
#laboratory dataset
l1 <- read_xlsx("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-labs-6_19-6_20.xlsx",
                sheet = 1,n_max = Inf,col_names = TRUE)
l2 <- read_xlsx("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-labs-6_18-6_19.xlsx",
                sheet = 1,n_max = Inf,col_names = TRUE)
l3 <- read_xlsx("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-labs-1_18-6_18.xlsx",
                sheet = 1,n_max = Inf,col_names = TRUE)
l4 <- read_xlsx("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-labs-6_20-3_21.xlsx",
                sheet = 1,n_max = Inf,col_names = TRUE)
# l <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Lab Master.csv",
#                 n_max = Inf)
l <- rbind(l1,l2,l3,l4) %>%
  select(encounter_id = `Encounter Alias`  , 
         begin_hour = `Last Updated Date-Time`,
         result = `Numeric Value`,
         lab = `Result Name`) %>%
  mutate(encounter_id = as.numeric(encounter_id))
rm(l1,l2,l3,l4)
gc()

#write_csv(l %>% group_by(lab) %>% summarise(ns=n()),
#          "original_group_labs.csv")
# manually added the column 'true_labs' to tfile original_group_labs.csv
a <- read_csv("original_group_labs.csv")
a <- a %>% select(lab,true_labs) %>% filter(!is.na(true_labs))
l <- a %>% inner_join(l) %>% 
  select(-lab) %>% 
  rename(super_group = true_labs,
         numeric_result = result) %>%
  inner_join(encs %>% distinct(encounter_id)) %>%
  distinct_all() %>%
  mutate(begin_hour = floor_date(begin_hour,unit = "hour"))

# a <- l %>% group_by(super_group) %>%
#   summarise(ns = n()) %>%
#  ungroup() %>%
#   arrange(desc(ns)) %>%
#   mutate(group = row_number())
#   write_csv(a, "labs_keys.csv")
labs <- read_csv("labs_keys.csv") %>%
  filter(ns > 1000)

labs <- labs %>% select(super_group,groups=group)

l <- l %>% left_join(labs, by = "super_group")  %>% 
  select(encounter_id , begin_hour,groups , numeric_result)

##############################################
###############################################
# vital signs:
d1 <- read_csv("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-vitals-12_19-6_20.csv") %>%
  select(encounter_id = `Encounter Alias`,
         groups = `Result Name`,
         numeric_result = `Numeric Value`,
         begin_hour = `Service Date`)  %>% 
  inner_join(encs %>% distinct(encounter_id))
d <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-vitals-6_19-12_19).csv") %>%
  select(encounter_id = `Encounter Alias`,
         groups = `Result Name`,
         numeric_result = `Numeric Value`,
         begin_hour = `Service Date`)  %>% 
  inner_join(encs %>% distinct(encounter_id))
d2 <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-vitals-12_18-6_19.csv") %>%
  select(encounter_id = `Encounter Alias`,
         groups = `Result Name`,
         numeric_result = `Numeric Value`,
         begin_hour = `Service Date`)  %>% 
  inner_join(encs %>% distinct(encounter_id))
d3 <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-vitals-6_1-12_1_18.csv") %>%
  select(encounter_id = `Encounter Alias`,
         groups = `Result Name`,
         numeric_result = `Numeric Value`,
         begin_hour = `Service Date`)  %>% 
  inner_join(encs %>% distinct(encounter_id))
d4 <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-vitals-12_1_18-6_1_19.csv") %>%
  select(encounter_id = `Encounter Alias`,
         groups = `Result Name`,
         numeric_result = `Numeric Value`,
         begin_hour = `Service Date`)  %>% 
  inner_join(encs %>% distinct(encounter_id))
d5 <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-vitals-3_1-6_1_18.csv") %>%
  select(encounter_id = `Encounter Alias`,
         groups = `Result Name`,
         numeric_result = `Numeric Value`,
         begin_hour = `Service Date`)  %>% 
  inner_join(encs %>% distinct(encounter_id))
d6 <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-vitals-1_18-3_18.csv") %>%
  select(encounter_id = `Encounter Alias`,
         groups = `Result Name`,
         numeric_result = `Numeric Value`,
         begin_hour = `Service Date`)  %>% 
  inner_join(encs %>% distinct(encounter_id))

d <- rbind(d1,d2,d3,d4,d5,d6,d)
rm(d1,d2,d3,d4,d5,d6)
gc()

# write_csv(d %>%  group_by(groups) %>% summarise(ns = n()) %>% ungroup(),
#            "original_vital_groups.csv" )
###########################
# GROUPD OF VITALS
#respiratori rate: 2
#pulso rate: 3
#systolic blood pressure: 1
# iastolic blood preasrue: 4
#COMA SCORE: 5
#  temperature: 6

#  I manualy added the colmn 'numeric_group' to the file "original_vital_groups.csv"
vitals <- read_csv("original_vital_groups.csv")
vitals <- vitals %>% select(groups , numeric_group) %>%
  filter(!is.na(numeric_group))

d <- d  %>% filter(!is.na(numeric_result)) %>%
  inner_join(vitals) %>%
  select(encounter_id,numeric_result,begin_hour,groups = numeric_group) %>% 
  distinct_all()  %>%
  mutate(begin_hour = floor_date(begin_hour,unit = "hour"))

d %>% distinct(encounter_id) %>% nrow()
# 9,090

d <- d 

######### Checking the supergroups:
meds 

labs 

vitals %>% distinct(numeric_group , .keep_all = T)

##################################################
## Replication of the code:
# /Users/jlaubach/Dropbox/Documents Mac Work/Creating Data for Neural Networks All/2.R
#############################################################
enc_m <- m %>% distinct(encounter_id) %>% mutate(meds = T)
enc_v <- d %>% distinct(encounter_id) %>% mutate(vitals = T)
enc_l <- l %>% distinct(encounter_id) %>% mutate(labs = T)

enc <- enc_m %>%
  full_join(enc_l , by = c("encounter_id")) %>%
  full_join(enc_v , by = c("encounter_id"))

rm(enc_m,enc_l,enc_v)
gc()

enc$meds[is.na(enc$meds)] <- F
enc$labs[is.na(enc$labs)] <- F
enc$vitals[is.na(enc$vitals)] <- F

a <- enc %>% 
  group_by(meds,labs,vitals) %>%
  summarise(n_enc = n())   %>%
  ungroup() %>%
  mutate(meds = if_else(meds,"yes","no"),
         labs = if_else(labs,"yes","no"),
         vitals = if_else(vitals,"yes","no")) 

a <- a %>%
  rbind( tibble(meds = "no" , labs = "no" ,
                vitals = "no" ,
                n_enc = enc %>% nrow() - sum(a$n_enc) ))

a

# meds  labs  vitals n_enc
#no    no    yes     1158
# no    yes   no       928
# no    yes   yes     2455
# yes   no    no       392
# yes   no    yes     1502
# yes   yes   no      2086
# yes   yes   yes     3975
# no    no    no         0

write_csv(a,"available_encounters_and_their_records_CN.csv")

#######################################
## Afte talking with the doctors, we decided we wil use only those patients that have
## those that have meds, vitals, labs and that are present in the file that identify admissions
## and discharges:

a  <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-ICU_DEMOGRAPHICS-4-23.csv")
a <- a %>% select(encounter_id = `Encounter Alias`,
                  picu_ad = `PICU Admit Date-Time`,
                  picu_disc = `PICU Discharge Date-Time`)
a <- a %>% distinct_all() %>%
  filter(!is.na(picu_ad))



encs  <- encs %>% inner_join(enc) %>% 
  #  filter( vitals # & meds & labs
  #        ) %>%
  inner_join(a %>% distinct(encounter_id)) 
# x patients
rm(enc,enc_l,enc_v,enc_m)

d <- d %>% #inner_join(enc) %>% 
  filter(!is.na(begin_hour) & !is.na(numeric_result)) %>%
  select(encounter_id, begin_hour , events = groups , valor = numeric_result)

l <- l %>% #inner_join(enc) %>% 
  filter(!is.na(begin_hour) & !is.na(numeric_result)) %>%
  select(encounter_id, begin_hour , labs = groups , valor = numeric_result )

m <- m %>% #inner_join(enc) %>% 
  filter(!is.na(begin_hour)) %>%
  select(encounter_id, begin_hour , meds = groups , ns = n_distinct_generic_names_this_period)

rm(enc)
gc()



##########################################################
#### Handling the date time of the vital sign dataset
######################################################
d <- d %>% 
  mutate(begin_hour2 = as.POSIXct(begin_hour ,  format = "%Y/%m/%d %H:%M:%OS"),
         begin_hour2 = floor_date(begin_hour2,unit = "hour") )
###
d1 <- d %>% filter(!is.na(begin_hour2)) %>%  ## READY
  select(encounter_id,begin_hour=begin_hour2,events,valor)  
d <- d %>% filter(is.na(begin_hour2)) %>% ## NOT READY
  select(encounter_id,begin_hour,events,valor)

###############
d <- d %>%
  mutate(count1  =  str_count(begin_hour,":"),  #  numbber  of ":" in the date-time
         count2 = str_length(begin_hour))
d %>% group_by(count1 , count2) %>% summarise(ns = n())
# count1 count2      ns
# 1     11  325622      caso 5
# 1     12 1225704      caso 4
# 1     13 1085066       caso 3
# 1     14   58109       caso 2
# 2     19    4196      caso 1

#Caso 1
# the hours here are impossible to have dues to the daylight svaing time occuring on
# march 8 2020, march 2019, march 14th 2021 between 2 - 3 am.
#  I will add one hour to all these cases:
z1 <- d %>% filter(count1 > 1) %>%   
  distinct(encounter_id,begin_hour,events,valor)  %>%
  mutate(hora = as.numeric(str_sub(begin_hour ,  start = 12 , end = 13)),
         begin_hour = as.POSIXct( str_sub(begin_hour,end = 10) , format = "%Y/%m/%d" ) + hours(hora+1))  %>%
  select(-hora)   #  READY

#Caso 2, 3, 4 , 5
z2 <- d %>%   filter(count1 < 2  )  %>%
  distinct(encounter_id,begin_hour,events,valor)    %>% 
  mutate(hora =  str_sub(word(begin_hour, 2) , start = 1,end = 2) ,
         hora = as.numeric(str_remove(hora,":")),
         begin_hour = word(begin_hour, 1),
         begin_hour = as.POSIXct( begin_hour, format = "%m/%d/%Y" ),
         anio  = year(begin_hour),
         begin_hour2 = begin_hour + hours(hora) + years(if_else(anio < 22 , 2000,1900) )  )  

### ready z2
d <- z2 %>% filter(!is.na(begin_hour2))  %>% # READY
  select(encounter_id , begin_hour = begin_hour2 , events,valor)

##  from z2, the hours that are impossible to have due to the daylight svaing time occuring on
# march 8 2020, march 2019, march 14th 2021 between 2 - 3 am.
z2 <- z2 %>% filter(is.na(begin_hour2)) %>%
  mutate(begin_hour2 = begin_hour + hours(hora + 1) + years(if_else(anio < 22 , 2000,1900)) )  %>% # READY
  select(encounter_id , begin_hour = begin_hour2 , events,valor)

dim(d)
dim(z1)
dim(z2)
dim(d1)
d <- rbind(d,d1,z1,z2)
rm(d1,z1,z2)
gc()
##########################################################
#### Done handling the date time of the vital sign dataset
######################################################
# l <- l %>% mutate(begin_hour = as.POSIXct(begin_hour))
# d <- d %>% mutate(begin_hour = as.POSIXct(begin_hour))
l <- l %>% mutate(valor =  as.numeric(valor)) 
save.image("ambiente.RData")

################################################################
load("ambiente.RData") 


encs_hours <- d %>% select(encounter_id, begin_hour) %>%
  rbind(m %>% select(encounter_id, begin_hour)) %>%
  rbind(l %>% select(encounter_id, begin_hour)) %>%
  distinct(encounter_id,begin_hour)

## Making space in the data sets: (HERE I CAN RUN  OUT OF RAM MEMORY)
d <- d %>% full_join(encs_hours , by = c("encounter_id", "begin_hour"))
m <- m %>% full_join(encs_hours, by = c("encounter_id", "begin_hour"))
l <- l %>% full_join(encs_hours, by = c("encounter_id", "begin_hour"))

a <- is.na(m$meds)
m$meds[a] <- 0 ## provisional creation of a group
l$labs[is.na(l$labs)] <- 0 ## callad 0
d$events[is.na(d$events)] <- 0

m$ns[a] <- 0
# l$valor[is.na(l$labs)] <- 0
# d$valor[is.na(d$events)] <- 0
rm(a)

## First and last hours
times <- encs_hours %>% 
  group_by(encounter_id) %>% 
  summarise(first_hour = min(begin_hour),
            last_hour = max(begin_hour)) %>%
  ungroup()

save(encs_hours , file = "encs_hours_prov.RData")
save(times,file = "First_hour_info_ICU_patients.RData") ## This file will be used in programs months in the future

#load("encs_hours_prov.RData")
#load("First_hour_info_ICU_patients.RData")

## Slicing the data
encs_hours <- encs_hours %>%
  inner_join(times %>% select(encounter_id , first_hour) , by = "encounter_id") %>%
  mutate(n_hours = as.numeric(difftime(time1=begin_hour ,  ## number of hours that hava passed since the very beginning
                                       time2=first_hour,  ## this will be used and erased
                                       units = "hours") )+1,
         s1 = floor(n_hours)+1,
         s2 = floor(n_hours/2)+1,
         s3 = floor(n_hours/3)+1,
         s4 = floor(n_hours/4)+1,
         s5 = floor(n_hours/5)+1,
         s6 = floor(n_hours/6)+1) %>%
  select(encounter_id , begin_hour , 
         s1,s2,s3,s4,s5,s6)

d <- encs_hours %>% left_join(d , by = c("encounter_id" , "begin_hour")) %>% ungroup()
m <- encs_hours %>% left_join(m , by = c("encounter_id" , "begin_hour")) %>% ungroup()
l <- encs_hours %>% left_join(l , by = c("encounter_id" , "begin_hour"))  %>% ungroup()

save(d,m,l,file="dml.RData")
rm(d,m,l)
gc()

#load("dml.RData")

### In order to identify the transition slices (later on), I need to have 
# the first hour and the last hour of each slice, so
## in the code bbelow I will add all the hours of each observed slice

expand_keep <- function(times , encs_hours , p){
  ## funtion to expand and keep the sliced hours
  # encs_hours must contain unique rows of encounter_id, sliced_hour
  a <- times  %>% 
    mutate(tos = last_hour + hours(p),
           tos = if_else(is.na(tos) , last_hour + hours(p-1) , tos )) %>%   ## seasonal change of hour (once a year) will create and NA hour 2017-03-12 02:00:00 does not exists
    select(-last_hour) %>%
    group_by(encounter_id) %>%
    expand(begin_hour = seq(from = first_hour,
                            to=tos,
                            by=p*3600)) %>%
    inner_join(times %>% select(encounter_id , first_hour) , 
               by = "encounter_id") %>%
    mutate(n_hours = as.numeric(difftime(time1=begin_hour ,  ## number of hours that hava passed since the very beginning
                                         time2=first_hour,  ## this will be used and erased
                                         units = "hours") )+1,
           sliced_hour = floor(n_hours/p)+1) %>%
    inner_join( encs_hours, by = c("encounter_id", "sliced_hour"))  %>% ## keeping the original slices
    select(encounter_id , b1=begin_hour , sliced_hour) %>%
    mutate(b2 = b1 + hours(p-1))  %>% # ??
    pivot_longer(c(b1,b2) , 
                 names_to = "bs",
                 values_to = "begin_hour")  %>%
    distinct(encounter_id , begin_hour , sliced_hour)
  
  ##keeping the very first and very last hour of each slice
  b <- a %>%
    group_by(encounter_id, sliced_hour) %>%
    summarise(h1 = min(begin_hour),
              h2 = max(begin_hour)) %>%
    ungroup()
  
  a <- a %>% 
    inner_join(b ,  by = c("encounter_id", "sliced_hour")) %>%
    filter(begin_hour == h1 | begin_hour == h2) %>%
    select(encounter_id , begin_hour , sliced_hour)
  
  return(a)
} 


encs_hours <- encs_hours %>% select(-begin_hour) %>%
  pivot_longer(c(s1,s2,s3,s4,s5,s6),
               names_to = "length_slice",
               values_to = "sliced_hour")   %>%
  mutate(length_slice = as.integer(str_remove(length_slice,"s"))  ) %>%
  distinct(encounter_id,length_slice,sliced_hour)

cl <- makeCluster(2) 
registerDoSNOW(cl)
casos <- c(1,6,2,5,4,3)
pb <- txtProgressBar(max = length(casos), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

horas <- foreach(caso = casos,
                 .combine = rbind,
                 .inorder = F,
                 .errorhandling = "remove",
                 .packages = c("tidyverse","lubridate"),
                 .options.snow = opts) %dopar%{
                   
                   return(
                     expand_keep(times = times,
                                 encs_hours = encs_hours %>% 
                                   filter(length_slice == caso) %>%
                                   select(encounter_id,sliced_hour),
                                 p = caso) %>%
                       mutate( p = caso) ## the  lengtj of the slice
                   )
                 }
close(pb)
stopCluster(cl) 

save(horas, file = "horas.RData")
rm(horas)
### contains each slices and its repective very first hour and very last hour of the slice
# for the cases when the length os sliced_hour is 1, 2, 3, .... , or 6 hours
################
##

# JUNE 18 2021 I AM HERE!!!!!!!!!!!!!1

load("ambiente.RData") 
rm(d,l,m,meds,vitals,labs) # keeping encs only


load("horas.RData")
#  HERE FEB 1  2021 IS OK
### THE PROBLEM IS THE IDENTIFICATION OF THE ICU THAT MUST BE
# READY FOR THE STEP NEXT:

# 
# a  <- horas %>%
#   group_by(encounter_id,begin_hour,p) %>%
#   summarise(ns = n_distinct(sliced_hour),
#             ns2 = n(),
#             rangos = max(sliced_hour) - min(sliced_hour)) %>%
#   ungroup() %>%
#    filter(ns > 1 | ns2 > 1)
#  summary(a$rangos)
#  ## 1

## I have  decided to use only length of slices 2 , 3 ,  6
horas <- horas %>% 
  mutate(p = as.integer(p)) %>%
  filter( p == 2 | p == 3 | p == 6  )
########################

horas <- horas %>%
  group_by(encounter_id,begin_hour,p) %>%
  summarise(sliced_hour = max(sliced_hour)) %>%
  ungroup() 


################################################################


## computing the column in_icu:
add_icu_info <- function(d,dicu){
  dcopy <- d ### Saving a copy of the data set
  
  dicu <- d %>% distinct(encounter_id) %>%  
    inner_join(dicu) %>%
    arrange(encounter_id , desc(in_icu)) %>%
    distinct(encounter_id , begin_hour , .keep_all = T)
  
  first_hour <- dicu %>% filter(in_icu==1) %>%
    group_by(encounter_id) %>% summarise(icu_first_entrance = min(begin_hour))
  
  d <- d %>% filter(!is.na(begin_hour))  %>%
    distinct(encounter_id,begin_hour) %>% 
    full_join(dicu , by = c("encounter_id","begin_hour")) %>%
    arrange(encounter_id , begin_hour) %>%
    group_by(encounter_id) %>%
    mutate(line_number = row_number()) %>% 
    ungroup() %>% left_join(first_hour,by="encounter_id") %>%
    mutate(icu_cum = case_when(line_number == 1 & is.na(in_icu) ~ as.numeric(0),
                               TRUE ~  as.numeric(icu_cum)),
           hours_in_icu = case_when(line_number == 1 & is.na(in_icu) ~ as.numeric(0),
                                    TRUE ~ as.numeric(hours_in_icu)),
           hours_before_icu =  case_when(line_number == 1 & !is.na(icu_first_entrance) ~ 
                                           as.numeric(difftime(time1 = icu_first_entrance,
                                                               time2 = begin_hour,
                                                               units = "hours")),
                                         line_number == 1 & is.na(icu_first_entrance) ~
                                           as.numeric(-2),
                                         TRUE  ~ as.numeric(hours_before_icu)),
           in_icu = case_when( line_number == 1 & is.na(in_icu) ~ as.numeric(0), 
                               TRUE ~ as.numeric(in_icu) ) ### adding 0 to the very first line if it is empty
    ) %>% select(-icu_first_entrance,-line_number)  %>% 
    mutate(line_number = row_number(),
           no_nas = !is.na(in_icu) * 1, ## 0 when values are needed to be inputed
           cum_no_nas = cumsum(no_nas)) %>%
    select(-no_nas) %>%
    group_by(encounter_id , cum_no_nas) %>%
    mutate(line_value = min(line_number)) %>%
    ungroup() %>% select(-cum_no_nas,-line_number) %>%
    mutate(in_icu = in_icu[line_value],   ## Here we are adding the in_icu imfoprmation
           icu_cum = icu_cum[line_value],
           hours_in_icu = if_else(in_icu == 0,0,as.numeric(difftime(begin_hour,begin_hour[line_value],units = "hours")) + hours_in_icu[line_value]),
           hours_before_icu = if_else(in_icu == 0,hours_before_icu[line_value]-as.numeric(difftime(begin_hour,begin_hour[line_value],units = "hours")),0),
           hours_before_icu = if_else(hours_before_icu < 0 , -1 , hours_before_icu)) %>%
    select(-line_value)
  
  return( d %>%  right_join(dcopy , by = c("encounter_id","begin_hour")) )
}


dicu <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/dummy_vars_0_icu_0_floor_july_1_2021.csv")

#dicu  <- dicu %>% rename(encounter_id = fin_id , begin_hour = dt) 

dicu <- dicu %>% inner_join(encs %>% select(encounter_id))

#dicu$begin_hour <- as.POSIXct(dicu$begin_hour)


dicu %>% distinct(encounter_id) %>% nrow()

a <- dicu %>% distinct(in_icu, encounter_id) %>%
  mutate(into = 1) %>%
  pivot_wider(names_from = in_icu , values_from = into,
              values_fill = list(into = NA))

a %>% group_by(`0`,`1`) %>% summarise(ns = n())
# 18168


cl <- makeCluster(3) 
registerDoSNOW(cl)
casos <- c(6,2,3)
pb <- txtProgressBar(max = length(casos), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

icu_info <- foreach(caso = casos,
                    .combine = rbind,
                    .inorder = F,
                    #.errorhandling = "remove",
                    .packages = c("tidyverse","lubridate"),
                    .options.snow = opts) %dopar%{
                      
                      icu_info <- add_icu_info(d = horas %>% 
                                                 filter(p == caso) %>%
                                                 select(encounter_id , begin_hour,sliced_hour),
                                               dicu = dicu)  %>% 
                        group_by(encounter_id,sliced_hour) %>% 
                        arrange(begin_hour) %>%
                        summarise(
                          primera = first(in_icu),
                          ultima = last(in_icu),
                          in_icu = if_else(#primera == ultima ,   ## this techinque is correct becasue a patient can not enter
                                           mean(in_icu) == primera,
                                           primera, 0.5), ## and exti the icu or floor in less than 9 hours: 10 hours ICU limit and 28 hours Floor limit
                          floor_to_icu_transition = (primera == 0 & ultima == 1) ,
                          icu_to_floor_transition = (primera == 1 & ultima == 0) ,
                          icu_cum = first(icu_cum)    ) %>%
                        ungroup() %>%
                        select(encounter_id,
                               sliced_hour,
                               in_icu,icu_cum ,
                               floor_to_icu_transition ,
                               icu_to_floor_transition  ) %>%
                        mutate(length_slice = caso)
                      
                      return(icu_info)
                    }
close(pb)
stopCluster(cl) 

save(icu_info , file = "icu_info.RData")

rm(dicu, add_icu_info,encs_hours,encs_hours2,times,aux)

