library(tidyverse)
library(doSNOW)

load("meds_with_joined_multum.RData")

## Route Desctiptions
# write_csv(d %>% distinct(RX_ROUTE),
#           "Routes.csv")
# m <- read_csv("~/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/Medication dec 19 from HF 2018/med classified table PICU 2018.csv")
# 
# unique(m$ranked_route_description)
# write_csv(tibble(route_description = unique(m$ranked_route_description)),
#           "needed route descriptions.csv")

d <- d %>% 
  select(encounter_id,
         med_started_dt_tm = start_dt_tm,
         med_stopped_dt_tm = stop_dt_tm,
         route_description = route,
         generic_name = d2,
         first_tier_category_name = first_tier_1,
         second_tier_category_name = second_tier_1,
         third_tier_category_name = third_tier_1,
         first_tier_category_name_2 = first_tier_2,
         second_tier_category_name_2 = second_tier_2,
         third_tier_category_name_2 = third_tier_2,
         classified)

 sum(is.na(d$med_started_dt_tm)) # 0 
 sum(is.na(d$med_stopped_dt_tm)) # 0 

## FROM ANITA:
#Ok so I looked through – the only medications that I question are norepinephrine 
# and vasopressin. The rest of the medications it makes sense. I think we can just
# apply our one hour rule for these medications in the absence of a stop time – ie.
# if they got the medication in an hour and there is no end time, we consider the patient
# to have received the medication for one hour.

#Especially if there are just a few cases of this for those 2 medications I
#mentioned (and I only see once example for each of those medications).
#Does this make sense?
# d <- d %>%
#   mutate(med_stopped_dt_tm = if_else(is.na(med_stopped_dt_tm),
#                                       med_stopped_dt_tm2,
#                                       med_stopped_dt_tm)) %>%
#   select(-med_stopped_dt_tm2)
#
# d %>% filter(generic_name %in% c("norepinephrine" , "vasopressin") &
#                    is.na(med_stopped_dt_tm) )

#################
d <- d %>% ##  909 rows, but the differences were for one second
  mutate(start_aux = med_started_dt_tm ,
         med_started_dt_tm = if_else(med_started_dt_tm < med_stopped_dt_tm, ## if med_stop>med_Start,
                                     med_started_dt_tm,med_stopped_dt_tm), ## exchangethem...
         med_stopped_dt_tm = if_else(start_aux < med_stopped_dt_tm,
                                     med_stopped_dt_tm , med_started_dt_tm)) %>%
  #filter(med_started_dt_tm<=med_stopped_dt_tm) # or keep only the records that have the correct order
  select(-start_aux) %>%
   distinct(encounter_id, med_started_dt_tm, med_stopped_dt_tm, route_description,generic_name, .keep_all = T) %>%
  mutate(number_row = row_number())

#########################################################
###Defining partitions to parallelize
parts <- 21
encs <- sort(unique(d$encounter_id))
k <- round(seq(1,length(encs),length.out = parts))

cl <- makeCluster(7) # PARALELIZING. DO NOT DO MORE THAN 2 CORES, NOT ENOUGHT MEMORY.
registerDoSNOW(cl)
pb <- txtProgressBar(max = length(k)-1 , style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
relations <- foreach(  j = 1:(length(k)-1),
                  .combine = rbind,
                  .options.snow = opts,
                  .errorhandling = "remove",
                  .packages = c( "tidyverse","igraph"),
                  .inorder = F) %dopar% {
                    
                    b <-  d %>% filter(encounter_id %in% encs[ k[j]:k[j+1] ] )
                    
                    ij <- apply(matrix(1:nrow(b)) , 1 , function(x)
                      c((b %>% filter( encounter_id ==  b$encounter_id[x] &
                                         generic_name == b$generic_name[x] &
                                         ((med_started_dt_tm <= b$med_started_dt_tm[x] & b$med_started_dt_tm[x] <= med_stopped_dt_tm ) |
                                            (med_started_dt_tm <= b$med_stopped_dt_tm[x] & b$med_stopped_dt_tm[x] <= med_stopped_dt_tm )
                                         )))$number_row , b$number_row[x] )   )
                    ij <- sapply(ij,unique)
                    
                    l <- sapply(ij , length)
                    
                    g <- graph_from_data_frame( cbind( rep(b$number_row ,  l) , unlist(ij) ), directed=F)
                    g <- clusters(g)$membership
                    
                    return(tibble(number_row = as.numeric(names(g)),
                                  cluster_group = g) )
                  }
close(pb)
stopCluster(cl) 

save(relations , file = "relations.RData")

##################
load("relations.RData")

relations <- relations %>% distinct(number_row , .keep_all = T)

d <- d %>% left_join(relations) %>% select(-number_row) %>%
  arrange(cluster_group)

#save(d,file = "step1_meds_icu_dec_13_2019.RData")
#save(d,file = "step1_meds_icu_dec_4_2020.RData")

########################################
## Command 9

d <- d %>% filter(!is.na(med_started_dt_tm) & 
                    !is.na(med_stopped_dt_tm)) %>%  ## this line is a Murray instruction
  mutate(dif_times = as.numeric(difftime(med_stopped_dt_tm,med_started_dt_tm,units="secs")) ) %>%
  group_by(encounter_id,cluster_group) %>%
  summarise( #encounter_id = first(encounter_id),
    # begin_hour = min(begin_hour),
    route_description = route_description[which.max(dif_times)],
    med_started_dt_tm = min(med_started_dt_tm),
    med_stopped_dt_tm = max(med_stopped_dt_tm),
    generic_name = first(generic_name),
    first_tier_category_name = first_tier_category_name[which.max(dif_times)],
    second_tier_category_name = second_tier_category_name[which.max(dif_times)],
    third_tier_category_name = third_tier_category_name[which.max(dif_times)],
    first_tier_category_name_2 = first_tier_category_name_2[which.max(dif_times)],
    second_tier_category_name_2 = second_tier_category_name_2[which.max(dif_times)],
    third_tier_category_name_2 = third_tier_category_name_2[which.max(dif_times)]  ) %>% 
  ungroup()

#save(d,file="prov_env_dec_13_2019.Rdata")
save(d,file="prov_env_june_15_2021.Rdata")

####################################################
########################################
## After command 10. Or the same: after line 253 of
## /Users/jlaubach/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/Medication/Meds Inpatients No PICU/NEW ITERATION MARCH 1 2019/Repeated Drugs by Generic Name.R
load("prov_env_june_15_2021.Rdata")
# 428,517 rows
d <- d %>% filter(!is.na(cluster_group))

# 428,517 rows

### translation of route descriptions
x <- readxl::read_excel("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/Medications/Copy of Routes-AKp.xlsx",sheet = 1)
x <- x %>% mutate(route_description = tolower(route_description),
                  route_description = trimws(route_description) )   %>%
  distinct(route_description , .keep_all = T)

d <- d %>% mutate(route_description = tolower( route_description ),
                  route_description = trimws(route_description)   )

d %>% distinct(route_description) %>%  View()
d  <- d %>% left_join(x,by = "route_description") %>%
  mutate(translation = case_when(str_detect(route_description,"nostril") ~ "nasal",
                                 str_detect(route_description,"intravariceal|intraosseous|intradialysis|rbul") ~ "OIVSQIM",
                                 route_description == "rbul"  | route_description == "if" | route_description == "ic" ~ "OIVSQIM",
                                 T ~ translation))

#  d <- d %>% select(-translation)
d %>% filter(is.na(translation)) %>%
  group_by(route_description) %>%
  summarise(ns = n()) %>%
  View()

d <- d %>% select(-route_description)  %>%
  rename(route_description = translation)

d %>%  group_by(route_description) %>%
  summarise(ns = n() , n_encs = n_distinct(encounter_id)) %>%
View()
#####

###
# Loading the classification:
s <- readxl::read_excel("/Users/jlaubach/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/Medication/Rpeated Rows Study/Medication priority.xlsx",sheet = 1)
s <- s %>% select(orden  , "AP-MP Ranking" ,route_description)
names(s)[1:2] <- c("rank" , "ap_mp")

s <- s %>% mutate(ap_mp = gsub("(^\\s+)|(\\s+$)", "",tolower(ap_mp)) ,
                  route_description = gsub("(^\\s+)|(\\s+$)", "",tolower(route_description))
)
s <- s %>% mutate(route_description = case_when(
  route_description == "intravenous or injectable" ~ "intravenous",
  route_description == "by mouth" ~ "oral",
  TRUE ~ route_description))
## Correcting spelling from Anita table classification:
s <- s %>% mutate( ap_mp = case_when(
  ap_mp == "ace-inhibitor with calcium channel blocking agents" ~ "ace inhibitors with calcium channel blocking agents",
  ap_mp == "ace-inhibitor with thiazides"  ~ "ace inhibitors with thiazides" ,
  ap_mp == "angiotensin converting enzyme (ace) inhibitor"  ~ "angiotensin converting enzyme (ace) inhibitors" ,
  ap_mp == "angiotensin ii inhibitor"  ~ "angiotensin ii inhibitors" ,
  ap_mp == "angiotensin ii inhibitor with thiazide,"  ~ "angiotensin ii inhibitors with thiazides" ,
  ap_mp == "angiotensin ii inhibitors with calcium channel blockers"  ~ "angiotensin ii inhibitors with calcium channel blockers" ,
  ap_mp == "angiotensin receptor blockers and neprilysin inhibitors"  ~ "angiotensin receptor blockers and neprilysin inhibitors" ,
  ap_mp == "anti-arrhythmic agents"  ~ "antiarrhythmic agents" ,
  ap_mp == "anti-emetic/anti-vertigo medications"  ~ "antiemetic/antivertigo agents" , # not sure
  ap_mp == "anti-neoplastic"  ~ "antineoplastics" ,
  ap_mp == "anti-platelet agents"  ~ "antiplatelet agents" ,
  ap_mp == "anticholinergic/antispasmodic"  ~ "anticholinergics/antispasmodics" ,
  ap_mp == "anticholinergic/chronotropic"  ~ "anticholinergic chronotropic agents" ,
  ap_mp == "anticoagulant"  ~ "anticoagulants" ,
  ap_mp == "anticonvulsant"  ~ "anticonvulsants" ,
  ap_mp == "antidepressant"  ~ "antidepressants" ,
  ap_mp == "antihistamine"  ~ "antihistamines" ,
  ap_mp == "antihypertensives"  ~ "antihypertensive combinations" ,
  ap_mp == "anxiolytics, sedatives, hypnotics"  ~ "anxiolytics, sedatives, and hypnotics" ,
  ap_mp == "beta blockers with thiazides"  ~ "beta blockers with thiazides" ,
  ap_mp == "coagulant modifiers"  ~ "coagulation modifiers" ,
  ap_mp == "hormone/hormone modifiers"  ~ "hormones/hormone modifiers" ,
  ap_mp == "neuromuscular blockade agents"  ~ "neuromuscular blocking agents" ,
  ap_mp == "pulmonary hypertension drugs"  ~ "agents for pulmonary hypertension" ,
  ap_mp == "respiratory inhalant products – inhaled"  ~ "respiratory inhalant products" ,
  ap_mp == "urinary antispasmodic"  ~ "urinary antispasmodics" ,
  ap_mp == "urinary antispasmodic - transdermal"  ~ "urinary antispasmodics" ,
  ap_mp == "vasopressor"  ~ "vasopressors",
  TRUE ~ap_mp
))

d <- d %>% mutate(first_tier_category_name = gsub("(^\\s+)|(\\s+$)", "",tolower(first_tier_category_name)) ,
                  second_tier_category_name = gsub("(^\\s+)|(\\s+$)", "",tolower(second_tier_category_name)) ,
                  third_tier_category_name = gsub("(^\\s+)|(\\s+$)", "",tolower(third_tier_category_name)) ,
                  first_tier_category_name_2 = gsub("(^\\s+)|(\\s+$)", "",tolower(first_tier_category_name_2)) ,
                  second_tier_category_name_2 = gsub("(^\\s+)|(\\s+$)", "",tolower(second_tier_category_name_2)) ,
                  third_tier_category_name_2 = gsub("(^\\s+)|(\\s+$)", "",tolower(third_tier_category_name_2)),
                  route_description = gsub("(^\\s+)|(\\s+$)", "",tolower(route_description)),
                  route_description = case_when(  route_description =="injectable" ~ "intravenous", 
                                                  route_description == "by mouth" ~ "oral", 
                                                  TRUE ~ route_description),
                  rown = row_number())
##### checking  the categories
ij <- d %>% select( first_tier_category_name, second_tier_category_name , third_tier_category_name,
                   first_tier_category_name_2 , second_tier_category_name_2,  third_tier_category_name_2  ) %>% 
  pivot_longer(c(first_tier_category_name, second_tier_category_name , third_tier_category_name,
                 first_tier_category_name_2 , second_tier_category_name_2,  third_tier_category_name_2) ,
               names_to = "category",
               values_to = "ap_mp" )   %>%
distinct_all()
write_csv(ij,"tier_names_in_new_data.csv")


### This is an auxiliary table to help in the classificy the drugs
ij <- d %>% select(rown ,route_description, first_tier_category_name, second_tier_category_name , third_tier_category_name,
                   first_tier_category_name_2 , second_tier_category_name_2,  third_tier_category_name_2  ) %>% 
  pivot_longer(c(-rown,-route_description),
               names_to = "category",
               values_to = "ap_mp" ) %>%
  select(rown , ap_mp , route_description)  %>% 
  mutate(ap_mp_route = paste(ap_mp , route_description,sep=" ")) %>% select(rown ,ap_mp_route )

s <- s %>% mutate(ap_mp_route = paste(ap_mp , route_description,sep=" "))

ij <- ij %>% left_join(s ,by = "ap_mp_route")

ij <- ij  %>% filter(!is.na(ap_mp)) 

ij <- ij %>% 
  arrange(rown , rank) %>%
  group_by(rown) %>%
  summarise(ap_mp = first(ap_mp),
            route_description = first(route_description)) %>%
  ungroup()

names(ij)[2:3] <- c("ranked_tier_name" , "ranked_route_description")

d <- d %>% left_join(ij) %>% select(-rown) %>% 
  arrange(encounter_id , med_started_dt_tm)

## until  here, some  of the drugs have been clissifed
# d has 543,592 rows

#save(d,ij,s,file="prov_env_dec_31_2020.Rdata")
save(d,ij,s,file="prov_env_june_16_2021.RData")

d  %>% filter(is.na(ranked_tier_name))  %>%  nrow()
d  %>% filter(!is.na(ranked_tier_name))  %>%  nrow()
# THE REST OF THE CODE IS LEFT HERE FOR HISTORICAL POURPOUSES,
## IT  IS NOT NEEDED FOR THIS DATA SET
