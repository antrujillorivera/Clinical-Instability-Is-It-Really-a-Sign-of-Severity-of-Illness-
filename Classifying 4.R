
library(tidyverse)

load("prov_env_june_16_2021_2.RData")

# Anita requests:
d$ranked_tier_name[d$first_tier_category_name %in% 
                     c("narcotic analgesic combinations",
                       "narcotic analgesics")] <- "narcotic analgesics"

d$ranked_tier_name[d$first_tier_category_name_2 %in% 
                     c("narcotic analgesic combinations",
                       "narcotic analgesics")] <- "narcotic analgesics"

d_class <- d %>% filter(!is.na(ranked_tier_name))  # 270,609 rows
d <- d %>% filter(is.na(ranked_tier_name)) %>% 
  select(-ranked_tier_name , -ranked_route_description)
## d has 157,908  rows

# Doing the classification again  using new table. Loading the classification:
s <- readxl::read_excel("/Users/jlaubach/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/Medication/Rpeated Rows Study/AP MP Medication priority 3-1-18.xlsx",sheet = 1)  
s <- s %>%
  select( rank = rango , ap_mp="AP-MP Ranking" ,route_description) %>% filter(!is.na(route_description) & !ap_mp %in% c("Nitric Oxide" , "vasopressin" ) ) %>%
  distinct(ap_mp, .keep_all = T) %>% arrange(rank) %>%
  select(-route_description) %>% 
  mutate(rank = row_number(),
         ap_mp = gsub("(^\\s+)|(\\s+$)", "",tolower(ap_mp)))

d <- d %>% mutate(first_tier_category_name = gsub("(^\\s+)|(\\s+$)", "",tolower(first_tier_category_name)) ,
                  second_tier_category_name = gsub("(^\\s+)|(\\s+$)", "",tolower(second_tier_category_name)) ,
                  third_tier_category_name = gsub("(^\\s+)|(\\s+$)", "",tolower(third_tier_category_name)) ,
                  first_tier_category_name_2 = gsub("(^\\s+)|(\\s+$)", "",tolower(first_tier_category_name_2)) ,
                  second_tier_category_name_2 = gsub("(^\\s+)|(\\s+$)", "",tolower(second_tier_category_name_2)) ,
                  third_tier_category_name_2 = gsub("(^\\s+)|(\\s+$)", "",tolower(third_tier_category_name_2)) ,
                  route_description = gsub("(^\\s+)|(\\s+$)", "",tolower(route_description))
) %>% mutate(rown = row_number())


### This is an auxiliary table to help in the classificy the drugs
ij <- d %>% select(rown ,first_tier_category_name, second_tier_category_name , third_tier_category_name,
                   first_tier_category_name_2 , second_tier_category_name_2, third_tier_category_name_2 ) %>% 
  gather(category , ap_mp , -rown) %>%
  select(rown , ap_mp) 

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


ij <- ij %>% left_join(s , by = "ap_mp" ) %>% filter(!is.na(ap_mp)) %>% 
  arrange(rown , rank) %>%
  group_by(rown) %>%
  summarise(ap_mp = first(ap_mp)) %>%
  ungroup() %>% rename(ranked_tier_name = ap_mp)

d <- d %>% left_join(ij) %>% select(-rown) %>%
  mutate(ranked_route_description = "OIVSQIM" )
d$ranked_route_description[is.na(d$ranked_tier_name)] <- NA
rm(ij)

#####################
# HERE CHECKING
dim(d)  # 157,908
d %>% filter(is.na(ranked_tier_name)) %>% nrow() ##  37,831

d <- rbind(d[,names(d_class)],d_class) # 428,517 rows
rm(d_class)

### 
# d_class <- d %>% filter(!is.na(ranked_tier_name))
# d <- d %>% filter(is.na(ranked_tier_name)) %>%
#   select(-ranked_tier_name , -ranked_route_description)
# 
# sum(is.na(d$generic_name)) # 0
# sum(is.na(d$route_description)) # 6,619
# route description are missing, itt comes from the original file
# # 
# # #saving the classifications
# write_csv(d_class %>% distinct(generic_name,ranked_tier_name,ranked_route_description),
#           "CN meds classified June 16 2021.csv")

###### OM JANUARY 14TH 2021, I used Anita and Jim C classification of the medications with no
# druug name. The resulting tabble does not have ranked_route_description.
## the generated file is read in the next line and used with d_class to creaste the final file.

# d_class <- d %>% filter(!is.na(ranked_tier_name)) %>%
#   select(encounter_id , med_started_dt_tm,med_stopped_dt_tm,generic_name,ranked_tier_name)
# 
# load("difficul_classified_jan_14_2021.RData") # d
# d <- rbind(d,d_class)
# rm(d_class)
####################
### Fixing the non classifications by using Anitas reponse
# the exel file being read below was created by Anita so we can assign a ranked_tier_name
## to the medications that still there are not classifications
f <- readxl::read_excel("/Users/jlaubach/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/Medication/Meds Inpatients No PICU/NEW ITERATION MARCH 1 2019/unique_ndc_codes_reclassiffied_by_anita-3-4 for inpatient population.xlsx",
                        sheet = 2) %>%
  select(generic_name, ranked_tier_name)
f$ranked_tier_name <- tolower(f$ranked_tier_name)
f$ranked_tier_name <- str_trim(f$ranked_tier_name)

########## this is from a series of emils I had with Anita
a <- f %>% distinct(generic_name , ranked_tier_name) %>%
  group_by(generic_name) %>% mutate(ns= n_distinct(ranked_tier_name) ) %>%
  ungroup() %>%
  filter(ns>1) %>% select(-ns) %>%
  arrange(generic_name)
a <- a %>% filter(ranked_tier_name!="vaccine")
a$ranked_tier_name[a$generic_name == "alginic acid/al hydroxide/mg carbonate"] <- "gastrointestinal agents"
a$ranked_tier_name[a$generic_name == "antipyrine-benzocaine otic"] <- "miscellaneous topical agents"
a$ranked_tier_name[a$generic_name == "erythromycin-sulfisoxazole"] <- "anti-infectives"

a <- a %>% distinct(generic_name,ranked_tier_name)
f <- f %>% anti_join(a %>% distinct(generic_name)) %>%
  rbind(a) %>% distinct(generic_name,ranked_tier_name)
rm(a)
##############

d2 <- d %>% filter(is.na(ranked_tier_name))
d <- d %>% filter(!is.na(ranked_tier_name))

d2 <- d2 %>% select(-ranked_tier_name) %>%
  left_join(f ,by = "generic_name")

d2 %>% filter(!is.na(ranked_tier_name)) %>%  nrow()
# 
#############################################
# still not able to clasify
a <- d2 %>% select(generic_name ) %>%
  group_by(generic_name ) %>%
  summarise(n_records = n())
  write_csv(a,"Meds_not_able_to_classify_June_16_21.csv")
  
a <- d %>% filter(str_detect(generic_name,"accune")) %>%
  distinct(generic_name,ranked_tier_name,ranked_route_description)
View(a)
######################################################################
#  manuall adding by Eduardo criteria
d2 <- d2 %>%
  mutate(ranked_tier_name = case_when(str_detect(generic_name,"omnipaque") ~ "non-ionic iodinated contrast media",
                                      str_detect(generic_name,"accuneb") ~ "respiratory inhalant products",
                                      generic_name == "lmx 4 topical cream" ~ "topical anesthetics",
                                      T ~ ranked_tier_name),
         
         ranked_route_description =  case_when( str_detect(generic_name,"omnipaque") ~ "OIVSQIM",
                                                str_detect(generic_name,"accuneb") ~ "inhalation",
                                                generic_name == "lmx 4 topical cream" ~ "OIVSQIM",
                                                T ~ ranked_route_description) )
d2 %>% filter(!is.na(ranked_tier_name)) %>%  nrow() #  11071



# #####################################################################
d <- rbind(d ,d2[,names(d)]) %>%
  arrange(encounter_id , med_started_dt_tm)
rm(d2)


sum(is.na(d$ranked_tier_name)) ##  26760

d$ranked_tier_name[is.na(d$ranked_tier_name)] <- "other"


############################### unique medication file
# m <- d %>% distinct(ndc_code , .keep_all=T) %>%
#   select(ndc_code, generic_name, first_tier_category_name,
#          first_tier_category_name_2 , second_tier_category_name, second_tier_category_name_2,
#          third_tier_category_name, third_tier_category_name_2 , route_description,
#          ranked_tier_name , ranked_route_description)
# b <- readxl::read_excel("/Users/jlaubach/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/Medication/Rpeated Rows Study/Copy of Merge_Multum_Tables_with_6_categories with Route-2-15-18.xlsx",sheet = 1,
#                         col_names=T) %>% 
#   select(generic_drug_name ="generic drug_name","ndc_code")
# b$ndc_code <- as.numeric(b$ndc_code)
# m <- m %>% left_join(b , by = "ndc_code")
# #m <- m[,c(1:4 , 14 , 5:13)]
# m <- m %>% distinct(ndc_code , .keep_all=T)
# #####################################################
# 
# a <- d %>% filter(is.na(ranked_tier_name)) %>%
#   distinct(generic_name)
# 
# sum(is.na(d$ranked_tier_name)) ## 1566


#d$ranked_tier_name[is.na(d$ranked_tier_name)] <- "other"


#write_csv(d , "med_no_PICU_classified_by_generic_name_August_22_2018.csv")
#write_csv(d , "med_no_PICU_classified_by_generic_name_march_5_2019_2009_10_11_12_13_14_15_16.csv")
#write_csv(d , "med classified table Inpatients/med_no_PICU.csv")

d$ranked_tier_name <- tolower(d$ranked_tier_name)
d$ranked_route_description <- tolower(d$ranked_route_description)

d <- d %>% select(encounter_id, med_started_dt_tm,med_stopped_dt_tm,
                  generic_name,ranked_tier_name,ranked_route_description) %>%
  distinct(encounter_id,med_started_dt_tm,med_stopped_dt_tm,generic_name,ranked_route_description,.keep_all = T)

write_csv(d , "med classified table PICU CN 2021.csv")
#write_csv(m,"unique_ndc_codes_classiffied.csv")

## 150,250 pacientes