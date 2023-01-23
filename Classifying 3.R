library(tidyverse)

load("prov_env_june_16_2021.RData")

# 428,517 rows

d_class <- d %>% filter(!is.na(ranked_tier_name)) # 249,572 rows
d <- d %>% filter(is.na(ranked_tier_name)) %>%  # 178,945 rows
  select(-ranked_tier_name , -ranked_route_description)

# Doing the classification again  using new table. Loading the classification:
s <- readxl::read_excel("/Users/jlaubach/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/Medication/Rpeated Rows Study/AP MP Medication priority 3-1-18.xlsx",sheet = 1)  
s <- s %>% select( rango ,"AP-MP Ranking" ,route_description)  
names(s)[1:2] <- c("rank","ap_mp")
s <- s %>% filter(!is.na(route_description) & !ap_mp %in% c("Nitric Oxide" , "vasopressin" ) )
####### Correcting the file using the next ugly way of programming:
# replacing characters " or " by the character ",", it will be useful
## in the next cycle.
s$route_description <- gsub(" or ", ",",   s$route_description)

a <-plyr::ldply(strsplit(s$route_description,split=',', fixed=TRUE), rbind)
a <- as_tibble(a)
names(a) <- paste("v",names(a),sep="")
a$ap_mp <- s$ap_mp
a$rank <- s$rank
s <- a %>% gather( provs , route_description  , -ap_mp,-rank ) %>%
  filter(!is.na(route_description)) %>% select(-provs) %>%
  arrange(rank) %>% mutate(rank = row_number())
rm(a)
###################
s <- s %>% mutate(ap_mp = gsub("(^\\s+)|(\\s+$)", "",tolower(ap_mp)) ,
                  route_description = gsub("(^\\s+)|(\\s+$)", "",tolower(route_description))
)
d <- d %>% mutate(first_tier_category_name = gsub("(^\\s+)|(\\s+$)", "",tolower(first_tier_category_name)) ,
                  second_tier_category_name = gsub("(^\\s+)|(\\s+$)", "",tolower(second_tier_category_name)) ,
                  third_tier_category_name = gsub("(^\\s+)|(\\s+$)", "",tolower(third_tier_category_name)) ,
                  first_tier_category_name_2 = gsub("(^\\s+)|(\\s+$)", "",tolower(first_tier_category_name_2)) ,
                  second_tier_category_name_2 = gsub("(^\\s+)|(\\s+$)", "",tolower(second_tier_category_name_2)) ,
                  third_tier_category_name_2 = gsub("(^\\s+)|(\\s+$)", "",tolower(third_tier_category_name_2)) ,
                  route_description = gsub("(^\\s+)|(\\s+$)", "",tolower(route_description))
)

s <- s %>% mutate(route_description = case_when(
  route_description == "intravenous or injectable" ~ "intravenous",
  route_description == "injectable" ~ "intravenous",
  route_description == "by mouth" ~ "oral",
  route_description == "transmucosal and oral" ~ "oral transmucosal",
  TRUE ~ route_description)) %>% distinct(ap_mp,route_description) %>%
  mutate(rank = row_number() )

d <- d %>% mutate(route_description = case_when(
  route_description =="injectable" ~ "intravenous",
  route_description == "intravenous or injectable" ~ "intravenous",
  route_description == "by mouth" ~ "oral",
  TRUE ~ route_description))
d$rown <- 1:nrow(d)

### This is an auxiliary table to help in the classificy the drugs
ij <- d %>% select(rown ,route_description, first_tier_category_name, second_tier_category_name , third_tier_category_name,
                   first_tier_category_name_2 , second_tier_category_name_2, third_tier_category_name_2 ) %>% 
  gather(category , ap_mp , -rown,-route_description) %>%
  select(rown , ap_mp , route_description) 

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

ij <- ij %>% mutate(ap_mp_route = paste(ap_mp , route_description,sep=" ")) %>% select(rown ,ap_mp_route )
s <- s %>% mutate(ap_mp_route = paste(ap_mp , route_description,sep=" "))

ij <- ij %>% left_join(s ) %>% filter(!is.na(ap_mp)) %>% 
  arrange(rown , rank) %>%
  group_by(rown) %>%
  summarise(ap_mp = first(ap_mp),
            route_description = first(route_description)) %>%
  ungroup()

names(ij)[2:3] <- c("ranked_tier_name" , "ranked_route_description")

d <- d %>% left_join(ij) %>% select(-rown)

dim(d) # 178,945 rows
sum(is.na(d$ranked_tier_name))   # 169,840 rows

## These are the ones we still can not classifiy
# a <- d %>% filter(is.na(ranked_tier_name)) %>% distinct(generic_name , route_description , .keep_all=T) %>%
#   select(generic_name , contains("category_name"),route_description)
# write_csv(a , "Not able to classify march 4th.csv")

#####################
d <- rbind(d[,names(d_class)],d_class)
rm(d_class)

save(d , file = "prov_env_june_16_2021_2.RData")

#load("prov_env3_dec_31_2020.Rdata")
## I CAN NOT USE THE REST OF THE CODE FOR THIS DATA SET, I WOULD NEED THE NDC CODE
## NOT AVAILABLE FOR THIS DATA SET
############################
#########################################################
## A final classifiction using the generic names of the medications:
# d$generic_name <- gsub("(^\\s+)|(\\s+$)", "",tolower(d$generic_name))
# 
# d_class <- d %>% filter(!is.na(ranked_tier_name))
# d <- d %>% filter(is.na(ranked_tier_name)) %>% 
#   select(-ranked_tier_name , -ranked_route_description)
# 
# d <- d %>% mutate( ranked_tier_name = case_when( 
#   generic_name == "nitric oxide" & (route_description %in% c("inhalation","oral")) ~ "nitric oxide",
#   generic_name == "vasopressin" & (route_description %in% c("intravenous","oral")) ~ "vasopressin",
#   TRUE ~ "NA"),
#   ranked_route_description = case_when(
#     generic_name == "nitric oxide" & route_description == "inhalation" ~ "inhalation",
#     generic_name == "nitric oxide" & route_description == "oral" ~ "oral",
#     generic_name == "vasopressin" & route_description == "intravenous" ~ "intravenous",
#     generic_name == "vasopressin" & route_description == "oral" ~ "oral",
#     TRUE ~ "NA")
# )
# 
# d$ranked_tier_name[d$ranked_tier_name=="NA"] <- NA
# d$ranked_route_description[d$ranked_route_description=="NA"] <- NA
# sum(is.na(d$ranked_route_description))
# 
# d <- rbind(d[,names(d_class)],d_class)
# rm(d_class)                     
# 
# d <- d %>% distinct() %>% arrange(encounter_id , med_started_dt_tm)
# 
# ## These are the ones we still can not classifiy
# b <- readxl::read_excel("/Users/jlaubach/Dropbox/Documents Mac Work/Create Table As Time Series Analysis/Medication/Rpeated Rows Study/Copy of Merge_Multum_Tables_with_6_categories with Route-2-15-18.xlsx",sheet = 1,
#                         col_names=T) %>% 
#   select("generic drug_name","ndc_code")
# b$ndc_code <- as.numeric(b$ndc_code)
# 
# 
# a <- d %>% 
#   filter(is.na(ranked_tier_name)) %>% 
#   distinct(generic_name , route_description , .keep_all=T)
# 
# 
# #%>%
#   select(generic_name, ndc_code , contains("category_name"),route_description) %>%
#   #left_join(m,by="medication_id") %>% 
#   left_join(b,by="ndc_code") %>%
#   arrange(generic_name) %>% distinct(ndc_code,.keep_all=T)
# ###############
# 
# dim(a)
# # 776 unique drugs without classification
# # #save(d , file = "med_classified_aug_22_2018.RData")
# # save(d , file = "med_classified_sept_5_2018.RData")
# 
# save(d , file = "med_classified_dec_19_2019.RData")
