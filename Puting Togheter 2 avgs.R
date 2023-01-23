library(tidyverse)
#library(e1071)
library(lubridate)

#Sys.sleep(1 *  3600)

setwd("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Putting togheter All")

rm(list = ls())
gc()
########## Identifying the age class of each patient  to impute
# load("dml.RData")
# encs  <- l %>% distinct(encounter_id)
load("ambiente.RData") 
rm(d,l,m,meds,vitals,labs,a,la) # keeping encs only

encs <- encs %>% filter(vitals) %>% distinct(encounter_id)

z <- read_csv("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Critical_Care_Dataset-ICU_DEMOGRAPHICS-4-23.csv")
z <- z  %>%
  select(encounter_id = `Encounter Alias` , gender = Gender , birth = `Birth Date` ,
         admission =`Hospital Admission/Arrival Date-Time`)  %>%
  distinct_all()  %>%
  inner_join(encs) %>%
  mutate(birth2 = as.POSIXct(birth , format = "%m/%d/%Y %H:%M"),
         admission2 = as.POSIXct(admission, format = "%m/%d/%Y %H:%M"),
         admission2 = if_else(admission2 < birth2 , admission2 +years(100) , admission2 ),
         age_in_hours = as.numeric(difftime(time1 = admission2  , time2 = birth2 , units = "hours" )),
         age_in_years = as.numeric(difftime(time1 = admission2  , time2 = birth2 , units = "days" )) / 365
  )  %>%
  select(encounter_id , age_in_years,age_in_hours  ) 


## how many
f <- z %>% group_by(encounter_id) %>%
  summarise(n1 = n() , 
            n2 = n_distinct(age_in_hours)) %>%
  filter(n2 > 1)
# two encuunters have two ages
# erase them
z <- z %>% 
  distinct(encounter_id , .keep_all = T) %>%
  anti_join(f,by = "encounter_id") %>%
  distinct(encounter_id , .keep_all = T) %>%
  mutate(class_age = case_when( age_in_hours < 168 ~ "[1hr,1wk)",
                                age_in_hours >= 168 & age_in_hours < 672 ~ "[1wk,4wks)",
                                age_in_hours >= 672 & age_in_hours < 2160 ~ "[4wks,3mnths)",
                                age_in_hours >= 2160 & age_in_years < 1 ~ "[3mnths,1yr)",
                                age_in_years >= 1 & age_in_years < 2 ~ "[1yr,2yrs)",
                                age_in_years >= 2 & age_in_years < 3 ~ "[2yrs,3yrs)",
                                age_in_years >= 3 & age_in_years < 8 ~ "[3yrs,8yrs)",
                                age_in_years >= 8 & age_in_years < 12 ~ "[8yrs,12yrs)",
                                age_in_years >= 12  ~ "[12yrs,22yrs)")) %>%
  select(encounter_id , class_age)
rm(encs,f,d,m,l)
gc()

# 9,090 second round
############### AQUI VOY  June 19 2021

p <- 6
### p: number of hours the time-period will have
#for(p in c(2:9)  )
{
  ### the medication, labs and events information
  load("dml.RData")
  m$ns[is.na(m$ns)] <- 0 #  I  thoughth I already eliminated the ns. 28,596 na's.
  
  d <- z %>% select(encounter_id) %>% inner_join(d)
  l <- z %>% select(encounter_id) %>% inner_join(l)
  m <- z %>% select(encounter_id) %>% inner_join(m)
  
  d  <- d %>% select(encounter_id,  
                     sliced_hour = ends_with(as.character(p)) , 
                     events , valor)  
  d <- d %>%
    group_by(encounter_id,events,sliced_hour) %>%
    summarise(n = sum(!is.na(valor)),
              mins = if_else( n > 0, min(valor,na.rm = T),-50),
              maxs = if_else(n > 0, max(valor,na.rm = T),-50),
              average = if_else(n > 0, mean(valor,na.rm=T ),-50),
              sdt = if_else(n > 1, sd(valor,na.rm=T ),0) ) %>%
    ungroup() 
  d$average[d$average == -50] <- NA
  d$mins[d$mins==-50] <- NA
  d$maxs[d$maxs==-50] <- NA
  
  save(d,file="prov_d.RData")
  rm(d)
  gc()
  
  l  <- l %>% select(encounter_id,  
                     sliced_hour = ends_with(as.character(p)) , 
                     labs , valor)  %>%
    group_by(encounter_id,labs,sliced_hour) %>%
    summarise(n = sum(!is.na(valor)),
              mins = if_else( n > 0, min(valor,na.rm = T),-50),
              maxs = if_else(n > 0, max(valor,na.rm = T),-50),
              average = if_else(n > 0, mean(valor,na.rm=T ),-50),
              sdt = if_else(n > 1, sd(valor,na.rm=T ),0)) %>%
    ungroup() 
  l$average[l$average == -50] <- NA
  l$mins[l$mins==-50] <- NA
  l$maxs[l$maxs==-50] <- NA
  
  save(l,file="prov_l.RData")
  rm(l)
  gc()
  
  ################################## MEDS
  m <- m %>% select(encounter_id,  
                    sliced_hour = ends_with(as.character(p)) , 
                    meds , ns) 
  
  encs_sm <- m %>% 
    distinct(encounter_id) %>%
    slice_sample(prop = 0.5)
  m0 <- m %>% inner_join(encs_sm)
  m <- m %>% anti_join(encs_sm)
  
  save(m0,file = "prov_m0.RData")
  rm(m0)
  gc()
  
  
  m <- m %>% 
    group_by(encounter_id , sliced_hour,meds) %>%
    summarise(ns = sum(ns)) %>%  ## First we compute the number of hours the encounter received each generic name, max = 6,
    ## then within each group of medicine we sum the number of hours the encounter received each of the generic names,
    ##  the sum could be more than 6 indeed
    ungroup()
  m <- m %>%
    pivot_wider( names_from = meds,
                 values_from = ns,
                 values_fill = list(ns = 0))  %>%
    pivot_longer(c(-encounter_id ,-sliced_hour),
                 names_to = "meds",
                 values_to = "ns") %>%
    arrange(encounter_id , meds , sliced_hour) %>%
    group_by(encounter_id , meds) %>%
    mutate(slices_on = cumsum(ns>0), ## sliced_on
           prop_slices_on = slices_on / sliced_hour) %>%
    ungroup()
  # save(m,file="prov_m.RData")
  #  rm(m)
  # # gc
  # 
  # load("prov_m.RData") ## here
  #the nedications
  m <- m %>%
    filter(meds != "0") %>% 
    select(encounter_id,sliced_hour , meds , 
           med_ns = ns , med_on = slices_on , med_pon = prop_slices_on) 
  m <- m %>%
    pivot_wider(names_from = meds,
                values_from = c(med_ns,med_on,med_pon),
                # values_fill = list(med_ns = 0,med_on = 0,med_pon = 0),
                names_sep = "_")
  
  save(m,file = "prov_m3.RData")
  rm(m)
  gc()
  ###
  load("prov_m0.RData")
  
  m0 <- m0 %>% 
    group_by(encounter_id , sliced_hour,meds) %>%
    summarise(ns = sum(ns)) %>%  ## First we compute the number of hours the encounter received each generic name, max = 6,
    ## then within each group of medicine we sum the number of hours the encounter received each of the generic names,
    ##  the sum could be more than 6 indeed
    ungroup()
  m0 <- m0 %>%
    pivot_wider( names_from = meds,
                 values_from = ns,
                 values_fill = list(ns = 0))  %>%
    pivot_longer(c(-encounter_id ,-sliced_hour),
                 names_to = "meds",
                 values_to = "ns") %>%
    arrange(encounter_id , meds , sliced_hour) %>%
    group_by(encounter_id , meds) %>%
    mutate(slices_on = cumsum(ns>0), ## sliced_on
           prop_slices_on = slices_on / sliced_hour) %>%
    ungroup()
  
  m0 <- m0 %>%
    filter(meds != "0") %>% 
    select(encounter_id,sliced_hour , meds , 
           med_ns = ns , med_on = slices_on , med_pon = prop_slices_on) 
  m0 <- m0 %>%
    pivot_wider(names_from = meds,
                values_from = c(med_ns,med_on,med_pon),
                # values_fill = list(med_ns = 0,med_on = 0,med_pon = 0),
                names_sep = "_")
  
  save(m0,file = "prov_m30.RData")
  rm(m0)
  gc()
  
  #########################################################
  ################ Imputing slices values on LABS
  
  #labs kesy from the first of the midels ears ago
  load("~/Dropbox/Documents Mac Work/Creating Data for Neural Network HF 2018/labs_classes.RData") # labs, loading the group labels
  labs$super_group[labs$super_group == "PCO2 (venous, capillary or arterial)"] <- "PCO2"
  labs$super_group[labs$super_group == "HCO3 (bicarb)"] <- "HCO3"
  labs$super_group <- gsub( pattern = " ",replacement = "_",x= labs$super_group)
  labs$super_group <- tolower(labs$super_group)
  labs$super_group[labs$super_group == "base_excess_(or_deficit)"] <- "base_excess"
  
  newlabs <- read_csv("labs_keys.csv") %>%
    select(super_group,new_group=group)  %>%
    mutate(super_group = tolower(super_group),
           super_group = str_replace_all(super_group," ","_"),
           super_group = case_when(super_group == "po2" ~ "arterial_po2",
                                   super_group == "bicarbonate" ~ "hco3",
                                   super_group == "lactate" ~ "lactate_arterial",
                                   super_group == "white_blood_count" ~ "white_blood_cell_count",
                                   super_group == "ptt" ~ "partial_thromboplastin_time",
                                   super_group == "neutrophil_count" ~ "platlet_count" , ### THIS IS NOT CORRECT!
                                   T ~ super_group)) 
  
  newlabs <- newlabs %>%  left_join(labs)
  
  
  ### Obtaining the medians of the firs slices values for the labs (file a)
  a <- read_csv("~/Dropbox/Documents Mac Work/On Missing Values/First Record per Patient/Summaries_labs_first_recorded_value_by_class_age.csv")
  a  <- a %>% select(labs , class_age , medians) %>%
    left_join(labs , by = c("labs" = "super_group"))  %>%
    select(groups , class_age,medians) ## values to inpute
  
  a <- newlabs %>% left_join(a) %>%
    select(groups = new_group , class_age,medians)
  
  load("prov_l.RData")
  rm(newlabs,labs)
  
  l1 <- l %>%
    select(encounter_id , labs , sliced_hour,average) %>%
    pivot_wider(names_from = labs , 
                values_from = average,
                values_fill = list(average = NA)) %>% ## Here I am adding the space for the records of trure super groups
    pivot_longer(c(-encounter_id,-sliced_hour),
                 names_to = "labs" , 
                 values_drop_na = F , 
                 values_to = "average")   %>%
    mutate(added_nas =is.na(average),
           labs = as.integer(labs))   %>%
    arrange(encounter_id ,labs,sliced_hour,average) %>%  ## THIS IS IMPPORTAN, DO NOT ERASE
    group_by(encounter_id , labs) %>%
    mutate(line_number = row_number()) %>%
    ungroup() %>%
    inner_join(z , by = "encounter_id")  %>%
    left_join(a , by = c("class_age" =  "class_age",
                         "labs" = "groups")) %>%
    mutate(average = case_when(labs == 0 ~ 0,  # no labs
                               line_number == 1 & added_nas & labs != 0 ~ medians, ### Inputing the m as the average
                               T ~ average)  ) %>% ## to the very first record of each encounter if there is no numeric value
    select(-line_number,-medians,-class_age) %>% # , -added_nas) %>%
    mutate(line_number = row_number(),
           no_nas = !is.na(average) * 1, ## 0 when values are needed to be inputed
           cum_no_nas = cumsum(no_nas)) %>%
    select(-no_nas) %>%
    group_by(encounter_id , labs , cum_no_nas) %>%
    mutate(line_value = min(line_number)) %>%
    ungroup() %>%
    mutate(average = average[line_value]) %>%
    select(-line_value,-cum_no_nas,-line_number)
  
  l2 <- l %>% ###### HERE I AM REPEATING THE CODE FOR THE MINS:
    select(encounter_id , labs , sliced_hour,mins) %>%
    pivot_wider(names_from = labs , 
                values_from = mins,
                values_fill = list(mins = NA)) %>% ## Here I am adding the space for the records of trure super groups
    pivot_longer(c(-encounter_id,-sliced_hour),
                 names_to = "labs" , 
                 values_drop_na = F , 
                 values_to = "mins")   %>%
    mutate(added_nas =is.na(mins),
           labs = as.integer(labs))   %>%
    arrange(encounter_id ,labs,sliced_hour,mins) %>%  ## THIS IS IMPPORTAN, DO NOT ERASE
    group_by(encounter_id , labs) %>%
    mutate(line_number = row_number()) %>%
    ungroup() %>%
    inner_join(z , by = "encounter_id")  %>%
    left_join(a , by = c("class_age" =  "class_age",
                         "labs" = "groups")) %>%
    mutate(mins = case_when(labs == 0 ~ 0,  # no labs
                            line_number == 1 & added_nas & labs != 0 ~ medians, ### Inputing the m as the mins
                            T ~ mins)  ) %>% ## to the very first record of each encounter if there is no numeric value
    select(-line_number,-medians,-class_age , -added_nas) %>%
    mutate(line_number = row_number(),
           no_nas = !is.na(mins) * 1, ## 0 when values are needed to be inputed
           cum_no_nas = cumsum(no_nas)) %>%
    select(-no_nas) %>%
    group_by(encounter_id , labs , cum_no_nas) %>%
    mutate(line_value = min(line_number)) %>%
    ungroup() %>%
    mutate(mins = mins[line_value]) %>%
    select(-line_value,-cum_no_nas,-line_number) 
  
  l3 <-   l %>%   ###### HERE I AM REPEATING THE CODE FOR THE MAXS:
    select(encounter_id , labs , sliced_hour,maxs) %>%
    pivot_wider(names_from = labs , 
                values_from = maxs,
                values_fill = list(maxs = NA)) %>% ## Here I am adding the space for the records of trure super groups
    pivot_longer(c(-encounter_id,-sliced_hour),
                 names_to = "labs" , 
                 values_drop_na = F , 
                 values_to = "maxs")   %>%
    mutate(added_nas =is.na(maxs),
           labs = as.integer(labs))   %>%
    arrange(encounter_id ,labs,sliced_hour,maxs) %>%  ## THIS IS IMPPORTAN, DO NOT ERASE
    group_by(encounter_id , labs) %>%
    mutate(line_number = row_number()) %>%
    ungroup() %>%
    inner_join(z , by = "encounter_id")  %>%
    left_join(a , by = c("class_age" =  "class_age",
                         "labs" = "groups")) %>%
    mutate(maxs = case_when(labs == 0 ~ 0,  # no labs
                            line_number == 1 & added_nas & labs != 0 ~ medians, ### Inputing the m as the maxs
                            T ~ maxs)  ) %>% ## to the very first record of each encounter if there is no numeric value
    select(-line_number,-medians,-class_age , -added_nas) %>%
    mutate(line_number = row_number(),
           no_nas = !is.na(maxs) * 1, ## 0 when values are needed to be inputed
           cum_no_nas = cumsum(no_nas)) %>%
    select(-no_nas) %>%
    group_by(encounter_id , labs , cum_no_nas) %>%
    mutate(line_value = min(line_number)) %>%
    ungroup() %>%
    mutate(maxs = maxs[line_value]) %>%
    select(-line_value,-cum_no_nas,-line_number)
  
  l <- l %>% select(encounter_id , labs , sliced_hour , n,sdt)
  
  l <- l %>% 
    right_join(l1 , by = c("encounter_id","labs","sliced_hour")) %>%
    left_join(l2 , by = c("encounter_id","labs","sliced_hour")) %>%
    left_join(l3 , by = c("encounter_id","labs","sliced_hour"))
  
  rm(l1,l2,l3,a)
  gc()
  
  l$n[is.na(l$n)] <- 0
  l$sdt[is.na(l$sdt)] <- 0
  
  save(l,file="prov_l2.RData")
  rm(l)
  gc()
  
  ## INPUTING ON EVENTS
  # June 19 2021 I checked the label of the vital groups, I am using the correct version
  a <- read_csv("~/Dropbox/Documents Mac Work/On Missing Values/First Record per Patient/Summaries_events_first_recorded_value_by_class_age.csv")
  a <- a %>%  select(  events,class_age,medians )
  
  load("~/Dropbox/Documents Mac Work/Creating Data for Neural Network HF 2018/vitals_classes.RData") # vitals
  
  vitals <- vitals %>%
    mutate(super_group = tolower(super_group),
           super_group = str_replace_all(super_group," ","_"),
           super_group = str_replace_all(super_group,"hearth","heart"),
           super_group =  str_replace(super_group , "bp" , "blood_preasure"))
  
  a <- a %>%
    left_join(vitals , by = c("events" = "super_group"))  %>%
    select(groups , class_age,medians) ## values to inpute
  
  load("prov_d.RData")
  
  d1 <- d %>%
    select(encounter_id , events , sliced_hour,average) %>%
    pivot_wider(names_from = events , 
                values_from = average,
                values_fill = list(average = NA)) %>% ## Here I am adding the space for the records of trure super groups
    pivot_longer(c(-encounter_id,-sliced_hour),
                 names_to = "events" , 
                 values_drop_na = F , 
                 values_to = "average")   %>%
    mutate(added_nas =is.na(average),
           events = as.integer(events))   %>%
    arrange(encounter_id ,events,sliced_hour,average) %>%  ## THIS IS IMPPORTAN, DO NOT ERASE
    group_by(encounter_id , events) %>%
    mutate(line_number = row_number()) %>%
    ungroup() %>%
    inner_join(z , by = "encounter_id")  %>%
    left_join(a , by = c("class_age" =  "class_age",
                         "events" = "groups")) %>%
    mutate(average = case_when(events == 0 ~ 0,  # no events
                               line_number == 1 & added_nas & events != 0 ~ medians, ### Inputing the m as the average
                               T ~ average)  ) %>% ## to the very first record of each encounter if there is no numeric value
    select(-line_number,-medians,-class_age) %>% # , -added_nas) %>%
    mutate(line_number = row_number(),
           no_nas = !is.na(average) * 1, ## 0 when values are needed to be inputed
           cum_no_nas = cumsum(no_nas)) %>%
    select(-no_nas) %>%
    group_by(encounter_id , events , cum_no_nas) %>%
    mutate(line_value = min(line_number)) %>%
    ungroup() %>%
    mutate(average = average[line_value]) %>%
    select(-line_value,-cum_no_nas,-line_number)
  
  d2 <- d %>% ###### HERE I AM REPEATING THE CODE FOR THE MINS:
    select(encounter_id , events , sliced_hour,mins) %>%
    pivot_wider(names_from = events , 
                values_from = mins,
                values_fill = list(mins = NA)) %>% ## Here I am adding the space for the records of trure super groups
    pivot_longer(c(-encounter_id,-sliced_hour),
                 names_to = "events" , 
                 values_drop_na = F , 
                 values_to = "mins")   %>%
    mutate(added_nas =is.na(mins),
           events = as.integer(events))   %>%
    arrange(encounter_id ,events,sliced_hour,mins) %>%  ## THIS IS IMPPORTAN, DO NOT ERASE
    group_by(encounter_id , events) %>%
    mutate(line_number = row_number()) %>%
    ungroup() %>%
    inner_join(z , by = "encounter_id")  %>%
    left_join(a , by = c("class_age" =  "class_age",
                         "events" = "groups")) %>%
    mutate(mins = case_when(events == 0 ~ 0,  # no events
                            line_number == 1 & added_nas & events != 0 ~ medians, ### Inputing the m as the mins
                            T ~ mins)  ) %>% ## to the very first record of each encounter if there is no numeric value
    select(-line_number,-medians,-class_age , -added_nas) %>%
    mutate(line_number = row_number(),
           no_nas = !is.na(mins) * 1, ## 0 when values are needed to be inputed
           cum_no_nas = cumsum(no_nas)) %>%
    select(-no_nas) %>%
    group_by(encounter_id , events , cum_no_nas) %>%
    mutate(line_value = min(line_number)) %>%
    ungroup() %>%
    mutate(mins = mins[line_value]) %>%
    select(-line_value,-cum_no_nas,-line_number) 
  
  d3 <-   d %>%   ###### HERE I AM REPEATING THE CODE FOR THE MAXS:
    select(encounter_id , events , sliced_hour,maxs) %>%
    pivot_wider(names_from = events , 
                values_from = maxs,
                values_fill = list(maxs = NA)) %>% ## Here I am adding the space for the records of trure super groups
    pivot_longer(c(-encounter_id,-sliced_hour),
                 names_to = "events" , 
                 values_drop_na = F , 
                 values_to = "maxs")   %>%
    mutate(added_nas =is.na(maxs),
           events = as.integer(events))   %>%
    arrange(encounter_id ,events,sliced_hour,maxs) %>%  ## THIS IS IMPPORTAN, DO NOT ERASE
    group_by(encounter_id , events) %>%
    mutate(line_number = row_number()) %>%
    ungroup() %>%
    inner_join(z , by = "encounter_id")  %>%
    left_join(a , by = c("class_age" =  "class_age",
                         "events" = "groups")) %>%
    mutate(maxs = case_when(events == 0 ~ 0,  # no events
                            line_number == 1 & added_nas & events != 0 ~ medians, ### Inputing the m as the maxs
                            T ~ maxs)  ) %>% ## to the very first record of each encounter if there is no numeric value
    select(-line_number,-medians,-class_age , -added_nas) %>%
    mutate(line_number = row_number(),
           no_nas = !is.na(maxs) * 1, ## 0 when values are needed to be inputed
           cum_no_nas = cumsum(no_nas)) %>%
    select(-no_nas) %>%
    group_by(encounter_id , events , cum_no_nas) %>%
    mutate(line_value = min(line_number)) %>%
    ungroup() %>%
    mutate(maxs = maxs[line_value]) %>%
    select(-line_value,-cum_no_nas,-line_number)
  
  d <- d %>% select(encounter_id , events , sliced_hour , n,sdt)
  
  d <- d %>% 
    right_join(d1 , by = c("encounter_id","events","sliced_hour")) %>%
    left_join(d2 , by = c("encounter_id","events","sliced_hour")) %>%
    left_join(d3 , by = c("encounter_id","events","sliced_hour"))
  
  rm(d1,d2,d3,a)
  gc()
  
  d$n[is.na(d$n)] <- 0
  d$sdt[is.na(d$sdt)] <- 0
  
  #save(d , file = "prov_d2.RData")
  # rm(d)
  # gc()
  
  
  
  #####################################
  ############################# PUTTING EVERYTHING TOGHETER
  
  
  #######
  #  load("prov_d2.RData")
  d <- d %>%
    filter(events > 0) %>%
    select(encounter_id,sliced_hour , events ,
           vital_n = n , 
           vital_average = average,  
           vital_sdt = sdt , 
           vital_mins = mins ,  
           vital_maxs = maxs )  %>% 
    mutate(events = as.character(events)) %>%
    pivot_wider(names_from = events,
                values_from = c(vital_n,
                                vital_average,
                                vital_sdt,
                                vital_mins,
                                vital_maxs),
                names_sep = "_")
  
  #####
  load("prov_l2.RData")
  
  l <- l %>%
    filter(labs > 0) %>%
    select(encounter_id,sliced_hour , labs ,
           lab_n = n ,
           lab_average = average,
           lab_sdt = sdt , 
           lab_mins = mins ,  
           lab_maxs = maxs )  %>%
    mutate(labs = as.character(labs))  %>%
    pivot_wider(names_from = labs,
                values_from = c(lab_n,
                                lab_average,
                                lab_sdt,
                                lab_mins,
                                lab_maxs),
                names_sep = "_")
  
  l <- l %>% inner_join(d,by = c("encounter_id","sliced_hour") )
  
  save(l,file="ld.RData")
  rm(d,l)
  gc()
  
  
  ###
  
  load("prov_m3.RData")
  load("prov_m30.RData")
  a <- tibble(nombres = names(m))
  a0 <- tibble(nombres = names(m0))
  mnew <- a0 %>% anti_join(a) ##additions for m
  m0new <- a %>% anti_join(a0)# adittions for m0
  
  mnews <- matrix(0,ncol = mnew %>% nrow,nrow = m %>% nrow() )
  colnames(mnews) <- mnew$nombres
  mnews <- as_tibble(mnews)
  m <- m %>% cbind(mnews)
  rm(mnews)
  
  m0news <- matrix(0,ncol = m0new %>% nrow,nrow = m0 %>% nrow() )
  colnames(m0news) <- m0new$nombres
  m0news <- as_tibble(m0news)
  m0 <- m0 %>% cbind(m0news)
  rm(m0news)
  gc()
  m0 <- m0[,names(m)]
  
  m <- m %>% rbind(m0)
  rm(m0,a0,m0new,mnew,a)
  gc()
  
  #save(m,file="mfinal.RData")
  
  #putting everything togheter
  load("ld.RData")
  
  m <- l %>% inner_join(m,by = c("encounter_id","sliced_hour"))
  
  d <- m
  rm(m,l)
  gc()
  ################################
  ## All was put togheter
  
  names(d) <- tolower(names(d)) ## Just in case
  
  #LOADING icu_info
  ## contains the icu information of "horas", all the slices for each patient (already
  ##  taking into account the meds, labs and events)
  ## The times are sliced by 1, 2, 3,... 9 hours intervals "length_slice"
  load("icu_info.RData")
  
  d <- icu_info %>%
    filter(length_slice == p) %>% select(-length_slice) %>%
    inner_join(d, by = c("encounter_id","sliced_hour"))
  
  save(d,file = paste("labs_meds_events_for_nn_",p,"_hours.RData" ,sep = "") )
  write_csv(d %>% slice_head(n=50),paste("sample_labs_meds_events_for_nn_",p,"_hours.csv",sep=""))
  
  rm(d,icu_info)
  gc()
  
}

rm(list = ls())
gc()
.rs.restartR()
