library(tidyverse)
library(lubridate)
library(doSNOW)


resolution_time <- "hour" #"sec","min","hour","day","week"

d <- read_csv("med classified table PICU CN 2021.csv")

d <- d %>%
  mutate(ranked_tier_name = if_else(generic_name == "vasopressin",   ## Anita requested to put vasopressins in a separate ranked_tier_name
                                    "vasopressin",ranked_tier_name),   ## I will call it vasopressins
         generic_name = gsub(" ", "_", generic_name, fixed = TRUE),
         generic_name = gsub("-", "_", generic_name, fixed = TRUE),
         generic_name = gsub(",", "_", generic_name, fixed = TRUE),
         generic_name = gsub("__", "_", generic_name, fixed = TRUE),
         med_started_dt_tm = floor_date(med_started_dt_tm,unit = resolution_time),
         med_stopped_dt_tm = ceiling_date(med_stopped_dt_tm,unit = resolution_time)) %>%
  distinct(encounter_id , ranked_tier_name ,generic_name ,
           med_started_dt_tm,med_stopped_dt_tm) %>%
  mutate(cases = row_number(),
         lengh_time = as.numeric( difftime(time1 = med_stopped_dt_tm,
                                           time2 = med_started_dt_tm,
                                           units = paste(resolution_time,"s",sep="") ))+1) 
d %>% distinct(encounter_id) %>% nrow()
# 8,340

##
n_clusters <- 6
parts <-  n_clusters*100
encs <- sort(unique(d$encounter_id))
k <- round(seq(1,length(encs),length.out = parts))

cl <- makeCluster(n_clusters) # PARALELIZING. DO NOT DO MORE THAN 2 CORES, NOT ENOUGHT MEMORY.
registerDoSNOW(cl)
pb <- txtProgressBar(max = length(k)-1 , style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
b <- foreach(  j = 1:(length(k)-1),
                       .combine = rbind,
                       .options.snow = opts,
                       .errorhandling = "remove",
                       .packages = c( "tidyverse","lubridate"),
                       .inorder = F) %dopar% {
                         return(      d %>% 
                                        inner_join( tibble(encounter_id = encs[ k[j]:k[j+1]]) ) %>%
                                        group_by(encounter_id , generic_name,cases) %>%
                                        expand(length_time = seq(1:lengh_time),
                                               begin_hour = med_started_dt_tm,
                                               ranked_tier_name=ranked_tier_name) %>%
                                        mutate(begin_hour = begin_hour + hours(length_time - 1)) %>%
                                        ungroup() %>%
                                        group_by(encounter_id , ranked_tier_name , begin_hour) %>% #,cases) %>% ## By not including 'cases' I do not doule count the intersect times (if any)!!!!!!!!
                                        summarise(n_generic_names_this_period = n(),
                                                  n_distinct_generic_names_this_period = n_distinct(generic_name)) %>% ungroup()
                                      ## in princicple this case could happend because of the rounding of the mes start and mes stop
                         )
                       }
close(pb)
stopCluster(cl) ### FRO THE MACHINE LEANRING DATA, I NEED TO CREATE
## n_generic_names_this_period = n() ## creo que este es el que sue
## n_distinct_generic_names_this_period = n_distinct(generic_name) ## este lo

#############################################
###Defining partitions to parallelize
d <- b
rm(b)
#save("gathered_hours_med_presence_2018.RData")
save(d,file = "ICU_gathered_hours_med_presence_for_ML_CN.RData")
#write_csv(b,"gathered_hours_med_presence_2018.csv")
d %>% distinct(encounter_id) %>% nrow()
#8,340

d %>% transmute(difs = n_generic_names_this_period -
                  n_distinct_generic_names_this_period ) %>% 
  group_by(difs) %>% summarise(ns=n()) %>% ungroup() %>% 
  mutate(totales = sum(ns), props = ns/totales*100)
difs       ns  totales       props
0 64195403 64246206 99.9       
1    48301 64246206  0.0752    
2     2384 64246206  0.00371   
3      101 64246206  0.000157  
4       15 64246206  0.0000233 
5        2 64246206  0.00000311