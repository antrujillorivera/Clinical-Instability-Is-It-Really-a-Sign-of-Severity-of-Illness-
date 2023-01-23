library(tidyverse)

# k <- read_csv("CN meds classified Dec 31 2020.csv")
# k <- k %>% distinct(ranked_tier_name)  ## the unique classes of medications
# write_csv(k , "fijas.csv")

#############
d <- read_delim("~/Dropbox/Documents Mac Work/Creating Data from CN/Oct 28 2020/Meds_Data_2018_01_01_2020_02_29.txt",
                delim ="|", col_names = T,n_max =Inf)

d %>% filter(!is.na(drug_name)) %>% nrow()  # 1,068,527
d <- d %>% filter(is.na(drug_name)) %>%  ## 63,531
  select(encounter_id = FIN_SCR ,
         med_started_dt_tm = Order_DT_SCR,
         med_stopped_dt_tm = Order_Completed_DT_SCR,
         med_stopped_dt_tm2 = ORD_DISCON_DT_SCR,
         ORDER_MNEMONIC,
         RX_ROUTE) %>%
  mutate(med_stopped_dt_tm = if_else(is.na(med_stopped_dt_tm),
                                     med_stopped_dt_tm2,
                                     med_stopped_dt_tm)) %>%
  select(-med_stopped_dt_tm2) %>%
  mutate(med_stopped_dt_tm = if_else(is.na(med_stopped_dt_tm),
                                     med_started_dt_tm,
                                     med_stopped_dt_tm)) %>%
  mutate(start_aux = med_started_dt_tm ,
         med_started_dt_tm = if_else(med_started_dt_tm < med_stopped_dt_tm, ## if med_stop>med_Start,
                                     med_started_dt_tm,med_stopped_dt_tm), ## exchangethem...
         med_stopped_dt_tm = if_else(start_aux < med_stopped_dt_tm,
                                     med_stopped_dt_tm , med_started_dt_tm)) %>%
  #filter(med_started_dt_tm<=med_stopped_dt_tm) # or keep only the records that have the correct order
  select(-start_aux)

################################## loading the medications classified one by one by Anita and Jim C
f <- readxl::read_excel("Missing medication names updated JC 01_13_21.xlsx",sheet = 1 )
f <- f %>% 
  select(ORDER_MNEMONIC, RX_ROUTE, ranked_tier_name) %>%
  mutate(ranked_tier_name = case_when(ranked_tier_name == "bronchodilator" ~ "bronchodilators",
                                      ranked_tier_name == "gastrointestinal agent" ~ "gastrointestinal agents",
                                      T ~ ranked_tier_name))

# f %>% mutate(nulls = ranked_tier_name == "NULL") %>%
#   group_by(nulls) %>%
#   summarise(n_records = sum(ns),
#             meds = n())
# nulls n_records  meds
# FALSE     49817  1224
# TRUE      13714  2429
d <- d %>% left_join(f, by = c("ORDER_MNEMONIC", "RX_ROUTE")) %>%
  filter(ranked_tier_name != "NULL") %>% 
  select(encounter_id , 
                  med_started_dt_tm , 
                  med_stopped_dt_tm , 
                  generic_name = ORDER_MNEMONIC,  ### for now I change the name so that I can use the code copied from Classifying 2.R line 82
                  ranked_tier_name) %>%
  mutate(g2 = str_extract(generic_name, '\\D*(?=\\d)'),
         g2 = if_else(is.na(g2),generic_name,g2),
         g2 = str_to_lower(g2),
         g2 = str_trim(string = g2,side = "both"),
         g2 = str_remove_all(g2,"\\(<"),
         g2 = str_remove_all(g2,"\\(elem\\)"),
         g2 = str_remove_all(g2,"\\(human\\)"),
         g2 = str_remove_all(g2,"\\(>/="),
         g2 = str_remove(g2,"\\($"),
         g2 = str_remove(g2,"\\(vsl#"),
         g2 = str_remove(g2,"-d$"),
         g2 = str_remove(g2," tab$"),
         g2 = str_remove(g2,"size"),
         g2 = str_trim(string = g2,side = "both")) %>%
  select(encounter_id , 
         med_started_dt_tm , 
         med_stopped_dt_tm , 
         generic_name = g2, 
         ranked_tier_name)
#write_csv(d %>% distinct(generic_name),"gen_names.csv")

## eliminate these: 1343 records and the class medications are not used by the models
d <- d  %>% filter(ranked_tier_name != "hematologic agents" &
                     ranked_tier_name != "hypertonic saline")

# d <- d  %>% mutate(diffs_time = as.numeric(difftime(time2 = med_started_dt_tm,
#                                                     time1 = med_stopped_dt_tm,
#                                                     units = "hours")))
# a <- d %>% filter(diffs_time>=5)
# summary(d$diffs_time)

d <- d %>%
  mutate(number_row = row_number())

#########################################################
###Defining partitions to parallelize
parts <- 18
encs <- sort(unique(d$encounter_id))
k <- round(seq(1,length(encs),length.out = parts))

cl <- makeCluster(6) # PARALELIZING. DO NOT DO MORE THAN 2 CORES, NOT ENOUGHT MEMORY.
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

relations <- relations %>% distinct(number_row , .keep_all = T)

d <- d %>% left_join(relations) %>% select(-number_row) %>%
  arrange(cluster_group)

d <- d %>% filter(!is.na(med_started_dt_tm) & 
                    !is.na(med_stopped_dt_tm)) %>%  ## this line is a Murray instruction
  group_by(encounter_id,cluster_group) %>%
  summarise(
    med_started_dt_tm = min(med_started_dt_tm),
    med_stopped_dt_tm = max(med_stopped_dt_tm),
    generic_name = first(generic_name),
    ranked_tier_name = first(ranked_tier_name)  ) %>% 
  ungroup() %>%
  select(-cluster_group)

write_csv(d %>% distinct(ranked_tier_name),"new_ranked.csv")

########################
save(d,file = "difficul_classified_jan_14_2021.RData")
