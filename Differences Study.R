library(tidyverse)
library(data.table)
library(gg.gap)

library(ggthemes)


#loading the data models
#loading the data models
load("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Modeling Deaths/Calibrating FC3/calibrated predictions p3.RData")
d <- d %>% 
  # filter( !type_data ) %>% ##keeping the test set only
  filter(lbk == 1 & 
           lmb == 1 &
           !by_time_periods & 
           calibrator == 1)

ys <- d %>% distinct(encounter_id,outcome)

d <- d %>% filter(slice_in_icu <= 264/3 #& !type_data
                  ) %>% 
  select(encounter_id , slice_in_icu,#type_data,
            p) 
  

ps <- d
rm(d)

ps <- as.data.table(ps)
setkey(x = ps , encounter_id  )
####################

ps <- ps %>%
  group_by(encounter_id) %>%
  arrange(slice_in_icu)  %>%
  mutate(f1 = lead(slice_in_icu,1) - slice_in_icu, ## forward difference [x+h] - x
         f2 = lead(slice_in_icu,2) - slice_in_icu,
         f3 = lead(slice_in_icu,3) - slice_in_icu,
         f4 = lead(slice_in_icu,4) - slice_in_icu,
         
         pf1 = if_else(f1 == 1,lead(p,1), -1 ), ##forward values
         pf2 = case_when(f1 == 2 ~ lead(p,1),
                         f2 == 2 ~ lead(p,2),
                         T ~ -1), 
         pf3 = case_when(f1 == 3 ~ lead(p,1), 
                         f2 == 3 ~ lead(p,2),
                         f3 == 3 ~ lead(p,3),
                         T ~ -1),
         pf4 = case_when(f1 == 4 ~ lead(p,1),
                         f2 == 4 ~ lead(p,2),
                         f3 == 4 ~ lead(p,3),
                         f4 == 4 ~ lead(p,4),
                         T ~ -1)) %>%
  ungroup()
ps[ps==-1] <- NA

## computing  the changes in mirtality   risks
ps <- ps %>%
  mutate( df1 = pf1 - p,
          df2 = pf2 - p,
          df3 = pf3 - p,
          df4 = pf4 - p ) %>%
  select(encounter_id , slice_in_icu , 
          p , p1 = df1 , p2 = df2,  p3 = df3, p4 = df4) 
save.image(file = "enviroment all.RData")
# 
# load("enviroment all.RData")
################################################################
ps <- ps %>%
  pivot_longer(c(p1,p2,p3,p4),
               names_to = "difference",
               values_to = "valor_diff",
               values_drop_na = T) %>%
  mutate(difference = as.integer(str_remove(difference,"p")),
         ratio_diff = valor_diff / p)

#maximum change per  patient
# smax  <- ps %>% 
#   group_by(encounter_id,difference) %>%
#   summarise(valor_diff = max(valor_diff),
#             ratio_diff = max(ratio_diff)) %>%
#   ungroup() %>%
#   mutate(max_mins = "maxs")
smax  <- ps %>% 
  group_by(encounter_id,difference) %>%
  slice_max(order_by = valor_diff,
            n = 1) %>%
  ungroup() %>%
  select(encounter_id,difference , 
         valor_diff , start_slice_diff = slice_in_icu ) %>%
  inner_join(
    ps %>% 
      group_by(encounter_id,difference) %>%
      slice_max(order_by = ratio_diff,
                n = 1) %>%
      ungroup() %>%
      select(encounter_id,difference , 
             ratio_diff , start_slice_r_diff = slice_in_icu ),
    by = c("encounter_id","difference") )  %>%
  mutate(max_mins = "maxs")

# smin  <- ps %>% 
#   group_by(encounter_id, difference) %>%
#   summarise(valor_diff = min(valor_diff),
#             ratio_diff = min(ratio_diff)) %>%
#   ungroup() %>%
#   mutate(max_mins = "mins")
smin <- ps %>% 
  group_by(encounter_id,difference) %>%
  slice_min(order_by = valor_diff,
            n = 1) %>%
  ungroup() %>%
  select(encounter_id,difference , 
         valor_diff , start_slice_diff = slice_in_icu ) %>%
  inner_join(
    ps %>% 
      group_by(encounter_id,difference) %>%
      slice_min(order_by = ratio_diff,
                n = 1) %>%
      ungroup() %>%
      select(encounter_id,difference , 
             ratio_diff , start_slice_r_diff = slice_in_icu ),
    by = c("encounter_id","difference") )  %>%
  mutate(max_mins = "mins")

s <- rbind(smin,smax)  %>% 
  inner_join(ys) %>%
  mutate(outcome = if_else(outcome > 0 ,"Deaths", "Survivors") )
rm(smin,smax) 

 

#save(s,file = "maxs_mins_diffs_test.RData" )
#save(s,file = "maxs_mins_diffs.RData" )
save(s,file = "maxs_mins_diffs_10_18_22.RData" )
  #################################
########################################

load("maxs_mins_diffs.RData")

s <- s %>%
  mutate(max_mins = if_else(max_mins == "mins", "Best Improvement","Worst Deterioration"),
         difference = paste("lag",difference))


pdf(paste("Maximum - Minimum Deterioration dif.pdf",sep=""),
    width=12,height=6 )
ggplot(s  , 
       aes(x=valor_diff, 
           fill=outcome)) +
  geom_histogram( aes(y=..density..),
                  position="identity", 
                  alpha=0.5 , bins = 60 )+
  theme_light() +
  facet_wrap(max_mins~ difference, scales = "free", ncol = 4) +
  theme(legend.position = "bottom",legend.title=element_blank()) +
  labs(y = "Density",
       x = "Change Risk Mortality" ) +
  scale_fill_colorblind()
dev.off()

# pdf(paste("Maximum - Minimum ratio Deterioration dif.pdf",sep=""),
#     width=12,height=6)
# ggplot(s  , 
#        aes(x=ratio_diff, 
#            fill=outcome)) +
#   geom_histogram( aes(y=..density..),
#                   position="identity", 
#                   alpha=0.5 , bins = 30 )+
#   theme_light() +
#   facet_wrap( paste(max_mins,"per encounter") ~ paste("lag",difference), scales = "free", ncol = 4) +
#   theme(legend.position = "bottom",legend.title=element_blank()) +
#   labs(y = "Density",
#        x = "Change Risk Mortality" ) +
#   scale_fill_colorblind()
# dev.off()

