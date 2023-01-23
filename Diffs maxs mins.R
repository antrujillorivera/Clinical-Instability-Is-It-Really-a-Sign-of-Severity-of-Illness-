library(tidyverse)
library(psych)
library(ggthemes)

#load("maxs_mins_diffs_test.RData") # s
load("maxs_mins_diffs.RData") # s

# x axis: Mortality  Risk Change
##  Maximum Risk Decrease, "Maximum Risk Increase" #
## for the strip titles : maximum decrease in risk, maximum increase in risk
# get rid of the ourliers, and change  the scale
## change colors to grwy
##decrease  the thikness of the box countour
#

## test the difference  of tthe medians and means


s <- s %>% select(-encounter_id) %>%
  rename( diff = valor_diff, ratios = ratio_diff ) %>%
  pivot_longer( c(diff , ratios ) , 
                names_to = "summaries",
                values_to = "values") %>%
   mutate(max_mins = if_else(max_mins == "mins", 
                             "Best Improvement","Worst Deterioration"),
          difference = paste(difference*3,"-hour",sep=""),
          difference = factor(difference , 
                              levels = c("12-hour" , "9-hour" , "6-hour" , "3-hour"))
          #difference = paste("lag",difference)
          )

# pdf(paste("Fig 2 Maximum - Minimum Deterioration BoxPlots dif.pdf",sep=""),
#     width=10,height=5 )
#png("Fig 2 Maximum - Minimum Deterioration BoxPlots dif.png",
#    width=10,height=5 ,res=200,units = "in")
tiff(filename = "Fig 2 Maximum - Minimum Deterioration BoxPlots dif.tif" ,
     width = 10, height = 5, units = "in", res = 400 , pointsize = 12, compression = "lzw")

ggplot(s %>% 
         filter(str_starts(summaries , "d") &
                  !(str_ends(max_mins,"t") & values > 0.25)
                  )  ) +
  geom_boxplot(aes(x = difference,
               fill = outcome,
               y = values),
               outlier.size = rel(0.5)) +
  theme_light() +

  facet_wrap(max_mins~ . , scales = "free", ncol = 2) +
  theme(legend.position = "bottom",legend.title=element_blank(),
        strip.text = element_text(face = "bold",size = rel(1))) +
  labs(x = "",
       y = "Mortality Risk Change" ) +
 scale_fill_manual(values=c("white","grey")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5))+
  coord_flip()
dev.off()

###########################################
# Computing summaries
sm <- s %>%
  group_by(difference , outcome ,max_mins , summaries ) %>%
  summarise(N = n(),
            avg = mean(values),
            sds = sd(values),
            medians = median(values),
            q1 = quantile(values , probs = .25),
            q2 = quantile(values , probs = .75)) %>%
  ungroup() %>%
  pivot_wider(names_from = outcome,
              values_from = c(N, avg  ,  sds , medians , q1,q2),
              names_sep = "_")   %>%
  mutate(s = sqrt( ((N_Deaths-1)*sds_Deaths+ (N_Survivors-1)*sds_Survivors) / (N_Deaths  + N_Survivors - 2 ) ),
         cohens_d = (avg_Deaths - avg_Survivors) / s) %>%
  select(-s)
################################
library(effectsize)# rank biserials and proportions one is larger  than the othert risk change

a <- NULL
for( i in unique(s$max_mins))
  for(j in unique(s$difference))
  for(k in unique(s$summaries)){
    a1 <- rank_biserial( values ~ outcome , 
                         data = s %>% 
                           filter(max_mins == i & difference == j & summaries ==  k ) %>%
                           select(outcome , values) ,
                         ci = .95 ) 
    
    a <- tibble(max_mins = i , difference = j , summaries =  k,
                rb = a1$r_rank_biserial,
                lrb = a1$CI_low,
                urb = a1$CI_high ) %>%
      mutate(p = (abs(rb)+1)/2, # ## proportions of pairs between deaths and survivors whenre sdt error is larger for
             l = (abs(lrb)+1)/2, ### deaths to be
             u = (abs(urb)+1)/2) %>%
      rbind(a)
  }

a  <- a %>% 
  rename( rank_biserial = rb,
          l95_rank_biserial = lrb,
          u95_rank_biserial = urb,
          props_death_larger_surv = p,
          l95_props_death_larger_surv = l,
          u95_props_death_larger_surv = u,)

sm <- sm %>%  inner_join(a, by = c("difference", "max_mins", "summaries"))
rm(a)

### rounding
sm <- sm %>% 
  pivot_longer(-c(difference , max_mins , summaries,N_Deaths,N_Survivors),
               names_to = "caca",
               values_to = "pipi") %>%
  mutate(pipi = sprintf('%.3f',pipi)) %>%
  pivot_wider(names_from = caca , values_from = pipi)


#write_csv(sm , "maxs_mins_changes_summaries_test.csv")
write_csv(sm , "maxs_mins_changes_summaries.csv")




##########################################
##   Computing for all of the differences, not only the maxs and minis
#####################################
rm(list = ls())
gc()
load("enviroment all.RData")
ps <- ps  %>%
  inner_join(ys) %>%
  select(  -slice_in_icu) %>%
  pivot_longer(c(p1,p2,p3,p4),
               names_to = "difference",
               values_to = "valor_diff",
               values_drop_na = T) %>%
  mutate(difference = as.integer(str_remove(difference,"p")),
         ratio_diff = valor_diff / p)

ps <- ps %>%  
  select(-p)  %>%
   rename( diff = valor_diff, ratios = ratio_diff ) %>%
  pivot_longer( c(diff , ratios ) , 
                names_to = "summaries",
                values_to = "values") 

# rank biserials and proportions one is larger  than the othert risk change
a <- NULL
  for(j in unique(ps$difference))
    for(k in unique(ps$summaries)){
      a1 <- rank_biserial( values ~ outcome , 
                           data = ps %>% 
                             filter(difference == j & summaries ==  k ) %>%
                             select(outcome , values) ,
                           ci = .95 ) 
      
      a <- tibble(difference = j , summaries =  k,
                  rb = a1$r_rank_biserial,
                  lrb = a1$CI_low,
                  urb = a1$CI_high ) %>%
        mutate(p = (abs(rb)+1)/2, # ## proportions of pairs between deaths and survivors whenre sdt error is larger for
               l = (abs(lrb)+1)/2, ### deaths to be
               u = (abs(urb)+1)/2) %>%
        rbind(a)
    }

a  <- a %>% 
  rename( rank_biserial = rb,
          l95_rank_biserial = lrb,
          u95_rank_biserial = urb,
          props_death_larger_surv = p,
          l95_props_death_larger_surv = l,
          u95_props_death_larger_surv = u)
#################################################################

 ps1 <- ps %>% group_by( difference , outcome , summaries ) %>%
  summarise(N = n_distinct(encounter_id),
            n_intervals = n(),
            avg = mean(values),
            sds = sd(values),
            medians = median(values),
            q1 = quantile(values , probs = .25),
            q2 = quantile(values , probs = .75)) %>%
  ungroup() %>%
   mutate(outcome = if_else(outcome > 0 ,"Deaths", "Survivors")) %>%
  pivot_wider(names_from = outcome,
              values_from = c(N,n_intervals, avg  ,  sds , medians , q1,q2),
              names_sep = "_")   %>%
  mutate(s = sqrt( ((N_Deaths-1)*sds_Deaths+ (N_Survivors-1)*sds_Survivors) / (N_Deaths  + N_Survivors - 2 ) ),
         cohens_d = (avg_Deaths - avg_Survivors) / s) %>%
  select(-s)
############

 ps1 <- ps1 %>%
  inner_join(a) %>%
   mutate( difference = paste("lag",difference) )  
 
 write_csv(ps1 , "changes_summaries.csv")
 
 ###########################################plots
 
 # ps <- ps %>%
 #   mutate(outcome = if_else(outcome > 0 ,"Deaths", "Survivors"),
 #          difference = paste("lag",difference))
 # 
 # pdf(paste("Change Risk BoxPlots dif.pdf",sep=""),
 #     width=10,height=5 )
 # ggplot(ps %>% filter(str_starts(summaries , "d"))  ) +
 #   geom_boxplot(aes(x = difference,
 #                    fill = outcome,
 #                    y = values),
 #                outlier.size = rel(0.5)) +
 #   theme_light() +
 #   #facet_wrap(max_mins~ summaries, scales = "free", ncol = 2) +
 #   #facet_wrap(max_mins~ . , scales = "free", ncol = 2) +
 #   theme(legend.position = "bottom",legend.title=element_blank()) +
 #   labs(x = "",
 #        y = "Change Risk Mortality" ) +
 #   scale_fill_colorblind() +
 #   coord_flip()
 # dev.off()
 
 #############################################################
 #3 VOLSTILIT WITHIN EACH PATIENT: IT IS JUST TJE STANDARD DEVIATION
 #################################################
 
 v <- ps %>%
   group_by(encounter_id, difference, summaries ) %>%
   summarise( outcome = first(outcome) , 
              sds = sd(values),
              avgs = mean(values)) %>%
   filter(!is.na(sds)) %>%
   ungroup()
 
 # rank biserials and proportions one is larger  than the othert risk change
 a <- NULL
 for(j in unique(v$difference))
   for(k in unique(v$summaries)){
     a1 <- rank_biserial( sds ~ outcome , 
                          data = v %>% 
                            filter(difference == j & summaries ==  k ) %>%
                            select(outcome , sds) ,
                          ci = .95 ) 
     
     a <- tibble(difference = j , summaries =  k,
                 rb = a1$r_rank_biserial,
                 lrb = a1$CI_low,
                 urb = a1$CI_high ) %>%
       mutate(p = (abs(rb)+1)/2, # ## proportions of pairs between deaths and survivors whenre sdt error is larger for
              l = (abs(lrb)+1)/2, ### deaths to be
              u = (abs(urb)+1)/2) %>%
       rbind(a)
   }
 
 a  <- a %>% 
   rename( rank_biserial = rb,
           l95_rank_biserial = lrb,
           u95_rank_biserial = urb,
           props_death_larger_surv = p,
           l95_props_death_larger_surv = l,
           u95_props_death_larger_surv = u)
 
 
 vs <- vs %>%
   group_by( outcome , difference, summaries ) %>%
   summarise(N = n(),
             avg = mean(sds),
             sdss = sd(sds),
             medians = median(sds),
             q1 = quantile(sds , probs = .25),
             q2 = quantile(sds , probs = .75)) %>%
   ungroup() %>%
   mutate(outcome = if_else(outcome == 1 , "Deaths","Survivors"))  %>% 
   rename(sds = sdss) %>%
   pivot_wider(names_from = outcome,
               values_from = c(N, avg  ,  sds  , medians , q1,q2),
               names_sep = "_")    %>%
   mutate(s = sqrt( ((N_Deaths-1)*sds_Deaths+ (N_Survivors-1)*sds_Survivors) / (N_Deaths  + N_Survivors - 2 ) ),
          cohens_d = (avg_Deaths - avg_Survivors) / s) %>%
   select(-s)
 vs <- vs %>% inner_join(a)
 rm(a)
 write_csv(vs,"summaries_volatilit_simple.csv")
 
 ##plots
 
 v <- v %>%
   mutate(outcome = if_else(outcome > 0 ,"Deaths", "Survivors"),
          difference = paste(difference*3,"-hour",sep=""),
          difference = factor(difference , levels = c("3-hour" , "6-hour" , "9-hour" , "12-hour")))


#########################
 #### 
 # Legend: Variability of the Changes within patients
 ## change the strps to delat (symbol)
 ### 
 #More  legend:
 #  for each lag period (delta), the values for a single patient were collected and a standard deviation was calcuated.  THe plot is a box and whisker plot of the standard deviations obtained from  all  patients in the survivor or  death group
 ### x  axis label:  Standard Deviation of Mortality Risk Changes within Individual Patients
 ### the values of the x axis should  be fully expressed
 #################################
 png("Volatility simple Risk BoxPlots dif.png" ,
     width = 8, height = 4, units = "in",res = 200)
 ggplot(v %>% filter(str_starts(summaries , "d"))  ) +
   geom_boxplot(aes(x = outcome,
                    y = sds),
                outlier.size = rel(0.5)) +
   theme_light() +
   #facet_wrap(max_mins~ summaries, scales = "free", ncol = 2) +
  # facet_wrap( difference ~ . , scales = "free", ncol = 1) +
   facet_grid(difference ~ . ) +
   theme(legend.position = "bottom",legend.title=element_blank()) +
   labs(x = "",
        y = "Stds of Changes Within Patient" ) +
  # scale_fill_colorblind() +
   #scale_y_continuous(trans = "log10") +
   coord_flip()
 dev.off()
 
 
 ggplot(v %>% filter(str_starts(summaries , "d"))  ) +
   # geom_boxplot(aes(x = outcome,
   #                  y = sds),
   #              outlier.size = rel(0.5)) +
   geom_violin(aes(x = outcome,
                   y = sds))+
   theme_light() +
   #facet_wrap(max_mins~ summaries, scales = "free", ncol = 2) +
   # facet_wrap( difference ~ . , scales = "free", ncol = 1) +
   facet_grid(difference ~ . ) +
   theme(legend.position = "bottom",legend.title=element_blank()) +
   labs(x = "",
        y = "Stds of Changes Within Patient" ) +
   # scale_fill_colorblind() +
   scale_y_continuous(trans = "log10") +
   coord_flip()
 
 ############################
# png("Fig 1 Average change Risk within patient BoxPlots dif.png" ,
#     width = 8, height = 4, units = "in",res = 200)
# pdf("Fig 1 Average change Risk within patient BoxPlots dif.pdf" ,
#     width = 8, height = 4 )
 tiff(filename = "Fig 1 Average change Risk within patient BoxPlots dif.tif" ,
          width = 8, height = 4, units = "in", res = 400 , pointsize = 12, compression = "lzw")
 ggplot(v %>% filter(str_starts(summaries , "d"))  ) +
   geom_boxplot(aes(x = outcome,
                    y = avgs),
                outlier.size = rel(0.5)) +
   theme_light() +
   #facet_wrap(max_mins~ summaries, scales = "free", ncol = 2) +
   # facet_wrap( difference ~ . , scales = "free", ncol = 1) +
   facet_grid(difference ~ . ) +
   theme(legend.position = "bottom",
         strip.text = element_text(face = "bold"),
         legend.title=element_blank()) +
   labs(x = "",
        y = "Within Patient Average Risk Change" ) +
   # scale_fill_colorblind() +
   scale_y_continuous(limits = c(-0.15,0.15),
                      breaks = scales::pretty_breaks(n=11)) +
   coord_flip()
 dev.off()
 
 #######
 ## mann whitney u test comparing four times the four classes
 #perform the Mann Whitney U test
 wilcox.test(x = (v %>% filter(summaries == "diff" & outcome == "Survivors" &
                                difference == "12-hour"))$avgs,
             y =(v %>% filter(summaries == "diff" & outcome != "Survivors" &
                                difference == "12-hour"))$avgs,
             paired = F)
 
 
 
 
 ####
 
 
 ## distribution of time periods within patients
 r <- ps %>% filter(str_starts(summaries,"d")) %>%
   group_by(encounter_id,difference) %>%
   summarise(outcome = first(outcome),
             n = n()) %>%
   ungroup() %>%
   group_by(difference,outcome) %>%
   summarise(n_patients = n(),
             q1n = quantile(n,0.25),
             q2n = quantile(n,0.5),
             q3n = quantile(n,0.75)) %>%
   ungroup()  %>%
   mutate(ka = paste(n_patients,", ",q2n,
                     " (",q1n,",",q3n,")",sep="")) %>%
   select(difference,outcome,ka) %>%
   pivot_wider(names_from = outcome,
               values_from = ka)
 r <- r[,c(1,3,2)]
 write_csv(r,"summaries_patients_time_changes.csv")
 
 # 
 # 
 # 
 # ########################################
 # # computing the differences such abs(diff) > epsilon
 # rm(list = ls())
 # gc()
 # load("enviroment.RData")
 # 
 #  ps <- ps %>%
 #   inner_join(ys) %>%
 #   select(-encounter_id , -slice_in_icu) %>%
 #   pivot_longer(c(p1,p2,p3,p4),
 #                names_to = "difference",
 #                values_to = "valor_diff",
 #                values_drop_na = T) %>%
 #   filter(abs(valor_diff) > 10^(-2) ) %>%
 #   mutate(difference = as.integer(str_remove(difference,"p")),
 #          ratio_diff = valor_diff / p) %>%
 #   select(-p)  %>%
 #   rename( diff = valor_diff, ratios = ratio_diff ) %>%
 #   pivot_longer( c(diff , ratios ) , 
 #                 names_to = "summaries",
 #                 values_to = "values") 
 # 
 # ps <- ps %>% group_by(indeces, difference , outcome , summaries ) %>%
 #   summarise(N = n(),
 #             avg = mean(values),
 #             sds = sd(values),
 #             medians = median(values),
 #             q1 = quantile(values , probs = .25),
 #             q2 = quantile(values , probs = .75)) %>%
 #   ungroup() %>%
 #   mutate(outcome = if_else(outcome > 0 ,"Deaths", "Survivors")) %>%
 #   pivot_wider(names_from = outcome,
 #               values_from = c(N, avg  ,  sds , medians , q1,q2),
 #               names_sep = "_")   %>%
 #   mutate(s = sqrt( ((N_Deaths-1)*sds_Deaths+ (N_Survivors-1)*sds_Survivors) / (N_Deaths  + N_Survivors - 2 ) ),
 #          cohens_d = (avg_Deaths - avg_Survivors) / s) %>%
 #   select(-s)
 # 
 # ps <- casos %>%
 #   inner_join(ps) %>%
 #   select(-indeces) %>%
 #   mutate( difference = as.character(difference),
 #           modelo = paste("model",modelo) ,
 #           alphas = paste("alphas",alphas),
 #           gss_modelo = if_else(gss_modelo == 2 , "Uses Mech Ventilation", "No Mech Ventilation")) 
 # 
 # write_csv(ps , "changes_summaries_epsilon.csv")
 # 
 # pdf(paste("Cohens d Overall Changes epsilon.pdf",sep=""),
 #     width=9,height=7)
 # ggplot(ps %>%  filter(   abs(cohens_d) < 5) ) +
 #   geom_point(aes(y = cohens_d ,
 #                  x=paste(modelo,gss_modelo,alphas),
 #                  color = difference)) +
 #   facet_grid(  ~ summaries ,  scales = "free" )  +
 #   theme_light() +
 #   labs(y = "Cohens d",
 #        x = "" ) +
 #   coord_flip()  +
 #   scale_color_colorblind()
 # dev.off()
 # 