library(keras)
#library(tensorflow)
library(tidyverse)

setwd("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Modeling Deaths/FC3")
rm(list = ls())
gc()


p_hours <- 3 # LENGTH OF SLICE
step <- 1


#thereshold_death <- 30 ## days until discharged from icu  to be considers 'death'
## this  is to  change the outcome
### in this dataset there are only patients with a unique ICU admission

thereshold_obs <- 11.375 #days, number of days in the icu until after which 
#the time periods ae not  used  anymore 

include_response  =  F

origen  <- "C:/Users/etrujillor/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Modeling Deaths/FC"

### Loading the data that contains the info of meds, labs and clinical events:
source("~/Dropbox/Documents Mac Work/Creating Data for Neural Networks All/Train Separate Models Mine/Paper 3 Dynamic/Training on Ouput NN/Metrics and losses/Binary Classification.R")

create_model <- function(){
  modelo <- keras_model_sequential() %>%
    layer_dense(input_shape = n_covs, 
                units = 2^5,          
                activation = "relu",
                use_bias = T,
                kernel_regularizer = regularizer_l2( l = .05)) %>% 
    layer_dropout(rate = 0.1) %>%
    layer_dense(#input_shape = n_covs,
      units = 2^5,
      activation = "relu",
      use_bias = T,
      kernel_regularizer = regularizer_l2(l = .05)) %>%
    layer_dropout(rate = 0.05) %>%
    layer_dense(#input_shape = n_covs,
      units = 2^5,
      activation = "relu",
      use_bias = T,
      kernel_regularizer = regularizer_l2(l = .05)) %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  
  # COmpiling the model:
  modelo %>% compile(
    optimizer = optimizer_rmsprop(lr = 0.0002),
    loss = 'binary_crossentropy',
    metrics = c( metric_bin_mcc , 
                 metric_bin_mcc_09,
                 metric_bin_mcc_02,
                 metric_sensitivity,
                 metric_precision,
                 metric_specificity,
                 metric_neg_pred_val,
                 metric_sensitivity_09,
                 metric_sensitivity_02,
                 metric_precision_09,
                 metric_precision_02,
                 metric_specificity_09,
                 metric_specificity_02,
                 metric_neg_pred_val_09,
                 metric_neg_pred_val_02,
                 "accuracy" ) )
  modelo
}

for(lookback in c(1,2,3,4)){
  
  load(paste(origen,"/hours_in_icu p_",p_hours," lkb_",lookback,
             " steps_",step,".RData",sep="")) #horas
  
  horas <- horas %>%
    select(encounter_id,sliced_hour,slice_in_icu  )
  #######################
  ##loading the standardized train set:
  load(paste(origen , "/xs_ys_i_p",p_hours," train t_0 lookback_",lookback," step_",step,".RData",sep=""))
  
  ## keeping  the hours when there is still 5 death patients
  ##  in  the test set
  i_train <- i_train %>%left_join(horas)
  #a <- i_train$slice_in_icu < thereshold_obs *  24 / p_hours +1
  #i_train <- i_train[a,]
  #x_train <- x_train[a,]
  #y_train <- y_train[a]
  i_train <- i_train %>%
    select(encounter_id,sliced_hour,slice_in_icu)  %>%
    mutate(within_trained_time = (slice_in_icu < thereshold_obs *  24 / p_hours +1))
  ###########################
  
  ######################################################
  #loading the standardized validation
  load(paste(origen,"/xs_ys_i_p",p_hours," valid t_0 lookback_",lookback," step_",step,".RData",sep=""))
  
  ## keeping  the hours when there is still 5 death patients
  ##  in  the test set
  i_valid <- i_valid %>%left_join(horas)
 # a <- i_valid$slice_in_icu < thereshold_obs *  24 / p_hours +1
#  i_valid <- i_valid[a,]
 # x_valid <- x_valid[a,]
#  y_valid <- y_valid[a]
  i_valid <- i_valid %>% select(encounter_id,sliced_hour,slice_in_icu) %>%
    mutate(within_trained_time = (slice_in_icu < thereshold_obs *  24 / p_hours +1))
  ###########################
  
  # #loading the standardized test
  load(paste(origen,"/xs_ys_i_p",p_hours," test t_0 lookback_",lookback," step_",step,".RData",sep=""))
  
  ##  in  the test set
  i_test <- i_test %>%left_join(horas)
 # a <- i_test$slice_in_icu < thereshold_obs *  24 / p_hours +1
  #i_test <- i_test[a,]
  #x_test <- x_test[a,]
  #y_test <- y_test[a]
  i_test <- i_test %>% select(encounter_id,sliced_hour,slice_in_icu) %>%
    mutate(within_trained_time = (slice_in_icu < thereshold_obs *  24 / p_hours +1))
  
  ###########################
  
  x_test <- x_test[,colnames(x_train)]
  x_valid <- x_valid[,colnames(x_train)]
  
  #############################################################################
  #########re arranging the patients for trainig, validation, testing
  ###################################
  load("/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Modeling Deaths/FC2/new_randomization.RData")  #es
  
  x <- rbind(x_train,x_valid,x_test)
  rm(x_train,x_valid,x_test)
  
  y <- c(y_train,y_valid,y_test)
  rm(y_train,y_valid,y_test)
  
  i <- rbind(i_train,i_valid,i_test)
  rm(i_train,i_valid,i_test)
  
  i <- i %>% left_join(es)
  rm(es)
  ####
  x_test <- x[i$type_data ==  3,]
  x_valid <- x[i$type_data == 2,]
  x_train <- x[i$type_data == 1,]
  rm(x)
  
  y_train <- y[i$type_data == 1]
  y_valid <- y[i$type_data == 2]
  y_test <- y[i$type_data == 3]
  rm(y)
  
  i_train  <- i %>% filter(type_data==1) %>% select(-type_data)
  i_valid <- i %>% filter(type_data == 2) %>% select(-type_data)
  i_test <- i %>% filter(type_data == 3)%>% select(-type_data)
  rm(i)
  
##################################
  
  n_covs <- x_train %>% ncol()
  ###################
  gc()
  #######################################
  # Modeling with a sequantiel model
  # The input_shape argument to the first layer specifies 
  # the shape of the input data (a length ncol(x_test) numeric 
  #  vector representing lab and med data, and age). 
  # The final layer outputs a length 3 numeric vector (probabilities for 
  # beaing ICU, no ICU or transition) using a softmax activation function.
  
  for(lambda in c(0.94,1,1.05,1.1)){
    checkpoint_dir <- paste("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Modeling Deaths/FC3/",
                            "checks_o1_",p_hours,"/FC3 lbk_",lookback," l_",lambda,sep="") 
    
    filepath <- file.path(checkpoint_dir,"w.{epoch:03d}-mcc_{val_bin_matthews_correlation:.2f}-{bin_matthews_correlation:.2f}-mcc9_{val_bin_matthews_correlation_09:.2f}-{bin_matthews_correlation_09:.2f}-mcc2_{val_bin_matthews_correlation_02:.2f}-{bin_matthews_correlation_02:.2f}.hdf5")   
    
    
    a <- list.files(checkpoint_dir)
    a <- a[length(a)] ### choosing the model with the highest ...
    modelo <- create_model() %>%
      load_model_weights_hdf5(filepath = file.path(checkpoint_dir, a))
    ###############################################################
    
    prob <- modelo %>% predict(x_train)
    prob_val <- modelo %>% predict(x_valid)
    prob_test <- modelo %>% predict(x_test)
    
    prob_test <- i_test %>%
      mutate(ps = prob_test[,1],
             outcome = y_test,
             type_data = 3)
    
    prob_val <- i_valid %>%
      mutate(ps = prob_val[,1],
             outcome = y_valid,
             type_data = 2)
    
    prob <- i_train %>%
      mutate(ps = prob[,1],
             outcome = y_train,
             type_data = 1)
    
    pss <- rbind(prob_val,prob_test,prob)
    rm(prob_val,prob_test,prob)
    
    
    
    save(pss , file = paste("Scores_o1/",
                            "Preds FC p",p_hours,
                            " lbk_",lookback,
                            " step_",step,
                            " l_",lambda,".RData",sep=""))
    rm(pss)
    gc()
    
  }
  
  
}
