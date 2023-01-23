library(keras)
#library(tensorflow)
library(tidyverse)

setwd("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Modeling Deaths/FC3")
rm(list = ls())
gc()

lookback <- 1 ### only > 0
p_hours <- 3 # LENGTH OF SLICE
lambda <- 0.94
step <- 1

#thereshold_death <- 30 ## days until discharged from icu  to be considers 'death'
## this  is to  change the outcome
### in this dataset there are only patients with a unique ICU admission

thereshold_obs <- 11.375 #days, number of days in the icu until after which 
#the time periods ae not  used  anymore 


include_response  =  F

origen  <- "/Users/jlaubach/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Modeling Deaths/FC"

### Loading the data that contains the info of meds, labs and clinical events:
source("~/Dropbox/Documents Mac Work/Creating Data for Neural Networks All/Train Separate Models Mine/Paper 3 Dynamic/Training on Ouput NN/Metrics and losses/Binary Classification.R")

rm(create_model)
create_model <- function(){
  modelo <- keras_model_sequential() %>%
    layer_dense(input_shape = n_covs, 
                units = 2^5,          
                activation = "relu",
                use_bias = T,
                kernel_regularizer = regularizer_l2( l = .02)) %>% 
    layer_dropout(rate = 0.1) %>%
    layer_dense(#input_shape = n_covs,
      units = 2^5,
      activation = "relu",
      use_bias = T,
      kernel_regularizer = regularizer_l2(l = .02)) %>%
    layer_dropout(rate = 0.05) %>%
    layer_dense(#input_shape = n_covs,
      units = 2^5,
      activation = "relu",
      use_bias = T,
      kernel_regularizer = regularizer_l2(l = .02)) %>%
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
a <- i_train$slice_in_icu < thereshold_obs *  24 / p_hours +1
i_train <- i_train[a,]
x_train <- x_train[a,]
y_train <- y_train[a]
i_train <- i_train %>% select(encounter_id,sliced_hour,slice_in_icu)
###########################

#loading the standardized validation
load(paste(origen,"/xs_ys_i_p",p_hours," valid t_0 lookback_",lookback," step_",step,".RData",sep=""))

## keeping  the hours when there is still 11 death patients
##  in  the test set
i_valid <- i_valid %>%left_join(horas)
a <- i_valid$slice_in_icu < thereshold_obs *  24 / p_hours +1
i_valid <- i_valid[a,]
x_valid <- x_valid[a,]
y_valid <- y_valid[a]
i_valid <- i_valid %>% select(encounter_id,sliced_hour,slice_in_icu)
###########################

# #loading the standardized test
load(paste(origen,"/xs_ys_i_p",p_hours," test t_0 lookback_",lookback," step_",step,".RData",sep=""))

##  in  the test set
i_test <- i_test %>%left_join(horas)
a <- i_test$slice_in_icu < thereshold_obs *  24 / p_hours +1
i_test <- i_test[a,]
x_test <- x_test[a,]
y_test <- y_test[a]
i_test <- i_test %>% select(encounter_id,sliced_hour,slice_in_icu)
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
#x_test <- x[i$type_data ==  3,]
x_valid <- x[i$type_data == 2,]
x_train <- x[i$type_data == 1,]
rm(x)

y_train <- y[i$type_data == 1]
y_valid <- y[i$type_data == 2]
#y_test <- y[i$type_data == 3]
rm(y)

i_train  <- i %>% filter(type_data==1) %>% select(-type_data)
i_valid <- i %>% filter(type_data == 2) %>% select(-type_data)
#i_test <- i %>% filter(type_data == 3)%>% select(-type_data)
 rm(i)

 ###################################################################################
## computing weights
pesos <- tibble(in_icu = y_train) %>% group_by(in_icu) %>%
  summarise(ns = n()) %>%
  ungroup() %>%
  mutate(maxs = max(ns),
         w =  maxs/ns)
class_weight <- list(  '0' = pesos$w[pesos$in_icu == 0]^lambda,
                       '1' = pesos$w[pesos$in_icu == 1]^lambda )
rm(pesos)

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

checkpoint_dir <- paste("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Modeling Deaths/FC3/",
                        "checks_o1_",p_hours,"/FC3 lbk_",lookback," l_",lambda,sep="") 

filepath <- file.path(checkpoint_dir,"w.{epoch:03d}-mcc_{val_bin_matthews_correlation:.2f}-{bin_matthews_correlation:.2f}-mcc9_{val_bin_matthews_correlation_09:.2f}-{bin_matthews_correlation_09:.2f}-mcc2_{val_bin_matthews_correlation_02:.2f}-{bin_matthews_correlation_02:.2f}.hdf5")   

callbacks_list <- list(
  callback_model_checkpoint(filepath = filepath ,
                            save_weights_only = TRUE,
                            monitor = "val_bin_matthews_correlation",
                            # monitor = "val_loss",
                            mode = "max",
                            save_best_only = F,
                            save_freq = 'epoch'),
  callback_reduce_lr_on_plateau(monitor = "val_bin_matthews_correlation", ## weighted loss
                                mode = "max", 
                                patience = 30, 
                                factor = 0.1),
  callback_early_stopping(monitor = "val_bin_matthews_correlation",
                          patience = 300,
                          verbose = 1,
                          min_delta = 0.01,
                          mode = "max",
                          restore_best_weights = T),
  callback_csv_logger(filename = paste("~/Dropbox/Documents Mac Work/Creating Data from CN/CNMC Data for Eduardo/Modeling Deaths/FC3/checks_o1_",p_hours,"/",
                                       "FC f_",0," lbk_",lookback," l_",lambda,".csv",sep=""), 
                      separator = ",", append = F)
)  

unlink(checkpoint_dir, recursive = TRUE)
dir.create(checkpoint_dir)

rm(modelo)
modelo <- create_model()


# a <- list.files(checkpoint_dir)
# a <- a[length(a)] ### choosing the model with the highest ...
# modelo <- create_model() %>%
#   load_model_weights_hdf5(filepath = file.path(checkpoint_dir, a))


modelo %>% 
  fit(
    x = x_train, 
    y = y_train, 
    epochs = 500 ,
    initial_epoch = 0,
    view_metrics = F,
    batch_size = nrow(x_valid), 
    class_weight = class_weight,
    validation_data = list(x_valid,
                           y_valid),
    callbacks = callbacks_list ) 

rm(list = ls())
gc()
.rs.restartR()

