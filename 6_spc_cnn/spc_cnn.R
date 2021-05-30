# Library loading ################################
library(stringr)
library(dplyr)
library(abind)
library(readr)

# load data from file##########################
# it is a horizon level file
spc_cnn_dat <- read.csv("./0_general/spc_train.csv",stringsAsFactors=FALSE)
spc_cnn_dat = spc_cnn_dat[!is.na(spc_cnn_dat$SPC),]
spc_cnn_cluster = read_rds('./7_spc_clustering/cluster_df_onehot.rds')
spc_cnn_dat = left_join(spc_cnn_dat,spc_cnn_cluster,by= c('SPC'='spc'))
spc_cnn_dat$CLUSTER = as.character(spc_cnn_dat$cluster)
spc_cnn_dat= spc_cnn_dat[,!colnames(spc_cnn_dat) =='SPC']
spc_cnn_dat= spc_cnn_dat[,!colnames(spc_cnn_dat) =='cluster']
# data prparation ##################################################################################
# Exchange ordinal data
# exchange one ordinal attribute in a dataframe into numeric 
# based on the list in "l"
# input: x: single attribute of a dataframe, e.g.: A$a
#        l: list of unique attribute values and their replace number
#            example of list "l" : list(c('a','b','c'), c(1,2,3))
#        na: na: the value for NA, default as 0
# output: the numeric attribute
ex_ordinal <- function(x,l,na = 0) {
  if (length(l[[1]]) == length(l[[2]])) {
    x[is.na(x)] = na
    for (i in 1 : length(l[[1]])) {
      x[x==l[[1]][i]] = l[[2]][i]
    }
    return(as.integer(x))
  } else { print('Error, the length of the list not matched.')}
}
# only keep the first three charactors of "VEG_SPC_CODE"
spc_cnn_dat$VEG_SPEC_CODE = str_extract(spc_cnn_dat$VEG_SPEC_CODE, "^.{3}")

spc_cnn_dat$STATUS[spc_cnn_dat$STATUS %in% c('P','R','O','T')] = NA

spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('QS','S')] = 'Siliceous Upper'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('GS','MS','MU','PL','PH','PO','ST',
                                'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('AC','FC','FS','IS')] = 'Sesquioxide'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('CO','CC')] = 'Organic'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('AH','GY','HA')] = 'Evaporite'
spc_cnn_dat$OBS_LITH_CODE[spc_cnn_dat$OBS_LITH_CODE %in% 
                              c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# $OCF_LITH_CODE
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('QS','S')] = 'Siliceous Upper'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('GS','MS','MU','PL','PH','PO','ST',
                                'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('AC','FC','FS','IS')] = 'Sesquioxide'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('CO','CC')] = 'Organic'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('AH','GY','HA')] = 'Evaporite'
spc_cnn_dat$OCF_LITH_CODE[spc_cnn_dat$OCF_LITH_CODE %in% 
                              c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'

spc_cnn_dat$REL_MOD_SLOPE_CLASS = ex_ordinal(spc_cnn_dat$REL_MOD_SLOPE_CLASS,
                                               list(c('LP','GP','UP','RP','B1','B2','B3',
                                                      'GR','UR','RR','SR','B4','B5',
                                                      'UL','RL','SL','VL','B6',
                                                      'UH','RH','SH','VH','PH',
                                                      'RM','SM','VM','PM'),
                                                    c(0.02,0.1,0.3,1,2,3.5,7.5,
                                                      0.3,0.9,3,6,10.5,22.5,
                                                      3,10,20,35,75,
                                                      9,30,60,105,225,
                                                      100,200,350,750)))

spc_cnn_dat$TEXTURE_CODE[spc_cnn_dat$TEXTURE_CODE %in% 
                             c('S','LS','CS','SL','SCL','CFS','CKS','FS','FSCL',
                               'FSCLZ','FSL','FSLZ','KS','FSCL','KSL','KSS','LFS',
                               'LFSY','LFSYZ','LKS','LMS','LSY','MS','SCFLS','SLZ','SS','ST',
                               'KSCL','SCLFS')] = 'Sandy'
spc_cnn_dat$TEXTURE_CODE[spc_cnn_dat$TEXTURE_CODE %in% 
                             c('L','ZL','ZCL','CLFS','CL','CLS','CLFSZ','CLKS','CLMS','CLZ','ZCL')] = 'Loamy'
spc_cnn_dat$TEXTURE_CODE[spc_cnn_dat$TEXTURE_CODE %in% 
                             c('C','LC','LMC','MC','MHC','HC','CSC','FSHC','FSLC','FSLCZ','FSLMC',
                               'FSMC','FSMHC','KSHC','KSLMC','KSMC','KSMHC','LCFS',
                               'LCKS','LCS','LMCFS','LMCKS','LMCS','LMCZ','LMCKS',
                               'LMCS','MCFS','MCS','MCZ','MHCFS','MHCS','MSC','SC','SHC',
                               'SLC','SLMC','SMC','SMHC','ZC','ZHC','ZLC','ZLCFS','ZLMC','ZLMCS',
                               'ZMC','ZMHC','LCZ','FSC','KSC','KSLC')] = 'Clayey'
spc_cnn_dat$TEXTURE_CODE[spc_cnn_dat$TEXTURE_CODE %in% 
                             c('AP','CP','GP','GR','HP','IP','LP','SP')] = 'Organic'
sort(unique(spc_cnn_dat$TEXTURE_CODE))
spc_cnn_dat$MOTT_TYPE[!spc_cnn_dat$MOTT_TYPE =='M'] = NA
spc_cnn_dat$MOTT_TYPE = ex_ordinal(spc_cnn_dat$MOTT_TYPE,
                                        list(c('M'),1))
#spc_cnn_dat$MOTT_TYPE = as.integer(spc_cnn_dat$MOTT_TYPE)
spc_cnn_dat$PEDALITY_GRADE = ex_ordinal(spc_cnn_dat$PEDALITY_GRADE,
                                          list(c('G','V','W','M','S'),1:5))

spc_cnn_dat[spc_cnn_dat$NATURE %in% c('U','O'),'NATURE'] = NA
spc_cnn_dat$BOUND_DISTINCT[spc_cnn_dat$BOUND_DISTINCT %in% c('S','A','C')] = "GRP1"

spc_cnn_dat$BOUND_DISTINCT[spc_cnn_dat$BOUND_DISTINCT %in% c('G','D')] = "GRP2"

# data reorganization #############################################
# check "sn" attributes
#spc_cnn_dat$DRAINAGE = as.integer(spc_cnn_dat$DRAINAGE)

# One hot encoding
# one hot encoding for all character type attributes in dataframe x,
# except the attributes with the order number in vector c, 
# and delete the original attributes afterwards
# inputs: datafram x, int vector c (exceptional  attributes)
# output: one hot encoding completed dataframe
cnn_onehot <- function(x,c) {
  y = x
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) == "character" & !( colnames(x[i]) %in% c)) { # sc
      for(unique_value in unique(x[[i]])){
        if (is.na(unique_value)) {
          y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(is.na(x[[i]]), 1, 0)
        } 
        else { 
          y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(x[[i]] == unique_value, 1, 0)
          #y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(unique_value %in% x[[i]], 1, 0)
        }
      }
      y = y[ , !(names(y) %in% c(names(x[i])))]
    }
  }
  return (y)
}
spc_cnn_dat = cnn_onehot(spc_cnn_dat, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))
spc_cnn_dat[is.na(spc_cnn_dat)]=0
spc_cnn_dat = spc_cnn_dat[,-c(3)] # drop "OBS_NO"
# a = scale(spc_cnn_dat[,-c(1:3)])
# make every data to be same horizon levels
k2 = 8
spc_cnn_dat = spc_cnn_dat[spc_cnn_dat$HORIZON_NO<=k2,] # drop all rows with "HORIZON_NO" > K2(5)
# # a function to standardize the column value
# nor <- function(x) { 
#   return((x-min(x))/(max(x)-min(x)))
# }
# spc_cnn_dat[,-c(1:3)]= apply(spc_cnn_dat[,-c(1:3)],2,nor) # make column value to be [0,1]
write.csv(spc_cnn_dat,'./6_spc_cnn/spc_cnn_bf_split.csv',row.names = F)
write_rds(spc_cnn_dat,'./6_spc_cnn/spc_cnn_bf_split.rds')
# spc_cnn_dat = read.csv('./6_spc_cnn/spc_cnn_bf_split.csv',stringsAsFactors =F)

# split data into a list of set of data point grouped by project and stie id###########
spc_cnn_dat <- split(spc_cnn_dat,list(spc_cnn_dat$PROJECT_CODE,spc_cnn_dat$SITE_ID),drop = T)

# add  empty rows for those "HORIZON_NO" < K2(5)
for ( i in 1: length(spc_cnn_dat)) {
  n2 = nrow(spc_cnn_dat[[i]])
  if (n2<k2) {
    spc_cnn_dat[[i]][(n2+1):k2,1:2] = spc_cnn_dat[[i]][1,1:2]
    spc_cnn_dat[[i]][(n2+1):k2,'HORIZON_NO'] = (n2+1):k2
    spc_cnn_dat[[i]][(n2+1):k2,-c(1:3)] = 0 
  }
  print(i)
}
# it is time to drop 3 columns 
spc_cnn_dat <- lapply(spc_cnn_dat, function(x) x[!(names(x) %in% c("PROJECT_CODE", "SITE_ID","HORIZON_NO"))])
write_rds(spc_cnn_dat,'./6_spc_cnn/spc_cnn_bf_abind.rds')
# spc_cnn_dat = read.csv('./6_spc_cnn/spc_cnn_bf_abind.csv',stringsAsFactors =F)

# convert to a 3D array: [data index, horizons, features]##############################
# start_time = Sys.time()
spc_cnn_dat1 <- abind(spc_cnn_dat,along = 0)
dimnames(spc_cnn_dat1)[[2]] <- row.names(spc_cnn_dat[[1]])
write_rds(spc_cnn_dat1,'./6_spc_cnn/spc_cnn_3darray1.rds')

# sampling###################################################
spc_cnn_dat1 = readRDS('./6_spc_cnn/spc_cnn_3darray1.rds')
class = dimnames(spc_cnn_dat1)[[3]][grep('cluster',dimnames(spc_cnn_dat1)[[3]])]
spc_cnn_dat2 = array(numeric(),c(0,8,351));spc_cnn_dat3 = array(numeric(),c(0,8,351)) 
size = 150
for (c in class) { 
  temp =  spc_cnn_dat1[spc_cnn_dat1[,1,c]==1,,]
  idx = sample(nrow(temp),round(nrow(temp)*0.7),replace = F)
  if (length(dim(temp))>2) {
    spc_cnn_dat3 = abind(spc_cnn_dat3,temp[-idx,,],along = 1)
    if (nrow(temp) >= size) {
      spc_cnn_dat2 = abind(spc_cnn_dat2, temp[idx,,],along = 1)
    } else {
      spc_cnn_dat2 = abind(spc_cnn_dat2, 
                           temp[sample(idx,round(size*0.7),replace = T),,],along = 1)
    }
  } else {
    temp = array(temp,dim = c(1,nrow(temp),ncol(temp)))
    spc_cnn_dat2 = abind(spc_cnn_dat2,
                         temp[sample(1,round(size*0.7),replace = T),,],along = 1)
    spc_cnn_dat3 = abind(spc_cnn_dat3,temp,along = 1)
  }
  print(paste(c,nrow(temp)))
}
# shuffling the dataset
spc_cnn_dat2 = spc_cnn_dat2[sample(nrow(spc_cnn_dat2),nrow(spc_cnn_dat2)),,]
spc_cnn_dat3 = spc_cnn_dat3[sample(nrow(spc_cnn_dat3),nrow(spc_cnn_dat3)),,]

library(keras)
install_keras(tensorflow = 'gpu')
k2=8
spc_cnn_train <- array_reshape(asub(spc_cnn_dat2, -c(234:351),3), c(nrow(spc_cnn_dat2), k2, 233, 1))
spc_cnn_test <- array_reshape(asub(spc_cnn_dat3, -c(234:351),3), c(nrow(spc_cnn_dat3), k2, 233, 1))
spc_cnn_tr_tag = asub(spc_cnn_dat2,list(1,c(234:351)),c(2,3))
spc_cnn_test_tag = asub(spc_cnn_dat3,list(1,c(234:351)),c(2,3))
write_rds(spc_cnn_train,'./6_spc_cnn/spc_cnn_train.rds')
write_rds(spc_cnn_test,'./6_spc_cnn/spc_cnn_test.rds')
write_rds(spc_cnn_tr_tag,'./6_spc_cnn/spc_cnn_tr_tag.rds')
write_rds(spc_cnn_test_tag,'./6_spc_cnn/spc_cnn_test_tag.rds')

# cnn with validation
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3 ), activation = "relu",
                input_shape = c(k2, 233, 1),padding = 'same'
  ) %>% #padding = 'same'   kernel_regularizer = regularizer_l2(0.001)
  #layer_dropout(rate = 0.2) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",padding = 'same'
  ) %>%
  #layer_dropout(rate = 0.2) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",padding = 'same'
  ) %>%
  #layer_dropout(rate = 0.2) %>%
  layer_max_pooling_2d(pool_size = c(1, 2)) #%>%
# layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",padding = 'same') %>%
# layer_max_pooling_2d(pool_size = c(1, 2)) %>%
# layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") 

model
model <- model %>%
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 256, activation = "relu",kernel_regularizer = regularizer_l2(0.001)) %>%
  
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = 118, activation = "softmax")
model

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(spc_cnn_train, spc_cnn_tr_tag, epochs = 20, batch_size =32,
                         validation_split =0.2)# validation_data=list(cnn_val,cnn_val_tag))

# cnn with epoch=4
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(2,3 ), activation = "relu",
                input_shape = c(k2, 377, 1)) %>% #padding = 'same'
  layer_max_pooling_2d(pool_size = c(1, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(2, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(1, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(2, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(1, 2)) #%>%
# layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",padding = 'same') %>%
# layer_max_pooling_2d(pool_size = c(1, 2)) %>%
# layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") 

model
model <- model %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = 13, activation = "softmax")
model
model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
#cnn_train_n <- abind(cnn_train,cnn_val,along = 1)
#cnn_tr_tag_n <- rbind(cnn_tr_tag,cnn_val_tag)
history <- model %>% fit(cnn_train, cnn_tr_tag, epochs = 17, batch_size =32)

results <- model %>% evaluate(spc_cnn_test, spc_cnn_test_tag)

spc_cnn_predict <- model %>% predict(spc_cnn_test)
colnames(spc_cnn_predict) = colnames(spc_cnn_test_tag)
rownames(spc_cnn_predict) = rownames(spc_cnn_test_tag)
spc_cnn_predict = as.data.frame(spc_cnn_predict)
spc_cnn_predict = cbind(colnames(spc_cnn_predict)[apply(spc_cnn_predict,1,which.max)],
                    colnames(spc_cnn_test_tag)[apply(spc_cnn_test_tag,1,which.max)], 
                    spc_cnn_predict)
colnames(spc_cnn_predict)[1:2]=c('PREDICT','REAL')
library(caret)
confusionMatrix(spc_cnn_predict[,1],spc_cnn_predict[,2])
# library(nnet)
# cnn_predict1 = cnn_predict
# for ( i in 1: nrow(cnn_predict)) {
#   cnn_predict[i,which.is.max(cnn_predict[i,])] = 1
#   cnn_predict[i,-which.is.max(cnn_predict[i,])] = 0
# }
model %>% save_model_hdf5("./2_asc_cnn/asc_cnn.h5")
#test <- load_model_hdf5("cnn_ASC.h5")

library(OpenImageR)
writeImage(cnn_train[1,,,],'test_image.jpg')
test=image_load(
  'test_image.jpg',
  grayscale = T,
  target_size = NULL,
  interpolation = "nearest"
)
a = image_to_array(test, datwriteImage(cnn_train[1,,,],'test_image.jpg'))



library(tensorflow)
#install_tensorflow(version = 'gpu')
library(keras)
install_keras(tensorflow = 'gpu')
library(abind)
