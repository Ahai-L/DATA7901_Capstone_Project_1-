library(abind)
library(readr)
library(dplyr)
library(stringr)
spc_cnn <- read.csv("./0_general/spc_train.csv",stringsAsFactors=FALSE)
spc_cnn = spc_cnn[!is.na(spc_cnn$SPC),]
spc_cnn_cluster = read_rds('./7_spc_clustering/cluster_df_onehot.rds')
spc_cnn = left_join(spc_cnn,spc_cnn_cluster,by= c('SPC'='spc'))
spc_cnn$CLUSTER = as.character(spc_cnn$cluster)
spc_cnn= spc_cnn[,!colnames(spc_cnn) =='SPC']
spc_cnn= spc_cnn[,!colnames(spc_cnn) =='cluster']
spc_cnn[spc_cnn=='-']=NA
# data prparation ##################################################################################
# Exchange ordinal data
# exchange one ordinal attribute in a dataframe into numeric 
# spcd on the list in "l"
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
# standardize the column value if sn mn###############
nor <- function(x,c) {
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) %in% c( "double","integer") & !( colnames(x[i]) %in% c)) { # sn and mn
      x[[i]][is.na(x[[i]])]=0
      x[[i]]=(x[[i]]-min(x[[i]]))/(max(x[[i]])-min(x[[i]]))
    }
  }
  return(x)
}
spc_cnn= nor(spc_cnn,c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))# make column value to be [0,1]
#categorical values encoding##########################################################
# keep the first three charactors of "VEG_SPC_CODE"
spc_cnn$VEG_SPEC_CODE = str_extract(spc_cnn$VEG_SPEC_CODE, "^.{3}")

spc_cnn$STATUS[spc_cnn$STATUS %in% c('P','R','O','T')] = NA

spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('QS','S')] = 'Siliceous Upper'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('GS','MS','MU','PL','PH','PO','ST',
                              'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('AC','FC','FS','IS')] = 'Sesquioxide'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('CO','CC')] = 'Organic'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('AH','GY','HA')] = 'Evaporite'
spc_cnn$OBS_LITH_CODE[spc_cnn$OBS_LITH_CODE %in% 
                            c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# $OCF_LITH_CODE
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('QS','S')] = 'Siliceous Upper'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('GS','MS','MU','PL','PH','PO','ST',
                              'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('AC','FC','FS','IS')] = 'Sesquioxide'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('CO','CC')] = 'Organic'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('AH','GY','HA')] = 'Evaporite'
spc_cnn$OCF_LITH_CODE[spc_cnn$OCF_LITH_CODE %in% 
                            c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'

spc_cnn$REL_MOD_SLOPE_CLASS = ex_ordinal(spc_cnn$REL_MOD_SLOPE_CLASS,
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

spc_cnn$TEXTURE_CODE[spc_cnn$TEXTURE_CODE %in% 
                           c('S','LS','CS','SL','SCL','CFS','CKS','FS','FSCL',
                             'FSCLZ','FSL','FSLZ','KS','FSCL','KSL','KSS','LFS',
                             'LFSY','LFSYZ','LKS','LMS','LSY','MS','SCFLS','SLZ','SS','ST',
                             'KSCL','SCLFS')] = 'Sandy'
spc_cnn$TEXTURE_CODE[spc_cnn$TEXTURE_CODE %in% 
                           c('L','ZL','ZCL','CLFS','CL','CLS','CLFSZ','CLKS','CLMS','CLZ','ZCL')] = 'Loamy'
spc_cnn$TEXTURE_CODE[spc_cnn$TEXTURE_CODE %in% 
                           c('C','LC','LMC','MC','MHC','HC','CSC','FSHC','FSLC','FSLCZ','FSLMC',
                             'FSMC','FSMHC','KSHC','KSLMC','KSMC','KSMHC','LCFS',
                             'LCKS','LCS','LMCFS','LMCKS','LMCS','LMCZ','LMCKS',
                             'LMCS','MCFS','MCS','MCZ','MHCFS','MHCS','MSC','SC','SHC',
                             'SLC','SLMC','SMC','SMHC','ZC','ZHC','ZLC','ZLCFS','ZLMC','ZLMCS',
                             'ZMC','ZMHC','LCZ','FSC','KSC','KSLC')] = 'Clayey'
spc_cnn$TEXTURE_CODE[spc_cnn$TEXTURE_CODE %in% 
                           c('AP','CP','GP','GR','HP','IP','LP','SP')] = 'Organic'
sort(unique(spc_cnn$TEXTURE_CODE))
spc_cnn$MOTT_TYPE[!spc_cnn$MOTT_TYPE =='M'] = NA
spc_cnn$MOTT_TYPE = ex_ordinal(spc_cnn$MOTT_TYPE,
                                   list(c('M'),1))
#spc_cnn$MOTT_TYPE = as.integer(spc_cnn$MOTT_TYPE)
spc_cnn$PEDALITY_GRADE = ex_ordinal(spc_cnn$PEDALITY_GRADE,
                                        list(c('G','V','W','M','S'),1:5))

spc_cnn[spc_cnn$NATURE %in% c('U','O'),'NATURE'] = NA
spc_cnn$BOUND_DISTINCT[spc_cnn$BOUND_DISTINCT %in% c('S','A','C')] = "GRP1"

spc_cnn$BOUND_DISTINCT[spc_cnn$BOUND_DISTINCT %in% c('G','D')] = "GRP2"

# categorical to numeric factor#####################################
cat_to_numFac <- function(x,c) {
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) == "character" & !( colnames(x[i]) %in% c)) { # sc and mc
      x[[i]][is.na(x[[i]])] = "0"
      x[[i]] <- factor(x[[i]])
      levels(x[[i]]) <- 0: (length(levels(x[[i]]))-1)
      x[[i]] = as.numeric(as.character(x[[i]]))
    }
  }
  return(x)
}
spc_cnn <- cat_to_numFac(spc_cnn,c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO",
                                       'ASC_ORD','SUBORD_ASC_CODE','GREAT_GROUP_ASC_CODE',
                                       'SUBGROUP_ASC_CODE','CLUSTER'))

# One hot encoding #########################################################################
# one hot encoding for all character type attributes in dataframe x,
# except the attributes with the index number in vector c,
# and delete the original attributes afterwards
# inputs: datafram x, int vector c (exceptional  attributes)
# output: one hot encoding completed dataframe
# exclude 0 feature values as a column
cnn_onehot_exZero <- function(x) {
  y = x
  for (i in 1 : length(x)) {
      for(unique_value in unique(x)){
        if (!is.na(unique_value)) {
          y[paste('CLUSTER',unique_value, sep = "_")] <- ifelse(x[i] == unique_value, 1, 0)
        }
      }
    print(i)
      #y = y[,-1]
    }
  return (y)
}
#spc_cnn = cnn_onehot_exZero(spc_cnn, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))
# NA to 0
spc_cnn[is.na(spc_cnn)]=0
# Fix horizons to be k2=8#############################
k2 = 8
spc_cnn = spc_cnn[spc_cnn$HORIZON_NO<=k2,] # drop all rows with "HORIZON_NO" > K2(5)
sn_sc_idx = grep('STATUS|OBS_LITH_CODE|REL_MOD_SLOP_CLASS|VEG_SPEC_CODE|CLUSTER',
                 colnames(spc_cnn))

#split data into a big list group by project code and site id
spc_cnn <- split(spc_cnn,list(spc_cnn$PROJECT_CODE,spc_cnn$SITE_ID),drop = T)
#write_rds(spc_cnn,'./6_spc_cnn/spc_cnn_aftSplit.rds')
# add  empty rows for those "HORIZON_NO" < K2(5)##############
for ( i in 1: length(spc_cnn)) {
  n2 = nrow(spc_cnn[[i]])
  if (n2<k2) {
    spc_cnn[[i]][(n2+1):k2,c(1:2,sn_sc_idx)] = spc_cnn[[i]][1,c(1:2,sn_sc_idx)]
    spc_cnn[[i]][(n2+1):k2,'HORIZON_NO'] = (n2+1):k2
    spc_cnn[[i]][(n2+1):k2,-c(1:3,sn_sc_idx)] = 0
  }
  print(i)
}

# it is time to drop 4 columns 
spc_cnn <- lapply(spc_cnn, function(x) x[!(names(x) %in% c("PROJECT_CODE", "SITE_ID","OBS_NO","HORIZON_NO"))])
write_rds(spc_cnn,'./6_spc_cnn/spc_cnn_aftSplit.rds')
# convert to a 3D array: [data index, horizons, features]##############################
spc_cnn = readRDS('./6_spc_cnn/spc_cnn_aftSplit.rds')
spc_cnn <- abind(spc_cnn,along = 0)
#rename dim(2)
dimnames(spc_cnn)[[2]] <- 1:8
write_rds(spc_cnn,'./6_spc_cnn/spc_cnn_3darray.rds')
# sampling###################################################
spc_cnn=read_rds('./6_spc_cnn/spc_cnn_3darray.rds')
library(keras)
spc_cnn = spc_cnn[sample(nrow(spc_cnn),nrow(spc_cnn)),,]
#option two: prune only###########################
cnn_prune_spc <- function(x,features,s=200) {
  domains = unique(x[,1,features])
  l=c()
  for (i in domains) {
    count =  nrow(x[x[,1,features]==i,,])
    if (count < s) {
      l=c(l,i)
      #x = x[x[,1,i]==0,,]
    } 
  }
  return(x[!x[,1,features] %in% l,,])
}
spc_cnn1 = cnn_prune(spc_cnn,"CLUSTER")
unique(spc_cnn1[,1,"CLUSTER"])
write_rds(spc_cnn1,'./6_spc_cnn/spc_cnn_prune.rds')
# only prune but not scaling
spc_cnn1 = spc_cnn1[sample(nrow(spc_cnn1),nrow(spc_cnn1)),,]
tr_rate=0.85
idx = sample(nrow(spc_cnn1),round(nrow(spc_cnn1)*tr_rate),replace = F)
spc_cnn_tr= spc_cnn1[idx,,]
spc_cnn_test= spc_cnn1[-idx,,]

#extract different input and tag list#########################

spc_tag_list = grep('CLUSTER',dimnames(spc_cnn_tr)[[3]])
#spc_list = 1:(spc_tag_list[1]-1)

# generate 4d(input #, horizons, features, channels) data
library(keras)
library(abind)
k2=8
for (i in dimnames(spc_cnn_tr)[[3]][1:14]) {
  assign(paste(i,"TR",sep = '_'),asub(spc_cnn_tr,i,3))
  assign(paste(i,"TEST",sep = '_'),asub(spc_cnn_test,i,3))
} 

spc_tag_tr = asub(spc_cnn_tr,list(1,spc_tag_list),c(2,3)) %>%
  cnn_onehot_exZero(.)

spc_tag_test = asub(spc_cnn_test,list(1,spc_tag_list),c(2,3))
# model construction ###############################
spc_in <- layer_input(shape=c(8,length(spc_list),1))

#spc_con <- layer_concatenate(list(spc_in,spc_ex_in),axis=2)
spc_model <- spc_in %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3 ), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(1, 2)) #%>%
# layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
#               padding = 'same') %>%
# layer_max_pooling_2d(pool_size = c(1, 2))

spc_model

spc_out <- spc_model %>%
  layer_flatten() %>%
  #layer_dropout(rate = 0.5) %>%
  layer_dense(units =256, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(spc_tag_list), activation = "softmax")
spc_out

model <- keras_model(spc_in,spc_out)
model
model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = "acc"
)
model
model %>% save_model_hdf5('./6_spc_cnn/spc_model.h5')
# tensorboard
# dir.create("./2_spc_cnn/tensorlog")
# tensorboard ("tensorlog")
# callbacks = list(callback_tensorboard(
#   log_dir = "./2_spc_cnn/tensorlog"))
#   #histogram_freq = 1,
#   #embeddings_freq = 1))

# model fitting and predicting, confusion matrix###############
history = model %>% fit(spc_in_tr,spc_tag_tr,
                        epochs = 10, batch_size =64,validation_split =0.15)#,callbacks=callbacks)

# fitting with best epcho, remember to reset the model
history = model %>% fit(spc_in_tr,spc_tag_tr,
                        epochs = 4, batch_size =64)#,validation_split =0.15)#,callbacks=callbacks)


result = model %>% evaluate(spc_in_test,spc_tag_test)
spc_predict <- model %>% predict(spc_in_test)
colnames(spc_predict) = colnames(spc_tag_test)
rownames(spc_predict) = rownames(spc_tag_test)

spc_predict = as.data.frame(spc_predict)
spc_predict = cbind(colnames(spc_predict)[apply(spc_predict,1,which.max)],
                    colnames(spc_tag_test)[apply(spc_tag_test,1,which.max)], 
                    spc_predict)
colnames(spc_predict)[1:2]=c('PREDICT','REAL')
library(caret)
spc_confusion=confusionMatrix(spc_predict[,1],spc_predict[,2])
#spc_confusion[["table"]]=spc_confusion[["table"]][c(1,9,10,14,6,8,12,3,2,5,4,7,11,13),c(1,9,10,14,6,8,12,3,2,5,4,7,11,13)]
#spc_confusion$byClass=spc_confusion$byClass[c(1,9,10,14,6,8,12,3,2,5,4,7,11,13),]
spc_confusion

