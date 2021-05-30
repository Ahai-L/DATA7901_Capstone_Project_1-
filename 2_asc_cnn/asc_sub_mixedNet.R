library(abind)
library(readr)
asc_sub = readRDS('./0_general/asc_sub.rds')
#asc_sub[is.na(asc_sub)]=0 do this later, after one hot encoding
asc_sub[asc_sub=='-']=NA
# check "sn" attributes############################################
asc_sub$DRAINAGE = as.integer(asc_sub$DRAINAGE)
# Exchange ordinal data#######################################################################
# exchange one ordinal attribute in a dataframe into numeric 
# based on the list in "l"
# input: x: single attribute of a dataframe, e.g.: A$a
#        l: list of unique attribute values and their replace number
#            example of list "l" : list(c('a','b','c'), c(1,2,3))
#        na: na: the value for NA, default as 0
# output: the numeric attribute
ex_ordinal <- function(x,l,na = NA) {
  if (length(l[[1]]) == length(l[[2]])) {
    x[is.na(x)] = na
    for (i in 1 : length(l[[1]])) {
      x[x==l[[1]][i]] = l[[2]][i]
    }
    return(as.integer(x))
  } else { print('Error, the length of the list not matched.')}
}
asc_sub$SOIL_WATER_STAT = ex_ordinal(asc_sub$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
asc_sub$BOUND_DISTINCT = ex_ordinal(asc_sub$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
asc_sub$PEDALITY_GRADE = ex_ordinal(asc_sub$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))
#asc_sub$PEDALITY_GRADE = ex_ordinal(asc_sub$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))
# One hot encoding #########################################################################  
# one hot encoding for all character type attributes in dataframe x,
# except the attributes with the index number in vector c, 
# and delete the original attributes afterwards
# inputs: datafram x, int vector c (exceptional  attributes)
# output: one hot encoding completed dataframe
# exclude 0 feature values as a column
cnn_onehot_exZero <- function(x,c) {
  y = x
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) == "character" & !( colnames(x[i]) %in% c)) { # sc
      for(unique_value in unique(x[[i]])){
        if (!is.na(unique_value)) {
          y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(x[[i]] == unique_value, 1, 0)
        }
      }
      y = y[ , !(names(y) %in% c(names(x[i])))]
    }
  }
  return (y)
}
asc_sub = cnn_onehot_exZero(asc_sub, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))
# NA to 0
asc_sub[is.na(asc_sub)]=0
# Fix horizons to be k2=8#############################
k2 = 8
asc_sub = asc_sub[asc_sub$HORIZON_NO<=k2,] # drop all rows with "HORIZON_NO" > K2(5)
# # a function to standardize the column value
# nor <- function(x) { 
#   return((x-min(x))/(max(x)-min(x)))
# }
# asc_sub[,-c(1:3)]= apply(asc_sub[,-c(1:3)],2,nor) # make column value to be [0,1]
#
#split data into a big list group by project code and site id
asc_sub <- split(asc_sub,list(asc_sub$PROJECT_CODE,asc_sub$SITE_ID),drop = T)
write_rds(asc_sub,'./2_asc_cnn/cnn_asc_aftSplit.rds')
# add  empty rows for those "HORIZON_NO" < K2(5)
for ( i in 1: length(asc_sub)) {
  n2 = nrow(asc_sub[[i]])
  if (n2<k2) {
    asc_sub[[i]][(n2+1):k2,1:2] = asc_sub[[i]][1,1:2]
    asc_sub[[i]][(n2+1):k2,'HORIZON_NO'] = (n2+1):k2
    asc_sub[[i]][(n2+1):k2,-c(1:3)] = 0 
  }
  print(i)
}
# it is time to drop 4 columns 
asc_sub <- lapply(asc_sub, function(x) x[!(names(x) %in% c("PROJECT_CODE", "SITE_ID","OBS_NO","HORIZON_NO"))])
write_rds(asc_sub,'./2_asc_cnn/asc_sub_aftSplit.rds')
# convert to a 3D array: [data index, horizons, features]##############################
asc_sub = readRDS('./2_asc_cnn/asc_sub_aftSplit.rds')
asc_sub <- abind(asc_sub,along = 0)
#rename dim(2)
dimnames(asc_sub)[[2]] <- 1:8
# rearrange ASC_ORD
asc_sub=asc_sub[,,c(1:376,389,388,387,377,381,386,382,385,390,378:380,384,383,391:dim(asc_sub)[3])]
#asc_tag_list = grep('ASC_ORD_',dimnames(asc_sub)[[3]])
write_rds(asc_sub,'./2_asc_cnn/asc_sub_3darray.rds')
# sampling###################################################
asc_sub=read_rds('./2_asc_cnn/asc_sub_3darray.rds')
library(stringr)
library(keras)
#library(tensorflow)
#install_keras(tensorflow = 'gpu')
# without scaling
#shuffing 
asc_sub = asc_sub[sample(nrow(asc_sub),nrow(asc_sub)),,]
# # option one: devide tr and test set without prune and scaling########
# tr_rate=0.85
# idx = sample(nrow(asc_sub),round(nrow(asc_sub)*tr_rate),replace = F)
# asc_sub_tr= asc_sub[idx,,]
# asc_sub_test= asc_sub[-idx,,]

# scaling date for a specific class is less than s2, scale up to "s2".
# and delete the classes that are less than s1 data
#input: x: the data; class: list of a class; s1,s2: threshold of the size
#output: the scaled data of x

# #option two: prune only###########################
# cnn_prune <- function(x,features,s=500) {
#   class = grep(features,dimnames(x)[[3]])
#   l=c()
#   for (i in class) {
#     count =  nrow(x[x[,1,i]==1,,])
#     if (count < s) {
#       l=c(l,i)
#       x = x[x[,1,i]==0,,]
#     } 
#   }
#   return(x[,,-l])
# }
# asc_sub1 = cnn_prune(asc_sub,"ASC_ORD")
# asc_sub1 = cnn_prune(asc_sub1,"SUBORD|GREAT_GROUP|SUBGROUP",200)
# write_rds(asc_sub1,'./2_asc_cnn/asc_sub1_prune.rds')
# # only prune but not scaling
# asc_sub1 = asc_sub1[sample(nrow(asc_sub1),nrow(asc_sub1)),,]
tr_rate=0.85
idx = sample(nrow(asc_sub),round(nrow(asc_sub)*tr_rate),replace = F)
asc_sub_tr= asc_sub[idx,,]
asc_sub_test= asc_sub[-idx,,]

# # option three: prune and sample with scaling#######################
# sample_1 <- function(x,features,r=0.7,s=1000) {
#   class = grep(features,dimnames(x)[[3]])
#   tr = array(numeric(),c(0,dim(x)[[2]],dim(x)[[3]]))
#   dimnames(tr)[[3]]=dimnames(x)[[3]]
#   dimnames(tr)[[2]]=dimnames(x)[[2]]
#   te = array(numeric(),c(0,dim(x)[[2]],dim(x)[[3]]))
#   dimnames(te)[[3]]=dimnames(x)[[3]]
#   dimnames(te)[[2]]=dimnames(x)[[2]]
#   for (i in class) {
#     temp =  x[x[,1,i]==1,,]
#     idx = sample(nrow(temp),round(nrow(temp)*r))
#     te = abind(te,temp[-idx,,][!dimnames(temp[-idx,,])[[1]] %in% dimnames(te)[[1]],,],along = 1)
#     
#     if ((nrow(temp[idx,,][!dimnames(temp[idx,,])[[1]] %in% dimnames(tr)[[1]],, ])+nrow(tr[tr[,1,i]==1,,])) >= s) {
#       tr = abind(tr, temp[idx,,][!dimnames(temp[idx,,])[[1]] %in% dimnames(tr)[[1]],, ],along = 1)
#     } else {
#       tr = abind(tr,temp[sample(idx,max(1,round(s*r-nrow(tr[tr[,1,i]==1,,]))),
#                                 replace = round(s*r-nrow(tr[tr[,1,i]==1,,]))>length(idx)),,],along = 1)
#     }
#     print(paste(i,dimnames(x)[[3]][i],nrow(tr),sum(tr[,1,dimnames(tr)[[3]][i]]==1),
#                 nrow(te),sum(te[,1,dimnames(te)[[3]][i]]==1)))
#   }
#   return(list(tr[sample(nrow(tr),nrow(tr)),,],te[sample(nrow(te),nrow(te)),,]))
# } 
# asc_sample = sample_1(asc_sub1,'ASC_ORD|SUBORD|GREAT_GROUP|SUBGROUP')
# asc_sub_tr = asc_sample[[1]]
# asc_sub_test = asc_sample[[2]]
# library(readr)
# write_rds(asc_sample,'./2_asc_cnn/asc_sample.rds')
# write_rds(asc_sub_tr,'./2_asc_cnn/asc_sub_tr.rds')
# write_rds(asc_sub_test,'./2_asc_cnn/asc_sub_test.rds')

#extract different input and tag list#########################
#asc_sub_test=readRDS('./2_asc_cnn/asc_sub_test.rds')
#asc_sub_tr=readRDS('./2_asc_cnn/asc_sub_tr.rds')
#asc_sub_tr = asc_sub_tr[sample(nrow(asc_sub_tr),round(nrow(asc_sub_tr)/2)),,]

base_list = grep('STATUS|ELEM_TYPE_CODE|PEDALITY_GRADE|UPPER_DEPTH|LOWER_DEPTH|BOUND_DISTINCT|TEXTURE_CODE|HOR_PREFIX|HOR_SUBHOR|HOR_SUFFIX|CUTAN_TYPE|PEDALITY_TYPE|NATURE',
                 dimnames(asc_sub_tr)[[3]])
asc_ex_list = grep('DRAINAGE|SOIL_WATER_STAT|VALUE_13C1_Fe|VALUE_15N1|VALUE_6B_6A|VALUE_4A1|VALUE_2Z2_Clay|HOR_MASTER'
                   ,dimnames(asc_sub_tr)[[3]])
asc_tag_list = grep('ASC_ORD_',dimnames(asc_sub_tr)[[3]])
so_ex_list = grep('COLOUR_CLASS|MOTT_TYPE',dimnames(asc_sub_tr)[[3]])
so_tag_list = grep('SUBORD_ASC_CODE',dimnames(asc_sub_tr)[[3]])
gg_tag_list = grep('GREAT_GROUP_ASC_CODE',dimnames(asc_sub_tr)[[3]])
sg_tag_list = grep('SUBGROUP_ASC_CODE',dimnames(asc_sub_tr)[[3]])
# generate 4d(input #, horizons, features, channels) data
#library(keras)
#library(abind)
k2=8
base_in_tr <- array_reshape(asub(asc_sub_tr, base_list,3), 
                            c(nrow(asc_sub_tr), k2, length(base_list), 1))
asc_ex_tr <- array_reshape(asub(asc_sub_tr, asc_ex_list,3), 
                            c(nrow(asc_sub_tr), k2, length(asc_ex_list), 1))
so_ex_tr <- array_reshape(asub(asc_sub_tr, so_ex_list,3), 
                           c(nrow(asc_sub_tr), k2, length(so_ex_list), 1))
# asc_tin_tr <- array_reshape(asub(asc_sub_tr, asc_tag_list,3), 
#                            c(nrow(asc_sub_tr), k2, length(asc_tag_list), 1))
# so_tin_tr <- array_reshape(asub(asc_sub_tr, so_tag_list,3), 
#                            c(nrow(asc_sub_tr), k2, length(so_tag_list), 1))
# gg_tin_tr <- array_reshape(asub(asc_sub_tr, gg_tag_list,3), 
#                           c(nrow(asc_sub_tr), k2, length(gg_tag_list), 1))

base_in_test <- array_reshape(asub(asc_sub_test, base_list,3), 
                            c(nrow(asc_sub_test), k2, length(base_list), 1))
asc_ex_test <- array_reshape(asub(asc_sub_test, asc_ex_list,3), 
                           c(nrow(asc_sub_test), k2, length(asc_ex_list), 1))
so_ex_test <- array_reshape(asub(asc_sub_test, so_ex_list,3), 
                          c(nrow(asc_sub_test), k2, length(so_ex_list), 1))
# asc_tin_test <- array_reshape(asub(asc_sub_test, asc_tag_list,3), 
#                             c(nrow(asc_sub_test), k2, length(asc_tag_list), 1))
# so_tin_test <- array_reshape(asub(asc_sub_test, so_tag_list,3), 
#                            c(nrow(asc_sub_test), k2, length(so_tag_list), 1))
# gg_tin_test <- array_reshape(asub(asc_sub_test, gg_tag_list,3), 
#                            c(nrow(asc_sub_test), k2, length(gg_tag_list), 1))

asc_tag_tr = asub(asc_sub_tr,list(1,asc_tag_list),c(2,3))
so_tag_tr = asub(asc_sub_tr,list(1,so_tag_list),c(2,3))
gg_tag_tr = asub(asc_sub_tr,list(1,gg_tag_list),c(2,3))
sg_tag_tr = asub(asc_sub_tr,list(1,sg_tag_list),c(2,3))

asc_tag_test = asub(asc_sub_test,list(1,asc_tag_list),c(2,3))
so_tag_test = asub(asc_sub_test,list(1,so_tag_list),c(2,3))
gg_tag_test = asub(asc_sub_test,list(1,gg_tag_list),c(2,3))
sg_tag_test = asub(asc_sub_test,list(1,sg_tag_list),c(2,3))
# model construction ###############################
base_in <- layer_input(shape=c(8,length(base_list),1))
asc_ex_in <- layer_input(shape=c(8,length(asc_ex_list),1))
so_ex_in <- layer_input(shape=c(8,length(so_ex_list),1))

asc_con <- layer_concatenate(list(base_in,asc_ex_in),axis=2)
asc_model <- asc_con %>%
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

asc_model

asc_out <- asc_model %>%
  layer_flatten() %>%
  #layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(asc_tag_list), activation = "softmax")
asc_out

#so_con <- layer_concatenate(list(so_ex_in),axis=2)
so_model <- so_ex_in %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3 ), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) #%>%
#layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
#              padding = 'same') %>%
#layer_max_pooling_2d(pool_size = c(1, 2))# %>%
# layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
#               padding = 'same') %>%
# layer_max_pooling_2d(pool_size = c(1, 2))
so_model

so_flat <- layer_concatenate(list(so_model,asc_model),axis = 2) %>%
  layer_flatten()
so_flat <-layer_concatenate(list(so_flat,asc_out),axis = 1) 
#layer_dropout(rate = 0.5) %>%
so_out <- so_flat %>%
  layer_dense(units =32, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(so_tag_list), activation = "softmax")
so_out

gg_flat <-layer_concatenate(list(so_flat,so_out),axis = 1) 
#layer_dropout(rate = 0.5) %>%
gg_out <- gg_flat %>%
  layer_dense(units = 64, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(gg_tag_list), activation = "softmax")
gg_out

sg_flat <-layer_concatenate(list(gg_flat,gg_out),axis = 1) 
#layer_dropout(rate = 0.5) %>%
sg_out <- sg_flat %>%
  layer_dense(units = 64, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(sg_tag_list), activation = "softmax")
sg_out

model <- keras_model(list(base_in,asc_ex_in,so_ex_in),list(asc_out,so_out,gg_out,sg_out))
model
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = c("categorical_crossentropy","categorical_crossentropy",
           "categorical_crossentropy","categorical_crossentropy","categorical_crossentropy"),
  metrics = metric_categorical_accuracy
)
model
model %>% save_model_hdf5('./2_asc_cnn/asc_2d.h5')
# model fitting and predicting, confusion matrix###############
history = model %>% fit(list(base_in_tr,asc_ex_tr,so_ex_tr),
                        list(asc_tag_tr,so_tag_tr,gg_tag_tr,sg_tag_tr),
                        epochs = 20, batch_size =128,validation_split =0.15)#,callbacks=callbacks)

# fitting with best epcho, remember to reset the model##################
model <- keras_model(list(base_in,asc_ex_in,so_ex_in),list(asc_out,so_out,gg_out,sg_out))
model
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = c("categorical_crossentropy","categorical_crossentropy",
           "categorical_crossentropy","categorical_crossentropy","categorical_crossentropy"),
  metrics = metric_categorical_accuracy
)
model
history = model %>% fit(list(base_in_tr,asc_ex_tr,so_ex_tr),
                        list(asc_tag_tr,so_tag_tr,gg_tag_tr,sg_tag_tr),
                        epochs = 8, batch_size =128)


result = model %>% evaluate(list(base_in_test,asc_ex_test,so_ex_test),
                            list(asc_tag_test,so_tag_test,gg_tag_test,sg_tag_test))
cnn_predict <- model %>% predict(list(base_in_test,asc_ex_test,so_ex_test))

# confusion matrix####################
colnames(cnn_predict[[1]]) = colnames(asc_tag_test)
rownames(cnn_predict[[1]]) = rownames(asc_tag_test)
colnames(cnn_predict[[2]]) = colnames(so_tag_test)
rownames(cnn_predict[[2]]) = rownames(so_tag_test)
colnames(cnn_predict[[3]]) = colnames(gg_tag_test)
rownames(cnn_predict[[3]]) = rownames(gg_tag_test)
colnames(cnn_predict[[4]]) = colnames(sg_tag_test)
rownames(cnn_predict[[4]]) = rownames(sg_tag_test)
ord_predict = as.data.frame(cnn_predict[[1]])
so_predict = as.data.frame(cnn_predict[[2]])
gg_predict = as.data.frame(cnn_predict[[3]])
sg_predict = as.data.frame(cnn_predict[[4]])
# 95% confidence interval top_k matching####################
# ORD ###########
ord_predict = cbind(colnames(ord_predict)[apply(ord_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                    colnames(ord_predict)[apply(ord_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                                 match(sort(x[x>0],decreasing = T)[1],x),
                                                                                 match(sort(x[x>0],decreasing = T)[2],x)))],
                    colnames(ord_predict)[apply(ord_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[3],x)),
                                                                                 match(sort(x[x>0],decreasing = T)[1],x),
                                                                                 match(sort(x[x>0],decreasing = T)[3],x)))],
                    colnames(ord_predict)[apply(ord_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[4],x)),
                                                                                 match(sort(x[x>0],decreasing = T)[1],x),
                                                                                 match(sort(x[x>0],decreasing = T)[4],x)))],
                    colnames(asc_tag_test)[apply(asc_tag_test,1,which.max)],
                    ord_predict)
colnames(ord_predict)[1:5]=c('ORD_PRE1','ORD_PRE2','ORD_PRE3','ORD_PRE4','ORD_REAL')
ord_predict$ORD_PRE_TOP2 = ord_predict$ORD_PRE1
ord_predict[as.character(ord_predict$ORD_PRE2)==as.character(ord_predict$ORD_REAL),'ORD_PRE_TOP2'] =
  ord_predict[as.character(ord_predict$ORD_PRE2)==as.character(ord_predict$ORD_REAL),2]
ord_predict[as.character(ord_predict$ORD_PRE3)==as.character(ord_predict$ORD_REAL),'ORD_PRE_TOP2'] =
  ord_predict[as.character(ord_predict$ORD_PRE3)==as.character(ord_predict$ORD_REAL),3]
ord_predict[as.character(ord_predict$ORD_PRE4)==as.character(ord_predict$ORD_REAL),'ORD_PRE_TOP2'] =
  ord_predict[as.character(ord_predict$ORD_PRE4)==as.character(ord_predict$ORD_REAL),4]

ord_predict=ord_predict[,c(1,length(ord_predict),5)]
ord_con=confusionMatrix(ord_predict[,1],ord_predict[,6])
ord_con_top2 = confusionMatrix(ord_predict[,5],ord_predict[,6])
ord_con$overall
ord_con_top2$overall
# SO#############
so_predict = cbind(colnames(so_predict)[apply(so_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(so_predict)[apply(so_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)))],
                   colnames(so_predict)[apply(so_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[3],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[3],x)))],
                   colnames(so_predict)[apply(so_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[4],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[4],x)))],
                   colnames(so_tag_test)[apply(so_tag_test,1,which.max)],
                   so_predict)

colnames(so_predict)[1:5]=c('SO_PRE1','SO_PRE2','SO_PRE3','SO_PRE4','SO_REAL')
so_predict$SO_PRE_TOP2 = so_predict$SO_PRE1
so_predict[as.character(so_predict$SO_PRE2)==as.character(so_predict$SO_REAL),'SO_PRE_TOP2'] =
  so_predict[as.character(so_predict$SO_PRE2)==as.character(so_predict$SO_REAL),2]
so_predict[as.character(so_predict$SO_PRE3)==as.character(so_predict$SO_REAL),'SO_PRE_TOP2'] =
  so_predict[as.character(so_predict$SO_PRE3)==as.character(so_predict$SO_REAL),3]
so_predict[as.character(so_predict$SO_PRE4)==as.character(so_predict$SO_REAL),'SO_PRE_TOP2'] =
  so_predict[as.character(so_predict$SO_PRE4)==as.character(so_predict$SO_REAL),4]
so_predict=so_predict[,c(1,length(so_predict),5)]
# top_2
so_top2= so_predict[,c(2,3)]
so_top2 = rbind(so_top2,
                data.frame('SO_PRE_TOP2'=rep(NA,
                                             length(so_top2[which(is.na(match(so_top2$SO_PRE_TOP2,so_top2$SO_REAL))),'SO_PRE_TOP2'])),
                           'SO_REAL'=so_top2[which(is.na(match(so_top2$SO_PRE_TOP2,so_top2$SO_REAL))),'SO_PRE_TOP2']))
so_top2 = rbind(so_top2,
                data.frame('SO_PRE_TOP2'=so_top2[which(is.na(match(so_top2$SO_REAL,so_top2$SO_PRE_TOP2))),'SO_REAL'],
                           'SO_REAL'=rep(NA,length(so_top2[which(is.na(match(so_top2$SO_REAL,so_top2$SO_PRE_TOP2))),'SO_REAL']))
                ))

so_con_top2=confusionMatrix(so_top2[,1],so_top2[,2])
so_con_top2$overall
# gg########################
gg_predict = cbind(colnames(gg_predict)[apply(gg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(gg_predict)[apply(gg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(gg_predict)[apply(gg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[3],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[3],x)
                   ))],
                   colnames(gg_predict)[apply(gg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[4],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[4],x)
                   ))],
                   colnames(gg_predict)[apply(gg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[5],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[5],x)
                   ))],
                   colnames(gg_predict)[apply(gg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[6],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[6],x)
                   ))],
                   colnames(gg_tag_test)[apply(gg_tag_test,1,which.max)],
                   gg_predict)

#gg_predict[gg_predict<0.1]=0
# 
# gg_predict = cbind(colnames(gg_predict)[apply(gg_predict,1,which.max)],
#                    colnames(gg_tag_test)[apply(gg_tag_test,1,which.max)], 
#                    gg_predict)
# 
colnames(gg_predict)[1:7]=c('GG_PRE1','GG_PRE2','GG_PRE3','GG_PRE4','GG_PRE5','GG_PRE6','GG_REAL')
gg_predict$GG_PRE_TOP2 = gg_predict$GG_PRE1
gg_predict[as.character(gg_predict$GG_PRE2)==as.character(gg_predict$GG_REAL),'GG_PRE_TOP2'] =
  gg_predict[as.character(gg_predict$GG_PRE2)==as.character(gg_predict$GG_REAL),2]
gg_predict[as.character(gg_predict$GG_PRE3)==as.character(gg_predict$GG_REAL),'GG_PRE_TOP2'] =
  gg_predict[as.character(gg_predict$GG_PRE3)==as.character(gg_predict$GG_REAL),3]
gg_predict[as.character(gg_predict$GG_PRE4)==as.character(gg_predict$GG_REAL),'GG_PRE_TOP2'] =
  gg_predict[as.character(gg_predict$GG_PRE4)==as.character(gg_predict$GG_REAL),4]
gg_predict[as.character(gg_predict$GG_PRE5)==as.character(gg_predict$GG_REAL),'GG_PRE_TOP2'] =
  gg_predict[as.character(gg_predict$GG_PRE5)==as.character(gg_predict$GG_REAL),5]
gg_predict[as.character(gg_predict$GG_PRE6)==as.character(gg_predict$GG_REAL),'GG_PRE_TOP2'] =
  gg_predict[as.character(gg_predict$GG_PRE6)==as.character(gg_predict$GG_REAL),6]

gg_predict=gg_predict[,c(1,length(gg_predict),7)]
# top_2
gg_top2= gg_predict[,c(2,3)]
gg_top2 = rbind(gg_top2,
                data.frame('GG_PRE_TOP2'=rep(NA,
                                             length(gg_top2[which(is.na(match(gg_top2$GG_PRE_TOP2,gg_top2$GG_REAL))),'GG_PRE_TOP2'])),
                           'GG_REAL'=gg_top2[which(is.na(match(gg_top2$GG_PRE_TOP2,gg_top2$GG_REAL))),'GG_PRE_TOP2']))
gg_top2 = rbind(gg_top2,
                data.frame('GG_PRE_TOP2'=gg_top2[which(is.na(match(gg_top2$GG_REAL,gg_top2$GG_PRE_TOP2))),'GG_REAL'],
                           'GG_REAL'=rep(NA,length(gg_top2[which(is.na(match(gg_top2$GG_REAL,gg_top2$GG_PRE_TOP2))),'GG_REAL']))
                ))

gg_con_top2=confusionMatrix(gg_top2[,1],gg_top2[,2])


gg_con_top2$overall
#SG######
sg_predict = cbind(colnames(sg_predict)[apply(sg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[3],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[3],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[4],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[4],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[5],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[5],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[6],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[6],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[7],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[7],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[8],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[8],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[9],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[9],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[10],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[10],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[11],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[11],x)))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[12],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[12],x)))],
                   colnames(sg_tag_test)[apply(sg_tag_test,1,which.max)],
                   sg_predict)

#sg_predict[sg_predict<0.1]=0

# sg_predict = cbind(colnames(sg_predict)[apply(sg_predict,1,which.max)],
#                    colnames(sg_tag_test)[apply(sg_tag_test,1,which.max)], 
#                    sg_predict)
colnames(sg_predict)[1:13]=c('SG_PRE1','SG_PRE2','SG_PRE3','SG_PRE4','SG_PRE5','SG_PRE6','SG_PRE7',
                             'SG_PRE8','SG_PRE9','SG_PRE10','SG_PRE11','SG_PRE12','SG_REAL')
sg_predict$SG_PRE_TOP2 = sg_predict$SG_PRE1
sg_predict[as.character(sg_predict$SG_PRE2)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE2)==as.character(sg_predict$SG_REAL),2]
sg_predict[as.character(sg_predict$SG_PRE3)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE3)==as.character(sg_predict$SG_REAL),3]
sg_predict[as.character(sg_predict$SG_PRE4)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE4)==as.character(sg_predict$SG_REAL),4]
sg_predict[as.character(sg_predict$SG_PRE5)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE5)==as.character(sg_predict$SG_REAL),5]
sg_predict[as.character(sg_predict$SG_PRE6)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE6)==as.character(sg_predict$SG_REAL),6]
sg_predict[as.character(sg_predict$SG_PRE7)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE7)==as.character(sg_predict$SG_REAL),7]
sg_predict[as.character(sg_predict$SG_PRE8)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE8)==as.character(sg_predict$SG_REAL),8]
sg_predict[as.character(sg_predict$SG_PRE9)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE9)==as.character(sg_predict$SG_REAL),9]
sg_predict[as.character(sg_predict$SG_PRE10)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE10)==as.character(sg_predict$SG_REAL),10]
sg_predict[as.character(sg_predict$SG_PRE11)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE11)==as.character(sg_predict$SG_REAL),11]
sg_predict[as.character(sg_predict$SG_PRE12)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE12)==as.character(sg_predict$SG_REAL),12]
sg_predict=sg_predict[,c(1,length(sg_predict),13)]
# top_2
sg_top2= sg_predict[,c(2,3)]
sg_top2 = rbind(sg_top2,
                data.frame('SG_PRE_TOP2'=rep(NA,
                                             length(sg_top2[which(is.na(match(sg_top2$SG_PRE_TOP2,sg_top2$SG_REAL))),'SG_PRE_TOP2'])),
                           'SG_REAL'=sg_top2[which(is.na(match(sg_top2$SG_PRE_TOP2,sg_top2$SG_REAL))),'SG_PRE_TOP2']))
sg_top2 = rbind(sg_top2,
                data.frame('SG_PRE_TOP2'=sg_top2[which(is.na(match(sg_top2$SG_REAL,sg_top2$SG_PRE_TOP2))),'SG_REAL'],
                           'SG_REAL'=rep(NA,length(sg_top2[which(is.na(match(sg_top2$SG_REAL,sg_top2$SG_PRE_TOP2))),'SG_REAL']))
                ))

sg_con_top2=confusionMatrix(sg_top2[,1],sg_top2[,2])
sg_con_top2$overall

# Single level acc###############
# ord###################
ord_predict = cbind(colnames(ord_predict)[apply(ord_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                    colnames(ord_predict)[apply(ord_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                                 match(sort(x[x>0],decreasing = T)[1],x),
                                                                                 match(sort(x[x>0],decreasing = T)[2],x)
                    ))],
                    colnames(asc_tag_test)[apply(asc_tag_test,1,which.max)],
                    ord_predict)

colnames(ord_predict)[1:3]=c('ORD_PRE1','ORD_PRE2','ORD_REAL')
ord_predict$ORD_PRE_TOP2 = ord_predict$ORD_PRE1
ord_predict[as.character(ord_predict$ORD_PRE2)==as.character(ord_predict$ORD_REAL),'ORD_PRE_TOP2'] =
  ord_predict[as.character(ord_predict$ORD_PRE2)==as.character(ord_predict$ORD_REAL),2]
ord_predict=ord_predict[,c(1,2,length(ord_predict),3)]
#
ord_con=confusionMatrix(ord_predict[,1],ord_predict[,4])
ord_con_top2 = confusionMatrix(ord_predict[,3],ord_predict[,4])
#so  #################
so_predict = cbind(colnames(so_predict)[apply(so_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(so_predict)[apply(so_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(so_tag_test)[apply(so_tag_test,1,which.max)],
                   so_predict)

colnames(so_predict)[1:3]=c('SO_PRE1','SO_PRE2','SO_REAL')
so_predict$SO_PRE_TOP2 = so_predict$SO_PRE1
so_predict[as.character(so_predict$SO_PRE2)==as.character(so_predict$SO_REAL),'SO_PRE_TOP2'] =
  so_predict[as.character(so_predict$SO_PRE2)==as.character(so_predict$SO_REAL),2]
so_predict=so_predict[,c(1,2,length(so_predict),3)]
# single matching
so_single= so_predict[,c(1,4)]
so_single = rbind(so_single,
                  data.frame('SO_PRE1'=rep(NA,
                                           length(so_single[which(is.na(match(so_single$SO_PRE1,so_single$SO_REAL))),'SO_PRE1'])),
                             'SO_REAL'=so_single[which(is.na(match(so_single$SO_PRE1,so_single$SO_REAL))),'SO_PRE1']))
so_single = rbind(so_single,
                  data.frame('SO_PRE1'=so_single[which(is.na(match(so_single$SO_REAL,so_single$SO_PRE1))),'SO_REAL'],
                             'SO_REAL'=rep(NA,length(so_single[which(is.na(match(so_single$SO_REAL,so_single$SO_PRE1))),'SO_REAL']))
                  ))

so_con_s=confusionMatrix(so_single[,1],so_single[,2])
# top_2
so_top2= so_predict[,c(3,4)]
so_top2 = rbind(so_top2,
                data.frame('SO_PRE_TOP2'=rep(NA,
                                             length(so_top2[which(is.na(match(so_top2$SO_PRE_TOP2,so_top2$SO_REAL))),'SO_PRE_TOP2'])),
                           'SO_REAL'=so_top2[which(is.na(match(so_top2$SO_PRE_TOP2,so_top2$SO_REAL))),'SO_PRE_TOP2']))
so_top2 = rbind(so_top2,
                data.frame('SO_PRE_TOP2'=so_top2[which(is.na(match(so_top2$SO_REAL,so_top2$SO_PRE_TOP2))),'SO_REAL'],
                           'SO_REAL'=rep(NA,length(so_top2[which(is.na(match(so_top2$SO_REAL,so_top2$SO_PRE_TOP2))),'SO_REAL']))
                ))

so_con_top2=confusionMatrix(so_top2[,1],so_top2[,2])

#gg#################
gg_predict = cbind(colnames(gg_predict)[apply(gg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(gg_predict)[apply(gg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(gg_tag_test)[apply(gg_tag_test,1,which.max)],
                   gg_predict)

#gg_predict[gg_predict<0.1]=0
# 
# gg_predict = cbind(colnames(gg_predict)[apply(gg_predict,1,which.max)],
#                    colnames(gg_tag_test)[apply(gg_tag_test,1,which.max)], 
#                    gg_predict)
# 
colnames(gg_predict)[1:3]=c('GG_PRE1','GG_PRE2','GG_REAL')
gg_predict$GG_PRE_TOP2 = gg_predict$GG_PRE1
gg_predict[as.character(gg_predict$GG_PRE2)==as.character(gg_predict$GG_REAL),'GG_PRE_TOP2'] =
  gg_predict[as.character(gg_predict$GG_PRE2)==as.character(gg_predict$GG_REAL),2]
gg_predict=gg_predict[,c(1,2,length(gg_predict),3)]
# single matching
gg_single= gg_predict[,c(1,4)]
gg_single = rbind(gg_single,
                  data.frame('GG_PRE1'=rep(NA,
                                           length(gg_single[which(is.na(match(gg_single$GG_PRE1,gg_single$GG_REAL))),'GG_PRE1'])),
                             'GG_REAL'=gg_single[which(is.na(match(gg_single$GG_PRE1,gg_single$GG_REAL))),'GG_PRE1']))
gg_single = rbind(gg_single,
                  data.frame('GG_PRE1'=gg_single[which(is.na(match(gg_single$GG_REAL,gg_single$GG_PRE1))),'GG_REAL'],
                             'GG_REAL'=rep(NA,length(gg_single[which(is.na(match(gg_single$GG_REAL,gg_single$GG_PRE1))),'GG_REAL']))
                  ))

gg_con_s=confusionMatrix(gg_single[,1],gg_single[,2])
# top_2
gg_top2= gg_predict[,c(3,4)]
gg_top2 = rbind(gg_top2,
                data.frame('GG_PRE_TOP2'=rep(NA,
                                             length(gg_top2[which(is.na(match(gg_top2$GG_PRE_TOP2,gg_top2$GG_REAL))),'GG_PRE_TOP2'])),
                           'GG_REAL'=gg_top2[which(is.na(match(gg_top2$GG_PRE_TOP2,gg_top2$GG_REAL))),'GG_PRE_TOP2']))
gg_top2 = rbind(gg_top2,
                data.frame('GG_PRE_TOP2'=gg_top2[which(is.na(match(gg_top2$GG_REAL,gg_top2$GG_PRE_TOP2))),'GG_REAL'],
                           'GG_REAL'=rep(NA,length(gg_top2[which(is.na(match(gg_top2$GG_REAL,gg_top2$GG_PRE_TOP2))),'GG_REAL']))
                ))

gg_con_top2=confusionMatrix(gg_top2[,1],gg_top2[,2])
#SG######
sg_predict = cbind(colnames(sg_predict)[apply(sg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(sg_tag_test)[apply(sg_tag_test,1,which.max)],
                   sg_predict)

#sg_predict[sg_predict<0.1]=0

# sg_predict = cbind(colnames(sg_predict)[apply(sg_predict,1,which.max)],
#                    colnames(sg_tag_test)[apply(sg_tag_test,1,which.max)], 
#                    sg_predict)
colnames(sg_predict)[1:3]=c('SG_PRE1','SG_PRE2','SG_REAL')
sg_predict$SG_PRE_TOP2 = sg_predict$SG_PRE1
sg_predict[as.character(sg_predict$SG_PRE2)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE2)==as.character(sg_predict$SG_REAL),2]
sg_predict=sg_predict[,c(1,2,length(sg_predict),3)]
# single matching
sg_single= sg_predict[,c(1,4)]
sg_single = rbind(sg_single,
                  data.frame('SG_PRE1'=rep(NA,
                                           length(sg_single[which(is.na(match(sg_single$SG_PRE1,sg_single$SG_REAL))),'SG_PRE1'])),
                             'SG_REAL'=sg_single[which(is.na(match(sg_single$SG_PRE1,sg_single$SG_REAL))),'SG_PRE1']))
sg_single = rbind(sg_single,
                  data.frame('SG_PRE1'=sg_single[which(is.na(match(sg_single$SG_REAL,sg_single$SG_PRE1))),'SG_REAL'],
                             'SG_REAL'=rep(NA,length(sg_single[which(is.na(match(sg_single$SG_REAL,sg_single$SG_PRE1))),'SG_REAL']))
                  ))

sg_con_s=confusionMatrix(sg_single[,1],sg_single[,2])
# top_2
sg_top2= sg_predict[,c(3,4)]
sg_top2 = rbind(sg_top2,
                data.frame('SG_PRE_TOP2'=rep(NA,
                                             length(sg_top2[which(is.na(match(sg_top2$SG_PRE_TOP2,sg_top2$SG_REAL))),'SG_PRE_TOP2'])),
                           'SG_REAL'=sg_top2[which(is.na(match(sg_top2$SG_PRE_TOP2,sg_top2$SG_REAL))),'SG_PRE_TOP2']))
sg_top2 = rbind(sg_top2,
                data.frame('SG_PRE_TOP2'=sg_top2[which(is.na(match(sg_top2$SG_REAL,sg_top2$SG_PRE_TOP2))),'SG_REAL'],
                           'SG_REAL'=rep(NA,length(sg_top2[which(is.na(match(sg_top2$SG_REAL,sg_top2$SG_PRE_TOP2))),'SG_REAL']))
                ))

sg_con_top2=confusionMatrix(sg_top2[,1],sg_top2[,2])

# result##########
ord_con$overall
ord_con_top2$overall
so_con_s$overall
so_con_top2$overall
gg_con_s$overall
gg_con_top2$overall
sg_con_s$overall
sg_con_top2$overall

# Hierarchical acc#################
asc_predict = data.frame(ord_predict$ORD_PRE1,ord_predict$ORD_PRE_TOP2,ord_predict$ORD_REAL,
                         so_predict$SO_PRE1,so_predict$SO_PRE_TOP2,so_predict$SO_REAL,
                         gg_predict$GG_PRE1,gg_predict$GG_PRE_TOP2,gg_predict$GG_REAL,
                         sg_predict$SG_PRE1,sg_predict$SG_PRE_TOP2,sg_predict$SG_REAL)

asc_predict$P_SG = paste(str_sub(asc_predict[[1]],-2,-1),
                         str_sub(asc_predict[[4]],-2,-1),
                         str_sub(asc_predict[[7]],-2,-1),
                         str_sub(asc_predict[[10]],-2,-1),
                         sep='_')
asc_predict$P_SG_TOP2 = paste(str_sub(asc_predict[[2]],-2,-1),
                              str_sub(asc_predict[[5]],-2,-1),
                              str_sub(asc_predict[[8]],-2,-1),
                              str_sub(asc_predict[[11]],-2,-1),
                              sep='_')

asc_predict$R_SG = paste(str_sub(asc_predict[[3]],-2,-1),
                         str_sub(asc_predict[[6]],-2,-1),
                         str_sub(asc_predict[[9]],-2,-1),
                         str_sub(asc_predict[[12]],-2,-1),
                         sep='_')
asc_predict$P_GG = paste(str_sub(asc_predict[[1]],-2,-1),
                         str_sub(asc_predict[[4]],-2,-1),
                         str_sub(asc_predict[[7]],-2,-1),
                         sep='_')
asc_predict$P_GG_TOP2 = paste(str_sub(asc_predict[[2]],-2,-1),
                              str_sub(asc_predict[[5]],-2,-1),
                              str_sub(asc_predict[[8]],-2,-1),
                              sep='_')
asc_predict$R_GG = paste(str_sub(asc_predict[[3]],-2,-1),
                         str_sub(asc_predict[[6]],-2,-1),
                         str_sub(asc_predict[[9]],-2,-1),
                         sep='_')
asc_predict$P_SO = paste(str_sub(asc_predict[[1]],-2,-1),
                         str_sub(asc_predict[[4]],-2,-1),
                         sep='_')
asc_predict$P_SO_TOP2 = paste(str_sub(asc_predict[[2]],-2,-1),
                              str_sub(asc_predict[[5]],-2,-1),
                              sep='_')
asc_predict$R_SO = paste(str_sub(asc_predict[[3]],-2,-1),
                         str_sub(asc_predict[[6]],-2,-1),
                         sep='_')
asc_predict$P_ORD = paste(str_sub(asc_predict[[1]],-2,-1),
                          sep='_')
asc_predict$P_ORD_TOP2 = paste(str_sub(asc_predict[[2]],-2,-1),
                               sep='_')
asc_predict$R_ORD = paste(str_sub(asc_predict[[3]],-2,-1),
                          sep='_')

asc_p = asc_predict[,13:24]
# custom_confusion <- function(x) {
#   x = rbind(x,
#                data.frame(x[[1]]=rep(NA,
#                                      length(x[which(is.na(match(x[[1]],x[[2]]))),x[[1]]])),
#                           x[[2]]=x[which(is.na(match(x[[1]],x[[2]]))),x[[1]]]))
#   x = rbind(x,
#                data.frame(x[[1]]=x[which(is.na(match(x[[2]],x[[1]]))),x[[2]]],
#                           x[[2]]=rep(NA,length(x[which(is.na(match(x[[2]],x[[1]]))),x[[2]]]))
#                ))
#   return(confusionMatrix(factor(x[,1]),factor(x[[2]])))
# }
#SG SINGLE##################
sg_p = asc_p[,c(1,3)]
sg_p = rbind(sg_p,
             data.frame('P_SG'=rep(NA,
                                   length(sg_p[which(is.na(match(sg_p$P_SG,sg_p$R_SG))),'P_SG'])),
                        'R_SG'=sg_p[which(is.na(match(sg_p$P_SG,sg_p$R_SG))),'P_SG']))
sg_p = rbind(sg_p,
             data.frame('P_SG'=sg_p[which(is.na(match(sg_p$R_SG,sg_p$P_SG))),'R_SG'],
                        'R_SG'=rep(NA,length(sg_p[which(is.na(match(sg_p$R_SG,sg_p$P_SG))),'R_SG']))
             ))
sg_confusion=confusionMatrix(factor(sg_p$P_SG),factor(sg_p$R_SG))
#GG SINGLE##################
gg_p = asc_p[,c(4,6)]
gg_p = rbind(gg_p,
             data.frame('P_GG'=rep(NA,
                                   length(gg_p[which(is.na(match(gg_p$P_GG,gg_p$R_GG))),'P_GG'])),
                        'R_GG'=gg_p[which(is.na(match(gg_p$P_GG,gg_p$R_GG))),'P_GG']))
gg_p = rbind(gg_p,
             data.frame('P_GG'=gg_p[which(is.na(match(gg_p$R_GG,gg_p$P_GG))),'R_GG'],
                        'R_GG'=rep(NA,length(gg_p[which(is.na(match(gg_p$R_GG,gg_p$P_GG))),'R_GG']))
             ))
gg_confusion=confusionMatrix(factor(gg_p$P_GG),factor(gg_p$R_GG))
#SO SINGLE##################
so_p = asc_p[,c(7,9)]
so_p = rbind(so_p,
             data.frame('P_SO'=rep(NA,
                                   length(so_p[which(is.na(match(so_p$P_SO,so_p$R_SO))),'P_SO'])),
                        'R_SO'=so_p[which(is.na(match(so_p$P_SO,so_p$R_SO))),'P_SO']))
so_p = rbind(so_p,
             data.frame('P_SO'=so_p[which(is.na(match(so_p$R_SO,so_p$P_SO))),'R_SO'],
                        'R_SO'=rep(NA,length(so_p[which(is.na(match(so_p$R_SO,so_p$P_SO))),'R_SO']))
             ))
so_confusion=confusionMatrix(factor(so_p$P_SO),factor(so_p$R_SO))
#ORD SINGLE##################
ord_p = asc_p[,c(10,12)]
ord_p = rbind(ord_p,
              data.frame('P_ORD'=rep(NA,
                                     length(ord_p[which(is.na(match(ord_p$P_ORD,ord_p$R_ORD))),'P_ORD'])),
                         'R_ORD'=ord_p[which(is.na(match(ord_p$P_ORD,ord_p$R_ORD))),'P_ORD']))
ord_p = rbind(ord_p,
              data.frame('P_ORD'=ord_p[which(is.na(match(ord_p$R_ORD,ord_p$P_ORD))),'R_ORD'],
                         'R_ORD'=rep(NA,length(ord_p[which(is.na(match(ord_p$R_ORD,ord_p$P_ORD))),'R_ORD']))
              ))
ord_confusion=confusionMatrix(factor(ord_p$P_ORD),factor(ord_p$R_ORD))

# SG TPO2###################
sg_top2 = asc_p[,c(2,3)]
sg_top2 = rbind(sg_top2,
                data.frame('P_SG_TOP2'=rep(NA,
                                           length(sg_top2[which(is.na(match(sg_top2$P_SG_TOP2,sg_top2$R_SG))),'P_SG_TOP2'])),
                           'R_SG'=sg_top2[which(is.na(match(sg_top2$P_SG_TOP2,sg_top2$R_SG))),'P_SG_TOP2']))
sg_top2 = rbind(sg_top2,
                data.frame('P_SG_TOP2'=sg_top2[which(is.na(match(sg_top2$R_SG,sg_top2$P_SG_TOP2))),'R_SG'],
                           'R_SG'=rep(NA,length(sg_top2[which(is.na(match(sg_top2$R_SG,sg_top2$P_SG_TOP2))),'R_SG']))
                ))
sg_confusion_top2=confusionMatrix(factor(sg_top2$P_SG_TOP2),factor(sg_top2$R_SG))
# GG TPO2###################
gg_top2 = asc_p[,c(5,6)]
gg_top2 = rbind(gg_top2,
                data.frame('P_GG_TOP2'=rep(NA,
                                           length(gg_top2[which(is.na(match(gg_top2$P_GG_TOP2,gg_top2$R_GG))),'P_GG_TOP2'])),
                           'R_GG'=gg_top2[which(is.na(match(gg_top2$P_GG_TOP2,gg_top2$R_GG))),'P_GG_TOP2']))
gg_top2 = rbind(gg_top2,
                data.frame('P_GG_TOP2'=gg_top2[which(is.na(match(gg_top2$R_GG,gg_top2$P_GG_TOP2))),'R_GG'],
                           'R_GG'=rep(NA,length(gg_top2[which(is.na(match(gg_top2$R_GG,gg_top2$P_GG_TOP2))),'R_GG']))
                ))
gg_confusion_top2=confusionMatrix(factor(gg_top2$P_GG_TOP2),factor(gg_top2$R_GG))

# SO TPO2###################
so_top2 = asc_p[,c(8,9)]
so_top2 = rbind(so_top2,
                data.frame('P_SO_TOP2'=rep(NA,
                                           length(so_top2[which(is.na(match(so_top2$P_SO_TOP2,so_top2$R_SO))),'P_SO_TOP2'])),
                           'R_SO'=so_top2[which(is.na(match(so_top2$P_SO_TOP2,so_top2$R_SO))),'P_SO_TOP2']))
so_top2 = rbind(so_top2,
                data.frame('P_SO_TOP2'=so_top2[which(is.na(match(so_top2$R_SO,so_top2$P_SO_TOP2))),'R_SO'],
                           'R_SO'=rep(NA,length(so_top2[which(is.na(match(so_top2$R_SO,so_top2$P_SO_TOP2))),'R_SO']))
                ))
so_confusion_top2=confusionMatrix(factor(so_top2$P_SO_TOP2),factor(so_top2$R_SO))


# ORD TPO2###################
ord_top2 = asc_p[,c(11,12)]
ord_top2 = rbind(ord_top2,
                 data.frame('P_ORD_TOP2'=rep(NA,
                                             length(ord_top2[which(is.na(match(ord_top2$P_ORD_TOP2,ord_top2$R_ORD))),'P_ORD_TOP2'])),
                            'R_ORD'=ord_top2[which(is.na(match(ord_top2$P_ORD_TOP2,ord_top2$R_ORD))),'P_ORD_TOP2']))
ord_top2 = rbind(ord_top2,
                 data.frame('P_ORD_TOP2'=ord_top2[which(is.na(match(ord_top2$R_ORD,ord_top2$P_ORD_TOP2))),'R_ORD'],
                            'R_ORD'=rep(NA,length(ord_top2[which(is.na(match(ord_top2$R_ORD,ord_top2$P_ORD_TOP2))),'R_ORD']))
                 ))
ord_confusion_top2=confusionMatrix(factor(ord_top2$P_ORD_TOP2),factor(ord_top2$R_ORD))

# result##############
ord_confusion$overall
ord_confusion_top2$overall
so_confusion$overall
so_confusion_top2$overall
gg_confusion$overall
gg_confusion_top2$overall
sg_confusion$overall
sg_confusion_top2$overall



