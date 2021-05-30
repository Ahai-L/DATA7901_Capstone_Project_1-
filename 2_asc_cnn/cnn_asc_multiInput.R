library(readr)
library(abind)
cnn_sub <- readRDS("./1_asc_mlp/asc_train_sub.rds") 
subord_exInput = cnn_sub[,c(1,2,4,27,29)]
# One hot encoding #########################################################################  
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
subord_exInput = cnn_onehot(subord_exInput, c("PROJECT_CODE","SITE_ID","HORIZON_NO"))
# Fix to k2=8 horizons#################################
subord_exInput[is.na(subord_exInput)]=0
# = subord_exInput[,c(1,2,4:394)]
# a = scale(cnn_dat[,-c(1:3)])
# make every data to be same horizon levels
k2 = 8
subord_exInput = subord_exInput[subord_exInput$HORIZON_NO<=k2,] # drop all rows with "HORIZON_NO" > K2(5)
# # a function to standardize the column value
# nor <- function(x) { 
#   return((x-min(x))/(max(x)-min(x)))
# }
# cnn_dat[,-c(1:3)]= apply(cnn_dat[,-c(1:3)],2,nor) # make column value to be [0,1]
subord_exInput <- split(subord_exInput,list(subord_exInput$PROJECT_CODE,subord_exInput$SITE_ID),drop = T)
#write_rds(cnn_dat,'./2_asc_cnn/cnn_asc_aftSplit.rds')
# n = nrow(cnn_dat)
# i=1
# while (i<=n) { # add  empty rows for those "HORIZON_NO" < K2(5) too slow!!!
#   if (cnn_dat[i+1,'PROJECT_CODE'] != cnn_dat[i,'PROJECT_CODE'] |
#       cnn_dat[i+1,'SITE_ID'] != cnn_dat[i,'SITE_ID'] |
#       i == n) {
#     n1 = cnn_dat[i,'HORIZON_NO']
#     while ( n1 < k2) {
#         cnn_dat[nrow(cnn_dat)+1,] = c(cnn_dat[i,1:2],n1+1,rep(0,length(cnn_dat)-3))
#         n1 = n1+1
#       } 
#     }
#   i = i+1
#   print(i)
# }
# add  empty rows for those "HORIZON_NO" < K2(5)
for ( i in 1: length(subord_exInput)) {
  n2 = nrow(subord_exInput[[i]])
  if (n2<k2) {
    subord_exInput[[i]][(n2+1):k2,1:2] = subord_exInput[[i]][1,1:2]
    subord_exInput[[i]][(n2+1):k2,'HORIZON_NO'] = (n2+1):k2
    subord_exInput[[i]][(n2+1):k2,-c(1:3)] = 0 
  }
  print(i)
}
# it is time to drop 3 columns 
subord_exInput <- lapply(subord_exInput, function(x) x[!(names(x) %in% c("PROJECT_CODE", "SITE_ID","HORIZON_NO"))])
subord_exInput <- abind(subord_exInput,along = 0)
dimnames(subord_exInput)[[1]] <- dimnames(cnn_dat1)[[1]]
dimnames(subord_exInput)[[2]] <- dimnames(cnn_dat1)[[2]]
#subord_exInput = array_reshape(subord_exInput, c(nrow(cnn_dat2), k2, 13, 1))
# sampling###################################################
cnn_dat1=read_rds('./2_asc_cnn/cnn_asc_3darray.rds')
library(stringr)
class = dimnames(cnn_dat1)[[3]][grep('ASC_ORD',dimnames(cnn_dat1)[[3]])]
cnn_dat2 = array(numeric(),c(0,8,390));cnn_dat3 = array(numeric(),c(0,8,390))
dimnames(cnn_dat2)[[3]]=dimnames(cnn_dat1)[[3]]
dimnames(cnn_dat3)[[3]]=dimnames(cnn_dat1)[[3]]
subord_ex_tr = array(numeric(),c(0,8,67));subord_ex_test = array(numeric(),c(0,8,67));
k2=8
size = 3000
# extract test set before adding size
for (c in class) {
  temp1 =  cnn_dat1[cnn_dat1[,1,c]==1,,]
  temp2 =  subord_exInput[cnn_dat1[,1,c]==1,,]
  idx = sample(nrow(temp1),round(nrow(temp1)*0.85),replace = F)
  cnn_dat3 = abind(cnn_dat3,temp1[-idx,,],along = 1)
  subord_ex_test = abind(subord_ex_test,temp2[-idx,,],along = 1)
  if (nrow(temp1) >= size) {
    subord_ex_tr = abind(subord_ex_tr, temp2[idx,,],along = 1)
    cnn_dat2 = abind(cnn_dat2, temp1[idx,,],along = 1)
  } else {
    idx1=sample(idx,size*0.85,replace = T)
    subord_ex_tr = abind(subord_ex_tr, temp2[idx1,,],along = 1)
    cnn_dat2 = abind(cnn_dat2, temp1[idx1,,],along = 1)
  }
}
# data shuffling
idx3=sample(nrow(cnn_dat2),nrow(cnn_dat2))
idx4=sample(nrow(cnn_dat3),nrow(cnn_dat3))
cnn_dat2 = cnn_dat2[idx3,,]
cnn_dat3 = cnn_dat3[idx4,,]
subord_ex_tr= subord_ex_tr[idx3,,]
subord_ex_test= subord_ex_test[idx4,,]

library(keras)
dimnames(subord_ex_tr)[[2]]=dimnames(cnn_dat2)[[2]]
dimnames(subord_ex_test)[[2]]=dimnames(cnn_dat3)[[2]]
cnn_train <- array_reshape(asub(cnn_dat2, -c(378:390),3), c(nrow(cnn_dat2), k2, 377, 1))
dimnames(cnn_train)[[3]]=dimnames(cnn_dat2)[[3]][1:377]
cnn_test <- array_reshape(asub(cnn_dat3, -c(378:390),3), c(nrow(cnn_dat3), k2, 377, 1))
dimnames(cnn_test)[[3]]=dimnames(cnn_dat2)[[3]][1:377]
cnn_tr_tag = asub(cnn_dat2,list(1,c(378:390)),c(2,3))
cnn_test_tag = asub(cnn_dat3,list(1,c(378:390)),c(2,3))
subord_ex_train= array_reshape(asub(subord_ex_tr,-c(7:67),3), c(nrow(subord_ex_tr), k2, 6, 1))
subord_ex_testt= array_reshape(asub(subord_ex_test,-c(7:67),3), c(nrow(subord_ex_test), k2, 6, 1))
subord_tr_tag = asub(subord_ex_tr,list(1,c(7:67)),c(2,3))
subord_test_tag = asub(subord_ex_test,list(1,c(7:67)),c(2,3))

asc_tr_tag_input = array_reshape(asub(cnn_dat2, c(378:390),3), c(nrow(cnn_dat2), k2, 13, 1))
asc_test_tag_input = array_reshape(asub(cnn_dat3, c(378:390),3), c(nrow(cnn_dat3), k2, 13, 1))


write_rds(cnn_train,'./2_asc_cnn/asc_in_tr.rds')
write_rds(cnn_test,'./2_asc_cnn/asc_in_test.rds')
write_rds(cnn_tr_tag,'./2_asc_cnn/asc_tag_tr.rds')
write_rds(cnn_test_tag,'./2_asc_cnn/asc_tag_test.rds')
write_rds(asc_tr_tag_input,'./2_asc_cnn/so_tin_tr.rds')
write_rds(asc_test_tag_input,'./2_asc_cnn/so_tin_test.rds')
write_rds(subord_ex_tr,'./2_asc_cnn/so_ex_tr.rds')
write_rds(subord_ex_test,'./2_asc_cnn/so_ex_test.rds')
write_rds(subord_tr_tag,'./2_asc_cnn/so_tag_tr.rds')
write_rds(subord_test_tag,'./2_asc_cnn/so_tag_test.rds')

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
  layer_dense(units = 90, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(so_tag_list), activation = "softmax")
so_out

gg_flat <-layer_concatenate(list(so_flat,so_out),axis = 1) 
#layer_dropout(rate = 0.5) %>%
gg_out <- gg_flat %>%
  layer_dense(units = 190, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(gg_tag_list), activation = "softmax")
gg_out

sg_flat <-layer_concatenate(list(gg_flat,gg_out),axis = 1) 
#layer_dropout(rate = 0.5) %>%
sg_out <- sg_flat %>%
  layer_dense(units = 190, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(sg_tag_list), activation = "softmax")
sg_out




model <- keras_model(list(base_in,asc_ex_in,so_ex_in),list(asc_out,so_out,gg_out,sg_out))
model
model %>% compile(
  optimizer = "rmsprop",
  loss = c("categorical_crossentropy","categorical_crossentropy",
           "categorical_crossentropy","categorical_crossentropy"),
  metrics = "acc"
)
# tensorboard
# dir.create("./2_asc_cnn/tensorlog")
# tensorboard ("tensorlog")
# callbacks = list(callback_tensorboard(
#   log_dir = "./2_asc_cnn/tensorlog"))
#   #histogram_freq = 1,
#   #embeddings_freq = 1))


# model fitting and predicting, confusion matrix###############
history = model %>% fit(list(cnn_train,asc_tr_tag_input,subord_ex_train),list(cnn_tr_tag,subord_tr_tag),
                        epochs = 15, batch_size =64,validation_split =0.2)
result = model %>% evaluate(list(cnn_test,asc_test_tag_input,subord_ex_testt),list(cnn_test_tag,subord_test_tag))
cnn_predict <- model %>% predict(list(cnn_test,asc_test_tag_input,subord_ex_testt))
colnames(cnn_predict[[1]]) = colnames(cnn_test_tag)
rownames(cnn_predict[[1]]) = rownames(cnn_test_tag)
ord_predict = as.data.frame(cnn_predict[[1]])
ord_predict = cbind(colnames(ord_predict)[apply(ord_predict,1,which.max)],
                    colnames(cnn_test_tag)[apply(cnn_test_tag,1,which.max)], 
                    ord_predict)
colnames(ord_predict)[1:2]=c('PREDICT','REAL')
library(caret)
ord_confusion=confusionMatrix(ord_predict[,1],ord_predict[,2])
ord_confusion[["table"]]=ord_confusion[["table"]][c(1,9,10,13,6,8,12,3,2,5,4,7,11),c(1,9,10,13,6,8,12,3,2,5,4,7,11)]
ord_confusion$byClass=ord_confusion$byClass[c(1,9,10,13,6,8,12,3,2,5,4,7,11),]
ord_confusion


colnames(cnn_predict[[2]]) = colnames(subord_test_tag)
rownames(cnn_predict[[2]]) = rownames(subord_test_tag)
subord_predict = as.data.frame(cnn_predict[[2]])
subord_predict = cbind(colnames(subord_predict)[apply(subord_predict,1,which.max)],
                    colnames(subord_test_tag)[apply(subord_test_tag,1,which.max)], 
                    subord_predict)
colnames(subord_predict)[1:2]=c('PREDICT','REAL')
library(caret)
subord_confusion=confusionMatrix(subord_predict[,1],subord_predict[,2])
#subord_confusion[["table"]]=subord_confusion[["table"]][c(1,9,10,13,6,8,12,3,2,5,4,7,11),c(1,9,10,13,6,8,12,3,2,5,4,7,11)]
#subord_confusion$byClass=subord_confusion$byClass[c(1,9,10,13,6,8,12,3,2,5,4,7,11),]
subord_confusion
