#library(tensorflow)
#install_tensorflow(version = 'gpu')

library(abind)
library(readr)

cnn_dat <- read.csv("./0_general/asc_train_split.csv",
                    stringsAsFactors=FALSE) 
# check "sn" attributes############################################
cnn_dat$DRAINAGE = as.integer(cnn_dat$DRAINAGE)

# Exchange ordinal data#######################################################################
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

cnn_dat$SOIL_WATER_STAT = ex_ordinal(cnn_dat$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
cnn_dat$BOUND_DISTINCT = ex_ordinal(cnn_dat$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
cnn_dat$PEDALITY_GRADE = ex_ordinal(cnn_dat$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))


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
cnn_dat = cnn_onehot(cnn_dat, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))
cnn_dat[is.na(cnn_dat)]=0
cnn_dat = cnn_dat[,c(1,2,4:394)]
# a = scale(cnn_dat[,-c(1:3)])
# make every data to be same horizon levels
k2 = 8
cnn_dat = cnn_dat[cnn_dat$HORIZON_NO<=k2,] # drop all rows with "HORIZON_NO" > K2(5)
# # a function to standardize the column value
# nor <- function(x) { 
#   return((x-min(x))/(max(x)-min(x)))
# }
# cnn_dat[,-c(1:3)]= apply(cnn_dat[,-c(1:3)],2,nor) # make column value to be [0,1]
cnn_dat <- split(cnn_dat,list(cnn_dat$PROJECT_CODE,cnn_dat$SITE_ID),drop = T)
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
for ( i in 1: length(cnn_dat)) {
  n2 = nrow(cnn_dat[[i]])
  if (n2<k2) {
    cnn_dat[[i]][(n2+1):k2,1:2] = cnn_dat[[i]][1,1:2]
    cnn_dat[[i]][(n2+1):k2,'HORIZON_NO'] = (n2+1):k2
    cnn_dat[[i]][(n2+1):k2,-c(1:3)] = 0 
  }
  print(i)
}
# it is time to drop 3 columns 
cnn_dat <- lapply(cnn_dat, function(x) x[!(names(x) %in% c("PROJECT_CODE", "SITE_ID","HORIZON_NO"))])
write_rds(cnn_dat,'./2_asc_cnn/cnn_asc_aftSplit.rds')
# convert to a 3D array: [data index, horizons, features]##############################
# start_time = Sys.time()
cnn_dat1 <- abind(cnn_dat,along = 0)
dimnames(cnn_dat1)[[2]] <- row.names(cnn_dat[[1]])
cnn_dat1=cnn_dat1[,,c(1:377,389,388,387,378,382,386,383,384,390,379:381,385)]
write_rds(cnn_dat1,'./2_asc_cnn/cnn_asc_3darray.rds')
# print(Sys.time()-start_time)
# list2ary = function(input.list){  #input a list of lists
#   rows.cols <- dim(input.list[[1]])
#   sheets <- length(input.list)
#   output.ary <- array(unlist(input.list), dim = c(rows.cols,sheets))
#   output.ary <- aperm(output.ary, c(3,1,2))
#   dimnames(output.ary)[[1]] <- names(input.list)
#   dimnames(output.ary)[[2]] <- row.names(input.list[[1]])
#   dimnames(output.ary)[[3]] <- colnames(input.list[[1]])
#   # colnames(output.ary) <- colnames(input.list[[1]])
#   # row.names(output.ary) <- row.names(input.list[[1]])
#   return(output.ary)    # output as a 3-D array
# }
# start_time = Sys.time()
# cnn_dat1 = list2ary(cnn_dat)
# print(Sys.time()-start_time)
# sampling###################################################
cnn_dat1=read_rds('./2_asc_cnn/cnn_asc_3darray.rds')
library(stringr)
class = dimnames(cnn_dat1)[[3]][grep('ASC_ORD',dimnames(cnn_dat1)[[3]])]
cnn_dat2 = array(numeric(),c(0,8,390));cnn_dat3 = array(numeric(),c(0,8,390))
dimnames(cnn_dat2)[[3]]=dimnames(cnn_dat1)[[3]]
dimnames(cnn_dat3)[[3]]=dimnames(cnn_dat1)[[3]]
k2=8
size = 3000
# extract test set before adding size
for (c in class) {
  temp =  cnn_dat1[cnn_dat1[,1,c]==1,,]
  idx = sample(nrow(temp),round(nrow(temp)*0.85),replace = F)
  cnn_dat3 = abind(cnn_dat3,temp[-idx,,],along = 1)
  if (nrow(temp) >= size) {
    cnn_dat2 = abind(cnn_dat2, temp[idx,,],along = 1)
  } else {
    cnn_dat2 = abind(cnn_dat2, temp[sample(idx,round(size*0.85),replace = T),,],along = 1)
  }
}
# data shuffling
cnn_dat2 = cnn_dat2[sample(nrow(cnn_dat2),nrow(cnn_dat2)),,]
cnn_dat3 = cnn_dat3[sample(nrow(cnn_dat3),nrow(cnn_dat3)),,]
library(keras)

cnn_train <- array_reshape(asub(cnn_dat2, -c(378:390),3), c(nrow(cnn_dat2), k2, 377, 1))
dimnames(cnn_train)[[3]]=dimnames(cnn_dat2)[[3]][1:377]
cnn_test <- array_reshape(asub(cnn_dat3, -c(378:390),3), c(nrow(cnn_dat3), k2, 377, 1))
dimnames(cnn_test)[[3]]=dimnames(cnn_dat2)[[3]][1:377]
cnn_tr_tag = asub(cnn_dat2,list(1,c(378:390)),c(2,3))
cnn_test_tag = asub(cnn_dat3,list(1,c(378:390)),c(2,3))
write_rds(cnn_train,'./2_asc_cnn/cnn_asc_train.rds')
write_rds(cnn_test,'./2_asc_cnn/cnn_asc_test.rds')
write_rds(cnn_tr_tag,'./2_asc_cnn/cnn_asc_tr_tag.rds')
write_rds(cnn_test_tag,'./2_asc_cnn/cnn_asc_test_tag.rds')
# cnn with validation######################
cnn_train = read_rds('./2_asc_cnn/cnn_asc_train.rds')
cnn_test=read_rds('./2_asc_cnn/cnn_asc_test.rds')
cnn_tr_tag=read_rds('./2_asc_cnn/cnn_asc_tr_tag.rds')
cnn_test_tag=read_rds('./2_asc_cnn/cnn_asc_test_tag.rds')
k2=8
library(keras)
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3 ), activation = "relu",
                 input_shape = c(k2, 377, 1),padding = 'same'
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
  layer_dense(units = 32, activation = "relu",kernel_regularizer = regularizer_l2(0.001)) %>%
  
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = 13, activation = "softmax")
model

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
#install_keras(tensorflow = 'gpu')
history <- model %>% fit(cnn_train, cnn_tr_tag, epochs = 30, batch_size =30,
                         validation_split =0.2)# validation_data=list(cnn_val,cnn_val_tag))

# cnn with epoch=6#########################
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3 ), activation = "relu",
                input_shape = c(k2, 377, 1),padding = 'same'
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
  layer_dense(units = 32, activation = "relu",kernel_regularizer = regularizer_l2(0.001)) %>%
  
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = 13, activation = "softmax")
model
model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
model
#cnn_train_n <- abind(cnn_train,cnn_val,along = 1)
#cnn_tr_tag_n <- rbind(cnn_tr_tag,cnn_val_tag)
history <- model %>% fit(cnn_train, cnn_tr_tag, epochs = 15, batch_size =30)

results <- model %>% evaluate(cnn_test, cnn_test_tag)

cnn_predict <- model %>% predict(cnn_test)
colnames(cnn_predict) = colnames(cnn_test_tag)
rownames(cnn_predict) = rownames(cnn_test_tag)
cnn_predict = as.data.frame(cnn_predict)
cnn_predict = cbind(colnames(cnn_predict)[apply(cnn_predict,1,which.max)],
                    colnames(cnn_test_tag)[apply(cnn_test_tag,1,which.max)], 
                    cnn_predict)
colnames(cnn_predict)[1:2]=c('PREDICT','REAL')
library(caret)
cnn_confusion=confusionMatrix(cnn_predict[,1],cnn_predict[,2])
cnn_confusion[["table"]]=cnn_confusion[["table"]][c(1,9,10,13,6,8,12,3,2,5,4,7,11),c(1,9,10,13,6,8,12,3,2,5,4,7,11)]
cnn_confusion$byClass=cnn_confusion$byClass[c(1,9,10,13,6,8,12,3,2,5,4,7,11),]
cnn_confusion

# library(nnet)
# cnn_predict1 = cnn_predict
# for ( i in 1: nrow(cnn_predict)) {
#   cnn_predict[i,which.is.max(cnn_predict[i,])] = 1
#   cnn_predict[i,-which.is.max(cnn_predict[i,])] = 0
# }
# model %>% save_model_hdf5("./2_asc_cnn/asc_cnn.h5")
# #test <- load_model_hdf5("cnn_ASC.h5")
# 
# library(OpenImageR)
# writeImage(cnn_train[1,,,],'test_image.jpg')
# test=image_load(
#   'test_image.jpg',
#   grayscale = T,
#   target_size = NULL,
#   interpolation = "nearest"
# )
# a = image_to_array(test, datwriteImage(cnn_train[1,,,],'test_image.jpg'))
# View(a)
