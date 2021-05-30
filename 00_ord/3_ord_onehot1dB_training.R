# libraries and data loading###############
library(caret)
library(readr)
library(tensorflow)
library(keras)
library(dplyr)
# load files generated from "2_ord_transform.r"
# for trainig data:
#                                         normalization         
#                               yes                               no
#resampling   yes       ord_tr_onehot1d_re.rds           ord_tr_onehot1d_nn_re.rds
#             no        ord_tr_onehot1d.rds              ord_tr_onehot1d_nn.rds
# for test and predict data:
#                                         normalization         
#                               yes                               no
#                       ord_ts(nd)_onehot1d.rds           ord_ts(nd)_onehot1d_nn.rds

ord_tr = readRDS('./00_ord/ord_tr_onehot1d_re.rds')
ord_ts = readRDS('./00_ord/ord_ts_onehot1d.rds')
ord_nd = readRDS('./00_ord/ord_nd_onehot1d.rds')
# split features and labels #######################
# list of label names 
ord_names = c('ASC_ORD_AN','ASC_ORD_OR','ASC_ORD_PO','ASC_ORD_VE','ASC_ORD_HY','ASC_ORD_KU','ASC_ORD_SO',
              'ASC_ORD_CH','ASC_ORD_CA','ASC_ORD_FE','ASC_ORD_DE','ASC_ORD_KA','ASC_ORD_RU','ASC_ORD_TE')
# split into tr,tr_tag, ts,ts_tag, and pred
tr_in_base = as.matrix(ord_tr[,-which(
  names(ord_tr) %in% c('PROJECT_CODE','SITE_ID',ord_names))])
ts_in_base = as.matrix(ord_ts[,-which(
  names(ord_ts) %in% c('PROJECT_CODE','SITE_ID',ord_names))])
pred_in_base = as.matrix(ord_nd[,-which(
  names(ord_nd) %in% c('PROJECT_CODE','SITE_ID',ord_names))])
tr_tag_base = ord_tr[,ord_names]
ts_tag_base = ord_ts[,c('PROJECT_CODE','SITE_ID',ord_names)]
# model training#########################################
# initialize
ord_models = list()
ord_performance = list()
ord_predict = cbind(ord_ts[,1:2],colnames(ts_tag_base[,-c(1,2)])[apply(ts_tag_base[,-c(1,2)],1,which.max)])
names(ord_predict)[3]='ORD_REAL'
# start the for loop through the labels
for (tag_name  in ord_names[1:13]) {
  # from ord_name to index
  idx = match(tag_name,ord_names)
  # + extract training and test data set according to index################
  #e.g, training data for OR will not include data  labeled with AN, and so on.
  if (idx==1) {
    tr = tr_in_base
    tr_tag = tr_tag_base
    ts = ts_in_base
    ts_tag = ts_tag_base
  } else if (idx==2) {
    tr = tr_in_base[tr_tag_base[,(idx-1)]==0,]
    tr_tag = tr_tag_base[tr_tag[,(idx-1)]==0,]
    ts = ts_in_base[ts_tag_base[,(idx+1)]==0,]
    ts_tag = ts_tag_base[ts_tag_base[,(idx+1)]==0,]
  } else {
    tr = tr_in_base[rowSums(tr_tag_base[,1:(idx-1)])==0,]
    tr_tag = tr_tag_base[rowSums(tr_tag_base[,1:(idx-1)])==0,]
    ts = ts_in_base[rowSums(ts_tag_base[,3:(idx+1)])==0,]
    ts_tag = ts_tag_base[rowSums(ts_tag_base[,3:(idx+1)])==0,]
  }
  # + model construct########################
  base_in = layer_input(shape(c(ncol(tr_in_base))),name = 'base_in')
  output = base_in %>%
    layer_dense(units = 256, activation = 'relu', name = 'dense_1') %>%
    layer_dense(units = 128, activation = 'relu', name = 'dense_2') %>%
    layer_dense(units = 64, activation = 'relu', name = 'dense_3') %>%
    layer_dense(units = 1, activation = 'sigmoid', name = 'output')
  model = keras_model(base_in,output)
  model %>% compile(
    optimizer=optimizer_rmsprop(0.0001),
    #loss = loss_binary_crossentropy,
    loss = "binary_crossentropy",#"sparse_categorical_crossentropy",
    metrics = "binary_accuracy"#metric_binary_accuracy#metric_sparse_categorical_crossentropy 
  )
  # + model fitting##############
  print(paste('Training',tag_name, 'Model:'))
  model %>% fit(
    x = tr,
    y = tr_tag[[tag_name]],
    epochs=30,
    batch_size=min(512,round(nrow(tr)/20)),
    class_weight = NULL,
    view_metrics = F,)
  # + training results################
  # record fitted models
  ord_models[[tag_name]] = model
  # use the fitted model to predict test data
  pred = ord_models[[tag_name]] %>% predict(ts)
  # retrieve predict back to full range
  pred_retrive = cbind(ts_tag[,1:2],pred) %>% right_join(.,ts_tag_base[,1:2])
  # names the column 
  names(pred_retrive)[length(pred_retrive)] = tag_name
  # NA -> 0
  pred_retrive[is.na(pred_retrive)]=0
  # recorded the predict results into a list: predict
  ord_predict[[tag_name]] = pred_retrive[[tag_name]]
  # generate and print confusion matrix, and record it into performance  as well
  pred[pred>0.5]=1
  pred[pred!=1]=0
  con = confusionMatrix(factor(pred),factor(ts_tag[[tag_name]]),positive = '1')
  ord_performance[[tag_name]] = append(ord_performance[[tag_name]],con)
  print(con)
  # fix the evaluation value of nan to 0
  con$byClass[is.nan(con$byClass)]=0
  # if all evaluation values( TPR,TNR,PPV,NPV, ACC)>70%
  if (sum(con$byClass[c(1:4,11)]>0.7)==5) { 
    ord_performance[[tag_name]][['Result']] = "Succes"
    print(paste('Model_',tag_name,' is succesful.',sep = ""))
  } else {
    ord_performance[[tag_name]][['Result']] = "Fail"
    print(paste('Model_',tag_name,' is failed.',sep = ""))
  }
  print('____________________________________________________________')
}
# dispaly the results###################
for (i in ord_names){
  print(i)
  print(ord_performance[[i]])
}
for (i in ord_names[1:13]){
  print(paste(i,':',ord_performance[[i]]$Result))
}
# write to files######################
write_rds(ord_models,"./00_ord/ord_models.rds")
write_rds(ord_performance,"./00_ord/ord_performance.rds")
write_rds(ord_predict,"./00_ord/ord_predict.rds")

# test##########################
# use the fitted model to predict test data
test_data = ts_in_base
test_tag = ts_tag_base
ord_ts_predict = test_tag[,1:2]
for (tag_name in ord_names[1:13]) {
  pred_test = ord_models[[tag_name]] %>% predict(test_data)
  # recorded the predict results into a list: predict
  ord_ts_predict[[tag_name]] = pred_test
}
ord_ts_predict[[ord_names[14]]] <- apply(ord_ts_predict[,ord_names[1:13]], 1, function(x) ifelse(max(x)>=0.5,0,1))
# ord_ts_predict[['Pred']] <- apply(ord_ts_predict[,ord_names], 1, function(x) which.max(x))
# ord_ts_predict[['first']] <- apply(ord_ts_predict[,ord_names], 1, function(x) which(x>0.5)[1])
# ord_ts_predict[['first_n']] <- colnames(ord_ts_predict[,ord_names])[apply(ord_ts_predict[,ord_names], 1, function(x) which(x>0.5)[1])]
# ord_ts_predict[['second']] <- colnames(ord_ts_predict[,ord_names])[apply(ord_ts_predict[,ord_names], 1, function(x) which(x>0.5)[2])]
# ord_ts_predict[['ORD_REAL']] 
# dispaly the results###################
asc_evaluation_single <- function(pred,tag) {
  pred = cbind(colnames(pred)[apply(pred,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
               colnames(pred)[apply(pred,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                              match(sort(x[x>0],decreasing = T)[2],x)
               ))],
               colnames(tag)[apply(tag,1,which.max)],
               pred)
  
  # pred = cbind(colnames(pred)[apply(pred,1,function(x) which(x>0.5)[1])],
  #              colnames(pred)[apply(pred,1,function(x) which(x>0.5)[2])],
  #              colnames(tag)[apply(tag,1,which.max)],
  #              pred)
  # 
  colnames(pred)[1:3]=c('ORD_PRE1','ORD_PRE2','ORD_REAL')
  pred$ORD_PRE_TOP2 = pred$ORD_PRE1
  pred=pred[,c(1,2,length(pred),3)]
  pred[] <- lapply(pred, as.character)
  pred[as.character(pred$ORD_PRE2)==as.character(pred$ORD_REAL),'ORD_PRE_TOP2'] =
    pred[as.character(pred$ORD_PRE2)==as.character(pred$ORD_REAL),'ORD_PRE2']
  pred[] = lapply(pred,as.factor)
  pred[is.na(pred)] = "NO"
  single = pred[,c(1,4)]
  single = rbind(single,
                 data.frame('ORD_PRE1'=rep(NA,
                                           length(single[which(is.na(match(single$ORD_PRE1,single$ORD_REAL))),'ORD_PRE1'])),
                            'ORD_REAL'=single[which(is.na(match(single$ORD_PRE1,single$ORD_REAL))),'ORD_PRE1']))
  single = rbind(single,
                 data.frame('ORD_PRE1'=single[which(is.na(match(single$ORD_REAL,single$ORD_PRE1))),'ORD_REAL'],
                            'ORD_REAL'=rep(NA,length(single[which(is.na(match(single$ORD_REAL,single$ORD_PRE1))),'ORD_REAL']))
                 ))
  
  pred_con=confusionMatrix(single[,1],single[,2])
  top2 = pred[,c(3,4)]
  top2 = rbind(top2,
               data.frame('ORD_PRE_TOP2'=rep(NA,
                                             length(top2[which(is.na(match(top2$ORD_PRE_TOP2,top2$ORD_REAL))),'ORD_PRE_TOP2'])),
                          'ORD_REAL'=top2[which(is.na(match(top2$ORD_PRE_TOP2,top2$ORD_REAL))),'ORD_PRE_TOP2']))
  top2 = rbind(top2,
               data.frame('ORD_PRE_TOP2'=top2[which(is.na(match(top2$ORD_REAL,top2$ORD_PRE_TOP2))),'ORD_REAL'],
                          'ORD_REAL'=rep(NA,length(top2[which(is.na(match(top2$ORD_REAL,top2$ORD_PRE_TOP2))),'ORD_REAL']))
               ))
  
  pred_con_top2 =confusionMatrix(top2[,1],top2[,2])
  
  return(list(pred_con,pred_con_top2))
}
ord_evaluate = asc_evaluation_single(ord_ts_predict[,ord_names],test_tag[,ord_names])
ord_evaluate[[1]]

ord_evaluate[[2]]
for (i in ord_names){
  print(i)
  print(ord_performance[[i]])
}
for (i in ord_names[1:13]){
  print(paste(i,':',ord_performance[[i]]$Result))
}
#predict######################
for (tag_name in ord_names[1:13]) {
  ord_pred = ord_models[[tagname]] %>% predict(pred_in_base)
}



a = cbind(colnames(a)[apply(a,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
             colnames(a)[apply(a,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                            match(sort(x[x>0],decreasing = T)[1],x),
                                                            match(sort(x[x>0],decreasing = T)[2],x)
             ))],
             a)
