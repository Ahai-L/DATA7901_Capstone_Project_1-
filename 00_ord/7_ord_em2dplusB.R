# libraries and data loading###############
library(caret)
library(readr)
library(tensorflow)
library(keras)
library(dplyr)
library(abind)
# load files generated from "2_ord_transform.r"
#ord_em2d = readRDS('./00_ord/ord_em2d.rds')
ord_tr = readRDS('./00_ord/ord_tr_em2d.rds')
ord_ts = readRDS('./00_ord/ord_ts_em2d.rds')
ord_nd = readRDS('./00_ord/ord_nd_em2d.rds')
vocabrulary = readRDS('./00_ord/em_factor_dic.rds')
# names and indexes  #######################
# list of label names
title = c('PROJECT_CODE','SITE_ID')
sn = c('DRAINAGE')
sc = c('ELEM_TYPE_CODE','STATUS')
vn = c('UPPER_DEPTH','LOWER_DEPTH','SOIL_WATER_STAT','FTS_PH','BOUND_DISTINCT','FE_13C1',
       'ESP_15N1','CARBON_6B_6A','PH_4A1','Clay_2Z2','PEDALITY_GRADE','HOR_PREFIX')
vc = c('PEDALITY_TYPE','NATURE','CUTAN_TYPE','TEXTURE_CODE','HOR_MASTER','HOR_SUBHOR','HOR_SUFFIX')

tag = c('ASC_ORD_AN','ASC_ORD_OR','ASC_ORD_PO','ASC_ORD_VE','ASC_ORD_HY','ASC_ORD_KU','ASC_ORD_SO',
        'ASC_ORD_CH','ASC_ORD_CA','ASC_ORD_FE','ASC_ORD_DE','ASC_ORD_KA','ASC_ORD_RU','ASC_ORD_TE')
num_idx = grep('DRAINAGE|UPPER_DEPTH|LOWER_DEPTH|SOIL_WATER_STAT|FTS_PH|BOUND_DISTINCT|FE_13C1|ESP_15N1|CARBON_6B_6A|PH_4A1|Clay_2Z2|PEDALITY_GRADE|HOR_PREFIX',
               dimnames(ord_tr)[[3]])
num_names =c(sn,vn)
em_names = c(sc,vc)
em_idx = grep('ELEM_TYPE_CODE|STATUS|PEDALITY_TYPE|NATURE|CUTAN_TYPE|TEXTURE_CODE|HOR_MASTER|HOR_SUBHOR|HOR_SUFFIX',
              dimnames(ord_tr)[[3]])
tag_idx = grep('ASC_ORD',dimnames(ord_tr)[[3]])

# data preparation###################
tag_tr =  as.data.frame(ord_tr[,1,tag])
tag_ts =  as.data.frame(ord_ts[,1,tag])#as.data.frame(ord_tr[,1,tag])
ord <- read_rds("./00_ord/ord_not_normalize.rds")
ord = ord[ord$ASC_CONFIDENCE==1 & ord$HORIZON_NO==1,c(title,'ASC_ORD')]
rownames(ord) <- with(ord, paste(PROJECT_CODE, SITE_ID, sep = "."))
tag_ts =  merge(ord,tag_ts,by="row.names",sort = F)[,-1]
tr_num = ord_tr[,,num_names]
ts_num = ord_ts[,,num_names]
tr_list = list()
ts_list = list()
j = 1
for (i in em_names) {
  tr_list[[j]] = assign(paste(i,'tr',sep = '_'), as.matrix(ord_tr[,,i]))
  #append(tr_list,list(ord_tr[,,i]))
  ts_list[[j]] = assign(paste(i,'tr',sep = '_'), as.matrix(ord_ts[,,i]))
  j = j+1
}
tr_list[[length(tr_list)+1]] = tr_num
ts_list[[length(ts_list)+1]] = ts_num
# model training#########################################
# initialize
ord_models = list()
ord_performance = list()
ord_predict = tag_ts[,1:3]
colnames(ord_predict)[3]='ORD_REAL'
# start the for loop through the labels
for (tag_name  in tag[1:13]) {
  # from ord_name to index
  idx = match(tag_name,tag)
  # + extract training and test data set according to index################
  #e.g, training data for OR will not include data  labeled with AN, and so on.
  if (idx==1) {
    tr = tr_list
    tr_tag = tag_tr
    ts = ts_list
    ts_tag = tag_ts
  } else if (idx==2) {
    tr = tr_list
    ts = ts_list
    for (i in 1:(length(tr_list)-1)){
      tr[[i]] = tr_list[[i]][tag_tr[,(idx-1)]==0,]
      ts[[i]] = ts_list[[i]][tag_ts[,(idx+2)]==0,]
    }
    tr[[length(tr_list)]] = tr_list[[length(tr_list)]][tag_tr[,(idx-1)]==0,,]
    ts[[length(ts_list)]] = ts_list[[length(ts_list)]][tag_ts[,(idx+2)]==0,,]
    tr_tag = tag_tr[tag_tr[,(idx-1)]==0,]
    ts_tag = tag_ts[tag_ts[,(idx+2)]==0,]
  } else {
    tr = tr_list
    ts = ts_list
    for (i in 1:(length(tr_list)-1)){
      tr[[i]] = tr_list[[i]][rowSums(tag_tr[,1:(idx-1)])==0,]
      ts[[i]] = ts_list[[i]][rowSums(tag_ts[,4:(idx+2)])==0,]
    }
    tr[[length(tr_list)]] = tr_list[[length(tr_list)]][rowSums(tag_tr[,1:(idx-1)])==0,,]
    ts[[length(ts_list)]] = ts_list[[length(ts_list)]][rowSums(tag_ts[,4:(idx+2)])==0,,]
    tr_tag = tag_tr[rowSums(tag_tr[,1:(idx-1)])==0,]
    ts_tag = tag_ts[rowSums(tag_ts[,4:(idx+2)])==0,]
  }
  # +model construct#############
  k2=8
  input_num = layer_input(c(k2,length(c(sn,vn))),name='input_num')
  input_list = list()
  embed_layer_list = list()
  j=1
  for (i in em_names) {
    input_list = append(input_list,assign(paste(i,'in',sep='_'),
                                          layer_input(shape = c(k2),name = paste(i,'in',sep='_'))))
    size = min(50,(round(0.5*vocabrulary[[i]])+1))
    embed_layer_list = append(embed_layer_list,assign(paste(i,'em',sep = '_'),
                                                      input_list[[j]] %>%
                                                        layer_embedding(input_dim = vocabrulary[[i]]+1,
                                                                        output_dim = size,
                                                                        mask_zero = T,
                                                                        name = paste(i,'em',sep = '_'))))
    j = j+1
  }
  input_list = append(input_list,input_num)
  em_con = layer_concatenate(embed_layer_list,axis = 2,name = 'em_con')
  input_con = layer_concatenate(list(input_num,em_con),axis = 2,name = 'input_con') %>%
    layer_reshape(c(k2,input_num$shape[[3]]+ em_con$shape[[3]],1),name = 'input_reshape')
  ord_out <- input_con %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                  padding = 'same') %>%#,data_format='channels_first') #%>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%#,data_format='channels_first') #%>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                  padding = 'same') %>%#,data_format='channels_first') %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%#,data_format='channels_first') %>%
    layer_flatten %>% 
    #layer_dropout(rate = 0.5) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 1, activation = 'sigmoid', name = 'output')
  model = keras_model(input_list,ord_out)
  model %>% compile(
    optimizer=optimizer_rmsprop(0.0001),
    #loss = loss_binary_crossentropy,
    loss = "binary_crossentropy",#"sparse_categorical_crossentropy",
    metrics = "binary_accuracy"#metric_binary_accuracy#metric_sparse_categorical_crossentropy 
  )  
  #model
  # + model fitting##############
  print(paste('Training',tag_name, 'Model:'))
  model %>% fit(
    x = tr,
    y = tr_tag[[tag_name]],
    epochs=30,
    batch_size=min(512,round(nrow(tr)/20)),
    class_weight = NULL,
    view_metrics = F,
    validation_data = list(ts,ts_tag[[tag_name]]))
  # + training results################
  # record fitted models
  ord_models[[tag_name]] = model
  # use the fitted model to predict test data
  pred = ord_models[[tag_name]] %>% predict(ts)
  # retrieve predict back to full range
  pred_retrive = cbind(ts_tag[,1:2],pred) %>% right_join(.,tag_ts[,1:2])
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
for (i in tag){
  print(i)
  print(ord_performance[[i]])
}
for (i in tag[1:13]){
  print(paste(i,':',ord_performance[[i]]$Result))
}
# write to files######################
write_rds(ord_models,"./00_ord/ord_models_EM2DB.rds")
write_rds(ord_performance,"./00_ord/ord_performance_EM2DB.rds")
write_rds(ord_predict,"./00_ord/ord_predictEM2DB.rds")

# test##########################
# use the fitted model to predict test data
test_data = ts_in_base
test_tag = tag_ts
ord_ts_predict = test_tag[,1:2]
for (tag_name in tag[1:13]) {
  pred_test = ord_models[[tag_name]] %>% predict(test_data)
  # recorded the predict results into a list: predict
  ord_ts_predict[[tag_name]] = pred_test
}
ord_ts_predict[[tag[14]]] <- apply(ord_ts_predict[,tag[1:13]], 1, function(x) ifelse(max(x)>=0.5,0,1))
# ord_ts_predict[['Pred']] <- apply(ord_ts_predict[,tag], 1, function(x) which.max(x))
# ord_ts_predict[['first']] <- apply(ord_ts_predict[,tag], 1, function(x) which(x>0.5)[1])
# ord_ts_predict[['first_n']] <- colnames(ord_ts_predict[,tag])[apply(ord_ts_predict[,tag], 1, function(x) which(x>0.5)[1])]
# ord_ts_predict[['second']] <- colnames(ord_ts_predict[,tag])[apply(ord_ts_predict[,tag], 1, function(x) which(x>0.5)[2])]
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
ord_evaluate = asc_evaluation_single(ord_ts_predict[,tag],test_tag[,tag])
ord_evaluate[[1]]

ord_evaluate[[2]]
for (i in tag){
  print(i)
  print(ord_performance[[i]])
}
for (i in tag[1:13]){
  print(paste(i,':',ord_performance[[i]]$Result))
}
#predict######################
for (tag_name in tag[1:13]) {
  ord_pred = ord_models[[tagname]] %>% predict(pred_in_base)
}



a = cbind(colnames(a)[apply(a,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
          colnames(a)[apply(a,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                   match(sort(x[x>0],decreasing = T)[1],x),
                                                   match(sort(x[x>0],decreasing = T)[2],x)
          ))],
          a)
