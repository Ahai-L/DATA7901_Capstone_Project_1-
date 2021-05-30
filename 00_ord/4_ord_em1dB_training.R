# libraries and data loading###############
library(readr)
library(keras)
library(dplyr)
library(caret)
# load files generated from "2_ord_transform.r"
#ord_em2d = readRDS('./00_ord/ord_em2d.rds')
ord_tr = readRDS('./00_ord/ord_tr_em1d_re.rds')
ord_ts = readRDS('./00_ord/ord_ts_em1d_nn.rds')
ord_nd = readRDS('./00_ord/ord_nd_em1d_nn.rds')
vocabrulary = readRDS('./00_ord/em_factor_dic.rds')
# list of label names
title = c('PROJECT_CODE','SITE_ID')
sn = c('DRAINAGE')
sn_idx = grep('DRAINAGE',names(ord_tr))
sc = c('ELEM_TYPE_CODE','STATUS')
sc_idx = grep('ELEM_TYPE_CODE|STATUS',names(ord_tr))
vn = grep('UPPER_DEPTH|LOWER_DEPTH|SOIL_WATER_STAT|FTS_PH|BOUND_DISTINCT|FE_13C1|ESP_15N1|CARBON_6B_6A|PH_4A1|Clay_2Z2|PEDALITY_GRADE|HOR_PREFIX'
          ,names(ord_tr),value = T)
vn_idx = grep('UPPER_DEPTH|LOWER_DEPTH|SOIL_WATER_STAT|FTS_PH|BOUND_DISTINCT|FE_13C1|ESP_15N1|CARBON_6B_6A|PH_4A1|Clay_2Z2|PEDALITY_GRADE|HOR_PREFIX'
          ,names(ord_tr))
vc = grep('PEDALITY_TYPE|NATURE|CUTAN_TYPE|TEXTURE_CODE|HOR_MASTER|HOR_SUBHOR|HOR_SUFFIX'
          ,names(ord_tr),value = T)
vc_idx = grep('PEDALITY_TYPE|NATURE|CUTAN_TYPE|TEXTURE_CODE|HOR_MASTER|HOR_SUBHOR|HOR_SUFFIX'
          ,names(ord_tr))
tag = c('ASC_ORD_AN','ASC_ORD_OR','ASC_ORD_PO','ASC_ORD_VE','ASC_ORD_HY','ASC_ORD_KU','ASC_ORD_SO',
        'ASC_ORD_CH','ASC_ORD_CA','ASC_ORD_FE','ASC_ORD_DE','ASC_ORD_KA','ASC_ORD_RU','ASC_ORD_TE')
num_idx = c(sn_idx,vn_idx)
num_names =c(sn,vn)
em_names = c(sc,vc)
em_idx = c(sc_idx,vc_idx)
tag_idx = grep('ASC_ORD',names(ord_tr))
# data preparation###################
tag_tr =  as.data.frame(ord_tr[,tag])
tag_ts =  as.data.frame(ord_ts[,c(title,tag)])
ord <- read_rds("./00_ord/ord_not_normalize.rds")
ord = ord[ord$ASC_CONFIDENCE==1 & ord$HORIZON_NO==1,c(title,'ASC_ORD')]
#rownames(ord) <- with(ord, paste(PROJECT_CODE, SITE_ID, sep = "."))
tag_ts =  merge(ord,tag_ts,by=title,sort = T)
tr_num = as.matrix(ord_tr[,num_names])
ts_num = as.matrix(ord_ts[,num_names])
tr_list = list()
ts_list = list()
j = 1
for (i in em_names) {
  tr_list[[j]] = assign(paste(i,'tr',sep = '_'), as.matrix(ord_tr[,i]))
  #append(tr_list,list(ord_tr[,,i]))
  ts_list[[j]] = assign(paste(i,'tr',sep = '_'), as.matrix(ord_ts[,i]))
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
      tr[[i]] = tr_list[[i]][tag_tr[,(idx-1)]==0]
      ts[[i]] = ts_list[[i]][tag_ts[,(idx+2)]==0]
    }
    tr[[length(tr_list)]] = tr_list[[length(tr_list)]][tag_tr[,(idx-1)]==0,]
    ts[[length(ts_list)]] = ts_list[[length(ts_list)]][tag_ts[,(idx+2)]==0,]
    tr_tag = tag_tr[tag_tr[,(idx-1)]==0,]
    ts_tag = tag_ts[tag_ts[,(idx+2)]==0,]
  } else {
    tr = tr_list
    ts = ts_list
    for (i in 1:(length(tr_list)-1)){
      tr[[i]] = tr_list[[i]][rowSums(tag_tr[,1:(idx-1)])==0]
      ts[[i]] = ts_list[[i]][rowSums(tag_ts[,4:(idx+2)])==0]
    }
    tr[[length(tr_list)]] = tr_list[[length(tr_list)]][rowSums(tag_tr[,1:(idx-1)])==0,]
    ts[[length(ts_list)]] = ts_list[[length(ts_list)]][rowSums(tag_ts[,4:(idx+2)])==0,]
    tr_tag = tag_tr[rowSums(tag_tr[,1:(idx-1)])==0,]
    ts_tag = tag_ts[rowSums(tag_ts[,4:(idx+2)])==0,]
  }
  # +model construct#############
  input_num = layer_input(c(length(c(sn,vn))),name='input_num')
  input_list = list()
  embed_layer_list = list()
  j=1
  for (i in em_names) {
    input_list = append(input_list,assign(paste(i,'in',sep='_'),
                                          layer_input(shape = c(1),name = paste(i,'in',sep='_'))))
    # k: original col names of vcs ( "NATURE_h_1" -> "NATURE)
    k = ifelse(i %in% vc, substr(i,1,nchar(i)-4),i)
    size = min(50,(round(0.5*vocabrulary[[k]])+1))
    embed_layer_list = append(embed_layer_list,assign(paste(i,'em',sep = '_'),
                                                      input_list[[j]] %>%
                                                        layer_embedding(input_dim = vocabrulary[[k]]+1,
                                                                        output_dim = size,
                                                                        mask_zero = T,
                                                                        name = paste(i,'em',sep = '_')) %>%
                                                        layer_reshape(c(size),name = paste(i,'1d',sep = '_')) ))
    j = j+1
  }
  input_list = append(input_list,input_num)
  em_con = layer_concatenate(embed_layer_list,axis = 1,name = 'em_con')
  input_con = layer_concatenate(list(input_num,em_con),axis = 1,name = 'input_con') 
  
  ord_out <- input_con %>%
    layer_dense(units = 512, activation = "relu") %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 1, activation = 'sigmoid', name = 'output')
  model = keras_model(input_list,ord_out)
  model %>% compile(
    optimizer= optimizer_rmsprop(0.0001),#optimizer_adam(lr=0.0001), #optimizer_rmsprop(0.0001),
    #loss = loss_binary_crossentropy,
    loss = "binary_crossentropy",#"sparse_categorical_crossentropy",
    metrics = metric_binary_accuracy #"binary_accuracy"#metric_binary_accuracy#metric_sparse_categorical_crossentropy 
  )  
  model
  # + model fitting##############
  print(paste('Training',tag_name, 'Model:'))
  model %>% fit(
    x = tr,
    y = tr_tag[[tag_name]],
    epochs=40,
    batch_size=min(1024,round(nrow(tr)/20)),
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
write_rds(ord_models,"./00_ord/ord_models_EM1DB.rds")
write_rds(ord_performance,"./00_ord/ord_performance_EM1DB.rds")
write_rds(ord_predict,"./00_ord/ord_predict_EM1DB.rds")


