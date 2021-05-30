library(abind)
library(stringr)
library(caret)
library(keras)
#  data preprocessing################
asc_1d_onehot_4 = readRDS('./0_general/asc_1d_onehot_4.rds')
# prune <- function(x,features,s=200) {
#   class = grep(features,colnames(x))
#   l=c()
#   for (i in class) {
#     count =  nrow(x[x[,i]==1,])
#     if (count < s) {
#       l=c(l,i)
#       #x = x[x[,i]==0,]
#     } else {print(paste(colnames(x[i]),count))}
#   }
#   return(x[,-l])
# }
# asc_1d_onehot_4 = prune(mlp_matlab_sub,"ASC_ORD",500)
# asc_1d_onehot_4 = prune(asc_1d_onehot_4,"SUBORD|GREAT_GROUP|SUBGROUP")

#asc_1d_onehot_4 = asc_1d_onehot_4[,c(1:81,91,82,86,90,87,88,83,84,85,89,92:length(asc_1d_onehot_4))]
#asc_1d_onehot_4 = mlp_matlab_sub[,c(1:81,93,92,91,82,86,90,87,88,94,83,84,85,89,95:length(mlp_matlab_sub))]


tr_rate=0.85
idx = sample(nrow(asc_1d_onehot_4),round(nrow(asc_1d_onehot_4)*tr_rate),replace = F)
mlp_asc_tr= asc_1d_onehot_4[idx,]
mlp_asc_test= asc_1d_onehot_4[-idx,]

mlp_so_ex = grep('COLOUR_CLASS|MOTT_',colnames(asc_1d_onehot_4))
mlp_so_ex_names = grep('COLOUR_CLASS|MOTT_',colnames(asc_1d_onehot_4),value = T)
mlp_asc_list = grep('ASC_ORD',colnames(asc_1d_onehot_4))
mlp_asc_names = grep('ASC_ORD',colnames(asc_1d_onehot_4),value = T)
mlp_so_list  = grep('SUBORD',colnames(asc_1d_onehot_4))
mlp_so_names  = grep('SUBORD',colnames(asc_1d_onehot_4),value=T)
mlp_so_ka_list = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE',colnames(asc_1d_onehot_4))
mlp_so_ka_names = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE',
                       colnames(asc_1d_onehot_4),value = T)
mlp_so_na_list = grep('SUBORD_ASC_CODE_IT|SUBORD_ASC_CODE_HR|SUBORD_ASC_CODE_HS|SUBORD_ASC_CODE_HT|SUBORD_ASC_CODE_HU|SUBORD_ASC_CODE_HV|SUBORD_ASC_CODE_HW|SUBORD_ASC_CODE_HX',colnames(asc_1d_onehot_4))
mlp_so_na_names = grep('SUBORD_ASC_CODE_IT|SUBORD_ASC_CODE_HR|SUBORD_ASC_CODE_HS|SUBORD_ASC_CODE_HT|SUBORD_ASC_CODE_HU|SUBORD_ASC_CODE_HV|SUBORD_ASC_CODE_HW|SUBORD_ASC_CODE_HX',colnames(asc_1d_onehot_4),value = T)
mlp_so_ca_list = grep('SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CV|SUBORD_ASC_CODE_DA|SUBORD_ASC_CODE_FB|SUBORD_ASC_CODE_CQ|SUBORD_ASC_CODE_BD',colnames(asc_1d_onehot_4))
mlp_so_ca_names = grep('SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CV|SUBORD_ASC_CODE_DA|SUBORD_ASC_CODE_FB|SUBORD_ASC_CODE_CQ|SUBORD_ASC_CODE_BD',colnames(asc_1d_onehot_4),value = T)
mlp_so_de_list = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC',colnames(asc_1d_onehot_4))
mlp_so_de_names = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC',
                       colnames(asc_1d_onehot_4),value = T)
mlp_so_ru_list = grep('SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_HG|SUBORD_ASC_CODE_AO|SUBORD_ASC_CODE_GV|SUBORD_ASC_CODE_ER|SUBORD_ASC_CODE_HH|SUBORD_ASC_CODE_CY',colnames(asc_1d_onehot_4))
mlp_so_ru_names = grep('SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_HG|SUBORD_ASC_CODE_AO|SUBORD_ASC_CODE_GV|SUBORD_ASC_CODE_ER|SUBORD_ASC_CODE_HH|SUBORD_ASC_CODE_CY',colnames(asc_1d_onehot_4),value = T)

mlp_so_hy_list = grep('SUBORD_ASC_CODE_IU|SUBORD_ASC_CODE_IV|SUBORD_ASC_CODE_CW|SUBORD_ASC_CODE_EW|SUBORD_ASC_CODE_BT|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EG|SUBORD_ASC_CODE_ED|SUBORD_ASC_CODE_DT',colnames(asc_1d_onehot_4))
mlp_so_hy_names = grep('SUBORD_ASC_CODE_IU|SUBORD_ASC_CODE_IV|SUBORD_ASC_CODE_CW|SUBORD_ASC_CODE_EW|SUBORD_ASC_CODE_BT|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EG|SUBORD_ASC_CODE_ED|SUBORD_ASC_CODE_DT',colnames(asc_1d_onehot_4),value = T)
mlp_so_te_list = grep('SUBORD_ASC_CODE_BF|SUBORD_ASC_CODE_BE|SUBORD_ASC_CODE_IL|SUBORD_ASC_CODE_IM|SUBORD_ASC_CODE_AW|SUBORD_ASC_CODE_CY|SUBORD_ASC_CODE_GZ|SUBORD_ASC_CODE_IN|SUBORD_ASC_CODE_IO|SUBORD_ASC_CODE_IP|SUBORD_ASC_CODE_IQ|SUBORD_ASC_CODE_IR',colnames(asc_1d_onehot_4))
mlp_so_te_names = grep('SUBORD_ASC_CODE_BF|SUBORD_ASC_CODE_BE|SUBORD_ASC_CODE_IL|SUBORD_ASC_CODE_IM|SUBORD_ASC_CODE_AW|SUBORD_ASC_CODE_CY|SUBORD_ASC_CODE_GZ|SUBORD_ASC_CODE_IN|SUBORD_ASC_CODE_IO|SUBORD_ASC_CODE_IP|SUBORD_ASC_CODE_IQ|SUBORD_ASC_CODE_IR',colnames(asc_1d_onehot_4),value = T)
mlp_so_ve_list = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE|SUBORD_ASC_CODE_AM',colnames(asc_1d_onehot_4))
mlp_so_ve_names = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE|SUBORD_ASC_CODE_AM',colnames(asc_1d_onehot_4),value = T)

mlp_gg_list = grep('GREAT_GROUP',colnames(asc_1d_onehot_4))#,value = T)
mlp_gg_names = grep('GREAT_GROUP',colnames(asc_1d_onehot_4),value = T)
mlp_gg_ve_list = grep('GREAT_GROUP_ASC_CODE_EI|GREAT_GROUP_ASC_CODE_GS|GREAT_GROUP_ASC_CODE_BH|GREAT_GROUP_ASC_CODE_DF|GREAT_GROUP_ASC_CODE_DW',colnames(asc_1d_onehot_4))
mlp_gg_ve_names = grep('GREAT_GROUP_ASC_CODE_EI|GREAT_GROUP_ASC_CODE_GS|GREAT_GROUP_ASC_CODE_BH|GREAT_GROUP_ASC_CODE_DF|GREAT_GROUP_ASC_CODE_DW',colnames(asc_1d_onehot_4),value = T)

mlp_sg_list = grep('SUBGROUP',colnames(asc_1d_onehot_4))#,value = T)
mlp_sg_names = grep('SUBGROUP',colnames(asc_1d_onehot_4),value = T)
mlp_sg_ve_list = grep('SUBGROUP_ASC_CODE_EG|SUBGROUP_ASC_CODE_EV|SUBGROUP_ASC_CODE_EU|SUBGROUP_ASC_CODE_BJ|SUBGROUP_ASC_CODE_DZ|SUBGROUP_ASC_CODE_GQ|SUBGROUP_ASC_CODE_BI|SUBGROUP_ASC_CODE_GG|SUBGROUP_ASC_CODE_GH|SUBGROUP_ASC_CODE_GI|SUBGROUP_ASC_CODE_BN|SUBGROUP_ASC_CODE_CU|SUBGROUP_ASC_CODE_GN|SUBGROUP_ASC_CODE_GK|SUBGROUP_ASC_CODE_GJ|SUBGROUP_ASC_CODE_EP|SUBGROUP_ASC_CODE_GA|SUBGROUP_ASC_CODE_FM|SUBGROUP_ASC_CODE_GO|SUBGROUP_ASC_CODE_BR|SUBGROUP_ASC_CODE_GB|SUBGROUP_ASC_CODE_FY|SUBGROUP_ASC_CODE_GL|SUBGROUP_ASC_CODE_BL|SUBGROUP_ASC_CODE_GM|SUBGROUP_ASC_CODE_BP|SUBGROUP_ASC_CODE_HE|SUBGROUP_ASC_CODE_FZ|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(asc_1d_onehot_4))
mlp_sg_ve_names = grep('SUBGROUP_ASC_CODE_EG|SUBGROUP_ASC_CODE_EV|SUBGROUP_ASC_CODE_EU|SUBGROUP_ASC_CODE_BJ|SUBGROUP_ASC_CODE_DZ|SUBGROUP_ASC_CODE_GQ|SUBGROUP_ASC_CODE_BI|SUBGROUP_ASC_CODE_GG|SUBGROUP_ASC_CODE_GH|SUBGROUP_ASC_CODE_GI|SUBGROUP_ASC_CODE_BN|SUBGROUP_ASC_CODE_CU|SUBGROUP_ASC_CODE_GN|SUBGROUP_ASC_CODE_GK|SUBGROUP_ASC_CODE_GJ|SUBGROUP_ASC_CODE_EP|SUBGROUP_ASC_CODE_GA|SUBGROUP_ASC_CODE_FM|SUBGROUP_ASC_CODE_GO|SUBGROUP_ASC_CODE_BR|SUBGROUP_ASC_CODE_GB|SUBGROUP_ASC_CODE_FY|SUBGROUP_ASC_CODE_GL|SUBGROUP_ASC_CODE_BL|SUBGROUP_ASC_CODE_GM|SUBGROUP_ASC_CODE_BP|SUBGROUP_ASC_CODE_HE|SUBGROUP_ASC_CODE_FZ|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(asc_1d_onehot_4),value = T)

mlp_sg_ve_tr = mlp_asc_tr[rowSums(mlp_asc_tr[,mlp_sg_ve_list])==1,]
mlp_sg_ve_test = mlp_asc_test[rowSums(mlp_asc_test[,mlp_sg_ve_list])==1,]

mlp_asc_in_tr = mlp_asc_tr[,-c(1:2,mlp_so_ex,mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)]
mlp_sg_ve_in_tr = mlp_sg_ve_tr[,-c(1:2,mlp_so_ex,mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)]

mlp_asc_tag_tr = mlp_asc_tr[,mlp_asc_list]
mlp_soex_tr = mlp_asc_tr[,mlp_so_ex]
mlp_soex_sg_ve_tr = mlp_sg_ve_tr[,mlp_so_ex]
mlp_so_tag_tr = mlp_asc_tr[,mlp_so_list]
mlp_so_ka_tag_tr = mlp_asc_tr[,mlp_so_ka_list]
mlp_so_ka_tag_tr$OTHERS = abs(1-rowSums(mlp_so_ka_tag_tr[,1:length(mlp_so_ka_tag_tr)]))
mlp_so_na_tag_tr = mlp_asc_tr[,mlp_so_na_list]
mlp_so_na_tag_tr$OTHERS = abs(1-rowSums(mlp_so_na_tag_tr[,1:length(mlp_so_na_tag_tr)]))
mlp_so_ca_tag_tr = mlp_asc_tr[,mlp_so_ca_list]
mlp_so_ca_tag_tr$OTHERS = abs(1-rowSums(mlp_so_ca_tag_tr[,1:length(mlp_so_ca_tag_tr)]))
mlp_so_de_tag_tr = mlp_asc_tr[,mlp_so_de_list]
mlp_so_de_tag_tr$OTHERS = abs(1-rowSums(mlp_so_de_tag_tr[,1:length(mlp_so_de_tag_tr)]))
mlp_so_hy_tag_tr = mlp_asc_tr[,mlp_so_hy_list]
mlp_so_hy_tag_tr$OTHERS = abs(1-rowSums(mlp_so_hy_tag_tr[,1:length(mlp_so_hy_tag_tr)]))
mlp_so_ru_tag_tr = mlp_asc_tr[,mlp_so_ru_list]
mlp_so_ru_tag_tr$OTHERS = abs(1-rowSums(mlp_so_ru_tag_tr[,1:length(mlp_so_ru_tag_tr)]))
mlp_so_te_tag_tr = mlp_asc_tr[,mlp_so_te_list]
mlp_so_te_tag_tr$OTHERS = abs(1-rowSums(mlp_so_te_tag_tr[,1:length(mlp_so_te_tag_tr)]))
mlp_so_ve_tag_tr = mlp_asc_tr[,mlp_so_ve_list]
mlp_so_ve_tag_tr$OTHERS = abs(1-rowSums(mlp_so_ve_tag_tr[,1:length(mlp_so_ve_tag_tr)]))

mlp_gg_tag_tr = mlp_asc_tr[,mlp_gg_list]
mlp_gg_ve_tag_tr = mlp_asc_tr[,mlp_gg_ve_list]
mlp_gg_ve_tag_tr$OTHERS = abs(1-rowSums(mlp_gg_ve_tag_tr[,1:length(mlp_gg_ve_tag_tr)]))

mlp_sg_tag_tr = mlp_asc_tr[,mlp_sg_list]
mlp_sg_ve_tag_tr_w = mlp_sg_ve_tr[,mlp_sg_ve_list]
mlp_sg_ve_tag_tr = mlp_asc_tr[,mlp_sg_ve_list]
mlp_sg_ve_tag_tr$OTHERS = abs(1-rowSums(mlp_sg_ve_tag_tr[,1:length(mlp_sg_ve_tag_tr)]))

mlp_asc_in_test = mlp_asc_test[,-c(1:2,mlp_so_ex,mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)]
mlp_sg_ve_in_test = mlp_sg_ve_test[,-c(1:2,mlp_so_ex,mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)]
mlp_asc_tag_test = mlp_asc_test[,mlp_asc_list]
mlp_soex_test = mlp_asc_test[,mlp_so_ex]
mlp_soex_sg_ve_test = mlp_sg_ve_test[,mlp_so_ex]
mlp_so_tag_test = mlp_asc_test[,mlp_so_list]
mlp_so_ka_tag_test = mlp_asc_test[,mlp_so_ka_list]
mlp_so_ka_tag_test$OTHERS = abs(1-rowSums(mlp_so_ka_tag_test[,1:length(mlp_so_ka_tag_test)]))
mlp_so_na_tag_test = mlp_asc_test[,mlp_so_na_list]
mlp_so_na_tag_test$OTHERS = abs(1-rowSums(mlp_so_na_tag_test[,1:length(mlp_so_na_tag_test)]))
mlp_so_ca_tag_test = mlp_asc_test[,mlp_so_ca_list]
mlp_so_ca_tag_test$OTHERS = abs(1-rowSums(mlp_so_ca_tag_test[,1:length(mlp_so_ca_tag_test)]))
mlp_so_de_tag_test = mlp_asc_test[,mlp_so_de_list]
mlp_so_de_tag_test$OTHERS = abs(1-rowSums(mlp_so_de_tag_test[,1:length(mlp_so_de_tag_test)]))
mlp_so_hy_tag_test = mlp_asc_test[,mlp_so_hy_list]
mlp_so_hy_tag_test$OTHERS = abs(1-rowSums(mlp_so_hy_tag_test[,1:length(mlp_so_hy_tag_test)]))
mlp_so_ru_tag_test = mlp_asc_test[,mlp_so_ru_list]
mlp_so_ru_tag_test$OTHERS = abs(1-rowSums(mlp_so_ru_tag_test[,1:length(mlp_so_ru_tag_test)]))
mlp_so_te_tag_test = mlp_asc_test[,mlp_so_te_list]
mlp_so_te_tag_test$OTHERS = abs(1-rowSums(mlp_so_te_tag_test[,1:length(mlp_so_te_tag_test)]))
mlp_so_ve_tag_test = mlp_asc_test[,mlp_so_ve_list]
mlp_so_ve_tag_test$OTHERS = abs(1-rowSums(mlp_so_ve_tag_test[,1:length(mlp_so_ve_tag_test)]))

mlp_gg_tag_test = mlp_asc_test[,mlp_gg_list]
mlp_gg_ve_tag_test = mlp_asc_test[,mlp_gg_ve_list]
mlp_gg_ve_tag_test$OTHERS = abs(1-rowSums(mlp_gg_ve_tag_test[,1:length(mlp_gg_ve_tag_test)]))

mlp_sg_tag_test = mlp_asc_test[,mlp_sg_list]
mlp_sg_ve_tag_test_w = mlp_sg_ve_test[,mlp_sg_ve_list]
mlp_sg_ve_tag_test = mlp_asc_test[,mlp_sg_ve_list]
mlp_sg_ve_tag_test$OTHERS = abs(1-rowSums(mlp_sg_ve_tag_test[,1:length(mlp_sg_ve_tag_test)]))



# ord model ####################
base_in = layer_input(shape(c(length(mlp_asc_in_tr))),name = "base_in_layer")
ord_out = base_in %>% 
  layer_dense(units = 128,activation = 'relu',name = 'ord_dense_128') %>%
  #layer_dense(units = 64,activation = 'relu',name = 'ord_dense_64') %>%
  layer_dense(units = length(mlp_asc_names),activation = 'softmax',name = 'ord_out_layer')
model_ord = keras_model(base_in,ord_out)
model_ord %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = metric_categorical_accuracy
)
history_ord <- model_ord %>% fit(as.matrix(mlp_asc_in_tr),
                                 as.matrix(mlp_asc_tag_tr),
                                 epochs=10,
                                 batch_size=512,
                                 validation_split=0.15)
ord_predict <- model_ord %>% predict(as.matrix(mlp_asc_in_test))
colnames(ord_predict) = colnames(mlp_asc_tag_test)
rownames(ord_predict) = rownames(mlp_asc_tag_test)
ord_predict = as.data.frame(ord_predict)

ord_predict = cbind(colnames(ord_predict)[apply(ord_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(ord_predict)[apply(ord_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                                match(sort(x[x>0],decreasing = T)[1],x),
                                                                                match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(mlp_asc_tag_test)[apply(mlp_asc_tag_test,1,which.max)],
                   ord_predict)

colnames(ord_predict)[1:3]=c('ORD_PRE1','ORD_PRE2','ORD_REAL')
ord_predict$ORD_PRE_TOP2 = ord_predict$ORD_PRE1
ord_predict[as.character(ord_predict$ORD_PRE2)==as.character(ord_predict$ORD_REAL),'ORD_PRE_TOP2'] =
  ord_predict[as.character(ord_predict$ORD_PRE2)==as.character(ord_predict$ORD_REAL),2]
ord_predict=ord_predict[,c(1,2,length(ord_predict),3)]
#
ord_con=confusionMatrix(ord_predict[,1],ord_predict[,4])
ord_con_top2 = confusionMatrix(ord_predict[,3],ord_predict[,4])
ord_con
ord_con_top2  
# so model#################
so_ex_in = layer_input(shape(c(length(mlp_soex_tr))),name = "so_in_layer")
so_in = layer_concatenate(list(base_in,so_ex_in,ord_out),axis = 1, name = 'so_con_in')
so_ve_out = so_in %>% 
  layer_dense(units = 64,activation = 'relu',name = 'so_dense_256') %>%
  #layer_dense(units =32,activation = 'relu',name = 'so_dense_128') %>%
  layer_dense(units = length(mlp_so_ve_names)+1,activation = 'softmax',name = 'so_ve_out_layer')
  #layer_dense(units = length(mlp_so_ve_names),activation = 'softmax',name = 'so_ve_out_layer')
model_so_ve = keras_model(list(base_in,so_ex_in),so_ve_out)
model_so_ve %>% compile(
  #optimizer=optimizer_rmsprop(lr=0.0001),
  optimizer="rmsprop",
  loss = "categorical_crossentropy",
  #optimizer="rmsprop",
  #loss = loss_binary_crossentropy,
  #loss = "categorical_crossentropy",
  metrics = metric_categorical_accuracy
)
model_so_ve
# so_class_weights <- function(x){ # x: the ord
#   
# }
history_so_ve <- model_so_ve %>% fit(list(as.matrix(mlp_asc_in_tr),as.matrix(mlp_soex_tr)),
                             as.matrix(mlp_so_ve_tag_tr),
                             epochs=10,
                             batch_size=512,
                             # class_weight= list("0"= 10,"1"=10,"2"= 10,"3"=10,"4"= 10,"5"=10,"6"= 10,"7"=10,"8"=10,"9"= 10,"10"=10,"11"=10,"12"=1),#,
                             #                    # list("0"= 100,"1"=100,"2"=100,"3"=0,"4"=100,"5"=0,"6"=100,"7"=0,"8"=0,"9"=0,"10"= 0,
                             #                    #      "11"=0,"12"= 0,"13"=0,"14"= 0,"15"=0,"16"= 0,"17"=0,"18"= 0,"19"=0,"20"= 0,
                             #                    #      "21"=0,"22"= 0,"23"=0,"24"= 0,"25"=0,"26"= 0,"27"=0,"28"= 0,"29"=0,"30"= 0,
                             #                    #      "31"=0,"32"= 0,"33"=0,"34"= 0,"35"=0,"36"= 0,"37"=0,"38"= 0,"39"=0,"40"= 0,
                             #                    #      "41"=0,"42"= 0,"43"=0,"44"= 0,"45"=0,"46"= 0,"47"=0,"48"= 0,"49"=0,"50"= 0,
                             #                    #      "51"=0,"52"= 0,"53"=0,"54"= 0,"55"=0,"56"= 0,"57"=0,"58"= 0,"59"=0,"60"= 0,
                             #                    #      "61"=0,"62"= 0,"63"=0),

                             validation_split=0.15)

so_predict <- model_so_ve %>% predict(list(as.matrix(mlp_asc_in_test),as.matrix(mlp_soex_test)))
colnames(so_predict) = colnames(mlp_so_ve_tag_test)
rownames(so_predict) = rownames(mlp_so_ve_tag_test)
so_predict = as.data.frame(so_predict)

so_predict = cbind(colnames(so_predict)[apply(so_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(so_predict)[apply(so_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(mlp_so_ve_tag_test)[apply(mlp_so_ve_tag_test,1,which.max)],
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
so_con_s
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
so_con_top2

# gg model#################
#so_ex_in = layer_input(shape(c(length(mlp_soex_tr))),name = "gg_in_layer")
gg_in = layer_concatenate(list(base_in,so_ex_in,ord_out,so_ve_out),axis = 1, name = 'gg_con_in')
gg_ve_out = gg_in %>% 
  layer_dense(units = 32,activation = 'relu',name = 'gg_dense_256') %>%
  #layer_dense(units =32,activation = 'relu',name = 'gg_dense_128') %>%
  layer_dense(units = length(mlp_gg_ve_names)+1,activation = 'softmax',name = 'gg_ve_out_layer')
#layer_dense(units = length(mlp_gg_ve_names),activation = 'softmax',name = 'gg_ve_out_layer')
model_gg_ve = keras_model(list(base_in,so_ex_in),gg_ve_out)
model_gg_ve %>% compile(
  #optimizer=optimizer_rmsprop(lr=0.0001),
  optimizer="rmsprop",
  loss = "categorical_crossentropy",
  #optimizer="rmsprop",
  #loss = loss_binary_crossentropy,
  #loss = "categorical_crossentropy",
  metrics = metric_categorical_accuracy
)
model_gg_ve
# gg_class_weights <- function(x){ # x: the ord
#   
# }
history_gg_ve <- model_gg_ve %>% fit(list(as.matrix(mlp_asc_in_tr),as.matrix(mlp_soex_tr)),
                                     as.matrix(mlp_gg_ve_tag_tr),
                                     epochs=10,
                                     batch_size=512,
                                     class_weight= list("0"= 5,"1"=3,"2"= 60,"3"=100,"4"= 1),#,
                                     #                    # list("0"= 100,"1"=100,"2"=100,"3"=0,"4"=100,"5"=0,"6"=100,"7"=0,"8"=0,"9"=0,"10"= 0,
                                     #                    #      "11"=0,"12"= 0,"13"=0,"14"= 0,"15"=0,"16"= 0,"17"=0,"18"= 0,"19"=0,"20"= 0,
                                     #                    #      "21"=0,"22"= 0,"23"=0,"24"= 0,"25"=0,"26"= 0,"27"=0,"28"= 0,"29"=0,"30"= 0,
                                     #                    #      "31"=0,"32"= 0,"33"=0,"34"= 0,"35"=0,"36"= 0,"37"=0,"38"= 0,"39"=0,"40"= 0,
                                     #                    #      "41"=0,"42"= 0,"43"=0,"44"= 0,"45"=0,"46"= 0,"47"=0,"48"= 0,"49"=0,"50"= 0,
                                     #                    #      "51"=0,"52"= 0,"53"=0,"54"= 0,"55"=0,"56"= 0,"57"=0,"58"= 0,"59"=0,"60"= 0,
                                     #                    #      "61"=0,"62"= 0,"63"=0),
                                     
                                     validation_split=0.15)

gg_predict <- model_gg_ve %>% predict(list(as.matrix(mlp_asc_in_test),as.matrix(mlp_soex_test)))
colnames(gg_predict) = colnames(mlp_gg_ve_tag_test)
rownames(gg_predict) = rownames(mlp_gg_ve_tag_test)
gg_predict = as.data.frame(gg_predict)

gg_predict = cbind(colnames(gg_predict)[apply(gg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(gg_predict)[apply(gg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(mlp_gg_ve_tag_test)[apply(mlp_gg_ve_tag_test,1,which.max)],
                   gg_predict)

colnames(gg_predict)[1:3]=c('SO_PRE1','SO_PRE2','SO_REAL')
gg_predict$SO_PRE_TOP2 = gg_predict$SO_PRE1
gg_predict[as.character(gg_predict$SO_PRE2)==as.character(gg_predict$SO_REAL),'SO_PRE_TOP2'] =
  gg_predict[as.character(gg_predict$SO_PRE2)==as.character(gg_predict$SO_REAL),2]
gg_predict=gg_predict[,c(1,2,length(gg_predict),3)]
# single matching
gg_single= gg_predict[,c(1,4)]
gg_single = rbind(gg_single,
                  data.frame('SO_PRE1'=rep(NA,
                                           length(gg_single[which(is.na(match(gg_single$SO_PRE1,gg_single$SO_REAL))),'SO_PRE1'])),
                             'SO_REAL'=gg_single[which(is.na(match(gg_single$SO_PRE1,gg_single$SO_REAL))),'SO_PRE1']))
gg_single = rbind(gg_single,
                  data.frame('SO_PRE1'=gg_single[which(is.na(match(gg_single$SO_REAL,gg_single$SO_PRE1))),'SO_REAL'],
                             'SO_REAL'=rep(NA,length(gg_single[which(is.na(match(gg_single$SO_REAL,gg_single$SO_PRE1))),'SO_REAL']))
                  ))

gg_con_s=confusionMatrix(gg_single[,1],gg_single[,2])
gg_con_s$overall
# top_2
gg_top2= gg_predict[,c(3,4)]
gg_top2 = rbind(gg_top2,
                data.frame('SO_PRE_TOP2'=rep(NA,
                                             length(gg_top2[which(is.na(match(gg_top2$SO_PRE_TOP2,gg_top2$SO_REAL))),'SO_PRE_TOP2'])),
                           'SO_REAL'=gg_top2[which(is.na(match(gg_top2$SO_PRE_TOP2,gg_top2$SO_REAL))),'SO_PRE_TOP2']))
gg_top2 = rbind(gg_top2,
                data.frame('SO_PRE_TOP2'=gg_top2[which(is.na(match(gg_top2$SO_REAL,gg_top2$SO_PRE_TOP2))),'SO_REAL'],
                           'SO_REAL'=rep(NA,length(gg_top2[which(is.na(match(gg_top2$SO_REAL,gg_top2$SO_PRE_TOP2))),'SO_REAL']))
                ))

gg_con_top2=confusionMatrix(gg_top2[,1],gg_top2[,2])
gg_con_top2$overall


# sg model#################
#so_ex_in = layer_input(shape(c(length(mlp_soex_tr))),name = "sg_in_layer")
sg_in = layer_concatenate(list(base_in,so_ex_in,ord_out,so_ve_out,gg_ve_out),axis = 1, name = 'sg_con_in')
sg_ve_out = sg_in %>% 
  layer_dense(units = 128,activation = 'relu',name = 'sg_dense_256') %>%
  #layer_dense(units =32,activation = 'relu',name = 'sg_dense_128') %>%
  layer_dense(units = length(mlp_sg_ve_names)+1,activation = 'softmax',name = 'sg_ve_out_layer')
#layer_dense(units = length(mlp_sg_ve_names),activation = 'softmax',name = 'sg_ve_out_layer')
model_sg_ve = keras_model(list(base_in,so_ex_in),sg_ve_out)
model_sg_ve %>% compile(
  #optimizer=optimizer_rmsprop(lr=0.0001),
  optimizer="rmsprop",
  loss = "categorical_crossentropy",
  #optimizer="rmsprop",
  #loss = loss_binary_crossentropy,
  #loss = "categorical_crossentropy",
  metrics = metric_categorical_accuracy
)
model_sg_ve
# sg_class_weights <- function(x){ # x: the ord
#   
# }
history_sg_ve <- model_sg_ve %>% fit(list(as.matrix(mlp_asc_in_tr),as.matrix(mlp_soex_tr)),
                                     as.matrix(mlp_sg_ve_tag_tr),
                                     epochs=15,
                                     batch_size=512,
                                     # class_weight= list("0"= 5,"1"=3,"2"= 60,"3"=100,"4"= 1),#,
                                     #                    # list("0"= 100,"1"=100,"2"=100,"3"=0,"4"=100,"5"=0,"6"=100,"7"=0,"8"=0,"9"=0,"10"= 0,
                                     #                    #      "11"=0,"12"= 0,"13"=0,"14"= 0,"15"=0,"16"= 0,"17"=0,"18"= 0,"19"=0,"20"= 0,
                                     #                    #      "21"=0,"22"= 0,"23"=0,"24"= 0,"25"=0,"26"= 0,"27"=0,"28"= 0,"29"=0,"30"= 0,
                                     #                    #      "31"=0,"32"= 0,"33"=0,"34"= 0,"35"=0,"36"= 0,"37"=0,"38"= 0,"39"=0,"40"= 0,
                                     #                    #      "41"=0,"42"= 0,"43"=0,"44"= 0,"45"=0,"46"= 0,"47"=0,"48"= 0,"49"=0,"50"= 0,
                                     #                    #      "51"=0,"52"= 0,"53"=0,"54"= 0,"55"=0,"56"= 0,"57"=0,"58"= 0,"59"=0,"60"= 0,
                                     #                    #      "61"=0,"62"= 0,"63"=0),
                                     
                                     validation_split=0.15)

sg_predict <- model_sg_ve %>% predict(list(as.matrix(mlp_asc_in_test),as.matrix(mlp_soex_test)))
colnames(sg_predict) = colnames(mlp_sg_ve_tag_test)
rownames(sg_predict) = rownames(mlp_sg_ve_tag_test)
sg_predict = as.data.frame(sg_predict)

sg_predict = cbind(colnames(sg_predict)[apply(sg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(mlp_sg_ve_tag_test)[apply(mlp_sg_ve_tag_test,1,which.max)],
                   sg_predict)

colnames(sg_predict)[1:3]=c('SO_PRE1','SO_PRE2','SO_REAL')
sg_predict$SO_PRE_TOP2 = sg_predict$SO_PRE1
sg_predict[as.character(sg_predict$SO_PRE2)==as.character(sg_predict$SO_REAL),'SO_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SO_PRE2)==as.character(sg_predict$SO_REAL),2]
sg_predict=sg_predict[,c(1,2,length(sg_predict),3)]
# single matching
sg_single= sg_predict[,c(1,4)]
sg_single = rbind(sg_single,
                  data.frame('SO_PRE1'=rep(NA,
                                           length(sg_single[which(is.na(match(sg_single$SO_PRE1,sg_single$SO_REAL))),'SO_PRE1'])),
                             'SO_REAL'=sg_single[which(is.na(match(sg_single$SO_PRE1,sg_single$SO_REAL))),'SO_PRE1']))
sg_single = rbind(sg_single,
                  data.frame('SO_PRE1'=sg_single[which(is.na(match(sg_single$SO_REAL,sg_single$SO_PRE1))),'SO_REAL'],
                             'SO_REAL'=rep(NA,length(sg_single[which(is.na(match(sg_single$SO_REAL,sg_single$SO_PRE1))),'SO_REAL']))
                  ))

sg_con_s=confusionMatrix(sg_single[,1],sg_single[,2])
sg_con_s
# top_2
sg_top2= sg_predict[,c(3,4)]
sg_top2 = rbind(sg_top2,
                data.frame('SO_PRE_TOP2'=rep(NA,
                                             length(sg_top2[which(is.na(match(sg_top2$SO_PRE_TOP2,sg_top2$SO_REAL))),'SO_PRE_TOP2'])),
                           'SO_REAL'=sg_top2[which(is.na(match(sg_top2$SO_PRE_TOP2,sg_top2$SO_REAL))),'SO_PRE_TOP2']))
sg_top2 = rbind(sg_top2,
                data.frame('SO_PRE_TOP2'=sg_top2[which(is.na(match(sg_top2$SO_REAL,sg_top2$SO_PRE_TOP2))),'SO_REAL'],
                           'SO_REAL'=rep(NA,length(sg_top2[which(is.na(match(sg_top2$SO_REAL,sg_top2$SO_PRE_TOP2))),'SO_REAL']))
                ))

sg_con_top2=confusionMatrix(sg_top2[,1],sg_top2[,2])
#sg_con_top2







# sg_without OTHERS model#################
#so_ex_in = layer_input(shape(c(length(mlp_soex_tr))),name = "sg_in_layer")
sg_in = layer_concatenate(list(base_in,so_ex_in),axis = 1, name = 'sg_con_in')
sg_ve_out = sg_in %>% 
  layer_dense(units = 128,activation = 'relu',name = 'sg_dense_256') %>% 
  #layer_dense(units =32,activation = 'relu',name = 'sg_dense_128') %>%
  #layer_dense(units = length(mlp_sg_ve_names)+1,activation = 'softmax',name = 'sg_ve_out_layer')
  layer_dense(units = length(mlp_sg_ve_names),activation = 'softmax',name = 'sg_ve_out_layer')
model_sg_ve = keras_model(list(base_in,so_ex_in),sg_ve_out)
model_sg_ve %>% compile(
  #optimizer=optimizer_rmsprop(lr=0.0001),
  optimizer="rmsprop",
  loss = "categorical_crossentropy",
  #optimizer="rmsprop",
  #loss = loss_binary_crossentropy,
  #loss = "categorical_crossentropy",
  metrics = metric_categorical_accuracy
)
model_sg_ve
# sg_class_weights <- function(x){ # x: the ord
#   
# }
history_sg_ve <- model_sg_ve %>% fit(list(as.matrix(mlp_sg_ve_in_tr),as.matrix(mlp_soex_sg_ve_tr)),
                                     as.matrix(mlp_sg_ve_tag_tr_w),
                                     epochs=10,
                                     batch_size=128,
                                     # class_weight= list("0"= 5,"1"=3,"2"= 60,"3"=100,"4"= 1),#,
                                     #                    # list("0"= 100,"1"=100,"2"=100,"3"=0,"4"=100,"5"=0,"6"=100,"7"=0,"8"=0,"9"=0,"10"= 0,
                                     #                    #      "11"=0,"12"= 0,"13"=0,"14"= 0,"15"=0,"16"= 0,"17"=0,"18"= 0,"19"=0,"20"= 0,
                                     #                    #      "21"=0,"22"= 0,"23"=0,"24"= 0,"25"=0,"26"= 0,"27"=0,"28"= 0,"29"=0,"30"= 0,
                                     #                    #      "31"=0,"32"= 0,"33"=0,"34"= 0,"35"=0,"36"= 0,"37"=0,"38"= 0,"39"=0,"40"= 0,
                                     #                    #      "41"=0,"42"= 0,"43"=0,"44"= 0,"45"=0,"46"= 0,"47"=0,"48"= 0,"49"=0,"50"= 0,
                                     #                    #      "51"=0,"52"= 0,"53"=0,"54"= 0,"55"=0,"56"= 0,"57"=0,"58"= 0,"59"=0,"60"= 0,
                                     #                    #      "61"=0,"62"= 0,"63"=0),
                                     
                                     validation_split=0.15)

sg_predict <- model_sg_ve %>% predict(list(as.matrix(mlp_sg_ve_in_test),as.matrix(mlp_soex_sg_ve_test)))
colnames(sg_predict) = colnames(mlp_sg_ve_tag_test_w)
rownames(sg_predict) = rownames(mlp_sg_ve_tag_test_w)
sg_predict = as.data.frame(sg_predict)

sg_predict = cbind(colnames(sg_predict)[apply(sg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(mlp_sg_ve_tag_test_w)[apply(mlp_sg_ve_tag_test_w,1,which.max)],
                   sg_predict)

colnames(sg_predict)[1:3]=c('SO_PRE1','SO_PRE2','SO_REAL')
sg_predict$SO_PRE_TOP2 = sg_predict$SO_PRE1
sg_predict[as.character(sg_predict$SO_PRE2)==as.character(sg_predict$SO_REAL),'SO_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SO_PRE2)==as.character(sg_predict$SO_REAL),2]
sg_predict=sg_predict[,c(1,2,length(sg_predict),3)]
# single matching
sg_single= sg_predict[,c(1,4)]
sg_single = rbind(sg_single,
                  data.frame('SO_PRE1'=rep(NA,
                                           length(sg_single[which(is.na(match(sg_single$SO_PRE1,sg_single$SO_REAL))),'SO_PRE1'])),
                             'SO_REAL'=sg_single[which(is.na(match(sg_single$SO_PRE1,sg_single$SO_REAL))),'SO_PRE1']))
sg_single = rbind(sg_single,
                  data.frame('SO_PRE1'=sg_single[which(is.na(match(sg_single$SO_REAL,sg_single$SO_PRE1))),'SO_REAL'],
                             'SO_REAL'=rep(NA,length(sg_single[which(is.na(match(sg_single$SO_REAL,sg_single$SO_PRE1))),'SO_REAL']))
                  ))

sg_con_s=confusionMatrix(sg_single[,1],sg_single[,2])
sg_con_s$overall
# top_2
sg_top2= sg_predict[,c(3,4)]
sg_top2 = rbind(sg_top2,
                data.frame('SO_PRE_TOP2'=rep(NA,
                                             length(sg_top2[which(is.na(match(sg_top2$SO_PRE_TOP2,sg_top2$SO_REAL))),'SO_PRE_TOP2'])),
                           'SO_REAL'=sg_top2[which(is.na(match(sg_top2$SO_PRE_TOP2,sg_top2$SO_REAL))),'SO_PRE_TOP2']))
sg_top2 = rbind(sg_top2,
                data.frame('SO_PRE_TOP2'=sg_top2[which(is.na(match(sg_top2$SO_REAL,sg_top2$SO_PRE_TOP2))),'SO_REAL'],
                           'SO_REAL'=rep(NA,length(sg_top2[which(is.na(match(sg_top2$SO_REAL,sg_top2$SO_PRE_TOP2))),'SO_REAL']))
                ))

sg_con_top2=confusionMatrix(sg_top2[,1],sg_top2[,2])
sg_con_top2$overall










