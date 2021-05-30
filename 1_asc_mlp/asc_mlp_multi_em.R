library(abind)
library(stringr)
library(caret)
library(keras)
library(crayon)
# # so model not in use#################
# # generate input layer, embedding layer, reshape to null x 50, and strore in a list
# input_layer_list_so = list(); embed_layer_list_so = list();j = 1
# for (i in sc_vc_names) {
#   input_layer_list_so = append(input_layer_list_so,
#                                assign(paste(i, 'in','so', sep = "_"),
#                                       layer_input(shape = c(1),name = paste(i,'in','so',sep = '_'))))
#   vocabrulary = length(unique(asc_1d_em[[i]]))
#   size = min(50,(round(0.5*vocabrulary)+1))
#   embed_layer_list_so = append(embed_layer_list_so,assign(paste(i,'1d','so',sep = '_'), 
#                                                           assign(paste(i,'em','so',sep='_'), 
#                                                                  input_layer_list_so[[j]] %>%
#                                                                    layer_embedding(input_dim = vocabrulary+1,
#                                                                                    output_dim = size,
#                                                                                    mask_zero = T,
#                                                                                    name = paste(i,'em','so',sep='_'))) %>%
#                                                             layer_reshape(c(size),name = paste(i,'1d','so',sep = '_'))))
#   j = j+1
# }
# sn_vn_in_so = layer_input(shape(c(length(sn_vn_list))),name = 'sn_vn_in_so')
# con_layer_list_so = append(embed_layer_list_so,sn_vn_in_so)
# input_layer_list_so = append(input_layer_list_so,sn_vn_in_so)
# base_in_so = layer_concatenate(con_layer_list_so,name = 'base_in_so')
# # so_ex layer
# so_ex_layer_list = list(); so_ex_embed_layer_list = list();j = 1
# for (i in mlp_so_ex_names) {
#   so_ex_layer_list = append(so_ex_layer_list,
#                             assign(paste(i, 'in', sep = "_"),
#                                    layer_input(shape = c(1),name = paste(i,'in',sep = '_'))))
#   vocabrulary = length(unique(asc_1d_em[[i]]))
#   size = min(50,(round(0.5*vocabrulary)+1))
#   so_ex_embed_layer_list = append(so_ex_embed_layer_list,assign(paste(i,'1d',sep = '_'), 
#                                                                 assign(paste(i,'em',sep='_'), 
#                                                                        so_ex_layer_list[[j]] %>%
#                                                                          layer_embedding(input_dim = vocabrulary+1,
#                                                                                          output_dim = size,
#                                                                                          mask_zero = T,
#                                                                                          name = paste(i,'em',sep='_'))) %>%
#                                                                   layer_reshape(c(size),name = paste(i,'1d',sep = '_'))))
#   j = j+1
# }
# so_ex_in = layer_concatenate(so_ex_embed_layer_list,name = 'so_ex_in')
# 
# so_in = layer_concatenate(list(base_in_so,so_ex_in), name = 'so_con_in')
# so_ve_out = so_in %>% 
#   layer_dense(units = 256,activation = 'relu',name = 'so_dense_256') %>%
#   #layer_dense(units =32,activation = 'relu',name = 'so_dense_128') %>%
#   layer_dense(units = length(mlp_so_ve_names)+1,activation = 'softmax',name = 'so_ve_out_layer')
# #layer_dense(units = length(mlp_so_ve_names),activation = 'softmax',name = 'so_ve_out_layer')
# model_so_ve = keras_model(c(so_ex_layer_list,input_layer_list_so),so_ve_out)
# model_so_ve %>% compile(
#   #optimizer=optimizer_rmsprop(lr=0.0001),
#   optimizer="rmsprop",
#   loss = "categorical_crossentropy",
#   #optimizer="rmsprop",
#   #loss = loss_binary_crossentropy,
#   metrics = metric_categorical_accuracy
# )
# model_so_ve
# model_so_ve %>% save_model_hdf5('./1_asc_mlp/asc_1d_multi_em_so_ve.h5')
# history_so_ve <- model_so_ve %>% fit(c(tr_so_ex_in_list,tr_in_list),
#                                      as.matrix(mlp_so_ve_tag_tr),
#                                      epochs=30,
#                                      batch_size=512,
#                                      # class_weight= list("0"= 10,"1"=10,"2"= 10,"3"=10,"4"= 10,"5"=10,"6"= 10,"7"=10,"8"=10,"9"= 10,"10"=10,"11"=10,"12"=1),#,
#                                      #                    # list("0"= 100,"1"=100,"2"=100,"3"=0,"4"=100,"5"=0,"6"=100,"7"=0,"8"=0,"9"=0,"10"= 0,
#                                      #                    #      "11"=0,"12"= 0,"13"=0,"14"= 0,"15"=0,"16"= 0,"17"=0,"18"= 0,"19"=0,"20"= 0,
#                                      #                    #      "21"=0,"22"= 0,"23"=0,"24"= 0,"25"=0,"26"= 0,"27"=0,"28"= 0,"29"=0,"30"= 0,
#                                      #                    #      "31"=0,"32"= 0,"33"=0,"34"= 0,"35"=0,"36"= 0,"37"=0,"38"= 0,"39"=0,"40"= 0,
#                                      #                    #      "41"=0,"42"= 0,"43"=0,"44"= 0,"45"=0,"46"= 0,"47"=0,"48"= 0,"49"=0,"50"= 0,
#                                      #                    #      "51"=0,"52"= 0,"53"=0,"54"= 0,"55"=0,"56"= 0,"57"=0,"58"= 0,"59"=0,"60"= 0,
#                                      #                    #      "61"=0,"62"= 0,"63"=0),
#                                      
#                                      validation_split=0.15
# )
# 
# so_predict <- model_so_ve %>% predict(c(test_so_ex_in_list,test_in_list))
# colnames(so_predict) = colnames(mlp_so_ve_tag_test)
# rownames(so_predict) = rownames(mlp_so_ve_tag_test)
# so_predict = as.data.frame(so_predict)
# 
# so_predict = cbind(colnames(so_predict)[apply(so_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
#                    colnames(so_predict)[apply(so_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
#                                                                               match(sort(x[x>0],decreasing = T)[1],x),
#                                                                               match(sort(x[x>0],decreasing = T)[2],x)
#                    ))],
#                    colnames(mlp_so_ve_tag_test)[apply(mlp_so_ve_tag_test,1,which.max)],
#                    so_predict)
# 
# colnames(so_predict)[1:3]=c('SO_PRE1','SO_PRE2','SO_REAL')
# so_predict$SO_PRE_TOP2 = so_predict$SO_PRE1
# so_predict[as.character(so_predict$SO_PRE2)==as.character(so_predict$SO_REAL),'SO_PRE_TOP2'] =
#   so_predict[as.character(so_predict$SO_PRE2)==as.character(so_predict$SO_REAL),2]
# so_predict=so_predict[,c(1,2,length(so_predict),3)]
# # single matching
# so_single= so_predict[,c(1,4)]
# so_single = rbind(so_single,
#                   data.frame('SO_PRE1'=rep(NA,
#                                            length(so_single[which(is.na(match(so_single$SO_PRE1,so_single$SO_REAL))),'SO_PRE1'])),
#                              'SO_REAL'=so_single[which(is.na(match(so_single$SO_PRE1,so_single$SO_REAL))),'SO_PRE1']))
# so_single = rbind(so_single,
#                   data.frame('SO_PRE1'=so_single[which(is.na(match(so_single$SO_REAL,so_single$SO_PRE1))),'SO_REAL'],
#                              'SO_REAL'=rep(NA,length(so_single[which(is.na(match(so_single$SO_REAL,so_single$SO_PRE1))),'SO_REAL']))
#                   ))
# 
# so_con_s=confusionMatrix(so_single[,1],so_single[,2])
# so_con_s
# # top_2
# so_top2= so_predict[,c(3,4)]
# so_top2 = rbind(so_top2,
#                 data.frame('SO_PRE_TOP2'=rep(NA,
#                                              length(so_top2[which(is.na(match(so_top2$SO_PRE_TOP2,so_top2$SO_REAL))),'SO_PRE_TOP2'])),
#                            'SO_REAL'=so_top2[which(is.na(match(so_top2$SO_PRE_TOP2,so_top2$SO_REAL))),'SO_PRE_TOP2']))
# so_top2 = rbind(so_top2,
#                 data.frame('SO_PRE_TOP2'=so_top2[which(is.na(match(so_top2$SO_REAL,so_top2$SO_PRE_TOP2))),'SO_REAL'],
#                            'SO_REAL'=rep(NA,length(so_top2[which(is.na(match(so_top2$SO_REAL,so_top2$SO_PRE_TOP2))),'SO_REAL']))
#                 ))
# 
# so_con_top2=confusionMatrix(so_top2[,1],so_top2[,2])
# so_con_top2
# 
# 

#  data preprocessing################
asc_1d_em = readRDS('./0_general/asc_1d_em.rds')
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
# asc_1d_em = prune(mlp_matlab_sub,"ASC_ORD",500)
# asc_1d_em = prune(asc_1d_em,"SUBORD|GREAT_GROUP|SUBGROUP")

#asc_1d_em = asc_1d_em[,c(1:81,91,82,86,90,87,88,83,84,85,89,92:length(asc_1d_em))]
#asc_1d_em = mlp_matlab_sub[,c(1:81,93,92,91,82,86,90,87,88,94,83,84,85,89,95:length(mlp_matlab_sub))]


tr_rate=0.85
idx = sample(nrow(asc_1d_em),round(nrow(asc_1d_em)*tr_rate),replace = F)
mlp_asc_tr= asc_1d_em[idx,]
mlp_asc_test= asc_1d_em[-idx,]


# index list and name list################
sn_vn_list = grep('DRAINAGE|FTS_|UPPER_|LOWER_|HOR_PREFIX|VALUE_',colnames(asc_1d_em))
sn_vn_names = grep('DRAINAGE|FTS_|UPPER_|LOWER_|HOR_PREFIX|VALUE_',colnames(asc_1d_em),value = T)
sc_vc_list = grep('STATUS|ELEM|PEDALITY|BOUND|TEXTURE|SUBHOR|SUFFIX|MASTER|CUTAN|NATURE|SOIL',colnames(asc_1d_em))
sc_vc_names = grep('STATUS|ELEM|PEDALITY|BOUND|TEXTURE|SUBHOR|SUFFIX|MASTER|CUTAN|NATURE|SOIL',
                   colnames(asc_1d_em),value = T)
mlp_so_ex = grep('COLOUR_CLASS|MOTT_',colnames(asc_1d_em))
mlp_so_ex_names = grep('COLOUR_CLASS|MOTT_',colnames(asc_1d_em),value = T)
mlp_asc_list = grep('ASC_ORD',colnames(asc_1d_em))
mlp_asc_names = grep('ASC_ORD',colnames(asc_1d_em),value = T)
mlp_so_list  = grep('SUBORD',colnames(asc_1d_em))
mlp_so_names  = grep('SUBORD',colnames(asc_1d_em),value=T)
mlp_so_ka_list = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE',colnames(asc_1d_em))
mlp_so_ka_names = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE',
                       colnames(asc_1d_em),value = T)
mlp_so_or_list = grep('SUBORD_ASC_CODE_BW|SUBORD_ASC_CODE_CE|SUBORD_ASC_CODE_EH',colnames(asc_1d_em))
mlp_so_or_names = grep('SUBORD_ASC_CODE_BW|SUBORD_ASC_CODE_CE|SUBORD_ASC_CODE_EH',colnames(asc_1d_em),value = T)

mlp_so_po_list = grep('SUBORD_ASC_CODE_AL|SUBORD_ASC_CODE_EJ|SUBORD_ASC_CODE_JD|SUBORD_ASC_CODE_JE|SUBORD_ASC_CODE_AM',colnames(asc_1d_em))
mlp_so_po_names = grep('SUBORD_ASC_CODE_AL|SUBORD_ASC_CODE_EJ|SUBORD_ASC_CODE_JD|SUBORD_ASC_CODE_JE|SUBORD_ASC_CODE_AM',colnames(asc_1d_em),value = T)

mlp_so_an_list = grep('SUBORD_ASC_CODE_IT|SUBORD_ASC_CODE_HR|SUBORD_ASC_CODE_HS|SUBORD_ASC_CODE_HT|SUBORD_ASC_CODE_HU|SUBORD_ASC_CODE_HV|SUBORD_ASC_CODE_HW|SUBORD_ASC_CODE_HX',colnames(asc_1d_em))
mlp_so_an_names = grep('SUBORD_ASC_CODE_IT|SUBORD_ASC_CODE_HR|SUBORD_ASC_CODE_HS|SUBORD_ASC_CODE_HT|SUBORD_ASC_CODE_HU|SUBORD_ASC_CODE_HV|SUBORD_ASC_CODE_HW|SUBORD_ASC_CODE_HX',colnames(asc_1d_em),value = T)
mlp_so_ca_list = grep('SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CV|SUBORD_ASC_CODE_DA|SUBORD_ASC_CODE_FB|SUBORD_ASC_CODE_CQ|SUBORD_ASC_CODE_BD',colnames(asc_1d_em))
mlp_so_ca_names = grep('SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CV|SUBORD_ASC_CODE_DA|SUBORD_ASC_CODE_FB|SUBORD_ASC_CODE_CQ|SUBORD_ASC_CODE_BD',colnames(asc_1d_em),value = T)
mlp_so_de_list = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC',colnames(asc_1d_em))
mlp_so_de_names = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC',
                       colnames(asc_1d_em),value = T)
mlp_so_ru_list = grep('SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_HG|SUBORD_ASC_CODE_AO|SUBORD_ASC_CODE_GV|SUBORD_ASC_CODE_ER|SUBORD_ASC_CODE_HH|SUBORD_ASC_CODE_CY',colnames(asc_1d_em))
mlp_so_ru_names = grep('SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_HG|SUBORD_ASC_CODE_AO|SUBORD_ASC_CODE_GV|SUBORD_ASC_CODE_ER|SUBORD_ASC_CODE_HH|SUBORD_ASC_CODE_CY',colnames(asc_1d_em),value = T)

mlp_so_hy_list = grep('SUBORD_ASC_CODE_IU|SUBORD_ASC_CODE_IV|SUBORD_ASC_CODE_CW|SUBORD_ASC_CODE_EW|SUBORD_ASC_CODE_BT|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EG|SUBORD_ASC_CODE_ED|SUBORD_ASC_CODE_DT',colnames(asc_1d_em))
mlp_so_hy_names = grep('SUBORD_ASC_CODE_IU|SUBORD_ASC_CODE_IV|SUBORD_ASC_CODE_CW|SUBORD_ASC_CODE_EW|SUBORD_ASC_CODE_BT|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EG|SUBORD_ASC_CODE_ED|SUBORD_ASC_CODE_DT',colnames(asc_1d_em),value = T)
mlp_so_te_list = grep('SUBORD_ASC_CODE_BF|SUBORD_ASC_CODE_BE|SUBORD_ASC_CODE_IL|SUBORD_ASC_CODE_IM|SUBORD_ASC_CODE_AW|SUBORD_ASC_CODE_CY|SUBORD_ASC_CODE_GZ|SUBORD_ASC_CODE_IN|SUBORD_ASC_CODE_IO|SUBORD_ASC_CODE_IP|SUBORD_ASC_CODE_IQ|SUBORD_ASC_CODE_IR',colnames(asc_1d_em))
mlp_so_te_names = grep('SUBORD_ASC_CODE_BF|SUBORD_ASC_CODE_BE|SUBORD_ASC_CODE_IL|SUBORD_ASC_CODE_IM|SUBORD_ASC_CODE_AW|SUBORD_ASC_CODE_CY|SUBORD_ASC_CODE_GZ|SUBORD_ASC_CODE_IN|SUBORD_ASC_CODE_IO|SUBORD_ASC_CODE_IP|SUBORD_ASC_CODE_IQ|SUBORD_ASC_CODE_IR',colnames(asc_1d_em),value = T)
mlp_so_ve_list = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE|SUBORD_ASC_CODE_AM',colnames(asc_1d_em))
mlp_so_ve_names = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE|SUBORD_ASC_CODE_AM',colnames(asc_1d_em),value = T)

asc_ord_so_list = list('mlp_so_an'=mlp_so_an_names,'mlp_so_or'=mlp_so_or_names,'mlp_so_po'=mlp_so_po_names,'mlp_so_ve'=mlp_so_ve_names,
                       'mlp_so_hy'=mlp_so_hy_names,'mlp_so_ka'=mlp_so_ka_names,'mlp_so_ca'=mlp_so_ca_names,'mlp_so_de'=mlp_so_de_names,
                       'mlp_so_ru'=mlp_so_ru_names,'mlp_so_te'=mlp_so_te_names)

mlp_gg_list = grep('GREAT_GROUP',colnames(asc_1d_em))#,value = T)
mlp_gg_names = grep('GREAT_GROUP',colnames(asc_1d_em),value = T)
mlp_gg_ve_list = grep('GREAT_GROUP_ASC_CODE_EI|GREAT_GROUP_ASC_CODE_GS|GREAT_GROUP_ASC_CODE_BH|GREAT_GROUP_ASC_CODE_DF|GREAT_GROUP_ASC_CODE_DW',colnames(asc_1d_em))
mlp_gg_ve_names = grep('GREAT_GROUP_ASC_CODE_EI|GREAT_GROUP_ASC_CODE_GS|GREAT_GROUP_ASC_CODE_BH|GREAT_GROUP_ASC_CODE_DF|GREAT_GROUP_ASC_CODE_DW',colnames(asc_1d_em),value = T)

mlp_sg_list = grep('SUBGROUP',colnames(asc_1d_em))#,value = T)
mlp_sg_names = grep('SUBGROUP',colnames(asc_1d_em),value = T)
mlp_sg_ve_list = grep('SUBGROUP_ASC_CODE_EG|SUBGROUP_ASC_CODE_EV|SUBGROUP_ASC_CODE_EU|SUBGROUP_ASC_CODE_BJ|SUBGROUP_ASC_CODE_DZ|SUBGROUP_ASC_CODE_GQ|SUBGROUP_ASC_CODE_BI|SUBGROUP_ASC_CODE_GG|SUBGROUP_ASC_CODE_GH|SUBGROUP_ASC_CODE_GI|SUBGROUP_ASC_CODE_BN|SUBGROUP_ASC_CODE_CU|SUBGROUP_ASC_CODE_GN|SUBGROUP_ASC_CODE_GK|SUBGROUP_ASC_CODE_GJ|SUBGROUP_ASC_CODE_EP|SUBGROUP_ASC_CODE_GA|SUBGROUP_ASC_CODE_FM|SUBGROUP_ASC_CODE_GO|SUBGROUP_ASC_CODE_BR|SUBGROUP_ASC_CODE_GB|SUBGROUP_ASC_CODE_FY|SUBGROUP_ASC_CODE_GL|SUBGROUP_ASC_CODE_BL|SUBGROUP_ASC_CODE_GM|SUBGROUP_ASC_CODE_BP|SUBGROUP_ASC_CODE_HE|SUBGROUP_ASC_CODE_FZ|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(asc_1d_em))
mlp_sg_ve_names = grep('SUBGROUP_ASC_CODE_EG|SUBGROUP_ASC_CODE_EV|SUBGROUP_ASC_CODE_EU|SUBGROUP_ASC_CODE_BJ|SUBGROUP_ASC_CODE_DZ|SUBGROUP_ASC_CODE_GQ|SUBGROUP_ASC_CODE_BI|SUBGROUP_ASC_CODE_GG|SUBGROUP_ASC_CODE_GH|SUBGROUP_ASC_CODE_GI|SUBGROUP_ASC_CODE_BN|SUBGROUP_ASC_CODE_CU|SUBGROUP_ASC_CODE_GN|SUBGROUP_ASC_CODE_GK|SUBGROUP_ASC_CODE_GJ|SUBGROUP_ASC_CODE_EP|SUBGROUP_ASC_CODE_GA|SUBGROUP_ASC_CODE_FM|SUBGROUP_ASC_CODE_GO|SUBGROUP_ASC_CODE_BR|SUBGROUP_ASC_CODE_GB|SUBGROUP_ASC_CODE_FY|SUBGROUP_ASC_CODE_GL|SUBGROUP_ASC_CODE_BL|SUBGROUP_ASC_CODE_GM|SUBGROUP_ASC_CODE_BP|SUBGROUP_ASC_CODE_HE|SUBGROUP_ASC_CODE_FZ|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(asc_1d_em),value = T)



# ord tr and test inputs and outputs#################
#mlp_asc_in_tr = mlp_asc_tr[,-c(1:2,mlp_so_ex,mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)]
#mlp_sg_ve_in_tr = mlp_sg_ve_tr[,-c(1:2,mlp_so_ex,mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)]
#mlp_soex_tr = mlp_asc_tr[,mlp_so_ex]
#mlp_soex_sg_ve_tr = mlp_sg_ve_tr[,mlp_so_ex]
#mlp_so_tag_tr = mlp_asc_tr[,mlp_so_list]
# mlp_so_ka_tag_tr = mlp_asc_tr[,mlp_so_ka_list]
# mlp_so_ka_tag_tr$OTHERS = abs(1-rowSums(mlp_so_ka_tag_tr[,1:length(mlp_so_ka_tag_tr)]))
# mlp_so_na_tag_tr = mlp_asc_tr[,mlp_so_na_list]
# mlp_so_na_tag_tr$OTHERS = abs(1-rowSums(mlp_so_na_tag_tr[,1:length(mlp_so_na_tag_tr)]))
# mlp_so_ca_tag_tr = mlp_asc_tr[,mlp_so_ca_list]
# mlp_so_ca_tag_tr$OTHERS = abs(1-rowSums(mlp_so_ca_tag_tr[,1:length(mlp_so_ca_tag_tr)]))
# mlp_so_de_tag_tr = mlp_asc_tr[,mlp_so_de_list]
# mlp_so_de_tag_tr$OTHERS = abs(1-rowSums(mlp_so_de_tag_tr[,1:length(mlp_so_de_tag_tr)]))
# mlp_so_hy_tag_tr = mlp_asc_tr[,mlp_so_hy_list]
# mlp_so_hy_tag_tr$OTHERS = abs(1-rowSums(mlp_so_hy_tag_tr[,1:length(mlp_so_hy_tag_tr)]))
# mlp_so_ru_tag_tr = mlp_asc_tr[,mlp_so_ru_list]
# mlp_so_ru_tag_tr$OTHERS = abs(1-rowSums(mlp_so_ru_tag_tr[,1:length(mlp_so_ru_tag_tr)]))
# mlp_so_te_tag_tr = mlp_asc_tr[,mlp_so_te_list]
# mlp_so_te_tag_tr$OTHERS = abs(1-rowSums(mlp_so_te_tag_tr[,1:length(mlp_so_te_tag_tr)]))

#mlp_so_ve_tag_tr = mlp_asc_tr[,mlp_so_ve_list]
#mlp_so_ve_tag_tr$OTHERS = abs(1-rowSums(mlp_so_ve_tag_tr[,1:length(mlp_so_ve_tag_tr)]))

#mlp_gg_tag_tr = mlp_asc_tr[,mlp_gg_list]

#mlp_gg_ve_tag_tr = mlp_asc_tr[,mlp_gg_ve_list]
#mlp_gg_ve_tag_tr$OTHERS = abs(1-rowSums(mlp_gg_ve_tag_tr[,1:length(mlp_gg_ve_tag_tr)]))

#mlp_sg_tag_tr = mlp_asc_tr[,mlp_sg_list]

#mlp_sg_ve_tag_tr = mlp_asc_tr[,mlp_sg_ve_list]
#mlp_sg_ve_tag_tr$OTHERS = abs(1-rowSums(mlp_sg_ve_tag_tr[,1:length(mlp_sg_ve_tag_tr)]))

#mlp_asc_in_test = mlp_asc_test[,-c(1:2,mlp_so_ex,mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)]
#mlp_sg_ve_in_test = mlp_sg_ve_test[,-c(1:2,mlp_so_ex,mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)]

#mlp_soex_test = mlp_asc_test[,mlp_so_ex]
#mlp_soex_sg_ve_test = mlp_sg_ve_test[,mlp_so_ex]
#mlp_so_tag_test = mlp_asc_test[,mlp_so_list]
# mlp_so_ka_tag_test = mlp_asc_test[,mlp_so_ka_list]
# mlp_so_ka_tag_test$OTHERS = abs(1-rowSums(mlp_so_ka_tag_test[,1:length(mlp_so_ka_tag_test)]))
# mlp_so_na_tag_test = mlp_asc_test[,mlp_so_na_list]
# mlp_so_na_tag_test$OTHERS = abs(1-rowSums(mlp_so_na_tag_test[,1:length(mlp_so_na_tag_test)]))
# mlp_so_ca_tag_test = mlp_asc_test[,mlp_so_ca_list]
# mlp_so_ca_tag_test$OTHERS = abs(1-rowSums(mlp_so_ca_tag_test[,1:length(mlp_so_ca_tag_test)]))
# mlp_so_de_tag_test = mlp_asc_test[,mlp_so_de_list]
# mlp_so_de_tag_test$OTHERS = abs(1-rowSums(mlp_so_de_tag_test[,1:length(mlp_so_de_tag_test)]))
# mlp_so_hy_tag_test = mlp_asc_test[,mlp_so_hy_list]
# mlp_so_hy_tag_test$OTHERS = abs(1-rowSums(mlp_so_hy_tag_test[,1:length(mlp_so_hy_tag_test)]))
# mlp_so_ru_tag_test = mlp_asc_test[,mlp_so_ru_list]
# mlp_so_ru_tag_test$OTHERS = abs(1-rowSums(mlp_so_ru_tag_test[,1:length(mlp_so_ru_tag_test)]))
# mlp_so_te_tag_test = mlp_asc_test[,mlp_so_te_list]
# mlp_so_te_tag_test$OTHERS = abs(1-rowSums(mlp_so_te_tag_test[,1:length(mlp_so_te_tag_test)]))

#mlp_so_ve_tag_test = mlp_asc_test[,mlp_so_ve_list]
#mlp_so_ve_tag_test$OTHERS = abs(1-rowSums(mlp_so_ve_tag_test[,1:length(mlp_so_ve_tag_test)]))

#mlp_gg_tag_test = mlp_asc_test[,mlp_gg_list]

#mlp_gg_ve_tag_test = mlp_asc_test[,mlp_gg_ve_list]
#mlp_gg_ve_tag_test$OTHERS = abs(1-rowSums(mlp_gg_ve_tag_test[,1:length(mlp_gg_ve_tag_test)]))

#mlp_sg_tag_test = mlp_asc_test[,mlp_sg_list]

#mlp_sg_ve_tag_test = mlp_asc_test[,mlp_sg_ve_list]
#mlp_sg_ve_tag_test$OTHERS = abs(1-rowSums(mlp_sg_ve_tag_test[,1:length(mlp_sg_ve_tag_test)]))
# ord input list
tr_in_list = list(); test_in_list = list();j=1
for (i in sc_vc_names) {
  tr_in_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_asc_tr[,i]))
  test_in_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_asc_test[,i]))
  j= j+1
}
tr_sn_vn = as.matrix(mlp_asc_tr[,sn_vn_list])
tr_in_list[[53]] = tr_sn_vn
test_sn_vn = as.matrix(mlp_asc_test[,sn_vn_list])
test_in_list[[53]] = test_sn_vn
# so_ex input list
tr_so_ex_in_list = list(); test_so_ex_in_list = list(); j = 1
for (i in mlp_so_ex_names) {
  tr_so_ex_in_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_asc_tr[,i]))
  test_so_ex_in_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_asc_test[,i]))
  j= j+1
}
mlp_asc_tag_tr = mlp_asc_tr[,mlp_asc_list]
mlp_asc_tag_test = mlp_asc_test[,mlp_asc_list]
# ord model ####################
# generate input layer, embedding layer, reshape to null x 50, and strore in a list
input_layer_list = list(); embed_layer_list = list();j = 1
for (i in sc_vc_names) {
  input_layer_list = append(input_layer_list,
                            assign(paste(i, 'in', sep = "_"),
                                   layer_input(shape = c(1),name = paste(i,'in',sep = '_'))))
  vocabrulary = length(unique(asc_1d_em[[i]]))
  size = min(50,(round(0.5*vocabrulary)+1))
  embed_layer_list = append(embed_layer_list,assign(paste(i,'1d',sep = '_'), 
       assign(paste(i,'em',sep='_'), 
              input_layer_list[[j]] %>%
                layer_embedding(input_dim = vocabrulary+1,
                                output_dim = size,
                                mask_zero = T,
                                name = paste(i,'em',sep='_'))) %>%
         layer_reshape(c(size),name = paste(i,'1d',sep = '_'))))
  j = j+1
}
sn_vn_in = layer_input(shape(c(length(sn_vn_list))),name = 'sn_vn_in')
con_layer_list = append(embed_layer_list,sn_vn_in)
input_layer_list = append(input_layer_list,sn_vn_in)
base_in = layer_concatenate(con_layer_list,name = 'base_in')
ord_out = base_in %>% 
  layer_dense(units = 256,activation = 'relu',name = 'ord_dense_128') %>%
  #layer_dense(units = 64,activation = 'relu',name = 'ord_dense_64') %>%
  layer_dense(units = length(mlp_asc_names),activation = 'softmax',name = 'ord_out_layer')
model_ord = keras_model(input_layer_list,ord_out)
model_ord %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = metric_categorical_accuracy
)
#model_ord
model_ord %>% save_model_hdf5('./1_asc_mlp/modelasc_1d_multi_em_ord.h5')

history_ord <- model_ord %>% fit(tr_in_list,
                                 as.matrix(mlp_asc_tag_tr),
                                 epochs=30,
                                 batch_size=1024,
                                 validation_split=0.15)
ord_predict <- model_ord %>% predict(test_in_list)
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
# so_ve_without others data ###################################

mlp_so_ve_tr = mlp_asc_tr[rowSums(mlp_asc_tr[,mlp_so_ve_list])==1,]
mlp_so_ve_test = mlp_asc_test[rowSums(mlp_asc_test[,mlp_so_ve_list])==1,]
# ord list

tr_in_so_ve_list = list(); test_in_so_ve_list = list();j=1
for (i in sc_vc_names) {
  tr_in_so_ve_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_so_ve_tr[,i]))
  test_in_so_ve_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_so_ve_test[,i]))
  j= j+1
}
tr_sn_vn_so_ve = as.matrix(mlp_so_ve_tr[,sn_vn_list])
tr_in_so_ve_list[[53]] = tr_sn_vn_so_ve
test_sn_vn_so_ve = as.matrix(mlp_so_ve_test[,sn_vn_list])
test_in_so_ve_list[[53]] = test_sn_vn_so_ve
# so_ex input list
tr_so_ex_in_so_ve_list = list(); test_so_ex_in_so_ve_list = list(); j = 1
for (i in mlp_so_ex_names) {
  tr_so_ex_in_so_ve_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_so_ve_tr[,i]))
  test_so_ex_in_so_ve_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_so_ve_test[,i]))
  j= j+1
}
mlp_so_ve_tag_tr_w = mlp_so_ve_tr[,mlp_so_ve_list]
mlp_so_ve_tag_test_w = mlp_so_ve_test[,mlp_so_ve_list]
# so model#################
# generate input layer, embedding layer, reshape to null x 50, and strore in a list
input_layer_list_so = list(); embed_layer_list_so = list();j = 1
for (i in sc_vc_names) {
  input_layer_list_so = append(input_layer_list_so,
                            assign(paste(i, 'in','so', sep = "_"),
                                   layer_input(shape = c(1),name = paste(i,'in','so',sep = '_'))))
  vocabrulary = length(unique(asc_1d_em[[i]]))
  size = min(50,(round(0.5*vocabrulary)+1))
  embed_layer_list_so = append(embed_layer_list_so,assign(paste(i,'1d','so',sep = '_'), 
                                                    assign(paste(i,'em','so',sep='_'), 
                                                           input_layer_list_so[[j]] %>%
                                                             layer_embedding(input_dim = vocabrulary+1,
                                                                             output_dim = size,
                                                                             mask_zero = T,
                                                                             name = paste(i,'em','so',sep='_'))) %>%
                                                      layer_reshape(c(size),name = paste(i,'1d','so',sep = '_'))))
  j = j+1
}
sn_vn_in_so = layer_input(shape(c(length(sn_vn_list))),name = 'sn_vn_in_so')
con_layer_list_so = append(embed_layer_list_so,sn_vn_in_so)
input_layer_list_so = append(input_layer_list_so,sn_vn_in_so)
base_in_so = layer_concatenate(con_layer_list_so,name = 'base_in_so')
# so_ex layer
so_ex_layer_list = list(); so_ex_embed_layer_list = list();j = 1
for (i in mlp_so_ex_names) {
  so_ex_layer_list = append(so_ex_layer_list,
                            assign(paste(i, 'in', sep = "_"),
                                   layer_input(shape = c(1),name = paste(i,'in',sep = '_'))))
  vocabrulary = length(unique(asc_1d_em[[i]]))
  size = min(50,(round(0.5*vocabrulary)+1))
  so_ex_embed_layer_list = append(so_ex_embed_layer_list,assign(paste(i,'1d',sep = '_'), 
                                                    assign(paste(i,'em',sep='_'), 
                                                           so_ex_layer_list[[j]] %>%
                                                             layer_embedding(input_dim = vocabrulary+1,
                                                                             output_dim = size,
                                                                             mask_zero = T,
                                                                             name = paste(i,'em',sep='_'))) %>%
                                                      layer_reshape(c(size),name = paste(i,'1d',sep = '_'))))
  j = j+1
}
so_ex_in = layer_concatenate(so_ex_embed_layer_list,name = 'so_ex_in')

so_ve_in = layer_concatenate(list(base_in_so,so_ex_in), name = 'so_con_in')
so_ve_out_w = so_ve_in %>% 
  layer_dense(units = 256,activation = 'relu',name = 'so_dense_256') %>%
  #layer_dense(units =32,activation = 'relu',name = 'so_dense_128') %>%
  #layer_dense(units = length(mlp_so_ve_names)+1,activation = 'softmax',name = 'so_ve_out_layer')
  layer_dense(units = length(mlp_so_ve_names),activation = 'softmax',name = 'so_ve_out_layer')
model_so_ve_w = keras_model(c(so_ex_layer_list,input_layer_list_so),so_ve_out_w)
model_so_ve_w %>% compile(
  #optimizer=optimizer_rmsprop(lr=0.0001),
  optimizer="rmsprop",
  loss = "categorical_crossentropy",
  #optimizer="rmsprop",
  #loss = loss_binary_crossentropy,
  metrics = metric_categorical_accuracy
)
model_so_ve_w
model_so_ve_w %>% save_model_hdf5('./1_asc_mlp/asc_1d_multi_em_so_ve.h5')
history_so_ve_w <- model_so_ve_w %>% fit(c(tr_so_ex_in_so_ve_list,tr_in_so_ve_list),
                             as.matrix(mlp_so_ve_tag_tr_w),
                             epochs=30,
                             batch_size=512,
                             # class_weight= list("0"= 10,"1"=10,"2"= 10,"3"=10,"4"= 10,"5"=10,"6"= 10,"7"=10,"8"=10,"9"= 10,"10"=10,"11"=10,"12"=1),#,
                             #                    # list("0"= 100,"1"=100,"2"=100,"3"=0,"4"=100,"5"=0,"6"=100,"7"=0,"8"=0,"9"=0,"10"= 0,
                             #                    #      "11"=0,"12"= 0,"13"=0,"14"= 0,"15"=0,"16"= 0,"17"=0,"18"= 0,"19"=0,"20"= 0,
                             #                    #      "21"=0,"22"= 0,"23"=0,"24"= 0,"25"=0,"26"= 0,"27"=0,"28"= 0,"29"=0,"30"= 0,
                             #                    #      "31"=0,"32"= 0,"33"=0,"34"= 0,"35"=0,"36"= 0,"37"=0,"38"= 0,"39"=0,"40"= 0,
                             #                    #      "41"=0,"42"= 0,"43"=0,"44"= 0,"45"=0,"46"= 0,"47"=0,"48"= 0,"49"=0,"50"= 0,
                             #                    #      "51"=0,"52"= 0,"53"=0,"54"= 0,"55"=0,"56"= 0,"57"=0,"58"= 0,"59"=0,"60"= 0,
                             #                    #      "61"=0,"62"= 0,"63"=0),

                             validation_split=0.15
                             )

so_predict <- model_so_ve_w %>% predict(c(test_so_ex_in_so_ve_list,test_in_so_ve_list))
colnames(so_predict) = colnames(mlp_so_ve_tag_test_w)

rownames(so_predict) = rownames(mlp_so_ve_tag_test_w)
so_predict = as.data.frame(so_predict)

so_predict = cbind(colnames(so_predict)[apply(so_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(so_predict)[apply(so_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(mlp_so_ve_tag_test_w)[apply(mlp_so_ve_tag_test_w,1,which.max)],
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

# gg_ve_without others data ###################################
mlp_gg_ve_tr = mlp_asc_tr[rowSums(mlp_asc_tr[,mlp_gg_ve_list])==1,]
mlp_gg_ve_test = mlp_asc_test[rowSums(mlp_asc_test[,mlp_gg_ve_list])==1,]
# ord list
tr_in_gg_ve_list = list(); test_in_gg_ve_list = list();j=1
for (i in sc_vc_names) {
  tr_in_gg_ve_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_gg_ve_tr[,i]))
  test_in_gg_ve_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_gg_ve_test[,i]))
  j= j+1
}
tr_sn_vn_gg_ve = as.matrix(mlp_gg_ve_tr[,sn_vn_list])
tr_in_gg_ve_list[[53]] = tr_sn_vn_gg_ve
test_sn_vn_gg_ve = as.matrix(mlp_gg_ve_test[,sn_vn_list])
test_in_gg_ve_list[[53]] = test_sn_vn_gg_ve
# so_ex input list
tr_so_ex_in_gg_ve_list = list(); test_so_ex_in_gg_ve_list = list(); j = 1
for (i in mlp_so_ex_names) {
  tr_so_ex_in_gg_ve_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_gg_ve_tr[,i]))
  test_so_ex_in_gg_ve_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_gg_ve_test[,i]))
  j= j+1
}
mlp_gg_ve_tag_tr_w = mlp_gg_ve_tr[,mlp_gg_ve_list]
mlp_gg_ve_tag_test_w = mlp_gg_ve_test[,mlp_gg_ve_list]
# gg model##########################
# generate input layer, embedding layer, reshape to null x 50, and strore in a list
input_layer_list_gg = list(); embed_layer_list_gg = list();j = 1
for (i in sc_vc_names) {
  input_layer_list_gg = append(input_layer_list_gg,
                               assign(paste(i, 'in','gg', sep = "_"),
                                      layer_input(shape = c(1),name = paste(i,'in','gg',sep = '_'))))
  vocabrulary = length(unique(asc_1d_em[[i]]))
  size = min(50,(round(0.5*vocabrulary)+1))
  embed_layer_list_gg = append(embed_layer_list_gg,assign(paste(i,'1d','gg',sep = '_'), 
                                                          assign(paste(i,'em','gg',sep='_'), 
                                                                 input_layer_list_gg[[j]] %>%
                                                                   layer_embedding(input_dim = vocabrulary+1,
                                                                                   output_dim = size,
                                                                                   mask_zero = T,
                                                                                   name = paste(i,'em','gg',sep='_'))) %>%
                                                            layer_reshape(c(size),name = paste(i,'1d','gg',sep = '_'))))
  j = j+1
}
sn_vn_in_gg = layer_input(shape(c(length(sn_vn_list))),name = 'sn_vn_in_gg')
con_layer_list_gg = append(embed_layer_list_gg,sn_vn_in_gg)
input_layer_list_gg = append(input_layer_list_gg,sn_vn_in_gg)
base_in_gg = layer_concatenate(con_layer_list_gg,name = 'base_in_gg')
# so_ex layer
so_ex_layer_list_gg = list(); so_ex_embed_layer_list_gg = list();j = 1
for (i in mlp_so_ex_names) {
  so_ex_layer_list_gg = append(so_ex_layer_list_gg,
                            assign(paste(i, 'in','gg', sep = "_"),
                                   layer_input(shape = c(1),name = paste(i,'in','gg',sep = '_'))))
  vocabrulary = length(unique(asc_1d_em[[i]]))
  size = min(50,(round(0.5*vocabrulary)+1))
  so_ex_embed_layer_list_gg = append(so_ex_embed_layer_list_gg,assign(paste(i,'1d','gg',sep = '_'), 
                                                                assign(paste(i,'em','gg',sep='_'), 
                                                                       so_ex_layer_list_gg[[j]] %>%
                                                                         layer_embedding(input_dim = vocabrulary+1,
                                                                                         output_dim = size,
                                                                                         mask_zero = T,
                                                                                         name = paste(i,'em','gg',sep='_'))) %>%
                                                                  layer_reshape(c(size),name = paste(i,'1d','gg',sep = '_'))))
  j = j+1
}
so_ex_in_gg = layer_concatenate(so_ex_embed_layer_list_gg,name = 'so_ex_in_gg')
gg_ve_in = layer_concatenate(list(base_in_gg,so_ex_in_gg),name = 'gg_con_in')

gg_ve_out_w = gg_ve_in %>% 
  layer_dense(units = 32,kernel_regularizer = regularizer_l1_l2(),activation = 'relu',name = 'gg_dense_256') %>%
  #layer_dense(units =32,activation = 'relu',name = 'gg_dense_128') %>%
  #layer_dense(units = length(mlp_gg_ve_names)+1,activation = 'softmax',name = 'gg_ve_out_layer')
  layer_dense(units = length(mlp_gg_ve_names),activation = 'softmax',name = 'gg_ve_out_layer')
model_gg_ve_w = keras_model(c(so_ex_layer_list_gg,input_layer_list_gg),gg_ve_out_w)


model_gg_ve_w %>% compile(
  #optimizer=optimizer_rmsprop(lr=0.0001),
  optimizer=optimizer_rmsprop(lr=0.0001),
  loss = "categorical_crossentropy",
  #optimizer="rmsprop",
  #loss = loss_binary_crossentropy,
  #loss = "categorical_crossentropy",
  metrics = metric_categorical_accuracy
)
model_gg_ve_w
# gg_class_weights <- function(x){ # x: the ord
#   
# }
history_gg_ve_w <- model_gg_ve_w %>% fit(c(tr_so_ex_in_gg_ve_list,tr_in_gg_ve_list),
                                     as.matrix(mlp_gg_ve_tag_tr_w),
                                     epochs=40,
                                     batch_size=64,
                                     class_weight= list("0"= 40,"1"=25,"2"= 300,"3"=300),#,"4"= 1),#,
                                     #                    # list("0"= 100,"1"=100,"2"=100,"3"=0,"4"=100,"5"=0,"6"=100,"7"=0,"8"=0,"9"=0,"10"= 0,
                                     #                    #      "11"=0,"12"= 0,"13"=0,"14"= 0,"15"=0,"16"= 0,"17"=0,"18"= 0,"19"=0,"20"= 0,
                                     #                    #      "21"=0,"22"= 0,"23"=0,"24"= 0,"25"=0,"26"= 0,"27"=0,"28"= 0,"29"=0,"30"= 0,
                                     #                    #      "31"=0,"32"= 0,"33"=0,"34"= 0,"35"=0,"36"= 0,"37"=0,"38"= 0,"39"=0,"40"= 0,
                                     #                    #      "41"=0,"42"= 0,"43"=0,"44"= 0,"45"=0,"46"= 0,"47"=0,"48"= 0,"49"=0,"50"= 0,
                                     #                    #      "51"=0,"52"= 0,"53"=0,"54"= 0,"55"=0,"56"= 0,"57"=0,"58"= 0,"59"=0,"60"= 0,
                                     #                    #      "61"=0,"62"= 0,"63"=0),
                                     
                                     validation_split=0.15)

gg_predict <- model_gg_ve_w %>% predict(c(test_so_ex_in_gg_ve_list,test_in_gg_ve_list))
colnames(gg_predict) = colnames(mlp_gg_ve_tag_test_w)
rownames(gg_predict) = rownames(mlp_gg_ve_tag_test_w)
gg_predict = as.data.frame(gg_predict)

gg_predict = cbind(colnames(gg_predict)[apply(gg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(gg_predict)[apply(gg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(mlp_gg_ve_tag_test_w)[apply(mlp_gg_ve_tag_test_w,1,which.max)],
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
gg_con_s
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
gg_con_top2



# sg_ve_without others tr and test input data list#########################
mlp_sg_ve_tr = mlp_asc_tr[rowSums(mlp_asc_tr[,mlp_sg_ve_list])==1,]
mlp_sg_ve_test = mlp_asc_test[rowSums(mlp_asc_test[,mlp_sg_ve_list])==1,]
# ord input list
tr_in_sg_ve_list = list(); test_in_sg_ve_list = list();j=1
for (i in sc_vc_names) {
  tr_in_sg_ve_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_sg_ve_tr[,i]))
  test_in_sg_ve_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_sg_ve_test[,i]))
  j= j+1
}
tr_sn_vn_sg_ve = as.matrix(mlp_sg_ve_tr[,sn_vn_list])
tr_in_sg_ve_list[[53]] = tr_sn_vn_sg_ve
test_sn_vn_sg_ve = as.matrix(mlp_sg_ve_test[,sn_vn_list])
test_in_sg_ve_list[[53]] = test_sn_vn_sg_ve
# so_ex input list
tr_so_ex_in_sg_ve_list = list(); test_so_ex_in_sg_ve_list = list(); j = 1
for (i in mlp_so_ex_names) {
  tr_so_ex_in_sg_ve_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_sg_ve_tr[,i]))
  test_so_ex_in_sg_ve_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_sg_ve_test[,i]))
  j= j+1
}
mlp_sg_ve_tag_tr_w = mlp_sg_ve_tr[,mlp_sg_ve_list]
mlp_sg_ve_tag_test_w = mlp_sg_ve_test[,mlp_sg_ve_list]

# sg_without OTHERS model#################
#so_ex_in = layer_input(shape(c(length(mlp_soex_tr))),name = "sg_in_layer")
#sg_in = layer_concatenate(list(base_in,so_ex_in),axis = 1, name = 'sg_con_in')
# generate input layer, embedding layer, reshape to null x 50, and strore in a list
input_layer_list_sg = list(); embed_layer_list_sg = list();j = 1
for (i in sc_vc_names) {
  input_layer_list_sg = append(input_layer_list_sg,
                               assign(paste(i, 'in','sg', sep = "_"),
                                      layer_input(shape = c(1),name = paste(i,'in','sg',sep = '_'))))
  vocabrulary = length(unique(asc_1d_em[[i]]))
  size = min(50,(round(0.5*vocabrulary)+1))
  embed_layer_list_sg = append(embed_layer_list_sg,assign(paste(i,'1d','sg',sep = '_'), 
                                                          assign(paste(i,'em','sg',sep='_'), 
                                                                 input_layer_list_sg[[j]] %>%
                                                                   layer_embedding(input_dim = vocabrulary+1,
                                                                                   output_dim = size,
                                                                                   mask_zero = T,
                                                                                   name = paste(i,'em','sg',sep='_'))) %>%
                                                            layer_reshape(c(size),name = paste(i,'1d','sg',sep = '_'))))
  j = j+1
}
sn_vn_in_sg = layer_input(shape(c(length(sn_vn_list))),name = 'sn_vn_in_sg')
con_layer_list_sg = append(embed_layer_list_sg,sn_vn_in_sg)
input_layer_list_sg = append(input_layer_list_sg,sn_vn_in_sg)
base_in_sg = layer_concatenate(con_layer_list_sg,name = 'base_in_sg')
# so_ex layer
so_ex_layer_list_sg = list(); so_ex_embed_layer_list_sg = list();j = 1
for (i in mlp_so_ex_names) {
  so_ex_layer_list_sg = append(so_ex_layer_list_sg,
                               assign(paste(i, 'in','sg', sep = "_"),
                                      layer_input(shape = c(1),name = paste(i,'in','sg',sep = '_'))))
  vocabrulary = length(unique(asc_1d_em[[i]]))
  size = min(50,(round(0.5*vocabrulary)+1))
  so_ex_embed_layer_list_sg = append(so_ex_embed_layer_list_sg,assign(paste(i,'1d','sg',sep = '_'), 
                                                                      assign(paste(i,'em','sg',sep='_'), 
                                                                             so_ex_layer_list_sg[[j]] %>%
                                                                               layer_embedding(input_dim = vocabrulary+1,
                                                                                               output_dim = size,
                                                                                               mask_zero = T,
                                                                                               name = paste(i,'em','sg',sep='_'))) %>%
                                                                        layer_reshape(c(size),name = paste(i,'1d','sg',sep = '_'))))
  j = j+1
}
so_ex_in_sg = layer_concatenate(so_ex_embed_layer_list_sg,name = 'so_ex_in_sg')
sg_ve_in = layer_concatenate(list(base_in_sg,so_ex_in_sg),name = 'sg_con_in')

sg_ve_out_w = sg_ve_in %>% 
  layer_dense(units = 128, kernel_regularizer = regularizer_l1_l2(), activation = 'relu',name = 'sg_dense_256') %>% 
  #layer_dense(units =128,activation = 'relu',name = 'sg_dense_128') %>%
  #layer_dense(units = length(mlp_sg_ve_names)+1,activation = 'softmax',name = 'sg_ve_out_layer')
  layer_dense(units = length(mlp_sg_ve_names),activation = 'softmax',name = 'sg_ve_out_layer')
model_sg_ve_w = keras_model(c(so_ex_layer_list_sg,input_layer_list_sg),sg_ve_out_w)
model_sg_ve_w %>% compile(
  #optimizer=optimizer_rmsprop(lr=0.0001),
  optimizer=optimizer_rmsprop(lr=0.0001),
  loss = "categorical_crossentropy",
  #optimizer="rmsprop",
  #loss = loss_binary_crossentropy,
  #loss = "categorical_crossentropy",
  metrics = metric_categorical_accuracy
)
model_sg_ve_w
# sg_class_weights <- function(x){ # x: the ord
#   
# }
history_sg_ve_w <- model_sg_ve_w %>% fit(c(tr_so_ex_in_sg_ve_list,tr_in_sg_ve_list),
                                     as.matrix(mlp_sg_ve_tag_tr_w),
                                     epochs=40,
                                     batch_size=128,
                                     class_weight= #list("0"= 5,"1"=3,"2"= 60,"3"=100,"4"= 1),#,
                                                        list("0"= 50,"1"=25,"2"=50,"3"=60,"4"=50,"5"=50,"6"=50,"7"=50,"8"=50,"9"=50,"10"=40,
                                                             "11"=50,"12"=50,"13"=80,"14"=50,"15"=50,"16"=50,"17"=50,"18"=50,"19"=50,"20"=50,
                                                             "21"=50,"22"=50,"23"=50,"24"=50,"25"=50,"26"=50,"27"=50,"28"=50,"29"=50,"30"=50,
                                                             "31"=50,"32"=50,"33"=50#,"34"=50,"35"=50,"36"=50,"37"=50,"38"=50,"39"=50,"40"=50,
                                                             #"41"=50,"42"=50,"43"=50,"44"=50,"45"=50,"46"=50,"47"=50,"48"=50,"49"=50,"50"=50,
                                                             #"51"=50,"52"=50,"53"=50,"54"=50,"55"=50,"56"=50,"57"=50,"58"=50,"59"=50,"60"=50,
                                                             #"61"=50,"62"=50,"63"=50
                                                             ),

                                     validation_split=0.15)

sg_predict <- model_sg_ve_w %>% predict(c(test_so_ex_in_sg_ve_list,test_in_sg_ve_list))
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
sg_con_top2$overall
# save model and weights####################
model_ord %>% save_model_hdf5("./1_asc_mlp/asc_mlp_multi_em_ord.h5")
model_so_ve_w %>% save_model_hdf5("./1_asc_mlp/asc_mlp_multi_em_so_ve.h5")
model_gg_ve_w %>% save_model_hdf5("./1_asc_mlp/asc_mlp_multi_em_gg_ve.h5")
model_sg_ve_w %>% save_model_hdf5("./1_asc_mlp/asc_mlp_multi_em_sg_ve.h5")
#model_test <- load_model_hdf5("./1_asc_mlp/asc_mlp_multi_em_ord.h5")
#predict###################################
predict_one_data <- function(x,idx) {
  # ord input list
  pre_in_list = list();j=1
  for (i in sc_vc_names) {
    pre_in_list[[j]] = assign(paste(i,'tr','so',sep = "_"),as.matrix(x[idx,i]))
    j = j+1
  }
  pre_sn_vn = as.matrix(x[idx,sn_vn_list])
  pre_in_list[[53]] = pre_sn_vn
  
  # so_ex input list
  pre_so_ex_in_list = list(); j = 1
  for (i in mlp_so_ex_names) {
    pre_so_ex_in_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(x[idx,i]))
    j= j+1
  }
  
  predict_ord = model_ord %>% predict(pre_in_list)
  predict_so = model_so_ve_w %>% predict(c(pre_so_ex_in_list,pre_in_list))
  predict_gg = model_gg_ve_w %>% predict(c(pre_so_ex_in_list,pre_in_list))
  predict_sg = model_sg_ve_w %>%  predict(c(pre_so_ex_in_list,pre_in_list))
  colnames(predict_ord) = colnames(mlp_asc_tag_test)
  #rownames(predict_ord) = rownames(mlp_asc_tag_test)
  colnames(predict_so) = colnames(mlp_so_ve_tag_test_w)
  #rownames(predict_so) = rownames(mlp_sg_ve_tag_test_w)
  colnames(predict_gg) = colnames(mlp_gg_ve_tag_test_w)
  #rownames(predict_gg) = rownames(mlp_sg_ve_tag_test_w)
  colnames(predict_sg) = colnames(mlp_sg_ve_tag_test_w)
  #rownames(predict_sg) = rownames(mlp_sg_ve_tag_test_w)
  #sg_predict = as.data.frame(sg_predict)
  pre_list = c(colnames(predict_ord)[which.max(predict_ord)],
               colnames(predict_so)[which.max(predict_so)],
               colnames(predict_gg)[which.max(predict_gg)],
               colnames(predict_sg)[which.max(predict_sg)])
  real_list = c(colnames(x[,3:16])[which.max(x[idx,3:16])],
                colnames(x[,17:80])[which.max(x[idx,17:80])],
                colnames(x[,81:204])[which.max(x[idx,81:204])],
                colnames(x[,205:361])[which.max(x[idx,205:361])])
  
  pre_result =
    paste(colnames(predict_ord)[which.max(predict_ord)],
          colnames(predict_so)[which.max(predict_so)],
          colnames(predict_gg)[which.max(predict_gg)],
          colnames(predict_sg)[which.max(predict_sg)],
          sep = " ")
  real_result = 
    paste(colnames(x[,3:16])[which.max(x[idx,3:16])],
          colnames(x[,17:80])[which.max(x[idx,17:80])],
          colnames(x[,81:204])[which.max(x[idx,81:204])],
          colnames(x[,205:361])[which.max(x[idx,205:361])],
          sep = " "
          
    )
  record = c(0,0,0,0,0)
 if (pre_result==real_result){
    cat(green(paste(idx, x[idx,1], x[idx,2],'\n',
                    "Predict: ",pre_result,'\n',
                    "   Real: ",real_result,'\n',
                    'Great work!!! 100% correct\n')))
    record[[1]] = record[[1]]+1
  } 
  if (sum(pre_list==real_list)==3){
    cat(blue(paste(idx, x[idx,1], x[idx,2],'\n',
                    "Predict: ",pre_result,'\n',
                    "   Real: ",real_result,'\n',
                    'Good work!! 75% correct\n')))
    record[[2]] = record[[2]]+1
  } 
  if(sum(pre_list==real_list)==2) {
    cat(yellow(paste(idx, x[idx,1], x[idx,2],'\n',
                    "Predict: ",pre_result,'\n',
                    "   Real: ",real_result,'\n',
                    'Fair work! 50% correct\n')))
    record[[3]] = record[[3]]+1
  } 
  if (sum(pre_list==real_list)==1) {
    cat(magenta(paste(idx, x[idx,1], x[idx,2],'\n',
                    "Predict: ",pre_result,'\n',
                    "   Real: ",real_result,'\n',
                    'Poor work!! 25% correct\n')))
    record[[4]] = record[[4]]+1
  } 
  if (sum(pre_list==real_list)==0) {
    cat(red(paste(idx, x[idx,1], x[idx,2],'\n',
                    "Predict: ",pre_result,'\n',
                    "   Real: ",real_result,'\n',
                    'Failed!!! 0% correct\n')))
    record[[5]] = record[[5]]+1
  }
  return(record)
  
}
performance = c(0,0,0,0,0)
for (i in 1: nrow(mlp_asc_test[mlp_asc_test$ASC_ORD_VE==1,])) {
  idx = sample(nrow(mlp_asc_test[mlp_asc_test$ASC_ORD_VE==1,]),1,replace = F)
  performance = performance + predict_one_data(mlp_asc_test[mlp_asc_test$ASC_ORD_VE==1,],idx)
  cat(green(paste('100%:',performance[[1]],'/',sum(performance))),
      blue(paste('75%:',performance[[2]],'/',sum(performance))),
      yellow(paste('50%:',performance[[3]],'/',sum(performance))),
      magenta(paste('25%:',performance[[4]],'/',sum(performance))),
      red(paste('0%:',performance[[5]],'/',sum(performance))),'\n'
      )
}
# for (i in 1: nrow(asc_1d_em[asc_1d_em$ASC_ORD_VE==1,])) {
#   idx = sample(nrow(asc_1d_em[asc_1d_em$ASC_ORD_VE==1,]),1,replace = F)
#   predict_one_data(asc_1d_em[asc_1d_em$ASC_ORD_VE==1,],idx)
# }
