# data processing###############
library(abind)
library(stringr)
library(caret)
library(keras)
library(crayon)
library(readr)
asc_1d_onehot_4 = readRDS('./0_general/asc_1d_onehot_4.rds')
# index list#######################
ex_names = grep('COLOUR_CLASS|MOTT_',colnames(asc_1d_onehot_4),value = T)
ord_names = grep('ASC_ORD',colnames(asc_1d_onehot_4),value = T)
so_names  = grep('SUBORD',colnames(asc_1d_onehot_4),value=T)
gg_names = grep('GREAT_GROUP',colnames(asc_1d_onehot_4),value = T)
sg_names = grep('SUBGROUP',colnames(asc_1d_onehot_4),value = T)
tag_names = c(ord_names,so_names,gg_names,sg_names)
# if for model fitting, run this code split training and test sample############
tr_rate = 0.85
idx = sample(nrow(asc_1d_onehot_4),round(nrow(asc_1d_onehot_4)*tr_rate),replace = F)
tr_dat = asc_1d_onehot_4[idx,]
test_dat = asc_1d_onehot_4[-idx,]
write_rds(test_dat,"./1_asc_mlp/onehot_binary_1d/test_dat.rds")
# if for evaluation or predict run this code, then go to import models section######################
test_dat = readRDS("./1_asc_mlp/onehot_binary_1d/test_dat.rds")
test_in_base = test_dat[,-which(
  names(test_dat) %in% c('PROJECT_CODE','SITE_ID',ex_names,ord_names,so_names,gg_names,sg_names))]
test_in_ex = test_dat[,ex_names]
test_tag = test_dat[,tag_names]
# input data and tags for tr and test ######################
tr_in_base = tr_dat[,-which(
  names(tr_dat) %in% c('PROJECT_CODE','SITE_ID',ex_names,ord_names,so_names,gg_names,sg_names))]
tr_in_ex = tr_dat[,ex_names]

test_in_base = test_dat[,-which(
  names(test_dat) %in% c('PROJECT_CODE','SITE_ID',ex_names,ord_names,so_names,gg_names,sg_names))]
test_in_ex = test_dat[,ex_names]

tr_tag = tr_dat[,tag_names]
test_tag = test_dat[,tag_names]
# tr_tag = list(); test_tag = list(); j= 1
# for (i in tag_names) {
#   tag = switch(substr(i,1,4),
#                'ASC-'= paste('ord',tolower(substr(i,9,10)),sep = '_'),
#                'SUBO'= paste('so',tolower(substr(i,17,18)),sep = '_'),
#                'GREA'= paste('gg',tolower(substr(i,22,23)),sep = '_'),
#                'SUBG'= paste('sg',tolower(substr(i,19,20)),sep = '_')
#   )
#   tr_tag[[j]] = assign(paste('tr_tag',tag,sep = '_'),as.data.frame(tr_dat[,i]))
#   test_tag[[j]] = assign(paste('test_tag',tag,sep = '_'),as.data.frame(test_dat[,i]))
#   j = j+1
# }


#  model config and fitting ###############
j = 1; list_base_in = list(); list_ex_in = list();
list_output = list(); list_model = list();
data_predict = as.data.frame(matrix(0, ncol = length(tag_names), nrow = nrow(test_in_base)))

for (i in tag_names) {
  tag = switch(substr(i,1,4),
               'ASC_'= paste('ord',tolower(substr(i,9,10)),sep = '_'),
               'SUBO'= paste('so',tolower(substr(i,17,18)),sep = '_'),
               'GREA'= paste('gg',tolower(substr(i,22,23)),sep = '_'),
               'SUBG'= paste('sg',tolower(substr(i,19,20)),sep = '_')
  )
  x_tr = switch(substr(i,1,4),
             'ASC_'= as.matrix(tr_in_base),
             'SUBO'= list(as.matrix(tr_in_base),as.matrix(tr_in_ex)),
             'GREA'= list(as.matrix(tr_in_base),as.matrix(tr_in_ex)),
             'SUBG'= list(as.matrix(tr_in_base),as.matrix(tr_in_ex))
  )
  x_test = switch(substr(i,1,4),
             'ASC_'= as.matrix(test_in_base),
             'SUBO'= list(as.matrix(test_in_base),as.matrix(test_in_ex)),
             'GREA'= list(as.matrix(test_in_base),as.matrix(test_in_ex)),
             'SUBG'= list(as.matrix(test_in_base),as.matrix(test_in_ex))
  )
  list_base_in = c(list_base_in,assign(paste('base_in',tag,sep = '_'),
                                       layer_input(shape(c(length(tr_in_base))),name = paste('base_in',tag,sep = '_'))))
  if (substr(i,1,3)=='ASC') {
    list_ex_in = c(list_ex_in,j)
    list_output = c(list_output,assign(paste('output',tag,sep = '_'),list_base_in[[j]] %>%
                                         layer_dense(units = 64, activation = 'relu',
                                                     name = paste('dense',tag,sep = '_')) %>%
                                         layer_dense(units = 1, activation = 'sigmoid',
                                                     name = paste('output',tag,sep = '_'))))
    list_model = c(list_model, assign(paste('model',tag,sep = '_'),
                                      keras_model(list_base_in[[j]],list_output[[j]])))
  }
 
  else {
    list_ex_in = c(list_ex_in, assign(paste('so_ex_in',tag,sep = '_'),
                                            layer_input(shape(c(length(tr_in_ex))),
                                                        name = paste('so_ex_in',tag,sep = '_'))))
    list_output = c(list_output,assign(paste('output',tag,sep = '_'),
                                       layer_concatenate(list(list_base_in[[j]],list_ex_in[[j]]),
                                                         axis = 1,name = paste('con',tag,sep = '_')) %>%
                                         layer_dense(units = 64, activation = 'relu',
                                                     name = paste('dense',tag,sep = '_')) %>%
                                         layer_dense(units = 1, activation = 'sigmoid',
                                                     name = paste('output',tag,sep = '_'))))
    list_model = c(list_model, assign(paste('model',tag,sep = '_'),
                                      keras_model(list(list_base_in[[j]],list_ex_in[[j]]),list_output[[j]])))
    
    
  }
  
  list_model[[j]] %>% compile(
    optimizer=optimizer_rmsprop(),
    #loss = loss_binary_crossentropy,
    loss = "binary_crossentropy",
    metrics = metric_binary_accuracy
  )
  #assign(paste('history',tag,sep = '_'),
  list_model[[j]] %>% fit (
    x = x_tr,
    y = tr_tag[[j]],
    epochs=20,
    batch_size=512,
    class_weight = NULL,
    view_metrics = F,
    #validation_split=0.15,
    validation_data = list(x_test,test_tag[[j]])
  )#)
  
  list_model[[j]] %>% save_model_hdf5(paste('./1_asc_mlp/onehot_binary_1d/model_',tag,'.h5',sep = ''))
  
  # assign(paste('relust',tag,sep = '_'),
  #        list_model[[j]] %>% evaluate (
  #          as.matrix(test_in_base),
  #          test_tag[[j]]))
  data_predict[[j]] = #assign(paste('predict',tag,sep = '_'),
         list_model[[j]] %>% predict(x_test)#)
  names(data_predict)[j] = colnames(test_tag[j])
  j = j+1
}


# import models ##########################
j = 1
for (i in tag_names) {
  tag = switch(substr(i,1,4),
               'ASC_'= paste('ord',tolower(substr(i,9,10)),sep = '_'),
               'SUBO'= paste('so',tolower(substr(i,17,18)),sep = '_'),
               'GREA'= paste('gg',tolower(substr(i,22,23)),sep = '_'),
               'SUBG'= paste('sg',tolower(substr(i,19,20)),sep = '_')
  )
  file = paste('model_',tag,'.h5',sep = '')
  path = "./1_asc_mlp/onehot_binary_1d/"
  list_model[[j]] = load_model_hdf5(paste(path,file,sep = ''))
  j = j+1
}
# predict on test data ##########################
j=1
data_predict = as.data.frame(matrix(0, ncol = length(tag_names), nrow = nrow(test_in_base)))
for ( i in tag_names) {
  
  x_test = switch(substr(i,1,4),
                  'ASC_'= as.matrix(test_in_base),
                  'SUBO'= list(as.matrix(test_in_base),as.matrix(test_in_ex)),
                  'GREA'= list(as.matrix(test_in_base),as.matrix(test_in_ex)),
                  'SUBG'= list(as.matrix(test_in_base),as.matrix(test_in_ex))
  )
  data_predict[[j]] = list_model[[j]] %>% predict(x_test)
  names(data_predict)[j] = colnames(test_tag[j])
  j = j+1
}

# predict update ###################
# dat: the predict result data frame, such as data_predict
#out put : updated predict result only related value will remain
predict_update <- function(dat, threshold = 0.5) {
  # the list ######################
  ka_SO = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE',colnames(dat[,tag_names]))
  or_SO = grep('SUBORD_ASC_CODE_BW|SUBORD_ASC_CODE_CE|SUBORD_ASC_CODE_EH',colnames(dat[,tag_names]))
  po_SO = grep('SUBORD_ASC_CODE_AL|SUBORD_ASC_CODE_EJ|SUBORD_ASC_CODE_JD|SUBORD_ASC_CODE_JE|SUBORD_ASC_CODE_AM',colnames(dat[,tag_names]))
  an_SO = grep('SUBORD_ASC_CODE_IT|SUBORD_ASC_CODE_HR|SUBORD_ASC_CODE_HS|SUBORD_ASC_CODE_HT|SUBORD_ASC_CODE_HU|SUBORD_ASC_CODE_HV|SUBORD_ASC_CODE_HW|SUBORD_ASC_CODE_HX',colnames(dat[,tag_names]))
  ca_SO = grep('SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CV|SUBORD_ASC_CODE_DA|SUBORD_ASC_CODE_FB|SUBORD_ASC_CODE_CQ|SUBORD_ASC_CODE_BD',colnames(dat[,tag_names]))
  de_SO = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC',colnames(dat[,tag_names]))
  ru_SO = grep('SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_HG|SUBORD_ASC_CODE_AO|SUBORD_ASC_CODE_GV|SUBORD_ASC_CODE_ER|SUBORD_ASC_CODE_HH|SUBORD_ASC_CODE_CY',colnames(dat[,tag_names]))
  hy_SO = grep('SUBORD_ASC_CODE_IU|SUBORD_ASC_CODE_IV|SUBORD_ASC_CODE_CW|SUBORD_ASC_CODE_EW|SUBORD_ASC_CODE_BT|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EG|SUBORD_ASC_CODE_ED|SUBORD_ASC_CODE_DT',colnames(dat[,tag_names]))
  te_SO = grep('SUBORD_ASC_CODE_BF|SUBORD_ASC_CODE_BE|SUBORD_ASC_CODE_IL|SUBORD_ASC_CODE_IM|SUBORD_ASC_CODE_AW|SUBORD_ASC_CODE_CY|SUBORD_ASC_CODE_GZ|SUBORD_ASC_CODE_IN|SUBORD_ASC_CODE_IO|SUBORD_ASC_CODE_IP|SUBORD_ASC_CODE_IQ|SUBORD_ASC_CODE_IR',colnames(dat[,tag_names]))
  ve_SO = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE|SUBORD_ASC_CODE_AM',colnames(dat[,tag_names]))
  
  GG_na = c(0)
  or_GG =  grep('GREAT_GROUP_ASC_CODE_IF|GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_BC|GREAT_GROUP_ASC_CODE_AR|GREAT_GROUP_ASC_CODE_AI',colnames(dat[,tag_names]))
  po_al =  grep('GREAT_GROUP_ASC_CODE_EB|GREAT_GROUP_ASC_CODE_EK|GREAT_GROUP_ASC_CODE_CO|GREAT_GROUP_ASC_CODE_IG',colnames(dat[,tag_names]))
  po_ej =  grep('GREAT_GROUP_ASC_CODE_EB|GREAT_GROUP_ASC_CODE_EK|GREAT_GROUP_ASC_CODE_CO|GREAT_GROUP_ASC_CODE_CG|GREAT_GROUP_ASC_CODE_CJ|GREAT_GROUP_ASC_CODE_CI|GREAT_GROUP_ASC_CODE_IH',colnames(dat[,tag_names]))
  po_OT =  grep('GREAT_GROUP_ASC_CODE_CG|GREAT_GROUP_ASC_CODE_IH',colnames(dat[,tag_names]))
  ve_GG =  grep('GREAT_GROUP_ASC_CODE_EI|GREAT_GROUP_ASC_CODE_GS|GREAT_GROUP_ASC_CODE_BH|GREAT_GROUP_ASC_CODE_DF|GREAT_GROUP_ASC_CODE_DW',colnames(dat[,tag_names]))
  hy_iu =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IY|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JB|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_CF|GREAT_GROUP_ASC_CODE_FW|GREAT_GROUP_ASC_CODE_FY|GREAT_GROUP_ASC_CODE_AQ|GREAT_GROUP_ASC_CODE_FX|GREAT_GROUP_ASC_CODE_BV',colnames(dat[,tag_names]))
  hy_ew =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_BZ|GREAT_GROUP_ASC_CODE_FY|GREAT_GROUP_ASC_CODE_DQ|GREAT_GROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  hy_cs =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_BZ|GREAT_GROUP_ASC_CODE_FY|GREAT_GROUP_ASC_CODE_DQ|GREAT_GROUP_ASC_CODE_CD|GREAT_GROUP_ASC_CODE_CC',colnames(dat[,tag_names]))
  hy_bt =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_CB|GREAT_GROUP_ASC_CODE_CX|GREAT_GROUP_ASC_CODE_EQ|GREAT_GROUP_ASC_CODE_BG|GREAT_GROUP_ASC_CODE_FQ|GREAT_GROUP_ASC_CODE_FR|GREAT_GROUP_ASC_CODE_GT|GREAT_GROUP_ASC_CODE_GR',colnames(dat[,tag_names]))
  ku_GG =  grep('GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_GP|GREAT_GROUP_ASC_CODE_DB|GREAT_GROUP_ASC_CODE_FD|GREAT_GROUP_ASC_CODE_AF|GREAT_GROUP_ASC_CODE_AG|GREAT_GROUP_ASC_CODE_AH',colnames(dat[,tag_names]))
  so_GG =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_BK|GREAT_GROUP_ASC_CODE_IE|GREAT_GROUP_ASC_CODE_FN|GREAT_GROUP_ASC_CODE_ES|GREAT_GROUP_ASC_CODE_FO|GREAT_GROUP_ASC_CODE_DP|GREAT_GROUP_ASC_CODE_FP|GREAT_GROUP_ASC_CODE_CR',colnames(dat[,tag_names]))
  ch_GG =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_BK|GREAT_GROUP_ASC_CODE_ET|GREAT_GROUP_ASC_CODE_DB|GREAT_GROUP_ASC_CODE_AF|GREAT_GROUP_ASC_CODE_AG|GREAT_GROUP_ASC_CODE_AH|GREAT_GROUP_ASC_CODE_CV|GREAT_GROUP_ASC_CODE_DA|GREAT_GROUP_ASC_CODE_FB|GREAT_GROUP_ASC_CODE_CQ|GREAT_GROUP_ASC_CODE_BD',colnames(dat[,tag_names]))
  ca_OT =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_EE|GREAT_GROUP_ASC_CODE_AP|GREAT_GROUP_ASC_CODE_DY|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_DD|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  fe_GG =  grep('GREAT_GROUP_ASC_CODE_BD|GREAT_GROUP_ASC_CODE_AF|GREAT_GROUP_ASC_CODE_AG|GREAT_GROUP_ASC_CODE_AH|GREAT_GROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  ka_GG =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_EC|GREAT_GROUP_ASC_CODE_DO|GREAT_GROUP_ASC_CODE_DB|GREAT_GROUP_ASC_CODE_AF|GREAT_GROUP_ASC_CODE_AG|GREAT_GROUP_ASC_CODE_AH|GREAT_GROUP_ASC_CODE_CV|GREAT_GROUP_ASC_CODE_DA|GREAT_GROUP_ASC_CODE_FB|GREAT_GROUP_ASC_CODE_CQ|GREAT_GROUP_ASC_CODE_BD',colnames(dat[,tag_names]))
  ru_cs =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_BZ|GREAT_GROUP_ASC_CODE_CC',colnames(dat[,tag_names]))
  ru_hh =  grep('GREAT_GROUP_ASC_CODE_HF|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_AS|GREAT_GROUP_ASC_CODE_HI|GREAT_GROUP_ASC_CODE_BX|GREAT_GROUP_ASC_CODE_HJ',colnames(dat[,tag_names]))
  ru_cy =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_GE|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU',colnames(dat[,tag_names]))
  te_be =  grep('GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_EC|GREAT_GROUP_ASC_CODE_AK|GREAT_GROUP_ASC_CODE_HF|GREAT_GROUP_ASC_CODE_AS|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_IA|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_EL|GREAT_GROUP_ASC_CODE_DD|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  te_il =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_EF|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_AP|GREAT_GROUP_ASC_CODE_IA|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  te_im =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_AK|GREAT_GROUP_ASC_CODE_HF|GREAT_GROUP_ASC_CODE_AP|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_AO|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  te_aw =  grep('GREAT_GROUP_ASC_CODE_GE|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU',colnames(dat[,tag_names]))
  te_gz =  grep('GREAT_GROUP_ASC_CODE_FK|GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_GE|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_IS|GREAT_GROUP_ASC_CODE_EF|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_AS|GREAT_GROUP_ASC_CODE_AK|GREAT_GROUP_ASC_CODE_HF|GREAT_GROUP_ASC_CODE_AP|GREAT_GROUP_ASC_CODE_IA|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_AO|GREAT_GROUP_ASC_CODE_EL|GREAT_GROUP_ASC_CODE_DD|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  
  po_SG = grep('SUBGROUP_ASC_CODE_DX|SUBGROUP_ASC_CODE_GD|SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_CN|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DJ|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_HN|SUBGROUP_ASC_CODE_BI|SUBGROUP_ASC_CODE_EC|SUBGROUP_ASC_CODE_EM|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DV|SUBGROUP_ASC_CODE_BY',colnames(dat[,tag_names]))
  ve_SG = grep('SUBGROUP_ASC_CODE_EG|SUBGROUP_ASC_CODE_EV|SUBGROUP_ASC_CODE_IW|SUBGROUP_ASC_CODE_EU|SUBGROUP_ASC_CODE_IX|SUBGROUP_ASC_CODE_IZ|SUBGROUP_ASC_CODE_JA|SUBGROUP_ASC_CODE_JC|SUBGROUP_ASC_CODE_BJ|SUBGROUP_ASC_CODE_DZ|SUBGROUP_ASC_CODE_GQ|SUBGROUP_ASC_CODE_BZ|SUBGROUP_ASC_CODE_EP|SUBGROUP_ASC_CODE_GG|SUBGROUP_ASC_CODE_GH|SUBGROUP_ASC_CODE_GI|SUBGROUP_ASC_CODE_BN|SUBGROUP_ASC_CODE_CU|SUBGROUP_ASC_CODE_GN|SUBGROUP_ASC_CODE_GK|SUBGROUP_ASC_CODE_GA|SUBGROUP_ASC_CODE_GJ|SUBGROUP_ASC_CODE_FM|SUBGROUP_ASC_CODE_GO|SUBGROUP_ASC_CODE_BR|SUBGROUP_ASC_CODE_GB|SUBGROUP_ASC_CODE_FY|SUBGROUP_ASC_CODE_GL|SUBGROUP_ASC_CODE_BL|SUBGROUP_ASC_CODE_GM|SUBGROUP_ASC_CODE_JF|SUBGROUP_ASC_CODE_JG|SUBGROUP_ASC_CODE_BP|SUBGROUP_ASC_CODE_JH|SUBGROUP_ASC_CODE_JI|SUBGROUP_ASC_CODE_HE|SUBGROUP_ASC_CODE_FZ|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  hy_cw_iy = grep('SUBGROUP_ASC_CODE_BW|SUBGROUP_ASC_CODE_CE|SUBGROUP_ASC_CODE_EH',colnames(dat[,tag_names]))
  hy_eg_GG = grep('SUBGROUP_ASC_CODE_HM|SUBGROUP_ASC_CODE_HL|SUBGROUP_ASC_CODE_DZ',colnames(dat[,tag_names]))
  hy_ed_GG = grep('SUBGROUP_ASC_CODE_IT|SUBGROUP_ASC_CODE_GD|SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_CL|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_GU|SUBGROUP_ASC_CODE_EY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DH|SUBGROUP_ASC_CODE_EZ|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DL|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_BB|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_GW|SUBGROUP_ASC_CODE_HC|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_GX|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_EM|SUBGROUP_ASC_CODE_HO|SUBGROUP_ASC_CODE_AU|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_GP|SUBGROUP_ASC_CODE_FD|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_AX|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_AF|SUBGROUP_ASC_CODE_AG|SUBGROUP_ASC_CODE_AH|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  ku_SG = grep('SUBGROUP_ASC_CODE_EY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_EZ|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_BB|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_HB|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_AZ|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  so_SG = grep('SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_BZ|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_EM|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AF|SUBGROUP_ASC_CODE_AG|SUBGROUP_ASC_CODE_AH|SUBGROUP_ASC_CODE_CV|SUBGROUP_ASC_CODE_DA|SUBGROUP_ASC_CODE_FB|SUBGROUP_ASC_CODE_CQ|SUBGROUP_ASC_CODE_BD',colnames(dat[,tag_names]))
  ch_SG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_EY|SUBGROUP_ASC_CODE_CM|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DI|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DM|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_BB|SUBGROUP_ASC_CODE_EXVBZ|SUBGROUP_ASC_CODE_HC|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_EI|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_HB|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_AZ|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  ca_SG = grep('SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_FL|SUBGROUP_ASC_CODE_ET|SUBGROUP_ASC_CODE_BZ|SUBGROUP_ASC_CODE_IB|SUBGROUP_ASC_CODE_HK|SUBGROUP_ASC_CODE_CP|SUBGROUP_ASC_CODE_BR|SUBGROUP_ASC_CODE_BP|SUBGROUP_ASC_CODE_IC',colnames(dat[,tag_names]))
  fe_SG = grep('SUBGROUP_ASC_CODE_EN|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DM|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_GW|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  de_SG = grep('SUBGROUP_ASC_CODE_CM|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_GC|SUBGROUP_ASC_CODE_DI|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DM|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_HA|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_BB|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_BZ|SUBGROUP_ASC_CODE_GW|SUBGROUP_ASC_CODE_HC|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_GX|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_HO|SUBGROUP_ASC_CODE_AU|SUBGROUP_ASC_CODE_AJ|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_HB|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_AZ|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  ka_SG = grep('SUBGROUP_ASC_CODE_CM|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DI|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DM|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_AP|SUBGROUP_ASC_CODE_AS|SUBGROUP_ASC_CODE_GW|SUBGROUP_ASC_CODE_HC|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_GX|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_HO|SUBGROUP_ASC_CODE_AU|SUBGROUP_ASC_CODE_AJ|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_HB|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_AZ|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  ru_fj_GG = grep('SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_bf_GG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DK',colnames(dat[,tag_names]))
  te_be_GG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_GU|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_FU|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_FC|SUBGROUP_ASC_CODE_DK',colnames(dat[,tag_names]))
  te_il_GG = grep('SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_im_GG = grep('SUBGROUP_ASC_CODE_DA|SUBGROUP_ASC_CODE_FB|SUBGROUP_ASC_CODE_CQ',colnames(dat[,tag_names]))
  te_aw_GG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_GU|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_FU|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_FC|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_cy_GG = grep('SUBGROUP_ASC_CODE_ID|SUBGROUP_ASC_CODE_DR|SUBGROUP_ASC_CODE_FF|SUBGROUP_ASC_CODE_FG|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_gz_GG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_GU|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_FU|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_FC|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_in_GG = grep('SUBGROUP_ASC_CODE_ID|SUBGROUP_ASC_CODE_DR|SUBGROUP_ASC_CODE_FF|SUBGROUP_ASC_CODE_FG|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  # code###################
  for (i in 1 : nrow(dat)) {
    idx =which(dat[i,ord_names]>threshold)[1]
    if (is.na(idx)){
      tag_ord = colnames(dat[,ord_names])[apply(dat[i,ord_names],1,which.max)] 
    }
    else {
      dat[i,ord_names][-idx] = 0
    }
    tag_ord = colnames(dat[,ord_names])[apply(dat[i,ord_names],1,which.max)] 
    so_idx = switch(substr(tag_ord,9,10),
                    'AN'= an_SO,'OR'= or_SO,'PO'= po_SO,'VE'= ve_SO,'HY'= hy_SO,'KU'= ka_SO,'SO'= ka_SO,
                    'CH'= ka_SO,'CA'= ca_SO,'FE'= de_SO,'DE'= de_SO,'KA'= ka_SO,'RU'= ru_SO,'TE'= te_SO,)
    dat[i,so_names][-(so_idx-length(ord_names))]=0
    tag_so = colnames(dat[,so_names])[apply(dat[i,so_names],1,which.max)] 
    gg_idx = switch(substr(tag_ord,9,10),
                    'AN'= GG_na,
                    'OR'= or_GG,
                    'PO'= switch(substr(tag_so,17,18),'AL'= po_al,'EJ'=po_ej, 'JD'=po_OT,'JE'=po_OT,'AM'=po_OT),
                    'VE'= ve_GG,
                    'HY'= switch(substr(tag_so,17,18),'IU'= hy_iu,'IV'=hy_iu, 'CW'=hy_iu,'EW'=hy_ew,'BT'=hy_bt,
                                 'CS'=hy_cs,'EG'=hy_bt,'ED'=hy_bt,'DT'=hy_bt),
                    'KU'= ku_GG,
                    'SO'= so_GG,
                    'CH'= ch_GG,
                    'CA'= switch(substr(tag_so,17,18),'EL'= GG_na,'FJ'= GG_na, 'CV'= ca_OT,'DA'= ca_OT,
                                 'FB'= ca_OT,'CQ'= ca_OT,'BD'= ca_OT),
                    'FE'= fe_GG,
                    'DE'= ch_GG,
                    'KA'= ka_GG,
                    'RU'= switch(substr(tag_so,17,18),'FJ'= GG_na,'CS'= ru_cs, 'EL'= GG_na,'HG'= GG_na,
                                 'AO'= GG_na,'GV'= GG_na,'ER'= GG_na,'HH'=ru_hh,'CY'=ru_cy),
                    'TE'= switch(substr(tag_so,17,18),'BF'= ru_cy,'BE'= te_be, 'IL'= te_il,'IM'=te_im,'AW'= te_aw,
                                 'CY'= ru_cy,'GZ'= te_gz,'IN'= te_gz,'IO'=te_gz,'IP'=te_gz,'IQ'=te_gz,'IR'=te_gz)
    )
    if (gg_idx[1] != 0) {
      dat[i,gg_names][-(gg_idx-length(ord_names)-length(so_names))]=0
      tag_gg = colnames(dat[,gg_names])[apply(dat[i,gg_names],1,which.max)] 
      
      sg_idx = switch(substr(tag_ord,9,10),
                      'AN'= GG_na,
                      'OR'= GG_na,
                      'PO'= po_SG,
                      'VE'= ve_SG,
                      'HY'= switch(substr(tag_so,17,18),
                                   'IU'= GG_na,
                                   'IV'=GG_na, 
                                   'CW'=switch(substr(tag_gg,22,23),'EV'= GG_na,'IW'= GG_na, 'EU'= GG_na,'IX'= GG_na,
                                               'IY'= hy_cw_iy,'IZ'= GG_na,'JA'= GG_na,'JB'= hy_cw_iy,'JC'= GG_na,
                                               'CF'= hy_cw_iy,'FW'= GG_na,'FY'= GG_na,'AQ'= GG_na,'FX'= GG_na,'BV'= GG_na),
                                   'EW'=GG_na,
                                   'BT'=GG_na,
                                   'CS'=GG_na,
                                   'EG'=hy_eg_GG,
                                   'ED'=hy_ed_GG,
                                   'DT'=hy_ed_GG),
                      'KU'= ku_SG,
                      'SO'= so_SG,
                      'CH'= ch_SG,
                      'CA'= ca_SG,
                      'FE'= fe_SG,
                      'DE'= de_SG,
                      'KA'= ka_SG,
                      'RU'= switch(substr(tag_so,17,18),'FJ'= ru_fj_GG,'CS'= ru_fj_GG, 'EL'= GG_na,'HG'= GG_na,
                                   'AO'= ru_fj_GG,'GV'= ru_fj_GG,'ER'= ru_fj_GG,'HH'=ru_fj_GG,'CY'=ru_fj_GG),
                      'TE'= switch(substr(tag_so,17,18),'BF'= te_bf_GG,'BE'= te_be_GG, 'IL'= te_il_GG,'IM'=te_im_GG,'AW'= te_aw_GG,
                                   'CY'= te_cy_GG,'GZ'= te_gz_GG,'IN'= te_in_GG,'IO'=te_in_GG,'IP'=te_in_GG,'IQ'=te_in_GG,'IR'=te_in_GG))
      if (sg_idx[1]!=0) {
        dat[i,sg_names][-(sg_idx-length(ord_names)-length(so_names)-length(gg_names))] = 0
      }
      
    }
  }
  
  return(dat)
}

updated_predict = predict_update(data_predict) 
  
# Single level acc analysis ###########################
ord_predict = updated_predict[,ord_names]
so_predict = updated_predict[,so_names]
gg_predict = updated_predict[,gg_names]
sg_predict = updated_predict[,sg_names]
# pred: a single data, 
asc_evaluation_single <- function(pred,tag) {
  pred = cbind(colnames(pred)[apply(pred,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
               colnames(pred)[apply(pred,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                              match(sort(x[x>0],decreasing = T)[2],x)
               ))],
               colnames(tag)[apply(tag,1,which.max)],
               pred)
  
  colnames(pred)[1:3]=c('ORD_PRE1','ORD_PRE2','ORD_REAL')
  pred$ORD_PRE_TOP2 = pred$ORD_PRE1
  pred=pred[,c(1,2,length(pred),3)]
  pred[] <- lapply(pred, as.character)
  pred[as.character(pred$ORD_PRE2)==as.character(pred$ORD_REAL),'ORD_PRE_TOP2'] =
    pred[as.character(pred$ORD_PRE2)==as.character(pred$ORD_REAL),'ORD_PRE2']
  pred[] = lapply(pred,as.factor)
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
ord_evaluate = asc_evaluation_single(ord_predict,test_tag[ord_names])
so_evaluate = asc_evaluation_single(so_predict,test_tag[so_names])
gg_evaluate = asc_evaluation_single(gg_predict,test_tag[gg_names])
sg_evaluate = asc_evaluation_single(sg_predict,test_tag[sg_names])
ord_evaluate[[1]]$overall
ord_evaluate[[2]]$overall
so_evaluate[[1]]$overall
so_evaluate[[2]]$overall
gg_evaluate[[1]]$overall
gg_evaluate[[2]]$overall
sg_evaluate[[1]]$overall
sg_evaluate[[2]]$overall

# Hierarchical acc#################
top1_top2 <- function(pred,tag) {
  name = switch(substr(colnames(tag)[1],1,4),
                'ASC_' = 'ORD',
                'SUBO' = 'SO',
                'GREA' = 'GG',
                'SUBG' = 'SG')
  
  pred = cbind(colnames(pred)[apply(pred,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
               colnames(pred)[apply(pred,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                              match(sort(x[x>0],decreasing = T)[2],x)
               ))],
               colnames(tag)[apply(tag,1,which.max)],
               pred)
  
  colnames(pred)[1:3]=c(paste(name,'PRE1',sep = '_'),
                        paste(name,'PRE2',sep = '_'),
                        paste(name,'REAL',sep = '_'))
  pred[,paste(name,'PRE_TOP2',sep = '_')] = pred[,1]
  pred=pred[,c(1,2,length(pred),3)]
  pred[] <- lapply(pred, as.character)
  pred[as.character(pred[,2])==as.character(pred[,4]),3] =
    pred[as.character(pred[,2])==as.character(pred[,4]),2]
  pred[] = lapply(pred,as.factor)
  return(pred)
}

ord_predict_h = top1_top2(updated_predict[,ord_names],test_tag[ord_names])
so_predict_h = top1_top2(updated_predict[,so_names],test_tag[so_names])
gg_predict_h = top1_top2(updated_predict[,gg_names],test_tag[gg_names])
sg_predict_h = top1_top2(updated_predict[,sg_names],test_tag[sg_names])

asc_predict_h = data.frame(ord_predict_h$ORD_PRE1,ord_predict_h$ORD_PRE_TOP2,ord_predict_h$ORD_REAL,
                         so_predict_h$SO_PRE1,so_predict_h$SO_PRE_TOP2,so_predict_h$SO_REAL,
                         gg_predict_h$GG_PRE1,gg_predict_h$GG_PRE_TOP2,gg_predict_h$GG_REAL,
                         sg_predict_h$SG_PRE1,sg_predict_h$SG_PRE_TOP2,sg_predict_h$SG_REAL)

asc_predict_h$P_SG = paste(str_sub(asc_predict_h[[1]],-2,-1),
                         str_sub(asc_predict_h[[4]],-2,-1),
                         str_sub(asc_predict_h[[7]],-2,-1),
                         str_sub(asc_predict_h[[10]],-2,-1),
                         sep='_')
asc_predict_h$P_SG_TOP2 = paste(str_sub(asc_predict_h[[2]],-2,-1),
                              str_sub(asc_predict_h[[5]],-2,-1),
                              str_sub(asc_predict_h[[8]],-2,-1),
                              str_sub(asc_predict_h[[11]],-2,-1),
                              sep='_')

asc_predict_h$R_SG = paste(str_sub(asc_predict_h[[3]],-2,-1),
                         str_sub(asc_predict_h[[6]],-2,-1),
                         str_sub(asc_predict_h[[9]],-2,-1),
                         str_sub(asc_predict_h[[12]],-2,-1),
                         sep='_')
asc_predict_h$P_GG = paste(str_sub(asc_predict_h[[1]],-2,-1),
                         str_sub(asc_predict_h[[4]],-2,-1),
                         str_sub(asc_predict_h[[7]],-2,-1),
                         sep='_')
asc_predict_h$P_GG_TOP2 = paste(str_sub(asc_predict_h[[2]],-2,-1),
                              str_sub(asc_predict_h[[5]],-2,-1),
                              str_sub(asc_predict_h[[8]],-2,-1),
                              sep='_')
asc_predict_h$R_GG = paste(str_sub(asc_predict_h[[3]],-2,-1),
                         str_sub(asc_predict_h[[6]],-2,-1),
                         str_sub(asc_predict_h[[9]],-2,-1),
                         sep='_')
asc_predict_h$P_SO = paste(str_sub(asc_predict_h[[1]],-2,-1),
                         str_sub(asc_predict_h[[4]],-2,-1),
                         sep='_')
asc_predict_h$P_SO_TOP2 = paste(str_sub(asc_predict_h[[2]],-2,-1),
                              str_sub(asc_predict_h[[5]],-2,-1),
                              sep='_')
asc_predict_h$R_SO = paste(str_sub(asc_predict_h[[3]],-2,-1),
                         str_sub(asc_predict_h[[6]],-2,-1),
                         sep='_')
asc_predict_h$P_ORD = paste(str_sub(asc_predict_h[[1]],-2,-1),
                          sep='_')
asc_predict_h$P_ORD_TOP2 = paste(str_sub(asc_predict_h[[2]],-2,-1),
                               sep='_')
asc_predict_h$R_ORD = paste(str_sub(asc_predict_h[[3]],-2,-1),
                          sep='_')

asc_p = asc_predict_h[,13:24]

#SG first match ##################
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
#GG first match##################
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
#SO first match ##################
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
#ORD first match ##################
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
                data.frame('P_SG_TOP2'=rep(NA,length(sg_top2[which(is.na(match(sg_top2$P_SG_TOP2,sg_top2$R_SG))),'P_SG_TOP2'])),
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

#result ##############
ord_confusion$overall
ord_confusion_top2$overall
so_confusion$overall
so_confusion_top2$overall
gg_confusion$overall
gg_confusion_top2$overall
sg_confusion$overall
sg_confusion_top2$overall
write_rds(ord_confusion,"./1_asc_mlp/onehot_binary_1d/onehot_binary_1d_confusion_h_ord.rds")
write_rds(ord_confusion_top2,"./1_asc_mlp/onehot_binary_1d/onehot_binary_1d_confusion_h_ord_top2.rds")
write_rds(so_confusion,"./1_asc_mlp/onehot_binary_1d/onehot_binary_1d_confusion_h_so.rds")
write_rds(so_confusion_top2,"./1_asc_mlp/onehot_binary_1d/onehot_binary_1d_confusion_h_so_top2.rds")
write_rds(gg_confusion,"./1_asc_mlp/onehot_binary_1d/onehot_binary_1d_confusion_h_gg.rds")
write_rds(gg_confusion_top2,"./1_asc_mlp/onehot_binary_1d/onehot_binary_1d_confusion_h_gg_top2.rds")
write_rds(sg_confusion,"./1_asc_mlp/onehot_binary_1d/onehot_binary_1d_confusion_h_sg.rds")
write_rds(sg_confusion_top2,"./1_asc_mlp/onehot_binary_1d/onehot_binary_1d_confusion_h_sg_top2.rds")
# evaluation demos ##########################
# dat: a single data same as test file at beginning of file
# idx: a list of selected classes indexes to be compared and decided which one is the best selection
# threshold: 
#ouyput : the selected class name
tag_select <- function(dat,idx,threshole = 0.5) {
  dat_in_base = as.matrix(dat[,-which(
    names(dat) %in% c('PROJECT_CODE','SITE_ID',ex_names,ord_names,so_names,gg_names,sg_names))])
  dat_in_so_ex = as.matrix(dat[,ex_names])
  pred = as.data.frame(matrix(0,nrow = 1,ncol= length(tag_names)))
  tag = 'NA'
  
  if (! idx[1]==0) {
    if (idx[1]< 14) {
      for (i in idx) {
        pred[[i]] = list_model[[i]] %>% predict(dat_in_base)
        names(pred)[i] = tag_names[i]
        if (pred[[i]] > threshole) {
          tag = tag_names[i]
          break
        }
      }
      if (tag == 'NA') {
        tag = colnames(pred)[apply(pred,1,which.max)] }
    }
    else {
      for (i in idx) {
        pred[[i]] = list_model[[i]] %>% predict(list(dat_in_base,dat_in_so_ex))
        names(pred)[i] = tag_names[i]
        if (pred[[i]] > threshole) {
          tag = tag_names[i]
          break
        }
      }
      if (tag == 'NA') {
        tag = colnames(pred)[apply(pred,1,which.max)] }
    }
  }
  return(tag)
}
# dat: a single data same as test data at begining of this file
# threshole: a indicator deside the positive class in binary classifilers
tag_predict <- function(dat,threshole = 0.5) {
  
  ka_SO = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE',colnames(dat[,tag_names]))
  or_SO = grep('SUBORD_ASC_CODE_BW|SUBORD_ASC_CODE_CE|SUBORD_ASC_CODE_EH',colnames(dat[,tag_names]))
  po_SO = grep('SUBORD_ASC_CODE_AL|SUBORD_ASC_CODE_EJ|SUBORD_ASC_CODE_JD|SUBORD_ASC_CODE_JE|SUBORD_ASC_CODE_AM',colnames(dat[,tag_names]))
  an_SO = grep('SUBORD_ASC_CODE_IT|SUBORD_ASC_CODE_HR|SUBORD_ASC_CODE_HS|SUBORD_ASC_CODE_HT|SUBORD_ASC_CODE_HU|SUBORD_ASC_CODE_HV|SUBORD_ASC_CODE_HW|SUBORD_ASC_CODE_HX',colnames(dat[,tag_names]))
  ca_SO = grep('SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CV|SUBORD_ASC_CODE_DA|SUBORD_ASC_CODE_FB|SUBORD_ASC_CODE_CQ|SUBORD_ASC_CODE_BD',colnames(dat[,tag_names]))
  de_SO = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC',colnames(dat[,tag_names]))
  ru_SO = grep('SUBORD_ASC_CODE_FJ|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EL|SUBORD_ASC_CODE_HG|SUBORD_ASC_CODE_AO|SUBORD_ASC_CODE_GV|SUBORD_ASC_CODE_ER|SUBORD_ASC_CODE_HH|SUBORD_ASC_CODE_CY',colnames(dat[,tag_names]))
  hy_SO = grep('SUBORD_ASC_CODE_IU|SUBORD_ASC_CODE_IV|SUBORD_ASC_CODE_CW|SUBORD_ASC_CODE_EW|SUBORD_ASC_CODE_BT|SUBORD_ASC_CODE_CS|SUBORD_ASC_CODE_EG|SUBORD_ASC_CODE_ED|SUBORD_ASC_CODE_DT',colnames(dat[,tag_names]))
  te_SO = grep('SUBORD_ASC_CODE_BF|SUBORD_ASC_CODE_BE|SUBORD_ASC_CODE_IL|SUBORD_ASC_CODE_IM|SUBORD_ASC_CODE_AW|SUBORD_ASC_CODE_CY|SUBORD_ASC_CODE_GZ|SUBORD_ASC_CODE_IN|SUBORD_ASC_CODE_IO|SUBORD_ASC_CODE_IP|SUBORD_ASC_CODE_IQ|SUBORD_ASC_CODE_IR',colnames(dat[,tag_names]))
  ve_SO = grep('SUBORD_ASC_CODE_AA|SUBORD_ASC_CODE_AB|SUBORD_ASC_CODE_AC|SUBORD_ASC_CODE_AD|SUBORD_ASC_CODE_AE|SUBORD_ASC_CODE_AM',colnames(dat[,tag_names]))
  
  GG_na = c(0)
  or_GG =  grep('GREAT_GROUP_ASC_CODE_IF|GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_BC|GREAT_GROUP_ASC_CODE_AR|GREAT_GROUP_ASC_CODE_AI',colnames(dat[,tag_names]))
  po_al =  grep('GREAT_GROUP_ASC_CODE_EB|GREAT_GROUP_ASC_CODE_EK|GREAT_GROUP_ASC_CODE_CO|GREAT_GROUP_ASC_CODE_IG',colnames(dat[,tag_names]))
  po_ej =  grep('GREAT_GROUP_ASC_CODE_EB|GREAT_GROUP_ASC_CODE_EK|GREAT_GROUP_ASC_CODE_CO|GREAT_GROUP_ASC_CODE_CG|GREAT_GROUP_ASC_CODE_CJ|GREAT_GROUP_ASC_CODE_CI|GREAT_GROUP_ASC_CODE_IH',colnames(dat[,tag_names]))
  po_OT =  grep('GREAT_GROUP_ASC_CODE_CG|GREAT_GROUP_ASC_CODE_IH',colnames(dat[,tag_names]))
  ve_GG =  grep('GREAT_GROUP_ASC_CODE_EI|GREAT_GROUP_ASC_CODE_GS|GREAT_GROUP_ASC_CODE_BH|GREAT_GROUP_ASC_CODE_DF|GREAT_GROUP_ASC_CODE_DW',colnames(dat[,tag_names]))
  hy_iu =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IY|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JB|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_CF|GREAT_GROUP_ASC_CODE_FW|GREAT_GROUP_ASC_CODE_FY|GREAT_GROUP_ASC_CODE_AQ|GREAT_GROUP_ASC_CODE_FX|GREAT_GROUP_ASC_CODE_BV',colnames(dat[,tag_names]))
  hy_ew =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_BZ|GREAT_GROUP_ASC_CODE_FY|GREAT_GROUP_ASC_CODE_DQ|GREAT_GROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  hy_cs =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_BZ|GREAT_GROUP_ASC_CODE_FY|GREAT_GROUP_ASC_CODE_DQ|GREAT_GROUP_ASC_CODE_CD|GREAT_GROUP_ASC_CODE_CC',colnames(dat[,tag_names]))
  hy_bt =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_CB|GREAT_GROUP_ASC_CODE_CX|GREAT_GROUP_ASC_CODE_EQ|GREAT_GROUP_ASC_CODE_BG|GREAT_GROUP_ASC_CODE_FQ|GREAT_GROUP_ASC_CODE_FR|GREAT_GROUP_ASC_CODE_GT|GREAT_GROUP_ASC_CODE_GR',colnames(dat[,tag_names]))
  ku_GG =  grep('GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_GP|GREAT_GROUP_ASC_CODE_DB|GREAT_GROUP_ASC_CODE_FD|GREAT_GROUP_ASC_CODE_AF|GREAT_GROUP_ASC_CODE_AG|GREAT_GROUP_ASC_CODE_AH',colnames(dat[,tag_names]))
  so_GG =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_BK|GREAT_GROUP_ASC_CODE_IE|GREAT_GROUP_ASC_CODE_FN|GREAT_GROUP_ASC_CODE_ES|GREAT_GROUP_ASC_CODE_FO|GREAT_GROUP_ASC_CODE_DP|GREAT_GROUP_ASC_CODE_FP|GREAT_GROUP_ASC_CODE_CR',colnames(dat[,tag_names]))
  ch_GG =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_BK|GREAT_GROUP_ASC_CODE_ET|GREAT_GROUP_ASC_CODE_DB|GREAT_GROUP_ASC_CODE_AF|GREAT_GROUP_ASC_CODE_AG|GREAT_GROUP_ASC_CODE_AH|GREAT_GROUP_ASC_CODE_CV|GREAT_GROUP_ASC_CODE_DA|GREAT_GROUP_ASC_CODE_FB|GREAT_GROUP_ASC_CODE_CQ|GREAT_GROUP_ASC_CODE_BD',colnames(dat[,tag_names]))
  ca_OT =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_EE|GREAT_GROUP_ASC_CODE_AP|GREAT_GROUP_ASC_CODE_DY|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_DD|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  fe_GG =  grep('GREAT_GROUP_ASC_CODE_BD|GREAT_GROUP_ASC_CODE_AF|GREAT_GROUP_ASC_CODE_AG|GREAT_GROUP_ASC_CODE_AH|GREAT_GROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  ka_GG =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_EC|GREAT_GROUP_ASC_CODE_DO|GREAT_GROUP_ASC_CODE_DB|GREAT_GROUP_ASC_CODE_AF|GREAT_GROUP_ASC_CODE_AG|GREAT_GROUP_ASC_CODE_AH|GREAT_GROUP_ASC_CODE_CV|GREAT_GROUP_ASC_CODE_DA|GREAT_GROUP_ASC_CODE_FB|GREAT_GROUP_ASC_CODE_CQ|GREAT_GROUP_ASC_CODE_BD',colnames(dat[,tag_names]))
  ru_cs =  grep('GREAT_GROUP_ASC_CODE_EV|GREAT_GROUP_ASC_CODE_IW|GREAT_GROUP_ASC_CODE_EU|GREAT_GROUP_ASC_CODE_IX|GREAT_GROUP_ASC_CODE_IZ|GREAT_GROUP_ASC_CODE_JA|GREAT_GROUP_ASC_CODE_JC|GREAT_GROUP_ASC_CODE_BZ|GREAT_GROUP_ASC_CODE_CC',colnames(dat[,tag_names]))
  ru_hh =  grep('GREAT_GROUP_ASC_CODE_HF|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_AS|GREAT_GROUP_ASC_CODE_HI|GREAT_GROUP_ASC_CODE_BX|GREAT_GROUP_ASC_CODE_HJ',colnames(dat[,tag_names]))
  ru_cy =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_GE|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU',colnames(dat[,tag_names]))
  te_be =  grep('GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_EC|GREAT_GROUP_ASC_CODE_AK|GREAT_GROUP_ASC_CODE_HF|GREAT_GROUP_ASC_CODE_AS|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_IA|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_EL|GREAT_GROUP_ASC_CODE_DD|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  te_il =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_EF|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_AP|GREAT_GROUP_ASC_CODE_IA|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  te_im =  grep('GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_AK|GREAT_GROUP_ASC_CODE_HF|GREAT_GROUP_ASC_CODE_AP|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_AO|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  te_aw =  grep('GREAT_GROUP_ASC_CODE_GE|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU',colnames(dat[,tag_names]))
  te_gz =  grep('GREAT_GROUP_ASC_CODE_FK|GREAT_GROUP_ASC_CODE_BJ|GREAT_GROUP_ASC_CODE_GE|GREAT_GROUP_ASC_CODE_EA|GREAT_GROUP_ASC_CODE_EM|GREAT_GROUP_ASC_CODE_DZ|GREAT_GROUP_ASC_CODE_IS|GREAT_GROUP_ASC_CODE_EF|GREAT_GROUP_ASC_CODE_BU|GREAT_GROUP_ASC_CODE_AS|GREAT_GROUP_ASC_CODE_AK|GREAT_GROUP_ASC_CODE_HF|GREAT_GROUP_ASC_CODE_AP|GREAT_GROUP_ASC_CODE_IA|GREAT_GROUP_ASC_CODE_CZ|GREAT_GROUP_ASC_CODE_DU|GREAT_GROUP_ASC_CODE_AO|GREAT_GROUP_ASC_CODE_EL|GREAT_GROUP_ASC_CODE_DD|GREAT_GROUP_ASC_CODE_GF',colnames(dat[,tag_names]))
  
  po_SG = grep('SUBGROUP_ASC_CODE_DX|SUBGROUP_ASC_CODE_GD|SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_CN|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DJ|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_HN|SUBGROUP_ASC_CODE_BI|SUBGROUP_ASC_CODE_EC|SUBGROUP_ASC_CODE_EM|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DV|SUBGROUP_ASC_CODE_BY',colnames(dat[,tag_names]))
  ve_SG = grep('SUBGROUP_ASC_CODE_EG|SUBGROUP_ASC_CODE_EV|SUBGROUP_ASC_CODE_IW|SUBGROUP_ASC_CODE_EU|SUBGROUP_ASC_CODE_IX|SUBGROUP_ASC_CODE_IZ|SUBGROUP_ASC_CODE_JA|SUBGROUP_ASC_CODE_JC|SUBGROUP_ASC_CODE_BJ|SUBGROUP_ASC_CODE_DZ|SUBGROUP_ASC_CODE_GQ|SUBGROUP_ASC_CODE_BZ|SUBGROUP_ASC_CODE_EP|SUBGROUP_ASC_CODE_GG|SUBGROUP_ASC_CODE_GH|SUBGROUP_ASC_CODE_GI|SUBGROUP_ASC_CODE_BN|SUBGROUP_ASC_CODE_CU|SUBGROUP_ASC_CODE_GN|SUBGROUP_ASC_CODE_GK|SUBGROUP_ASC_CODE_GA|SUBGROUP_ASC_CODE_GJ|SUBGROUP_ASC_CODE_FM|SUBGROUP_ASC_CODE_GO|SUBGROUP_ASC_CODE_BR|SUBGROUP_ASC_CODE_GB|SUBGROUP_ASC_CODE_FY|SUBGROUP_ASC_CODE_GL|SUBGROUP_ASC_CODE_BL|SUBGROUP_ASC_CODE_GM|SUBGROUP_ASC_CODE_JF|SUBGROUP_ASC_CODE_JG|SUBGROUP_ASC_CODE_BP|SUBGROUP_ASC_CODE_JH|SUBGROUP_ASC_CODE_JI|SUBGROUP_ASC_CODE_HE|SUBGROUP_ASC_CODE_FZ|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  hy_cw_iy = grep('SUBGROUP_ASC_CODE_BW|SUBGROUP_ASC_CODE_CE|SUBGROUP_ASC_CODE_EH',colnames(dat[,tag_names]))
  hy_eg_GG = grep('SUBGROUP_ASC_CODE_HM|SUBGROUP_ASC_CODE_HL|SUBGROUP_ASC_CODE_DZ',colnames(dat[,tag_names]))
  hy_ed_GG = grep('SUBGROUP_ASC_CODE_IT|SUBGROUP_ASC_CODE_GD|SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_CL|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_GU|SUBGROUP_ASC_CODE_EY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DH|SUBGROUP_ASC_CODE_EZ|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DL|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_BB|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_GW|SUBGROUP_ASC_CODE_HC|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_GX|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_EM|SUBGROUP_ASC_CODE_HO|SUBGROUP_ASC_CODE_AU|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_GP|SUBGROUP_ASC_CODE_FD|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_AX|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_AF|SUBGROUP_ASC_CODE_AG|SUBGROUP_ASC_CODE_AH|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  ku_SG = grep('SUBGROUP_ASC_CODE_EY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_EZ|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_BB|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_HB|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_AZ|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  so_SG = grep('SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_BZ|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_EM|SUBGROUP_ASC_CODE_DB|SUBGROUP_ASC_CODE_AF|SUBGROUP_ASC_CODE_AG|SUBGROUP_ASC_CODE_AH|SUBGROUP_ASC_CODE_CV|SUBGROUP_ASC_CODE_DA|SUBGROUP_ASC_CODE_FB|SUBGROUP_ASC_CODE_CQ|SUBGROUP_ASC_CODE_BD',colnames(dat[,tag_names]))
  ch_SG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_EY|SUBGROUP_ASC_CODE_CM|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DI|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DM|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_BB|SUBGROUP_ASC_CODE_EXVBZ|SUBGROUP_ASC_CODE_HC|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_EI|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_HB|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_AZ|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  ca_SG = grep('SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_FL|SUBGROUP_ASC_CODE_ET|SUBGROUP_ASC_CODE_BZ|SUBGROUP_ASC_CODE_IB|SUBGROUP_ASC_CODE_HK|SUBGROUP_ASC_CODE_CP|SUBGROUP_ASC_CODE_BR|SUBGROUP_ASC_CODE_BP|SUBGROUP_ASC_CODE_IC',colnames(dat[,tag_names]))
  fe_SG = grep('SUBGROUP_ASC_CODE_EN|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DM|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_GW|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  de_SG = grep('SUBGROUP_ASC_CODE_CM|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_GC|SUBGROUP_ASC_CODE_DI|SUBGROUP_ASC_CODE_DN|SUBGROUP_ASC_CODE_DM|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_HA|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_BB|SUBGROUP_ASC_CODE_EX|SUBGROUP_ASC_CODE_BZ|SUBGROUP_ASC_CODE_GW|SUBGROUP_ASC_CODE_HC|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_GX|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_HO|SUBGROUP_ASC_CODE_AU|SUBGROUP_ASC_CODE_AJ|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_HB|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_AZ|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  ka_SG = grep('SUBGROUP_ASC_CODE_CM|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DI|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DM|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_AP|SUBGROUP_ASC_CODE_AS|SUBGROUP_ASC_CODE_GW|SUBGROUP_ASC_CODE_HC|SUBGROUP_ASC_CODE_AV|SUBGROUP_ASC_CODE_BU|SUBGROUP_ASC_CODE_GX|SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_HO|SUBGROUP_ASC_CODE_AU|SUBGROUP_ASC_CODE_AJ|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_BA|SUBGROUP_ASC_CODE_HB|SUBGROUP_ASC_CODE_EO|SUBGROUP_ASC_CODE_AZ|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_EF|SUBGROUP_ASC_CODE_DQ|SUBGROUP_ASC_CODE_CD',colnames(dat[,tag_names]))
  ru_fj_GG = grep('SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_bf_GG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_DK',colnames(dat[,tag_names]))
  te_be_GG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_GU|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_FU|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_FC|SUBGROUP_ASC_CODE_DK',colnames(dat[,tag_names]))
  te_il_GG = grep('SUBGROUP_ASC_CODE_AY|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_AT|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_im_GG = grep('SUBGROUP_ASC_CODE_DA|SUBGROUP_ASC_CODE_FB|SUBGROUP_ASC_CODE_CQ',colnames(dat[,tag_names]))
  te_aw_GG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_GU|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_FU|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_FC|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_cy_GG = grep('SUBGROUP_ASC_CODE_ID|SUBGROUP_ASC_CODE_DR|SUBGROUP_ASC_CODE_FF|SUBGROUP_ASC_CODE_FG|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_gz_GG = grep('SUBGROUP_ASC_CODE_DW|SUBGROUP_ASC_CODE_GY|SUBGROUP_ASC_CODE_GU|SUBGROUP_ASC_CODE_CK|SUBGROUP_ASC_CODE_FU|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_FV|SUBGROUP_ASC_CODE_FC|SUBGROUP_ASC_CODE_DK|SUBGROUP_ASC_CODE_DC|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  te_in_GG = grep('SUBGROUP_ASC_CODE_ID|SUBGROUP_ASC_CODE_DR|SUBGROUP_ASC_CODE_FF|SUBGROUP_ASC_CODE_FG|SUBGROUP_ASC_CODE_DG|SUBGROUP_ASC_CODE_AI|SUBGROUP_ASC_CODE_AR|SUBGROUP_ASC_CODE_BC',colnames(dat[,tag_names]))
  #tag_ord = 'ASC_ORE_PO'
  tag_ord = tag_select(dat,1:14)
  #print(tag_ord)
  so_idx = switch(substr(tag_ord,9,10),
                  'AN'= an_SO,'OR'= or_SO,'PO'= po_SO,'VE'= ve_SO,'HY'= hy_SO,'KU'= ka_SO,'SO'= ka_SO,
                  'CH'= ka_SO,'CA'= ca_SO,'FE'= de_SO,'DE'= de_SO,'KA'= ka_SO,'RU'= ru_SO,'TE'= te_SO,)
  tag_so = tag_select(dat,so_idx)
  #print(tag_so)
  #tag_so = 'SUBORD_ASC_CODE_AL'
  
  gg_idx = GG_na
  gg_idx = switch(substr(tag_ord,9,10),
                  'AN'= GG_na,
                  'OR'= or_GG,
                  'PO'= switch(substr(tag_so,17,18),'AL'= po_al,'EJ'=po_ej, 'JD'=po_OT,'JE'=po_OT,'AM'=po_OT),
                  'VE'= ve_GG,
                  'HY'= switch(substr(tag_so,17,18),'IU'= hy_iu,'IV'=hy_iu, 'CW'=hy_iu,'EW'=hy_ew,'BT'=hy_bt,
                               'CS'=hy_cs,'EG'=hy_bt,'ED'=hy_bt,'DT'=hy_bt),
                  'KU'= ku_GG,
                  'SO'= so_GG,
                  'CH'= ch_GG,
                  'CA'= switch(substr(tag_so,17,18),'EL'= GG_na,'FJ'= GG_na, 'CV'= ca_OT,'DA'= ca_OT,
                               'FB'= ca_OT,'CQ'= ca_OT,'BD'= ca_OT),
                  'FE'= fe_GG,
                  'DE'= ch_GG,
                  'KA'= ka_GG,
                  'RU'= switch(substr(tag_so,17,18),'FJ'= GG_na,'CS'= ru_cs, 'EL'= GG_na,'HG'= GG_na,
                               'AO'= GG_na,'GV'= GG_na,'ER'= GG_na,'HH'=ru_hh,'CY'=ru_cy),
                  'TE'= switch(substr(tag_so,17,18),'BF'= ru_cy,'BE'= te_be, 'IL'= te_il,'IM'=te_im,'AW'= te_aw,
                               'CY'= ru_cy,'GZ'= te_gz,'IN'= te_gz,'IO'=te_gz,'IP'=te_gz,'IQ'=te_gz,'IR'=te_gz)
                  )
  tag_gg = tag_select(dat,gg_idx)
  #print(tag_gg)
  sg_idx = GG_na
  
  sg_idx = switch(substr(tag_ord,9,10),
                  'AN'= GG_na,
                  'OR'= GG_na,
                  'PO'= po_SG,
                  'VE'= ve_SG,
                  'HY'= switch(substr(tag_so,17,18),
                               'IU'= GG_na,
                               'IV'=GG_na, 
                               'CW'=switch(substr(tag_gg,22,23),'EV'= GG_na,'IW'= GG_na, 'EU'= GG_na,'IX'= GG_na,
                                           'IY'= hy_cw_iy,'IZ'= GG_na,'JA'= GG_na,'JB'= hy_cw_iy,'JC'= GG_na,
                                           'CF'= hy_cw_iy,'FW'= GG_na,'FY'= GG_na,'AQ'= GG_na,'FX'= GG_na,'BV'= GG_na),
                               'EW'=GG_na,
                               'BT'=GG_na,
                               'CS'=GG_na,
                               'EG'=hy_eg_GG,
                               'ED'=hy_ed_GG,
                               'DT'=hy_ed_GG),
                  'KU'= ku_SG,
                  'SO'= so_SG,
                  'CH'= ch_SG,
                  'CA'= ca_SG,
                  'FE'= fe_SG,
                  'DE'= de_SG,
                  'KA'= ka_SG,
                  'RU'= switch(substr(tag_so,17,18),'FJ'= ru_fj_GG,'CS'= ru_fj_GG, 'EL'= GG_na,'HG'= GG_na,
                               'AO'= ru_fj_GG,'GV'= ru_fj_GG,'ER'= ru_fj_GG,'HH'=ru_fj_GG,'CY'=ru_fj_GG),
                  'TE'= switch(substr(tag_so,17,18),'BF'= te_bf_GG,'BE'= te_be_GG, 'IL'= te_il_GG,'IM'=te_im_GG,'AW'= te_aw_GG,
                               'CY'= te_cy_GG,'GZ'= te_gz_GG,'IN'= te_in_GG,'IO'=te_in_GG,'IP'=te_in_GG,'IQ'=te_in_GG,'IR'=te_in_GG))
  tag_sg = tag_select(dat,sg_idx)
  
  return(c(tag_ord,tag_so,tag_gg,tag_sg))
}
# dat: a set of data for test, format same as the tr and test file at begining of this file
# idx: no. of row to be test
data_evaluate <- function(dat,idx) {
  #j =0
  pre_result = tag_predict(dat[idx,])
  pre_str = paste(pre_result[1],pre_result[2],pre_result[3],pre_result[4],sep = ' ')
  real_result = 
    c(colnames(dat[,ord_names])[which.max(dat[idx,ord_names])],
      colnames(dat[,so_names])[which.max(dat[idx,so_names])],
      colnames(dat[,gg_names])[which.max(dat[idx,gg_names])],
      colnames(dat[,sg_names])[which.max(dat[idx,sg_names])]
    )
  real_str = paste(real_result[1],real_result[2],real_result[3],real_result[4],sep = ' ')
  record = c(0,0,0,0,0)
  if (sum(pre_result==real_result)==4){
    cat(green(paste(idx, dat[idx,1], dat[idx,2],'\n',
                    "Predict: ",pre_str,'\n',
                    "   Real: ",real_str,'\n',
                    'Great work!!! 100% correct\n')))
    record[[1]] = record[[1]]+1
  } 
  if (sum(pre_result==real_result)==3){
    cat(blue(paste(idx, dat[idx,1], dat[idx,2],'\n',
                   "Predict: ",pre_str,'\n',
                   "   Real: ",real_str,'\n',
                   'Good work!! 75% correct\n')))
    record[[2]] = record[[2]]+1
  } 
  if(sum(pre_result==real_result)==2) {
    cat(yellow(paste(idx, dat[idx,1], dat[idx,2],'\n',
                     "Predict: ",pre_str,'\n',
                     "   Real: ",real_str,'\n',
                     'Fair work! 50% correct\n')))
    record[[3]] = record[[3]]+1
  } 
  if (sum(pre_result==real_result)==1) {
    cat(magenta(paste(idx, dat[idx,1], dat[idx,2],'\n',
                      "Predict: ",pre_str,'\n',
                      "   Real: ",real_str,'\n',
                      'Poor work!! 25% correct\n')))
    record[[4]] = record[[4]]+1
  } 
  if (sum(pre_result==real_result)==0) {
    cat(red(paste(idx, dat[idx,1], dat[idx,2],'\n',
                  "Predict: ",pre_str,'\n',
                  "   Real: ",real_str,'\n',
                  'Failed!!! 0% correct\n')))
    record[[5]] = record[[5]]+1
  }
  return(record)
}
performance = c(0,0,0,0,0)
for (i in 1: nrow(test_dat)) {
  idx = sample(nrow(test_dat),1,replace = F)
  performance = performance + data_evaluate(test_dat,idx)
  cat(green(paste('100%:',performance[[1]],'/',sum(performance),',   ')),
      blue(paste('75%:',performance[[2]],'/',sum(performance),',   ')),
      yellow(paste('50%:',performance[[3]],'/',sum(performance),',   ')),
      magenta(paste('25%:',performance[[4]],'/',sum(performance),',   ')),
      red(paste('0%:',performance[[5]],'/',sum(performance))),'\n'
  )
}
