library(abind)
library(readr)
asc_spc = readRDS('./0_general/asc_spc.rds')
spc_cluster = read_rds('./7_spc_clustering/cluster_df_onehot.rds')
asc_spc = left_join(asc_spc,spc_cluster,by= c('SPC'='spc'))
asc_spc$CLUSTER = as.character(asc_spc$cluster)
asc_spc= asc_spc[,!colnames(asc_spc) =='SPC']
asc_spc= asc_spc[,!colnames(asc_spc) =='cluster']
#asc_spc[is.na(asc_spc)]=0 do this later, after one hot encoding
asc_spc[asc_spc=='-']=NA
# check "sn" attributes############################################
asc_spc$DRAINAGE = as.integer(asc_spc$DRAINAGE)

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
asc_spc$SOIL_WATER_STAT = ex_ordinal(asc_spc$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
asc_spc$BOUND_DISTINCT = ex_ordinal(asc_spc$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
asc_spc$PEDALITY_GRADE = ex_ordinal(asc_spc$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))
asc_spc$REL_MOD_SLOPE_CLASS = ex_ordinal(asc_spc$REL_MOD_SLOPE_CLASS,
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
# categorical values encoding###########################################################
asc_spc$TEXTURE_CODE[asc_spc$TEXTURE_CODE %in% 
                       c('S','LS','CS','SL','SCL','CFS','CKS','FS','FSCL',
                         'FSCLZ','FSL','FSLZ','KS','FSCL','KSL','KSS','LFS',
                         'LFSY','LFSYZ','LKS','LMS','LSY','MS','SCFLS','SLZ','SS','ST',
                         'KSCL','SCLFS')] = 'Sandy'
asc_spc$TEXTURE_CODE[asc_spc$TEXTURE_CODE %in% 
                       c('L','ZL','ZCL','CLFS','CL','CLS','CLFSZ','CLKS','CLMS','CLZ','ZCL')] = 'Loamy'
asc_spc$TEXTURE_CODE[asc_spc$TEXTURE_CODE %in% 
                       c('C','LC','LMC','MC','MHC','HC','CSC','FSHC','FSLC','FSLCZ','FSLMC',
                         'FSMC','FSMHC','KSHC','KSLMC','KSMC','KSMHC','LCFS',
                         'LCKS','LCS','LMCFS','LMCKS','LMCS','LMCZ','LMCKS',
                         'LMCS','MCFS','MCS','MCZ','MHCFS','MHCS','MSC','SC','SHC',
                         'SLC','SLMC','SMC','SMHC','ZC','ZHC','ZLC','ZLCFS','ZLMC','ZLMCS',
                         'ZMC','ZMHC','LCZ','FSC','KSC','KSLC')] = 'Clayey'
asc_spc$TEXTURE_CODE[asc_spc$TEXTURE_CODE %in% 
                       c('AP','CP','GP','GR','HP','IP','LP','SP')] = 'Organic'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('QS','S')] = 'Siliceous Upper'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('GS','MS','MU','PL','PH','PO','ST',
                          'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('AC','FC','FS','IS')] = 'Sesquioxide'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('CO','CC')] = 'Organic'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('AH','GY','HA')] = 'Evaporite'
asc_spc$OBS_LITH_CODE[asc_spc$OBS_LITH_CODE %in% 
                        c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# $OCF_LITH_CODE
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('QS','S')] = 'Siliceous Upper'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('GS','MS','MU','PL','PH','PO','ST',
                          'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('AC','FC','FS','IS')] = 'Sesquioxide'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('CO','CC')] = 'Organic'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('AH','GY','HA')] = 'Evaporite'
asc_spc$OCF_LITH_CODE[asc_spc$OCF_LITH_CODE %in% 
                        c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# VEG
asc_spc$VEG_SPEC_CODE = str_extract(asc_spc$VEG_SPEC_CODE, "^.{3}")


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
asc_spc = cnn_onehot_exZero(asc_spc, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))
# NA to 0
asc_spc[is.na(asc_spc)]=0
# Fix horizons to be k2=8#############################
k2 = 8
asc_spc = asc_spc[asc_spc$HORIZON_NO<=k2,] # drop all rows with "HORIZON_NO" > K2(5)
# # a function to standardize the column value
# nor <- function(x) { 
#   return((x-min(x))/(max(x)-min(x)))
# }
# asc_spc[,-c(1:3)]= apply(asc_spc[,-c(1:3)],2,nor) # make column value to be [0,1]
#
#split data into a big list group by project code and site id
asc_spc <- split(asc_spc,list(asc_spc$PROJECT_CODE,asc_spc$SITE_ID),drop = T)
#write_rds(asc_spc,'./2_asc_cnn/asc_spc_aftSplit.rds')
# add  empty rows for those "HORIZON_NO" < K2(5)
for ( i in 1: length(asc_spc)) {
  n2 = nrow(asc_spc[[i]])
  if (n2<k2) {
    asc_spc[[i]][(n2+1):k2,1:2] = asc_spc[[i]][1,1:2]
    asc_spc[[i]][(n2+1):k2,'HORIZON_NO'] = (n2+1):k2
    asc_spc[[i]][(n2+1):k2,-c(1:3)] = 0 
  }
  print(i)
}
# it is time to drop 4 columns 
asc_spc <- lapply(asc_spc, function(x) x[!(names(x) %in% c("PROJECT_CODE", "SITE_ID","OBS_NO","HORIZON_NO"))])
write_rds(asc_spc,'./2_asc_cnn/asc_spc_aftSplit.rds')
# convert to a 3D array: [data index, horizons, features]##############################
asc_spc = readRDS('./2_asc_cnn/asc_spc_aftSplit.rds')
asc_spc <- abind(asc_spc,along = 0)
#rename dim(2)
dimnames(asc_spc)[[2]] <- 1:8
# rearrange ASC_ORD
#asc_spc=asc_spc[,,c(1:376,389,388,387,377,381,386,382,385,390,378:380,384,383,391:dim(asc_spc)[3])]
#asc_tag_list = grep('ASC_ORD_',dimnames(asc_spc)[[3]])
write_rds(asc_spc,'./2_asc_cnn/asc_spc_3darray.rds')
# sampling###################################################
asc_spc=read_rds('./2_asc_cnn/asc_spc_3darray.rds')
library(stringr)
library(keras)
#library(tensorflow)
#install_keras(tensorflow = 'gpu')
# without scaling
#shuffing 
asc_spc = asc_spc[sample(nrow(asc_spc),nrow(asc_spc)),,]
# option one: devide tr and test set without prune and scaling########
tr_rate=0.85
idx = sample(nrow(asc_spc),round(nrow(asc_spc)*tr_rate),replace = F)
asc_spc_tr= asc_spc[idx,,]
asc_spc_test= asc_spc[-idx,,]

# scaling date for a specific class is less than s2, scale up to "s2".
# and delete the classes that are less than s1 data
#input: x: the data; class: list of a class; s1,s2: threshold of the size
#output: the scaled data of x

#option two: prune only###########################
cnn_prune <- function(x,features,s=500) {
  class = grep(features,dimnames(x)[[3]])
  l=c()
  for (i in class) {
    count =  nrow(x[x[,1,i]==1,,])
    if (count < s) {
      l=c(l,i)
      x = x[x[,1,i]==0,,]
    } 
  }
  return(x[,,-l])
}
asc_spc1 = cnn_prune(asc_spc,"ASC_ORD")
asc_spc1 = cnn_prune(asc_spc1,"SUBORD|GREAT_GROUP|SUBGROUP",50)
asc_spc1 = cnn_prune(asc_spc1,"SPC_",50)
write_rds(asc_spc1,'./2_asc_cnn/asc_spc1_prune.rds')
# only prune but not scaling
asc_spc1 = asc_spc1[sample(nrow(asc_spc1),nrow(asc_spc1)),,]
tr_rate=0.85
idx = sample(nrow(asc_spc1),round(nrow(asc_spc1)*tr_rate),replace = F)
asc_spc_tr= asc_spc1[idx,,]
asc_spc_test= asc_spc1[-idx,,]

# option three: prune and sample with scaling#######################
sample_1 <- function(x,features,r=0.7,s=1000) {
  class = grep(features,dimnames(x)[[3]])
  tr = array(numeric(),c(0,dim(x)[[2]],dim(x)[[3]]))
  dimnames(tr)[[3]]=dimnames(x)[[3]]
  dimnames(tr)[[2]]=dimnames(x)[[2]]
  te = array(numeric(),c(0,dim(x)[[2]],dim(x)[[3]]))
  dimnames(te)[[3]]=dimnames(x)[[3]]
  dimnames(te)[[2]]=dimnames(x)[[2]]
  for (i in class) {
    temp =  x[x[,1,i]==1,,]
    idx = sample(nrow(temp),round(nrow(temp)*r))
    te = abind(te,temp[-idx,,][!dimnames(temp[-idx,,])[[1]] %in% dimnames(te)[[1]],,],along = 1)
    
    if ((nrow(temp[idx,,][!dimnames(temp[idx,,])[[1]] %in% dimnames(tr)[[1]],, ])+nrow(tr[tr[,1,i]==1,,])) >= s) {
      tr = abind(tr, temp[idx,,][!dimnames(temp[idx,,])[[1]] %in% dimnames(tr)[[1]],, ],along = 1)
    } else {
      tr = abind(tr,temp[sample(idx,max(1,round(s*r-nrow(tr[tr[,1,i]==1,,]))),
                                replace = round(s*r-nrow(tr[tr[,1,i]==1,,]))>length(idx)),,],along = 1)
    }
    print(paste(i,dimnames(x)[[3]][i],nrow(tr),sum(tr[,1,dimnames(tr)[[3]][i]]==1),
                nrow(te),sum(te[,1,dimnames(te)[[3]][i]]==1)))
  }
  return(list(tr[sample(nrow(tr),nrow(tr)),,],te[sample(nrow(te),nrow(te)),,]))
} 
asc_sample = sample_1(asc_spc1,'ASC_ORD|SUBORD|GREAT_GROUP|SUBGROUP')
asc_spc_tr = asc_sample[[1]]
asc_spc_test = asc_sample[[2]]
library(readr)
write_rds(asc_sample,'./2_asc_cnn/asc_sample.rds')
write_rds(asc_spc_tr,'./2_asc_cnn/asc_spc_tr.rds')
write_rds(asc_spc_test,'./2_asc_cnn/asc_spc_test.rds')

#extract different input and tag list#########################
#asc_spc_test=readRDS('./2_asc_cnn/asc_spc_test.rds')
#asc_spc_tr=readRDS('./2_asc_cnn/asc_spc_tr.rds')

dimnames(asc_spc_tr)[[3]] = gsub('SPC_HOR','SPCHOR', dimnames(asc_spc_tr)[[3]]) 
dimnames(asc_spc_test)[[3]] = gsub('SPC_HOR','SPCHOR', dimnames(asc_spc_test)[[3]]) 

base_list = grep('STATUS|FTS_PH|PEDALITY_GRADE|UPPER_DEPTH|LOWER_DEPTH|BOUND_DISTINCT|TEXTURE_CODE|HOR_PREFIX|HOR_SUBHOR|HOR_SUFFIX|CUTAN_TYPE|NATURE',
                 dimnames(asc_spc_tr)[[3]])
asc_ex_list = grep('DRAINAGE|PEDALITY_TYPE|ELEM_TYPE_CODE|SOIL_WATER_STAT|VALUE_13C1_Fe|VALUE_15N1|VALUE_6B_6A|VALUE_4A1|VALUE_2Z2_Clay|HOR_MASTER'
                   ,dimnames(asc_spc_tr)[[3]])
spc_ex_list = grep('OBS_LITH_CODE|REL_MOD_SLOP_CLASS|VEG_SPEC_CODE|SPC_HOR_MASTER|MOTT_TYPE|OCF_LITH_CODE|COLOUR_CLASS'
                   ,dimnames(asc_spc_tr)[[3]])
so_ex_list = grep('COLOUR_CLASS|MOTT_TYPE',dimnames(asc_spc_tr)[[3]])
asc_tag_list = grep('ASC_ORD_',dimnames(asc_spc_tr)[[3]])
so_tag_list = grep('SUBORD_ASC_CODE',dimnames(asc_spc_tr)[[3]])
gg_tag_list = grep('GREAT_GROUP_ASC_CODE',dimnames(asc_spc_tr)[[3]])
sg_tag_list = grep('SUBGROUP_ASC_CODE',dimnames(asc_spc_tr)[[3]])
spc_tag_list = grep('SPC_',dimnames(asc_spc_tr)[[3]])
# generate 4d(input #, horizons, features, channels) data
library(keras)
library(abind)
k2=8
base_in_tr <- array_reshape(asub(asc_spc_tr, base_list,3), 
                            c(nrow(asc_spc_tr), k2, length(base_list), 1))
asc_ex_tr <- array_reshape(asub(asc_spc_tr, asc_ex_list,3), 
                           c(nrow(asc_spc_tr), k2, length(asc_ex_list), 1))
spc_ex_tr <- array_reshape(asub(asc_spc_tr, spc_ex_list,3), 
                           c(nrow(asc_spc_tr), k2, length(spc_ex_list), 1))
so_ex_tr <- array_reshape(asub(asc_spc_tr, so_ex_list,3), 
                          c(nrow(asc_spc_tr), k2, length(so_ex_list), 1))
# asc_tin_tr <- array_reshape(asub(asc_spc_tr, asc_tag_list,3), 
#                            c(nrow(asc_spc_tr), k2, length(asc_tag_list), 1))
# so_tin_tr <- array_reshape(asub(asc_spc_tr, so_tag_list,3), 
#                            c(nrow(asc_spc_tr), k2, length(so_tag_list), 1))
# gg_tin_tr <- array_reshape(asub(asc_spc_tr, gg_tag_list,3), 
#                           c(nrow(asc_spc_tr), k2, length(gg_tag_list), 1))

base_in_test <- array_reshape(asub(asc_spc_test, base_list,3), 
                              c(nrow(asc_spc_test), k2, length(base_list), 1))
asc_ex_test <- array_reshape(asub(asc_spc_test, asc_ex_list,3), 
                             c(nrow(asc_spc_test), k2, length(asc_ex_list), 1))
spc_ex_test <- array_reshape(asub(asc_spc_test, spc_ex_list,3), 
                           c(nrow(asc_spc_test), k2, length(spc_ex_list), 1))
so_ex_test <- array_reshape(asub(asc_spc_test, so_ex_list,3), 
                            c(nrow(asc_spc_test), k2, length(so_ex_list), 1))
# asc_tin_test <- array_reshape(asub(asc_spc_test, asc_tag_list,3), 
#                             c(nrow(asc_spc_test), k2, length(asc_tag_list), 1))
# so_tin_test <- array_reshape(asub(asc_spc_test, so_tag_list,3), 
#                            c(nrow(asc_spc_test), k2, length(so_tag_list), 1))
# gg_tin_test <- array_reshape(asub(asc_spc_test, gg_tag_list,3), 
#                            c(nrow(asc_spc_test), k2, length(gg_tag_list), 1))

asc_tag_tr = asub(asc_spc_tr,list(1,asc_tag_list),c(2,3))
spc_tag_tr = asub(asc_spc_tr,list(1,spc_tag_list),c(2,3))
so_tag_tr = asub(asc_spc_tr,list(1,so_tag_list),c(2,3))
gg_tag_tr = asub(asc_spc_tr,list(1,gg_tag_list),c(2,3))
sg_tag_tr = asub(asc_spc_tr,list(1,sg_tag_list),c(2,3))

asc_tag_test = asub(asc_spc_test,list(1,asc_tag_list),c(2,3))
spc_tag_test = asub(asc_spc_test,list(1,spc_tag_list),c(2,3))
so_tag_test = asub(asc_spc_test,list(1,so_tag_list),c(2,3))
gg_tag_test = asub(asc_spc_test,list(1,gg_tag_list),c(2,3))
sg_tag_test = asub(asc_spc_test,list(1,sg_tag_list),c(2,3))
# model construction ###############################
base_in <- layer_input(shape=c(8,length(base_list),1))
asc_ex_in <- layer_input(shape=c(8,length(asc_ex_list),1))
spc_ex_in <- layer_input(shape=c(8,length(spc_ex_list),1))
so_ex_in <- layer_input(shape=c(8,length(so_ex_list),1))

balse_model <- base_in %>%
  layer_conv_2d(filters = 32, kernel_size = c(2,3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(1, 2)) #%>%
# 

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
#spc
spc_con <- layer_concatenate(list(base_in,spc_ex_in),axis=2)
spc_model <- spc_con %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3 ), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(1, 2)) #%>%

spc_model <- spc_ex_in %>%
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
spc_model

spc_flat <- spc_model %>%
  layer_flatten()
spc_flat <-layer_concatenate(list(spc_flat,asc_out),axis = 1) 
#layer_dropout(rate = 0.5) %>%
spc_out <- spc_flat %>%
  layer_dense(units =256, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(spc_tag_list), activation = "softmax")
spc_out

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

model <- keras_model(list(base_in,asc_ex_in,so_ex_in,spc_ex_in),list(asc_out,so_out,gg_out,sg_out,spc_out))
model
model %>% compile(
  optimizer = "rmsprop",
  loss = c("categorical_crossentropy","categorical_crossentropy",
           "categorical_crossentropy","categorical_crossentropy","categorical_crossentropy"),
  metrics = "acc"
)
model
model %>% save_model_hdf5('./2_asc_cnn/modelasc_spc.h5')
# tensorboard
# dir.create("./2_asc_cnn/tensorlog")
# tensorboard ("tensorlog")
# callbacks = list(callback_tensorboard(
#   log_dir = "./2_asc_cnn/tensorlog"))
#   #histogram_freq = 1,
#   #embeddings_freq = 1))

# model fitting and predicting, confusion matrix###############
history = model %>% fit(list(base_in_tr,asc_ex_tr,so_ex_tr,spc_ex_in),
                        list(asc_tag_tr,so_tag_tr,gg_tag_tr,sg_tag_tr,spc_tag_tr),
                        epochs = 20, batch_size =64,validation_split =0.15)#,callbacks=callbacks)

# fitting with best epcho, remember to reset the model
history = model %>% fit(list(base_in_tr,asc_ex_tr,so_ex_tr),
                        list(asc_tag_tr,so_tag_tr,gg_tag_tr,sg_tag_tr),
                        epochs = 5, batch_size =64)


result = model %>% evaluate(list(base_in_test,asc_ex_test,so_ex_test),
                            list(asc_tag_test,so_tag_test,gg_tag_test,sg_tag_test))
cnn_predict <- model %>% predict(list(base_in_test,asc_ex_test,so_ex_test))

colnames(cnn_predict[[1]]) = colnames(asc_tag_test)
rownames(cnn_predict[[1]]) = rownames(asc_tag_test)

ord_predict = as.data.frame(cnn_predict[[1]])
ord_predict = cbind(colnames(ord_predict)[apply(ord_predict,1,which.max)],
                    colnames(asc_tag_test)[apply(asc_tag_test,1,which.max)], 
                    ord_predict)
colnames(ord_predict)[1:2]=c('PREDICT','REAL')
library(caret)
ord_confusion=confusionMatrix(ord_predict[,1],ord_predict[,2])
#ord_confusion[["table"]]=ord_confusion[["table"]][c(1,9,10,14,6,8,12,3,2,5,4,7,11,13),c(1,9,10,14,6,8,12,3,2,5,4,7,11,13)]
#ord_confusion$byClass=ord_confusion$byClass[c(1,9,10,14,6,8,12,3,2,5,4,7,11,13),]
ord_confusion


colnames(cnn_predict[[2]]) = colnames(so_tag_test)
rownames(cnn_predict[[2]]) = rownames(so_tag_test)
subord_predict = as.data.frame(cnn_predict[[2]])
subord_predict = cbind(colnames(subord_predict)[apply(subord_predict,1,which.max)],
                       colnames(so_tag_test)[apply(so_tag_test,1,which.max)], 
                       subord_predict)
colnames(subord_predict)[1:2]=c('PREDICT','REAL')
library(caret)
subord_confusion=confusionMatrix(subord_predict[,1],subord_predict[,2])
#subord_confusion[["table"]]=subord_confusion[["table"]][c(1,9,10,13,6,8,12,3,2,5,4,7,11),c(1,9,10,13,6,8,12,3,2,5,4,7,11)]
#subord_confusion$byClass=subord_confusion$byClass[c(1,9,10,13,6,8,12,3,2,5,4,7,11),]
subord_confusion

colnames(cnn_predict[[3]]) = colnames(gg_tag_test)
rownames(cnn_predict[[3]]) = rownames(gg_tag_test)
gg_predict = as.data.frame(cnn_predict[[3]])
gg_predict = cbind(colnames(gg_predict)[apply(gg_predict,1,which.max)],
                   colnames(gg_tag_test)[apply(gg_tag_test,1,which.max)], 
                   gg_predict)
colnames(gg_predict)[1:2]=c('PREDICT','REAL')
library(caret)
gg_confusion=confusionMatrix(gg_predict[,1],gg_predict[,2])
#gg_confusion[["table"]]=gg_confusion[["table"]][c(1,9,10,13,6,8,12,3,2,5,4,7,11),c(1,9,10,13,6,8,12,3,2,5,4,7,11)]
#gg_confusion$byClass=gg_confusion$byClass[c(1,9,10,13,6,8,12,3,2,5,4,7,11),]
gg_confusion

colnames(cnn_predict[[4]]) = colnames(sg_tag_test)
rownames(cnn_predict[[4]]) = rownames(sg_tag_test)
sg_predict = as.data.frame(cnn_predict[[4]])
sg_predict = cbind(colnames(sg_predict)[apply(sg_predict,1,which.max)],
                   colnames(sg_tag_test)[apply(sg_tag_test,1,which.max)], 
                   sg_predict)
colnames(sg_predict)[1:2]=c('PREDICT','REAL')
library(caret)
sg_confusion=confusionMatrix(sg_predict[,1],sg_predict[,2])
#sg_confusion[["table"]]=sg_confusion[["table"]][c(1,9,10,13,6,8,12,3,2,5,4,7,11),c(1,9,10,13,6,8,12,3,2,5,4,7,11)]
#sg_confusion$byClass=sg_confusion$byClass[c(1,9,10,13,6,8,12,3,2,5,4,7,11),]
sg_confusion

#

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
asc_predict =  as.data.frame(cbind(colnames(ord_predict)[apply(ord_predict,1,which.max)],
                                   colnames(so_predict)[apply(so_predict,1,which.max)],
                                   colnames(gg_predict)[apply(gg_predict,1,which.max)],
                                   colnames(sg_predict)[apply(sg_predict,1,which.max)],
                                   colnames(asc_tag_test)[apply(asc_tag_test,1,which.max)],
                                   colnames(so_tag_test)[apply(so_tag_test,1,which.max)],
                                   colnames(gg_tag_test)[apply(gg_tag_test,1,which.max)],
                                   colnames(sg_tag_test)[apply(sg_tag_test,1,which.max)]
))
#colnames(asc_predict)[1:2]=c('P_ORD','R_ORD')

asc_predict$P_SG = paste(str_sub(asc_predict[[1]],-2,-1),
                         str_sub(asc_predict[[2]],-2,-1),
                         str_sub(asc_predict[[3]],-2,-1),
                         str_sub(asc_predict[[4]],-2,-1),
                         sep='_')
asc_predict$R_SG = paste(str_sub(asc_predict[[5]],-2,-1),
                         str_sub(asc_predict[[6]],-2,-1),
                         str_sub(asc_predict[[7]],-2,-1),
                         str_sub(asc_predict[[8]],-2,-1),
                         sep='_')
asc_predict$P_GG = paste(str_sub(asc_predict[[1]],-2,-1),
                         str_sub(asc_predict[[2]],-2,-1),
                         str_sub(asc_predict[[3]],-2,-1),
                         sep='_')
asc_predict$R_GG = paste(str_sub(asc_predict[[5]],-2,-1),
                         str_sub(asc_predict[[6]],-2,-1),
                         str_sub(asc_predict[[7]],-2,-1),
                         sep='_')
asc_predict$P_SO = paste(str_sub(asc_predict[[1]],-2,-1),
                         str_sub(asc_predict[[2]],-2,-1),
                         sep='_')
asc_predict$R_SO = paste(str_sub(asc_predict[[5]],-2,-1),
                         str_sub(asc_predict[[6]],-2,-1),
                         sep='_')
asc_predict$P_ORD = paste(str_sub(asc_predict[[1]],-2,-1),
                          
                          sep='_')
asc_predict$R_ORD = paste(str_sub(asc_predict[[5]],-2,-1),
                          
                          sep='_')
asc_predict = asc_predict[,9:16]

sg_p = asc_predict[1:2]
sg_p = rbind(sg_p,
             data.frame('P_SG'=rep(NA,
                                   length(sg_p[which(is.na(match(sg_p$P_SG,sg_p$R_SG))),'P_SG'])),
                        'R_SG'=sg_p[which(is.na(match(sg_p$P_SG,sg_p$R_SG))),'P_SG']))
sg_p = rbind(sg_p,
             data.frame('P_SG'=sg_p[which(is.na(match(sg_p$R_SG,sg_p$P_SG))),'R_SG'],
                        'R_SG'=rep(NA,length(sg_p[which(is.na(match(sg_p$R_SG,sg_p$P_SG))),'R_SG']))
             ))
sg_confusion=confusionMatrix(factor(sg_p$P_SG),factor(sg_p$R_SG))

gg_p = asc_predict[3:4]
gg_p = rbind(gg_p,
             data.frame('P_GG'=rep(NA,
                                   length(gg_p[which(is.na(match(gg_p$P_GG,gg_p$R_GG))),'P_GG'])),
                        'R_GG'=gg_p[which(is.na(match(gg_p$P_GG,gg_p$R_GG))),'P_GG']))
gg_p = rbind(gg_p,
             data.frame('P_GG'=gg_p[which(is.na(match(gg_p$R_GG,gg_p$P_GG))),'R_GG'],
                        'R_GG'=rep(NA,length(gg_p[which(is.na(match(gg_p$R_GG,gg_p$P_GG))),'R_GG']))
             ))
gg_confusion=confusionMatrix(factor(gg_p$P_GG),factor(gg_p$R_GG))
gg_confusion$overall
so_p = asc_predict[5:6]
so_p = rbind(so_p,
             data.frame('P_SO'=rep(NA,
                                   length(so_p[which(is.na(match(so_p$P_SO,so_p$R_SO))),'P_SO'])),
                        'R_SO'=so_p[which(is.na(match(so_p$P_SO,so_p$R_SO))),'P_SO']))
so_p = rbind(so_p,
             data.frame('P_SO'=so_p[which(is.na(match(so_p$R_SO,so_p$P_SO))),'R_SO'],
                        'R_SO'=rep(NA,length(so_p[which(is.na(match(so_p$R_SO,so_p$P_SO))),'R_SO']))
             ))
so_confusion=confusionMatrix(factor(so_p$P_SO),factor(so_p$R_SO))
so_confusion
ord_confusion = confusionMatrix(factor(asc_predict$P_ORD),factor(asc_predict$R_ORD))
ord_confusion
