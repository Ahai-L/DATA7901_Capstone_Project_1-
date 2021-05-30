library(abind)
library(readr)
library(dplyr)
library(stringr)
ascspc_3d = readRDS('./0_general/asc_spc.rds')
# ascspc_3d = ascspc_3d[1:51,,]
spc_cluster = read_rds('./7_spc_clustering/cluster_df_onehot.rds')
ascspc_3d = left_join(ascspc_3d,spc_cluster,by= c('SPC'='spc'))
ascspc_3d$CLUSTER = as.character(ascspc_3d$cluster)
ascspc_3d= ascspc_3d[,!colnames(ascspc_3d) =='SPC']
ascspc_3d= ascspc_3d[,!colnames(ascspc_3d) =='cluster']
#ascspc_3d[is.na(ascspc_3d)]=0 do this later, after one hot encoding
ascspc_3d[ascspc_3d=='-']=NA
# check "sn" attributes############################################
ascspc_3d$DRAINAGE = as.integer(ascspc_3d$DRAINAGE)
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
ascspc_3d$SOIL_WATER_STAT = ex_ordinal(ascspc_3d$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
ascspc_3d$BOUND_DISTINCT = ex_ordinal(ascspc_3d$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
ascspc_3d$PEDALITY_GRADE = ex_ordinal(ascspc_3d$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))
ascspc_3d$REL_MOD_SLOPE_CLASS = ex_ordinal(ascspc_3d$REL_MOD_SLOPE_CLASS,
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
# standardize the column value if sn mn###############
nor <- function(x,c) {
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) %in% c( "double","integer") & !( colnames(x[i]) %in% c)) { # sn and mn
      x[[i]][is.na(x[[i]])]=0
      x[[i]]=(x[[i]]-min(x[[i]]))/(max(x[[i]])-min(x[[i]]))
    }
  }
  return(x)
}
ascspc_3d= nor(ascspc_3d,c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))# make column value to be [0,1]


# categorical values encoding###########################################################
ascspc_3d$TEXTURE_CODE[ascspc_3d$TEXTURE_CODE %in% 
                         c('S','LS','CS','SL','SCL','CFS','CKS','FS','FSCL',
                           'FSCLZ','FSL','FSLZ','KS','FSCL','KSL','KSS','LFS',
                           'LFSY','LFSYZ','LKS','LMS','LSY','MS','SCFLS','SLZ','SS','ST',
                           'KSCL','SCLFS')] = 'Sandy'
ascspc_3d$TEXTURE_CODE[ascspc_3d$TEXTURE_CODE %in% 
                         c('L','ZL','ZCL','CLFS','CL','CLS','CLFSZ','CLKS','CLMS','CLZ','ZCL')] = 'Loamy'
ascspc_3d$TEXTURE_CODE[ascspc_3d$TEXTURE_CODE %in% 
                         c('C','LC','LMC','MC','MHC','HC','CSC','FSHC','FSLC','FSLCZ','FSLMC',
                           'FSMC','FSMHC','KSHC','KSLMC','KSMC','KSMHC','LCFS',
                           'LCKS','LCS','LMCFS','LMCKS','LMCS','LMCZ','LMCKS',
                           'LMCS','MCFS','MCS','MCZ','MHCFS','MHCS','MSC','SC','SHC',
                           'SLC','SLMC','SMC','SMHC','ZC','ZHC','ZLC','ZLCFS','ZLMC','ZLMCS',
                           'ZMC','ZMHC','LCZ','FSC','KSC','KSLC')] = 'Clayey'
ascspc_3d$TEXTURE_CODE[ascspc_3d$TEXTURE_CODE %in% 
                         c('AP','CP','GP','GR','HP','IP','LP','SP')] = 'Organic'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('QS','S')] = 'Siliceous Upper'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('GS','MS','MU','PL','PH','PO','ST',
                            'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('AC','FC','FS','IS')] = 'Sesquioxide'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('CO','CC')] = 'Organic'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('AH','GY','HA')] = 'Evaporite'
ascspc_3d$OBS_LITH_CODE[ascspc_3d$OBS_LITH_CODE %in% 
                          c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# $OCF_LITH_CODE
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('QS','S')] = 'Siliceous Upper'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('GS','MS','MU','PL','PH','PO','ST',
                            'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('AC','FC','FS','IS')] = 'Sesquioxide'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('CO','CC')] = 'Organic'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('AH','GY','HA')] = 'Evaporite'
ascspc_3d$OCF_LITH_CODE[ascspc_3d$OCF_LITH_CODE %in% 
                          c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# VEG
ascspc_3d$VEG_SPEC_CODE = str_extract(ascspc_3d$VEG_SPEC_CODE, "^.{3}")
# categorical to numeric factor#####################################
cat_to_numFac <- function(x,c) {
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) == "character" & !( colnames(x[i]) %in% c)) { # sc and mc
      x[[i]][is.na(x[[i]])] = "0"
      x[[i]] <- factor(x[[i]])
      levels(x[[i]]) <- 0: (length(levels(x[[i]]))-1)
      x[[i]] = as.numeric(as.character(x[[i]]))
    }
  }
  return(x)
}
ascspc_3d <- cat_to_numFac(ascspc_3d,c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO",
                                       'ASC_ORD','SUBORD_ASC_CODE','GREAT_GROUP_ASC_CODE',
                                       'SUBGROUP_ASC_CODE','CLUSTER'))
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
ascspc_3d <- cnn_onehot_exZero(ascspc_3d,c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))

# NA to 0
ascspc_3d[is.na(ascspc_3d)]=0
# Fix horizons to be k2=8#############################
k2 = 8
ascspc_3d = ascspc_3d[ascspc_3d$HORIZON_NO<=k2,] 
sn_sc_idx = grep('STATUS|ELEM_TYPE_CODE|DRAINAGE|OBS_LITH_CODE|REL_MOD_SLOP_CLASS|VEG_SPEC_CODE|ASC_ORD|SUBORD_|GREAT_GROUP|SUBGROUP|SPC_|CLUSTER_',
                 colnames(ascspc_3d))
# split data into a big list group by project code and site id################
ascspc_3d <- split(ascspc_3d,list(ascspc_3d$PROJECT_CODE,ascspc_3d$SITE_ID),drop = T)
#write_rds(ascspc_3d,'./2_asc_cnn/ascspc_3d_aftSplit.rds')
# add  empty rows for those "HORIZON_NO" < K2(5)##############
for ( i in 1: length(ascspc_3d)) {
  n2 = nrow(ascspc_3d[[i]])
  if (n2<k2) {
    ascspc_3d[[i]][(n2+1):k2,c(1:2,sn_sc_idx)] = ascspc_3d[[i]][1,c(1:2,sn_sc_idx)]
    ascspc_3d[[i]][(n2+1):k2,'HORIZON_NO'] = (n2+1):k2
    ascspc_3d[[i]][(n2+1):k2,-c(1:3,sn_sc_idx)] = 0
  }
  print(i)
}

# it is time to drop 4 columns ############
ascspc_3d <- lapply(ascspc_3d, function(x) x[!(names(x) %in% c("PROJECT_CODE", "SITE_ID","OBS_NO","HORIZON_NO"))])
write_rds(ascspc_3d,'./2_asc_cnn/ascspc_3d_aftSplit.rds')

# convert to a 3D array: [data index, horizons, features]##############################
ascspc_3d = readRDS('./2_asc_cnn/ascspc_3d_aftSplit.rds')
ascspc_3d <- abind(ascspc_3d,along = 0)
#rename dim(2)
dimnames(ascspc_3d)[[2]] <- 1:8
# rearrange ASC_ORD
#ascspc_3d=ascspc_3d[,,c(1:376,389,388,387,377,381,386,382,385,390,378:380,384,383,391:dim(ascspc_3d)[3])]
#asc_tag_list = grep('ASC_ORD_',dimnames(ascspc_3d)[[3]])
write_rds(ascspc_3d,'./2_asc_cnn/ascspc_3d_3darray.rds')
# sampling###################################################
ascspc_3d=read_rds('./2_asc_cnn/ascspc_3d_3darray.rds')
library(stringr)
library(keras)
#library(tensorflow)
#install_keras(tensorflow = 'gpu')
# without scaling
#shuffing 
ascspc_3d = ascspc_3d[sample(nrow(ascspc_3d),nrow(ascspc_3d)),,]
# option one: devide tr and test set without prune and scaling########
tr_rate=0.85
idx = sample(nrow(ascspc_3d),round(nrow(ascspc_3d)*tr_rate),replace = F)
ascspc_3d_tr= ascspc_3d[idx,,]
ascspc_3d_test= ascspc_3d[-idx,,]

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
ascspc_3d1 = cnn_prune(ascspc_3d,"ASC_ORD")
ascspc_3d1 = cnn_prune(ascspc_3d1,"SUBORD|GREAT_GROUP|SUBGROUP",50)
ascspc_3d1 = cnn_prune(ascspc_3d1,"CLUSTER_",50)
write_rds(ascspc_3d1,'./2_asc_cnn/ascspc_3d1_prune.rds')
# only prune but not scaling
ascspc_3d1=readRDS('./2_asc_cnn/ascspc_3d1_prune.rds')
ascspc_3d1 = ascspc_3d1[sample(nrow(ascspc_3d1),nrow(ascspc_3d1)),,]
tr_rate=0.85
idx = sample(nrow(ascspc_3d1),round(nrow(ascspc_3d1)*tr_rate),replace = F)
ascspc_3d_tr= as.array(ascspc_3d1[idx,,])
ascspc_3d_test= as.array(ascspc_3d1[-idx,,])

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
asc_sample = sample_1(ascspc_3d1,'ASC_ORD|SUBORD|GREAT_GROUP|SUBGROUP')
ascspc_3d_tr = asc_sample[[1]]
ascspc_3d_test = asc_sample[[2]]
library(readr)
write_rds(asc_sample,'./2_asc_cnn/asc_sample.rds')
write_rds(ascspc_3d_tr,'./2_asc_cnn/ascspc_3d_tr.rds')
write_rds(ascspc_3d_test,'./2_asc_cnn/ascspc_3d_test.rds')

#extract different input and tag list#########################
#ascspc_3d_test=readRDS('./2_asc_cnn/ascspc_3d_test.rds')
#ascspc_3d_tr=readRDS('./2_asc_cnn/ascspc_3d_tr.rds')

dimnames(ascspc_3d_tr)[[3]] = gsub('SPC_HOR','SPCHOR', dimnames(ascspc_3d_tr)[[3]]) 
dimnames(ascspc_3d_test)[[3]] = gsub('SPC_HOR','SPCHOR', dimnames(ascspc_3d_test)[[3]]) 

# base_num_list = grep('FTS_PH|PEDALITY_GRADE|UPPER_DEPTH|LOWER_DEPTH|BOUND_DISTINCT|HOR_PREFIX',
#                  dimnames(ascspc_3d_tr)[[3]])
# asc_num_list = grep('DRAINAGE|SOIL_WATER_STAT|VALUE_13C1_Fe|VALUE_15N1|VALUE_6B_6A|VALUE_4A1|VALUE_2Z2_Clay'
#                    ,dimnames(ascspc_3d_tr)[[3]])
# spc_num_list = grep('REL_MOD_SLOPE_CLASS',dimnames(ascspc_3d_tr)[[3]])

asc_tag_list = grep('ASC_ORD_',dimnames(ascspc_3d_tr)[[3]])
so_tag_list = grep('SUBORD_ASC_CODE',dimnames(ascspc_3d_tr)[[3]])
gg_tag_list = grep('GREAT_GROUP_ASC_CODE',dimnames(ascspc_3d_tr)[[3]])
sg_tag_list = grep('SUBGROUP_ASC_CODE',dimnames(ascspc_3d_tr)[[3]])
spc_tag_list = grep('CLUSTER_',dimnames(ascspc_3d_tr)[[3]])
# generate 4d(input #, horizons, features, channels) data
library(keras)
library(abind)
k2=8
# base_num_tr <- asub(ascspc_3d_tr, base_num_list,3)
# asc_num_tr <- asub(ascspc_3d_tr, asc_num_list,3)
# spc_num_tr <- array_reshape(asub(ascspc_3d_tr, spc_num_list,3),
#                             c(nrow(ascspc_3d_tr), k2, length(spc_num_list), 1))
# j = 0;list_tr_in=list();
# for (i in dimnames(ascspc_3d_tr)[[3]][1:29]) {
#   j = j+1;
#   list_tr_in[[j]] = assign(paste(i,"TR",sep = '_'),asub(ascspc_3d_tr,i,3))
#   assign(paste(i,"TEST",sep = '_'),asub(ascspc_3d_test,i,3))
# }

for (i in dimnames(ascspc_3d_tr)[[3]][1:29]) {
  assign(paste(i,"TR",sep = '_'),asub(ascspc_3d_tr,i,3))
  assign(paste(i,"TEST",sep = '_'),asub(ascspc_3d_test,i,3))
} 


asc_tag_tr = asub(ascspc_3d_tr,list(1,asc_tag_list),c(2,3))
spc_tag_tr = asub(ascspc_3d_tr,list(1,spc_tag_list),c(2,3))
so_tag_tr = asub(ascspc_3d_tr,list(1,so_tag_list),c(2,3))
gg_tag_tr = asub(ascspc_3d_tr,list(1,gg_tag_list),c(2,3))
sg_tag_tr = asub(ascspc_3d_tr,list(1,sg_tag_list),c(2,3))

asc_tag_test = asub(ascspc_3d_test,list(1,asc_tag_list),c(2,3))
spc_tag_test = asub(ascspc_3d_test,list(1,spc_tag_list),c(2,3))
so_tag_test = asub(ascspc_3d_test,list(1,so_tag_list),c(2,3))
gg_tag_test = asub(ascspc_3d_test,list(1,gg_tag_list),c(2,3))
sg_tag_test = asub(ascspc_3d_test,list(1,sg_tag_list),c(2,3))
# model construction ###############################
library(keras)
# test = list();j = 0
# for (i in dimnames(ascspc_3d_tr)[[3]][1:29]) {
#   j=j+1
#   test[[j]]= layer_input(shape = c(8),name = paste(i,'IN',sep = '_'))
# }
for (i in dimnames(ascspc_3d_tr)[[3]][1:29]) {
  assign(paste(i,'3D',sep = '_'), assign(paste(i,'EM',sep='_'), assign(paste(i, 'IN', sep = "_"),
                                                                       layer_input(shape = c(8),name = paste(i,'IN',sep = '_'))) %>% 
                                           layer_embedding(input_dim = 200,output_dim = 50,name = paste(i,'EM',sep='_'))) %>% 
           layer_reshape(c(8,50,1),name = paste(i,'3D',sep='_')))
}
ASC_TAG = layer_input(shape=c(length(asc_tag_list)),name='ASC_TAG')
SPC_TAG = layer_input(shape=c(length(spc_tag_list)),name='SPC_TAG')
SO_TAG = layer_input(shape=c(length(so_tag_list)),name='SO_TAG')
GG_TAG = layer_input(shape=c(length(gg_tag_list)),name='GG_TAG')
#SG_TAG = layer_input(shape=c(length(sg_tag_list)),name='SG_TAG')


BASE_CON <- layer_concatenate(list(STATUS_3D,FTS_PH_3D,PEDALITY_GRADE_3D,UPPER_DEPTH_3D,
                                   LOWER_DEPTH_3D,BOUND_DISTINCT_3D,TEXTURE_CODE_3D,
                                   HOR_PREFIX_3D,HOR_SUBHOR_3D,HOR_SUFFIX_3D,CUTAN_TYPE_3D,
                                   NATURE_3D),axis = 3,name = 'BASE_CON')
BASE_CON
ASC_EX_CON <-layer_concatenate(list(DRAINAGE_3D,SOIL_WATER_STAT_3D,VALUE_13C1_Fe_3D,
                                    VALUE_15N1_3D,VALUE_4A1_3D,VALUE_6B_6A_3D,VALUE_2Z2_Clay_3D,
                                    HOR_MASTER_3D,ELEM_TYPE_CODE_3D,PEDALITY_TYPE_3D),
                               axis = 3,name = 'ASC_EX_CON')
ASC_EX_CON
# SPC_EX_CON <-layer_concatenate(list(OBS_LITH_CODE_3D,REL_MOD_SLOPE_CLASS_3D,VEG_SPEC_CODE_3D,
#                                     SPCHOR_MASTER_3D,MOTT_TYPE_3D,OCF_LITH_CODE_3D,COLOUR_CLASS_3D),
#                                axis = 3,name = 'SPC_EX_CON')
# SPC_EX_CON
ASC_CON <- layer_concatenate(list(BASE_CON,ASC_EX_CON),axis = 3,name = 'ASC_CON') 
ASC_OUT <- ASC_CON %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(1, 2),data_format='channels_first') %>%
  layer_flatten %>% list(.,SPC_TAG)%>%
  layer_concatenate(axis = 1) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = length(asc_tag_list), activation = "softmax",name = 'ASC_OUTPUT')
ASC_OUT

# SPC_OUT <- layer_concatenate(list(BASE_CON,SPC_EX_CON),axis = 3,name = 'SPC_CON') %>%
#   layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
#                 padding = 'same',data_format='channels_first') %>%
#   layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
#   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
#                 padding = 'same',data_format='channels_first') %>%
#   layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
#   layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
#                 padding = 'same',data_format='channels_first') %>%
#   layer_max_pooling_2d(pool_size = c(1, 2),data_format='channels_first') %>%
#   layer_flatten %>% list(.,ASC_TAG)%>%
#   layer_concatenate(axis = 1) %>%
#   layer_dropout(rate = 0.5) %>%
#   layer_dense(units =128, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
#   layer_dense(units = length(spc_tag_list), activation = "softmax",name = 'SPC_OUTPUT')
# SPC_OUT
# 
#so_con <- layer_concatenate(list(so_ex_in),axis=2)
SO_CON  <- layer_concatenate(list(MOTT_TYPE_3D,COLOUR_CLASS_3D,ASC_CON),axis=3,name = 'SO_CON')
SO_OUT <- SO_CON %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(1, 2),data_format='channels_first') %>%
  layer_flatten %>% list(.,ASC_TAG)%>%
  layer_concatenate(axis = 1) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = length(so_tag_list), activation = "softmax",name = 'SO_OUTPUT')
SO_OUT

#GG_CON  <- layer_concatenate(list(MOTT_TYPE_3D,COLOUR_CLASS_3D,ASC_CON),axis=3,name = 'SO_CON')
GG_OUT <- SO_CON %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(1, 2),data_format='channels_first') %>%
  layer_flatten %>% list(.,ASC_TAG,SO_TAG)%>%
  layer_concatenate(axis = 1) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = length(gg_tag_list), activation = "softmax",name = 'GG_OUTPUT')
GG_OUT

SG_OUT <- SO_CON %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(2, 2),data_format='channels_first') %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same',data_format='channels_first') %>%
  layer_max_pooling_2d(pool_size = c(1, 2),data_format='channels_first') %>%
  layer_flatten %>% list(.,ASC_TAG,SO_TAG,GG_TAG)%>%
  layer_concatenate(axis = 1) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = length(sg_tag_list), activation = "softmax",name = 'SG_OUTPUT')
SG_OUT
input_list_tr = list(STATUS_IN,FTS_PH_IN,PEDALITY_GRADE_IN,UPPER_DEPTH_IN,
                     LOWER_DEPTH_IN,BOUND_DISTINCT_IN,TEXTURE_CODE_IN,
                     HOR_PREFIX_IN,HOR_SUBHOR_IN,HOR_SUFFIX_IN,CUTAN_TYPE_IN,
                     NATURE_IN,DRAINAGE_IN,SOIL_WATER_STAT_IN,VALUE_13C1_Fe_IN,
                     VALUE_15N1_IN,VALUE_4A1_IN,VALUE_6B_6A_IN,VALUE_2Z2_Clay_IN,
                     HOR_MASTER_IN,ELEM_TYPE_CODE_IN,PEDALITY_TYPE_IN,MOTT_TYPE_IN,
                     COLOUR_CLASS_IN,ASC_TAG,SO_TAG,GG_TAG,SPC_TAG)
output_tag_tr = list(ASC_OUT,SO_OUT,GG_OUT,SG_OUT)

model <- keras_model(input_list_tr,output_tag_tr)
model
model %>% compile(
  optimizer = "rmsprop",
  loss = c("categorical_crossentropy","categorical_crossentropy",
           "categorical_crossentropy","categorical_crossentropy","categorical_crossentropy"),
  metrics = "acc"
)
model
model %>% save_model_hdf5('./2_asc_cnn/modelascspc_3d.h5')
# tensorboard
# dir.create("./2_asc_cnn/tensorlog")
# tensorboard ("tensorlog")
# callbacks = list(callback_tensorboard(
#   log_dir = "./2_asc_cnn/tensorlog"))
#   #histogram_freq = 1,
#   #embeddings_freq = 1))

# model fitting and predicting, confusion matrix###############
list_tr_data = list(STATUS_TR,FTS_PH_TR,PEDALITY_GRADE_TR,UPPER_DEPTH_TR,
                    LOWER_DEPTH_TR,BOUND_DISTINCT_TR,TEXTURE_CODE_TR,
                    HOR_PREFIX_TR,HOR_SUBHOR_TR,HOR_SUFFIX_TR,CUTAN_TYPE_TR,
                    NATURE_TR,DRAINAGE_TR,SOIL_WATER_STAT_TR,VALUE_13C1_Fe_TR,
                    VALUE_15N1_TR,VALUE_4A1_TR,VALUE_6B_6A_TR,VALUE_2Z2_Clay_TR,
                    HOR_MASTER_TR,ELEM_TYPE_CODE_TR,PEDALITY_TYPE_TR,MOTT_TYPE_TR,
                    COLOUR_CLASS_TR,asc_tag_tr,so_tag_tr,gg_tag_tr,spc_tag_tr)
list_tr_tag_data = list(asc_tag_tr,so_tag_tr,gg_tag_tr,sg_tag_tr)
history = model %>% fit(list_tr_data,list_tr_tag_data,
                        epochs = 40, batch_size =128,validation_split =0.15)#,callbacks=callbacks)

# fitting with best epcho, remember to reset the model
history = model %>% fit(list(base_in_tr,asc_ex_tr,so_ex_tr),
                        list(asc_tag_tr,so_tag_tr,gg_tag_tr,sg_tag_tr),
                        epochs = 5, batch_size =64)

list_test_data = list(STATUS_TEST,FTS_PH_TEST,PEDALITY_GRADE_TEST,UPPER_DEPTH_TEST,
                      LOWER_DEPTH_TEST,BOUND_DISTINCT_TEST,TEXTURE_CODE_TEST,
                      HOR_PREFIX_TEST,HOR_SUBHOR_TEST,HOR_SUFFIX_TEST,CUTAN_TYPE_TEST,
                      NATURE_TEST,DRAINAGE_TEST,SOIL_WATER_STAT_TEST,VALUE_13C1_Fe_TEST,
                      VALUE_15N1_TEST,VALUE_4A1_TEST,VALUE_6B_6A_TEST,VALUE_2Z2_Clay_TEST,
                      HOR_MASTER_TEST,MOTT_TYPE_TEST,COLOUR_CLASS_TEST,asc_tag_test,
                      so_tag_test,gg_tag_test,spc_tag_test)
list_test_tag_data = list(asc_tag_test,so_tag_test,gg_tag_test,sg_tag_test)

result = model %>% evaluate(list_test_data,list_test_tag_data)
cnn_predict <- model %>% predict(list_test_data)

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
ord_confusion$overall
so_confusion$overall
gg_confusion$overall
sg_confusion$overall
