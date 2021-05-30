library(abind)
library(readr)
library(dplyr)
library(stringr)
ascspc_em = readRDS('./0_general/asc_spc.rds')
spc_cluster = read_rds('./7_spc_clustering/cluster_df_onehot.rds')
ascspc_em = left_join(ascspc_em,spc_cluster,by= c('SPC'='spc'))
ascspc_em$CLUSTER = as.character(ascspc_em$cluster)
ascspc_em= ascspc_em[,!colnames(ascspc_em) =='SPC']
ascspc_em= ascspc_em[,!colnames(ascspc_em) =='cluster']
#ascspc_em[is.na(ascspc_em)]=0 do this later, after one hot encoding
ascspc_em[ascspc_em=='-']=NA
# check "sn" attributes############################################
ascspc_em$DRAINAGE = as.integer(ascspc_em$DRAINAGE)
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
ascspc_em$SOIL_WATER_STAT = ex_ordinal(ascspc_em$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
ascspc_em$BOUND_DISTINCT = ex_ordinal(ascspc_em$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
ascspc_em$PEDALITY_GRADE = ex_ordinal(ascspc_em$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))
ascspc_em$REL_MOD_SLOPE_CLASS = ex_ordinal(ascspc_em$REL_MOD_SLOPE_CLASS,
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
ascspc_em= nor(ascspc_em,c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))# make column value to be [0,1]


# categorical values encoding###########################################################
ascspc_em$TEXTURE_CODE[ascspc_em$TEXTURE_CODE %in% 
                       c('S','LS','CS','SL','SCL','CFS','CKS','FS','FSCL',
                         'FSCLZ','FSL','FSLZ','KS','FSCL','KSL','KSS','LFS',
                         'LFSY','LFSYZ','LKS','LMS','LSY','MS','SCFLS','SLZ','SS','ST',
                         'KSCL','SCLFS')] = 'Sandy'
ascspc_em$TEXTURE_CODE[ascspc_em$TEXTURE_CODE %in% 
                       c('L','ZL','ZCL','CLFS','CL','CLS','CLFSZ','CLKS','CLMS','CLZ','ZCL')] = 'Loamy'
ascspc_em$TEXTURE_CODE[ascspc_em$TEXTURE_CODE %in% 
                       c('C','LC','LMC','MC','MHC','HC','CSC','FSHC','FSLC','FSLCZ','FSLMC',
                         'FSMC','FSMHC','KSHC','KSLMC','KSMC','KSMHC','LCFS',
                         'LCKS','LCS','LMCFS','LMCKS','LMCS','LMCZ','LMCKS',
                         'LMCS','MCFS','MCS','MCZ','MHCFS','MHCS','MSC','SC','SHC',
                         'SLC','SLMC','SMC','SMHC','ZC','ZHC','ZLC','ZLCFS','ZLMC','ZLMCS',
                         'ZMC','ZMHC','LCZ','FSC','KSC','KSLC')] = 'Clayey'
ascspc_em$TEXTURE_CODE[ascspc_em$TEXTURE_CODE %in% 
                       c('AP','CP','GP','GR','HP','IP','LP','SP')] = 'Organic'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('QS','S')] = 'Siliceous Upper'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('GS','MS','MU','PL','PH','PO','ST',
                          'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('AC','FC','FS','IS')] = 'Sesquioxide'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('CO','CC')] = 'Organic'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('AH','GY','HA')] = 'Evaporite'
ascspc_em$OBS_LITH_CODE[ascspc_em$OBS_LITH_CODE %in% 
                        c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# $OCF_LITH_CODE
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('QS','S')] = 'Siliceous Upper'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('GS','MS','MU','PL','PH','PO','ST',
                          'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('AC','FC','FS','IS')] = 'Sesquioxide'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('CO','CC')] = 'Organic'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('AH','GY','HA')] = 'Evaporite'
ascspc_em$OCF_LITH_CODE[ascspc_em$OCF_LITH_CODE %in% 
                        c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# VEG
ascspc_em$VEG_SPEC_CODE = str_extract(ascspc_em$VEG_SPEC_CODE, "^.{3}")
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
ascspc_em <- cat_to_numFac(ascspc_em,c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO",
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
ascspc_em <- cnn_onehot_exZero(ascspc_em,c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))

# NA to 0
ascspc_em[is.na(ascspc_em)]=0
# Fix horizons to be k2=8#############################
k2 = 8
ascspc_em = ascspc_em[ascspc_em$HORIZON_NO<=k2,] 
# split data into a big list group by project code and site id################
ascspc_em <- split(ascspc_em,list(ascspc_em$PROJECT_CODE,ascspc_em$SITE_ID),drop = T)
#write_rds(ascspc_em,'./2_asc_cnn/ascspc_em_aftSplit.rds')
# add  empty rows for those "HORIZON_NO" < K2(5)##############
for ( i in 1: length(ascspc_em)) {
  n2 = nrow(ascspc_em[[i]])
  if (n2<k2) {
    ascspc_em[[i]][(n2+1):k2,1:2] = ascspc_em[[i]][1,1:2]
    ascspc_em[[i]][(n2+1):k2,'HORIZON_NO'] = (n2+1):k2
    ascspc_em[[i]][(n2+1):k2,-c(1:3)] = 0 
  }
  print(i)
}

# it is time to drop 4 columns ############
ascspc_em <- lapply(ascspc_em, function(x) x[!(names(x) %in% c("PROJECT_CODE", "SITE_ID","OBS_NO","HORIZON_NO"))])
write_rds(ascspc_em,'./2_asc_cnn/ascspc_em_aftSplit.rds')

# convert to a 3D array: [data index, horizons, features]##############################
ascspc_em = readRDS('./2_asc_cnn/ascspc_em_aftSplit.rds')
ascspc_em <- abind(ascspc_em,along = 0)
#rename dim(2)
dimnames(ascspc_em)[[2]] <- 1:8
# rearrange ASC_ORD
#ascspc_em=ascspc_em[,,c(1:376,389,388,387,377,381,386,382,385,390,378:380,384,383,391:dim(ascspc_em)[3])]
#asc_tag_list = grep('ASC_ORD_',dimnames(ascspc_em)[[3]])
write_rds(ascspc_em,'./2_asc_cnn/ascspc_em_3darray.rds')
# sampling###################################################
ascspc_em=read_rds('./2_asc_cnn/ascspc_em_3darray.rds')
library(stringr)
library(keras)
#library(tensorflow)
#install_keras(tensorflow = 'gpu')
# without scaling
#shuffing 
ascspc_em = ascspc_em[sample(nrow(ascspc_em),nrow(ascspc_em)),,]
# option one: devide tr and test set without prune and scaling########
tr_rate=0.85
idx = sample(nrow(ascspc_em),round(nrow(ascspc_em)*tr_rate),replace = F)
ascspc_em_tr= ascspc_em[idx,,]
ascspc_em_test= ascspc_em[-idx,,]

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
ascspc_em1 = cnn_prune(ascspc_em,"ASC_ORD")
ascspc_em1 = cnn_prune(ascspc_em1,"SUBORD|GREAT_GROUP|SUBGROUP",50)
ascspc_em1 = cnn_prune(ascspc_em1,"CLUSTER_",50)
write_rds(ascspc_em1,'./2_asc_cnn/ascspc_em1_prune.rds')
# only prune but not scaling
ascspc_em1=readRDS('./2_asc_cnn/ascspc_em1_prune.rds')
ascspc_em1 = ascspc_em1[sample(nrow(ascspc_em1),nrow(ascspc_em1)),,]
tr_rate=0.85
idx = sample(nrow(ascspc_em1),round(nrow(ascspc_em1)*tr_rate),replace = F)
ascspc_em_tr= as.array(ascspc_em1[idx,,])
ascspc_em_test= as.array(ascspc_em1[-idx,,])

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
asc_sample = sample_1(ascspc_em1,'ASC_ORD|SUBORD|GREAT_GROUP|SUBGROUP')
ascspc_em_tr = asc_sample[[1]]
ascspc_em_test = asc_sample[[2]]
library(readr)
write_rds(asc_sample,'./2_asc_cnn/asc_sample.rds')
write_rds(ascspc_em_tr,'./2_asc_cnn/ascspc_em_tr.rds')
write_rds(ascspc_em_test,'./2_asc_cnn/ascspc_em_test.rds')

#extract different input and tag list#########################
#ascspc_em_test=readRDS('./2_asc_cnn/ascspc_em_test.rds')
#ascspc_em_tr=readRDS('./2_asc_cnn/ascspc_em_tr.rds')

dimnames(ascspc_em_tr)[[3]] = gsub('SPC_HOR','SPCHOR', dimnames(ascspc_em_tr)[[3]]) 
dimnames(ascspc_em_test)[[3]] = gsub('SPC_HOR','SPCHOR', dimnames(ascspc_em_test)[[3]]) 

base_num_list = grep('FTS_PH|PEDALITY_GRADE|UPPER_DEPTH|LOWER_DEPTH|BOUND_DISTINCT|HOR_PREFIX',
                 dimnames(ascspc_em_tr)[[3]])
asc_num_list = grep('DRAINAGE|SOIL_WATER_STAT|VALUE_13C1_Fe|VALUE_15N1|VALUE_6B_6A|VALUE_4A1|VALUE_2Z2_Clay'
                   ,dimnames(ascspc_em_tr)[[3]])
spc_num_list = grep('REL_MOD_SLOPE_CLASS',dimnames(ascspc_em_tr)[[3]])

asc_tag_list = grep('ASC_ORD_',dimnames(ascspc_em_tr)[[3]])
so_tag_list = grep('SUBORD_ASC_CODE',dimnames(ascspc_em_tr)[[3]])
gg_tag_list = grep('GREAT_GROUP_ASC_CODE',dimnames(ascspc_em_tr)[[3]])
sg_tag_list = grep('SUBGROUP_ASC_CODE',dimnames(ascspc_em_tr)[[3]])
spc_tag_list = grep('CLUSTER_',dimnames(ascspc_em_tr)[[3]])
# generate 4d(input #, horizons, features, channels) data
library(keras)
library(abind)
k2=8
base_num_tr <- asub(ascspc_em_tr, base_num_list,3)
asc_num_tr <- asub(ascspc_em_tr, asc_num_list,3)
spc_num_tr <- array_reshape(asub(ascspc_em_tr, spc_num_list,3),
                            c(nrow(ascspc_em_tr), k2, length(spc_num_list), 1))
status_tr <- asub(ascspc_em_tr, 'STATUS',3)
elem_type_tr <- asub(ascspc_em_tr, 'ELEM_TYPE_CODE',3)
texture_tr <- asub(ascspc_em_tr, 'TEXTURE_CODE',3)
hor_subhor_tr <- asub(ascspc_em_tr, 'HOR_SUBHOR',3)
hor_suffix_tr <- asub(ascspc_em_tr, 'HOR_SUFFIX',3)
cutan_tr <- asub(ascspc_em_tr, 'CUTAN_TYPE',3)
nature_tr <- asub(ascspc_em_tr, 'NATURE',3)
hor_tr <- asub(ascspc_em_tr, 'HOR_MASTER',3)
obs_lith_tr <- asub(ascspc_em_tr, 'OBS_LITH_CODE',3)
veg_tr <- asub(ascspc_em_tr, 'VEG_SPEC_CODE',3)
spchor_tr <- asub(ascspc_em_tr, 'SPCHOR_MASTER',3)
mott_tr <- asub(ascspc_em_tr, 'MOTT_TYPE',3)
ocf_lith_tr <- asub(ascspc_em_tr, 'OCF_LITH_CODE',3)
colour_tr <- asub(ascspc_em_tr, 'COLOUR_CLASS',3)
pedality_type_tr <- asub(ascspc_em_tr, 'PEDALITY_TYPE',3)

# asc_ex_tr <- array_reshape(asub(ascspc_em_tr, asc_ex_list,3), 
#                            c(nrow(ascspc_em_tr), k2, length(asc_ex_list), 1))
# spc_ex_tr <- array_reshape(asub(ascspc_em_tr, spc_ex_list,3), 
#                            c(nrow(ascspc_em_tr), k2, length(spc_ex_list), 1))
# so_ex_tr <- array_reshape(asub(ascspc_em_tr, so_ex_list,3), 
#                           c(nrow(ascspc_em_tr), k2, length(so_ex_list), 1))
#                           c(nrow(ascspc_em_tr), k2, length(gg_tag_list), 1))

base_num_test <- asub(ascspc_em_test, base_num_list,3)
asc_num_test <- asub(ascspc_em_test, asc_num_list,3)
spc_num_test <- array_reshape(asub(ascspc_em_test, spc_num_list,3),
                            c(nrow(ascspc_em_test), k2, length(spc_num_list), 1))
status_test <- asub(ascspc_em_test, 'STATUS',3)
elem_type_test <- asub(ascspc_em_test, 'ELEM_TYPE_CODE',3)
texture_test <- asub(ascspc_em_test, 'TEXTURE_CODE',3)
hor_subhor_test <- asub(ascspc_em_test, 'HOR_SUBHOR',3)
hor_suffix_test <- asub(ascspc_em_test, 'HOR_SUFFIX',3)
cutan_test <- asub(ascspc_em_test, 'CUTAN_TYPE',3)

nature_test <- asub(ascspc_em_test, 'NATURE',3)
hor_test <- asub(ascspc_em_test, 'HOR_MASTER',3)
obs_lith_test <- asub(ascspc_em_test, 'OBS_LITH_CODE',3)
veg_test <- asub(ascspc_em_test, 'VEG_SPEC_CODE',3)
spchor_test <- asub(ascspc_em_test, 'SPCHOR_MASTER',3)
mott_test <- asub(ascspc_em_test, 'MOTT_TYPE',3)
ocf_lith_test <- asub(ascspc_em_test, 'OCF_LITH_CODE',3)
colour_test <- asub(ascspc_em_test, 'COLOUR_CLASS',3)
pedality_type_test <- asub(ascspc_em_test, 'PEDALITY_TYPE',3)

asc_tag_tr = asub(ascspc_em_tr,list(1,asc_tag_list),c(2,3))
spc_tag_tr = asub(ascspc_em_tr,list(1,spc_tag_list),c(2,3))
so_tag_tr = asub(ascspc_em_tr,list(1,so_tag_list),c(2,3))
gg_tag_tr = asub(ascspc_em_tr,list(1,gg_tag_list),c(2,3))
sg_tag_tr = asub(ascspc_em_tr,list(1,sg_tag_list),c(2,3))

asc_tag_test = asub(ascspc_em_test,list(1,asc_tag_list),c(2,3))
spc_tag_test = asub(ascspc_em_test,list(1,spc_tag_list),c(2,3))
so_tag_test = asub(ascspc_em_test,list(1,so_tag_list),c(2,3))
gg_tag_test = asub(ascspc_em_test,list(1,gg_tag_list),c(2,3))
sg_tag_test = asub(ascspc_em_test,list(1,sg_tag_list),c(2,3))
# model construction ###############################
library(keras)
status_in <- layer_input(shape = c(8),name = 'status_in')
elem_type_in <- layer_input(shape = c(8),name = 'elem_type_in')
texture_in <- layer_input(shape = c(8),name = 'texture_in')
hor_subhor_in <- layer_input(shape = c(8),name = 'hor_subhor_in')
hor_suffix_in <- layer_input(shape = c(8),name = 'hor_suffix_in')
cutan_in <- layer_input(shape = c(8),name = 'cutan_in')
nature_in <- layer_input(shape = c(8),name = 'nature_in')
hor_in <- layer_input(shape = c(8),name = 'hor_in')
obs_lith_in <- layer_input(shape = c(8),name = 'obs_lith_in')
veg_in <- layer_input(shape = c(8),name = 'veg_in')
spchor_in <- layer_input(shape = c(8),name = 'spchor_in')
mott_in <- layer_input(shape = c(8),name = 'mott_in')
ocf_lith_in <- layer_input(shape = c(8),name = 'ocf_lith_in')
colour_in <- layer_input(shape = c(8),name = 'colour_in')
pedality_type_in <- layer_input(shape = c(8),name = 'pedality_type_in')

base_num_in <- layer_input(shape= c(8,length(base_num_list)),name = 'base_num_in')
base_num_in
asc_num_in <- layer_input(shape= c(8,length(asc_num_list)),name = 'asc_num_in')
asc_num_in
spc_num_in <- layer_input(shape= c(8,length(spc_num_list)),name = 'spc_num_in')
spc_num_in
# embedding#######################
status_emb <- status_in %>% layer_embedding(input_dim = 100,#length(levels(factor(ascspc_em1[,,'STATUS'] )))+1,
                                           output_dim = min(50,length(levels(factor(ascspc_em1[,,'STATUS'] )))/2), 
                                           name = 'status_emb')

status_emb
elem_type_emb <- elem_type_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'ELEM_TYPE_CODE'] )))+1,
                                 output_dim = min(50,length(levels(factor(ascspc_em1[,,'ELEM_TYPE_CODE'] )))/2),
                                name = 'elem_type_emb')
texture_emb <- texture_in %>% layer_embedding(input_dim = 100,#length(levels(factor(ascspc_em1[,,'TEXTURE_CODE'] )))+1,
                               output_dim = min(50,length(levels(factor(ascspc_em1[,,'TEXTURE_CODE'] )))/2),
                                name = 'texture_emb')
hor_subhor_emb <- hor_subhor_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'HOR_SUBHOR'] )))+1,
                                  output_dim = min(50,length(levels(factor(ascspc_em1[,,'HOR_SUBHOR'] )))/2),
                                  name = 'hor_subhor_emb')
hor_suffix_emb <- hor_suffix_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'HOR_SUFFIX'] )))+1,
                                                    output_dim = min(50,length(levels(factor(ascspc_em1[,,'HOR_SUFFIX'] )))/2),
                                                     name = 'hor_suffix_emb')
cutan_emb <- cutan_in %>% layer_embedding(input_dim = 100,#length(levels(factor(ascspc_em1[,,'CUTAN_TYPE'] )))+1,
                                                    output_dim = min(50,length(levels(factor(ascspc_em1[,,'CUTAN_TYPE'] )))/2),
                                           name = 'cutan_emb')

nature_emb <- nature_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'NATURE'] )))+1,
                             output_dim = min(50,length(levels(factor(ascspc_em1[,,'NATURE'] )))/2),
                              name = 'nature_emb')

hor_emb <- hor_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'HOR_MASTER'] )))+1,
                           output_dim = min(50,length(levels(factor(ascspc_em1[,,'HOR_MASTER'] )))/2),
                            name = 'hor_emb')
obs_lith_emb <- obs_lith_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'OBS_LITH_CODE'] )))+1, 
                                output_dim = min(50,length(levels(factor(ascspc_em1[,,'OBS_LITH_CODE'] )))/2),
                                 name = 'obs_lith_emb')
veg_emb <- veg_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'VEG_SPEC_CODE'] )))+1,  
                           output_dim = min(50,length(levels(factor(ascspc_em1[,,'VEG_SPEC_CODE'] )))/2),
                            name = 'veg_emb')
veg_emb
spchor_emb <- spchor_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'SPC_HOR_MASTER'] )))+1, 
                              output_dim = min(50,length(levels(factor(ascspc_em1[,,'SPC_HOR_MASTER'] )))/2),
                               name = 'spchor_emb')
mott_emb <- mott_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'MOTT_TYPE'] )))+1,  
                            output_dim = min(50,length(levels(factor(ascspc_em1[,,'MOTT_TYPE'] )))/2),
                              name= 'mott_emb')
ocf_lith_emb <- ocf_lith_in %>% layer_embedding(input_dim = 300,#length(levels(factor(ascspc_em1[,,'OCF_LITH_CODE'] )))+1,  
                                output_dim = min(50,length(levels(factor(ascspc_em1[,,'OCF_LITH_CODE'] )))/2),
                                 name = 'ocf_lith_emb')
colour_emb <- colour_in %>% layer_embedding(input_dim = 8,#length(levels(factor(ascspc_em1[,,'COLOUR_CLASS'] )))+1, 
                              output_dim =5,# min(50,length(levels(factor(ascspc_em1[,,'COLOUR_CLASS'] )))/2),
                               name = 'colour_emb')
pedality_type_emb <- pedality_type_in %>% layer_embedding(input_dim = 15,#length(levels(factor(ascspc_em1[,,'PEDALITY_TYPE'] )))+1, 
                                     output_dim = min(50,length(levels(factor(ascspc_em1[,,'PEDALITY_TYPE'] )))/2),
                                      name = 'pedality_type_emb')
# end embedding#########################
base_con <- layer_concatenate(list(base_num_in,status_emb,texture_emb,hor_subhor_emb,
                                  hor_suffix_emb,cutan_emb,nature_emb),axis = 2,name = 'base_con')
asc_ex_con <-layer_concatenate(list(asc_num_in,elem_type_emb,hor_emb,pedality_type_emb),
                               axis = 2,name = 'asc_ex_con')
spc_ex_con <-layer_concatenate(list(spc_num_in,obs_lith_emb,veg_emb,spchor_emb,
                                    mott_emb,ocf_lith_emb,colour_emb),
                               axis = 2,name = 'spc_ex_con')

asc_con <- layer_concatenate(list(base_con,asc_ex_con),axis = 2,name = 'asc_con') %>%
  layer_reshape(c(8,dim(base_con)[[3]]+dim(asc_ex_con)[[3]],1))
asc_model <- asc_con %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3 ), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(1, 2))
asc_model
asc_out <- asc_model %>%
  layer_flatten() %>%
  #layer_dropout(rate = 0.5) %>%
  layer_dense(units = 32, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(asc_tag_list), activation = "softmax",name = 'asc_output')
asc_out


spc_model <- layer_concatenate(list(base_con,spc_ex_con),axis = 2,name = 'spc_model') %>%
  layer_reshape(c(8,dim(base_con)[[3]]+dim(spc_ex_con)[[3]],1)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3 ), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu",
                padding = 'same') %>%
  layer_max_pooling_2d(pool_size = c(1, 2))
spc_model
spc_flat <- spc_model %>% layer_flatten(name = 'spc_flat')
spc_flat <-layer_concatenate(list(spc_flat,asc_out),axis = 1) 
spc_out <- spc_flat %>%
  #layer_dropout(rate = 0.5) %>%
  layer_dense(units = 128, activation = "relu") %>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(spc_tag_list), activation = "softmax",name = 'spc_output')
spc_out

#so_con <- layer_concatenate(list(so_ex_in),axis=2)
so_con <- layer_concatenate(list(mott_emb,colour_emb),axis=2,name = 'so_con') %>%
  layer_reshape(c(8,dim(mott_emb)[[3]]+dim(colour_emb)[[3]],1)) 
so_con <- layer_concatenate(list(so_con,asc_con),axis=2)
so_con
so_model <- so_con %>%
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

so_flat <- so_model %>%  layer_flatten()
so_flat <-layer_concatenate(list(so_flat,asc_out),axis = 1) 
#layer_dropout(rate = 0.5) %>%
so_out <- so_flat %>%
  layer_dense(units =32, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(so_tag_list), activation = "softmax",name = 'ss_output')
so_out

gg_flat <-layer_concatenate(list(so_flat,so_out),axis = 1) 
#layer_dropout(rate = 0.5) %>%
gg_out <- gg_flat %>%
  layer_dense(units = 64, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(gg_tag_list), activation = "softmax",name = 'gg_output')
gg_out

sg_flat <-layer_concatenate(list(gg_flat,gg_out),axis = 1) 
#layer_dropout(rate = 0.5) %>%
sg_out <- sg_flat %>%
  layer_dense(units = 64, activation = "relu")%>%#,kernel_regularizer = regularizer_l2(0.001)) %>%
  #layer_dense(units = 13, activation = "relu") %>%
  layer_dense(units = length(sg_tag_list), activation = "softmax",name = 'sg_output')
sg_out

model <- keras_model(list(status_in,elem_type_in,texture_in,hor_subhor_in,
                          hor_suffix_in,cutan_in,nature_in,hor_in,obs_lith_in,
                          veg_in,spchor_in,mott_in,ocf_lith_in,colour_in,pedality_type_in,
                          base_num_in,asc_num_in,spc_num_in),
                     list(asc_out,so_out,gg_out,sg_out,spc_out))
model
model %>% compile(
  optimizer = "rmsprop",
  loss = c("categorical_crossentropy","categorical_crossentropy",
           "categorical_crossentropy","categorical_crossentropy","categorical_crossentropy"),
  metrics = "acc"
)
model
model %>% save_model_hdf5('./2_asc_cnn/modelascspc_em.h5')
# tensorboard
# dir.create("./2_asc_cnn/tensorlog")
# tensorboard ("tensorlog")
# callbacks = list(callback_tensorboard(
#   log_dir = "./2_asc_cnn/tensorlog"))
#   #histogram_freq = 1,
#   #embeddings_freq = 1))

# model fitting and predicting, confusion matrix###############
history = model %>% fit(list(status_tr,elem_type_tr,texture_tr,hor_subhor_tr,
                             hor_suffix_tr,cutan_tr,nature_tr,hor_tr,obs_lith_tr,
                             veg_tr,spchor_tr,mott_tr,ocf_lith_tr,colour_tr,pedality_type_tr,
                             base_num_tr,asc_num_tr,spc_num_tr),
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
