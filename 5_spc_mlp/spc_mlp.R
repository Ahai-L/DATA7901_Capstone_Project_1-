library(readr)
library(stringr)
library(dplyr)
library(caret)
library(RWeka)
# start with a horizon level dataset ###############################
spc_mlp <- read.csv("./0_general/spc_train.csv",stringsAsFactors=FALSE)
spc_mlp = spc_mlp[!is.na(spc_mlp$SPC),]
spc_mlp_cluster = read_rds('./7_spc_clustering/cluster_df_onehot.rds')
spc_mlp = left_join(spc_mlp,spc_mlp_cluster,by= c('SPC'='spc'))
spc_mlp$CLUSTER = as.character(spc_mlp$cluster)
spc_mlp= spc_mlp[,!colnames(spc_mlp) =='SPC']
spc_mlp= spc_mlp[,!colnames(spc_mlp) =='cluster']
spc_mlp[spc_mlp=='-']=NA
spc_mlp = spc_mlp[!is.na(spc_mlp$CLUSTER),]
#
spc_cluster_count = spc_mlp %>% group_by(CLUSTER,) %>% count_()
plot(spc_cluster_count$n, geom="histogram",bins=15) 
nrow(spc_cluster_count[spc_cluster_count$n<50,])/nrow(spc_cluster_count)

# check "sn" attributes############################################
#spc_mlp$HEIGHT_CLASS = as.integer(spc_mlp$HEIGHT_CLASS)
#spc_mlp$COVER_CLASS = as.integer(spc_mlp$COVER_CLASS)
#spc_mlp$PEDALITY_SIZE = as.integer(spc_mlp$PEDALITY_SIZE)
#spc_mlp$SURF_FRAG_SIZE = as.integer(spc_mlp$SURF_FRAG_SIZE)
#spc_mlp$OCF_ABUNDANCE = as.integer(spc_mlp$OCF_ABUNDANCE)

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
# only keep the first three charactors of "VEG_SPC_CODE"
spc_mlp$VEG_SPEC_CODE = str_extract(spc_mlp$VEG_SPEC_CODE, "^.{3}")

spc_mlp$STATUS[spc_mlp$STATUS %in% c('P','R','O','T')] = NA

spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('QS','S')] = 'Siliceous Upper'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('GS','MS','MU','PL','PH','PO','ST',
                          'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('AC','FC','FS','IS')] = 'Sesquioxide'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('CO','CC')] = 'Organic'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('AH','GY','HA')] = 'Evaporite'
spc_mlp$OBS_LITH_CODE[spc_mlp$OBS_LITH_CODE %in% 
                        c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# $OCF_LITH_CODE
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('QS','S')] = 'Siliceous Upper'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('GS','MS','MU','PL','PH','PO','ST',
                          'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('AC','FC','FS','IS')] = 'Sesquioxide'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('CO','CC')] = 'Organic'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('AH','GY','HA')] = 'Evaporite'
spc_mlp$OCF_LITH_CODE[spc_mlp$OCF_LITH_CODE %in% 
                        c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'

spc_mlp$REL_MOD_SLOPE_CLASS = ex_ordinal(spc_mlp$REL_MOD_SLOPE_CLASS,
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

spc_mlp$TEXTURE_CODE[spc_mlp$TEXTURE_CODE %in% 
                       c('S','LS','CS','SL','SCL','CFS','CKS','FS','FSCL',
                         'FSCLZ','FSL','FSLZ','KS','FSCL','KSL','KSS','LFS',
                         'LFSY','LFSYZ','LKS','LMS','LSY','MS','SCFLS','SLZ','SS','ST',
                         'KSCL','SCLFS')] = 'Sandy'
spc_mlp$TEXTURE_CODE[spc_mlp$TEXTURE_CODE %in% 
                       c('L','ZL','ZCL','CLFS','CL','CLS','CLFSZ','CLKS','CLMS','CLZ','ZCL')] = 'Loamy'
spc_mlp$TEXTURE_CODE[spc_mlp$TEXTURE_CODE %in% 
                       c('C','LC','LMC','MC','MHC','HC','CSC','FSHC','FSLC','FSLCZ','FSLMC',
                         'FSMC','FSMHC','KSHC','KSLMC','KSMC','KSMHC','LCFS',
                         'LCKS','LCS','LMCFS','LMCKS','LMCS','LMCZ','LMCKS',
                         'LMCS','MCFS','MCS','MCZ','MHCFS','MHCS','MSC','SC','SHC',
                         'SLC','SLMC','SMC','SMHC','ZC','ZHC','ZLC','ZLCFS','ZLMC','ZLMCS',
                         'ZMC','ZMHC','LCZ','FSC','KSC','KSLC')] = 'Clayey'
spc_mlp$TEXTURE_CODE[spc_mlp$TEXTURE_CODE %in% 
                       c('AP','CP','GP','GR','HP','IP','LP','SP')] = 'Organic'
sort(unique(spc_mlp$TEXTURE_CODE))
spc_mlp$MOTT_TYPE[!spc_mlp$MOTT_TYPE =='M'] = NA
spc_mlp$MOTT_TYPE = ex_ordinal(spc_mlp$MOTT_TYPE,
                               list(c('M'),1))
#spc_mlp$MOTT_TYPE = as.integer(spc_mlp$MOTT_TYPE)
spc_mlp$PEDALITY_GRADE = ex_ordinal(spc_mlp$PEDALITY_GRADE,
                                    list(c('G','V','W','M','S'),1:5))

spc_mlp[spc_mlp$NATURE %in% c('U','O'),'NATURE'] = NA
spc_mlp$BOUND_DISTINCT[spc_mlp$BOUND_DISTINCT %in% c('S','A','C')] = "GRP1"

spc_mlp$BOUND_DISTINCT[spc_mlp$BOUND_DISTINCT %in% c('G','D')] = "GRP2"


# MN rows swap to column for mn attributes ############################################
# set a hyperparameter: threshold of the numbers of horizons.
hr_max = 5
# delete all rows whose HORIZON_NO > hr_max
spc_mlp = spc_mlp[!(spc_mlp$HORIZON_NO > hr_max),]
# transform all row's value onto column
# input: x: a dataframe
#        c: exceptional attributes, should include sn
#        hr_max: threshold of the numbers of horizons.
# output: dataframe with mn attributes processing completed.
mn_row_to_col <- function(x,c,hr_max = 5) {
  y = x
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) %in% c("double","integer")  & !( i %in% c)) {
      for(hr in 1 : hr_max){
        y[paste(names(x[i]),paste('h',hr,sep = "_"), sep = "_")] <- ifelse(x$HORIZON_NO == hr, x[[i]], 0)
      }
      y = y[ , !(names(y) %in% c(names(x[i])))]
    }
  }
  return (y)
}
spc_mlp = mn_row_to_col(spc_mlp,c(1:4,7),hr_max)
# One hot encoding #########################################################################  
# one hot encoding for all character type attributes in dataframe x,
# except the attributes with the order number in vector c, 
# and delete the original attributes afterwards
# inputs: datafram x, vector c (exceptional  attributes)
# output: one hot encoding completed dataframe
one_hot_encode <- function(x,c) {
  y = x
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) == "character" & !( colnames(spc_mlp[i]) %in% c)) {
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
spc_mlp = one_hot_encode(spc_mlp, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))
spc_mlp[is.na(spc_mlp)]=0
# Aggregate to profile level##################################################################
#record the idex of sn,sc and spc 
sn_sc = grep('STATUS|LITH_CODE|ELEM_TYPE_CODE|REL_MOD_SLOPE_CLASS|VGE_SPEC_CODE|CLUSTER',colnames(spc_mlp))


spc_mlp_1 = aggregate(x=spc_mlp[,sn_sc],by=list(PROJECT_CODE=spc_mlp$PROJECT_CODE,
                                                SITE_ID=spc_mlp$SITE_ID),FUN = mean)

spc_mlp = aggregate(x=spc_mlp[,-c(1:4,sn_sc)],by=list(PROJECT_CODE=spc_mlp$PROJECT_CODE,
                                                      SITE_ID=spc_mlp$SITE_ID),FUN = sum)
spc_mlp = merge(spc_mlp,spc_mlp_1,by = c('PROJECT_CODE','SITE_ID'))

write_rds(spc_mlp,'./5_spc_mlp/spc_mlp.rds')
#write.arff(spc_mlp,'./5_spc_mlp/spc_mlp.arff')
# sampling: for classes  that have less than "s1" instances, delete####################
# for classes that less than s2,re-sampling to make it at least "s2" amount of inputs 
#feature: the suborder attributes that would apply the resampling
spc_mlp = readRDS('./5_spc_mlp/spc_mlp.rds')
# prune <- function(x,features,s=50) {
#   class = grep(features,colnames(x))
#   l=c()
#   for (i in class) {
#     count =  nrow(x[x[,i]==1,])
#     if (count < s) {
#       l=c(l,i)
#       x = x[x[,i]==0,]
#     } else {print(paste(colnames(x[i]),count))}
#   }
#   return(x[,-l])
# }
# 
# spc_mlp1 = prune(spc_mlp,"CLUSTER")
# 
# spc_mlp1 = spc_mlp1[sample(nrow(spc_mlp1),nrow(spc_mlp1)),]
# 
spc_list = grep('CLUSTER',colnames(spc_mlp))
# 
spc_data = spc_mlp[,-c(1:2,spc_list)]
spc_tag = spc_mlp[,spc_list]
# 
# write.csv(spc_prune,'./5_spc_mlp/spc_prune.csv',row.names = F)
# write.csv(spc_tag,'./5_spc_mlp/spc_tag.csv',row.names = F)

library(keras)

spc_data = as.matrix(spc_data)
spc_tag = as.matrix(spc_tag)
idx = sample( nrow(spc_data), round(nrow(spc_data)*0.85))
spc_tr = spc_data[idx,]
spc_te = spc_data[-idx,]
spc_tag_tr = spc_tag[idx,]
spc_tag_te= spc_tag[-idx,]

#  model construction###################
spc_in = layer_input(shape = c(ncol(spc_tr)))
spc_out = spc_in %>%
  layer_dense(units=1024,activation = 'relu') %>%
  layer_dense(units=512,activation = 'relu') %>%
  layer_dense(units=ncol(spc_tag_tr),activation = 'softmax')
model = keras_model(spc_in,spc_out)
model

model %>% compile (
  optimizer =  optimizer_rmsprop(lr =0.0005),
  loss = 'categorical_crossentropy',
  metrics= metric_categorical_accuracy)
# model fitting###############
history <- model %>% fit(spc_tr,
                         spc_tag_tr,
                         epochs=30,
                         batch_size=512,
                         validation_split =0.15 )
result = model %>% evaluate(spc_te,spc_tag_te)
spc_predict <- model %>% predict(spc_te)
spc_predict= as.data.frame(spc_predict)
# confusion matrix###################
colnames(spc_predict) = colnames(spc_tag_te)
rownames(spc_predict) = rownames(spc_tag_te)
spc_p = cbind(colnames(spc_predict)[apply(spc_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                    colnames(spc_predict)[apply(spc_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                                 match(sort(x[x>0],decreasing = T)[1],x),
                                                                                 match(sort(x[x>0],decreasing = T)[2],x)
                    ))],
                    colnames(spc_tag_te)[apply(spc_tag_te,1,which.max)],
                    spc_predict)
colnames(spc_p)[1:3]=c('SPC_PRE1','SPC_PRE2','SPC_REAL')
spc_p$SPC_PRE_TOP2 = as.character(spc_p$SPC_PRE1)
spc_p[as.character(spc_p$SPC_PRE2)==as.character(spc_p$SPC_REAL),'SPC_PRE_TOP2'] =
  spc_p[as.character(spc_p$SPC_PRE2)==as.character(spc_p$SPC_REAL),2]
spc_p=spc_p[,c(1,2,length(spc_p),3)]
spc_p$SPC_PRE_TOP2 =as.factor(spc_p$SPC_PRE_TOP2)
spc_p = rbind(spc_p,
             data.frame('SPC_PRE1'=rep(NA,
                                   length(spc_p[which(is.na(match(spc_p$SPC_PRE1,spc_p$SPC_REAL))),'SPC_PRE1'])),
                        'SPC_REAL'=spc_p[which(is.na(match(spc_p$SPC_PRE1,spc_p$SPC_REAL))),'SPC_PRE1']))
spc_p = rbind(spc_p,
             data.frame('P_S'=spc_p[which(is.na(match(spc_p$R_SG,spc_p$P_SG))),'R_SG'],
                        'R_S'=rep(NA,length(spc_p[which(is.na(match(spc_p$R_SG,spc_p$P_SG))),'R_SG']))
             ))
#
spc_con=confusionMatrix(spc_p[,1],spc_p[,4])
spc_con_top2 = confusionMatrix(spc_p[,3],spc_p[,4])
