# Library ###################################
library(stringr)
library(sqldf)
library(dplyr)

spc_onehot = read.csv('./7_spc_clustering/spc_kmean_dat.csv',stringsAsFactors=F)
spc_onehot = spc_onehot[!is.na(spc_onehot$SPC),c(1:4,19,5:18)]
# MN rows swap to column for mn attributes ############################################
# set a hyperparameter: threshold of the numbers of horizons.
k2 = 5
# delete all rows whose HORIZON_NO > hr_max
spc_onehot = spc_onehot[!(spc_onehot$HORIZON_NO > k2),]
# transform all row's value onto column
# input: x: a dataframe
#        c: exceptional attributes, should include sn
#        k2: threshold of the numbers of horizons.
# output: dataframe with mn attributes processing completed.
mn_row_to_col <- function(x,c,k2 = 5) { # fill with NA
  y = x
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) %in% c("double","integer")  & !( i %in% c)) {
      for(hr in 1 :k2){
        y[paste(names(x[i]),paste('h',hr,sep = "_"), sep = "_")] <- ifelse(x$HORIZON_NO == hr, x[[i]], NA)
      }
      y = y[ , !(names(y) %in% c(names(x[i])))]
    }
  }
  return (y)
}
spc_onehot = mn_row_to_col(spc_onehot,c(1:5,8),k2)
# One hot encoding #########################################################################  
# one hot encoding for all sc type attributes in dataframe x,
# all sc columns in vector c, 
# and delete the original attributes afterwards
# inputs: datafram x, int vector c (sc  attributes)
# output: one hot encoding completed dataframe
# one_hot_sc <- function(x,c) {
#   y = x
#   for (i in c) {
#     #if (typeof(x[[i]]) == "character" & !( colnames(spc_onehot[i]) %in% c)) {
#       for(unique_value in unique(x[[i]])){
#         if (is.na(unique_value)) {
#           y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(is.na(x[[i]]), 1, 0)
#         } 
#         else {
#           y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(x[[i]] == unique_value, 1, 0)
#           #y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(unique_value %in% x[[i]], 1, 0)
#         }
#       }
#       y = y[ , !(names(y) %in% c(names(x[i])))]
#     #}
#   }
#   return (y)
# }
# delete NA column

# one hot encoding for sc NA IS NA, 0 is 0
one_hot_sc <- function(x,c) {
  y = x
  for (i in c) {
    #if (typeof(x[[i]]) == "character" & !( colnames(spc_onehot[i]) %in% c)) {
    for(unique_value in unique(na.omit(x[[i]]))) {
      y[paste(names(x[i]),unique_value, sep = "_")] = 
        ifelse(is.na(x[[i]]),NA,ifelse(x[[i]] == unique_value,1,0)) 
    }
    y = y[ , !(names(y) %in% c(names(x[i])))]
  }
  return (y)
}

# one_hot_sc <- function(x,c) {
#   y = x
#   for (i in c) {
#     #if (typeof(x[[i]]) == "character" & !( colnames(spc_onehot[i]) %in% c)) {
#     for(unique_value in unique(x[[i]])){
#       if (!is.na(unique_value)) {
#         y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(!is.na(x[[i]]) & x[[i]] == unique_value, 1, 0)
#       }
#     }
#     y = y[ , !(names(y) %in% c(names(x[i])))]
#     #}
#   }
#   return (y)
# }
spc_onehot = one_hot_sc(spc_onehot, c(6,7,9))

# One hot encoding #########################################################################  
# one hot encoding for all mc type attributes in dataframe x,
# all mc columns in vector c, 
# and delete the original attributes afterwards
# inputs: datafram x, int vector c (mc  attributes)
# output: one hot encoding completed dataframe
one_hot_mc <- function(x,c,k1=2) {
  y = x
  for (i in c) {
    for (k in 1:k1) {# generate the list of attribute values in k1-gram
      if (k==1) {
        unique_col = unique(x[[i]])
        for(unique_value in unique_col){
          if (!is.na(unique_value)) {
            y[paste(names(x[i]),unique_value, sep = "_")] = 
              ifelse(is.na(x[[i]]),NA,ifelse(x[[i]] == unique_value,1,0))
            }
        }
      }
      else {
        #unique_col = c(unique(x[[i]]),do.call(paste,expand.grid(unique(x[[i]]),unique_col)) )
        unique_col = do.call(paste,expand.grid(unique(x[[i]]),unique_col))
        x1=x;k3 = k-1
        temp <- fn$sqldf('select B.PROJECT_CODE,B.SITE_ID,B.HORIZON_NO,
        B.HOR_MASTER|| " " ||A.HOR_MASTER HORMASTER_2 
        from x1 A,x B
        where A.PROJECT_CODE = B.PROJECT_CODE and A.SITE_ID = B.SITE_ID and
        A.HORIZON_NO = B.HORIZON_NO+"$k3" ') 
        x$HOR_MASTER = left_join(x,temp)$HORMASTER_2
        for(unique_value in unique_col){
          if (unique_value != str_c(rep('NA',k),collapse = ' ')) {
            y[paste(names(x[i]),unique_value, sep = "_")] = 
              ifelse(is.na(x[[i]]),NA,ifelse(x[[i]] == unique_value,1,0)) 
          }
        }
      }
    }
    y = y[ , !(names(y) %in% c(names(x[i])))]
  }
  return (y)
}
spc_onehot = one_hot_mc(spc_onehot, 7:13)
write.csv(spc_onehot,'./7_spc_clustering/spc_onehot.csv',row.names = F)

#spc_onehot <- read.csv("./7_spc_clustering/spc_onehot.csv",stringsAsFactors=FALSE)


# Aggregate to profile level##################################################################
sn_sc = grep('REL_MOD_SLOPE_CLASS|STATUS_|OBS_LITH_CODE_|VEG_SPEC_CODE_',colnames(spc_onehot))
# spc_onehot0 = spc_onehot[1:5,]
#merge sn,sc by mean
spc_onehot_1 = aggregate(x=spc_onehot[,sn_sc],
                         by=list(PROJECT_CODE=spc_onehot$PROJECT_CODE,SITE_ID=spc_onehot$SITE_ID,
                                 SPC=spc_onehot$SPC),FUN = mean,na.action = NA)
# merge mn,mc by sum
spc_onehot_2 = aggregate(x=spc_onehot[,-c(1:5,sn_sc)],by=list(PROJECT_CODE=spc_onehot$PROJECT_CODE,
                                                      SITE_ID=spc_onehot$SITE_ID),FUN = sum,na.rm=T)
spc_onehot_2 = merge(spc_onehot_1,spc_onehot_2,by = c('PROJECT_CODE','SITE_ID'))
# spc_onehot_2[is.na(spc_onehot_2)]=0
write.csv(spc_onehot_2, './7_spc_clustering/spc_onehot_2.csv',row.names = F)
#write.csv(centroid_spc_w,file = paste("C:/Users/lzccn/Downloads/des/centroid_spc_w",".csv",sep = " "))

