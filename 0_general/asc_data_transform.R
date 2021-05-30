library(stats)
library(readr)
# read data from file 
asc_data <- read_rds("./0_general/asc_train_split.csv")
asc_data[asc_data =="-"]= NA
# 1D onehot transformation############################
asc_1d_onehot <- asc_data
# check "sn" attributes############################################
asc_1d_onehot$DRAINAGE = as.integer(asc_1d_onehot$DRAINAGE)
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

asc_1d_onehot$SOIL_WATER_STAT = ex_ordinal(asc_1d_onehot$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
asc_1d_onehot$BOUND_DISTINCT = ex_ordinal(asc_1d_onehot$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
asc_1d_onehot$PEDALITY_GRADE = ex_ordinal(asc_1d_onehot$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))

# rows swap to column for attributes ############################################
# set a hyperparameter: threshold of the numbers of horizons.
hr_max = 5
# delete all rows whose HORIZON_NO > hr_max
asc_1d_onehot = asc_1d_onehot[!(asc_1d_onehot$HORIZON_NO > hr_max),]
# transform all row's value onto column
# input: x: a dataframe
#        c: exceptional attributes, should include sn
#        hr_max: threshold of the numbers of horizons.
# output: dataframe with mn attributes processing completed.
row_to_col <- function(x,c,hr_max = 5) {
    y = x
    for (i in 1 : length(x)) {
      if (!( i %in% c)) {
        for(hr in 1 : hr_max){
          y[paste(names(x[i]),paste('h',hr,sep = "_"), sep = "_")] <- ifelse(x$HORIZON_NO == hr, x[[i]], 0)
        }
        y = y[ , !(names(y) %in% c(names(x[i])))]
      }
    }
    return (y)  
}
# _0: for all vn,vc columns
# _1: for vn columns only
asc_1d_onehot_0 = row_to_col(asc_1d_onehot,c(1:6,19,29:32),hr_max)  
asc_1d_onehot_1 = row_to_col(asc_1d_onehot,c(1:6,19,29:32,12,14:18,26:28),hr_max)

# One hot encoding #########################################################################  
# one hot encoding for all character type attributes in dataframe x,
# except the attributes with the index number in vector c, 
# and delete the original attributes afterwards
# inputs: datafram x, int vector c (exceptional  attributes)
# output: one hot encoding completed dataframe
one_hot_encode <- function(x,c) {
  y = x
  for (i in 1 : length(x)) { # looping through the columns of x
    if (typeof(x[[i]]) == "character" & !( colnames(x[i]) %in% c)) { # sc and vc
      for(unique_value in unique(x[[i]])){ # looping unique domain values in i-th column
        if (!is.na(unique_value)) { # filter out "NA"
          # add column with 0 or 1 encoding
          y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(x[[i]] == unique_value, 1, 0)
        }
      }
      y = y[ , !(names(y) %in% c(names(x[i])))] # remove original column one hot encoded
    }
  }
  return (y)
}

asc_1d_onehot_0 = one_hot_encode(asc_1d_onehot_0, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO",
                                                    'ASC_ORD','SUBORD_ASC_DOCE','GREAT_GROUP_ASC_CODE','SUBGROUP_ASC_CODE'))
asc_1d_onehot_1 = one_hot_encode(asc_1d_onehot_1, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO",
                                                    'ASC_ORD','SUBORD_ASC_DOCE','GREAT_GROUP_ASC_CODE','SUBGROUP_ASC_CODE'))

# One hot encoding inc NA #########################################################################  
# one hot encoding for SELECTED ATTRIBUTES 
# x: input data frame
# c: COLNAMES OF attributes which will be encoded
# and delete the original attributes afterwards
# output: one hot encoding completed dataframe
one_hot_encode_incNA <- function(x,c) {
  y = x
  for (i in c) { # looping through the columns of x
    for(unique_value in unique(x[[i]])){ # looping unique domain values in i-th column
      if (!is.na(unique_value)) { # if is not "NA"
        # add column with 0 or 1 encoding
        y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(x[[i]] == unique_value, 1, 0)
      }
      else { # if is "NA"
        y[paste(i,unique_value, sep = "_")] <- ifelse(is.na(x[[i]]),1,0)
      }
    }
    y = y[ , !(names(y) == i)] # remove original column one hot encoded
  }
  return (y)
}
asc_1d_onehot_0 = one_hot_encode_incNA(asc_1d_onehot_0, c('ASC_ORD','SUBORD_ASC_DOCE','GREAT_GROUP_ASC_CODE','SUBGROUP_ASC_CODE'))
asc_1d_onehot_1 = one_hot_encode_incNA(asc_1d_onehot_1, c('ASC_ORD','SUBORD_ASC_DOCE','GREAT_GROUP_ASC_CODE','SUBGROUP_ASC_CODE'))


# Aggregate to profile level##################################################################
aggregate_to_profile <- function(x) {
  sn_sc = grep('DRAINAGE|ELEM_TYPE_CODE|STATUS|ASC_',colnames(x))
  
  x_1 = aggregate(x=x[,sn_sc],by=list(PROJECT_CODE=x$PROJECT_CODE,
                                      SITE_ID=x$SITE_ID),FUN = mean)
  
  x_2 = aggregate(x=x[,-c(1:4,sn_sc)],by=list(PROJECT_CODE=x$PROJECT_CODE,
                                              SITE_ID=x$SITE_ID),FUN = sum)
  return(merge(x_1,x_2,by = c('PROJECT_CODE','SITE_ID')))
}

# aggregate_to_profile_new <- function(x) {
#   sn_sc = grep('DRAINAGE|ELEM_TYPE_CODE|STATUS|ASC_',colnames(x))
#   x_1 = x[,sn_sc] %>%
#     group_by(PROJECT_CODE, SITE_ID) %>%
#     summarize(value = mean(value)) %>%
#     ungroup
# }
# 
# asc_1d_onehot_0 = aggregate_to_profile(asc_1d_onehot_0)
# asc_1d_onehot_1 = aggregate_to_profile(asc_1d_onehot_1)


normalization <- function(x,c,e=0.01) {
  # Normalization x by columns except index in c, use e as adjusted eps
  # formula: y = e+(1-2e)*(x-min(x))/(max(x)-min(x))
  for (i in 1 : length(x)) {
    if (!( i %in% c)){
      x[[i]] = e+(1-2*e)*(x[[i]]-min(x[[i]][which(!is.na(x[[i]]))]))/
        (max(x[[i]][which(!is.na(x[[i]]))])-min(x[[i]][which(!is.na(x[[i]]))]))
    }
  }
  return(x)
}
class_list_0 = grep("ASC_",names(asc_1d_onehot_0))
class_list_1 = grep("ASC_",names(asc_1d_onehot_1))
asc_1d_onehot_2 = normalization(asc_1d_onehot_0,c(1:2,class_list_0),0.01)
asc_1d_onehot_3 = normalization(asc_1d_onehot_1,c(1:2,class_list_1),0.01)

svc_list_0 = grep("ELEM_|STATUS|MASTER_|SUBHOR_|SUFFIX_|TEXTURE_CODE_|CUTAN_TYPE_|PEDALITY_TYPE_|NATURE_|COLOUR_CLASS_|MOTT_TYPE_"
               ,names(asc_1d_onehot_0))
asc_1d_onehot_4 = asc_1d_onehot_2
asc_1d_onehot_4[,svc_list_0][asc_1d_onehot_4[,svc_list_0]==0.01]= 0

svc_list_1 = grep("ELEM_TYPE_|STATUS_|MASTER_|SUBHOR_|SUFFIX_|TEXTURE_CODE_|CUTAN_TYPE_|PEDALITY_TYPE_|NATURE_|COLOUR_CLASS_|MOTT_TYPE_"
                 ,names(asc_1d_onehot_1))

asc_1d_onehot_5 = asc_1d_onehot_3
asc_1d_onehot_5[,svc_list_1][asc_1d_onehot_5[,svc_list_1]==0.01]= 0

svn_list_0 = grep("DRAINAGE|FTS|UPPER|LOWER|SOIL|BOUND_|VALUE_|PEDALITY_GRADE|PREFIX"
                  ,names(asc_1d_onehot_0))
svn_list_1 = grep("DRAINAGE|FTS|UPPER|LOWER|SOIL|BOUND_|VALUE_|PEDALITY_GRADE|PREFIX"
                  ,names(asc_1d_onehot_1))
ord_list_0 = grep("ASC_ORD",names(asc_1d_onehot_0))
ord_list_1 = grep("ASC_ORD",names(asc_1d_onehot_1))

asc_1d_onehot_0 = asc_1d_onehot_0[,c(1:2,svn_list_0,svc_list_0,class_list_0)]
asc_1d_onehot_1 = asc_1d_onehot_1[,c(1:2,svn_list_1,svc_list_1,class_list_1)]
asc_1d_onehot_2 = asc_1d_onehot_2[,c(1:2,svn_list_0,svc_list_0,class_list_0)]
asc_1d_onehot_3 = asc_1d_onehot_3[,c(1:2,svn_list_1,svc_list_1,class_list_1)]
asc_1d_onehot_4 = asc_1d_onehot_4[,c(1:2,svn_list_0,svc_list_0,class_list_0)]
asc_1d_onehot_5 = asc_1d_onehot_5[,c(1:2,svn_list_1,svc_list_1,class_list_1)]
# asc_1d_onehot_0 = asc_1d_onehot_0[,c(1:1122,1135,1134,1133,1123,1127,1132,1128,1131,1136,
#                                      1124:1126,1130,1129,1137:1481)]
# asc_1d_onehot_2 = asc_1d_onehot_2[,c(1:1122,1135,1134,1133,1123,1127,1132,1128,1131,1136,
#                                      1124:1126,1130,1129,1137:1481)]
# asc_1d_onehot_4 = asc_1d_onehot_4[,c(1:1122,1135,1134,1133,1123,1127,1132,1128,1131,1136,
#                                      1124:1126,1130,1129,1137:1481)]
# asc_1d_onehot_1 = asc_1d_onehot_1[,c(1:404,417,416,415,405,409,414,410,413,418,406:408,412,411,419:763)]
# asc_1d_onehot_3 = asc_1d_onehot_3[,c(1:404,417,416,415,405,409,414,410,413,418,406:408,412,411,419:763)]
# asc_1d_onehot_5 = asc_1d_onehot_5[,c(1:404,417,416,415,405,409,414,410,413,418,406:408,412,411,419:763)]
asc_1d_onehot_0[is.na(asc_1d_onehot_0)] = 0 
asc_1d_onehot_1[is.na(asc_1d_onehot_1)] = 0 
asc_1d_onehot_2[is.na(asc_1d_onehot_2)] = 0 
asc_1d_onehot_3[is.na(asc_1d_onehot_3)] = 0 
asc_1d_onehot_4[is.na(asc_1d_onehot_4)] = 0 
asc_1d_onehot_5[is.na(asc_1d_onehot_5)] = 0 

# shuffle
idx = sample(nrow(asc_1d_onehot_0),nrow(asc_1d_onehot_0),replace = F)
asc_1d_onehot_0 = asc_1d_onehot_0[idx,]
asc_1d_onehot_1 = asc_1d_onehot_1[idx,]
asc_1d_onehot_2 = asc_1d_onehot_2[idx,]
asc_1d_onehot_3 = asc_1d_onehot_3[idx,]
asc_1d_onehot_4 = asc_1d_onehot_4[idx,]
asc_1d_onehot_5 = asc_1d_onehot_5[idx,]

write_rds(asc_1d_onehot_0,"./0_general/asc_1d_onehot_0.rds")
write_rds(asc_1d_onehot_1,"./0_general/asc_1d_onehot_1.rds")
write_rds(asc_1d_onehot_2,"./0_general/asc_1d_onehot_2.rds")
write_rds(asc_1d_onehot_3,"./0_general/asc_1d_onehot_3.rds")
write_rds(asc_1d_onehot_4,"./0_general/asc_1d_onehot_4.rds")
write_rds(asc_1d_onehot_5,"./0_general/asc_1d_onehot_5.rds")
write.csv(asc_1d_onehot_4,"./0_general/asc_1d_onehot_4.csv")





# 1D entity embedding transformation###################
asc_1d_em = asc_data
# check "sn" attributes############################################
asc_1d_em$DRAINAGE = as.integer(asc_1d_em$DRAINAGE)
# rows swap to column for attributes ############################################
# set a hyperparameter: threshold of the numbers of horizons.
hr_max = 5
# delete all rows whose HORIZON_NO > hr_max
asc_1d_em = asc_1d_em[!(asc_1d_em$HORIZON_NO > hr_max),]
# transform all row's value onto column
# input: x: a dataframe
#        c: exceptional attributes, should include sn
#        hr_max: threshold of the numbers of horizons.
# output: dataframe with mn attributes processing completed.
row_to_col <- function(x,c,hr_max = 5) {
  y = x
  for (i in 1 : length(x)) {
    if (!( i %in% c)) {
      for(hr in 1 : hr_max){
        y[paste(names(x[i]),paste('h',hr,sep = "_"), sep = "_")] <- ifelse(x$HORIZON_NO == hr, x[[i]], 0)
      }
      y = y[ , !(names(y) %in% c(names(x[i])))]
    }
  }
  return (y)  
}
# for all vn,vc columns
asc_1d_em = row_to_col(asc_1d_em,c(1:6,19,29:32),hr_max) 
# categorical attributes to numerical factors#####################
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
asc_1d_em = cat_to_numFac(asc_1d_em,c('PROJECT_CODE','SITE)_ID','OBS_NO','HORIZON_NO','ASC_ORD',
                                      'SUBORD_ASC_CODE','GREAT_GROUP_ASC_CODE','SUBGROUP_ASC_CODE'))

# One hot encoding for labels only #########################################################################  
# one hot encoding for all character type attributes in dataframe x,
# except the attributes with the index number in vector c, 
# and delete the original attributes afterwards
# inputs: datafram x, int vector c (exceptional  attributes)
# output: one hot encoding completed dataframe
one_hot_encode <- function(x,c) {
  y = x
  for (i in 1 : length(x)) { # looping through the columns of x
    if (typeof(x[[i]]) == "character" & !( colnames(x[i]) %in% c)) { # sc and vc
      for(unique_value in unique(x[[i]])){ # looping unique domain values in i-th column
        if (!is.na(unique_value)) { # filter out "NA"
          # add column with 0 or 1 encoding
          y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(x[[i]] == unique_value, 1, 0)
        }
      }
      y = y[ , !(names(y) %in% c(names(x[i])))] # remove original column one hot encoded
    }
  }
  return (y)
}
asc_1d_em = one_hot_encode(asc_1d_em, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))



# Aggregate to profile level##################################################################
aggregate_to_profile <- function(x) {
  sn_sc = grep('DRAINAGE|ELEM_TYPE_CODE|STATUS|ASC_',colnames(x))
  
  x_1 = aggregate(x=x[,sn_sc],by=list(PROJECT_CODE=x$PROJECT_CODE,
                                      SITE_ID=x$SITE_ID),FUN = mean)
  
  x_2 = aggregate(x=x[,-c(1:4,sn_sc)],by=list(PROJECT_CODE=x$PROJECT_CODE,
                                              SITE_ID=x$SITE_ID),FUN = sum)
  return(merge(x_1,x_2,by = c('PROJECT_CODE','SITE_ID')))
}
asc_1d_em = aggregate_to_profile(asc_1d_em)
# Normalization##################################
# Normalization x by columns WITH index in c, use e as adjusted eps
# formula: y = e+(1-2e)*(x-min(x))/(max(x)-min(x))
normalization <- function(x,c,e=0) {
  for (i in 1 : length(x)) {
    if ( i %in% c){
      x[[i]] = e+(1-2*e)*(x[[i]]-min(x[[i]][which(!is.na(x[[i]]))]))/
        (max(x[[i]][which(!is.na(x[[i]]))])-min(x[[i]][which(!is.na(x[[i]]))]))
    }
  }
  return(x)
}
# normalize sn and vn 
sn_vn_list = grep('DRAINAGE|FTS_|UPPER_|LOWER_|HOR_PREFIX|VALUE_',colnames(asc_1d_em))
asc_1d_em = normalization(asc_1d_em,sn_vn_list,e=0.01)
asc_1d_em[is.na(asc_1d_em)]=0
# shuffle and store to rds####################################
idx = sample(nrow(asc_1d_em),nrow(asc_1d_em),replace = F)
asc_1d_em= asc_1d_em[idx,]
asc_1d_em = asc_1d_em[,c(1:2,6:364,3:5,365:469)]
write_rds(asc_1d_em,"./0_general/asc_1d_em.rds")

# 2D onehot transformation######################
