library(readr)
#write_rds(asc_train_sub,'./1_asc_mlp/asc_mlp_sub.rsd')

# start with a horizon level dataset ###############################
mlp_dat_sub = readRDS('./1_asc_mlp/asc_train_sub.rds')
#mlp_dat_sub[is.na(mlp_dat_sub)]=0
# check "sn" attributes############################################
mlp_dat_sub$DRAINAGE = as.integer(mlp_dat_sub$DRAINAGE)

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

mlp_dat_sub$SOIL_WATER_STAT = ex_ordinal(mlp_dat_sub$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
mlp_dat_sub$BOUND_DISTINCT = ex_ordinal(mlp_dat_sub$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
mlp_dat_sub$PEDALITY_GRADE = ex_ordinal(mlp_dat_sub$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))

# MN rows swap to column for mn attributes ############################################
# set a hyperparameter: threshold of the numbers of horizons.

hr_max = 5
# delete all rows whose HORIZON_NO > hr_max
mlp_dat_sub = mlp_dat_sub[!(mlp_dat_sub$HORIZON_NO > hr_max),]
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
mlp_dat_sub = mn_row_to_col(mlp_dat_sub,1:5,hr_max)
# One hot encoding #########################################################################  
# one hot encoding for all character type attributes in dataframe x,
# except the attributes with the order number in vector c, 
# and delete the original attributes afterwards
# inputs: datafram x, int vector c (exceptional  attributes)
# output: one hot encoding completed dataframe
one_hot_encode <- function(x,c) {
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
#   y = x
#   for (i in 1 : length(x)) {
#     if (typeof(x[[i]]) == "character" & !( colnames(mlp_dat_sub[i]) %in% c)) {
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
#     }
#   }
#   return (y)
# }
mlp_matlab_sub = one_hot_encode(mlp_dat_sub, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))
mlp_matlab_sub[is.na(mlp_matlab_sub)]=0
#write_rds(mlp_matlab,'./1_asc_mlp/asc_mlp_matlab_bfAggregate.rds')
# Aggregate to profile level##################################################################
aggregate_to_profile_matlab <- function(x) {
  sn_sc = grep('DRAINAGE|ELEM_TYPE_CODE|STATUS|ASC_',colnames(x))
  
  x_1 = aggregate(x=x[,sn_sc],by=list(PROJECT_CODE=x$PROJECT_CODE,
                                      SITE_ID=x$SITE_ID),FUN = mean)
  
  x_2 = aggregate(x=x[,-c(1:4,sn_sc)],by=list(PROJECT_CODE=x$PROJECT_CODE,
                                              SITE_ID=x$SITE_ID),FUN = sum)
  return(merge(x_1,x_2,by = c('PROJECT_CODE','SITE_ID')))
}
mlp_matlab_sub = aggregate_to_profile_matlab(mlp_matlab_sub)
library(readr)
write_rds(mlp_matlab_sub,'./1_asc_mlp/mlp_matlab_sub.rds')

# re-sampling sub classes
# sampling: for classes  that have less than "s1" instances, delete####################
# for classes that less than s2,re-sampling to make it at least "s2" amount of inputs 
#feature: the suborder attributes that would apply the resampling
mlp_matlab_sub = readRDS('./1_asc_mlp/mlp_matlab_sub.rds')
prune <- function(x,features,s=200) {
  class = grep(features,colnames(x))
  l=c()
  for (i in class) {
    count =  nrow(x[x[,i]==1,])
    if (count < s) {
      
      l=c(l,i)
    } else {print(paste(colnames(x[i]),count))}
  }
  return(x[,-l])
}
mlp_matlab_sub1 = prune(mlp_matlab_sub,"ASC_ORD",500)
mlp_matlab_sub1 = prune(mlp_matlab_sub1,"SUBORD|GREAT_GROUP|SUBGROUP")

mlp_matlab_sub1 = mlp_matlab_sub1[,c(1:81,91,82,86,90,87,88,83,84,85,89,92:length(mlp_matlab_sub1))]
mlp_so_ex = grep('COLOUR_CLASS',colnames(mlp_matlab_sub1))
mlp_asc_list = grep('ASC_ORD',colnames(mlp_matlab_sub1))
mlp_so_list  = grep('SUBORD',colnames(mlp_matlab_sub1))
mlp_gg_list = grep('GREAT_GROUP',colnames(mlp_matlab_sub1))#,value = T)
mlp_sg_list = grep('SUBGROUP',colnames(mlp_matlab_sub1))#,value = T)

mlp_asc_prune = mlp_matlab_sub1[,-c(1:2,mlp_so_ex,mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)]
mlp_asc_prune_tag = mlp_matlab_sub1[,mlp_asc_list]
mlp_so_prune = mlp_matlab_sub1[,-c(1:2,mlp_so_list,mlp_gg_list,mlp_sg_list)]
mlp_gg_prune = mlp_matlab_sub1[,-c(1:2,mlp_gg_list,mlp_sg_list)]
mlp_sg_prune = mlp_matlab_sub1[,-c(1:2,mlp_sg_list)]
mlp_so_prune_tag = mlp_matlab_sub1[,mlp_so_list]
mlp_gg_prune_tag = mlp_matlab_sub1[,mlp_gg_list]
mlp_sg_prune_tag = mlp_matlab_sub1[,mlp_sg_list]

write.csv(mlp_asc_prune,'./1_asc_mlp/matlab_file/mlp_asc_prune.csv',row.names = F)
write.csv(mlp_so_prune,'./1_asc_mlp/matlab_file/mlp_so_prune.csv',row.names = F)
write.csv(mlp_gg_prune,'./1_asc_mlp/matlab_file/mlp_gg_prune.csv',row.names = F)
write.csv(mlp_sg_prune,'./1_asc_mlp/matlab_file/mlp_sg_prune.csv',row.names = F)
write.csv(mlp_asc_prune_tag,'./1_asc_mlp/matlab_file/mlp_asc_prune_tag.csv',row.names = F)
write.csv(mlp_so_prune_tag,'./1_asc_mlp/matlab_file/mlp_so_prune_tag.csv',row.names = F)
write.csv(mlp_gg_prune_tag,'./1_asc_mlp/matlab_file/mlp_gg_prune_tag.csv',row.names = F)
write.csv(mlp_sg_prune_tag,'./1_asc_mlp/matlab_file/mlp_sg_prune_tag.csv',row.names = F)

resampling_sub <- function(x,feature,s1=100,s2=1000) {
  class = grep(feature,colnames(x))
  temp = data.frame(); l = c()
  for (c in class) {
    temp1 =  x[x[,c]==1,] # collect all instances belong to class c
    if (nrow(temp1) > s2) {
      temp = rbind(temp, temp1)

    } else if (nrow(temp1)>s1){
      temp = rbind(temp, temp1[sample(nrow(temp1),s2,replace = T),])

    } else {l = c(l,c)}
  }
  return(temp[sample(nrow(temp),nrow(temp)),-l]) #shuffle and return
}




mlp_asc_train = resampling_sub(mlp_matlab_sub,"SUBORD")
mlp_subord_train = mlp_subord_train[!mlp_subord_train$SUBORD_ASC_CODE_0==1,
                                    !colnames(mlp_subord_train) %in% c(asc_greatgroup_list,asc_subgroup_list,'SUBORD_ASC_CODE_0')]
subord=grep('SUBORD',colnames(mlp_subord_train))
mlp_subord_train=mlp_subord_train[,c(1:(subord[1]-1),
                                             (subord[1]+length(subord)):length(mlp_subord_train),subord)]
write.csv(mlp_subord_train,'./1_asc_mlp/matlab_file/mlp_subord_train.csv')

mlp_greatgroup_train = resampling_sub(mlp_matlab_sub,"GREAT_GROUP",s1=300)
mlp_greatgroup_train = mlp_greatgroup_train[!mlp_greatgroup_train$GREAT_GROUP_ASC_CODE_0==1,
                                            !colnames(mlp_greatgroup_train) %in% c(asc_subgroup_list,'GREAT_GROUP_ASC_CODE_0')]
greatgroup=grep('GREAT_GROUP',colnames(mlp_greatgroup_train))
mlp_greatgroup_train=mlp_greatgroup_train[,c(1:(greatgroup[1]-1),
                                             (greatgroup[1]+length(greatgroup)):length(mlp_greatgroup_train),greatgroup)]
write.csv(mlp_greatgroup_train,'./1_asc_mlp/matlab_file/mlp_greatgroup_19.csv')

mlp_subgroup_train = resampling_sub(mlp_matlab_sub,"SUBGROUP",s1=400)
mlp_subgroup_train = mlp_subgroup_train[!mlp_subgroup_train$SUBGROUP_ASC_CODE_0==1,
                                        !colnames(mlp_subgroup_train) %in% c('SUBGROUP_ASC_CODE_0')]
subgroup = grep('SUBGROUP',colnames(mlp_subgroup_train))
mlp_subgroup_train=mlp_subgroup_train[,c(1:(subgroup[1]-1),(subgroup[1]+length(subgroup)):length(mlp_subgroup_train),subgroup)]
write.csv(mlp_subgroup_train,'./1_asc_mlp/matlab_file/mlp_subgroup_20.csv')

# rare categories issues plot:
for (i in asc_subord_list){
    print(nrow(mlp_subord_train[mlp_subord_train[,i]==1,]))
}

ASC_LEAF_NODE = mlp_matlab_sub %>% group_by_at(c(mlp_asc_list,mlp_so_list,mlp_gg_list,mlp_sg_list)) %>% count()
qplot(ASC_LEAF_NODE$n, geom="histogram",bins=15) 
nrow(ASC_LEAF_NODE[ASC_LEAF_NODE$n<10,])/nrow(ASC_LEAF_NODE)

ASC_GG_NODE = mlp_matlab_sub %>% group_by_at(c(mlp_asc_list,mlp_so_list,mlp_gg_list)) %>% count()
qplot(ASC_GG_NODE$n, geom="histogram",bins=15) 
nrow(ASC_GG_NODE[ASC_GG_NODE$n<10,])/nrow(ASC_GG_NODE)

ASC_SO_NODE = mlp_matlab_sub %>% group_by_at(c(mlp_asc_list,mlp_so_list)) %>% count()
qplot(ASC_SO_NODE$n, geom="histogram",bins=15) 
nrow(ASC_SO_NODE[ASC_SO_NODE$n<10,])/nrow(ASC_SO_NODE)

ASC_ORD_NODE = mlp_matlab_sub %>% group_by_at(c(mlp_asc_list)) %>% count()
qplot(ASC_ORD_NODE$n, geom="histogram",bins=15) 
nrow(ASC_ORD_NODE[ASC_ORD_NODE$n<10,])/nrow(ASC_ORD_NODE)

