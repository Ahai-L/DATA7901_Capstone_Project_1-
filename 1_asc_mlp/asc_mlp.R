library(readr)
# start with a horizon level dataset ###############################
mlp_dat = read.csv('./0_general/asc_train_split.csv',stringsAsFactors=F) #asc_train_split
mlp_dat = readRDS('./0_general/asc_sub.rds')
# check "sn" attributes############################################
mlp_dat$DRAINAGE = as.integer(mlp_dat$DRAINAGE)

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

mlp_dat$SOIL_WATER_STAT = ex_ordinal(mlp_dat$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
mlp_dat$BOUND_DISTINCT = ex_ordinal(mlp_dat$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
mlp_dat$PEDALITY_GRADE = ex_ordinal(mlp_dat$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))

# MN rows swap to column for mn attributes ############################################
# set a hyperparameter: threshold of the numbers of horizons.

hr_max = 5
# delete all rows whose HORIZON_NO > hr_max
mlp_dat = mlp_dat[!(mlp_dat$HORIZON_NO > hr_max),]
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
mlp_dat = mn_row_to_col(mlp_dat,1:5,hr_max)
# One hot encoding #########################################################################  
# one hot encoding for all character type attributes in dataframe x,
# except the attributes with the order number in vector c, 
# and delete the original attributes afterwards
# inputs: datafram x, int vector c (exceptional  attributes)
# output: one hot encoding completed dataframe
one_hot_encode <- function(x,c) {
  y = x
  for (i in 1 : length(x)) {
    if (typeof(x[[i]]) == "character" & !( colnames(mlp_dat[i]) %in% c)) {
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
mlp_matlab = one_hot_encode(mlp_dat, c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO"))
write_rds(mlp_matlab,'./1_asc_mlp/asc_mlp_matlab_bfAggregate.rds')
# Aggregate to profile level##################################################################
aggregate_to_profile_matlab <- function(x) {
  sn_sc = grep('DRAINAGE|ELEM_TYPE_CODE|STATUS|ASC_',colnames(x))
  
  x_1 = aggregate(x=x[,sn_sc],by=list(PROJECT_CODE=x$PROJECT_CODE,
                                      SITE_ID=x$SITE_ID),FUN = mean)
  
  x_2 = aggregate(x=x[,-c(1:4,sn_sc)],by=list(PROJECT_CODE=x$PROJECT_CODE,
                                              SITE_ID=x$SITE_ID),FUN = sum)
  return(merge(x_1,x_2,by = c('PROJECT_CODE','SITE_ID')))
}

mlp_matlab = aggregate_to_profile_matlab(mlp_matlab)
# shuffle
mlp_matlab = mlp_matlab[sample(nrow(mlp_matlab),nrow(mlp_matlab),replace = F),]
# NA ->0,reorganize feature and targets
mlp_matlab[is.na(mlp_matlab)] = 0
mlp_matlab=mlp_matlab[,c(1:83,97:435,84:96)]

mlp_matlab=mlp_matlab[,c(3:422,434,433,432,423,427,431,428,429,435,424:426,430)]
mlp_matlab10=mlp_matlab[,c(1:420,423:428,430:433)]

mlp_matlab8 <- mlp_matlab10
mlp_matlab8$ASCORD_KU_SO_CH = rowSums(mlp_matlab8[,424:426])
mlp_matlab8 = mlp_matlab8[,c(1:423,431,427:430)]
write.csv(mlp_matlab8,'./1_asc_mlp/matlab_file/mlp_matlab8.csv',row.names = F)


# write to file
write_rds(mlp_matlab,'./1_asc_mlp/asc_mlp_matlab.rds')
write.csv(mlp_matlab10,'./1_asc_mlp/matlab_file/asc_mlp_matlab10.csv',row.names=F)
# sampling: for classes  that have less than "size" instances, ####################
# re-sampling to make it at least "size" amount of inputs  
resampling <- function(x,size=3000) {
  class = grep('ASC_ORD',colnames(x))
  temp = data.frame()
  for (c in class) {
    temp1 =  x[x[,c]==1,] # collect all instances belong to class c
    if (nrow(temp1) >= size) {
      temp = rbind(temp, temp1)
    } else {
      temp = rbind(temp, temp1[sample(nrow(temp1),size,replace = T),])
    }
  }
  return(temp[sample(nrow(temp),nrow(temp)),]) #shuffle and return
}
mlp_matlab_resample = resampling(mlp_matlab)
mlp_matlab_resample[is.na(mlp_matlab_resample)] = 0
mlp_matlab_resample=mlp_matlab_resample[,c(1:83,97:435,84:96)]
mlp_matlab_resample=mlp_matlab_resample[,c(1:422,434,433,432,423,427,431,428,429,435,424:426,430)]
#nrow(mlp_matlab_resample[mlp_matlab_resample$ASC_ORD_AN ==1,])
write.csv(mlp_matlab_resample,'./1_asc_mlp/matlab_file/mlp_matlab_resample.csv')
# reorganize: merge KU,SO, and CH
mlp_matlab_merge <- mlp_matlab_resample
mlp_matlab_merge$ASCORD_KU_SO_CH = rowSums(mlp_matlab_merge[,428:430])
mlp_matlab_merge = mlp_matlab_merge[,c(1:427,436,431:435)]
write.csv(mlp_matlab_merge,'./1_asc_mlp/matlab_file/mlp_matlab_merge.csv')
# Generate two classes files#####################################################################
asc_ord_list = c('ASC_ORD_AN', 'ASC_ORD_OR', 'ASC_ORD_PO','ASC_ORD_VE','ASC_ORD_HY',
                 'ASC_ORD_KU','ASC_ORD_SO','ASC_ORD_CH','ASC_ORD_CA','ASC_ORD_FE','ASC_ORD_DE',
                 'ASC_ORD_KA','ASC_ORD_RU')
# generate two classes csv file for matlab mlp model
generate_file <- function(x,c,size) {
  dat = x; cl_1 = c()
  for (i in c) {
    cl_1 = append(cl_1,i)
    if (length(cl_1) != 1) {
      for (j in cl_1[1:length(cl_1)-1]) {
        dat = filter(dat,dat[,j] == 0)
      }
    }
    dat$ASC_ORD_OTHER = ifelse( dat[,i] == 1,0,1)
    cl_2 = grep(paste(i,'|ASC_ORD_OTHER',sep = ''),colnames(dat))
    dat_s = data.frame()
    for (c in cl_2) {
      temp =  dat[ dat[,c]==1,]
      if (nrow(temp) >= size) {
        dat_s = rbind(dat_s, temp)
      } else {
        dat_s = rbind(dat_s, temp[sample(nrow(temp),size,replace = T),])
      }
    }
    dat_s = dat_s[sample(nrow(dat_s),nrow(dat_s)),]
    nrow(dat_s[dat_s$ASC_ORD_CH ==1,])
    nrow(dat_s[dat_s$ASC_ORD_OTHER ==1,])
    
    # dat_s = mlp_dat[sample(nrow(mlp_dat),5000),]
    dat_s[is.na(dat_s)] = 0
    asc = grep('ASC_',colnames(dat_s))
    dat_tag = dat_s[,cl_2]
    dat_input = dat_s[,3:(min(asc)-1)]
    file_path ="C:/Users/lzccn/Downloads/des/1_asc_mlp/matlab_file/"
    write.csv(dat_tag, 
              file =paste(file_path,i,
                          "_tag",".csv",sep = ""))
    write.csv(dat_input,
              file =paste(file_path,i,
                          ".csv",sep = ""))
  }
}
# 
# generate_file(mlp_dat,asc_ord_list,5000)
# 
# 
# mlp_dat_an = filter(mlp_dat,ASC_ORD_AN == 0#, ASC_ORD_OR == 0, ASC_ORD_PO == 0,ASC_ORD_VE == 0,ASC_ORD_HY == 0,
#                      #ASC_ORD_KU == 0,ASC_ORD_SO == 0#,ASC_ORD_CH ==0,ASC_ORD_CA==0,ASC_ORD_FE ==0,ASC_ORD_DE==0,
#                      #ASC_ORD_KA ==0
#                       )
# mlp_dat_an$ASC_ORD_OTHER = ifelse( mlp_dat_ch$ASC_ORD_AN == 1,0,1)
#  # mlp_dat_ch$ASC_ORD_CH = ifelse( (mlp_dat_ch$ASC_ORD_CH == 1|mlp_dat_ch$ASC_ORD_CH == 1|
#   #                                       mlp_dat_ch$ASC_ORD_CH == 1),1,0)
# # sampling ####################################################################
# nrow( mlp_dat_ch[ mlp_dat_ch$ASC_ORD_CH == 1 ,])
# # ttt= mlp_dat_ch[which(mlp_dat_ch$ASC_ORD_CH == 1 & (is.na(mlp_dat_ch$VALUE_15N1_h_1)|mlp_dat_ch$VALUE_15N1_h_1==0)&
# #                         (is.na(mlp_dat_ch$VALUE_15N1_h_2)|mlp_dat_ch$VALUE_15N1_h_2==0)&
# #                         (is.na(mlp_dat_ch$VALUE_15N1_h_3)|mlp_dat_ch$VALUE_15N1_h_3==0)&
# #                         (is.na(mlp_dat_ch$VALUE_15N1_h_4)|mlp_dat_ch$VALUE_15N1_h_4==0)&
# #                         (is.na(mlp_dat_ch$VALUE_15N1_h_5)|mlp_dat_ch$VALUE_15N1_h_5==0)),
# #            c(1,2,33,34,35,36,37,436)]
# # write.csv(ttt,
# #           file =paste("C:/Users/lzccn/iCloudDrive/DATA SCIENCE/DATA7703 Machine Learing/Home Works/mlp_split_so_na",
# #                       ".csv",sep = " "))
# class = grep('ASC_ORD_CH|ASC_ORD_OTHER',colnames( mlp_dat_ch))
# mlp_dat_s = data.frame()
# size = 5000
# 
# for (c in class) {
#   temp =  mlp_dat_ch[ mlp_dat_ch[,c]==1,]
#   if (nrow(temp) >= size) {
#     mlp_dat_s = rbind(mlp_dat_s, temp)
#   } else {
#     mlp_dat_s = rbind(mlp_dat_s, temp[sample(nrow(temp),size,replace = T),])
#   }
# }
# mlp_dat_s = mlp_dat_s[sample(nrow(mlp_dat_s),nrow(mlp_dat_s)),]
# nrow(mlp_dat_s[mlp_dat_s$ASC_ORD_CH ==1,])
# nrow(mlp_dat_s[mlp_dat_s$ASC_ORD_OTHER ==1,])
# 
# # mlp_dat_s = mlp_dat[sample(nrow(mlp_dat),5000),]
# mlp_dat_s[is.na(mlp_dat_s)] = 0
# asc = grep('ASC_',colnames(mlp_dat_s))
# mlp_dat_stag = mlp_dat_s[,class]
# mlp_dat_sinput = mlp_dat_s[,3:(min(asc)-1)]
# write.csv(mlp_dat_stag, 
#           file =paste("C:/Users/lzccn/iCloudDrive/DATA SCIENCE/DATA7703 Machine Learing/Home Works/mlp_split_ch_tag",
#                                ".csv",sep = " "))
# write.csv(mlp_dat_sinput,
#           file =paste("C:/Users/lzccn/iCloudDrive/DATA SCIENCE/DATA7703 Machine Learing/Home Works/mlp_split_ch_input",
#                       ".csv",sep = " "))
b = colnames(mlp_dat)[5:26]
for (i in 1:length(b)) {
  print(length(grep(b[i],colnames(a))))
}
View(a[1,,])
