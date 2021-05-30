# Transform the extracted data into suitable formatted input data for various models
# Also split into training, test, and predict data
library(stats)
library(readr)
library(abind)
library(dplyr)
resampling_2d <- function(x,feature,s1=1,s2=5000) {
  # re-sampling data with 2d format, 
  # if the example number of a class is less than s2, then repeat sampling until get to s2
  # if less then s1, then ignore this class.
  class = grep(feature, dimnames(x)[[3]])
  temp = array(numeric(),c(0,dim(x)[[2]],dim(x)[[3]])); l = c()
  for (c in class) {
    temp1 =  x[x[,1,c]==1,,] # collect all instances belong to class c
    if (nrow(temp1) > s2) {# if number of sample is larger than s2
      temp = abind(temp, temp1,along = 1)
      
    } else if (nrow(temp1)>s1){ # if s1 < # sample < s2 
      temp = abind(temp, temp1[sample(nrow(temp1),s2,replace = T),,], along = 1)
      
    } else {l = c(l,c)} # # sample < s1
  }
  #shuffle and return
  if (is.null(l)) {
    return(temp[sample(nrow(temp),nrow(temp),replace = F),,])
  }
  else {
    return(temp[sample(nrow(temp),nrow(temp)),,-l])
  }
}

# read data from file genetated by "1_ord_extract.r"
ord <- read_rds("./00_ord/ord_not_normalize.rds")
# 1D onehot transformation#################################
idx = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','ASC_CONFIDENCE')
sn = c('DRAINAGE')
sc = c('ELEM_TYPE_CODE','STATUS')
vn = c('UPPER_DEPTH','LOWER_DEPTH','SOIL_WATER_STAT','FTS_PH','BOUND_DISTINCT','FE_13C1',
       'ESP_15N1','CARBON_6B_6A','PH_4A1','Clay_2Z2','PEDALITY_GRADE','HOR_PREFIX')
vc = c('PEDALITY_TYPE','NATURE','CUTAN_TYPE','TEXTURE_CODE','HOR_MASTER','HOR_SUBHOR','HOR_SUFFIX')
tag = c('ASC_ORD')
# check "sn" attributes############################################
ord$DRAINAGE = as.integer(ord$DRAINAGE)
# Normalization##################################
# Normalization x by columns , use e as adjusted eps
# formula: y = e+(1-2e)*(x-min(x))/(max(x)-min(x))
normalization <- function(x,incl,e=0) {
  for (i in names(x)) {
    if ( i %in% incl){
      x[[i]] = e+(1-2*e)*(x[[i]]-min(x[[i]][which(!is.na(x[[i]]))]))/
        (max(x[[i]][which(!is.na(x[[i]]))])-min(x[[i]][which(!is.na(x[[i]]))]))
    }
  }
  return(x)
}
# # normalize sn and vn 
# sn_vn_list = grep('DRAINAGE|FTS_|UPPER_|LOWER_|HOR_PREFIX|VALUE_',colnames(ord_em1d))
# ord_em1d = normalization(ord_em1d,sn_vn_list,e=0.01)
# ord_em1d[is.na(ord_em1d)]=0

row_to_col <- function(x,incl=NA,excl=NA,hr_max = 5) {
  # transform selected row's value with same horizon_no onto column
  # input: x: a dataframe
  #        incl: selected columns in x which will process
  #        excl: selected columns in x which will not process
  #        incl has first priority
  #        if both incl and excl are NA, then all feature will conduct the row_to_col
  #        hr_max: threshold of the numbers of horizons.
  # output: dataframe with mn attributes processing completed.
  if (!is.na(incl[1])) {list = incl}
  else if (!is.na(excl[1])) {list = names(x)[!names[x] %in% excl]}
  else { list = names(x)}
  y = x
  for (i in list) {
    for (hr in 1:hr_max) {
      y[paste(i,paste('h',hr,sep = "_"), sep = "_")] <- ifelse(x$HORIZON_NO == hr, 
                                                               ifelse(is.na(x[[i]]),NA,x[[i]]), 0)
    }
    y = y[ , !(names(y) %in% c(i))]
  }
  return (y)  
}
one_hot_encode <- function(x,incl=NA,excl=NA,remove=T) {
  # One hot encoding incl NA  
  # x: input data frame
  # and delete the original attributes afterwards
  #        incl: selected columns in x which will process
  #        excl: selected columns in x which will not process
  #        incl has first priority
  #        if both incl and excl are NA, then all features will conduct onehot encoding
  # output: one hot encoding completed dataframe
  if (!is.na(incl[1])) {list = incl}
  else if (!is.na(excl[1])) {list = names(x)[!names[x] %in% excl]}
  else { list = names(x)}
  y = x
  for (i in list) { # looping through the columns of x
    for(unique_value in unique(x[[i]])){ # looping unique domain values in i-th column
      if (!is.na(unique_value)) { # if is not "NA"
        # add column with 0 or 1 encoding
        y[paste(names(x[i]),unique_value, sep = "_")] <- ifelse(x[[i]] == unique_value, 1, 0)
      }
      else { # if is "NA"
        y[paste(i,unique_value, sep = "_")] <- ifelse(is.na(x[[i]]),1,0)
      }
    }
    if (remove) {
      y = y[ , !(names(y) == i)] # remove original column one hot encoded
    }
  }
  return (y)
}
aggregate_to_profile <- function(x,idx,sn,SC,VN,VC,TAG) {
  # aggregate horizon level file into profile level file
  # input: x - horizon level dataframe
  #        idx: index columns, such as "PROJECT_CODE", "SITE_ID" etc
  #        sn: scalar numerical valued columns
  #        SC: treated (onehot encoded) scalar categorical valued columns
  #        TAG: treated tag columns
  x_1 = aggregate(x=x[,c(sn,SC,TAG)],
                  by=list(PROJECT_CODE=x$PROJECT_CODE,
                                      SITE_ID=x$SITE_ID,ASC_CONFIDENCE=x$ASC_CONFIDENCE),FUN = mean)
  
  x_2 = aggregate(x=x[,c(VN,VC)],
                    list(PROJECT_CODE=x$PROJECT_CODE,
                                      SITE_ID=x$SITE_ID,ASC_CONFIDENCE=x$ASC_CONFIDENCE),FUN = sum)
  return(merge(x_1,x_2,by = 
               c('PROJECT_CODE','SITE_ID','ASC_CONFIDENCE')))
}
ord_onehot1d_transform <- function(data,hr_max,idx,sn,sc,vn,vc,tag){
  # delete all rows whose HORIZON_NO > hr_max
  data = data[!(data$HORIZON_NO > hr_max),]
  # sc, vc, and tag onehot encoding
  data = one_hot_encode(data,incl = c(sc,vc,tag))
  # vn,VC, swap rows to columns
  VC = grep('PEDALITY_TYPE|NATURE|CUTAN_TYPE|TEXTURE_CODE|HOR_MASTER|HOR_SUBHOR|HOR_SUFFIX',
            colnames(data),value = T)
  data = row_to_col(data,incl=c(vn,VC),hr_max=hr_max)
  SC = grep('ELEM_TYPE_CODE|STATUS',colnames(data),value = T)
  VN = grep('UPPER_DEPTH|LOWER_DEPTH|SOIL_WATER_STAT|FTS_PH|BOUND_DISTINCT|FE_13C1|ESP_15N1|CARBON_6B_6A|PH_4A1|Clay_2Z2|PEDALITY_GRADE|HOR_PREFIX',colnames(data),value = T)
  VC = grep('PEDALITY_TYPE|NATURE|CUTAN_TYPE|TEXTURE_CODE|HOR_MASTER|HOR_SUBHOR|HOR_SUFFIX',
            colnames(data),value = T)
  TAG = grep('ASC_ORD', colnames(data),value = T)
  data = aggregate_to_profile(data,idx,sn,SC,VN,VC,TAG)
  return(data)
}
# data transforms to onehot1d
ord = ord_onehot1d_transform(ord,hr_max=5,idx=idx,sn=sn,sc=sc,vn = vn,vc = vc,tag=tag)
# all NA to 0
ord[is.na(ord)]=0
# split training, test, and predict datasets
ord_tr = ord[ord$ASC_CONFIDENCE!=1 & ord$ASC_ORD_NA!=1,! colnames(ord) %in% c('ASC_CONFIDENCE','ASC_ORD_NA')]
ord_ts =  ord[ord$ASC_CONFIDENCE ==1 & ord$ASC_ORD_NA!=1,! colnames(ord) %in% c('ASC_CONFIDENCE','ASC_ORD_NA')]
tag_names = grep('ASC_ORD|ASC_CONFIDENCE',names(ord),value = T)
ord_nd = ord[ ord$ASC_ORD_NA==1,!colnames(ord) %in% tag_names]
# write to file########################
write_rds(ord,"./00_ord/ord_onehot1d_nn.rds")
write_rds(ord_tr,"./00_ord/ord_tr_onehot1d_nn.rds")
write_rds(ord_ts,"./00_ord/ord_ts_onehot1d_nn.rds")
write_rds(ord_nd,"./00_ord/ord_nd_onehot1d_nn.rds")
# resampling for training dataset
ord_tr <- read_rds("./00_ord/ord_tr_onehot1d.rds")
resampling_1d <- function(x,feature,s1=1,s2=5000) {
  # resampling data, if the example number of a class is less than s2, then repeat sampling until get to s2
  # if less then s1, then ignore this class.
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
  if (is.null(l)) {
    return(temp[sample(nrow(temp),nrow(temp),replace = F),])
  }
  else {
    return(temp[sample(nrow(temp),nrow(temp)),-l])
  }
  #shuffle and return
}
ord_tr = resampling_1d(ord_tr,"ASC_ORD")
write_rds(ord_tr,"./00_ord/ord_tr_onehot1d_re.rds")

 
# 1D entity embedding transformation###################
ord_em1d = read_rds("./00_ord/ord_not_normalize.rds")
idx = c('PRODUCT_CODE','SITE_ID','OBS_NO','HORIZON_NO','ASC_CONFIDENCE')
sn = c('DRAINAGE')
sc = c('ELEM_TYPE_CODE','STATUS')
vn = c('UPPER_DEPTH','LOWER_DEPTH','SOIL_WATER_STAT','FTS_PH','BOUND_DISTINCT','FE_13C1',
       'ESP_15N1','CARBON_6B_6A','PH_4A1','Clay_2Z2','PEDALITY_GRADE','HOR_PREFIX')
vc = c('PEDALITY_TYPE','NATURE','CUTAN_TYPE','TEXTURE_CODE','HOR_MASTER','HOR_SUBHOR','HOR_SUFFIX')
tag = c('ASC_ORD')
# normalization sn and vn#############
ord_em1d = normalization(ord_em1d,c(sn,vn),e=0.01)
# rows swap to column for attributes ############################################
# set a hyperparameter: threshold of the numbers of horizons.
hr_max = 5
# delete all rows whose HORIZON_NO > hr_max
ord_em1d = ord_em1d[!(ord_em1d$HORIZON_NO > hr_max),]
# row to col for all vn,vc columns
ord_em1d = row_to_col(ord_em1d,incl=c(vn,vc),hr_max=hr_max) 
# categorical attributes to numerical factors#####################
cat_to_numFac <- function(x,excl) {
  for (i in names(x)) {
    if (typeof(x[[i]]) == "character" & !( colnames(x[i]) %in% excl)) { # sc and mc
      x[[i]][is.na(x[[i]])] = "0"
      x[[i]] <- factor(x[[i]])
      levels(x[[i]]) <- 0: (length(levels(x[[i]]))-1)
      x[[i]] = as.numeric(as.character(x[[i]]))
    }
  }
  return(x)
}

ord_em1d = cat_to_numFac(ord_em1d,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','ASC_ORD','ASC_CONFIDENCE'))

# One hot encoding for labels only #########################################################################
ord_em1d = one_hot_encode(ord_em1d, incl = tag)
VN = grep('UPPER_DEPTH|LOWER_DEPTH|SOIL_WATER_STAT|FTS_PH|BOUND_DISTINCT|FE_13C1|ESP_15N1|CARBON_6B_6A|PH_4A1|Clay_2Z2|PEDALITY_GRADE|HOR_PREFIX',colnames(ord_em1d),value = T)
VC = grep('PEDALITY_TYPE|NATURE|CUTAN_TYPE|TEXTURE_CODE|HOR_MASTER|HOR_SUBHOR|HOR_SUFFIX',
          colnames(ord_em1d),value = T)
TAG = grep('ASC_ORD', colnames(ord_em1d),value = T)

# Aggregate to profile level##################################################################
ord_em1d = aggregate_to_profile(ord_em1d,idx,sn,sc,VN,VC,TAG)
# all NA to 0
ord_em1d[is.na(ord_em1d)]=0
# split training, test, and predict datasets
ord_em1d_tr = ord_em1d[ord_em1d$ASC_CONFIDENCE!=1 & ord_em1d$ASC_ORD_NA!=1,! colnames(ord_em1d) %in% c('ASC_CONFIDENCE','ASC_ORD_NA')]
ord_em1d_ts =  ord_em1d[ord_em1d$ASC_CONFIDENCE ==1 & ord_em1d$ASC_ORD_NA!=1,! colnames(ord_em1d) %in% c('ASC_CONFIDENCE','ASC_ORD_NA')]
tag_names = grep('ASC_ORD|ASC_CONFIDENCE',names(ord_em1d),value = T)
ord_em1d_nd = ord_em1d[ ord_em1d$ASC_ORD_NA==1,!colnames(ord_em1d) %in% tag_names]
# write to file########################
write_rds(ord_em1d,"./00_ord/ord_em1d.rds")
write_rds(ord_em1d_tr,"./00_ord/ord_tr_em1d.rds")
write_rds(ord_em1d_ts,"./00_ord/ord_ts_em1d.rds")
write_rds(ord_em1d_nd,"./00_ord/ord_nd_em1d.rds")

ord_em1d_tr_re = resampling_1d(ord_em1d_tr,'ASC_ORD')
write_rds(ord_em1d_tr_re,"./00_ord/ord_tr_em1d_re.rds")

# 2D onehot transformation######################
# 2D Entity Embedding transformation###################
# read data from file genetated by "1_ord_extract.r"
ord_em2d = read_rds("./00_ord/ord_not_normalize.rds")
# ord_em2d = ord_em2d[1:1000,]
tag_conf = c('ASC_CONFIDENCE')
idx = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')
sn = c('DRAINAGE')
sc = c('ELEM_TYPE_CODE','STATUS')
vn = c('UPPER_DEPTH','LOWER_DEPTH','SOIL_WATER_STAT','FTS_PH','BOUND_DISTINCT','FE_13C1',
       'ESP_15N1','CARBON_6B_6A','PH_4A1','Clay_2Z2','PEDALITY_GRADE','HOR_PREFIX')
vc = c('PEDALITY_TYPE','NATURE','CUTAN_TYPE','TEXTURE_CODE','HOR_MASTER','HOR_SUBHOR','HOR_SUFFIX')
tag = c('ASC_ORD')

# normalization sn and vn#############
ord_em2d = normalization(ord_em2d,c(sn,vn),e=0.01)
# categorical to numeric factor#####################################
ord_em2d <- cat_to_numFac(ord_em2d,c(idx,tag))
# check the factors of ELEM_TYPE_CODE
#sort(unique(ord_em2d$ELEM_TYPE_CODE))
# record # of factors for each features###############
em_factor_dic = list()#data.frame(Features =NA,Dic_Number =NA)[-1,]
for (i in c(sc,vc)){
  em_factor_dic[[i]]= length(unique(ord_em2d[,i]))
}
write_rds(em_factor_dic,'./00_ord/em_factor_dic.rds')
# Fix horizons to be k2=8#############################
k2 = 8
# drop all rows with "HORIZON_NO" > K2(8)
ord_em2d = ord_em2d[ord_em2d$HORIZON_NO<=k2,]
# one-hot encoding to tag###############
ord_em2d = one_hot_encode(ord_em2d,incl = tag)
# NA to 0###################
ord_em2d[is.na(ord_em2d)]=0
#split data into a big list group by project code and site id
ord_em2d <- split(ord_em2d,list(ord_em2d$PROJECT_CODE,ord_em2d$SITE_ID),drop = T)
  #ord_em2d %>%split(.,list(.$PROJECT_CODE,.$SITE_ID),drop = T)

#write_rds(ord_em2d,'./6_ord_em2d/ord_em2d_aftSplit.rds')
# add  empty rows for those "HORIZON_NO" < K2(5)##############
# update tag list
tag = grep('ASC_ORD',names(ord_em2d[[1]]),value = T)
for ( i in 1: length(ord_em2d)) {
  n2 = nrow(ord_em2d[[i]])
  if (n2<k2) {
    ord_em2d[[i]][(n2+1):k2,c(idx,tag_conf,sn,sc,tag)] = ord_em2d[[i]][1,c(idx,tag_conf,sn,sc,tag)]
    ord_em2d[[i]][(n2+1):k2,'HORIZON_NO'] = (n2+1):k2
    ord_em2d[[i]][(n2+1):k2,-which(names(ord_em2d[[i]]) %in% c(idx,tag_conf,sn,sc,tag))] = 0
  }
  print(i)
}
# it is time to drop 4 columns 
ord_em2d <- lapply(ord_em2d, function(x) x[!(names(x) %in% idx)])
write_rds(ord_em2d,'./00_ord/ord_em2d_aftSplit.rds')
# convert to 3D data (N x HOR X FEATURE)###############
ord_em2d = readRDS('./00_ord/ord_em2d_aftSplit.rds')
# ALL FEATURES SHOULD BE NUMERIC, OTHER DATA TYPE WILL BE CHANGED TO FACTORS
ord_em2d <- abind(ord_em2d,along = 0)
# for (i in 1:dim(ord_em2d_0)[1]){#dimnames(ord_em2d_0)[[3]][-1]){
#   ord_em2d_0[i,,] = as.numeric(as.character(ord_em2d_0[i,,]))
# }

#rename dim(2)
#dimnames(ord_em2d)[[1]] <- 1:dim(ord_em2d)[1]
dimnames(ord_em2d)[[2]] <- 1:8
# split training, test, and predict datasets
ord_em2d_tr = ord_em2d[ord_em2d[,1,'ASC_CONFIDENCE']!=1 & ord_em2d[,1,'ASC_ORD_NA']!=1,,
                       !(dimnames(ord_em2d)[[3]] %in% c('ASC_CONFIDENCE','ASC_ORD_NA'))]
ord_em2d_ts = ord_em2d[ord_em2d[,1,'ASC_CONFIDENCE']==1 & ord_em2d[,1,'ASC_ORD_NA']!=1,,
                       !(dimnames(ord_em2d)[[3]] %in% c('ASC_CONFIDENCE','ASC_ORD_NA'))]
ord_em2d_nd = ord_em2d[ord_em2d[,1,'ASC_ORD_NA']==1,,
                       !(dimnames(ord_em2d)[[3]] %in% c('ASC_CONFIDENCE',tag))]
# write to file########################
write_rds(ord_em2d,"./00_ord/ord_em2d.rds")
write_rds(ord_em2d_tr,"./00_ord/ord_tr_em2d.rds")
write_rds(ord_em2d_ts,"./00_ord/ord_ts_em2d.rds")
write_rds(ord_em2d_nd,"./00_ord/ord_nd_em2d.rds")

ord_em2d_tr_re = resampling_2d(ord_em2d_tr,"ASC_ORD")
write_rds(ord_em2d_tr_re,"./00_ord/ord_tr_em2d_re.rds")
