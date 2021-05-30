library(RWeka)
# start with a horizon level dataset ###############################
mlp_weka = read.csv('./0_general/asc_train_split.csv',stringsAsFactors=F) #asc_train_split

# check "sn" attributes############################################
mlp_weka$DRAINAGE = as.integer(mlp_weka$DRAINAGE)

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

mlp_weka$SOIL_WATER_STAT = ex_ordinal(mlp_weka$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
mlp_weka$BOUND_DISTINCT = ex_ordinal(mlp_weka$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
mlp_weka$PEDALITY_GRADE = ex_ordinal(mlp_weka$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))

# MN rows swap to column for mn attributes ############################################
# set a hyperparameter: threshold of the numbers of horizons.

hr_max = 5
# delete all rows whose HORIZON_NO > hr_max
mlp_weka = mlp_weka[!(mlp_weka$HORIZON_NO > hr_max),]
# transform all row's value onto column
# input: x: a dataframe
#        c: exceptional attributes, should include sn,sc
#        hr_max: threshold of the numbers of horizons.
# output: dataframe with mn attributes processing completed.
mnmc_row_to_col <- function(x,c,hr_max = 5) {
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
mlp_weka = mnmc_row_to_col(mlp_weka,c(1:7,27),hr_max)
write.csv(mlp_weka,'./1_asc_mlp/mlp_weka_bfAggregate.csv')
# Aggregate to profile level##################################################################
# aggregate to profile level for weka
aggregate_to_profile_weka <- function(x) {
  sn_sc = grep('DRAINAGE|ELEM_TYPE_CODE|STATUS|ASC_',colnames(x))
  x_1 <- x %>%
    group_by(PROJECT_CODE, SITE_ID) %>%
    summarise(ASC_ORD=first(ASC_ORD),DRAINAGE = first(DRAINAGE),
              ELEM_TYPE_CODE=first(ELEM_TYPE_CODE),STATUS=first(STATUS))
  x_2 = aggregate(x=x[,-c(1:4,sn_sc)],by=list(PROJECT_CODE=x$PROJECT_CODE,
                                              SITE_ID=x$SITE_ID),FUN = max)
  return(merge(x_1,x_2,by = c('PROJECT_CODE','SITE_ID')))
}

mlp_weka = aggregate_to_profile_weka(mlp_weka)
#write.csv(mlp_weka,'./1_asc_mlp/asc_mlp_weka.csv')
write.arff(mlp_weka,'./1_asc_mlp/asc_mlp_weka.arff')

