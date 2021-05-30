# DT MODEL
library(rpart)
library(C50)
library(rpart.plot)
library (magrittr)
library(dplyr)
library(caret)
library(RWeka)
library(readr)
#asc_tree1 = read.csv('./0_general/asc_train_split.csv',stringsAsFactors=F)
asc_tree = readRDS('./0_general/asc_sub.rds')
asc_tree = asc_tree[,c(1:4,29:32,5:6,19,7:18,20:28)]
#summary((asc_tree))
# check "sn" attributes############################################
asc_tree$DRAINAGE = as.integer(asc_tree$DRAINAGE)

# MN rows swap to column for mn attributes ############################################
# set a hyperparameter: threshold of the numbers of horizons.
k2 = 5
# delete all rows whose HORIZON_NO > hr_max
asc_tree = asc_tree[!(asc_tree$HORIZON_NO > k2),]
# transform all row's value onto column
# input: x: a dataframe
#        c: exceptional attributes, should include sn
#        k2: threshold of the numbers of horizons.
# output: dataframe with mn attributes processing completed.
row_to_col <- function(x,c,k2 = 5) {
  y = x
  for (i in c) {
    for(hr in 1 : k2){
      y[paste(names(x[i]),paste('h',hr,sep = "_"), sep = "_")] <- ifelse(x$HORIZON_NO == hr, x[[i]], 0)
    }
    y = y[ , !(names(y) %in% c(names(x[i])))]
  }
  return (y)
}
asc_tree = row_to_col(asc_tree,12:32)
# Aggregate to profile level##################################################################
sn_sc = grep('DRAINAGE|ELEM_TYPE_CODE|STATUS|ASC_|SUBORD_|GREAT_GROUP|SUBGROUP_',colnames(asc_tree))
asc_tree_1 <- asc_tree %>%
  group_by(PROJECT_CODE, SITE_ID) %>%
  summarise(ASC_ORD=first(ASC_ORD),SUBORD_ASC_CODE=first(SUBORD_ASC_CODE),
            GREAT_GROUP_ASC_CODE=first(GREAT_GROUP_ASC_CODE),
            SUBGROUP_ASC_CODE=first(SUBGROUP_ASC_CODE),DRAINAGE = first(DRAINAGE),
            ELEM_TYPE_CODE=first(ELEM_TYPE_CODE),STATUS=first(STATUS))

#asc_tree_1 = aggregate(x=asc_tree[,sn_sc],by=list(PROJECT_CODE=asc_tree$PROJECT_CODE,
#                                                SITE_ID=asc_tree$SITE_ID),FUN = mean)

asc_tree_2 = aggregate(x=asc_tree[,-c(1:4,sn_sc)],by=list(PROJECT_CODE=asc_tree$PROJECT_CODE,
                                                      SITE_ID=asc_tree$SITE_ID),FUN = max)
asc_tree = merge(asc_tree_1,asc_tree_2,by = c('PROJECT_CODE','SITE_ID'))

write.csv(asc_tree,'./3_asc_decision_tree/asc_tree.csv',row.names = F)
write.arff(asc_tree,'./3_asc_decision_tree/dt_weka.arff')
write_rds(asc_tree,'./3_asc_decision_tree/dt.rds')

#write.csv(mlp_dat, file =paste("C:/Users/lzccn/iCloudDrive/DATA SCIENCE/DATA7703 
#Machine Learing/Home Works/mlp_split",".csv",sep = " "))


#a = rpart(ASC_ORD ~ .,method = "class", data = asc_tree[1:300,-(1:2)])
#rpart.plot(a)

asc_tree = read.csv('./3_asc_decision_tree/asc_tree.csv')

tr_rate=0.85
idx = sample(nrow(asc_tree),round(nrow(asc_tree)*tr_rate),replace = F)
asc_tree_tr= asc_tree[idx,]
asc_tree_test= asc_tree[-idx,]


ord = C5.0(x=asc_tree_tr[,-(1:6)],y =as.factor(asc_tree_tr[,3]))
so = C5.0(x=asc_tree_tr[,-(1:6)],y =as.factor(asc_tree_tr[,4]))
gg = C5.0(x=asc_tree_tr[,-(1:6)],y =as.factor(asc_tree_tr[,5]))
sg = C5.0(x=asc_tree_tr[,-(1:6)],y =as.factor(asc_tree_tr[,6]))

asc_tree_test=asc_tree_test[!asc_tree_test$HOR_SUFFIX_h_4=='yc',]
asc_tree_test=asc_tree_test[!asc_tree_test$TEXTURE_CODE_h_1=='SP',]
asc_tree_test=asc_tree_test[!asc_tree_test$HOR_SUFFIX_h_1=='a',]


p_ord = predict.C5.0(ord,asc_tree_test[,-(1:6)])
p_so = predict.C5.0(so,asc_tree_test[,-(1:6)])
p_gg = predict.C5.0(gg,asc_tree_test[,-(1:6)])
p_sg = predict.C5.0(sg,asc_tree_test[,-(1:6)])
dt_con = confusionMatrix(dt_predict,asc_tree_test[,3])
dt_con

