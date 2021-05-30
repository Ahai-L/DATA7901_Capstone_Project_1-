library(abind)
library(stringr)
library(caret)
library(keras)
# data processing###############
asc_1d_em = readRDS('./0_general/asc_1d_em.rds')
# prune <- function(x,features,s=200) {
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
# asc_1d_em = prune(mlp_matlab_sub,"ASC_ORD",500)
# asc_1d_em = prune(asc_1d_em,"SUBORD|GREAT_GROUP|SUBGROUP")

#asc_1d_em = asc_1d_em[,c(1:81,91,82,86,90,87,88,83,84,85,89,92:length(asc_1d_em))]
#asc_1d_em = asc_1d_em[,c(1:81,93,92,91,82,86,90,87,88,94,83,84,85,89,95:length(asc_1d_em))]

# split training and test sample
tr_rate=0.85
idx = sample(nrow(asc_1d_em),round(nrow(asc_1d_em)*tr_rate),replace = F)
mlp_asc_tr= asc_1d_em[idx,]
mlp_asc_test= asc_1d_em[-idx,]
# index list #########################
svn_names = grep('DRAINAGE|FTS_|UPPER_|LOWER_|HOR_PREFIX|VALUE_',colnames(asc_1d_em),value = T)
svc_names = grep('STATUS|ELEM|PEDALITY|BOUND|TEXTURE|SUBHOR|SUFFIX|MASTER|CUTAN|NATURE|SOIL',
                   colnames(asc_1d_em),value = T)
ex_names = grep('COLOUR_CLASS|MOTT_',colnames(asc_1d_em),value = T)
ord_names = grep('ASC_ORD',colnames(asc_1d_em),value = T)
so_names  = grep('SUBORD',colnames(asc_1d_em),value=T)
gg_names = grep('GREAT_GROUP',colnames(asc_1d_em),value = T)
sg_names = grep('SUBGROUP',colnames(asc_1d_em),value = T)
tag_names = c(ord_names,so_names,gg_names,sg_names)




# data and tag ####################

tr_in_list = list(); test_in_list = list();j=1

for (i in svc_names) {
  tr_in_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_asc_tr[,i]))
  test_in_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_asc_test[,i]))
  j= j+1
}
tr_sn_vn = as.matrix(mlp_asc_tr[,svn_names])
tr_in_list[[53]] = tr_sn_vn
test_sn_vn = as.matrix(mlp_asc_test[,svn_names])
test_in_list[[53]] = test_sn_vn
# so_ex input list
tr_so_ex_in_list = list(); test_so_ex_in_list = list(); j = 1
for (i in ex_names) {
  tr_so_ex_in_list[[j]] = assign(paste(i,'tr',sep = "_"),as.matrix(mlp_asc_tr[,i]))
  test_so_ex_in_list[[j]] = assign(paste(i,'test',sep = "_"),as.matrix(mlp_asc_test[,i]))
  j= j+1
}
mlp_asc_tag_tr = mlp_asc_tr[,ord_names]
mlp_asc_tag_test = mlp_asc_test[,ord_names]

# model construction####################
list_base_in = list(); k = 1
for (j in svc_names) {
  list_base_in = append(list_base_in, assign(paste(j,'base_in',sep = '_'),layer_input(shape(c(1)),name = paste(j,'base_in',sep = '_'))))
  k = k+1
}
list_ex_in = list(); k = 1
for (j in ex_names) {
  list_ex_in = append(list_ex_in, assign(paste(j,'ex_in',sep = '_'),layer_input(shape(c(1)),name = paste(j,'ex_in',sep = '_'))))
  k = k+1
}
svn_in = layer_input(shape(c(length(svn_names))),name = paste(j,'base_in',sep = '_'))
list_in = c(list_base_in,svn_in)
list_em = list_base_in
em_names = svc_names
extra_in_list = data.frame('INDEX'=c((length(ord_names)+1)),'EXTRA_IN'=c(length(ex_names)))
j=1; list_out = list();lists_em_layer = rep(list(list()), length(tag_names));list_con_layer = list();
for (i in tag_names[1:14]) {
  if(j %in% extra_in_list$INDEX ){
    list_in = c(list_in ,list_ex_in)
    list_em = c(list_em,list_ex_in)
    em_names = c(em_names,ex_names)
  }
  l = 1;
  for (k in em_names) {
    vocabrulary = length(unique(asc_1d_em[[k]]))
    size = min(50,(round(0.5*vocabrulary)+1))
    lists_em_layer[[j]] = append(lists_em_layer[[j]],assign(paste(i,k,'1d',sep = '_'),
                                                            assign(paste(i,k,'em',sep='_'),list_em[[l]] %>%
                                                                     layer_embedding(input_dim = vocabrulary+1,
                                                                                     output_dim = size,
                                                                                     mask_zero = T,
                                                                                     name = paste(i,k,'em',sep='_'))) %>%
                                                              layer_reshape(c(size),name = paste(i,k,'1d',sep = '_')))) 
    
    l = l+1
  }
  list_con_layer = c(list_con_layer,assign(paste(i,'con',sep = '_'),
                                           layer_concatenate(c(lists_em_layer[[j]],svn_in),name = paste(i,'con',sep = '_'))
                                           ))
  list_out <- c(list_out, assign(paste(i,'out',sep = '_'),list_con_layer[[j]] %>%  
                                   layer_dense(units = 64,activation = 'relu',name = paste(i,'dense',sep = '_')) %>%
                                   layer_dense(units = 1,activation = 'sigmoid',name = paste(i,'out',sep = '_'))))
  j = j+1
} 


model <- keras_model(list_in,list_out)

model %>% compile(
  optimizer=optimizer_rmsprop(),
  #loss = loss_binary_crossentropy,
  loss = rep("binary_crossentropy",length(tag_names)),
  metrics = metric_binary_accuracy
)
model
model %>% save_model_hdf5('./1_asc_mlp/em_binary_1d_InOneModel.h5')
#model = load_model_hdf5('./1_asc_mlp/em_binare_1d_InOneModel.h5')
# fitting################
tr_in_list = c(tr_in_list,tr_so_ex_in_list)
tr_out_list = c(asplit(mlp_asc_tag_tr,2))
for (i in 1:length(tr_out_list)){tr_out_list[[i]]=as.vector(tr_out_list[[i]])}
names(tr_out_list)=NULL
history <- model %>% fit(tr_in_list,
                         tr_out_list,
                         epochs=10,
                         batch_size=256,
                         view_metrics=F,
                         validation_split=0.15)

# predict######################
test_in_list = list(as.matrix(mlp_asc_in_test),as.matrix(mlp_soex_test))
test_out_list = c(asplit(mlp_asc_tag_test,2),asplit(mlp_so_tag_test,2),
                  asplit(mlp_gg_tag_test,2),asplit(mlp_sg_tag_test,2))
for (i in 1:length(test_out_list)){test_out_list[[i]]=as.vector(test_out_list[[1]])}
names(test_out_list)=NULL

result = model %>% evaluate(test_in_list,test_out_list)
asc_predict <- model %>% predict(test_in_list)

# confusion matrix
# single level acc#################
# ord###################
for (i in 1:length(mlp_asc_tag_test)) {
  colnames(asc_predict[[i]]) = colnames(mlp_asc_tag_test[i])
  rownames(asc_predict[[i]]) = rownames(mlp_asc_tag_test[i])
}
ord_predict = as.data.frame(abind(asc_predict[1:length(mlp_asc_tag_test)],along=2))
#ord_predict[ord_predict<0.1]=0
ord_predict = cbind(colnames(ord_predict)[apply(ord_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                    colnames(ord_predict)[apply(ord_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                                 match(sort(x[x>0],decreasing = T)[1],x),
                                                                                 match(sort(x[x>0],decreasing = T)[2],x)
                                                ))],
                     colnames(mlp_asc_tag_test)[apply(mlp_asc_tag_test,1,which.max)],
                     ord_predict)

colnames(ord_predict)[1:3]=c('ORD_PRE1','ORD_PRE2','ORD_REAL')
ord_predict$ORD_PRE_TOP2 = ord_predict$ORD_PRE1
ord_predict[as.character(ord_predict$ORD_PRE2)==as.character(ord_predict$ORD_REAL),'ORD_PRE_TOP2'] =
      ord_predict[as.character(ord_predict$ORD_PRE2)==as.character(ord_predict$ORD_REAL),2]
ord_predict=ord_predict[,c(1,2,length(ord_predict),3)]
#
ord_con=confusionMatrix(ord_predict[,1],ord_predict[,4])
ord_con_top2 = confusionMatrix(ord_predict[,3],ord_predict[,4])
ord_con$overall
ord_con_top2$overall
#so  #################
for (i in 1:length(mlp_so_tag_test)) {
  colnames(asc_predict[[i+length(mlp_asc_tag_test)]]) = colnames(mlp_so_tag_test[i])
  rownames(asc_predict[[i+length(mlp_asc_tag_test)]]) = rownames(mlp_so_tag_test[i])
}
so_predict = as.data.frame(abind(asc_predict[(length(mlp_asc_tag_test)+1):
                                               (length(mlp_asc_tag_test)+length(mlp_so_tag_test))],along=2))
so_predict = cbind(colnames(so_predict)[apply(so_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                    colnames(so_predict)[apply(so_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                                 match(sort(x[x>0],decreasing = T)[1],x),
                                                                                 match(sort(x[x>0],decreasing = T)[2],x)
                    ))],
                    colnames(mlp_so_tag_test)[apply(mlp_so_tag_test,1,which.max)],
                    so_predict)

colnames(so_predict)[1:3]=c('SO_PRE1','SO_PRE2','SO_REAL')
so_predict$SO_PRE_TOP2 = so_predict$SO_PRE1
so_predict[as.character(so_predict$SO_PRE2)==as.character(so_predict$SO_REAL),'SO_PRE_TOP2'] =
so_predict[as.character(so_predict$SO_PRE2)==as.character(so_predict$SO_REAL),2]
so_predict=so_predict[,c(1,2,length(so_predict),3)]
# single matching
so_single= so_predict[,c(1,4)]
so_single = rbind(so_single,
             data.frame('SO_PRE1'=rep(NA,
                                   length(so_single[which(is.na(match(so_single$SO_PRE1,so_single$SO_REAL))),'SO_PRE1'])),
                        'SO_REAL'=so_single[which(is.na(match(so_single$SO_PRE1,so_single$SO_REAL))),'SO_PRE1']))
so_single = rbind(so_single,
             data.frame('SO_PRE1'=so_single[which(is.na(match(so_single$SO_REAL,so_single$SO_PRE1))),'SO_REAL'],
                        'SO_REAL'=rep(NA,length(so_single[which(is.na(match(so_single$SO_REAL,so_single$SO_PRE1))),'SO_REAL']))
             ))

so_con_s=confusionMatrix(so_single[,1],so_single[,2])
so_con_s$overall
# top_2
so_top2= so_predict[,c(3,4)]
so_top2 = rbind(so_top2,
                  data.frame('SO_PRE_TOP2'=rep(NA,
                                           length(so_top2[which(is.na(match(so_top2$SO_PRE_TOP2,so_top2$SO_REAL))),'SO_PRE_TOP2'])),
                             'SO_REAL'=so_top2[which(is.na(match(so_top2$SO_PRE_TOP2,so_top2$SO_REAL))),'SO_PRE_TOP2']))
so_top2 = rbind(so_top2,
                  data.frame('SO_PRE_TOP2'=so_top2[which(is.na(match(so_top2$SO_REAL,so_top2$SO_PRE_TOP2))),'SO_REAL'],
                             'SO_REAL'=rep(NA,length(so_top2[which(is.na(match(so_top2$SO_REAL,so_top2$SO_PRE_TOP2))),'SO_REAL']))
                  ))

so_con_top2=confusionMatrix(so_top2[,1],so_top2[,2])
so_con_top2$overall

#gg#################
for (i in 1:length(mlp_gg_tag_test)) {
  colnames(asc_predict[[i+length(mlp_asc_tag_test)+length(mlp_so_tag_test)]]) = colnames(mlp_gg_tag_test[i])
  rownames(asc_predict[[i+length(mlp_asc_tag_test)+length(mlp_so_tag_test)]]) = rownames(mlp_gg_tag_test[i])
}
gg_predict = as.data.frame(abind(asc_predict[(length(mlp_asc_tag_test)+length(mlp_so_tag_test)+1):
                                               (length(mlp_asc_tag_test)+length(mlp_gg_tag_test)+
                                                  length(mlp_so_tag_test))],along=2))
gg_predict = cbind(colnames(gg_predict)[apply(gg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(gg_predict)[apply(gg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(mlp_gg_tag_test)[apply(mlp_gg_tag_test,1,which.max)],
                   gg_predict)

#gg_predict[gg_predict<0.1]=0
# 
# gg_predict = cbind(colnames(gg_predict)[apply(gg_predict,1,which.max)],
#                    colnames(mlp_gg_tag_test)[apply(mlp_gg_tag_test,1,which.max)], 
#                    gg_predict)
# 
colnames(gg_predict)[1:3]=c('GG_PRE1','GG_PRE2','GG_REAL')
gg_predict$GG_PRE_TOP2 = gg_predict$GG_PRE1
gg_predict[as.character(gg_predict$GG_PRE2)==as.character(gg_predict$GG_REAL),'GG_PRE_TOP2'] =
  gg_predict[as.character(gg_predict$GG_PRE2)==as.character(gg_predict$GG_REAL),2]
gg_predict=gg_predict[,c(1,2,length(gg_predict),3)]
# single matching
gg_single= gg_predict[,c(1,4)]
gg_single = rbind(gg_single,
                  data.frame('GG_PRE1'=rep(NA,
                                           length(gg_single[which(is.na(match(gg_single$GG_PRE1,gg_single$GG_REAL))),'GG_PRE1'])),
                             'GG_REAL'=gg_single[which(is.na(match(gg_single$GG_PRE1,gg_single$GG_REAL))),'GG_PRE1']))
gg_single = rbind(gg_single,
                  data.frame('GG_PRE1'=gg_single[which(is.na(match(gg_single$GG_REAL,gg_single$GG_PRE1))),'GG_REAL'],
                             'GG_REAL'=rep(NA,length(gg_single[which(is.na(match(gg_single$GG_REAL,gg_single$GG_PRE1))),'GG_REAL']))
                  ))

gg_con_s=confusionMatrix(gg_single[,1],gg_single[,2])
gg_con_s$overall
# top_2
gg_top2= gg_predict[,c(3,4)]
gg_top2 = rbind(gg_top2,
                data.frame('GG_PRE_TOP2'=rep(NA,
                                             length(gg_top2[which(is.na(match(gg_top2$GG_PRE_TOP2,gg_top2$GG_REAL))),'GG_PRE_TOP2'])),
                           'GG_REAL'=gg_top2[which(is.na(match(gg_top2$GG_PRE_TOP2,gg_top2$GG_REAL))),'GG_PRE_TOP2']))
gg_top2 = rbind(gg_top2,
                data.frame('GG_PRE_TOP2'=gg_top2[which(is.na(match(gg_top2$GG_REAL,gg_top2$GG_PRE_TOP2))),'GG_REAL'],
                           'GG_REAL'=rep(NA,length(gg_top2[which(is.na(match(gg_top2$GG_REAL,gg_top2$GG_PRE_TOP2))),'GG_REAL']))
                ))

gg_con_top2=confusionMatrix(gg_top2[,1],gg_top2[,2])
gg_con_top2$overall
#SG######
for (i in 1:length(mlp_sg_tag_test)) {
  colnames(asc_predict[[i+length(mlp_asc_tag_test)+length(mlp_so_tag_test)+length(mlp_gg_tag_test)]]) = colnames(mlp_sg_tag_test[i])
  rownames(asc_predict[[i+length(mlp_asc_tag_test)+length(mlp_so_tag_test)+length(mlp_gg_tag_test)]]) = rownames(mlp_sg_tag_test[i])
}
sg_predict = as.data.frame(abind(asc_predict[(length(mlp_asc_tag_test)+length(mlp_so_tag_test)+length(mlp_gg_tag_test)+1):
                                               (length(mlp_asc_tag_test)+length(mlp_gg_tag_test)+
                                                  length(mlp_so_tag_test)+length(mlp_sg_tag_test))],along=2))
sg_predict = cbind(colnames(sg_predict)[apply(sg_predict,1,function(x) match(sort(x[x>0],decreasing = T)[1],x))],
                   colnames(sg_predict)[apply(sg_predict,1,function(x) ifelse(is.na(match(sort(x[x>0],decreasing = T)[2],x)),
                                                                              match(sort(x[x>0],decreasing = T)[1],x),
                                                                              match(sort(x[x>0],decreasing = T)[2],x)
                   ))],
                   colnames(mlp_sg_tag_test)[apply(mlp_sg_tag_test,1,which.max)],
                   sg_predict)

#sg_predict[sg_predict<0.1]=0

# sg_predict = cbind(colnames(sg_predict)[apply(sg_predict,1,which.max)],
#                    colnames(mlp_sg_tag_test)[apply(mlp_sg_tag_test,1,which.max)], 
#                    sg_predict)
colnames(sg_predict)[1:3]=c('SG_PRE1','SG_PRE2','SG_REAL')
sg_predict$SG_PRE_TOP2 = sg_predict$SG_PRE1
sg_predict[as.character(sg_predict$SG_PRE2)==as.character(sg_predict$SG_REAL),'SG_PRE_TOP2'] =
  sg_predict[as.character(sg_predict$SG_PRE2)==as.character(sg_predict$SG_REAL),2]
sg_predict=sg_predict[,c(1,2,length(sg_predict),3)]
# single matching
sg_single= sg_predict[,c(1,4)]
sg_single = rbind(sg_single,
                  data.frame('SG_PRE1'=rep(NA,
                                           length(sg_single[which(is.na(match(sg_single$SG_PRE1,sg_single$SG_REAL))),'SG_PRE1'])),
                             'SG_REAL'=sg_single[which(is.na(match(sg_single$SG_PRE1,sg_single$SG_REAL))),'SG_PRE1']))
sg_single = rbind(sg_single,
                  data.frame('SG_PRE1'=sg_single[which(is.na(match(sg_single$SG_REAL,sg_single$SG_PRE1))),'SG_REAL'],
                             'SG_REAL'=rep(NA,length(sg_single[which(is.na(match(sg_single$SG_REAL,sg_single$SG_PRE1))),'SG_REAL']))
                  ))

sg_con_s=confusionMatrix(sg_single[,1],sg_single[,2])
sg_con_s$overall
# top_2
sg_top2= sg_predict[,c(3,4)]
sg_top2 = rbind(sg_top2,
                data.frame('SG_PRE_TOP2'=rep(NA,
                                             length(sg_top2[which(is.na(match(sg_top2$SG_PRE_TOP2,sg_top2$SG_REAL))),'SG_PRE_TOP2'])),
                           'SG_REAL'=sg_top2[which(is.na(match(sg_top2$SG_PRE_TOP2,sg_top2$SG_REAL))),'SG_PRE_TOP2']))
sg_top2 = rbind(sg_top2,
                data.frame('SG_PRE_TOP2'=sg_top2[which(is.na(match(sg_top2$SG_REAL,sg_top2$SG_PRE_TOP2))),'SG_REAL'],
                           'SG_REAL'=rep(NA,length(sg_top2[which(is.na(match(sg_top2$SG_REAL,sg_top2$SG_PRE_TOP2))),'SG_REAL']))
                ))

sg_con_top2=confusionMatrix(sg_top2[,1],sg_top2[,2])
sg_con_top2$overall

# Hierarchical acc#################
asc_predict = data.frame(ord_predict$ORD_PRE1,ord_predict$ORD_PRE_TOP2,ord_predict$ORD_REAL,
                         so_predict$SO_PRE1,so_predict$SO_PRE_TOP2,so_predict$SO_REAL,
                         gg_predict$GG_PRE1,gg_predict$GG_PRE_TOP2,gg_predict$GG_REAL,
                         sg_predict$SG_PRE1,sg_predict$SG_PRE_TOP2,sg_predict$SG_REAL)

asc_predict$P_SG = paste(str_sub(asc_predict[[1]],-2,-1),
                         str_sub(asc_predict[[4]],-2,-1),
                         str_sub(asc_predict[[7]],-2,-1),
                         str_sub(asc_predict[[10]],-2,-1),
                         sep='_')
asc_predict$P_SG_TOP2 = paste(str_sub(asc_predict[[2]],-2,-1),
                              str_sub(asc_predict[[5]],-2,-1),
                              str_sub(asc_predict[[8]],-2,-1),
                              str_sub(asc_predict[[11]],-2,-1),
                              sep='_')

asc_predict$R_SG = paste(str_sub(asc_predict[[3]],-2,-1),
                         str_sub(asc_predict[[6]],-2,-1),
                         str_sub(asc_predict[[9]],-2,-1),
                         str_sub(asc_predict[[12]],-2,-1),
                         sep='_')
asc_predict$P_GG = paste(str_sub(asc_predict[[1]],-2,-1),
                         str_sub(asc_predict[[4]],-2,-1),
                         str_sub(asc_predict[[7]],-2,-1),
                         sep='_')
asc_predict$P_GG_TOP2 = paste(str_sub(asc_predict[[2]],-2,-1),
                              str_sub(asc_predict[[5]],-2,-1),
                              str_sub(asc_predict[[8]],-2,-1),
                              sep='_')
asc_predict$R_GG = paste(str_sub(asc_predict[[3]],-2,-1),
                         str_sub(asc_predict[[6]],-2,-1),
                         str_sub(asc_predict[[9]],-2,-1),
                         sep='_')
asc_predict$P_SO = paste(str_sub(asc_predict[[1]],-2,-1),
                         str_sub(asc_predict[[4]],-2,-1),
                         sep='_')
asc_predict$P_SO_TOP2 = paste(str_sub(asc_predict[[2]],-2,-1),
                              str_sub(asc_predict[[5]],-2,-1),
                              sep='_')
asc_predict$R_SO = paste(str_sub(asc_predict[[3]],-2,-1),
                         str_sub(asc_predict[[6]],-2,-1),
                         sep='_')
asc_predict$P_ORD = paste(str_sub(asc_predict[[1]],-2,-1),
                         sep='_')
asc_predict$P_ORD_TOP2 = paste(str_sub(asc_predict[[2]],-2,-1),
                              sep='_')
asc_predict$R_ORD = paste(str_sub(asc_predict[[3]],-2,-1),
                         sep='_')

asc_p = asc_predict[,13:24]
# custom_confusion <- function(x) {
#   x = rbind(x,
#                data.frame(x[[1]]=rep(NA,
#                                      length(x[which(is.na(match(x[[1]],x[[2]]))),x[[1]]])),
#                           x[[2]]=x[which(is.na(match(x[[1]],x[[2]]))),x[[1]]]))
#   x = rbind(x,
#                data.frame(x[[1]]=x[which(is.na(match(x[[2]],x[[1]]))),x[[2]]],
#                           x[[2]]=rep(NA,length(x[which(is.na(match(x[[2]],x[[1]]))),x[[2]]]))
#                ))
#   return(confusionMatrix(factor(x[,1]),factor(x[[2]])))
# }
#SG SINGLE##################
sg_p = asc_p[,c(1,3)]
sg_p = rbind(sg_p,
             data.frame('P_SG'=rep(NA,
                                   length(sg_p[which(is.na(match(sg_p$P_SG,sg_p$R_SG))),'P_SG'])),
                        'R_SG'=sg_p[which(is.na(match(sg_p$P_SG,sg_p$R_SG))),'P_SG']))
sg_p = rbind(sg_p,
             data.frame('P_SG'=sg_p[which(is.na(match(sg_p$R_SG,sg_p$P_SG))),'R_SG'],
                        'R_SG'=rep(NA,length(sg_p[which(is.na(match(sg_p$R_SG,sg_p$P_SG))),'R_SG']))
             ))
sg_confusion=confusionMatrix(factor(sg_p$P_SG),factor(sg_p$R_SG))
#GG SINGLE##################
gg_p = asc_p[,c(4,6)]
gg_p = rbind(gg_p,
             data.frame('P_GG'=rep(NA,
                                   length(gg_p[which(is.na(match(gg_p$P_GG,gg_p$R_GG))),'P_GG'])),
                        'R_GG'=gg_p[which(is.na(match(gg_p$P_GG,gg_p$R_GG))),'P_GG']))
gg_p = rbind(gg_p,
             data.frame('P_GG'=gg_p[which(is.na(match(gg_p$R_GG,gg_p$P_GG))),'R_GG'],
                        'R_GG'=rep(NA,length(gg_p[which(is.na(match(gg_p$R_GG,gg_p$P_GG))),'R_GG']))
             ))
gg_confusion=confusionMatrix(factor(gg_p$P_GG),factor(gg_p$R_GG))
#SO SINGLE##################
so_p = asc_p[,c(7,9)]
so_p = rbind(so_p,
             data.frame('P_SO'=rep(NA,
                                   length(so_p[which(is.na(match(so_p$P_SO,so_p$R_SO))),'P_SO'])),
                        'R_SO'=so_p[which(is.na(match(so_p$P_SO,so_p$R_SO))),'P_SO']))
so_p = rbind(so_p,
             data.frame('P_SO'=so_p[which(is.na(match(so_p$R_SO,so_p$P_SO))),'R_SO'],
                        'R_SO'=rep(NA,length(so_p[which(is.na(match(so_p$R_SO,so_p$P_SO))),'R_SO']))
             ))
so_confusion=confusionMatrix(factor(so_p$P_SO),factor(so_p$R_SO))
#ORD SINGLE##################
ord_p = asc_p[,c(10,12)]
ord_p = rbind(ord_p,
             data.frame('P_ORD'=rep(NA,
                                   length(ord_p[which(is.na(match(ord_p$P_ORD,ord_p$R_ORD))),'P_ORD'])),
                        'R_ORD'=ord_p[which(is.na(match(ord_p$P_ORD,ord_p$R_ORD))),'P_ORD']))
ord_p = rbind(ord_p,
             data.frame('P_ORD'=ord_p[which(is.na(match(ord_p$R_ORD,ord_p$P_ORD))),'R_ORD'],
                        'R_ORD'=rep(NA,length(ord_p[which(is.na(match(ord_p$R_ORD,ord_p$P_ORD))),'R_ORD']))
             ))
ord_confusion=confusionMatrix(factor(ord_p$P_ORD),factor(ord_p$R_ORD))

# SG TPO2###################
sg_top2 = asc_p[,c(2,3)]
sg_top2 = rbind(sg_top2,
             data.frame('P_SG_TOP2'=rep(NA,
                                   length(sg_top2[which(is.na(match(sg_top2$P_SG_TOP2,sg_top2$R_SG))),'P_SG_TOP2'])),
                        'R_SG'=sg_top2[which(is.na(match(sg_top2$P_SG_TOP2,sg_top2$R_SG))),'P_SG_TOP2']))
sg_top2 = rbind(sg_top2,
             data.frame('P_SG_TOP2'=sg_top2[which(is.na(match(sg_top2$R_SG,sg_top2$P_SG_TOP2))),'R_SG'],
                        'R_SG'=rep(NA,length(sg_top2[which(is.na(match(sg_top2$R_SG,sg_top2$P_SG_TOP2))),'R_SG']))
             ))
sg_confusion_top2=confusionMatrix(factor(sg_top2$P_SG_TOP2),factor(sg_top2$R_SG))
# GG TPO2###################
gg_top2 = asc_p[,c(5,6)]
gg_top2 = rbind(gg_top2,
                data.frame('P_GG_TOP2'=rep(NA,
                                           length(gg_top2[which(is.na(match(gg_top2$P_GG_TOP2,gg_top2$R_GG))),'P_GG_TOP2'])),
                           'R_GG'=gg_top2[which(is.na(match(gg_top2$P_GG_TOP2,gg_top2$R_GG))),'P_GG_TOP2']))
gg_top2 = rbind(gg_top2,
                data.frame('P_GG_TOP2'=gg_top2[which(is.na(match(gg_top2$R_GG,gg_top2$P_GG_TOP2))),'R_GG'],
                           'R_GG'=rep(NA,length(gg_top2[which(is.na(match(gg_top2$R_GG,gg_top2$P_GG_TOP2))),'R_GG']))
                ))
gg_confusion_top2=confusionMatrix(factor(gg_top2$P_GG_TOP2),factor(gg_top2$R_GG))

# SO TPO2###################
so_top2 = asc_p[,c(8,9)]
so_top2 = rbind(so_top2,
                data.frame('P_SO_TOP2'=rep(NA,
                                           length(so_top2[which(is.na(match(so_top2$P_SO_TOP2,so_top2$R_SO))),'P_SO_TOP2'])),
                           'R_SO'=so_top2[which(is.na(match(so_top2$P_SO_TOP2,so_top2$R_SO))),'P_SO_TOP2']))
so_top2 = rbind(so_top2,
                data.frame('P_SO_TOP2'=so_top2[which(is.na(match(so_top2$R_SO,so_top2$P_SO_TOP2))),'R_SO'],
                           'R_SO'=rep(NA,length(so_top2[which(is.na(match(so_top2$R_SO,so_top2$P_SO_TOP2))),'R_SO']))
                ))
so_confusion_top2=confusionMatrix(factor(so_top2$P_SO_TOP2),factor(so_top2$R_SO))


# ORD TPO2###################
ord_top2 = asc_p[,c(11,12)]
ord_top2 = rbind(ord_top2,
                data.frame('P_ORD_TOP2'=rep(NA,
                                           length(ord_top2[which(is.na(match(ord_top2$P_ORD_TOP2,ord_top2$R_ORD))),'P_ORD_TOP2'])),
                           'R_ORD'=ord_top2[which(is.na(match(ord_top2$P_ORD_TOP2,ord_top2$R_ORD))),'P_ORD_TOP2']))
ord_top2 = rbind(ord_top2,
                data.frame('P_ORD_TOP2'=ord_top2[which(is.na(match(ord_top2$R_ORD,ord_top2$P_ORD_TOP2))),'R_ORD'],
                           'R_ORD'=rep(NA,length(ord_top2[which(is.na(match(ord_top2$R_ORD,ord_top2$P_ORD_TOP2))),'R_ORD']))
                ))
ord_confusion_top2=confusionMatrix(factor(ord_top2$P_ORD_TOP2),factor(ord_top2$R_ORD))

#result ##############
ord_confusion$overall
ord_confusion_top2$overall
so_confusion$overall
so_confusion_top2$overall
gg_confusion$overall
gg_confusion_top2$overall
sg_confusion$overall
sg_confusion_top2$overall

# one by one#####################
ord_an = data.frame('real'= mlp_asc_tag_test[,1], 'predict'= asc_predict_original[[1]])
ord_an$predict[ord_an$predict >0.1] = 1
ord_an$predict[ord_an$predict != 1] = 0
ord_an_con = confusionMatrix(as.factor(ord_an$predict),as.factor(ord_an$real))
ord_an_con
