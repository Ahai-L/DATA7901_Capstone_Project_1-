# SPC CLUSTERING MODEL
# Library##############################
library(plyr)
library(plotly)
library(readr)
library(dplyr)
library(analogue)
# read data from 'spc_onehot_dat'
spc_onehot_2 <- read.csv('./7_spc_clustering/spc_onehot_2.csv',stringsAsFactors=F)
a = spc_onehot_2
# delete spcs less then 10 instances
spc_count = spc_onehot_2 %>% count(spc_onehot_2$SPC) # count in dplyr, not plyr

  spc_onehot_2 <-left_join(spc_onehot_2,spc_count,by=c('SPC'='spc_onehot_2$SPC'))
  spc_onehot_2 = spc_onehot_2[,c(1:3,length(spc_onehot_2),4:(length(spc_onehot_2)-1))]
  spc_onehot_2 = spc_onehot_2[spc_onehot_2$n>10,]
  spc_onehot_2 = spc_onehot_2[,-4]
# sim for two onehot data######################
spc_sim <- function(d1,d2) {
  sn = c('REL_MOD_SLOPE_CLASS')#,'PEDALITY_GRADE','FTS_PH')
  sc_mc = c('STATUS','OBS_LITH_CODE','VEG_SPEC_CODE','HOR_MASTER','COLOUR_CLASS',
            'TEXTURE_CODE','NATURE','OCF_LITH_CODE','CUTAN_TYPE','BOUND_DISTINCT')
  mn = c('PEDALITY_GRADE','FTS_PH','MOTT_TYPE')
  # mc = c('HOR_MASTER','COLOUR_CLASS','MOTT_TYPE','TEXTURE_CODE','NATURE',
  #        'OCF_LITH_CODE','CUTAN_TYPE','BOUND_DISTINCT')
  sim = 0; n = 0
  for (id in sn) {
    if (!d1[id]==0 & !d2[id]==0) {
      sim = sim + exp(-(d1[[id]]-d2[[id]])^2/2)
      n = n+1
    }
  }
  for (id in mn) {
    # # imputation
    idx = grep(id,colnames(d1))
    if (!sum(d1[idx])==0 & !sum(d2[idx])==0 ) { #d1,d2 not all 0
      # if (sum(d1[idx]!=0)==1) { # if d1 only has one non zero value, fill others with this value
      #   d1[idx][,which(d1[idx]==0)] = d1[idx][,which(d1[idx]!=0)]
      # } else {
      #   for (i in 2 : (length(idx)-1)) { # fill one missing value between two non zero values
      #     if (d1[min(idx)+i-2]!=0 & d1[min(idx)+i]!=0 & d1[min(idx)+i-1]==0) {
      #       d1[min(idx)+i-1] = (d1[min(idx)+i-2] + d1[min(idx)+i])/2
      #     }
      #   }
      #   id1=1;id2=length(idx);a1=a2=0
      #   for (i in 1 : length(idx)-3) { # fill multiple miss values between two nonzero values
      #     if(d1[min(idx)+i-1]!=0 & d1[min(idx)+i]==0 & d1[min(idx)+i+1]==0 & a1 ==0) {
      #       a1 = d1[min(idx)+i-1]
      #       id1 = i
      #     }
      #     if(i>3 & d1[min(idx)+i-1]!=0 & d1[min(idx)+i-2]==0 & d1[min(idx)+i-3]==0 & i>id1 & a2==0) {
      #       a2 = d1[min(idx)+i-1]
      #       id2 = i
      #     }
      #   }
      #   if ( a1!=0 & a2!=0) {
      #     for (j in id1:(id2-1)) {
      #       d1[min(idx)+j]=d1[min(idx)+id1-1]+(d1[min(idx)+id2-1]-d1[min(idx)+id1-1])/(id2-id1)*(j-id1+1)
      #     }
      #   }
      #   for (i in 1:(length(idx)-2) ) { # backward filling
      #     if (d1[min(idx)+i-1]!=0 & d1[min(idx)+i]!=0 & d1[min(idx)+i+1] ==0 ) {
      #       d1[min(idx)+i+1] = max(0.01,2*d1[[min(idx)+i]]- d1[[min(idx)+i-1]])
      #     }
      #   }
      #   for ( i in length(idx):3) { # forward filling
      #     if (d1[min(idx)+i-1]!=0 & d1[min(idx)+i-2]!=0 & d1[min(idx)+i-3]==0) {
      #       d1[min(idx)+i-3] = max(0.01,2*d1[[min(idx)+i-2]]- d1[[min(idx)+i-1]])
      #     }
      #   }
      # }
      # if (sum(d2[idx]!=0)==1) { # if d1 only has one non zero value, fill others with this value
      #   d2[idx][,which(d2[idx]==0)] = d2[idx][,which(d2[idx]!=0)]
      # } else {
      #   for (i in 2 : (length(idx)-1)) { # fill one missing value between two non zero values
      #     if (d2[min(idx)+i-2]!=0 & d2[min(idx)+i]!=0 &d2[min(idx)+i-1]==0) {
      #       d2[min(idx)+i-1] = (d2[min(idx)+i-2] + d2[min(idx)+i])/2
      #     }
      #   }
      #   id1=1;id2=length(idx);a1=a2=0
      #   for (i in 1 : length(idx)-3) { # fill multiple miss values between two nonzero values
      #     if(d2[min(idx)+i-1]!=0 & d2[min(idx)+i]==0 & d2[min(idx)+i+1]==0 & a1 ==0) {
      #       a1 = d2[min(idx)+i-1]
      #       id1 = i
      #     }
      #     if(i>3 & d2[min(idx)+i-1]!=0 & d2[min(idx)+i-2]==0 & d2[min(idx)+i-3]==0 & i>id1 & a2==0) {
      #       a2 = d2[min(idx)+i-1]
      #       id2 = i
      #     }
      #   }
      #   if ( a1!=0 & a2!=0) {
      #     for (j in id1:(id2-1)) {
      #       d2[min(idx)+j]=d2[min(idx)+id1-1]+(d2[min(idx)+id2-1]-d2[min(idx)+id1-1])/(id2-id1)*(j-id1+1)
      #     }
      #   }
      #   for (i in 1:(length(idx)-2) ) { # backward filling
      #     if (d2[min(idx)+i-1]!=0 & d2[min(idx)+i]!=0 ) {
      #       d2[min(idx)+i+1] = max(0.01,2*d2[[min(idx)+i]]- d2[[min(idx)+i-1]])
      #     }
      #   }
      #   for ( i in length(idx):3) { #forward filling
      #     if (d2[min(idx)+i-1]!=0 & d2[min(idx)+i-2]!=0 ) {
      #       d2[min(idx)+i-3] = max(0.01,2*d2[[min(idx)+i-2]]- d2[[min(idx)+i-1]])
      #     }
      #   }
      # }
      sim = sim + sum(exp(-(d1[idx]-d2[idx])^2/2))/length(idx)
      n = n+1
      #}
    }
  }
  for (id in sc_mc) {
    idx = grep(id,colnames(d1))
    if (!sum(d1[idx])==0 & !sum(d2[idx])==0 ) {
      sim = sim + (1-sum(abs(d1[idx]/sum(d1[idx])-d2[idx]/sum(d2[idx])))/2)
      n = n+1
    }
  }
  return(sim/n)
}
# sim for two onehot centroids or btw data and centroid######################
spc_sim_cen <- function(d1,d2) {
  c = colnames(d1)
  sim = 0; n = 0
  for (id in c) {
    idx = grep(id,colnames(d1))
    if (!sum(d1[idx])==0 & !sum(d2[idx])==0 ) { #d1,d2 not all 0
      sim = sim + sum(exp(-(d1[idx]-d2[idx])^2/2))/length(idx)
      n = n+1
    }
  }
  return(sim/n)
}
matsim_cen_test = matrix(0,nrow = nrow(y),
                           ncol = nrow(y))
colnames(matsim_cen_test) = y$SPC
rownames(matsim_cen_test) =  colnames(matsim_cen_test)
start_time = Sys.time()
for ( i in 1: (nrow(y))) {
  for (j in min((i+1),nrow(y)): (nrow(y))) {
    matsim_cen_test[i,j] = spc_sim_cen(y[i,],y[j,])
    print(paste(i,j,matsim_cen_test[i,j]))
  }
}
# spc_centroid_onehot############################ 
# calculate the centroid point for a set of data
spc_centroid_onehot <- function(dat,idx=3) {
  centroid = data.frame(matrix(NA, ncol = ncol(dat)))
  colnames(centroid) = colnames(dat)
  #centroid$SPC = dat[1,]$SPC
  centroid[1,(idx+1):length(dat)] = colMeans(dat[,(idx+1):length(dat)],na.rm=T)
  #centroid[1,(idx+1):length(dat)] = colMeans(dat[,(idx+1):length(dat)]
  #                                           [sapply((idx+1):length(dat),is.numeric)],na.rm = T)
  return(centroid)
}
# get a list of all SPCs in sample data#########################
list_spc_onehot = count(spc_onehot_2, "SPC")[order(count(spc_onehot_2, "SPC")$freq,decreasing = T),]
nrow(list_spc_onehot[!list_spc_onehot$freq==1,])
nrow(list_spc_onehot)
#list_spc = unique(list_spc$SPC)
# set up an empty centroid dataframe
centroid_spc_onehot0 = data.frame(matrix( ncol = ncol(spc_onehot_2)))
colnames(centroid_spc_onehot0) = colnames(spc_onehot_2)

# calculate the centroid for every SPC ############################
for (s in list_spc_onehot$SPC ) {
  test = spc_onehot_2[(!is.na(spc_onehot_2$SPC) & spc_onehot_2$SPC == s ),]
  if (sum(is.na(centroid_spc_onehot0[1,]))/length(centroid_spc_onehot0)==1) {
    centroid_spc_onehot0 = spc_centroid_onehot(test)
  } else {centroid_spc_onehot0 = rbind(centroid_spc_onehot0,spc_centroid_onehot(test))}
  
  # centroid_spc_w = ifelse(sum(is.na(centroid_spc_w))/length(centroid_spc_w)==1,
  #                         spc_centroid(test,3,5),
  #                         rbind(centroid_spc_w,spc_centroid(test,3,5)))  
  centroid_spc_onehot0[nrow(centroid_spc_onehot0),'SPC'] = s
  print(centroid_spc_onehot0[nrow(centroid_spc_onehot0),1:5])
}
#centroid_spc_onehot = gsub("NaN", "NA", centroid_spc_onehot)
#centroid_spc_w = centroid_spc_w[!is.na(centroid_spc_w$SPC),]
write.csv(centroid_spc_onehot0,'./7_spc_clustering/centroid_spc_onehot.csv',
          row.names = F)

# sim_mat for centroid_spc##################################
# matsim_cen_onehot = matrix(0,nrow = nrow(centroid_spc_onehot[1:185,]),
#                            ncol = nrow(centroid_spc_onehot[1:185,]))
# colnames(matsim_cen_onehot) = centroid_spc_onehot[1:185,]$SPC
# rownames(matsim_cen_onehot) =  colnames(matsim_cen_onehot)
# start_time = Sys.time()
# for ( i in 1: (nrow(centroid_spc_onehot[1:185,]))) {
#   for (j in min((i+1),nrow(centroid_spc_onehot[1:185,])): (nrow(centroid_spc_onehot[1:185,]))) {
#     matsim_cen_onehot[i,j] = spc_sim(centroid_spc_onehot[1:185,][i,],centroid_spc_onehot[1:185,][j,])
#     print(paste(i,j,matsim_cen_onehot[i,j]))
#   }
# }
# end_time = Sys.time()
# end_time - start_time
# delete 0 columns
centroid_spc_onehot0=centroid_spc_onehot0[,colSums(centroid_spc_onehot0 != 0,na.rm = T) > 0]
spc_collist = c("REL_MOD_SLOPE_CLASS","STATUS","OBS_LITH_CODE", "VEG_SPEC_CODE",
                "MOTT_TYPE","PEDALITY_GRADE","FTS_PH","HOR_MASTER","COLOUR_CLASS", 
                "NATURE" ,"TEXTURE_CODE","OCF_LITH_CODE" ,"CUTAN_TYPE" , "BOUND_DISTINCT")
c.weights = c()
for (i in spc_collist) {
  print(rep(1/(length(spc_collist)* length(grep(i,colnames(centroid_spc_onehot0)))),
      length(grep(i,colnames(centroid_spc_onehot0)))))
  c.weights = c(c.weights, 
                rep(1/(length(spc_collist)* length(grep(i,colnames(centroid_spc_onehot0)))),
                    length(grep(i,colnames(centroid_spc_onehot0)))))
}
c.weights
sum(c.weights)
spc_cen_dist = distance(centroid_spc_onehot0[,-1],method='mixed',weights = c.weights)
colnames(spc_cen_dist)= centroid_spc_onehot0$SPC
rownames(spc_cen_dist)= centroid_spc_onehot0$SPC
write_rds(spc_cen_dist,'./7_spc_clustering/spc_cen_dist.rds')
#spc_cen_dist = read_rds('./7_spc_clustering/spc_cen_dist.rds')
#(spc_cen_dist,'./7_spc_clustering/spc_cen_dist.csv',row.names = F)
#a <- read.csv('./7_spc_clustering/spc_cen_dist.csv',stringsAsFactors=F,header = T)

# simmat_cen1 = simmat_cen[1:185,1:185]
# # rownames(simmat_cen1) =  colnames(simmat_cen1)
# matsim_cen_one = matsim_cen_onehot
# matsim_cen_one = matsim_cen_one+t(matsim_cen_one) 
# diag(matsim_cen_one) = 1

# hierarchical clustering##################################
spc_cen_dist = readRDS('./7_spc_clustering/spc_cen_dist.rds')
#dist_cen_one = as.dist(1-matsim_cen_one)
#typeof(dist_cen)
hc_onehot = hclust(as.dist(spc_cen_dist),method = 'average')
mem_onehot = cutree(hc_onehot,h=0.05)
#mem1 = cutree(hc,h=0.4)
#mem == mem1
cent_onehot = NULL
for (i in 1: length(table(mem_onehot))) {
  cent_onehot = rbind(cent_onehot,colMeans(spc_cen_dist[mem_onehot == i,,drop=F]))
}
hc1_onehot = hclust(dist(cent_onehot),members = table(mem_onehot),method = 'average')

opar <- par(mfrow = c(2,1))
plot(hc_onehot ,labels= colnames(spc_cen_dist), hang=-1,cex=0.4)
plot(hc1_onehot, hang=-1,cex=0.4)
#plot(hc1,hang=-1,cex = 0.6)
# par(opar)
# hc_dend = as.dendrogram(hc_onehot)
# nleaves(hc_dend)
# nnodes(hc_dend)
# 
# plot(hc_dend,leaflab = 'none')
# 
# plot(color_branches(hc_onehot,h=mem_onehot),leaflab = 'none')

#color_scheme = rev(brewer_pal(10,'RdBu'))
cluster_df_onehot <- data.frame(spc = names(mem_onehot), cluster = mem_onehot)


write_rds(cluster_df_onehot,'./7_spc_clustering/cluster_df_onehot.rds')
#heatmap.2(as.matrix(simmat_cen1[1:50,1:50]),Rowv = NULL,Colv = NULL,dendrogram = 'none',
#          trace = 'none', density.info = 'none', 
#          keysize = 1, labRow = F, labCol = F, xlab = "SPCs",
#          ylab = "SPCs")
cluster_onehot_0.1 = list()
for ( i in 1: length(table(mem_onehot))) {
  cluster_onehot_0.1[[i]] = names(mem_onehot[mem_onehot == i]) 
}

# surface plot

fig <- plot_ly(z = ~as.matrix(spc_cen_dist))
fig <- fig %>% add_surface()
fig

# Predict SPCs###########################################################
# this is for a set of dat

predict_spc_onehot <- function(dat,centroid,n=10) {
  output = data.frame()
  for (i in 1:nrow(dat)) {
    start_time = Sys.time()
    temp_output = cbind(dat[i,1:3],data.frame('ITEM'="Original Data","CLUSTER_ID"=NA,
                                              'CLUSTER_SIZE'=NA,'SPC_SIZE'=NA,'SIM_VAL'=1))
    for (j in 1:nrow(centroid)){
      temp_simval = spc_sim_cen(dat[i,],centroid[j,])
      if (nrow(temp_output)<(n+1)){ # add a record
        temp_clusterid =ifelse(centroid[j,'SPC'] %in% cluster_df_onehot$spc,
                               cluster_df_onehot[cluster_df_onehot$spc==centroid[j,'SPC'],'cluster'],
                               NA) 
        temp_clustersize = ifelse(is.na(temp_clusterid),
                                  NA,length(cluster_onehot_0.1[[temp_clusterid]]))
        temp_spcsize = nrow(spc_sample[spc_sample$SPC==centroid[j,'SPC'],])
        temp_output = rbind(temp_output,cbind(centroid[j,1:3],
                                              data.frame('ITEM'="SPC Centroid","CLUSTER_ID"=temp_clusterid,
                                                         'CLUSTER_SIZE'=temp_clustersize,
                                                         'SPC_SIZE'=temp_spcsize,'SIM_VAL'=temp_simval)))
      }
      else {# compare and replace if new record's simval bigger
        temp_output = temp_output[order(temp_output$SIM_VAL,decreasing = T),] # sort output by SIM_VAL
        if (temp_simval>temp_output[n+1,"SIM_VAL"]) {
          temp_clusterid =ifelse(centroid[j,'SPC'] %in% cluster_df_onehot$spc,
                                 cluster_df_onehot[cluster_df_onehot$spc==centroid[j,'SPC'],'cluster'],
                                 NA) 
          temp_clustersize = ifelse(is.na(temp_clusterid),
                                    NA,length(cluster_onehot_0.1[[temp_clusterid]]))
          temp_spcsize = nrow(spc_sample[spc_sample$SPC==centroid[j,'SPC'],])
          temp_output[n+1,]=cbind(centroid[j,1:3],
                                  data.frame('ITEM'="SPC Centroid","CLUSTER_ID"=temp_clusterid,
                                             'CLUSTER_SIZE'=temp_clustersize,
                                             'SPC_SIZE'=temp_spcsize,'SIM_VAL'=temp_simval))
        }
      }
      print(c(i,j,as.character(temp_output[nrow(temp_output),c(3,8)])))
    }
    temp_output[2:(n+1),c('PROJECT_CODE','SITE_ID')] = temp_output[1,c('PROJECT_CODE','SITE_ID')]
    temp_output[1,c('CLUSTER_ID','CLUSTER_SIZE','SPC','SPC_SIZE',"SIM_VAL")]=
      temp_output[2,c('CLUSTER_ID','CLUSTER_SIZE','SPC','SPC_SIZE',"SIM_VAL")]
    print(paste(i,dat[i,1],dat[i,2],'real:',dat[i,'SPC'],'predict:',temp_output[1,'SPC'],
                "sim_dat_Pcen:",round(temp_output[1,'SIM_VAL'],3)))
    # 'sim_dat_Ocen:',round(spc_sim(dat[i,],centroid[centroid$SPC==dat[i,'SPC'],]),3),
    # 'sim_cen_cen:',round(matsim_cen_onehot[dat[i,'SPC'],temp_output[1,'SPC']],3),
    # 'cluster_O:',cluster_df_onehot[cluster_df_onehot$spc==dat[i,'SPC'],'cluster'],
    # 'cluster_P:',temp_output[1,'CLUSTER_ID'],
    # 'time:',round((Sys.time()-start_time))))
    output = rbind(output,temp_output)
  }
  #output = output[,c(18,1:3,21,19,20,22,4:17)]
  return(output)
}
centroid_spc_onehot = read.csv('./7_spc_clustering/centroid_spc_onehot.csv',stringsAsFactors = F)
spc_predict_onehot = predict_spc_onehot(spc_onehot_2[9499,],centroid_spc_onehot)#[list_spc$freq>5,])
# this is for a single dat
# top 10 SPCs
predict_spc <- function(dat,centroid,e=3,k1=2) {
  output = cbind(dat,data.frame('ITEM'="Original Data","CLUSTER_ID"=NA,
                                'CLUSTER_SIZE'=NA,'SPC_SIZE'=NA,'SIM_VAL'=1))
  for (i in 1:nrow(centroid)){
    temp_clusterid =cluster_df[cluster_df$spc==centroid[i,'SPC'],'cluster']
    temp_clustersize = length(group_0.6[[temp_clusterid]])
    temp_spcsize = nrow(spc_sample[spc_sample$SPC==centroid[i,'SPC'],])
    temp_simval = sim_2data(dat,centroid[i,],e,k1)
    if (nrow(output)<11){
      output = rbind(output,cbind(centroid[i,],
                                  data.frame('ITEM'="SPC Centroid","CLUSTER_ID"=temp_clusterid,
                                             'CLUSTER_SIZE'=temp_clustersize,
                                             'SPC_SIZE'=temp_spcsize,'SIM_VAL'=temp_simval)))
      
      
      output = output[order(output$SIM_VAL,decreasing = T),] # sort output by SIM_VAL
    }
    else if(temp_simval>output[11,"SIM_VAL"]) {
      output[11,]=cbind(centroid[i,],
                        data.frame('ITEM'="SPC Centroid","CLUSTER_ID"=temp_clusterid,
                                   'CLUSTER_SIZE'=temp_clustersize,
                                   'SPC_SIZE'=temp_spcsize,'SIM_VAL'=temp_simval))
      output = output[order(output$SIM_VAL,decreasing = T),] # sort output by SIM_VAL
    }
  }
  output = output[,c(18,1:3,21,19,20,22,4:17)]
  output[1,c('CLUSTER_ID','CLUSTER_SIZE','SPC','SPC_SIZE',"SIM_VAL")]=
    output[2,c('CLUSTER_ID','CLUSTER_SIZE','SPC','SPC_SIZE',"SIM_VAL")]
  return(output)
}

spc_isna =spc_k1[is.na(spc_k1$SPC),]
#spc_predict_5 = list()
for (i in 1: nrow(spc_isna)) {
  start_time = Sys.time()
  spc_predict_5[[i]]=predict_spc(spc_isna[i,],centroid_spc_w[list_spc$freq>5,])
  print(paste(i,spc_isna[i,1],spc_isna[i,2],spc_predict_5[[i]][1,'SPC'],(Sys.time()-start_time)))
}
# store predict in dataframe 
spc_predict_df = data.frame()
for (i in 1: nrow(spc_isna)) {
  start_time = Sys.time()
  spc_predict_df[((i-1)*10+i):(i*10+i),]=precidt_spc(spc_isna[i,],centroid_spc_w[list_spc$freq>5,])
  print(paste(i,spc_isna[i,1],spc_isna[i,2],spc_predict_df[[i]][1,'SPC'],(Sys.time()-start_time)))
}

# calculate belongings of all labeled data to centroids
# drop outliers: min(dist)>t1, point drops
# drop clusters: # of data in the cluster < t2, delete this cluster, data renominated to other cluster

data_cen_belongs <- function(dat,centroid,t1=0.4) {
  output = cbind(dat,data.frame('SPC_SIZE'=NA,'NEW_SPC'=NA,'OUTLIERS'=FALSE,"NEW_SPC_SIZE"=NA,
                                'NEW_SPC_LIST'=NA,'SPC_LIST_SIZE'=NA,'SIM_VAL'=NA))
  for (i in 1: nrow(dat)) {
    sim = 0;idx = 0
    output[i,'SPC_SIZE']= nrow(dat[dat$SPC==dat[i,'SPC'],])
    for (j in 1:nrow(centroid)) {
      temp_sim = round(sim_2data(dat[i,],centroid[j,]),3)
      if (temp_sim>sim) {
        sim = temp_sim
        idx = j
      }
    }
    output[i,'SIM_VAL']= sim
    output[i,'NEW_SPC'] = ifelse( idx==0, NA,centroid[idx,'SPC'])
    print(paste(i,output[i,c(1,2,3,19,20,24)]))
  }  
  output = output[!is.na(output$NEW_SPC),]
  output[output$SIM_VAL<t1,'OUTLIERS']=TRUE
  return(output[,c(1:3,18:24,4:17)])
}
dat_recen = data_cen_belongs(spc_sample_1,centroid_spc_w[1:10,],0.4)

# DROP clusters that have less than t1 data in the clusters
cluster_drop <- function(dat,centroid,t2=50) {
  drop_list = c()
  for (m in unique(dat$NEW_SPC)) {
    Nspc_size = nrow(dat[which(!is.na(dat$NEW_SPC) & dat$NEW_SPC == m),])
    if (Nspc_size<t2) {
      dat[which(!is.na(dat$NEW_SPC) & dat$NEW_SPC == m),'NEW_SPC']=NA
      drop_list = c(drop_list,m)
    }
  }
  if (!is.null(drop_list)) {
    dat=rbind(dat[!is.na(dat$NEW_SPC),], data_cen_belongs(dat[is.na(dat$NEW_SPC),c(1:3,11:24)],
                                                          centroid[!centroid$SPC %in% drop_list,]))
  }
  return(dat)
}
dat_recen_1 = cluster_drop(dat_recen,centroid_spc_w[1:10,],50)
#

avg_sim = sum(dat_recen_1[dat_recen_1$OUTLIERS==F,'SIM_VAL'])/
  nrow(dat_recen_1[dat_recen_1$OUTLIERS==F,])

t3=0.7;avg_sim0=0;ite0=10;count_err0=5
ite = 0;count_err=0
while (avg_sim0<t3 & ite<ite0 & count_err<count_err0) {
  ite = ite+1;
  list_spc_1 = count(dat_recen_1, "NEW_SPC")[order(count(dat_recen_1,
                                                         "NEW_SPC")$freq,decreasing = T),]
  new_centroid = data.frame(matrix( ncol = ncol(dat_recen_1[,c(1:3,11:24)])))
  colnames(new_centroid) = colnames(dat_recen_1[,c(1:3,11:24)])
  for (s in list_spc_1$SPC ) {
    test = dat_recen_1[(!is.na(dat_recen_1$NEW_SPC) & dat_recen_1$NEW_SPC == s ),]
    if (sum(is.na(new_centroid))/length(new_centroid)==1) {
      new_centroid = spc_centroid(test,3,5)
    } else {
      new_centroid = rbind(new_centroid,spc_centroid(test,3,5))
    }
    new_centroid[nrow(new_centroid),'SPC'] = s
    print(new_centroid[nrow(new_centroid),])
  }
  dat_recen = data_cen_belongs(dat_recen_1[,c(1:3,11:24)],new_centroid)
  dat_recen_1 = cluster_drop(dat_recen,new_centroid)
  avg_sim = sum(dat_recen_1[dat_recen_1$OUTLIERS==F,'SIM_VAL'])/
    nrow(dat_recen_1[dat_recen_1$OUTLIERS==F,])
  if (avg_sim < avg_sim0) {
    count_err = count_err+1
  } else {
    avg_sim0 = avg_sim
  }
}







