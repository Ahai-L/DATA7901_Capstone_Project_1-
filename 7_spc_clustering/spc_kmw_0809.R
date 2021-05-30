## SPC Centroid #################################################################
#calculate the centroid of a set of data
# input: dat- data set
# idx- the list of columns including idx and lable that will be excluded 
# k1- k-gram expan for mc attributes, 
# k2:the number of horizons to keep for every soil data profile (horizons Pruning )
# output: the centroid point of the data set
# used functions : c_mc, c_mn, c_sc, c_sn

spc_centroid <- function(dat,idx,k1=2,k2=5) {
  centroid = data.frame(matrix(NA, ncol = ncol(dat)))
  colnames(centroid) = colnames(dat)
  for ( i in 1 : (length(dat)-idx)) {
    if (typeof(dat[[i+idx]]) == 'list') { # is mn/mc
      if (typeof(dat[[i+idx]][[1]]) == 'character') { # is mc 
        centroid[[i+idx]] = c_mc(dat[[i+idx]],k1,k2)
      } else { #mn
        centroid[[i+idx]] = c_mn(dat[[i+idx]],k2)}
    } else {
      if (typeof(dat[[i+idx]])=='character') {# is sc
        centroid[[i+idx]] = c_sc(dat[[i+idx]])
      } else { #sn
        centroid[[i+idx]] = c_sn(dat[[i+idx]])
      }
    }
  }
  return(centroid)
}
# c_sc -calculate the centroid of a sc column data
# input: sc - a sc type column data
# output: the centroid value of this sc column data
# which is the  most common domain value, including NA
# if return NA, means the center point of this attribute is unsure
# ALT: weighted centers, use first 50% of domains and each single domain has at lease 10%
# ALT: in this way, the center point is not a single value, but a vector ( domain values and their weights)
# ALT: need to write another function(sim_sc2cen) to calculate the similarity between sc and its center
c_sc <- function(sc) {
  sc[is.na(sc)] = 'NA' # convert NA value to countable charecter "NA"
  d = unique(sc[!is.na(sc)]) # list of all unique domain values in sc
  # calculate the density of every domain value in d
  if (length(d) > 1) { # if sc is not NULL (empty data)
    f = vector(mode = 'list',length = length(d))
    names(f) = d
    for (i in d) {
      f[i] = sum(sc[!is.na(sc)]==i) # Accumulate numbers of domain values equal to "i"
    }
    # return the most common domain value, including NA value
    # #(NA)/#(second common domain) < 2 and %(second common domain) > 15%, then use second common domain
    return(ifelse(names(which.max(f)) =='NA',
                  ifelse(( f[[which.max(f)]]/f[[which.max(f[names(f) != 'NA'])]]<2 & 
                             f[[which.max(f[names(f) != 'NA'])]]/length(sc) > 0.15)
                         ,names(which.max(f[names(f) != 'NA'])),NA),
                  names(which.max(f)) ))
    
  } else { return(ifelse(d == 'NA', NA, d))} 
}
# c_sn center point for a sn type attribute
# input: sn - a sn (scalar numeric) type column data
# output: the center value of this column data
# which is the value(in all possible domain values) 
# that produce the maximum sum similarity measurement
# used function: sim_2col
c_sn <- function(sn) {
  d = unique(as.character(as.double(sn[!is.na(sn)])))
  #d = as.character(unique(as.double(sn[!is.na(sn)])))
  sum_sim = vector(mode = 'list',length = length(d))
  names(sum_sim) = d
  if (!is_empty(d)) {
    for (i in d) {
      sn_temp = append(sn,as.double(i))
      sim = 0
      #count = length(sn[!is.na(sn)])
      for (j in 1:length(sn)) {
        temp = sim_2col(j,length(sn_temp),sn_temp)
        sim = ifelse(is.na(temp),sim,sim+temp)
      }
      sum_sim[i] = sim
    }
    return(as.double(names(which.max(sum_sim))))
  } else { return(NA)}
}
# c_mc: center point for a mc type attribute
# input: mc - a mc(multivalued categorical) type column data
# k1- k-gram expand for mc attribute, 
# k2:the number of horizons to keep for every soil data profile (horizons Pruning )
# output : the central value of this column data
# which is the value(in all possible domain values) 
# that produce the maximum sum similarity measurement
# used function: sim_2col


c_mc <- function(mc,k1=2,k2=5) {
  domain_list = c()
  # collect all possible domain values(vectors) in mc with fixed k2 length
  # add NA at the end if the origin horizon levels are less than k2
  for (i in 1 : length(mc)) { 
    domain_list = c(domain_list, 
                    ngramrr(append(mc[[i]],rep(NA,max(0,k2-length(mc[[i]])))) ,ngmin=k2,ngmax=k2))
  }
  if (!is.null(domain_list)) {
    #domain_list = unique(domain_list)
    # unique domain list in descend order of occurring
    domain_list = sort(table(domain_list),decreasing = T)
    # For efficiency purposes, only the top 10% (or top 10, whichever is greater) domains are checked
    domain_list = domain_list[1:min(length(domain_list),max(10,round(length(domain_list)/10)))]
    sum_sim = vector(mode = 'list',length = length(domain_list))
    #names(sim_mc ) = domain_list
    names(sum_sim ) = names(domain_list)
    for (i in 1: length(domain_list)) {
      #mc_temp = append(mc,domain_list[i])
      mc_temp = append(mc,str_split(names(domain_list[i]),' '))
      sim = 0
      #count =
      for (j in 1: length(mc) ) {
        temp = sim_2col(j,length(mc_temp),mc_temp,k1)
        sim = ifelse(is.na(temp),sim,sim+temp)
      }
      sum_sim[i] = sim
    }
    return(str_split(names(which.max(sum_sim)),' '))
  }
  return(list(rep(NA,k2)))
  
}
# c_mn: center point for a mn type attribute
# input: mn - a mn(multivalued numeric) type column data
# k2:the number of horizons to keep for every soil data profile (horizons Pruning )
# output: the center value (vector) of this mn column data
# which is the combinations of the center value for every horizon level
# the center value for every horizon level is calculated by function c_sn
# used function: c_sn

c_mn <-function(mn,k2=5) {
  cen_mn = c()
  for (l in 1 : k2) {
    temp = c()
    for (i in 1 : length(mn)) {
      temp[i] = ifelse(length(mn[[i]])<l,
                       # fill with last value if horizon levels less than k2
                       mn[[i]][length(mn[[i]])], 
                       mn[[i]][l]) 
    }
    cen_mn[l] = c_sn(temp)
  }
  return(list(cen_mn))
}

# distance with specific formula####################
sim_formula <- function(i,j,dat,e,k1) {
  sim = c()
  for ( m in 1 : (length(dat)-e)) {
    sim[m] = sim_2col(i,j,dat[[e+m]],k1)
  }
  n = length(sim); n_nna = sum(!is.na(sim))
  sim[n+1] = sum(sim[!is.na(sim)])/ n # min(sim)
  sim[n+2] = sim[n+1] + (n-n_nna)/n  # max (sum(sim[!is.na(sim)]) +(length(sim)-sum(!is.na(sim)))) /length(sim)
  sim[n+3] = sim[n+1]*n/n_nna #mean_1  #sum(sim[!is.na(sim)])/sum(!is.na(sim))
  sim[n+4] = sim[n+1]*(1+(sim[n+2]-sim[n+1]))  # mean_2
  sim[n+5] = (sim[n+1]+sim[n+2])/2 #mena_3
  #sim =  -log(sim)
  #sim[sim == 'Inf'] = 100
  #sim[is.na(sim)] = -1
  return(sim[[18]])
}
sim_2data <- function(dat1,dat2,e=3,k1=2) {
  sim = 0; count_na = 0
  for (m in 1 : (length(dat1)-e)) {
    if (typeof(dat1[[m+e]]) == 'list') { # is mn/mc group
      if (sum(is.na(dat1[[m+e]]))/length(dat1[[m+e]]) == 1 | 
          sum(is.na(dat2[[m+e]]))/length(dat2[[m+e]]) == 1) { # all NA value in vector i or j
        count_na = count_na + 1
      } else if (typeof(dat1[[m+e]][[1]]) == 'character') { # is mc 
        sim = sim + sim_mc(1,2,rbind(dat1[[m+e]],dat2[[m+e]]),k1)
      } else {
        sim_tem = sim_mn(1,2,rbind(dat1[[m+e]],dat2[[m+e]]))
        if (is.na(sim_tem)) { count_na = count_na+1}
        else {sim = sim + sim_tem }
      }
    } else { #sn/sc
      if (is.na(dat1[[m+e]]) | is.na(dat2[[m+e]])) {
        count_na = count_na + 1
      } else if (typeof(dat1[[m+e]])=='character') {# is sc
        sim = sim + sim_sc(1,2,rbind(dat1[[m+e]],dat2[[m+e]]))
      } else {sim = sim + sim_sn(1,2,rbind(dat1[[m+e]],dat2[[m+e]]))
      }
    }
  }
  if (count_na == (length(dat1)-e)) {sim = -1}
  else {sim = sim/(length(dat1)-e-count_na)}
  return(sim)
}
# similarity of a single attribute between two data#########
sim_2col <- function(i,j,attr,k1=2) {
  if (typeof(attr) == 'list') { # is mn/mc
    if (sum(is.na(attr[[i]]))/length(attr[[i]]) == 1 | 
        sum(is.na(attr[[j]]))/length(attr[[j]]) == 1) { # all NA value in vector i or j
      sim = NA
    } else if (typeof(attr[[i]]) == 'character') { # is mc 
      sim = sim_mc(i,j,attr,k1)
    } else {sim = sim_mn(i,j,attr)}
  } else {
    if (is.na(attr[[i]]) | is.na(attr[[j]])) {
      sim = NA
    } else if (typeof(attr[[i]])=='character') {# is sc
      sim = sim_sc(i,j,attr)
    } else {sim = sim_sn(i,j,attr)
    }
  }
  return(sim)
}
# similarity 
sim_sn <- function(i,j,attr){
  #return(exp(-((dat[[i]] - dat[[j]]))^2/2))
  return(exp(-abs(attr[[i]] - attr[[j]])))
  #return((dat[[i]] - dat[[j]])^2)
  #return(abs(dat[[i]] - dat[[j]]))
}
#
sim_sc <- function(i,j,attr) {
  return(ifelse(attr[i]==attr[j],1,0))
}
#
sim_mn <- function(i,j,attr) {
  sim = 0;count_na = 0
  r_x = max(length(attr[[i]]), length(attr[[j]]))
  if(r_x == length(attr[[i]])) {
    x = i; y = j; r_y = length(attr[[j]])
  } else {
    x = j; y = i; r_y = length(attr[[i]])
  }
  for ( p in 1 : r_y ) {
    if (is.na(attr[[x]][[p]]) | is.na(attr[[y]][[p]])) {
      count_na = count_na + 1
    } else {
      sim = sim + exp(-abs(attr[[x]][[p]] - attr[[y]][[p]]))
      #sim = sim + exp(-((dat[[x]][[p]] - dat[[y]][[p]]))^2/2)
    }
  }
  if (!is.na(attr[[y]][[r_y]])) {
    q = r_y + 1
    while ( q <= r_x) {
      if (is.na(attr[[x]][[q]])) {
        count_na = count_na + 1
      } else {
        sim = sim + exp(-abs(attr[[x]][[q]] - attr[[y]][[r_y]]))
        #sim = sim + exp(-((dat[[x]][[q]] - dat[[y]][[r_y]]))^2/2)
      }
      q = q + 1
    }
  } else {count_na = count_na +(r_x-r_y)}
  if (count_na == r_x) {sim = NA}
  else {sim = sim/(r_x-count_na)} # ? sim = sim/r_x or something else
  return(sim)
}
sim_mc <- function(i,j,attr,k1=2) {
  d_i = ngramrr(attr[[i]],ngmin = 1,ngmax = min(k1,length(attr[[i]])))
  d_j = ngramrr(attr[[j]],ngmin = 1,ngmax = min(k1,length(attr[[j]])))
  sim_max = sim_mc_full(d_i,d_j)
  d_i_ad = gsub('NA','NA_X',d_i)
  sim_min = sim_mc_full(d_i_ad,d_j)
  sim = sim_min*(1+(sim_max-sim_min))
  return(sim)
}

sim_mc_full <- function(v1,v2) {
  m1 = match(v1,v2)
  m2 = match(v2,v1)
  l1 = length(v1)
  l2 = length(v2)
  # add up the no matching part dis-similarity (0-2)
  s = sum(is.na(m1))/length(v1) + sum(is.na(m2))/length(v2)
  # add up the matching parts dis-similarity (0-2)
  for (i in c(unique(na.omit(m1)))) {
    s = s + abs(sum(m1 %in% i)/l1 - sum(m2 %in% m2[i])/l2)
  }
  return(1-s/2) # convert to similarity (0-1)
}


# data sampling ###################################
# only use data with SPC label
spc_sample = merge(spc_k1,spc[,c(1,2)],by=c('PROJECT_CODE','SITE_ID'),all.x = F)
# 500 samples
idx = sample(nrow(spc_sample),500)
spc_sample_500 = spc_sample[idx,]
spc_sample_1 = spc_sample[spc_sample$SPC %in% list_spc[1:10,1],]

# get a list of all SPCs in sample data#########################
list_spc = count(spc_sample, "SPC")[order(count(spc_sample, "SPC")$freq,decreasing = T),]
nrow(list_spc[!list_spc$freq==1,])
nrow(list_spc)
#list_spc = unique(list_spc$SPC)
# set up an empty centroid dataframe
centroid_spc_w = data.frame(matrix( ncol = ncol(spc_sample)))
colnames(centroid_spc_w) = colnames(spc_sample)

# calculate the centroid for every SPC ############################

for (s in list_spc$SPC ) {
  test = spc_sample[(!is.na(spc_sample$SPC) & spc_sample$SPC == s ),]
  if (sum(is.na(centroid_spc_w))/length(centroid_spc_w)==1) {
    centroid_spc_w = spc_centroid(test,3,5)
  } else {centroid_spc_w = rbind(centroid_spc_w,spc_centroid(test,3,5))}
  
  # centroid_spc_w = ifelse(sum(is.na(centroid_spc_w))/length(centroid_spc_w)==1,
  #                         spc_centroid(test,3,5),
  #                         rbind(centroid_spc_w,spc_centroid(test,3,5)))  
  centroid_spc_w[nrow(centroid_spc_w),'SPC'] = s
  print(centroid_spc_w[nrow(centroid_spc_w),])
}


#centroid_spc_w = centroid_spc_w[!is.na(centroid_spc_w$SPC),]
# sim_mat for centroid_spc##################################
# simmat_centroid_w = rep(list(data.frame(matrix(NA_real_,
#                                                nrow = nrow(centroid_spc_w),
#                                                ncol = nrow(centroid_spc_w)))),19)
# simmat_cen = data.frame(matrix(NA_real_,nrow = nrow(centroid_spc_w),
#                                ncol = nrow(centroid_spc_w)))
matsim_cen = matrix(0,nrow = nrow(centroid_spc_w),
                    ncol = nrow(centroid_spc_w))
colnames(matsim_cen) = centroid_spc_w$SPC
rownames(matsim_cen) =  colnames(matsim_cen)
start_time = Sys.time()
for ( i in 1: (nrow(centroid_spc_w))) {
  for (j in min((i+1),nrow(centroid_spc_w)): (nrow(centroid_spc_w))) {
    matsim_cen[i,j] = sim_2data(centroid_spc_w[i,],centroid_spc_w[j,],3,2)
    print(paste(i,j,matsim_cen[i,j]))
  }
}

end_time = Sys.time()
end_time - start_time

# simmat_cen1 = simmat_cen[1:185,1:185]
# rownames(simmat_cen1) =  colnames(simmat_cen1)
matdissim_cen = matsim_cen
matdissim_cen = matdissim_cen+t(matdissim_cen) 
diag(matdissim_cen) = 1
matdissim_cen = 1- matdissim_cen

# for (i in 1 : nrow(simmat_cen)) {
#   simmat_cen[i,i] = 1
#   for (j in 1: max((i-1),1)) {
#     simmat_cen[i,j] = simmat_cen[j,i]
#   }
# }


# extract top list of SPCs which are similar to each other, set a threshold 
spc_sim_list = list();
for (i in 1:nrow(matdissim_cen)) {
  cen_list = which(matdissim_cen[,i]<0.3)
  spc_sim_list[[i]] = centroid_spc_w[cen_list,]
  spc_sim_list[[i]] = spc_sim_list[[i]][order(matdissim_cen[cen_list,i],decreasing =F),]
  
}

# hierarchical clustering##################################
dist_cen = as.dist(1-matsim_cen_one)
#typeof(dist_cen)
hc = hclust(dist_cen,method = 'average')
mem = cutree(hc,h=0.3)
#mem1 = cutree(hc,h=0.4)
#mem == mem1
cent = NULL
for (i in 1: length(table(mem))) {
  cent = rbind(cent,colMeans(matdissim_cen[mem == i,,drop=F]))
}
hc1 = hclust(dist(cent),members = table(mem))

opar <- par(mfrow = c(1,1))
plot(hc ,labels= colnames(matsim_cen_one), hang=-1,cex=0.6)
plot(hc1, hang=-5,cex=0.03)
#plot(hc1,hang=-1,cex = 0.6)
par(opar)
hc_dend = as.dendrogram(hc)
nleaves(hc_dend)
nnodes(hc_dend)

plot(hc_dend,leaflab = 'none')

plot(color_branches(hc,h=mem),leaflab = 'none')

#color_scheme = rev(brewer_pal(10,'RdBu'))
cluster_df <- data.frame(spc = names(mem), cluster = mem)
#heatmap.2(as.matrix(simmat_cen1[1:50,1:50]),Rowv = NULL,Colv = NULL,dendrogram = 'none',
#          trace = 'none', density.info = 'none', 
#          keysize = 1, labRow = F, labCol = F, xlab = "SPCs",
#          ylab = "SPCs")
group_0.6 = list()
for ( i in 1: length(table(mem))) {
  group_0.6[[i]] = names(mem[mem == i]) 
}

# surface plot

fig <- plot_ly(z = ~as.matrix(simmat_cen1))
fig <- fig %>% add_surface()
fig
# Predict SPCs###########################################################
# this is for a set of dat

predict_spc <- function(dat,centroid,e=3,k1=2,n=10) {
  output = data.frame()
  for (i in 1:nrow(dat)) {
    start_time = Sys.time()
    temp_output = cbind(dat[i,],data.frame('ITEM'="Original Data","CLUSTER_ID"=NA,
                                           'CLUSTER_SIZE'=NA,'SPC_SIZE'=NA,'SIM_VAL'=1))
    for (j in 1:nrow(centroid)){
      temp_simval = sim_2data(dat[i,],centroid[j,],e,k1)
      if (nrow(temp_output)<(n+1)){ # add a record
        temp_clusterid =cluster_df[cluster_df$spc==centroid[j,'SPC'],'cluster']
        temp_clustersize = length(group_0.6[[temp_clusterid]])
        temp_spcsize = nrow(spc_sample[spc_sample$SPC==centroid[j,'SPC'],])
        temp_output = rbind(temp_output,cbind(centroid[j,],
                                              data.frame('ITEM'="SPC Centroid","CLUSTER_ID"=temp_clusterid,
                                                         'CLUSTER_SIZE'=temp_clustersize,
                                                         'SPC_SIZE'=temp_spcsize,'SIM_VAL'=temp_simval)))
      }
      else {# compare and replace if new record's simval bigger
        temp_output = temp_output[order(temp_output$SIM_VAL,decreasing = T),] # sort output by SIM_VAL
        if (temp_simval>temp_output[n+1,"SIM_VAL"]) {
          temp_clusterid =cluster_df[cluster_df$spc==centroid[j,'SPC'],'cluster']
          temp_clustersize = length(group_0.6[[temp_clusterid]])
          temp_spcsize = nrow(spc_sample[spc_sample$SPC==centroid[j,'SPC'],])
          temp_output[n+1,]=cbind(centroid[j,],
                                  data.frame('ITEM'="SPC Centroid","CLUSTER_ID"=temp_clusterid,
                                             'CLUSTER_SIZE'=temp_clustersize,
                                             'SPC_SIZE'=temp_spcsize,'SIM_VAL'=temp_simval))
        }
      }
    }
    temp_output[2:(n+1),c('PROJECT_CODE','SITE_ID')] = temp_output[1,c('PROJECT_CODE','SITE_ID')]
    temp_output[1,c('CLUSTER_ID','CLUSTER_SIZE','SPC','SPC_SIZE',"SIM_VAL")]=
      temp_output[2,c('CLUSTER_ID','CLUSTER_SIZE','SPC','SPC_SIZE',"SIM_VAL")]
    print(paste(i,dat[i,1],dat[i,2],'real:',dat[i,'SPC'],'predict:',temp_output[1,'SPC'],
                "sim_dat_Pcen:",round(temp_output[1,'SIM_VAL'],3),
                'sim_dat_Ocen:',round(sim_2data(dat[i,],centroid[centroid$SPC==dat[i,'SPC'],]),3),
                'sim_cen_cen:',round(1-matdissim_cen[dat[i,'SPC'],temp_output[1,'SPC']],3),
                'cluster_O:',cluster_df[cluster_df$spc==dat[i,'SPC'],'cluster'],
                'cluster_P:',temp_output[1,'CLUSTER_ID'],
                'time:',round((Sys.time()-start_time))))
    output = rbind(output,temp_output)
  }
  output = output[,c(18,1:3,21,19,20,22,4:17)]
  return(output)
}

spc_predict_test = predict_spc(spc_sample,centroid_spc_w)#[list_spc$freq>5,])
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






