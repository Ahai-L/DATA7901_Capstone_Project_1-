# Library loading ################################
library(stringr)


# Data input ########################################
# horizontal level dataset created by "data_and_library.r"
spc_kmean_dat = read.csv('./0_general/spc_train.csv',stringsAsFactors=F)
#spc_kmean_dat = spc_kmean_dat[1:5,]
# data reorganize##################################################################################
# Exchange ordinal data
# exchange one ordinal attribute in a dataframe into numeric 
# based on the list in "l"
# input: x: single attribute of a dataframe, e.g.: A$a
#        l: list of unique attribute values and their replace number
#            example of list "l" : list(c('a','b','c'), c(1,2,3))
#        na: na: the value for NA, default as 0
# output: the numeric attribute
ex_ordinal <- function(x,l,na = NA) {
  if (length(l[[1]]) == length(l[[2]])) {
    x[is.na(x)] = na
    for (i in 1 : length(l[[1]])) {
      x[x==l[[1]][i]] = l[[2]][i]
    }
    return(as.numeric(x))
  } else { print('Error, the length of the list not matched.')}
}
# spc_kmean_dat$CUTAN_TYPE[is.na(spc_kmean_dat$CUTAN_TYPE)] = 'Z'
spc_kmean_dat$VEG_SPEC_CODE = str_extract(spc_kmean_dat$VEG_SPEC_CODE, "^.{3}")
spc_kmean_dat$STATUS[spc_kmean_dat$STATUS %in% c('P','R','O','T')] = NA
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('QS','S')] = 'Siliceous Upper'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('GS','MS','MU','PL','PH','PO','ST',
                                'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('AC','FC','FS','IS')] = 'Sesquioxide'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('CO','CC')] = 'Organic'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('AH','GY','HA')] = 'Evaporite'
spc_kmean_dat$OBS_LITH_CODE[spc_kmean_dat$OBS_LITH_CODE %in% 
                              c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
# $OCF_LITH_CODE
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('CH','JA','PG','PC','QZ','QU','QP','LC','OW')] = 'Extremely Siliceous'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('QS','S')] = 'Siliceous Upper'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('AP','AR','AS','GN','MG','MI','RB','RH','SA','TO','PU')] = 'Siliceous Mid'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('AD','AE','DA','GD','GW','VD')] = 'Siliceous Lower'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('GS','MS','MU','PL','PH','PO','ST',
                                'SH','Z','ZS','SL','SY','TR')] = 'Intermediate Upper'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('AG','AN','BR','C','DI','MD','TU','VI','VG')] = 'Intermediate Lower'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('AM','AF','BA','BB','DR','GA','SK','SP','VB','VC')] = 'Mafic'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('GE','GR','HO','ME','PY','SR')] = 'Ultra Mafic'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('KA','KL','KM','KS','KR','KC','DM','LI','MB','ML','SS','K')] = 'Calcareous'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('AC','FC','FS','IS')] = 'Sesquioxide'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('CO','CC')] = 'Organic'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('AH','GY','HA')] = 'Evaporite'
spc_kmean_dat$OCF_LITH_CODE[spc_kmean_dat$OCF_LITH_CODE %in% 
                              c('AL','CB','CG','CL','CU','CZ','M','OT','SD','SN','GV','IG','MY','UC','H','R')] = 'Others'
spc_kmean_dat$REL_MOD_SLOPE_CLASS = ex_ordinal(spc_kmean_dat$REL_MOD_SLOPE_CLASS,
                                          list(c('LP','GP','UP','RP','B1','B2','B3',
                                                 'GR','UR','RR','SR','B4','B5',
                                                 'UL','RL','SL','VL','B6',
                                                 'UH','RH','SH','VH','PH',
                                                 'RM','SM','VM','PM'),
                                               c(0.02,0.1,0.3,1,2,3.5,7.5,
                                                 0.3,0.9,3,6,10.5,22.5,
                                                 3,10,20,35,75,
                                                 9,30,60,105,225,
                                                 100,200,350,750)))
spc_kmean_dat$TEXTURE_CODE[spc_kmean_dat$TEXTURE_CODE %in% 
                              c('S','LS','CS','SL','SCL','CFS','CKS','FS','FSCL',
                                'FSCLZ','FSL','FSLZ','KS','FSCL','KSL','KSS','LFS',
                                'LFSY','LFSYZ','LKS','LMS','LSY','MS','SCFLS','SLZ','SS','ST',
                                'KSCL','SCLFS')] = 'Sandy'
spc_kmean_dat$TEXTURE_CODE[spc_kmean_dat$TEXTURE_CODE %in% 
                             c('L','ZL','ZCL','CLFS','CL','CLS','CLFSZ','CLKS','CLMS','CLZ','ZCL')] = 'Loamy'
spc_kmean_dat$TEXTURE_CODE[spc_kmean_dat$TEXTURE_CODE %in% 
                             c('C','LC','LMC','MC','MHC','HC','CSC','FSHC','FSLC','FSLCZ','FSLMC',
                               'FSMC','FSMHC','KSHC','KSLMC','KSMC','KSMHC','LCFS',
                               'LCKS','LCS','LMCFS','LMCKS','LMCS','LMCZ','LMCKS',
                               'LMCS','MCFS','MCS','MCZ','MHCFS','MHCS','MSC','SC','SHC',
                               'SLC','SLMC','SMC','SMHC','ZC','ZHC','ZLC','ZLCFS','ZLMC','ZLMCS',
                               'ZMC','ZMHC','LCZ','FSC','KSC','KSLC')] = 'Clayey'
spc_kmean_dat$TEXTURE_CODE[spc_kmean_dat$TEXTURE_CODE %in% 
                             c('AP','CP','GP','GR','HP','IP','LP','SP')] = 'Organic'
sort(unique(spc_kmean_dat$TEXTURE_CODE))

#spc_kmean_dat$SLOPE_MORPH_TYPE[spc_kmean_dat$SLOPE_MORPH_TYPE %in%
#                                 c('F','M','S','V','R','D','H')] = NA
#spc_kmean_dat$HEIGHT_CLASS[is.na(spc_kmean_dat$HEIGHT_CLASS)] = 0
#spc_kmean_dat$HEIGHT_CLASS = as.integer(spc_kmean_dat$HEIGHT_CLASS)
#spc_kmean_dat$COVER_CLASS = ex_ordinal(spc_kmean_dat$COVER_CLASS,
#                                       list(c('D','M','S','V','I','L','E'),1:7))
#spc_kmean_dat$HOR_PREFIX[!is.na(spc_kmean_dat$HOR_PREFIX)] = 1
#spc_kmean_dat$HOR_PREFIX[is.na(spc_kmean_dat$HOR_PREFIX)] = 0
#spc_kmean_dat$HOR_SUBHOR[is.na(spc_kmean_dat$HOR_SUBHOR)] = 0
#spc_kmean_dat$HOR_SUBHOR = as.integer(spc_kmean_dat$HOR_SUBHOR)
#spc_kmean_dat$MOTT_TYPE[spc_kmean_dat$MOTT_TYPE =='M'] = 1
spc_kmean_dat$MOTT_TYPE[!spc_kmean_dat$MOTT_TYPE =='M'] = NA
spc_kmean_dat$MOTT_TYPE = ex_ordinal(spc_kmean_dat$MOTT_TYPE,
                                   list(c('M'),1))
#spc_kmean_dat$MOTT_TYPE = as.integer(spc_kmean_dat$MOTT_TYPE)
spc_kmean_dat$PEDALITY_GRADE = ex_ordinal(spc_kmean_dat$PEDALITY_GRADE,
                                          list(c('G','V','W','M','S'),1:5))
#spc_kmean_dat[spc_kmean_dat$PEDALITY_GRADE %in% c(1,2),c('PEDALITY_TYPE','PEDALITY_SIZE')] = NA
#spc_kmean_dat$PEDALITY_SIZE[is.na(spc_kmean_dat$PEDALITY_SIZE)] = 0
#spc_kmean_dat$PEDALITY_SIZE = as.integer(spc_kmean_dat$PEDALITY_SIZE)
#spc_kmean_dat$HSG_ABUNDANCE[is.na(spc_kmean_dat$HSG_ABUNDANCE)] = 0
#spc_kmean_dat$HSG_ABUNDANCE = as.integer(spc_kmean_dat$HSG_ABUNDANCE)
#spc_kmean_dat$SEG_SIZE[is.na(spc_kmean_dat$SEG_SIZE)] = 0
#spc_kmean_dat$SEG_SIZE = as.integer(spc_kmean_dat$SEG_SIZE)
spc_kmean_dat[spc_kmean_dat$NATURE %in% c('U','O'),'NATURE'] = NA
#spc_kmean_dat$SURF_FRAG_SIZE[is.na(spc_kmean_dat$SURF_FRAG_SIZE)] = 0
#spc_kmean_dat$SURF_FRAG_SIZE = as.integer(spc_kmean_dat$SURF_FRAG_SIZE)
#spc_kmean_dat$OCF_ABUNDANCE[is.na(spc_kmean_dat$OCF_ABUNDANCE)] = 0
#spc_kmean_dat$OCF_ABUNDANCE = as.integer(spc_kmean_dat$OCF_ABUNDANCE)
#spc_kmean_dat$FTS_PH[is.na(spc_kmean_dat$FTS_PH)] = 0
spc_kmean_dat$BOUND_DISTINCT[spc_kmean_dat$BOUND_DISTINCT %in% c('S','A','C')] = "GRP1"

spc_kmean_dat$BOUND_DISTINCT[spc_kmean_dat$BOUND_DISTINCT %in% c('G','D')] = "GRP2"
# spc_kmean_dat$BOUND_DISTINCT = ex_ordinal(spc_kmean_dat$BOUND_DISTINCT,
#                                           list(c('S','A','C','G','D'),1:5))
#spc_kmean_dat$HOR_DEPTH = spc_kmean_dat$LOWER_DEPTH - spc_kmean_dat$UPPER_DEPTH
#spc_kmean_dat = spc_kmean_dat[,!(colnames(spc_kmean_dat) %in% c('UPPER_DEPTH','LOWER_DEPTH'))]
#spc_kmean_dat = spc_kmean_dat[,c(1:2,8,3:7,9:19)]
write.csv(spc_kmean_dat,'./7_spc_clustering/spc_kmean_dat.csv',row.names = F)

#
sn_sc = grep('STATUS|OBS_LITH_CODE|REL_MOD_SLOPE_CLASS|VEG_SPEC_CODE|SPC',colnames(spc_kmean_dat))
#nrow(unique(spc_train[,c(1,2,sn_sc)]))

# reorganize dataset to profile level
# sn and sc merge to profile level

spc_k <- spc_kmean_dat %>% 
  distinct(PROJECT_CODE,SITE_ID,STATUS,OBS_LITH_CODE,REL_MOD_SLOPE_CLASS,VEG_SPEC_CODE,SPC)
#nrow(unique(spc_k[,1:2]))

# merge mn and mc data into vector type data
# split the data, group by project_code and site_id
spc_1 <- split(spc_kmean_dat, list(spc_kmean_dat$PROJECT_CODE,spc_kmean_dat$SITE_ID),drop = T)
spc_2 <- as.list(NA)
for (i in 1 : length(spc_1)){
  spc_2[[i]] <- subset(spc_1[[i]],select = -c(1:4,sn_sc))
}
# COMBINE sn,sc,mn,mc
spc_k$HOR <- spc_2
spc_k = spc_k[,c(1:2,7,3:6,8)]
spc_k2 = spc_k[!is.na(spc_k$SPC),]


# another structure use this structure
mn_mc = 9:18
spc_k1 = merge(spc_k[,1:7],
               aggregate(x=spc_kmean_dat[,mn_mc],
                         by=list(PROJECT_CODE=spc_kmean_dat$PROJECT_CODE,
                                 SITE_ID=spc_kmean_dat$SITE_ID),
                         FUN = I),
               by = c('PROJECT_CODE','SITE_ID'))

spc_k1 = spc_k1[,c(1:2,7,3:6,8:17)]
# change sc to list type, then then centroid point for sc can be multivariate
spc_k2 = spc_k1
spc_k2$STATUS = as.list(spc_k2$STATUS)
spc_k2$OBS_LITH_CODE = as.list(spc_k2$OBS_LITH_CODE)
spc_k2$VEG_SPEC_CODE = as.list(spc_k2$VEG_SPEC_CODE)
#
# Threshold of empty_rate##############################
th_e = 0.3;c = c();t = c()
for(i in 1: nrow(spc_k1)){
  t[i] = empty_rate_v(spc_k1[i,])[36,6]
  print(paste(i,t[i]))
}
spc_k3 = spc_k1[t<0.2,]
spc_k0 = is.na(spc)
# k value, the level of n-gram #################################################
k = 2
# Functions##################################################################

# Exchange ordinal data
# exchange one ordinal attribute in a dataframe into numeric 
# based on the list in "l"
# input: x: single attribute of a dataframe, e.g.: A$a
#        l: list of unique attribute values and their replace number
#            example of list "l" : list(c('a','b','c'), c(1,2,3))
#        na: na: the value for NA, default as NA
# output: the numeric attribute
ex_ordinal <- function(x,l,na = NA) {
  if (length(l[[1]]) == length(l[[2]])) {
    x[is.na(x)] = na
    for (i in 1 : length(l[[1]])) {
      x[x==l[[1]][i]] = l[[2]][i]
    }
    return(as.double(x))
  } else { print('Error, the length of the list not matched.')}
}
# Empty rate for vector type data
# empty rate of a dataframe which has vector type as value of element
# except: exceptional attributs (in list) that would not change 0 to NA
empty_rate_v <- function(x) {
  rate = data.frame('name'=NA,'EMPTY_RATE'=0, 'EMPTY_#'=0, 'NON_EMPTY_#'=0, "TOTAL"=0)
  n = nrow(x)
  d = length(x)
  for ( i in 1: d) {
    na = 0
    for (j in 1:n) {
      if (anyNA(x[[i]][[j]])) {
        na = na + 1
      }
    }
    s = na/n
    rate[i,] =  list(names(x[i]),s,na, n-na, n )
  }
  rate = rate[order(rate$EMPTY_RATE,decreasing = T),]
  na_r =  sum(rowSums(is.na(x)) != 0)
  rate = rbind(rate,list('By_rows',na_r/n, na_r, n-na_r, n))
  na_t = sum(is.na(x))
  rate = rbind(rate,list('Total', sum(rate[1:(nrow(rate)-1),3])/(n*d),
                         sum(rate[1:(nrow(rate)-1),3]),sum(rate[1:(nrow(rate)-1),4]),n*d ))
  rate$EMPTY_RATE_v = rate$EMPTY_RATE
  rate$EMPTY_RATE = label_percent()(rate$EMPTY_RATE)
  return(rate)
}

# Empty rate
# empty rate of a dataframe 
empty_rate <- function(x) {
  rate = data.frame('name'=NA,'EMPTY_RATE'=0, 'EMPTY_#'=0, 'NON_EMPTY_#'=0, "TOTAL"=0)
  n = nrow(x)
  d = length(x)
  for ( i in 1: d) {
    na = sum(is.na(x[[i]]) )
    s = na/n
    rate[i,] =  list(names(x[i]),s,na, n-na, n )
  }
  rate = rate[order(rate$EMPTY_RATE,decreasing = T),]
  na_r =  sum(rowSums(is.na(x)) != 0)
  rate = rbind(rate,list('By_rows',na_r/n, na_r, n-na_r, n))
  na_t = sum(is.na(x))
  rate = rbind(rate,list('Total',na_t/(n*d),na_t,n*d-na_t,n*d ))
  rate$EMPTY_RATE_v = rate$EMPTY_RATE
  rate$EMPTY_RATE = label_percent()(rate$EMPTY_RATE)
  return(rate)
}
#n-gram
# input : 
# x: a list/column of vector type data (mc), 
# k: the level of n-gram
# eg.  x = { (a,b),(b,c)}, k= 2
# output: the k-gram of x in vector type: (a,b,ab,b,c,bc)
n_gram <- function(x,k) {
  y = c()
  for ( i in 1:length(x)) {
    y = c(y, ngramrr(x[[i]],ngmin = 1,ngmax = min(k,length(x[[i]]))))
  }
  return(y)
}

# calculate H (entropy weights) for sc and mc attributes
# x: data frame includes sc ( typeof() = character) & mc( typeof() = list & character for sublevel) 
#attribute(s) ,
# c: list of attributes column indexs that would be excluded, default as 0
# k: the level of n-gram if is a mc type attribute, default as 2
# output : list of H (ENTROPY)

entropy_c <- function(x,c=0,k=2) {
  H = rep(0,length(x))
  for (n in 1 : length(x)) {
    if (!typeof(x[[n]]) %in% c('character','list')|(n %in% c) |
        (typeof(x[[n]]) == 'list' & typeof(x[[n]][[1]]) != 'character' )) {
      
    }
    else  {
      if (typeof(x[[n]]) == 'list' & typeof(x[[n]][[1]]) == 'character' ) {
        y = n_gram(x[[n]],k)
      } else { y = x[[n]]}
      A = unique(y)
      t = length(A)
      l = length(y)
      for ( i in 1: t) {
        l1 = ifelse(is.na(A[i]),sum(is.na(y)),length(which(y == A[i])))
        H[n] = H[n] + l1/l *log(l1/l)
      }
      H[n] = -(H[n]/t)
    }
  }
  return(H)
}
start_time = Sys.time()
H_c = entropy_c(spc_k1,1:3,k)
end_time = Sys.time()
end_time - start_time

for ( m in 14:34) {
  print(paste(colnames(spc_k1)[m],typeof(spc_k1[[m]][[1]])))
}
# sim_sc (0 or 1) similarity value of two scalar categorical type (sc) data 
# i: index of input data 1 (row #)
# j: index of input data 2 (row #)
# m: attribute m ( vector/column), where data 1 and 2 are located
# output: the similarity value( 0: different or 1: same ) of data 1 and 2
sim_sc_1 <- function(i,j,m) {
  if (is.na(m[i])&is.na(m[j])) {
    s = 1
  } else if (is.na(m[i])|is.na(m[j])) {
    s = 0
  } else if ( m[i]==m[j]) {
    s = 1
  } else { s = 0}
  return(s)
}
#calculate the similarity of two vectors ( 0: totally different, 1: totally the same)
sim_mc_1 <- function(v1,v2) {
  m1 = match(v1,v2)
  m2 = match(v2,v1)
  l1 = length(v1)
  l2 = length(v2)
  # add up the no matching part distances
  s = sum(is.na(m1))/length(v1) + sum(is.na(m2))/length(v2)
  # add up the matching parts distances
  for (i in c(unique(na.omit(m1)))) {
    s = s + abs(sum(m1 %in% i)/l1 - sum(m2 %in% m2[i])/l2)
  }
  return((2-s)/2)
}
# distance between two data
# input : i,j, the row index of two data 
# dat: the data
# c: list of attributes column indexs that would be excluded, default as 0
# k: the level of n-gram if is a mc type attribute, default as 2 
# H_c: entropy weights for sc and mc attributes, calculated by Function 'entropy_c'
# dist_ij <- function(i,j,dat,c=0,k=2,H_c) {
#   s_sn = 1;t_sn = 0; s_c = 0; t_c = 0;H = 0; t_mn = 1; s_mn = 0
#   for (m in 1 : length(dat)) {
#     if (!m %in% c){
#       if (!typeof(dat[[m]]) %in% c('character','list')) { 
#         s_sn = s_sn * exp(-(dat[[m]][[i]] - dat[[m]][[j]])^2/2)
#         t_sn = t_sn+1
#       } else if (typeof(dat[[m]]) == 'character') {
#         s = sim_sc(i,j,dat[,m])
#         s_c = s_c + H_c[m]*s
#         H = H + H_c[m]
#         t_c = t_c + 1
#       } else if (typeof(dat[[m]][[1]]) == 'character') {
#         s = sim_mc(ngramrr(dat[[m]][[i]],ngmax = min(k,length(dat[[m]][[i]]))),
#                    ngramrr(dat[[m]][[j]],ngmax = min(k,length(dat[[m]][[j]]))))
#         s_c = s_c + H_c[m]*s
#         H = H + H_c[m]
#         t_c = t_c + 1
#       } else {
#         r_i = length(dat[[m]][[i]]); r_j = length(dat[[m]][[j]])
#         if (r_i > r_j) {
#           for ( p in 1 : r_j ) {
#             t_mn = t_mn * exp(-(dat[[m]][[i]][[p]] - dat[[m]][[j]][[p]])^2/2)
#           }
#           for (q in (r_j+1) : r_i) {
#             t_mn = t_mn * exp(-(dat[[m]][[i]][[q]] - dat[[m]][[j]][[r_j]])^2/2)
#           }
#         }
#         else if ( r_i < r_j) {
#           for ( p in 1 : r_i ) {
#             t_mn = t_mn * exp(-(dat[[m]][[i]][[p]] - dat[[m]][[j]][[p]])^2/2)
#           }
#           for (q in (r_i+1) : r_j) {
#             t_mn = t_mn * exp(-(dat[[m]][[i]][[r_i]] - dat[[m]][[j]][[q]])^2/2)
#           }
#         } 
#         else {
#           for ( p in 1 : r_i ) {
#             t_mn = t_mn * exp(-(dat[[m]][[i]][[p]] - dat[[m]][[j]][[p]])^2/2)
#           }
#         }
#         s_mn = s_mn + t_mn
#       }
#     }
#   }
#   S = (s_sn+s_mn+t_c*s_c/H)/(length(dat)-length(c)-t_sn+1)
#   return(-log(S))
#   #return(S)
# }

# distance between two data, without entropy
# input : i,j, the row index of two data 
# dat: the data
# c: list of attributes column indexs that would be excluded, default as 0
# k: the level of n-gram if is a mc type attribute, default as 2 
# H_c: entropy weights for sc and mc attributes, calculated by Function 'entropy_c'

dist_ij_without_entropy <- function(i,j,dat,c=0,k=2) {
  s_sn = 1;t_sn = 0; s_c = 0; t_c = 0; s_mn = 0
  for (m in 1 : length(dat)) {
    if (!m %in% c){
      if (!typeof(dat[[m]]) %in% c('character','list')) { 
        dat[[m]][is.na(dat[[m]])]=0
        s_sn = s_sn * exp(-(dat[[m]][[i]] - dat[[m]][[j]])^2/2)
        t_sn = t_sn+1
      } else if (typeof(dat[[m]]) == 'character') {
        s = sim_sc_1(i,j,dat[,m])
        s_c = s_c + s
        # H = H + H_c[m]
        t_c = t_c + 1
      } else if (typeof(dat[[m]][[1]]) == 'character') {
        s = sim_mc_1(ngramrr(dat[[m]][[i]],ngmax = min(k,length(dat[[m]][[i]]))),
                   ngramrr(dat[[m]][[j]],ngmax = min(k,length(dat[[m]][[j]]))))
        s_c = s_c + s
        # H = H + H_c[m]
        t_c = t_c + 1
      } else {
        t_mn = 1;
        dat[[m]][[i]][is.na(dat[[m]][[i]])]=0
        dat[[m]][[j]][is.na(dat[[m]][[j]])]=0
        r_x = max(length(dat[[m]][[i]]), length(dat[[m]][[j]]))
        if(r_x == length(dat[[m]][[i]])) {
          x = i; y = j; r_y = length(dat[[m]][[j]])
        } else {
          x = j; y = i; r_y = length(dat[[m]][[i]])
        }
        for ( p in 1 : r_y ) {
          t_mn = t_mn * exp(-(dat[[m]][[x]][[p]] - dat[[m]][[y]][[p]])^2/2)
        }
        q = r_y + 1
        while ( q <= r_x) {
          t_mn = t_mn * exp(-(dat[[m]][[x]][[q]] - dat[[m]][[j]][[r_y]])^2/2)
          q = q + 1
        }
        s_mn = s_mn + t_mn
      }
    }
  }
  S = (s_sn+s_mn+s_c)/(length(dat)-length(c)-t_sn+1)
  return(-log(S))
  #return(S)
}
# dist_mat_without entropy
dist_mat_w = data.frame(NA)
start_time = Sys.time()
for ( i in 1: nrow(spc_sample)) {
  for (j in min(i+1,nrow(spc_sample)): nrow(spc_sample)) {
    dist_mat_w[i,j] = dist_ij_without_entropy(i,j,spc_sample,1:3,k)
    print(paste(i,j,dist_mat_w[i,j]))
  }
}
end_time = Sys.time()
end_time - start_time
# data sampling ###################################

spc_sample = merge(spc_k1,spc[,c(1,2)],by=c('PROJECT_CODE','SITE_ID'),all.x = F)
idx = sample(nrow(spc_sample),500)
spc_sample = spc_sample[idx,]
# H_c
# start_time = Sys.time()
# H_c = entropy_c(spc_sample,1:3,k)
# end_time = Sys.time()
# end_time - start_time

# sim_mat
# sim_mat = data.frame(NA)
# start_time = Sys.time()
# for ( i in 1: nrow(spc_sample)) {
#   for (j in min((i+1),nrow(spc_sample)): nrow(spc_sample)) {
#     sim_mat[i,j] = dist_ij_without_entropy(i,j,spc_sample,1:3,k)
#     print(paste(i,j,sim_mat[i,j]))
#   }
# }
# end_time = Sys.time()
# end_time - start_time
# 
# for (i in 1: nrow(sim_mat)) {
#   sim_mat[i,i] = 0
#   for (j in 1: max((i-1),1)) {
#     sim_mat[i,j] = sim_mat[j,i]
#   }
# }
#
p = data.frame("PROJECT_CODE"=spc_k1$PROJECT_CODE,"SITE_ID"=spc_k1$SITE_ID,
               'den'=rep(0,nrow(spc_k1)))
p = merge(p,spc[,c(1,2,4)],by=c('PROJECT_CODE','SITE_ID'),all.x = F)
p = p[idx,]

# sim_mat and dist_mat ######################################################
sim_m2 = rep(list(data.frame(matrix(NA_real_,nrow = 500,ncol = 500))),19)
start_time = Sys.time()
for ( i in 1: nrow(spc_sample)) {
  for (j in min((i+1),nrow(spc_sample)): nrow(spc_sample)) {
    sim_temp = sim_formula(i,j,spc_sample,3,k)
    for (m in 1: length(sim_temp)){
      sim_m2[[m]][[j]][[i]] = sim_temp[m]
      print(paste(m,i,j,sim_m2[[m]][i,j]))
    }
  }
}
end_time = Sys.time()
end_time - start_time

for (i in 1: length(sim_m2)) {
  for (j in 1 : nrow(sim_m2[[i]])) {
    sim_m2[[i]][j,j] = 1
    for (m in 1: max((j-1),1)) {
      sim_m2[[i]][j,m] = sim_m2[[i]][m,j]
    }
  }
}
dist_m2 = rep(list(data.frame(matrix(NA_real_,nrow = 500,ncol = 500))),19)

for (i in 1: length(sim_m2)) {
  dist_m2[[i]] = -log(sim_m2[[i]])
  dist_m2[[i]][dist_m2[[i]] == 'Inf'] = 100
  dist_m2[[i]][is.na(dist_m2[[i]])] = -1
}
dist_m2


# kmean#############################################
d_c = 0.9; max_d = 0; p$den = 0;k_m = 18
for ( i in 1 : nrow(spc_sample)) {
  for (j in min(i+1,nrow(spc_sample)): nrow(spc_sample)) {
    #d = dist_ij(i,j,spc_k1,1:3,k)
    d = dist_mat[[k_m]][i,j]
    #print(paste(i,j,d))
    max_d = ifelse(d==100,max_d,max(max_d, d))
    x = ifelse((d-d_c) <0,1,0)
    #x = ifelse((d-d_c) <0,(1-d/d_c),0)
    p[i,'den'] = p[i,'den']+ x
  }
}
#p1 = as.data.frame(sort(p$den,decreasing = T,index.return=T))
idx_den = order(p$den,decreasing = T)
p = p[idx_den,]
p[,'idx'] = idx_den
#names(p1)[1] = 'den'
p[,'dis']= max_d
for (i in 1: nrow(p)) {
  if (i ==1) {
    p[i,'dis'] = max_d
    p[i,'idx_cen'] = p[i,'idx']
    #p[i,'idx_cen'] = as.integer(rownames(p[i,]))
  } 
  else {
    for (j in 1 : (i-1)){
      #d = dist_ij(p[i,'idx'],p[j,'idx'],spc_sample,1:3,k)
      d = dist_mat[[k_m]][p[i,'idx'],p[j,'idx']]
      if (p[i,'dis'] > d) {
        p[i,'dis'] = d 
        p[i,'idx_cen'] = p[j,'idx']
      }
    }
  }
}
p[1,'dis']=2
#ggplot(p, aes(x = den, y = dis, color = SPC)) +
# geom_point(alpha=0.5) #+
#scale_color_gradient(low="red", high="yellow")
plot(p$den,p$dis, col = p$idx_cen, main="Distance/Density",
     xlab="Density ", ylab="Distance", pch=19)
th_den = 50#(max(p$den)-min(p$den))/4
th_dis = 0.5 #(max(p$dis)-min(p$dis))/2
th_dis_1 =1
lines(rep(th_den,10),c(-1,sort(runif(8, -1, 1.6)),1.6))
lines(c(-10,sort(runif(8, -10, 150)),150),rep(th_dis,10),)
lines(c(-10,sort(runif(8, -10, 150)),150),rep(th_dis_1,10),)


nrow(p[p$den>th_den & p$dis>th_dis,])
centroid = c(); 
for ( i in 1 : nrow(p)) {
  if ( p[i,'den']>th_den & p[i,'dis']>th_dis) {# on the top right conner of the cross line in the plot
    p[i,'idx_cen'] = p[i,'idx'] # this point is the center point
    centroid = c(centroid,p[i,'idx'])
    p[i,'dis'] = dist_mat[[k_m]][p[i,'idx'],p[i,'idx_cen']]
  } else if (p[i,'dis']>th_dis_1|p[i,'dis']<0) { # top left conner of the plot
    p[i,'idx_cen'] = 0 # outliers
  } else {
    while (!p[i,'idx_cen'] %in% c(centroid,0)) { # the idx_cen is not the nominated center point
      # p[i,'idx_cen'] = p[p$idx == p[i,'idx_cen'],'idx_cen']
      p[i,'idx_cen'] = centroid[which(dist_mat[[k_m]][p[i,'idx'],centroid] == min(dist_mat[[k_m]][p[i,'idx'],centroid]))]
    }
    p[i,'dis'] = dist_mat[[k_m]][p[i,'idx'],p[i,'idx_cen']]
  }
}
plot(p$den,p$dis, col = p$idx_cen, main="Distance/Density",
     xlab="Density ", ylab="Distance", pch=19)
for (i in c(0,centroid)) {
  print(paste(i,nrow(p[p$idx_cen == i,])))
  
}

# claculate dist grouped by features of attributes
# c = list(7,8:9,10:13,4:6,23:26,14,15,27,29:31,32:34,19:21,28,22,16:17,18)
# dist = 0
# for (i in c) {
#   dist = dist + dist_ij_without_entropy(1,2,spc_k1[,c(1:3,i)],1:3,k)
# }
# dist = dist/length(c)
# dist
# New distance calculation############################
#groups_list: attributes in same group would be scalar type (sn/sc)
#or vector type (mn/mc), not mixe up 
dist_two_data <- function(i,j,dat,groups_list,k) {
  sim = 0; count_na = 0 
  for (g in groups_list) {
    sim_g = sim_group(i,j,dat[g],k)
    if (sim_g == -1) {
      count_na = count_na + 1
    } else {
      sim = sim + sim_g
    }
  }
  dist = -log(sim/(length(groups_list)-count_na))
  return(dist)
}
# distance of one group of attributes between two data
sim_group <- function(i,j,dat,k) {
  sim = 0; count_na = 0
  for (m in 1 : length(dat)) {
    if (typeof(dat[[m]]) == 'list') { # is mn/mc group
      if (sum(is.na(dat[[m]][[i]]))/length(dat[[m]][[i]]) == 1 | 
          sum(is.na(dat[[m]][[j]]))/length(dat[[m]][[j]]) == 1) { # all NA value in vector i or j
        count_na = count_na + 1
      } else if (typeof(dat[[m]][[i]]) == 'character') { # is mc 
        sim = sim + sim_mc(i,j,dat[[m]],k)
      } else {
        sim_tem = sim_mn(i,j,dat[[m]])
        if (is.na(sim_tem)) { count_na = count_na+1}
        else {sim = sim + sim_tem }
      }
    } else { #sn/sc
      if (is.na(dat[[m]][[i]]) | is.na(dat[[m]][[j]])) {
        count_na = count_na + 1
      } else if (typeof(dat[[m]])=='character') {# is sc
        sim = sim + sim_sc(i,j,dat[[m]])
      } else {sim = sim + sim_sn(i,j,dat[[m]])
      }
    }
  }
  if (count_na == length(dat)) {sim = -1}
  else {sim = sim/(length(dat)-count_na)}
  return(sim)
}
#
sim_sn <- function(i,j,dat){
  #return(exp(-((dat[[i]] - dat[[j]]))^2/2))
  return(exp(-abs(dat[[i]] - dat[[j]])))
  #return((dat[[i]] - dat[[j]])^2)
  #return(abs(dat[[i]] - dat[[j]]))
}
#
sim_sc <- function(i,j,dat) {
  return(ifelse(dat[i]==dat[j],1,0))
}
#
sim_mn <- function(i,j,dat) {
  sim = 0;count_na = 0
  r_x = max(length(dat[[i]]), length(dat[[j]]))
  if(r_x == length(dat[[i]])) {
    x = i; y = j; r_y = length(dat[[j]])
  } else {
    x = j; y = i; r_y = length(dat[[i]])
  }
  for ( p in 1 : r_y ) {
    if (is.na(dat[[x]][[p]]) | is.na(dat[[y]][[p]])) {
      count_na = count_na + 1
    } else {
      sim = sim + exp(-abs(dat[[x]][[p]] - dat[[y]][[p]]))
      #sim = sim + exp(-((dat[[x]][[p]] - dat[[y]][[p]]))^2/2)
    }
  }
  if (!is.na(dat[[y]][[r_y]])) {
    q = r_y + 1
    while ( q <= r_x) {
      if (is.na(dat[[x]][[q]])) {
        count_na = count_na + 1
      } else {
        sim = sim + exp(-abs(dat[[x]][[q]] - dat[[y]][[r_y]]))
        #sim = sim + exp(-((dat[[x]][[q]] - dat[[y]][[r_y]]))^2/2)
      }
      q = q + 1
    }
  } else {count_na = count_na +(r_x-r_y)}
  if (count_na == r_x) {sim = NA}
  else {sim = sim/(r_x-count_na)} # ? sim = sim/r_x or something else
  return(sim)
}
sim_mc <- function(i,j,dat,k) {
  d_i = ngramrr(dat[[i]],ngmin = 1,ngmax = min(k,length(dat[[i]])))
  d_j = ngramrr(dat[[j]],ngmin = 1,ngmax = min(k,length(dat[[j]])))
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
# dist_mat
c = list(7,8:9,10:13,4:6,23:26,14,15,27,29:31,32:34,19:21,28,22,16:17,18)
dist_mat = data.frame(NA)
start_time = Sys.time()
for ( i in 1: nrow(spc_sample)) {
  for (j in min((i+1),nrow(spc_sample)): nrow(spc_sample)) {
    dist_mat[i,j] = dist_two_data(i,j,spc_sample,c,k)
    print(paste(i,j,dist_mat[i,j]))
  }
}
end_time = Sys.time()
end_time - start_time

# distance with specific formula####################
sim_formula <- function(i,j,dat,e,k) {
  sim = c()
  for ( m in 1 : (length(dat)-e)) {
    sim[m] = sim_2col(i,j,dat[[e+m]],k)
  }
  # sim[1] = sim_2col(i,j,dat[[e+1]],k)
  # sim[2] = sim_2col(i,j,dat[[e+2]],k)
  # sim[3] = sim_2col(i,j,dat[[e+3]],k)
  # sim[4] = sim_2col(i,j,dat[[e+4]],k)
  # sim[5] = sim_2col(i,j,dat[[e+5]],k)
  # sim[6] = sim_2col(i,j,dat[[e+6]],k)
  # sim[7] = sim_2col(i,j,dat[[e+7]],k)
  # sim[8] = sim_2col(i,j,dat[[e+8]],k)
  # sim[9] = sim_2col(i,j,dat[[e+10]],k)
  # sim[10] = sim_2col(i,j,dat[[e+11]],k)*sim_2col(i,j,dat[[e+12]],k)*sim_2col(i,j,dat[[e+13]],k)
  # sim[11] = sim_2col(i,j,dat[[e+14]],k)*sim_2col(i,j,dat[[e+15]],k)*sim_2col(i,j,dat[[e+16]],k)
  # sim[12] = sim_2col(i,j,dat[[e+17]],k)
  # sim[13] = sim_2col(i,j,dat[[e+18]],k)
  # sim[14] = sim_2col(i,j,dat[[e+19]],k)
  n = length(sim); n_nna = sum(!is.na(sim))
  sim[n+1] = sum(sim[!is.na(sim)])/ n # min(sim)
  sim[n+2] = sim[n+1] + (n-n_nna)/n  # max (sum(sim[!is.na(sim)]) +(length(sim)-sum(!is.na(sim)))) /length(sim)
  sim[n+3] = sim[n+1]*n/n_nna #mean_1  #sum(sim[!is.na(sim)])/sum(!is.na(sim))
  sim[n+4] = sim[n+1]*(1+(sim[n+2]-sim[n+1]))  # mean_2
  sim[n+5] = (sim[n+1]+sim[n+2])/2 #mena_3
  #sim =  -log(sim)
  #sim[sim == 'Inf'] = 100
  #sim[is.na(sim)] = -1
  return(sim)
}

# similarity of a single attribute between two data
sim_2col <- function(i,j,dat,k=2) {
  if (typeof(dat) == 'list') { # is mn/mc
    if (sum(is.na(dat[[i]]))/length(dat[[i]]) == 1 | 
        sum(is.na(dat[[j]]))/length(dat[[j]]) == 1) { # all NA value in vector i or j
      sim = NA
    } else if (typeof(dat[[i]]) == 'character') { # is mc 
      sim = sim_mc(i,j,dat,k)
    } else {sim = sim_mn(i,j,dat)}
  } else {
    if (is.na(dat[[i]]) | is.na(dat[[j]])) {
      sim = NA
    } else if (typeof(dat[[i]])=='character') {# is sc
      sim = sim_sc(i,j,dat)
    } else {sim = sim_sn(i,j,dat)
    }
  }
  return(sim)
}
# sim_mat and dist_mat ######################################################
sim_mat = rep(list(data.frame(matrix(NA_real_,nrow = 500,ncol = 500))),19)
start_time = Sys.time()
for ( i in 1: nrow(spc_sample)) {
  for (j in min((i+1),nrow(spc_sample)): nrow(spc_sample)) {
    sim_temp = sim_formula(i,j,spc_sample,3,k)
    for (m in 1: length(sim_temp)){
      sim_mat[[m]][[j]][[i]] = sim_temp[m]
      print(paste(m,i,j,sim_mat[[m]][i,j]))
    }
  }
}
end_time = Sys.time()
end_time - start_time

for (i in 1: length(sim_mat)) {
  for (j in 1 : nrow(sim_mat[[i]])) {
    sim_mat[[i]][j,j] = 1
    for (m in 1: max((j-1),1)) {
      sim_mat[[i]][j,m] = sim_mat[[i]][m,j]
    }
  }
}
dist_mat = rep(list(data.frame(matrix(NA_real_,nrow = 500,ncol = 500))),19)

for (i in 1: length(sim_mat)) {
  dist_mat[[i]] = -log(sim_mat[[i]])
  dist_mat[[i]][dist_mat[[i]] == 'Inf'] = 100
  dist_mat[[i]][is.na(dist_mat[[i]])] = -1
}
dist_mat

## SPC Centroid #################################################################
#calculate the centroid of a set of data
# input: dat- data set
# idx- the list of columns including idx and lable that will be excluded 
# k- k-gram expan for mc attributes, and the number of horizons to keep
#output: the centroid point of the data set

spc_centroid <- function(dat,idx,k) {
  centroid = data.frame(matrix(NA, ncol = ncol(dat)))
  colnames(centroid) = colnames(dat)
  for ( i in 1 : (length(dat)-idx)) {
    if (typeof(dat[[i+idx]]) == 'list') { # is mn/mc
      if (typeof(dat[[i+idx]][[1]]) == 'character') { # is mc 
        centroid[[i+idx]] = c_mc(dat[[i+idx]],k)
      } else { #mn
        centroid[[i+idx]] = c_mn(dat[[i+idx]],k)}
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

list_spc = count(spc_k1, "SPC")[order(count(spc_k1, "SPC")$freq,decreasing = T),]
nrow(a[!a$freq==1,])
nrow(a)
list_spc = unique(spc_k1$SPC)

centroid_spc_w = data.frame(matrix( ncol = ncol(spc_k1)))
colnames(centroid_spc_w) = colnames(spc_k1)
for (spc in list_spc ) {
  test = spc_k1[(!is.na(spc_k1$SPC) & spc_k1$SPC == spc ),]
  centroid_spc_w =ifelse(nrow(centroid_spc_w)==1,spc_centroid(test,3,5),
                         rbind(centroid_spc_w,spc_centroid(test,3,5)))  
  centroid_spc_w[nrow(centroid_spc_w),'SPC'] = spc
}
centroid_spc_w = centroid_spc_w[!is.na(centroid_spc_w$SPC),]
# sim_mat for centroid_spc
simmat_centroid_w = rep(list(data.frame(matrix(NA_real_,
                                               nrow = nrow(centroid_spc_w),ncol = nrow(centroid_spc_w)))),19)
start_time = Sys.time()
for ( i in 1: nrow(centroid_spc_w)) {
  for (j in min((i+1),nrow(centroid_spc_w)): nrow(centroid_spc_w)) {
    sim_temp = sim_formula(i,j,centroid_spc_w,3,k)
    for (m in 1: length(sim_temp)){
      simmat_centroid_w[[m]][[j]][[i]] = sim_temp[m]
      print(paste(m,i,j,simmat_centroid_w[[m]][i,j]))
    }
  }
}
end_time = Sys.time()
end_time - start_time
for (i in 1: length(simmat_centroid_w)) {
  for (j in 1 : nrow(simmat_centroid_w[[i]])) {
    simmat_centroid_w[[i]][j,j] = 1
    for (m in 1: max((j-1),1)) {
      simmat_centroid_w[[i]][j,m] = simmat_centroid_w[[i]][m,j]
    }
  }
}
spc_sim_list = list()
for (i in 1: length(simmat_centroid[[18]])) {
  cen_list = which(simmat_centroid[[18]][,i]>0.4)
  spc_sim_list[[i]] = centroid_spc[cen_list,]
  spc_sim_list[[i]] = spc_sim_list[[i]][order(simmat_centroid[[18]][cen_list,i],decreasing =T),]
}

# c_sc-calculate the centroid of a sc column data
# input: sc- a sc type column data
# output: the centroid value of this sc column data
# which is the list of most common domain values (single>10%, total<50%)
c_sc1 <- function(sc) {
  sc[is.na(sc)] = 'NA' # convert NA value to countable charecter "NA"
  d = unique(sc[!is.na(sc)]) # list of all domain values in sc
  if (length(d) > 1) { # calculate the density of every domain value in d
    f = vector(mode = 'list',length = length(d))
    names(f) = d
    for (i in d) {
      f[i] = sum(sc[!is.na(sc)]==i) 
    }
    centroid = c(); s = 0; l = length(sc)
    while (f[[which.max(f)]]/l >= 0.1 & s/l < 0.5)  {
      centroid = c(centroid, names(which.max(f)))
      s = s + f[[which.max(f)]]
      f = f[!names(f)== names(which.max(f))]
    }
    centroid[centroid =='NA'] = NA
    return(centroid)
  } else { return(ifelse(d == 'NA', NA, d))} 
}
# include NA , the max one
c_sc <- function(sc) {
  sc[is.na(sc)] = 'NA' # convert NA value to countable charecter "NA"
  d = unique(sc[!is.na(sc)]) # list of all domain values in sc
  if (length(d) > 1) { # calculate the density of every domain value in d
    f = vector(mode = 'list',length = length(d))
    names(f) = d
    for (i in d) {
      f[i] = sum(sc[!is.na(sc)]==i) 
    }
    return(ifelse(names(which.max(f)) =='NA',NA ,names(which.max(f)) ))
   
  } else { return(ifelse(d == 'NA', NA, d))} 
}
cen_status = c_sc(spc_k1$STATUS)
spc_test = append(spc_k1$STATUS , list(cen_status))
spc_test[92308]
sim_status=0; count = 0
for (i in 1: (length(spc_test)-1)) {
  temp = sim_2col(i,length(spc_test),spc_test)
  if (!is.na(temp)) {
    sim_status = sim_status + temp
    count = count + 1
  }
}

sim_status = sim_status/count


# c_sn
c_sn <- function(sn) {
  d = unique(as.character(as.double(sn[!is.na(sn)])))
  #d = as.character(unique(as.double(sn[!is.na(sn)])))
  sim_sn = vector(mode = 'list',length = length(d))
  names(sim_sn) = d
  if (!is_empty(d)) {
    for (i in d) {
      sn_temp = append(sn,as.double(i))
      sim = 0
      count = length(sn[!is.na(sn)])
      for (j in 1:length(sn)) {
        temp = sim_2col(j,length(sn_temp),sn_temp)
        sim = ifelse(is.na(temp),sim,sim+temp)
      }
      sim_sn[i] = sim/count
    }
    return(as.double(names(which.max(sim_sn))))
  } else { return(NA)}
}
# max similarity value
c_mc <- function(mc,k) {
  domain_list = c()
  for (i in 1 : length(mc)) {
    domain_list = c(domain_list, 
                    ngramrr(append(mc[[i]],rep(NA,max(0,k-length(mc[[i]])))) ,ngmin=k,ngmax=k))
    #   if (length(mc[[i]])>= k) {
    #     domain_list = c(domain_list, 
    #                     ngramrr(append(mc[[i]],rep(NA,max(0,k-length(mc[[i]])))) ,ngmin=k,ngmax=k))
    #   }
  }
  if (!is.null(domain_list)) {
    #domain_list = unique(domain_list)
    domain_list = sort(table(domain_list),decreasing = T)
    domain_list = domain_list[1:min(length(domain_list),max(10,round(length(domain_list)/10)))]
    sim_mc = vector(mode = 'list',length = length(domain_list))
    #names(sim_mc ) = domain_list
    names(sim_mc ) = names(domain_list)
    for (i in 1: length(domain_list)) {
      #mc_temp = append(mc,domain_list[i])
      mc_temp = append(mc,str_split(names(domain_list[i]),' '))
      sim = 0
      #count =
      for (j in 1: length(mc) ) {
        temp = sim_2col(j,length(mc_temp),mc_temp,k)
        sim = ifelse(is.na(temp),sim,sim+temp)
      }
      sim_mc[i] = sim
    }
    return(str_split(names(which.max(sim_mc)),' '))
  }
  return(list(rep(NA,k)))
  
}
# based on density
c_mc1 <- function(mc,k) {
  domain_max = c()
  for (l in 1:k) {
    domain_list = c()
    for (i in 1:length(mc)) {
      if (length(mc[[i]])>= l) {
        domain_list = c(domain_list, ngramrr(mc[[i]],ngmin=l,ngmax=l))
      }
      #print(paste(l,i))
    }
    domain_list = sort(table(domain_list),decreasing = T)
    if ( l == 1) { 
      domain_max[l] = names(domain_list[1])}
    else {
      m = 1
      while (!domain_max[l-1] %in%
             get.ngrams(ngram(names(domain_list)[m],n=wordcount(names(domain_list)[m])-1))) {
        m = m + 1
      }
      domain_max[l] = names(domain_list[m])
      #print(domain_max[l])
    }
  }
  return(domain_max[k])
}

c_mn <-function(mn,k) {
  cen_mn = c()
  for (l in 1 : k) {
    temp = c()
    for (i in 1 : length(mn)) {
      temp[i] = mn[[i]][l]
    }
    cen_mn[l] = c_sn(temp)
  }
  return(list(cen_mn))
}
c_mn(spc_sample$PEDALITY_GRADE,5)
a = spc_sample$PEDALITY_GRADE
sim_mnn = c()
for (i in 1:5) {
  for(j in 1:5) {
    for(l in 1:5) {
      for(m in 1:5) {
        for(n in 1:5) {
          mn_temp = append(a,list(c(i,j,l,m,n)))
          sim = 0
          for (p in 1: (length(a)-1)) {
            temp = sim_2col(p,length(mn_temp),mn_temp)
            sim = ifelse(is.na(temp),sim,sim+temp)
          }
          sim_mnn = rbind(sim_mnn,c(mn_temp[[501]],sim))
          print(sim_mnn[[length(sim_mnn)]])
        }
      }
    }
  }
}
sim_mnn = sim_mnn[order(sim_mnn[,6],decreasing = T),]

