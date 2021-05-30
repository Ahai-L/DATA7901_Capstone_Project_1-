# ASC predicting base on Red book
# Author: Leo Lu, University of Queensland

# Library & Datasets#############################################
library (magrittr)
library(sqldf)
library(dplyr)
library(caret)

#INPORT source data sets (SALI & SALI decode files)
SALI_de <- readRDS("SALI_SIT-data_decodes_20191101.rds")
SALI <-readRDS("SALI_SIT-data_20191101.rds")
soil_sample <- read.csv("samples.csv")
lab <- read.csv("labresults.csv")
# ?explain the code of "RESULT_STATUS"
lab1 <- lab[,c("PROJECT_CODE","SITE_ID","OBS_NO","HORIZON_NO","SAMPLE_NO",
               "LAB_METH_CODE","RESULT_STATUS","RAW_VALUE","NUMERIC_VALUE")]

# subset data sets from SALI##################################################
sit <- SALI$SIT

obs <- SALI$OBS 
obs <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,obs,min) %>%
  left_join(.,obs,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))

osc <- SALI$OSC 
osc <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,osc,min) %>%
  left_join(.,osc,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
osc <-  aggregate(SURF_COND_NO~PROJECT_CODE+SITE_ID+OBS_NO,osc,min) %>%
  left_join(.,osc,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO","SURF_COND_NO"))



# nrow(unique(osc[,1:2])) #91365
# nrow(unique(osc[,1:3])) #91365
# nrow(unique(osc[,1:4])) #91365

# obs_1 <- obs[, c('PROJECT_CODE','SITE_ID','OBS_NO','TAX_UNIT_TYPE','TAX_UNIT_CODE')]
# length(unique(obs_1[which(obs_1$TAX_UNIT_TYPE =='SPC'),c("TAX_UNIT_CODE")])) #1862
# #number of soil data without a SPC value
# nrow(unique(obs[which(obs$TAX_UNIT_TYPE == 'SPC' & is.na(obs$TAX_UNIT_CODE)),1:2]))

#OCL Soil classification
ocl <- SALI$OCL
ocl <- aggregate(OBS_NO~PROJECT_CODE+SITE_ID,ocl,min) %>%
  left_join(.,ocl,by=c("PROJECT_CODE" = "PROJECT_CODE" , "SITE_ID" = "SITE_ID","OBS_NO"="OBS_NO"))
#eliminate duplcates with different SOIL_CLASS_NO, only keep the one SOIL_CLASS_NO == MIN(MOST ARE 1)
ocl <- aggregate(SOIL_CLASS_NO~PROJECT_CODE+SITE_ID,ocl,min) %>%
  left_join(.,ocl,by=c("PROJECT_CODE" , "SITE_ID","SOIL_CLASS_NO"))
# HOR
hor <- SALI$HOR
hor <- aggregate(OBS_NO~PROJECT_CODE+SITE_ID,hor,min) %>%
  left_join(.,hor,by=c("PROJECT_CODE" = "PROJECT_CODE" , "SITE_ID" = "SITE_ID","OBS_NO"="OBS_NO"))
 
hcu <- SALI$HCU
hcu <- aggregate(OBS_NO~PROJECT_CODE+SITE_ID,hcu,min) %>%
  left_join(.,hcu,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
hcu <- aggregate(CUTAN_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,hcu,min) %>%
  left_join(.,hcu,by=c("PROJECT_CODE" ,"SITE_ID","OBS_NO",'HORIZON_NO',"CUTAN_NO"))

# hcu_confuse <- sqldf('SELECT a.PROJECT_CODE, a.SITE_ID, a.OBS_NO, a.HORIZON_NO,a.CUTAN_NO
#               FROM hcu a
#               JOIN hcu b
#               on b.PROJECT_CODE = a.PROJECT_CODE
#               AND b.SITE_ID = a.SITE_ID
#               AND b.OBS_NO = a.OBS_NO
#               AND b.HORIZON_NO = a.HORIZON_NO
#               AND b.CUTAN_NO <> a.CUTAN_NO
#               ORDER BY a.PROJECT_CODE,a.SITE_ID')

hst <- SALI$HST
hst <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,hst,min) %>%
  left_join(.,hst,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
hst <- aggregate(STRUCT_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,hst,min) %>%
  left_join(.,hst,by=c("PROJECT_CODE" ,"SITE_ID","OBS_NO",'HORIZON_NO',"STRUCT_NO"))

hsg <- SALI$HSG
hsg <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,hsg,min) %>%
  left_join(.,hsg,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
hsg <- aggregate(SEG_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,hsg,min) %>%
  left_join(.,hsg,by=c("PROJECT_CODE" ,"SITE_ID","OBS_NO",'HORIZON_NO',"SEG_NO"))

lab_15n1 <- lab[which(lab$LAB_METH_CODE == '15N1' & !is.na(lab$NUMERIC_VALUE)), 
                c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_CODE','NUMERIC_VALUE')]
names(lab_15n1)[7] <- "VALUE_15N1"
# use mean value if multiple values available for one horizon 
lab_15n1<-  aggregate(VALUE_15N1~PROJECT_CODE+SITE_ID+HORIZON_NO,lab_15n1,mean)


lab_13c1_fe <- lab[which(lab$LAB_METH_CODE == '13C1_Fe' & !is.na(lab$NUMERIC_VALUE)), 
                c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_CODE','NUMERIC_VALUE')]
names(lab_13c1_fe)[7] <- "VALUE_13C1_Fe"
# use mean value if multiple values available for one horizon 
lab_13c1_fe<-  aggregate(VALUE_13C1_Fe~PROJECT_CODE+SITE_ID+HORIZON_NO,lab_13c1_fe,mean)


lab_4a1 <- lab[which(lab$LAB_METH_CODE == '4A1' & !is.na(lab$NUMERIC_VALUE)), 
                   c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_CODE','NUMERIC_VALUE')]
names(lab_4a1)[7] <- "VALUE_4A1"
# use mean value if multiple values available for one horizon 
lab_4a1<-  aggregate(VALUE_4A1~PROJECT_CODE+SITE_ID+HORIZON_NO,lab_4a1,mean)

lab_2z2_clay <- lab[which(lab$LAB_METH_CODE == '2Z2_Clay' & !is.na(lab$NUMERIC_VALUE)), 
               c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_CODE','NUMERIC_VALUE')]
names(lab_2z2_clay)[7] <- "VALUE_2Z2_Clay"
# use mean value if multiple values available for one horizon 
lab_2z2_clay<-  aggregate(VALUE_2Z2_Clay~PROJECT_CODE+SITE_ID+HORIZON_NO,lab_2z2_clay,mean)


 # nrow(unique(lab_15n1[,1:2])) #91365
 # nrow(unique(lab_15n1[,1:3])) #91365
 # nrow(unique(lab_15n1[,c(1,2,3,4,5)])) #91365
 # nrow(unique(lab_15n1[,c(1,2,3,4)]))

# Functions  Subclass #################################### 

#data:includes c1:PROJECT_CODE,c2:SITE-ID
AL <- function(data,drain=data$DRAINAGE,perm=data$PERMEABILITY) {
  al <- drain >= 1 & perm >= 1
  #al <- c(which(drain %in% c(5,6,7) & perm %in% c(3,4)))
  # cbind(data,.)
  return(al)
}
#function....
AE <- function() {
  
}

# ASC-OR-Organosol ###############################################################
# datasets with non null value in attributes for judging OR (OBS_N0 == 1 ONLY ?) 
# ? how to exclude a data whic has some horizons with null values? 
or_nnull <- sit[which(sit$ELEM_TYPE_CODE != '-'),c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')] %>%
  anti_join(.,hor[which(hor$DESIGN_MASTER == '-'|is.na(hor$DESIGN_MASTER)),
                        c('PROJECT_CODE','SITE_ID','HORIZON_NO','DESIGN_MASTER','UPPER_DEPTH','LOWER_DEPTH')],
             by = c('PROJECT_CODE','SITE_ID')) %>%
  inner_join(.,hor[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','DESIGN_MASTER','UPPER_DEPTH','LOWER_DEPTH')],
            by = c('PROJECT_CODE','SITE_ID'))

sum(is.na(or_nnull_1))
# dataset with null value in attributes for judging OR, and is labeled with OR
or_null <- filter(ocl[,c(1,2,7)], ASC_ORD == 'OR') %>%
  anti_join(.,or_nnull,by = c('PROJECT_CODE','SITE_ID')) %>%
  left_join(.,or_nnull,by = c('PROJECT_CODE','SITE_ID'))

# or_1: not regularly inundated by saline tidal waters
or_1 <- or_nnull[which(!(or_nnull$ELEM_TYPE_CODE %in% c('TDF','ITF','LAG'))),] %>%
  distinct(.,PROJECT_CODE,SITE_ID)

#subset of HOR, HORIZON with "O" or "P", and UPPER_DEPTH within 0.8m
or_2 <- filter(or_nnull, DESIGN_MASTER %in% c('O','P'), UPPER_DEPTH < 0.8)
#Trim the LOWER_DEPTH more than 0.8m to 0.8m
or_2$LOWER_DEPTH <- pmin(or_2$LOWER_DEPTH,rep(0.8,nrow(or_2)))
#Calculate the thickness of each horizon
or_2$THICKNESS <- or_2$LOWER_DEPTH -or_2$UPPER_DEPTH
#sum up the thickness of horizons for same profile -385
or_2<-aggregate(THICKNESS ~ PROJECT_CODE+SITE_ID, data = or_2,FUN = sum)
#record only thicker than 0.4m -30
or_2 <- or_2[which(or_2$THICKNESS > 0.4),1:2] %>%
  distinct(.,PROJECT_CODE,SITE_ID)

# or_3: Have organic materials extending from the surface to a minimum depth of 0.1 m, INC "AB"?*
or_3 <- filter(or_nnull, DESIGN_MASTER %in% c('O','P'), HORIZON_NO == 1,
               LOWER_DEPTH - UPPER_DEPTH >=0.1) %>%
  distinct(.,PROJECT_CODE,SITE_ID)
              
# or_1 and (or_2 or or_3)
# or_p <- full_join(or_2, or_3,by = c('PROJECT_CODE','SITE_ID')) %>%
#   inner_join(.,or_1,by = c('PROJECT_CODE','SITE_ID'))
or_p <- full_join(or_2, or_3,by = c('PROJECT_CODE','SITE_ID')) 
or_p <-  inner_join(or_p,or_1,by = c('PROJECT_CODE','SITE_ID'))

#add a column to or
or_p$ASC_ORD_P <- rep("OR",nrow(or_p))

all_p <- or_p
# Confusion matrix

# or_nnull distinct with project_code and site_id, and add asc_ord from ocl
or_vat <- or_nnull %>%
  distinct(.,PROJECT_CODE,SITE_ID) %>%
  left_join(.,ocl[,c(1,2,7)],by = c('PROJECT_CODE','SITE_ID')) 
# unique check
nrow(unique(or_vat[,1:2])) == nrow(unique(or_vat[,1:3]))
#join the predict data
or_vat <- left_join(or_vat,or_p,by = c('PROJECT_CODE','SITE_ID'))
# mark other orders except "OR" in ASC_ORD with "OTHERS",
or_vat[which(!is.na(or_vat$ASC_ORD) & or_vat$ASC_ORD != 'OR'),'ASC_ORD'] = 'OTHERS'
# NA in asc_ord as "N/A"
or_vat[is.na(or_vat)]<-"N/A"
# value in ASC_ORD_P except for 'OR', and is "OTHERS" in ASC_ORD, mark as "OTHERS" as well
or_vat[which(or_vat$ASC_ORD == "OTHERS" & or_vat$ASC_ORD_P != 'OR'),'ASC_ORD_P'] = 'OTHERS'
# feed the data to confusionmatrix
or_confmat <- confusionMatrix(as.factor(or_vat$ASC_ORD_P),reference = as.factor(or_vat$ASC_ORD))
print(or_confmat)

# similar as above but tick out the "N/A" values in ASC_ORD
or_vat_1 <- or_nnull %>%
  distinct(.,PROJECT_CODE,SITE_ID) %>%
  left_join(.,ocl[,c(1,2,7)],by = c('PROJECT_CODE','SITE_ID')) 
or_vat_1 <- or_vat_1[which(!is.na(or_vat_1$ASC_ORD)),]
nrow(unique(or_vat_1[,1:2])) == nrow(unique(or_vat_1[,1:3]))
or_vat_1 <- left_join(or_vat_1,or_p,by = c('PROJECT_CODE','SITE_ID'))
or_vat_1[which(or_vat_1$ASC_ORD != 'OR'),'ASC_ORD'] = 'OTHERS'
or_vat_1[is.na(or_vat_1)]<-"OTHERS"
or_confmat_1 <- confusionMatrix(as.factor(or_vat_1$ASC_ORD_P),reference = as.factor(or_vat_1$ASC_ORD))
print(or_confmat_1)

# write to files
or_fp_fn <- filter(or_vat_1,ASC_ORD != ASC_ORD_P) 
or_fp_fn <- sqldf('select A.PROJECT_CODE, A.SITE_ID, B.ASC_ORD, A.ASC_ORD_P
        FROM or_fp_fn A, ocl B
        WHERE A.PROJECT_CODE = B.PROJECT_CODE AND
        A.SITE_ID = B.SITE_ID')
or_fp_fn[which(is.na(or_fp_fn)),]  

or_fp_fn_hor <- left_join(or_fp_fn,hor[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','DESIGN_MASTER',
                                               'UPPER_DEPTH','LOWER_DEPTH')], by = c('PROJECT_CODE','SITE_ID') ) %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')],by = c('PROJECT_CODE','SITE_ID'))

or_fp_fn_hor[which(is.na(or_fp_fn_hor)),]

or_predict <- filter(or_vat,ASC_ORD =="N/A",ASC_ORD_P == 'OR' )

write.csv(or_fp_fn, file =paste("C:/Users/lzccn/Downloads/des/files/or_fp_fn",
                                format(Sys.time(),"%d-%b-%Y %H.%M"), ".csv",sep = " "))
write.csv(or_fp_fn_hor, file =paste("C:/Users/lzccn/Downloads/des/files/or_fp_fn_hor",
                               format(Sys.time(),"%d-%b-%Y %H.%M"),".csv",sep = " "))
write.csv(or_predict, file =paste("C:/Users/lzccn/Downloads/des/files/or_predict",
                                  format(Sys.time(),"%d-%b-%Y %H.%M"), ".csv",sep = " "))
write.csv(or_null, file =paste("C:/Users/lzccn/Downloads/des/files/or_null",
                                  format(Sys.time(),"%d-%b-%Y %H.%M"), ".csv",sep = " "))
# #Suborder
# #or_p <- left_join(or_p,or_xx,by=c("PROJECT_CODE","SITE_ID"))
# or_p[,"SUBORD_P"] <- NA
# #GreatGroup
# or_p[,"GREAT_GROUP_P"] <- NA
# #SubGroup
# or_p[,"SUBGROUP_P"] <- NA

# ASC-PO- Podosols ###############################################################
# datasets with non null value in attributes for judging PO (OBS_N0 == 1 ONLY ?) 
# ? how to exclude a data whic has some horizons with null values? 
po_nnull <- anti_join(hor[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','DESIGN_MASTER','HORIZON_NAME')],
                      hor[which(hor$DESIGN_MASTER == '-'|is.na(hor$DESIGN_MASTER) ),
                        c('PROJECT_CODE','SITE_ID','HORIZON_NO','DESIGN_MASTER','HORIZON_NAME')],
                      by= c('PROJECT_CODE','SITE_ID')) %>%
  anti_join(.,all_p,by= c('PROJECT_CODE','SITE_ID'))
sum(is.na(po_nnull))
# dataset with null value in attributes for judging OR, and is labeled with OR
po_null <- filter(ocl[,c(1,2,7)], ASC_ORD == 'PO') %>%
  anti_join(.,po_nnull,by = c('PROJECT_CODE','SITE_ID')) %>%
  left_join(.,po_nnull,by = c('PROJECT_CODE','SITE_ID'))

unique(po_nnull$DESIGN_MASTER)
nrow(po_nnull[which(po_nnull$DESIGN_MASTER == 'B?'),])
#HOR$DESIGN_MASTER == "B",  and
po_p <- po_nnull[which(po_nnull$DESIGN_MASTER %in% c("B"),),]
# HOR$HORIZON_NAME contains "s" or "h"
unique(po_nnull$HORIZON_NAME)
po_p <- po_p[which(grepl("+h|+s|+hs",po_p$HORIZON_NAME)),] %>%
  distinct(PROJECT_CODE,SITE_ID)

#add a column to or
po_p$ASC_ORD_P <- rep("PO",nrow(po_p))

all_p <- full_join(all_p,po_p)
# Confusion matrix

# po_nnull distinct with project_code and site_id, and add asc_ord from ocl
ve_vat <- po_nnull %>%
  distinct(.,PROJECT_CODE,SITE_ID) %>%
  left_join(.,ocl[,c(1,2,7)],by = c('PROJECT_CODE','SITE_ID')) 
# unique check
nrow(unique(ve_vat[,1:2])) == nrow(unique(ve_vat[,1:3]))
#join the predict data
ve_vat <- left_join(ve_vat,po_p,by = c('PROJECT_CODE','SITE_ID'))
# mark other orders except "PO" in ASC_ORD with "OTHERS",
ve_vat[which(!is.na(ve_vat$ASC_ORD) & ve_vat$ASC_ORD != 'PO'),'ASC_ORD'] = 'OTHERS'
# NA in asc_ord as "N/A"
ve_vat[is.na(ve_vat)]<-"N/A"
# value in ASC_ORD_P except for 'PO', and is "OTHERS" in ASC_ORD, mark as "OTHERS" as well
ve_vat[which(ve_vat$ASC_ORD == "OTHERS" & ve_vat$ASC_ORD_P != 'PO'),'ASC_ORD_P'] = 'OTHERS'
# feed the data to confusionmatrix
po_confmat <- confusionMatrix(as.factor(ve_vat$ASC_ORD_P),reference = as.factor(ve_vat$ASC_ORD))
print(po_confmat)

# similar as above but tick out the "N/A" values in ASC_ORD
po_vat_1 <- po_nnull %>%
  distinct(.,PROJECT_CODE,SITE_ID) %>%
  left_join(.,ocl[,c(1,2,7)],by = c('PROJECT_CODE','SITE_ID')) 
po_vat_1 <- po_vat_1[which(!is.na(po_vat_1$ASC_ORD)),]
nrow(unique(po_vat_1[,1:2])) == nrow(unique(po_vat_1[,1:3]))
po_vat_1 <- left_join(po_vat_1,po_p,by = c('PROJECT_CODE','SITE_ID'))
po_vat_1[which(po_vat_1$ASC_ORD != 'PO'),'ASC_ORD'] = 'OTHERS'
po_vat_1[is.na(po_vat_1)]<-"OTHERS"
po_confmat_1 <- confusionMatrix(as.factor(po_vat_1$ASC_ORD_P),reference = as.factor(po_vat_1$ASC_ORD))
print(po_confmat_1)

# write to files
po_tp <- filter(po_vat_1,ASC_ORD == 'PO',ASC_ORD_P == 'PO') 
po_tp_hor <- left_join(po_tp,hor[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','DESIGN_MASTER',
                                               'HORIZON_NAME')], by = c('PROJECT_CODE','SITE_ID') )
po_fp_fn <- filter(po_vat_1,ASC_ORD != ASC_ORD_P) 
po_fp_fn <- sqldf('select A.PROJECT_CODE, A.SITE_ID, B.ASC_ORD, A.ASC_ORD_P
        FROM po_fp_fn A, ocl B
        WHERE A.PROJECT_CODE = B.PROJECT_CODE AND
        A.SITE_ID = B.SITE_ID')
po_fp_fn[which(is.na(po_fp_fn)),]  

po_fp_fn_hor <- left_join(po_fp_fn,hor[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','DESIGN_MASTER',
                                               'HORIZON_NAME')], by = c('PROJECT_CODE','SITE_ID') )
  

# po_fp_fn_hor[which(is.na(po_fp_fn_hor)),]

po_predict <- filter(po_vat,ASC_ORD =="N/A",ASC_ORD_P == 'PO' )

write.csv(po_fp_fn, file =paste("C:/Users/lzccn/Downloads/des/files/po_fp_fn",
                                format(Sys.time(),"%d-%b-%Y %H.%M"), ".csv",sep = " "))
write.csv(po_fp_fn_hor, file =paste("C:/Users/lzccn/Downloads/des/files/po_fp_fn_hor",
                                    format(Sys.time(),"%d-%b-%Y %H.%M"),".csv",sep = " "))
write.csv(po_predict, file =paste("C:/Users/lzccn/Downloads/des/files/po_predict",
                                  format(Sys.time(),"%d-%b-%Y %H.%M"), ".csv",sep = " "))
write.csv(po_null, file =paste("C:/Users/lzccn/Downloads/des/files/po_null",
                               format(Sys.time(),"%d-%b-%Y %H.%M"), ".csv",sep = " "))

# #Suborder of PO
# 
# #Suborder
# po_p[,"SUBORD_P"] <- NA
# 
# # AL
# po_al <- left_join(po_p[,1:2],hor[,c('PROJECT_CODE',"SITE_ID", "HORIZON_NO" ,"DRAINAGE","PERMEABILITY")],
#                    by=c("PROJECT_CODE","SITE_ID"))
# po_al$SUBORD_P <-AL(po_al)
# po_al <- aggregate(SUBORD_P~PROJECT_CODE+SITE_ID, po_al, all)
# po_al[po_al$SUBORD_P == "TRUE", "SUBORD_P"]<- "AL"
# po_al <- left_join(po_p[,1:2],po_al,by=c("PROJECT_CODE","SITE_ID"))
# po_al <- po_al[order(po_al[,1],po_al[,2]),]
# po_p$SUBORD_P <- ifelse(!is.na(po_al$SUBORD_P) & is.na(po_p$SUBORD_P), 
#                         po_al$SUBORD_P, po_p$SUBORD_P) ## Need to check !
# 
# #GreatGroup
# po_p[,"GREAT_GROUP_P"] <- NA
# #SubGroup
# po_p[,"SUBGROUP_P"] <- NA

# ASC-VE- Vertosols ########################################
# datasets with non null value in attributes for judging VE (OBS_N0 == 1 ONLY ?) 
# ? how to exclude a data whic has some horizons with null values? 
ve_nnull[which(ve_nnull$PROJECT_CODE=='CARB01'& ve_nnull$SITE_ID =="1"),]
# hcu[which(hcu$PROJECT_CODE=='CARB01'& hcu$SITE_ID =="1"),]
ve_nnull <- hor[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','DESIGN_MASTER',
                      'HORIZON_NAME','UPPER_DEPTH','LOWER_DEPTH','TEXTURE_CODE','SOIL_WATER_STAT')] %>%
  left_join(.,osc[,c('PROJECT_CODE','SITE_ID','OBS_NO','STATUS')],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO') ) %>%
  left_join(.,hcu[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','CUTAN_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','STRUCT_NO','PEDALITY_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')],by = c('PROJECT_CODE','SITE_ID')) %>%
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')],by = c('PROJECT_CODE','SITE_ID','OBS_NO')) %>%
  left_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  anti_join(.,.[which(.$DESIGN_MASTER=='-'|is.na(.$DESIGN_MASTER)|
                      .$SOIL_WATER_STAT == '-' | is.na(.$SOIL_WATER_STAT)|
                      .$TEXTURE_CODE == '-' | is.na(.$TEXTURE_CODE)|
                      .$STATUS == '-' | is.na(.$STATUS)|
                      .$CUTAN_TYPE == '-' | is.na(.$CUTAN_TYPE)|
                      .$PEDALITY_TYPE == '-' | is.na(.$PEDALITY_TYPE)|
                      .$ELEM_TYPE_CODE == '-' | is.na(.$ELEM_TYPE_CODE)
                      ),],by = c('PROJECT_CODE','SITE_ID'))




# ve_nnull <- anti_join(hor[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','DESIGN_MASTER',
#                  'HORIZON_NAME','UPPER_DEPTH','LOWER_DEPTH','TEXTURE_CODE','SOIL_WATER_STAT')],
#           hor[which((hor$DESIGN_MASTER == '-'|is.na(hor$DESIGN_MASTER)) | 
#                       (hor$SOIL_WATER_STAT == '-'|is.na(hor$SOIL_WATER_STAT))),],
#           by= c('PROJECT_CODE','SITE_ID')) %>%
#   inner_join(.,osc[which(osc$STATUS != '-' & !is.na(osc$STATUS)),
#                    c('PROJECT_CODE','SITE_ID','OBS_NO','STATUS')],
#              by = c('PROJECT_CODE','SITE_ID','OBS_NO')) %>%
#   inner_join(.,anti_join(hcu[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','CUTAN_TYPE')],
#                          hcu[which(hcu$CUTAN_TYPE == '-'|is.na(hcu$CUTAN_TYPE)),],
#              by = c('PROJECT_CODE','SITE_ID','OBS_NO'))) #%>%
#   # inner_join(.,anti_join(hst[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','STRUCT_NO','PEDALITY_TYPE')],
#   #                        hst[which(hst$PEDALITY_TYPE == '-'|is.na(hst$PEDALITY_TYPE)),],
#   #            by = c('PROJECT_CODE','SITE_ID','OBS_NO'))) %>%
#   # inner_join(.,sit[which(sit$ELEM_TYPE_CODE != '-'),
#   #                  c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')],
#   #            by = c('PROJECT_CODE','SITE_ID')) %>%
#   # inner_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')],
#   #            by = c('PROJECT_CODE','SITE_ID','OBS_NO')) %>%
#   # left_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
#   # anti_join(.,.[which((.$TEXTURE_CODE == '-'| is.na(.$TEXTURE_CODE)) & is.na(.$VALUE_2Z2_Clay)),],
#   #           by = c('PROJECT_CODE','SITE_ID','OBS_NO'))

# nrow(unique(ve_nnull[,1:2]))
# nrow(ve_nnull[which(ve_nnull$HORIZON_NO ==1),])
# nrow(ve_nnull[which(ve_nnull$HORIZON_NO ==2 & ve_nnull$UPPER_DEPTH <=0),])
# nrow(ve_nnull[which(ve_nnull$HORIZON_NO ==1 & ve_nnull$UPPER_DEPTH >0),])
# nrow(unique(hor[,1:2]))
# nrow(hor[which(hor$HORIZON_NO ==1),])
# nrow(unique(SALI$HOR[,1:2]))
# nrow(hor[which(SALI$HOR$HORIZON_NO ==1),])
# dataset with null value in attributes for judging OR, and is labeled with OR
ve_null <- filter(ocl[,c(1,2,7)], ASC_ORD == 'VE') %>%
  anti_join(.,ve_nnull,by = c('PROJECT_CODE','SITE_ID')) %>%
  left_join(.,hor[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','DESIGN_MASTER',
                    'HORIZON_NAME','UPPER_DEPTH','LOWER_DEPTH','TEXTURE_CODE','SOIL_WATER_STAT'),],
            by = c('PROJECT_CODE','SITE_ID')) %>%
  left_join(.,osc[,c('PROJECT_CODE','SITE_ID','OBS_NO','STATUS'),],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO')) %>%
  left_join(.,hcu[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','CUTAN_TYPE'),],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','STRUCT_NO','PEDALITY_TYPE'),],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE'),],
            by = c('PROJECT_CODE','SITE_ID')) %>%
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE'),],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO')) %>%
  left_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO'))
  



#ve_1
# ve_11 : horizons in the surface with crusty horizon and 0.03m or less thick LEVEL:HOR

ve_11 <- filter(ve_nnull, (HORIZON_NO == 1|UPPER_DEPTH ==0), LOWER_DEPTH-UPPER_DEPTH <=0.03,STATUS == 'C')
# horizons in the surface and have 35% or more clay
unique(ve_nnull$TEXTURE_CODE)
ve_12 <- filter(ve_nnull,HORIZON_NO == 1,VALUE_2Z2_Clay >=35) %>%
  full_join(.,filter(ve_nnull,HORIZON_NO == 1,grepl('LC|LMC|MC|MHC|HC',ve_nnull$TEXTURE_CODE)))
# ve_12 <- filter(ve_nnull,HORIZON_NO == 1,TEXTURE_CODE %in% c('LC', 'LMC','MC','MHC','HC','KLC','C'))
# All Solums except surface horizon, with a clay field texture or 35% or more clay
ve_13 <- filter(ve_nnull, HORIZON_NO != 1 ) %>%
  group_by(PROJECT_CODE,SITE_ID) %>%
  filter(all(VALUE_2Z2_Clay >=35 |grep('LC|LMC|MC|MHC|HC',ve_nnull$TEXTURE_CODE))) 
# ve_13 <- filter(ve_nnull, HORIZON_NO != 1 ) %>%
#   group_by(PROJECT_CODE,SITE_ID) %>%
#   filter(all(TEXTURE_CODE %in% c('LC', 'LMC','MC','MHC','HC','KLC','C'))) %>%
#   distinct(PROJECT_CODE,SITE_ID)
# 
ve_1 <- full_join(ve_11,ve_12,by = c('PROJECT_CODE','SITE_ID')) %>%
  inner_join(.,ve_13,by = c('PROJECT_CODE','SITE_ID')) %>%
  distinct(PROJECT_CODE,SITE_ID)
nrow(unique(ve_1[,1:2]))  

#ve_2 
# when dry
ve_21 <- ve_nnull[which(!ve_nnull$SOIL_WATER_STAT %in% c('M','W')),]
# open cracks occur at some time in most years. These are at least 5 mm wide and extend 
# upward to the surface or to the base of any plough layer, peaty horizon, self-mulching
# horizon, or thin, surface crusty horizon
ve_22 <- full_join(ve_nnull[which(ve_nnull$STATUS %in% c('G','M','C')),], 
                   ve_nnull[which(grepl("ap|p",ve_nnull$HORIZON_NAME) & ve_nnull$DESIGN_MASTER == 'A'),],
                   by = c('PROJECT_CODE','SITE_ID')) 
# ve_2: Not match the cracking conditons when dry
ve_2 <- inner_join(ve_21,ve_22,by = c('PROJECT_CODE','SITE_ID')) %>%
  distinct(PROJECT_CODE,SITE_ID)
nrow(unique(ve_1[,1:2]))  

#ve_3 Slickensides and/or lenticular peds occur at some depth in the solum 
ve_3 <- full_join(filter(ve_nnull, CUTAN_TYPE == 'K'),
                  filter(ve_nnull, PEDALITY_TYPE == 'LE'),
                  by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) 

ve_p <- inner_join(ve_1, ve_2,by = c('PROJECT_CODE','SITE_ID')) %>%
        inner_join(.,ve_3,by = c('PROJECT_CODE','SITE_ID'))


#add a column to or
ve_p$ASC_ORD_P <- rep("VE",nrow(ve_p))

all_p <- full_join(all_p,ve_p)


# Confusion matrix

# ve_nnull distinct with project_code and site_id, and add asc_ord from ocl
ve_vat <- ve_nnull %>%
  distinct(.,PROJECT_CODE,SITE_ID) %>%
  left_join(.,ocl[,c(1,2,7)],by = c('PROJECT_CODE','SITE_ID')) 
# unique check
nrow(unique(ve_vat[,1:2])) == nrow(unique(ve_vat[,1:3]))
#join the predict data
ve_vat <- left_join(ve_vat,ve_p,by = c('PROJECT_CODE','SITE_ID'))
# mark other orders except "ve" in ASC_ORD with "OTHERS",
ve_vat[which(!is.na(ve_vat$ASC_ORD) & ve_vat$ASC_ORD != 'VE'),'ASC_ORD'] = 'OTHERS'
# NA in asc_ord as "N/A"
ve_vat[is.na(ve_vat)]<-"N/A"
# value in ASC_ORD_P except for 've', and is "OTHERS" in ASC_ORD, mark as "OTHERS" as well
ve_vat[which(ve_vat$ASC_ORD == "OTHERS" & ve_vat$ASC_ORD_P != 'VE'),'ASC_ORD_P'] = 'OTHERS'
# feed the data to confusionmatrix
ve_confmat <- confusionMatrix(as.factor(ve_vat$ASC_ORD_P),reference = as.factor(ve_vat$ASC_ORD))
print(ve_confmat)

# similar as above but tick out the "N/A" values in ASC_ORD
ve_vat_1 <- ve_nnull %>%
  distinct(.,PROJECT_CODE,SITE_ID) %>%
  left_join(.,ocl[,c(1,2,7)],by = c('PROJECT_CODE','SITE_ID')) 
ve_vat_1 <- ve_vat_1[which(!is.na(ve_vat_1$ASC_ORD)),]
nrow(unique(ve_vat_1[,1:2])) == nrow(unique(ve_vat_1[,1:3]))
ve_vat_1 <- left_join(ve_vat_1,ve_p,by = c('PROJECT_CODE','SITE_ID'))
ve_vat_1[which(ve_vat_1$ASC_ORD != 'VE'),'ASC_ORD'] = 'OTHERS'
ve_vat_1[is.na(ve_vat_1)]<-"OTHERS"
ve_confmat_1 <- confusionMatrix(as.factor(ve_vat_1$ASC_ORD_P),reference = as.factor(ve_vat_1$ASC_ORD))
print(ve_confmat_1)



# write to files
ve_tp <- filter(ve_vat_1,ASC_ORD == 'VE',ASC_ORD_P == 'VE') 
ve_tp_hor <- left_join(ve_tp,ve_nnull, by = c('PROJECT_CODE','SITE_ID') )
ve_fp_fn <- filter(ve_vat_1,ASC_ORD != ASC_ORD_P) 
ve_fp_fn <- sqldf('select A.PROJECT_CODE, A.SITE_ID, B.ASC_ORD, A.ASC_ORD_P
        FROM ve_fp_fn A, ocl B
        WHERE A.PROJECT_CODE = B.PROJECT_CODE AND
        A.SITE_ID = B.SITE_ID')
ve_fp_fn[which(is.na(ve_fp_fn)),]  

ve_fp_fn_hor <- left_join(ve_fp_fn,ve_nnull)


ve_fp_fn_hor$ve_11 <- ifelse(is.na(match(paste0(ve_fp_fn_hor$PROJECT_CODE,ve_fp_fn_hor$SITE_ID,
                                                ve_fp_fn_hor$HORIZON_NO),
                                         paste0(ve_11$PROJECT_CODE,ve_11$SITE_ID,ve_11$HORIZON_NO))),"NO","YES")
ve_fp_fn_hor$ve_12 <- ifelse(is.na(match(paste0(ve_fp_fn_hor$PROJECT_CODE,ve_fp_fn_hor$SITE_ID,
                                                ve_fp_fn_hor$HORIZON_NO),
                                         paste0(ve_12$PROJECT_CODE,ve_12$SITE_ID,ve_12$HORIZON_NO))),"NO","YES")
ve_fp_fn_hor$ve_13 <- ifelse(is.na(match(paste0(ve_fp_fn_hor$PROJECT_CODE,ve_fp_fn_hor$SITE_ID,
                                                ve_fp_fn_hor$HORIZON_NO),
                                         paste0(ve_13$PROJECT_CODE,ve_13$SITE_ID,ve_13$HORIZON_NO))),"NO","YES")

ve_fp_fn_hor$ve_21<- ifelse(is.na(match(paste0(ve_fp_fn_hor$PROJECT_CODE,ve_fp_fn_hor$SITE_ID,
                                               ve_fp_fn_hor$HORIZON_NO),
                                        paste0(ve_21$PROJECT_CODE,ve_21$SITE_ID,ve_21$HORIZON_NO))),"NO","YES")
ve_fp_fn_hor$ve_22<- ifelse(is.na(match(paste0(ve_fp_fn_hor$PROJECT_CODE,ve_fp_fn_hor$SITE_ID
                                               ),
                                        paste0(ve_22$PROJECT_CODE,ve_22$SITE_ID))),"NO","YES")
ve_fp_fn_hor$ve_3<- ifelse(is.na(match(paste0(ve_fp_fn_hor$PROJECT_CODE,ve_fp_fn_hor$SITE_ID,
                                               ve_fp_fn_hor$HORIZON_NO),
                                        paste0(ve_3$PROJECT_CODE,ve_3$SITE_ID,ve_3$HORIZON_NO))),"NO","YES")
# ve_fp_fn_hor[which(is.na(ve_fp_fn_hor)),]

ve_predict <- filter(ve_vat,ASC_ORD =="N/A",ASC_ORD_P == 'VE' )

write.csv(ve_fp_fn, file =paste("C:/Users/lzccn/Downloads/des/files/ve_fp_fn",
                                format(Sys.time(),"%d-%b-%Y %H.%M"), ".csv",sep = " "))
write.csv(ve_fp_fn_hor, file =paste("C:/Users/lzccn/Downloads/des/files/ve_fp_fn_hor",
                                    format(Sys.time(),"%d-%b-%Y %H.%M"),".csv",sep = " "))
write.csv(ve_predict, file =paste("C:/Users/lzccn/Downloads/des/files/ve_predict",
                                  format(Sys.time(),"%d-%b-%Y %H.%M"), ".csv",sep = " "))
write.csv(ve_null, file =paste("C:/Users/lzccn/Downloads/des/files/ve_null",
                               format(Sys.time(),"%d-%b-%Y %H.%M"), ".csv",sep = " "))

# #Suborder of VE
# 
# #Suborder
# ve_p[,"SUBORD_P"] <- NA
# 
# #GreatGroup
# ve_p[,"GREAT_GROUP_P"] <- NA
# #SubGroup
# ve_p[,"SUBGROUP_P"] <- NA

# ASC-HY- Hydrosols ########################################
# Soils other than Organosols, Podosols and Vertosols in which the greater part of
# the profile is saturated for at least 2–3 months in most years.
#find solum with ELEM_TYPE_CODE means 'wet'
hy_11 <- filter(sit, ELEM_TYPE_CODE %in% c('TDF','ITF','STF','SWP','LAG','LAK')) %>%
  distinct(PROJECT_CODE,SITE_ID)
#find solum whith all horizons in which DRAINAGE == 1 OR 2
# 25/03/2020 change to : DRAINAGE == 1 only
hy_p <- as.data.frame(obs %>%
        group_by(PROJECT_CODE,SITE_ID) %>%
        filter(all(DRAINAGE %in% c(1))) %>%
        distinct(PROJECT_CODE,SITE_ID)) %>%
        full_join(.,hy_11)




#add a column to or
hy_p$ASC_ORD_P <- rep("HY",nrow(hy_p))


#Suborder of HY

#Suborder
hy_p[,"SUBORD_P"] <- NA

#GreatGroup
hy_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
hy_p[,"SUBGROUP_P"] <- NA

# ASC-KU- Kurosols ########################################
# Soils other than Organosols, Podosols and Vertosols in which the greater part of
# the profile is saturated for at least 2–3 months in most years.

# The first occuring of a clear or abrupt textural horizon in any solum
ku_11 <- filter(hor,BOUND_DISTINCT %in% c('S','A','C'))# %>%
  # aggregate(HORIZON_NO~PROJECT_CODE+SITE_ID,.,min) %>%
  #left_join(.,hor[,c('PROJECT_CODE','SITE_ID','TEXTURE_CODE')],
           # by = c('PROJECT_CODE','SITE_ID'))
ku_1 <- sqldf('select A.PROJECT_CODE, A.SITE_ID, A.HORIZON_NO,
      A.TEXTURE_CODE, A.DESIGN_MASTER
      from hor A,ku_11 B
      where A.PROJECT_CODE = B.PROJECT_CODE and A.SITE_ID = B.SITE_ID and
      A.HORIZON_NO = B.HORIZON_NO+1 and A.DESIGN_MASTER = "B" and 
      (B.TEXTURE_CODE IN("S","LS","FS","LFS","KS","LKS", "CS","CKS","CFS") and
      A.TEXTURE_CODE in ( "SL", "LFSY","L","ZL","SCL","CL","CLS","ZCL","LC","ZLC","LMC","MC","MHC","HC",
      "KSL", "KSCL","CLKS","KSLC","KSLMC","KSMC","KSMHC","KSHC", "SLC","SLMC","SMC","SMHC","SHC","FSL",
      "FSCL","CLFS","FSLC","FSLMC","FSMC","FSMHC","FSHC") OR
      B.TEXTURE_CODE IN ("SL","KSL","FSL") and
      A.TEXTURE_CODE in ( "CL","CLS","ZCL","LC","ZLC","LMC","MC","MHC","HC","CLKS","KSLC","KSLMC","KSMC",
      "KSMHC","KSHC", "SLC","SLMC","SMC","SMHC","SHC","FSL","L","ZL","FSCL","FSLC","FSLMC","FSMC","FSMHC"
      ,"FSHC") OR
      B.TEXTURE_CODE IN ( "L","ZL","SCL","LFSY","FSCL","KSCL") and
      A.TEXTURE_CODE in ( "MHC","HC","MC","LMC","KSMHC","KSHC","FSMHC","FSHC","KSMC","KSLMC","FSMC",
      "FSLMC") OR
      B.TEXTURE_CODE IN ( "CL","CLS","CLFS","CLKS") and
      A.TEXTURE_CODE in ( "MHC","HC","MC","KSMHC","KSHC","FSMHC","FSHC","KSMC","FSMC"))') %>%
  distinct(PROJECT_CODE,SITE_ID) 




# ku_1 <-ku_11 %>%
#   mutate(HORIZON_NO1 = HORIZON_NO + 1) %>%
#   inner_join(hor, ., by = c("PROJECT_CODE","SITE_ID", "HORIZON_NO" = "HORIZON_NO1"))
# #%>%
# #  filter(ku_11$TEXTURE_CODE == 'S' & hor$TEXTURE_CODE %in% c('SL','L'))

# contains a B2 horizon, which ph< 5.5
# ku_2 <- hor[which(grepl("B2",hor$HORIZON_NAME)),] %>%
#   left_join(.,fts, by=c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
#    filter(., TEST_TYPE == "PH",VALUE <5.5)%>%
#    distinct(PROJECT_CODE,SITE_ID)

ku_2 <- hor[which(grepl("B2",hor$HORIZON_NAME)),] %>%
  left_join(.,lab1, by=c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
    filter(., LAB_METH_CODE == "4A1",NUMERIC_VALUE <5.5,LOWER_DEPTH - UPPER_DEPTH>=0.2) %>%
    distinct(PROJECT_CODE,SITE_ID)


#combine ku_1 and ku_2
ku_p <- inner_join(ku_1,ku_2)


#add a column to or
ku_p$ASC_ORD_P <- rep("KU",nrow(ku_p))


#Suborder of HY

#Suborder
ku_p[,"SUBORD_P"] <- NA

#GreatGroup
ku_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
ku_p[,"SUBGROUP_P"] <- NA

# ASC-SO- Sodosols ########################################
# Other soils with a clear or abrupt textural B horizon and in which the major part of
# the upper 0.2 m of the B2t horizon (or the major part of the entire B2t horizon if it
# is less than 0.2 m thick) is sodic and is not strongly subplastic.

#contains B2, ESP>=6
# so_1 <-  hor[which(grepl("B2",hor$HORIZON_NAME)),] %>%
#   left_join(.,lab1, by=c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
#   anti_join(.,filter(., LAB_METH_CODE == "15N1",NUMERIC_VALUE < 6),
#             by=c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO'))  %>%
#   distinct(PROJECT_CODE,SITE_ID)
so_1 <-  hor[which(grepl("B2",hor$HORIZON_NAME)),] %>%
  left_join(.,lab1, by=c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  filter(., LAB_METH_CODE == "15N1",NUMERIC_VALUE >= 6) %>%
  distinct(PROJECT_CODE,SITE_ID)

# Other soils with a clear or abrupt textural B horizon,and ESP>=6
so_p <- anti_join(ku_1,ku_p) %>% inner_join(.,so_1)
  

#add a column to or
so_p$ASC_ORD_P <- rep("SO",nrow(so_p))

#Subclasses

#Suborder
so_p[,"SUBORD_P"] <- NA
#GreatGroup
so_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
so_p[,"SUBGROUP_P"] <- NA

# ASC-CH- Chromosols ########################################
# Other soils with a clear or abrupt textural B horizon and in which the major part of
# the upper 0.2 m of the B2t horizon (or the major part of the entire B2t horizon if it
#is less than 0.2 m thick) is not strongly acid.
ch_p <-anti_join(ku_1,ku_p) %>% anti_join(.,so_p)


#add a column to or
ch_p$ASC_ORD_P <- rep("CH",nrow(ch_p))

#Subclasses

#Suborder
ch_p[,"SUBORD_P"] <- NA
#GreatGroup
ch_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
ch_p[,"SUBGROUP_P"] <- NA



# ASC-CA- Calcarsols ########################################
# Other soils that:
#   1. Are either calcareous throughout the solum - or calcareous at least directly
# below the A1 or Ap horizon, or a depth of 0.2 m (whichever is shallower).
# Carbonate accumulations must be judged to be pedogenic, i.e. are a result of soil
# forming processes in situ (either current or relict). Soils dominated by nonpedogenic calcareous
# materials such as particles of limestone or shells are
# excluded. See also calcrete, and
# 2. Do not have deep sandy profiles that have a field texture of sand, loamy sand or
# clayey sand in 80% or more of the upper 1.0 m.

hsg <- SALI$HSG
fts <- fts

# hsg$NATURE == 'K', OR (fts$TEST_TYPE == 'CAREF' AND (VALUE ! N OR NULL))
ca_11 <- full_join(hsg[which(hsg$NATURE == 'K'),],filter(fts,TEST_TYPE == 'CAREF', 
                                                         (!is.na(VALUE)) != 'N'),
                   by = c("PROJECT_CODE","SITE_ID","OBS_NO", "HORIZON_NO"))
# first occuring of below A1 or AP horizon in every solum, and join with ca_11
ca_p <- hor[which(grepl("A1|Ap",hor$HORIZON_NAME)),] %>%
  aggregate(HORIZON_NO~PROJECT_CODE+SITE_ID,.,min) %>%
  mutate(HORIZON_NO1 = HORIZON_NO + 1) %>%
  inner_join(hor[,c(1:4)], ., by = c("PROJECT_CODE","SITE_ID", "HORIZON_NO" = "HORIZON_NO1"),) %>%
  inner_join(.,ca_11,by = c("PROJECT_CODE","SITE_ID","OBS_NO", "HORIZON_NO")) %>%
  distinct(PROJECT_CODE,SITE_ID)


#add a column to or
ca_p$ASC_ORD_P <- rep("CA",nrow(ca_p))

#Subclasses

#Suborder
ca_p[,"SUBORD_P"] <- NA
#GreatGroup
ca_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
ca_p[,"SUBGROUP_P"] <- NA



# ASC-FE- Ferrosols ########################################
# Other soils with B2 horizons in which the major part has a free iron oxide content
# greater than 5% Fe in the fine-earth fraction (<2 mm). Soils with a B2 horizon in
# which at least 0.3m has vertic properties are excluded (see also Comment and
# footnote in Ferrosols).
#
fe_p <-  hor[which(grepl("B2",hor$HORIZON_NAME)),] %>%
  left_join(.,lab1, by=c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  filter(., LAB_METH_CODE == "13C1_Fe",NUMERIC_VALUE >5) %>%
  distinct(PROJECT_CODE,SITE_ID)



#add a column to or
fe_p$ASC_ORD_P <- rep("FE",nrow(fe_p))

#Subclasses

#Suborder
fe_p[,"SUBORD_P"] <- NA
#GreatGroup
fe_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
fe_p[,"SUBGROUP_P"] <- NA




# ASC-DE- Demosols ########################################
# Other soils with B2 horizons that have grade of pedality greater than weak
# throughout the major part of the horizon.
#
de_p <-  hor[which(grepl("B2",hor$HORIZON_NAME)),] %>%
  left_join(.,hst, by=c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  filter(., PEDALITY_GRADE %in% c('M','S')) %>%
  distinct(PROJECT_CODE,SITE_ID)



#add a column to or
de_p$ASC_ORD_P <- rep("DE",nrow(de_p))

#Subclasses

#Suborder
de_p[,"SUBORD_P"] <- NA
#GreatGroup
de_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
de_p[,"SUBGROUP_P"] <- NA



# ASC-KA- Kandosols ########################################
# Other soils that:
# 1. Have well-developed B2 horizons in which the major part has a grade of pedality
# that is massive or weak , and
# 2. Have a maximum clay content in some part of the B2 horizon that exceeds 15%
# (i.e. heavy sandy loam [SL+]or heavier).

#
ka_p <-  hor[which(grepl("B2",hor$HORIZON_NAME)),] %>%
  left_join(.,hst, by=c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  filter(., PEDALITY_GRADE %in% c('V','W'),!TEXTURE_CODE %in% c('S','KS','FS','LS','KLS','FLS',
                                                               'CS','KCS','FCS','SL','KSL','FSL')) %>%
  distinct(PROJECT_CODE,SITE_ID)


#add a column to or
ka_p$ASC_ORD_P <- rep("KA",nrow(ka_p))

#Subclasses

#Suborder
ka_p[,"SUBORD_P"] <- NA
#GreatGroup
ka_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
ka_p[,"SUBGROUP_P"] <- NA



# ASC-RU- Rudosols ########################################
# Other soils with negligible (rudimentary), if any, pedologic organisation apart from the
# minimal development of an A1 horizon or the presence of less than 10% of B horizon
# material (including pedogenic carbonate) in fissures in the parent rock or saprolite. The soils
# have a grade of pedality of single grain, massive or weak in the A1 horizon and show no
# pedological colour change apart from darkening of an A1 horizon. There is little or no
# texture or colour change with depth unless stratified or buried soils are present. Cemented
# pans may be present as a substrate material. Soil may be shallow over unrelated rock.

#
ru_p <-   hor[which(!grepl("A2|A3|B1|B2|B3|AB|BC|A/B|B/C",hor$HORIZON_NAME)),] %>%
  distinct(PROJECT_CODE,SITE_ID)


#add a column to or
ru_p$ASC_ORD_P <- rep("RU",nrow(ru_p))

#Subclasses

#Suborder
ru_p[,"SUBORD_P"] <- NA
#GreatGroup
ru_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
ru_p[,"SUBGROUP_P"] <- NA



# ASC-TE- Tenosols ########################################
# All other soils
te_p <- ocl %>% distinct(PROJECT_CODE,SITE_ID)


#add a column to or
te_p$ASC_ORD_P <- rep("TE",nrow(te_p))

#Subclasses

#Suborder
te_p[,"SUBORD_P"] <- NA
#GreatGroup
te_p[,"GREAT_GROUP_P"] <- NA
#SubGroup
te_p[,"SUBGROUP_P"] <- NA


# Training /validating data sets from OCL##################################################
# whole dataset, incl .na data ( use 'rbine' only to merge data)
val_dat_wi <- ocl %>%
  anti_join(.,hor[which(is.na(hor$HORIZON_NAME)|is.na(hor$DESIGN_MASTER)|is.na(hor$TEXTURE_CODE)
            |hor$ASC_ORD == 'AN' ),],
            by =c("PROJECT_CODE","SITE_ID")) %>%
  .[,c("PROJECT_CODE","SITE_ID","ASC_ORD","SUBORD_ASC_CODE","GREAT_GROUP_ASC_CODE","SUBGROUP_ASC_CODE")]
# whole dataset, excl .na data (also use 'inner_join' to merge data)
val_dat_we <- filter(ocl,!is.na(ocl$ASC_ORD)) %>%
  anti_join(.,hor[which(is.na(hor$HORIZON_NAME)|is.na(hor$DESIGN_MASTER)|is.na(hor$TEXTURE_CODE)
                        |hor$ASC_ORD == 'AN' ),],
            by =c("PROJECT_CODE","SITE_ID")) %>%
  .[,c("PROJECT_CODE","SITE_ID","ASC_ORD","SUBORD_ASC_CODE","GREAT_GROUP_ASC_CODE","SUBGROUP_ASC_CODE")]
# data with ocl$ASC_CONFIDENCE == 1, incl .na data  ( use 'rbine' only to merge data)
# val_dat_ci <- filter(ocl,ASC_CONFIDENCE == 1) %>%
#   anti_join(.,hor[which(is.na(hor$HORIZON_NAME)|is.na(hor$DESIGN_MASTER)|is.na(hor$TEXTURE_CODE)),],
#             by =c("PROJECT_CODE","SITE_ID")) %>%
#  .[,c("PROJECT_CODE","SITE_ID","ASC_ORD","SUBORD_ASC_CODE","GREAT_GROUP_ASC_CODE","SUBGROUP_ASC_CODE")]
# data with ocl$ASC_CONFIDENCE == 1, excl .na data   (also use 'inner_join' to merge dat )                
# val_dat_ce <- filter(ocl,ASC_CONFIDENCE == 1,!is.na(ocl$ASC_ORD)) %>%
#   anti_join(.,hor[which(is.na(hor$HORIZON_NAME)|is.na(hor$DESIGN_MASTER)|is.na(hor$TEXTURE_CODE)
#                         |hor$ASC_ORD == 'AN' ),],
#             by =c("PROJECT_CODE","SITE_ID")) %>%
#   .[,c("PROJECT_CODE","SITE_ID","ASC_ORD","SUBORD_ASC_CODE","GREAT_GROUP_ASC_CODE","SUBGROUP_ASC_CODE")]


val_dat_ce <- SALI$OCL[which(!is.na(SALI$OCL$ASC_ORD)& SALI$OCL$ASC_ORD != 'AN'),] %>%
  anti_join(.,hor[which(is.na(hor$HORIZON_NAME)|is.na(hor$DESIGN_MASTER)|
                               is.na(hor$TEXTURE_CODE)|
                               is.na(hor$UPPER_DEPTH)|is.na(hor$LOWER_DEPTH)|
                               is.na(hor$SOIL_WATER_STAT)|is.na(hor$BOUN_DISTINCT)),],
            by =c("PROJECT_CODE","SITE_ID","OBS_NO")) %>%
  anti_join(.,sit[which(is.na(sit$ELEM_TYPE_CODE )),],by =c("PROJECT_CODE","SITE_ID")) %>%
  anti_join(.,hcu[which(is.na(hcu$CUTAN_TYPE )),],by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(.,hst[which(is.na(hst$PEDALITY_TYPE)|is.na(hst$PEDALITY_GRADE)),],
            by =c("PROJECT_CODE","SITE_ID","OBS_NO")) %>%
  anti_join(.,SALI$HSG[which(is.na(SALI$HSG$NATURE)),],by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(.,filter(lab,LAB_METH_CODE == '15N1',is.na(lab$NUMERIC_VALUE)),
            by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(.,filter(lab,LAB_METH_CODE == '13C1_Fe',is.na(lab$NUMERIC_VALUE)),
            by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(.,filter(lab,LAB_METH_CODE == '4A1',is.na(lab$NUMERIC_VALUE)),
            by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(., filter(obs, is.na(obs$DRAINAGE)),by =c("PROJECT_CODE","SITE_ID")) %>%
  anti_join(., osc[which(is.na(osc$STATUS)),],by =c("PROJECT_CODE","SITE_ID"))%>%
  .[,c("PROJECT_CODE","SITE_ID","ASC_ORD","SUBORD_ASC_CODE","GREAT_GROUP_ASC_CODE","SUBGROUP_ASC_CODE")]


val_dat_ci <- filter(SALI$OCL, ASC_ORD != 'AN') %>%
  anti_join(.,hor[which(is.na(hor$HORIZON_NAME)|is.na(hor$DESIGN_MASTER)|
                               is.na(hor$TEXTURE_CODE)|
                               is.na(hor$UPPER_DEPTH)|is.na(hor$LOWER_DEPTH)|
                               is.na(hor$SOIL_WATER_STAT)|is.na(hor$BOUN_DISTINCT)),],
            by =c("PROJECT_CODE","SITE_ID","OBS_NO")) %>%
  anti_join(.,sit[which(is.na(sit$ELEM_TYPE_CODE )),],by =c("PROJECT_CODE","SITE_ID")) %>%
  anti_join(.,hcu[which(is.na(hcu$CUTAN_TYPE )),],by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(.,hst[which(is.na(hst$PEDALITY_TYPE)|is.na(hst$PEDALITY_GRADE)),],
            by =c("PROJECT_CODE","SITE_ID","OBS_NO")) %>%
  anti_join(.,SALI$HSG[which(is.na(SALI$HSG$NATURE)),],by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(.,filter(lab,LAB_METH_CODE == '15N1',is.na(lab$NUMERIC_VALUE)),
            by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(.,filter(lab,LAB_METH_CODE == '13C1_Fe',is.na(lab$NUMERIC_VALUE)),
            by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(.,filter(lab,LAB_METH_CODE == '4A1',is.na(lab$NUMERIC_VALUE)),
            by =c("PROJECT_CODE","SITE_ID","OBS_NO"))%>%
  anti_join(., filter(obs, is.na(obs$DRAINAGE)),by =c("PROJECT_CODE","SITE_ID")) %>%
  anti_join(., osc[which(is.na(osc$STATUS)),],by =c("PROJECT_CODE","SITE_ID"))%>%
  .[,c("PROJECT_CODE","SITE_ID","ASC_ORD","SUBORD_ASC_CODE","GREAT_GROUP_ASC_CODE","SUBGROUP_ASC_CODE")]

# collection of predict dataset 
pre_dat_i <- data.frame("PROJECT_CODE" = character() ,"SITE_ID" =integer(),
                      "ASC_ORD_P"= character(),"SUBORD_P"= character(),
                      "GREAT_GROUP_P"= character(),"SUBGROUP_P"= character()) 
pre_dat_we <- data.frame("PROJECT_CODE" = character() ,"SITE_ID" =integer(),
                       "ASC_ORD_P"= character(),"SUBORD_P"= character(),
                       "GREAT_GROUP_P"= character(),"SUBGROUP_P"= character())
pre_dat_ce <- data.frame("PROJECT_CODE" = character() ,"SITE_ID" =integer(),
                         "ASC_ORD_P"= character(),"SUBORD_P"= character(),
                         "GREAT_GROUP_P"= character(),"SUBGROUP_P"= character())


#feed into precict data set
pre_dat_i <- po_p %>% rbind(pre_dat_i,.) %>%
  # rbind(.,anti_join(po_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>% 
  rbind(.,anti_join(ve_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(hy_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(ku_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(so_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(ch_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(ca_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(fe_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(de_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(ka_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(ru_p,.,by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(te_p,.,by=c("PROJECT_CODE","SITE_ID"))) 
  
  

pre_dat_we <- rbind(pre_dat_we,inner_join(po_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(po_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ve_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(hy_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ku_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(so_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ch_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ca_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(fe_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(de_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ka_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ru_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(te_p,val_dat_we[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) 

pre_dat_ce <- rbind(pre_dat_ce,inner_join(po_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID"))) %>%
  # rbind(.,anti_join(inner_join(po_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
  #       by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ve_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(hy_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ku_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(so_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ch_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ca_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(fe_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(de_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ka_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(ru_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID"))) %>%
  rbind(.,anti_join(inner_join(te_p,val_dat_ce[,1:2],by=c("PROJECT_CODE","SITE_ID")),.,
        by=c("PROJECT_CODE","SITE_ID")))

mat_wi <- merge(val_dat_wi,pre_dat_i,by=c(1,2),all=T)
mat_we <- merge(val_dat_we,pre_dat_we,by=c(1,2),all=T)
mat_ci <- merge(val_dat_ci,pre_dat_i,by=c(1,2),all=T)
mat_ce <- merge(val_dat_ce,pre_dat_ce,by=c(1,2),all=T)

mat_wi[is.na(mat_wi)]<-"N/A"
mat_we[is.na(mat_we)]<-"N/A"
mat_ci[is.na(mat_ci)]<-"N/A"
mat_ce[is.na(mat_ce)]<-"N/A"

# mat_wi[which(mat_wi$ASC_ORD == 'AN'),mat_wi$ASC_ORD]<-"N/A"
# mat_we[which(mat_we$ASC_ORD == 'AN'),mat_wi$ASC_ORD]<-"N/A"
# # mat_ci[is.na(mat_ci)]<-"N/A"
# mat_ce[which(mat_ce$ASC_ORD == 'AN'),mat_wi$ASC_ORD]<-"N/A"

con_mat_wi <- confusionMatrix(as.factor(mat_wi$ASC_ORD_P),reference = as.factor(mat_wi$ASC_ORD))
con_mat_we <- confusionMatrix(as.factor(mat_we$ASC_ORD_P),reference = as.factor(mat_we$ASC_ORD))
con_mat_ci <- confusionMatrix(as.factor(mat_ci$ASC_ORD_P),reference = as.factor(mat_ci$ASC_ORD))
con_mat_ce <- confusionMatrix(as.factor(mat_ce$ASC_ORD_P),reference = as.factor(mat_ce$ASC_ORD))

print(con_mat_wi)
print(con_mat_we)
print(con_mat_ci)
print(con_mat_ce)

# Write to files ##################################################################################

#confusioin matrix
write.csv(con_mat_wi$table, file =paste("C:/Users/lzccn/Downloads/des/files/con_mat_wi",
           format(Sys.time(),"%d-%b-%Y %H.%M"),".csv",sep = " "))
write.csv(con_mat_we$table, file =paste("C:/Users/lzccn/Downloads/des/files/con_mat_we",
          format(Sys.time(), "%d-%b-%Y %H.%M"),".csv",sep = " "))
# write.csv(con_mat_ci$table, file =paste("C:/Users/lzccn/Downloads/des/files/con_mat_ci",
#                                         format(Sys.time(),"%d-%b-%Y %H.%M"),".csv",sep = " "))
write.csv(con_mat_ce$table, file =paste("C:/Users/lzccn/Downloads/des/files/con_mat_ce",
                                        format(Sys.time(), "%d-%b-%Y %H.%M"),".csv",sep = " "))
# # Ku 
# ku_fn <- filter(mat_1[,c("PROJECT_CODE",'SITE_ID','ASC_ORD','ASC_ORD_P')],ASC_ORD =="KU", 
#                 ASC_ORD_P != "KU")
# Ku_fp <- filter(mat_1[,c("PROJECT_CODE",'SITE_ID','ASC_ORD','ASC_ORD_P')],ASC_ORD !="KU",
#                 ASC_ORD_P == "KU")
# ku_predict <- filter(mat[,c("PROJECT_CODE",'SITE_ID','ASC_ORD','ASC_ORD_P')],
#                ASC_ORD =="N/A", ASC_ORD_P == "KU")
# write.csv(ku_fn, file =paste("C:/Users/lzccn/Downloads/des/files/ku_fn",format(Sys.time(), 
#                "%d-%b-%Y %H.%M"), ".csv",sep = " "))
# write.csv(Ku_fp, file =paste("C:/Users/lzccn/Downloads/des/files/ku_fp",format(Sys.time(), 
#                "%d-%b-%Y %H.%M"),".csv",sep = " "))
# write.csv(ku_predict, file =paste("C:/Users/lzccn/Downloads/des/files/ku_predict",format(Sys.time(),
#                "%d-%b-%Y %H.%M"), ".csv",sep = " "))

#whole
fp_fn_ce <- filter(mat_ce[,c("PROJECT_CODE",'SITE_ID','ASC_ORD','ASC_ORD_P')],ASC_ORD != ASC_ORD_P)
tp_ce <- filter(mat_ce[,c("PROJECT_CODE",'SITE_ID','ASC_ORD','ASC_ORD_P')],ASC_ORD == ASC_ORD_P)
new_predict <- filter(mat_wi[,c("PROJECT_CODE",'SITE_ID','ASC_ORD','ASC_ORD_P')],mat_wi$ASC_ORD =="N/A" )
write.csv(fp_fn_ce, file =paste("C:/Users/lzccn/Downloads/des/files/fp_fn_ce",format(Sys.time(), 
               "%d-%b-%Y %H.%M"), ".csv",sep = " "))
write.csv(tp_ce, file =paste("C:/Users/lzccn/Downloads/des/files/tp_ce",format(Sys.time(), 
               "%d-%b-%Y %H.%M"),".csv",sep = " "))
write.csv(new_predict, file =paste("C:/Users/lzccn/Downloads/des/files/new_predict",format(Sys.time(),
             "%d-%b-%Y %H.%M"), ".csv",sep = " "))
