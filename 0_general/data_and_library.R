
# Libraries ###############################################################################
library(combinat)
library(purrr)
library(ngramrr)
library(ngram)
library(gtools)
library(data.table)
library (magrittr)
library(sqldf)
library(dplyr)
library(plyr)
library(caret)
library(scales)
library(tidyr)
library(ggplot2)
library(stringr)
library(plotly)
library(dendextend)
library(abind)
library(nnet)
library(readr)
# library(naniar)

# Original Data ###########################################################################
SALI_de <- readRDS("./0_general/SALI_SIT-data_decodes_20191101.rds")
SALI <-readRDS("./0_general/SALI_SIT-data_20191101.rds")
soil_sample <- read.csv("./0_general/samples.csv")
lab <- read.csv("./0_general/labresults.csv")
lab_15n1_extra <- read.csv('./0_general/15N1.csv') # Peter supplied on 27/May/2020 email


# Subset data sets from SALI###############################################################
sit <- SALI$SIT
sit$CREATION_DATE <- as.character(sit$CREATION_DATE)
sit$LAST_UPDATE_DATE <- as.character(sit$LAST_UPDATE_DATE)
sit[sit == "-"] <- NA
# sit <- sit %>%
#   mutate_at(vars(ELEM_TYPE_CODE), na_if, "-")

obs <- SALI$OBS 
obs <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,obs,min) %>%
  left_join(.,obs,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
obs$CREATION_DATE <- as.character(obs$CREATION_DATE)
obs$LAST_UPDATE_DATE <- as.character(obs$LAST_UPDATE_DATE)
obs$OBS_DATE <- as.character(obs$OBS_DATE)
obs[obs == "-"] <- NA
names(obs)[10] = 'OBS_LITH_CODE'
# ODS
ods <- SALI$ODS 
ods <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,ods,min) %>%
  left_join(.,ods,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
ods <-  aggregate(DISTURB_NO~PROJECT_CODE+SITE_ID+OBS_NO,ods,min) %>%
   left_join(.,ods,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO","DISTURB_NO"))
ods$CREATION_DATE <- as.character(ods$CREATION_DATE)
ods$LAST_UPDATE_DATE <- as.character(ods$LAST_UPDATE_DATE)

ods[ods == "-"] <- NA
nrow(unique(ods[,1:2]))
#SPC
spc <- obs[which(obs$TAX_UNIT_TYPE == 'SPC' & !is.na(obs$TAX_UNIT_CODE)), 
                c('PROJECT_CODE','SITE_ID','OBS_NO','TAX_UNIT_CODE')]
names(spc)[4] <- "SPC"
length(unique(spc$SPC))
nrow(unique(spc[,c(1:3)]))

#spc$SPC <- spc$TAX_UNIT_CODE
# use mean value if multiple values available for one horizon 
#spc<-  aggregate(SPC ~ PROJECT_CODE+SITE_ID+HORIZON_NO,SPC,mean)

osc <- SALI$OSC 
osc <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,osc,min) %>%
  left_join(.,osc,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
osc <-  aggregate(SURF_COND_NO~PROJECT_CODE+SITE_ID+OBS_NO,osc,min) %>%
  left_join(.,osc,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO","SURF_COND_NO"))
osc$CREATION_DATE <- as.character(osc$CREATION_DATE)
osc$LAST_UPDATE_DATE <- as.character(osc$LAST_UPDATE_DATE)

osc[osc == "-"] <- NA
# nrow(unique(osc[,1:2])) #91365
# nrow(unique(osc[,1:3])) #91365
# nrow(unique(osc[,1:4])) #91365

#vst

vst <- SALI$VST[which(SALI$VST$VEG_STRATA_CODE=='T'),] 
vst <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,vst,min) %>%
  left_join(.,vst,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
vst <-  aggregate(VEG_COMM_NO~PROJECT_CODE+SITE_ID+OBS_NO,vst,min) %>%
  left_join(.,vst,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO","VEG_COMM_NO"))
vst$CREATION_DATE <- as.character(vst$CREATION_DATE)
vst$LAST_UPDATE_DATE <- as.character(vst$LAST_UPDATE_DATE)

#

vsp <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,SALI$VSP,min) %>%
  left_join(.,SALI$VSP,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
vsp <-  aggregate(VEG_COMM_NO~PROJECT_CODE+SITE_ID+OBS_NO,vsp,min) %>%
  left_join(.,vsp,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO","VEG_COMM_NO"))
vsp$CREATION_DATE <- as.character(vsp$CREATION_DATE)
vsp$LAST_UPDATE_DATE <- as.character(vsp$LAST_UPDATE_DATE)
a = vsp[which(vsp$VEG_STRATA_CODE=='T' & vsp$VEG_STRATA_SPEC_NO==1),]
vsp <- vsp[which(vsp$VEG_STRATA_CODE=='L' & vsp$VEG_STRATA_SPEC_NO ==1),] %>%
  anti_join(.,vsp[which(vsp$VEG_STRATA_CODE=='T'),],
            by=c("PROJECT_CODE" , "SITE_ID","OBS_NO","VEG_COMM_NO")) %>%
  full_join(.,vsp[which(vsp$VEG_STRATA_CODE=='T' & vsp$VEG_STRATA_SPEC_NO==1),])
            
 
#anti_join(SALI$VSP,.,],
#by=c("PROJECT_CODE" , "SITE_ID","OBS_NO","VEG_COMM_NO"))

vsp[vsp == "-"] <- NA
ggplot(data.frame(vsp$VEG_SPEC_CODE), aes(x=vsp$VEG_SPEC_CODE)) + geom_bar()
nrow(unique(vsp[,1:4]))


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
ocl <- aggregate(ASC_ORD~PROJECT_CODE+SITE_ID,ocl,max) %>%
  left_join(.,ocl,by=c("PROJECT_CODE" , "SITE_ID","ASC_ORD"))
ocl$CREATION_DATE <- as.character(ocl$CREATION_DATE)
ocl$LAST_UPDATE_DATE <- as.character(ocl$LAST_UPDATE_DATE)
ocl[ocl == "-"] <- NA
#
# ocf 
ocf <- SALI$OCF
ocf <- aggregate(OBS_NO~PROJECT_CODE+SITE_ID,ocf,min) %>%
  left_join(.,ocf,by=c("PROJECT_CODE" = "PROJECT_CODE" , "SITE_ID" = "SITE_ID","OBS_NO"="OBS_NO"))
#eliminate duplcates with different SOIL_CLASS_NO, only keep the one SOIL_CLASS_NO == MIN(MOST ARE 1)
ocf <- aggregate(SURF_FRAG_NO~PROJECT_CODE+SITE_ID,ocf,min) %>%
  left_join(.,ocf,by=c("PROJECT_CODE" , "SITE_ID","SURF_FRAG_NO"))
ocf$CREATION_DATE <- as.character(ocf$CREATION_DATE)
ocf$LAST_UPDATE_DATE <- as.character(ocf$LAST_UPDATE_DATE)
ocf[ocf == "-"] <- NA
names(ocf)[6]='OCF_LITH_CODE'
names(ocf)[5]='OCF_ABUNDANCE'
nrow(unique(ocf[,1:2]))

# HMT ###
hmt = SALI$HMT
hmt = aggregate(OBS_NO~PROJECT_CODE+SITE_ID,hmt,min) %>%
  left_join(.,hmt,by=c("PROJECT_CODE" = "PROJECT_CODE" , "SITE_ID" = "SITE_ID","OBS_NO"="OBS_NO"))
hmt = aggregate(MOTT_NO~PROJECT_CODE+SITE_ID,hmt,min) %>%
  left_join(.,hmt,by =c("PROJECT_CODE" , "SITE_ID",'MOTT_NO' ))
nrow(unique(hmt[,1:3]))

# HOR
hor <- SALI$HOR
hor <- aggregate(OBS_NO~PROJECT_CODE+SITE_ID,hor,min) %>%
  left_join(.,hor,by=c("PROJECT_CODE" = "PROJECT_CODE" , "SITE_ID" = "SITE_ID","OBS_NO"="OBS_NO"))
hor$CREATION_DATE <- as.character(hor$CREATION_DATE)
hor$LAST_UPDATE_DATE <- as.character(hor$LAST_UPDATE_DATE)
hor[hor == "-"] <- NA
# hor$HORIZON_NAME = gsub("[[:punct:]]+",NA,hor$HORIZON_NAME , fixed = TRUE)
# hor$HORIZON_NAME = gsub("[[:punct:]]+",'',hor$HORIZON_NAME )
hor$HORIZON_NAME = gsub("\\Q+\\E|\\Q?\\E",'',hor$HORIZON_NAME )
hor$HORIZON_NAME = gsub("b23",'B23',hor$HORIZON_NAME)
hor$HORIZON_NAME = gsub("a1",'A1',hor$HORIZON_NAME)
#unique(hor$HORIZON_NAME)
length(unique(hor$HORIZON_NAME))
# hor$HORIZON_NAME = gsub("[O][0-9]","O",hor$HORIZON_NAME)
#unique(hor$HORIZON_NAME[grep("b1",hor$HORIZON_NAME)])

hor <- hor[grep("S|UB",hor$HORIZON_NAME),] %>%
  distinct(PROJECT_CODE,SITE_ID) %>%
  anti_join(hor,.,by = c('PROJECT_CODE','SITE_ID'))

# HORIZON_NAME spit

hor_split = separate(hor,HORIZON_NAME,c('HOR_PREFIX','HORIZON_NAME'),sep="(?<=[0-9]|[0-9][0-9]|^)(?=[A-Za-z]|$)",
             remove = T,convert = T,extra = "merge",fill = "left")

#unique(hor_split$HORIZON_NAME[grep("O",hor_split$HORIZON_NAME)])
hor_split$HORIZON_NAME = gsub("Ap",'AP',hor_split$HORIZON_NAME)
hor_split$HORIZON_NAME = gsub("AB|A/B",'A3',hor_split$HORIZON_NAME)
hor_split$HORIZON_NAME = gsub("B3/2D",'B3',hor_split$HORIZON_NAME)
hor_split$HORIZON_NAME = gsub("BCC",'BC',hor_split$HORIZON_NAME)
hor_split$HORIZON_NAME = gsub("C/B",'C',hor_split$HORIZON_NAME)
#hor_split$HORIZON_NAME = gsub("UB",'U',hor_split$HORIZON_NAME)
# hor_split$HORIZON_NAME = gsub("Ap|AP|M",'A1',hor_split$HORIZON_NAME)
# hor_split$HORIZON_NAME = gsub("Bp",'B1',hor_split$HORIZON_NAME)
# hor_split$HORIZON_NAME = gsub("BC|BCC|BD",'B3',hor_split$HORIZON_NAME)

hor_split= separate(hor_split,HORIZON_NAME,c('HOR_MASTER','HORIZON_NAME'),
                    sep="((?<=([AB]([0-9]|\\b))|[OPCDRMSU])(?=[0-9a-z]|$))|((?<=[ABOP])(?=[a-z]))",
            remove = T,convert = T,extra = "merge",fill = "left")

sort(unique(hor_split$HOR_MASTER))
length(unique(hor_split$HOR_MASTER))
#hor_split$HOR_MASTER[hor_split$HOR_MASTER %in% c('O','P')] = "GRP1"
# hor_split$HOR_MASTER = gsub("O|P",'GRP1',hor_split$HOR_MASTER)
# hor_split$HOR_MASTER = gsub("M|A1|AP|A",'GRP2',hor_split$HOR_MASTER)
# hor_split$HORIZON_NAME = gsub("A3|B1|AB|A/B",'GRP4',hor_split$HORIZON_NAME)
# hor_split$HORIZON_NAME = gsub("B3|BC|C|D",'A3',hor_split$HORIZON_NAME)


# hor_split$HOR_MASTER = gsub("B/C|A/C|C/B|A/B|C/B2|A/B2|A/B1|B/A|C/B3|AA|C/B1|UB2",'OTHERS',
#                             hor_split$HOR_MASTER) 

# hor_split$HOR_MASTER = gsub("[A][4-9]",'A_OTHERS',hor_split$HOR_MASTER) 
# hor_split$HOR_MASTER = gsub("[B][4-9]",'B_OTHERS',hor_split$HOR_MASTER)
# hor_split$HOR_MASTER = gsub("[P][3-9]",'P_OTHERS',hor_split$HOR_MASTER)

a=unique(hor_split[,c('HOR_PREFIX','HOR_MASTER','HORIZON_NAME')])
a[grep('[A-Z]',a$HORIZON_NAME),]
unique(a$HOR_MASTER)
hor_split= separate(hor_split,HORIZON_NAME,c('HOR_SUBHOR','HOR_SUFFIX'),sep="(?<=[0-9])(?=[a-z]|$)",
            remove = T,convert = T,extra = "merge",fill = "left")
hor_split$HOR_SUFFIX[hor_split$HOR_SUFFIX == ""] = NA
unique(hor_split$HOR_MASTER)
unique(hor_split$HOR_PREFIX)
unique(hor_split$HOR_SUBHOR)
unique(hor_split$HOR_SUFFIX)

hor_spc = hor_split
hor_spc$HOR_MASTER[hor_spc$HOR_MASTER %in% c('O','P','AO')] = "GRP1"
hor_spc$HOR_MASTER[hor_spc$HOR_MASTER %in% c('M','A1','AP','A','A/C','A0','AA','AC')] = "GRP2"
hor_spc$HOR_MASTER[hor_spc$HOR_MASTER %in% c('A3','B1','AB','A/B','A4','B/A')] = "GRP4"
hor_spc$HOR_MASTER[hor_spc$HOR_MASTER %in% c('B','B2')] = "GRP5"
hor_spc$HOR_MASTER[hor_spc$HOR_MASTER %in% c('B3','BC','B/C','C','D','B4','B9','BD')] = "GRP6"
names(hor_spc)[11]='SPC_HOR_MASTER'
# c= separate(b,a,c('c','a'),sep = '(?<=[Ap][1-2]|[B][1-2])(?=[1-9])',
#remove = T,convert = T,extra = "merge",fill = "left")
# hco
colour_convert <- function(hue,value,chroma) {
  color = c()
  for ( i in 1 : length(hue)) {
    if (hue[i]<5) {
      if (value[i]>5) {
        if (chroma[i] <= 3) {color[i] = 'GREY'}
        else {color[i] = 'YELLOW'}
      }
      else if (chroma[i] <= 2) {
        if (value[i]>3) {color[i] = 'GREY'}
        else {color[i] = 'BLACK'}
      }
      else {color[i] = 'BROWN'}
    }
    else if (chroma[i] <= 2) {
      if (value[i]>=4) {color[i] = 'GREY'}
      else {color[i] = 'BLACK'}
    }
    else {color[i] = 'RED'}
  }
  return(color)
}

hco = SALI$HCO
hco = hco[which(grepl('YR',hco$COLOUR_CODE)),]
hco = separate(hco,COLOUR_CODE,c('HUE','COLOUR_CODE'),sep='(?<=[0-9])(?=[A-Za-z])')
hco$COLOUR_CODE = gsub('YR|/','',hco$COLOUR_CODE)
hco = separate(hco,COLOUR_CODE,c('VALUE','CHROMA'),sep='(?<=[0-9])(?=[0-9])')
hco$COLOUR_CLASS = colour_convert(hco$HUE,hco$VALUE,hco$CHROMA)
hco = aggregate(OBS_NO~PROJECT_CODE+SITE_ID,hco,min) %>%
  left_join(.,hco,by=c("PROJECT_CODE" = "PROJECT_CODE" , "SITE_ID" = "SITE_ID","OBS_NO"="OBS_NO"))
hco = aggregate(HOR_COL_NO~PROJECT_CODE+SITE_ID,hco,min) %>%
  left_join(.,hco,by =c("PROJECT_CODE" , "SITE_ID",'HOR_COL_NO' ))
#ggplot(data.frame(hco$COLOUR_CLASS), aes(x=hco$COLOUR_CLASS)) + geom_bar()

#hcu
hcu <- SALI$HCU
hcu <- aggregate(OBS_NO~PROJECT_CODE+SITE_ID,hcu,min) %>%
  left_join(.,hcu,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
hcu <- aggregate(CUTAN_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,hcu,min) %>%
  left_join(.,hcu,by=c("PROJECT_CODE" ,"SITE_ID","OBS_NO",'HORIZON_NO',"CUTAN_NO"))
hcu$CREATION_DATE <- as.character(hcu$CREATION_DATE)
hcu$LAST_UPDATE_DATE <- as.character(hcu$LAST_UPDATE_DATE)
hcu[hcu == "-"] <- NA

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
hst$CREATION_DATE <- as.character(hst$CREATION_DATE)
hst$LAST_UPDATE_DATE <- as.character(hst$LAST_UPDATE_DATE)
hst[hst == "-"] <- NA

hsg <- SALI$HSG
hsg <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,hsg,min) %>%
  left_join(.,hsg,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
hsg <- aggregate(SEG_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,hsg,min) %>%
  left_join(.,hsg,by=c("PROJECT_CODE" ,"SITE_ID","OBS_NO",'HORIZON_NO',"SEG_NO"))
names(hsg)[6]='HSG_ABUNDANCE'
hsg$CREATION_DATE <- as.character(hsg$CREATION_DATE)
hsg$LAST_UPDATE_DATE <- as.character(hsg$LAST_UPDATE_DATE)
hsg[hsg == "-"] <- NA


fts <- SALI$FTS[which(SALI$FTS$TEST_TYPE == 'PH' & !is.na(SALI$FTS$VALUE)), 
                c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','TEST_NO','VALUE')]
names(fts)[6] <- "FTS_PH"
fts$FTS_PH <-as.numeric(fts$FTS_PH)
# use mean value if multiple values available for one horizon 
fts<-  aggregate(FTS_PH ~ PROJECT_CODE+SITE_ID+HORIZON_NO,fts,mean)


# lab_15n1 <- lab[which(lab$LAB_METH_CODE == '15N1' & !is.na(lab$NUMERIC_VALUE)), 
#                 c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_METH_CODE','NUMERIC_VALUE')] %>%
#   aggregate(OBS_NO~PROJECT_CODE+SITE_ID,.,min) %>%
#   left_join(.,lab[which(lab$LAB_METH_CODE == '15N1' & !is.na(lab$NUMERIC_VALUE)), 
#                   c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO',
#                     'LAB_METH_CODE','NUMERIC_VALUE')],by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
# # nrow(unique(lab_15n1[,1:2]))
# names(lab_15n1)[7] <- "VALUE_15N1"
# # use min value(first) of the SAMPLE_NO if multiple values available for one horizon 
# lab_15n1<-  aggregate(SAMPLE_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_15n1,min) %>%
#   left_join(.,lab_15n1[,c('PROJECT_CODE','SITE_ID',"OBS_NO",'HORIZON_NO','SAMPLE_NO',
#                             "VALUE_15N1")])

# nrow(unique(lab_15n1[,1:2]))

#add extra data
names(lab_15n1_extra)[4] <- "VALUE_15N1"

lab_15n1 <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,lab_15n1_extra,min) %>%
  left_join(.,lab_15n1_extra,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO")) 

lab_15n1 <- aggregate(LD~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_15n1,min) %>%
  left_join(.,lab_15n1[,c('PROJECT_CODE','SITE_ID',"OBS_NO",'HORIZON_NO','LD','SAMPLE_NO',
                            "VALUE_15N1")])
# lab_15n1_new <- full_join(lab_15n1_test,lab_15n1)
# lab_15n1 <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,lab_15n1,min) %>%
#   left_join(.,lab_15n1,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO")) 
nrow(unique(lab_15n1[,1:5]))

lab_15n1<-  aggregate(SAMPLE_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_15n1,min) %>%
  left_join(.,lab_15n1[,c('PROJECT_CODE','SITE_ID',"OBS_NO",'HORIZON_NO','SAMPLE_NO',
                          "VALUE_15N1")])
lab_15n1<-  aggregate(VALUE_15N1~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_15n1,min) #%>%
  #left_join(.,lab_15n1[,c('PROJECT_CODE','SITE_ID',"OBS_NO",'HORIZON_NO','SAMPLE_NO',
  #                        "VALUE_15N1")])
nrow(unique(lab_15n1[,1:2]))

#lab_15n1_dup <- lab_15n1[which(duplicated(lab_15n1[,1:5]) |
 #                                    duplicated(lab_15n1[,1:5], fromLast = TRUE)),] 
# write.csv(lab_15n1_dup,file =paste("C:/Users/lzccn/iCloudDrive/DATA SCIENCE/DATA7703
#Machine Learing/Home Works/lab_15n1_dup",
#                                    ".csv",sep = " "))


#nrow(unique(fts[,1:3]))

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

lab_6b_6a <- lab[which(lab$LAB_METH_CODE %in% c('6A1','6B2a','6B2b','6B2c','6B4') & !is.na(lab$NUMERIC_VALUE)), 
                 c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_CODE','NUMERIC_VALUE')]
names(lab_6b_6a)[7] <- "VALUE_6B_6A"
# use mean value if multiple values available for one horizon 
lab_6b_6a<-  aggregate(VALUE_6B_6A~PROJECT_CODE+SITE_ID+HORIZON_NO,lab_6b_6a,mean)


# nrow(unique(lab_15n1[,1:2])) #91365
# nrow(unique(lab_15n1[,1:3])) #91365
# nrow(unique(lab_15n1[,c(1,2,3,4,5)])) #91365
# nrow(unique(lab_15n1[,c(1,2,3,4)]))


# Empty rate############################################################
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
  rate$EMPTY_RATE = label_percent()(rate$EMPTY_RATE)
  return(rate)
}
  
# empty_rate = data.frame('name'=NA,'RATE'=0)
# s1=s2=0
# for ( i in 1: length(SALI)) {
#   s = sum(is.na(SALI[[i]])) /(length(SALI[[i]])*nrow(SALI[[i]]))
#   s1 = s1+sum(is.na(SALI[[i]]))
#   s2 = s2+(length(SALI[[i]])*nrow(SALI[[i]]))
#   # s = label_percent()(s)
#   # s = format(s,digits = 3,nsmall = 2)
#   empty_rate[i,] =  list(names(SALI[i]),s )
#   #print(c(names(SALI[i]),sum(is.na(SALI[[i]]))/(length(SALI[[i]])*nrow(SALI[[i]]))))
# }
# 
# empty_rate = empty_rate[order(empty_rate$RATE,decreasing = T),]
# empty_rate$RATE = label_percent()(empty_rate$RATE)
# empty_rate = rbind(empty_rate,list('Total',s1/s2))
# empty_rate

# sum(is.na(n_sit))/(length(n_sit)*nrow(n_sit))
# n_hor = SALI$HOR[,-c(24:27)]
# sum(is.na(n_hor))/(length(n_hor)*nrow(n_hor))
# nrow(n_hor[which(n_hor$HORIZON_NAME == '-'),])
# SALI$OCL[which(SALI$OCL$PROJECT_CODE == 'BAS' & SALI$OCL$SITE_ID == 28),]
# sum(is.na(SALI$HCU))/(length(SALI$HCU)*nrow(SALI$HCU))
# sort(empty_rate$RATE, decreasing = T)
# format(empty_rate$RATE,nsmall = 2)

# empty_rate_kmean = data.frame('name'=NA,'RATE'=0)
# for ( i in 5: length(kmean_dat2)) {
#   s = sum(is.na(kmean_dat2[[i]])/length(kmean_dat2[[i]]))
#   # s = label_percent()(s)
#   # s = format(s,digits = 3,nsmall = 2)
#   empty_rate_kmean[(i-4),] =  list(names(kmean_dat2)[i], s)
#   #print(c(names(SALI[i]),sum(is.na(SALI[[i]]))/(length(SALI[[i]])*nrow(SALI[[i]]))))
# }
# empty_rate_kmean = empty_rate_kmean[order(empty_rate_kmean$RATE,decreasing = T),]
# empty_rate_kmean = rbind(empty_rate_kmean,
#                          list('Total',
#                               sum(is.na(kmean_dat2[,-c(1:4)]))/((length(kmean_dat2)-4)*nrow(kmean_dat2))))
# empty_rate_kmean$RATE = label_percent(0.01 )(empty_rate_kmean$RATE)
# empty_rate_kmean
# #sum(is.na(kmean_dat2[[22]])/length(kmean_dat2[[22]]))


unique(SALI$FTS[which(SALI$FTS$TEST_TYPE=="PH"),'VALUE'])
length(unique(SALI$FTS[which(SALI$FTS$TEST_TYPE=="PH"),'VALUE']))
unique(SALI$HOR$UPPER_DEPTH)
length(unique(SALI$HOR$UPPER_DEPTH))
unique(lab[which(lab$LAB_METH_CODE=="13C1_Fe"),'NUMERIC_VALUE'])
length(unique(lab[which(lab$LAB_METH_CODE=="13C1_Fe"),'NUMERIC_VALUE']))
unique(lab[which(lab$LAB_METH_CODE %in% c('6A1','6B2a','6B2b','6B2c','6B4')),'NUMERIC_VALUE'])
length(unique(lab[which(lab$LAB_METH_CODE %in% c('6A1','6B2a','6B2b','6B2c','6B4')),'NUMERIC_VALUE']))
unique(lab[which(lab$LAB_METH_CODE %in% c('15N1')),'NUMERIC_VALUE'])
length(unique(lab[which(lab$LAB_METH_CODE %in% c('15N1')),'NUMERIC_VALUE']))
unique(lab[which(lab$LAB_METH_CODE %in% c('4A1')),'NUMERIC_VALUE'])
length(unique(lab[which(lab$LAB_METH_CODE %in% c('4A1')),'NUMERIC_VALUE']))
unique(lab[which(lab$LAB_METH_CODE %in% c('2Z2_Clay')),'NUMERIC_VALUE'])
length(unique(lab[which(lab$LAB_METH_CODE %in% c('2Z2_Clay')),'NUMERIC_VALUE']))


# Asc_train  full data set ############################################################
asc_train <- 
  hor[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')] %>%
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')],)  %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')]) %>%
  left_join(.,osc[,c('PROJECT_CODE','SITE_ID','STATUS')]) %>%
  left_join(.,hor[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                     'UPPER_DEPTH','LOWER_DEPTH','BOUND_DISTINCT','SOIL_WATER_STAT')]) %>%
  left_join(.,fts,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_13c1_fe,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_15n1_new,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_6b_6a,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_4a1,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hor[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','DESIGN_MASTER',
                     'HORIZON_NAME','TEXTURE_CODE')]) %>%
  left_join(.,hcu[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','CUTAN_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','PEDALITY_TYPE','PEDALITY_GRADE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hsg[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','NATURE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,ocl[,c('PROJECT_CODE','SITE_ID','ASC_ORD')])  %>%
  anti_join(.,.[which(.$ASC_ORD == '-' | is.na(.$ASC_ORD) #|
                      #.$DESIGN_MASTER == '-' | is.na(.$DESIGN_MASTER)|
                      #.$HORIZON_NAME == '-' | is.na(.$HORIZON_NAME)|
                      #.$BOUND_DISTINCT == '-' | is.na(.$BOUND_DISTINCT)#|
                      #.$TEXTURE_CODE == '-'|is.na(.$TEXTURE_CODE)|
                      #.$BOUND_DISTINCT == '-'|is.na(.$BOUND_DISTINCT)|
                      #.$SOIL_WATER_STAT == '-'|is.na(.$SOIL_WATER_STAT)|
                      #.$ELEM_TYPE_CODE == '-'|is.na(.$ELEM_TYPE_CODE)#|
                      #.$CUTAN_TYPE == '-'|is.na(.$CUTAN_TYPE)|
                      #.$PEDALITY_TYPE == '-'|is.na(.$PEDALITY_TYPE)#|
                      #.$PEDALITY_GRADE == '-'|is.na(.$PEDALITY_GRADE)|
                      #.$DRAINAGE == '-'|is.na(.$DRAINAGE)|
                      #.$STATUS == '-'|is.na(.$STATUS)
                      #.$NATURE == '-'|is.na(.$NATURE)
   ),],
   by = c('PROJECT_CODE','SITE_ID'))


asc_train_empty_rate_F = empty_rate(asc_train[,5:24])
asc_train_s1 <- asc_train[1:12,]

# asc_tr_1 <- asc_train %>% distinct(PROJECT_CODE,SITE_ID,OBS_NO,
#                                       DRAINAGE,ELEM_TYPE_CODE,STATUS,ASC_ORD,)
# asc_tr_1_empty_rate = empty_rate(asc_tr_1[,4:6])
# 
# z1 <- split(asc_train, list(asc_train$PROJECT_CODE,asc_train$SITE_ID),drop = T)
# z2 <- as.list(NA)
# for (i in 1 : length(z1)){
#   z2[[i]] <- subset(z1[[i]],select = c(6:15,18:24))
# }
# asc_tr_1$HOR <- z2
# # asc_tr_1 <- filter(dg,ASC_ORD !='TE')
# 
# asc_tr_s1 <- asc_tr_1[sample(nrow(asc_tr_1),10),]
#asc_train_s1 <- asc_train[sample(nrow(asc_train),10),]

# ASC_train_split Training set with split HORIZON_NAME ############################################
asc_train_split <- 
  hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')] %>%
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')],)  %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')]) %>%
  left_join(.,osc[,c('PROJECT_CODE','SITE_ID','STATUS')]) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                     'UPPER_DEPTH','LOWER_DEPTH','BOUND_DISTINCT','SOIL_WATER_STAT')]) %>%
  left_join(.,fts,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_13c1_fe,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_15n1,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,lab_6b_6a,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_4a1,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                     'HOR_PREFIX','HOR_MASTER','HOR_SUBHOR','HOR_SUFFIX', 'TEXTURE_CODE')]) %>%
  left_join(.,hcu[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','CUTAN_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','PEDALITY_TYPE','PEDALITY_GRADE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hsg[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','NATURE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,ocl[,c('PROJECT_CODE','SITE_ID','ASC_ORD')])  %>%
  anti_join(.,.[which(.$ASC_ORD == '-' | is.na(.$ASC_ORD) #|
                      #.$DESIGN_MASTER == '-' | is.na(.$DESIGN_MASTER)|
                      #.$HORIZON_NAME == '-' | is.na(.$HORIZON_NAME)|
                      #.$BOUND_DISTINCT == '-' | is.na(.$BOUND_DISTINCT)#|
                      #.$TEXTURE_CODE == '-'|is.na(.$TEXTURE_CODE)|
                      #.$BOUND_DISTINCT == '-'|is.na(.$BOUND_DISTINCT)|
                      #.$SOIL_WATER_STAT == '-'|is.na(.$SOIL_WATER_STAT)|
                      #.$ELEM_TYPE_CODE == '-'|is.na(.$ELEM_TYPE_CODE)#|
                      #.$CUTAN_TYPE == '-'|is.na(.$CUTAN_TYPE)|
                      #.$PEDALITY_TYPE == '-'|is.na(.$PEDALITY_TYPE)#|
                      #.$PEDALITY_GRADE == '-'|is.na(.$PEDALITY_GRADE)|
                      #.$DRAINAGE == '-'|is.na(.$DRAINAGE)|
                      #.$STATUS == '-'|is.na(.$STATUS)
                      #.$NATURE == '-'|is.na(.$NATURE)
  ),],
  by = c('PROJECT_CODE','SITE_ID'))
# remove TE from the dataset
asc_train_split = filter(asc_train_split,ASC_ORD !='TE')
write.csv(asc_train_split,file ="asc_train_split.csv",row.names = FALSE)
asc_train_empty_rate = empty_rate(asc_train_split[,5:24])

# ASC_train_sub Training set with split HORIZON_NAME ############################################
asc_train_sub <- 
  hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')] %>%
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')],)  %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')]) %>%
  left_join(.,osc[,c('PROJECT_CODE','SITE_ID','STATUS')]) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'UPPER_DEPTH','LOWER_DEPTH','BOUND_DISTINCT','SOIL_WATER_STAT')]) %>%
  left_join(.,fts,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_13c1_fe,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_15n1,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,lab_6b_6a,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_4a1,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'HOR_PREFIX','HOR_MASTER','HOR_SUBHOR','HOR_SUFFIX', 'TEXTURE_CODE')]) %>%
  left_join(.,hcu[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','CUTAN_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','PEDALITY_TYPE','PEDALITY_GRADE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hsg[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','NATURE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hco[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','COLOUR_CLASS')]) %>%
  left_join(.,ocl[,c('PROJECT_CODE','SITE_ID','ASC_ORD','SUBORD_ASC_CODE',
                     'GREAT_GROUP_ASC_CODE','SUBGROUP_ASC_CODE')])  %>%
  anti_join(.,.[which(.$ASC_ORD == '-' | is.na(.$ASC_ORD) #|
                      #.$DESIGN_MASTER == '-' | is.na(.$DESIGN_MASTER)|
                      #.$HORIZON_NAME == '-' | is.na(.$HORIZON_NAME)|
                      #.$BOUND_DISTINCT == '-' | is.na(.$BOUND_DISTINCT)#|
                      #.$TEXTURE_CODE == '-'|is.na(.$TEXTURE_CODE)|
                      #.$BOUND_DISTINCT == '-'|is.na(.$BOUND_DISTINCT)|
                      #.$SOIL_WATER_STAT == '-'|is.na(.$SOIL_WATER_STAT)|
                      #.$ELEM_TYPE_CODE == '-'|is.na(.$ELEM_TYPE_CODE)#|
                      #.$CUTAN_TYPE == '-'|is.na(.$CUTAN_TYPE)|
                      #.$PEDALITY_TYPE == '-'|is.na(.$PEDALITY_TYPE)#|
                      #.$PEDALITY_GRADE == '-'|is.na(.$PEDALITY_GRADE)|
                      #.$DRAINAGE == '-'|is.na(.$DRAINAGE)|
                      #.$STATUS == '-'|is.na(.$STATUS)
                      #.$NATURE == '-'|is.na(.$NATURE)
  ),],
  by = c('PROJECT_CODE','SITE_ID'))
# remove TE from the dataset
asc_train_sub = filter(asc_train_sub,ASC_ORD !='TE')
#write.csv(asc_train_sub,file ="./1_asc_mlp/asc_train_sub.csv",row.names = FALSE)
write_rds(asc_train_sub,"./1_asc_mlp/asc_train_sub.rds")

#asc_train_empty_rate = empty_rate(asc_train_split[,5:24])


# Predict dataset####################################################################################
asc_predict_split <- 
  hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')] %>%
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')],)  %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')]) %>%
  left_join(.,osc[,c('PROJECT_CODE','SITE_ID','STATUS')]) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'UPPER_DEPTH','LOWER_DEPTH','BOUND_DISTINCT','SOIL_WATER_STAT')]) %>%
  left_join(.,fts,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_13c1_fe,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_15n1,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,lab_6b_6a,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_4a1,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'HOR_PREFIX','HOR_MASTER','HOR_SUBHOR','HOR_SUFFIX', 'TEXTURE_CODE')]) %>%
  left_join(.,hcu[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','CUTAN_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','PEDALITY_TYPE','PEDALITY_GRADE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hsg[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','NATURE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,ocl[,c('PROJECT_CODE','SITE_ID','ASC_ORD')]) # %>%
  #anti_join(.,.[which(.$ASC_ORD != '-' | !is.na(.$ASC_ORD) #|
                      #.$DESIGN_MASTER == '-' | is.na(.$DESIGN_MASTER)|
                      #.$HORIZON_NAME == '-' | is.na(.$HORIZON_NAME)|
                      #.$BOUND_DISTINCT == '-' | is.na(.$BOUND_DISTINCT)#|
                      #.$TEXTURE_CODE == '-'|is.na(.$TEXTURE_CODE)|
                      #.$BOUND_DISTINCT == '-'|is.na(.$BOUND_DISTINCT)|
                      #.$SOIL_WATER_STAT == '-'|is.na(.$SOIL_WATER_STAT)|
                      #.$ELEM_TYPE_CODE == '-'|is.na(.$ELEM_TYPE_CODE)#|
                      #.$CUTAN_TYPE == '-'|is.na(.$CUTAN_TYPE)|
                      #.$PEDALITY_TYPE == '-'|is.na(.$PEDALITY_TYPE)#|
                      #.$PEDALITY_GRADE == '-'|is.na(.$PEDALITY_GRADE)|
                      #.$DRAINAGE == '-'|is.na(.$DRAINAGE)|
                      #.$STATUS == '-'|is.na(.$STATUS)
                      #.$NATURE == '-'|is.na(.$NATURE)
  #),]
  #by = c('PROJECT_CODE','SITE_ID'))
nrow(asc_predict_split[which(asc_predict_split$ASC_ORD == 'TE'),])
#asc_predict_split = filter(asc_predict_split,ASC_ORD !='TE')
# Normal distribution test ###############################################################################
a = aggregate(HORIZON_NO~PROJECT_CODE+SITE_ID, data = spc_kmean_dat ,FUN = length)
plt <- ggplot(a) + geom_bar(aes(x=PROJECT_CODE, y=HORIZON_NO), stat="identity")
print(plt)
histogram(a$HORIZON_NO)
hist(a$HORIZON_NO,xlim = c(0,20),breaks = 30)
nrow(a[a$HORIZON_NO > 6,])
nrow(a[a$HORIZON_NO < 6,])
nrow(a[a$HORIZON_NO > 6,])/nrow(a)
median(a$HORIZON_NO)
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(a$HORIZON_NO)
nrow(a[a$HORIZON_NO == 4,])




# SPC training dataset##############################################################

spc_train <- 
  hor_spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')] %>%
  left_join(.,osc[,c('PROJECT_CODE','SITE_ID','STATUS')]) %>%
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','OBS_LITH_CODE')],)  %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID', 'REL_MOD_SLOPE_CLASS')]) %>%
  #left_join(.,ods[,c('PROJECT_CODE','SITE_ID', 'DISTURB_TYPE')]) %>%
  #left_join(.,vst[,c('PROJECT_CODE','SITE_ID','OBS_NO','GROWTH_FORM','HEIGHT_CLASS','COVER_CLASS')]) %>%
  left_join(.,vsp[,c('PROJECT_CODE','SITE_ID','OBS_NO','VEG_SPEC_CODE')]) %>%
  left_join(.,hor_spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SPC_HOR_MASTER')]) %>%
  left_join(.,hco[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','COLOUR_CLASS')]) %>%
  left_join(.,hmt[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','MOTT_TYPE')]) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','TEXTURE_CODE')]) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','PEDALITY_GRADE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hsg[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','NATURE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,ocf[,c('PROJECT_CODE','SITE_ID','OBS_NO','OCF_LITH_CODE')]) %>%
  left_join(.,hcu[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','CUTAN_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,fts,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hor_spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','BOUND_DISTINCT')]) %>%
  left_join(.,spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','SPC')])  #%>%
#  anti_join(.,.[which(.$SPC == '-' | is.na(.$SPC) #|
                      #.$DESIGN_MASTER == '-' | is.na(.$DESIGN_MASTER)|
                      #.$HORIZON_NAME == '-' | is.na(.$HORIZON_NAME)|
                      #.$BOUND_DISTINCT == '-' | is.na(.$BOUND_DISTINCT)#|
                      #.$TEXTURE_CODE == '-'|is.na(.$TEXTURE_CODE)|
                      #.$BOUND_DISTINCT == '-'|is.na(.$BOUND_DISTINCT)|
                      #.$SOIL_WATER_STAT == '-'|is.na(.$SOIL_WATER_STAT)|
                      #.$ELEM_TYPE_CODE == '-'|is.na(.$ELEM_TYPE_CODE)#|
                      #.$CUTAN_TYPE == '-'|is.na(.$CUTAN_TYPE)|
                      #.$PEDALITY_TYPE == '-'|is.na(.$PEDALITY_TYPE)#|
                      #.$PEDALITY_GRADE == '-'|is.na(.$PEDALITY_GRADE)|
                      #.$DRAINAGE == '-'|is.na(.$DRAINAGE)|
                      #.$STATUS == '-'|is.na(.$STATUS)
                      #.$NATURE == '-'|is.na(.$NATURE)
#  ),],
#  by = c('PROJECT_CODE','SITE_ID'))
spc_train = na_if(spc_train,'-')
#ggplot(data.frame(spc_train$SPC), aes(x=spc_train$SPC)) + geom_bar()
write.csv(spc_train,file ="spc_train.csv", row.names = FALSE)
# ASC_SPC_mix Training set with split HORIZON_NAME ############################################
asc_spc <- 
  # base
  hor_spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')] %>%
  left_join(.,osc[,c('PROJECT_CODE','SITE_ID','STATUS')]) %>%
  left_join(.,fts,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','PEDALITY_GRADE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'UPPER_DEPTH','LOWER_DEPTH','BOUND_DISTINCT')]) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','TEXTURE_CODE')]) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'HOR_PREFIX','HOR_SUBHOR','HOR_SUFFIX')]) %>%
  left_join(.,hcu[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','CUTAN_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hsg[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','NATURE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  #asc_ex
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')]) %>%
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')],)  %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'SOIL_WATER_STAT')]) %>%
  left_join(.,lab_13c1_fe,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_15n1,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,lab_6b_6a,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_4a1,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'HOR_MASTER')]) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','PEDALITY_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  #spc_ex
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','OBS_LITH_CODE')],)  %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID', 'REL_MOD_SLOPE_CLASS')]) %>%
  #left_join(.,ods[,c('PROJECT_CODE','SITE_ID', 'DISTURB_TYPE')]) %>%
  #left_join(.,vst[,c('PROJECT_CODE','SITE_ID','OBS_NO','GROWTH_FORM','HEIGHT_CLASS','COVER_CLASS')]) %>%
  left_join(.,vsp[,c('PROJECT_CODE','SITE_ID','OBS_NO','VEG_SPEC_CODE')]) %>%
  left_join(.,hor_spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SPC_HOR_MASTER')]) %>%
  left_join(.,hmt[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','MOTT_TYPE')]) %>%
  left_join(.,ocf[,c('PROJECT_CODE','SITE_ID','OBS_NO','OCF_LITH_CODE')]) %>%
  #so_ex
  left_join(.,hco[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','COLOUR_CLASS')]) %>%
  #mottle tpye in spc
  #gg_ex
  #sg_ex
  #asc_tag
  left_join(.,ocl[,c('PROJECT_CODE','SITE_ID','ASC_ORD','SUBORD_ASC_CODE',
                     'GREAT_GROUP_ASC_CODE','SUBGROUP_ASC_CODE')])  %>%
  #spc_tag
  left_join(.,spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','SPC')]) %>%
  # DELETE NA TAG DATA
   anti_join(.,.[which(.$ASC_ORD == '-' | is.na(.$ASC_ORD)|
                       .$SUBORD_ASC_CODE == '-' | is.na(.$SUBORD_ASC_CODE)|
                       .$GREAT_GROUP_ASC_CODE == '-' | is.na(.$GREAT_GROUP_ASC_CODE)|
                       .$SUBGROUP_ASC_CODE == '-' | is.na(.$SUBGROUP_ASC_CODE)|
                       .$SPC == '-' | is.na(.$SPC)
                       ),],
             by = c('PROJECT_CODE','SITE_ID'))
# remove TE from the dataset
#asc_spc = filter(asc_spc,ASC_ORD !='TE')
write_rds(asc_spc,"./0_general/asc_spc.rds")
# ASC_SUB ##################################
asc_sub <- 
  # base
  hor_spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')] %>%
  left_join(.,osc[,c('PROJECT_CODE','SITE_ID','STATUS')]) %>%
  left_join(.,sit[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')]) %>%
  left_join(.,fts,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','PEDALITY_GRADE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'UPPER_DEPTH','LOWER_DEPTH','BOUND_DISTINCT')]) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','TEXTURE_CODE')]) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'HOR_PREFIX','HOR_SUBHOR','HOR_SUFFIX')]) %>%
  left_join(.,hcu[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','CUTAN_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hst[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','PEDALITY_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hsg[,c('PROJECT_CODE','SITE_ID','HORIZON_NO','NATURE')],
            by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  #asc_ex
  left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')],)  %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'SOIL_WATER_STAT')]) %>%
  left_join(.,lab_13c1_fe,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_15n1,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,lab_6b_6a,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_4a1,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','HORIZON_NO')) %>%
  left_join(.,hor_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
                           'HOR_MASTER')]) %>%
  #spc_ex
  # left_join(.,obs[,c('PROJECT_CODE','SITE_ID','OBS_NO','OBS_LITH_CODE')],)  %>%
  # left_join(.,sit[,c('PROJECT_CODE','SITE_ID', 'REL_MOD_SLOPE_CLASS')]) %>%
  # #left_join(.,ods[,c('PROJECT_CODE','SITE_ID', 'DISTURB_TYPE')]) %>%
  # #left_join(.,vst[,c('PROJECT_CODE','SITE_ID','OBS_NO','GROWTH_FORM','HEIGHT_CLASS','COVER_CLASS')]) %>%
  # left_join(.,vsp[,c('PROJECT_CODE','SITE_ID','OBS_NO','VEG_SPEC_CODE')]) %>%
  # left_join(.,hor_spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SPC_HOR_MASTER')]) %>%
  # left_join(.,hmt[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','MOTT_TYPE')]) %>%
  # left_join(.,ocf[,c('PROJECT_CODE','SITE_ID','OBS_NO','OCF_LITH_CODE')]) %>%
  #so_ex
  left_join(.,hco[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','COLOUR_CLASS')]) %>%
  left_join(.,hmt[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','MOTT_TYPE')]) %>%
  #mottle tpye in spc
  #gg_ex
  #sg_ex
  #asc_tag
  left_join(.,ocl[,c('PROJECT_CODE','SITE_ID','ASC_ORD','SUBORD_ASC_CODE',
                     'GREAT_GROUP_ASC_CODE','SUBGROUP_ASC_CODE')])  %>%
  #spc_tag
  #left_join(.,spc[,c('PROJECT_CODE','SITE_ID','OBS_NO','SPC')]) %>%
  # DELETE NA TAG DATA
  anti_join(.,.[which(.$ASC_ORD == '-' | is.na(.$ASC_ORD)#|
                        #.$SUBORD_ASC_CODE == '-' | is.na(.$SUBORD_ASC_CODE)|
                        #.$GREAT_GROUP_ASC_CODE == '-' | is.na(.$GREAT_GROUP_ASC_CODE)|
                        #.$SUBGROUP_ASC_CODE == '-' | is.na(.$SUBGROUP_ASC_CODE)#|
                        #.$SPC == '-' | is.na(.$SPC)
  ),],
  by = c('PROJECT_CODE','SITE_ID'))
# remove TE from the dataset
#asc_sub = filter(asc_sub,ASC_ORD !='TE')
nrow(unique(asc_sub[,1:2]))
write_rds(asc_sub,"./0_general/asc_sub_new.rds")

# for (i in names(SALI)) {
# assign(i,SALI[[i]])
# }



