# Prepare datasets for ORD classification, only keep necessary features.
# Libraries############################
library(readr)
library(dplyr)
library(tidyr)
library(scales) # label percent
# + Input source data####################
SALI <-readRDS("./0_general/SALI_SIT-data_20191101.rds")
LAB <- read.csv("./0_general/labresults.csv")
lab_15n1_extra <- read.csv('./0_general/15N1.csv') # Peter supplied on 27/May/2020 email
# + Subset data sets from SALI#####################

# split subsets
# names_subset_ord = c('SIT','HOR','OBS','HCU','HST','FTS','OCL','OSC','HSG')
# for (i in names_subset_ord) {
#   assign(i,SALI[[i]])
# }

# subsets preparation
# ++ SIT######################
SIT = SALI$SIT[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')]
# chech unique domain values
unique(SIT$ELEM_TYPE_CODE)
SIT[SIT == "-"] <- NA
# [1] "PLA" "DDE" "FAN" "LEV" "PST" "PED" "HCR" "HSL" "BEN" "FOO" NA    "VLF" "BAR" "SFS" "BKP" "SCA" "TEP" "SCR" "GUL" "SWP" "BRI" "SWL" "BEA"
# [24] "BAN" "ITF" "STC" "STF" "TEF" "CBE" "CON" "CRA" "DUN" "DUS" "PLY" "SCD" "SUS" "STB" "OXB" "TDF" "RER" "DUC" "FLD" "RIS" "CLI" "BRK" "CFS"
# [47] "HIL" "EMB" "ALC" "MOU" "FOR" "SRP" "TDC" "TRE" "LDS" "LUN" "DAM" "ETF" "PIT" "LAK" "LAG" "BER" "REC" "RES" "CUT" "BOU" "TOR" "REF" "CKF"
# [70] "FIL" "CKT" "TAL" "EST"
length(unique(SIT$ELEM_TYPE_CODE))
#73

# ++ OBS#########################
OBS = SALI$OBS[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')]
# only use min(OBS_NO) if there are multiple observations for unique site
OBS <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,OBS,min) %>%
  left_join(.,OBS,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
# chech unique domain values
unique(OBS$DRAINAGE)
# NA  "5" "3" "2" "4" "6" "1"

# ++ OSC###############
OSC = SALI$OSC[,c('PROJECT_CODE','SITE_ID','OBS_NO','SURF_COND_NO','STATUS')]
# only use min(OBS_NO) if there are multiple observations for unique site
OSC <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,OSC,min) %>%
  left_join(.,OSC,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
# only use min(SURF_COND_NO) if there are multiple observations for unique site
OSC <-  aggregate(SURF_COND_NO~PROJECT_CODE+SITE_ID+OBS_NO,OSC,min) %>%
  left_join(.,OSC,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO","SURF_COND_NO"))
# chech unique domain values
unique(OSC$STATUS)
# "G" "L" "H" "C" "R" "F" "X" "S" "M" "T" "P" "Z" "O" "Y"

# ++ OCL#################
OCL = SALI$OCL[,c('PROJECT_CODE','SITE_ID','OBS_NO','SOIL_CLASS_NO','ASC_CONFIDENCE','ASC_ORD')]
# only use min(OBS_NO) if there are multiple observations for unique site
OCL <- aggregate(OBS_NO~PROJECT_CODE+SITE_ID,OCL,min) %>%
  left_join(.,OCL,by=c("PROJECT_CODE" = "PROJECT_CODE" , "SITE_ID" = "SITE_ID","OBS_NO"="OBS_NO"))
#eliminate duplcates with different SOIL_CLASS_NO, only keep the one SOIL_CLASS_NO == MIN(MOST ARE 1)
OCL <- aggregate(SOIL_CLASS_NO~PROJECT_CODE+SITE_ID+OBS_NO,OCL,min) %>%
  left_join(.,OCL,by=c("PROJECT_CODE" , "SITE_ID",'OBS_NO',"SOIL_CLASS_NO"))
OCL = OCL[,c("PROJECT_CODE" , "SITE_ID",'ASC_CONFIDENCE','ASC_ORD')]
# extract test dataset, ASC_ORD is not NA, and ASC_CONFIDENCE == 1
OCL_ts <- aggregate(ASC_ORD~PROJECT_CODE+SITE_ID,OCL,max) %>%
  left_join(.,OCL,by=c("PROJECT_CODE" , "SITE_ID","ASC_ORD"))%>%
  filter(.,ASC_CONFIDENCE ==1)
unique(OCL_ts$ASC_ORD)
# "VE" "SO" "CH" "FE" "HY" "DE" "OR" "RU" "TE" "KA" "KU" "CA" "PO" "AN"
# extract training dataset, ASC_ORD is not NA, and ASC_CONFIDENCE !=1 or NA
OCL_tr <- aggregate(ASC_ORD~PROJECT_CODE+SITE_ID,OCL,max) %>%
  left_join(.,OCL,by=c("PROJECT_CODE" , "SITE_ID","ASC_ORD"))%>%
  filter(.,ASC_CONFIDENCE !=1|is.na(ASC_CONFIDENCE))
unique(OCL_tr$ASC_ORD)
# "FE" "DE" "KA" "HY" "SO" "CH" "TE" "VE" "RU" "KU" "PO" "AN" "CA" "OR"

# ++ HOR######################
HOR = SALI$HOR[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','UPPER_DEPTH','LOWER_DEPTH','TEXTURE_CODE',
             'SOIL_WATER_STAT','BOUND_DISTINCT','HORIZON_NAME','DESIGN_MASTER')]

HOR <- aggregate(OBS_NO~PROJECT_CODE+SITE_ID,HOR,min) %>%
  left_join(.,HOR,by=c("PROJECT_CODE" = "PROJECT_CODE" , "SITE_ID" = "SITE_ID","OBS_NO"="OBS_NO"))
HOR[HOR == "-"] <- NA
unique(HOR$TEXTURE_CODE)
# [1] "LMC"   "MC"    "MHC"   "CL"    "LC"    "CLFS"  "HC"    "SC"    NA      "FSMC"  "SMC"   "CLS"   "LFSY"  "FSCL"  "FSC"   "FS"    "KS"   
# [18] "LS"    "FSL"   "LKS"   "S"     "SL"    "SLC"   "SCL"   "FSLC"  "LFS"   "CKS"   "CS"    "ZMC"   "ZC"    "SHC"   "ZCL"   "L"     "ZLC"  
# [35] "SCLFS" "FSLMC" "SMHC"  "KSL"   "KSCL"  "KSC"   "KSLMC" "ZMHC"  "KSLC"  "KSMC"  "LCFS"  "CLKS"  "SLMC"  "MCS"   "LMCS"  "CFS"   "ZHC"  
# [52] "MCFS"  "ZL"    "KSHC"  "LMCZ"  "LMCKS" "SLZ"   "ZLMC"  "ZLCFS" "LMCFS" "MSC"   "KSMHC" "CLFSZ" "FSCLZ" "FSMHC" "IP"    "LCS"   "FSHC" 
# [69] "MCZ"   "GR"    "LFSYZ" "MHCFS" "LCKS"  "MHCS"  "CLZ"   "LCZ"   "LMS"   "FSLCZ" "MS"    "ZLMCS" "HP"    "LP"    "ST"    "LSY"   "AP"   
# [86] "SS"    "FSLZ"  "CSC"   "KSS"   "CLMS"  "SP"    "GP"    "CP"   
unique(HOR$SOIL_WATER_STAT)
# NA  "M" "D" "T" "W"
unique(HOR$BOUND_DISTINCT) #  ? 1
# NA  "D" "A" "C" "G" "S" "1"
HOR$BOUND_DISTINCT[HOR$BOUND_DISTINCT=='1']=NA
unique(HOR$BOUND_DISTINCT)

# clean domain values of DESIGN_MASTER
unique(HOR$DESIGN_MASTER)
# [1] "A"   "B"   NA    "D"   "O"   "C"   "BC"  "R"   "M"   "B/C" "AB"  "P"   "A/C" "AC"  "B?"  "C/B" "S"   "A/B" "BD"  "b"   "?"   "B/A" "D?" 
# [24] "BT"  "a"   "A?"  "B+"  "AO"  "B/D" "DK"  "BK"  "UB" 
HOR$DESIGN_MASTER = gsub('\\/|\\?|\\+','',HOR$DESIGN_MASTER)
unique(HOR$DESIGN_MASTER)
HOR$DESIGN_MASTER = gsub('a','A',HOR$DESIGN_MASTER)
unique(HOR$DESIGN_MASTER)
HOR$DESIGN_MASTER = gsub('b','B',HOR$DESIGN_MASTER)
unique(HOR$DESIGN_MASTER)
HOR$DESIGN_MASTER = gsub('CB','BC',HOR$DESIGN_MASTER)
unique(HOR$DESIGN_MASTER)
HOR[HOR == ""] <- NA
unique(HOR$DESIGN_MASTER)
# delete data with "S" or "UB" as DESIGN_MASTER
HOR <- HOR[grep("S|UB",HOR$DESIGN_MASTER),] %>%
  distinct(PROJECT_CODE,SITE_ID) %>%
  anti_join(HOR,.,by = c('PROJECT_CODE','SITE_ID'))
unique(HOR$DESIGN_MASTER) # ? any further cleaning?
# "A"  "B"  NA   "D"  "O"  "C"  "BC" "R"  "M"  "AB" "P"  "AC" "BD" "BA" "BT" "AO" "DK" "BK"

# clean and split HORIZON_NAME
HOR$HORIZON_NAME = gsub("\\Q+\\E|\\Q?\\E",'',HOR$HORIZON_NAME )
HOR$HORIZON_NAME = gsub("b23",'B23',HOR$HORIZON_NAME)
HOR$HORIZON_NAME = gsub("a1",'A1',HOR$HORIZON_NAME)
# spit HOR_PREFIX from HORIZON_NAME 
HOR_split = separate(HOR,HORIZON_NAME,c('HOR_PREFIX','HORIZON_NAME'),sep="(?<=[0-9]|[0-9][0-9]|^)(?=[A-Za-z]|$)",
                     remove = T,convert = T,extra = "merge",fill = "left")
unique(HOR_split$HOR_PREFIX) # ? do we need this column?
# [1] NA  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
# [48] 47
HOR_split$HORIZON_NAME = gsub("Ap",'AP',HOR_split$HORIZON_NAME)
HOR_split$HORIZON_NAME = gsub("AB|A/B",'A3',HOR_split$HORIZON_NAME)
HOR_split$HORIZON_NAME = gsub("B3/2D",'B3',HOR_split$HORIZON_NAME)
HOR_split$HORIZON_NAME = gsub("BCC",'BC',HOR_split$HORIZON_NAME)
HOR_split$HORIZON_NAME = gsub("C/B",'C',HOR_split$HORIZON_NAME)
# split HOR_MASTER from HORIZON_NAME
HOR_split= separate(HOR_split,HORIZON_NAME,c('HOR_MASTER','HORIZON_NAME'),
                    sep="((?<=([AB]([0-9]|\\b))|[OPCDRMSU])(?=[0-9a-z]|$))|((?<=[ABOP])(?=[a-z]))",
                    remove = T,convert = T,extra = "merge",fill = "left")
sort(unique(HOR_split$HOR_MASTER)) # ? please check
# [1] "A"   "A/C" "A0"  "A1"  "A2"  "A3"  "A4"  "AA"  "AC"  "AO"  "AP"  "B"   "B/A" "B/C" "B1"  "B2"  "B3"  "B4"  "BC"  "BD"  "C"   "D"   "M"  
# [24] "O"   "P"   "R" 

#split the left of HORIZON_NAME into HOR_SUBHOR and HOR_SUFIFIX
HOR_split= separate(HOR_split,HORIZON_NAME,c('HOR_SUBHOR','HOR_SUFFIX'),sep="(?<=[0-9])(?=[a-z]|$)",
                    remove = T,convert = T,extra = "merge",fill = "left")
unique(HOR_split$HOR_SUBHOR) # ? need this column?
# [1] "0"  "1"  "10" "11" "12" "13" "14" "15" "16" "17" "2"  "21" "22" "23" "24" "25" "3"  "31" "32" "33" "34" "4"  "41" "42" "43" "5"  "51" "52"
# [29] "6"  "7"  "71" "72" "8"  "9"  "b1" "b2" "c1" "c2" "j1" "j2" "p1" "p2"
HOR_split[HOR_split == ""] <- NA
unique(HOR_split$HOR_SUFFIX)
# [1] NA    "t"   "e"   "c"   "it"  "j"   "p"   "y"   "i"   "u"   "k"   "g"   "tu"  "bu"  "tk"  "b"   "h"   "ia"  "pj"  "sb"  "tj"  "hs"  "ca" 
# [24] "m"   "a"   "bkc" "bc"  "ec"  "s"   "ep"  "ct"  "fm"  "iu"  "ai"  "f"   "tc"  "n"   "w"   "bi"  "sh"  "ty"  "bk"  "tfm" "ap"  "ce"  "q"  
# [47] "qm"  "bai" "eq"  "cb"  "ck"  "kt"  "efm" "d"   "ts"  "x"   "tsm" "tn"  "yk"  "cfm" "ab"  "ky"  "sp"  "em"  "ie"  "r"   "wc"  "st"  "ecb"
# [70] "gct" "eb"  "jt"  "mb"  "kc"  "km"  "jc"  "tm"  "B2T" "iy"  "sx"  "jp"  "et"  "btg" "ebc" "cg"  "pe"  "te"  "kb"  "be"  "tb"  "je"  "bg" 
# [93] "iua" "bp"  "er"  "ui"  "cy"  "tcm" "by"  "es"  "cm"  "bj"  "ckt" "me"  "mc"  "hb"  "yi"  "ak"  "gh"  "tw"  "bt"  "ej"  "qh"  "pm"  "ib" 
# [116] "gb"  "fmc" "cbp" "iau" "2B"  "db"  "ta"  "ba"  "jfm" "pa"  "tky" "kf"  "tf"  "z"   "ks"  "ic"  "ci"  "tyc" "bia" "jm"  "DK"  "tia" "jb" 
# [139] "4K"  "jf"  "yb"  "yc"  "bhs" "cd"  "pk"  "pt"  "uy"  "iay" "fem" "mw"  "wr"  "ds"  "qmc" "dm"  "dx"  "mk"  "tg"  "tkc" "yf"  "gk"  "gfm"
# [162] "tgf" "tck" "tkb" "gt" 




# ++ HCU#############
HCU = SALI$HCU[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','CUTAN_NO','CUTAN_TYPE')]
HCU <- aggregate(OBS_NO~PROJECT_CODE+SITE_ID,HCU,min) %>%
  left_join(.,HCU,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
HCU <- aggregate(CUTAN_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,HCU,min) %>%
  left_join(.,HCU,by=c("PROJECT_CODE" ,"SITE_ID","OBS_NO",'HORIZON_NO',"CUTAN_NO"))
HCU[HCU == "-"] <- NA
unique(HCU$CUTAN_TYPE)
# "Z" "K" "C" "U" "O" "S" "M"

# ++ HST#################
HST = SALI$HST[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','STRUCT_NO','PEDALITY_GRADE','PEDALITY_TYPE')]
HST <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,HST,min) %>%
  left_join(.,HST,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
HST <- aggregate(STRUCT_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,HST,min) %>%
  left_join(.,HST,by=c("PROJECT_CODE" ,"SITE_ID","OBS_NO",'HORIZON_NO',"STRUCT_NO"))
HST[HST == "-"] <- NA
unique(HST$PEDALITY_GRADE)
# "S" "V" "W" "M" "G" NA 
unique(HST$PEDALITY_TYPE)
# "GR" NA   "SB" "AB" "BL" "PO" "PL" "CA" "FR" "PR" "LE" "CL" "CO"

# ++ HSG##################
HSG = SALI$HSG[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SEG_NO','NATURE')]
HSG <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,HSG,min) %>%
  left_join(.,HSG,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
HSG <- aggregate(SEG_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,HSG,min) %>%
  left_join(.,HSG,by=c("PROJECT_CODE" ,"SITE_ID","OBS_NO",'HORIZON_NO',"SEG_NO"))
HSG[HSG == "-"] <- NA
unique(HSG$NATURE)
# "F" "K" "M" NA  "N" "O" "U" "Y" "H" "G" "A" "Z" "E" "L" "S"

# ++ FTS###############
FTS <- SALI$FTS[SALI$FTS$TEST_TYPE == 'PH' , 
                c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','TEST_NO','VALUE')]
names(FTS)[6] <- "FTS_PH"
FTS$FTS_PH <-as.numeric(FTS$FTS_PH)
# use mean value if multiple values available for one horizon ?
FTS<-  aggregate(FTS_PH ~ PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,FTS,mean)
min(FTS$FTS_PH);max(FTS$FTS_PH)
#0.2; 11.2

# ++ LAB###############
# ESP
names(lab_15n1_extra)[4] <- "ESP_15N1" # Exchangeable sodium percentage
lab_15n1 = lab_15n1_extra[,c("PROJECT_CODE" , "SITE_ID","OBS_NO",'HORIZON_NO','LD','SAMPLE_NO','ESP_15N1')]
lab_15n1 <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,lab_15n1_extra,min) %>%
  left_join(.,lab_15n1_extra,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO")) 
lab_15n1 <- aggregate(LD~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_15n1,min) %>%
  left_join(.,lab_15n1[,c('PROJECT_CODE','SITE_ID',"OBS_NO",'HORIZON_NO','LD','SAMPLE_NO',
                          "ESP_15N1")])
lab_15n1<-  aggregate(SAMPLE_NO~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_15n1,min) %>%
  left_join(.,lab_15n1[,c('PROJECT_CODE','SITE_ID',"OBS_NO",'HORIZON_NO','SAMPLE_NO',"ESP_15N1")])
lab_15n1<-  aggregate(ESP_15N1~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_15n1,min) 
min(lab_15n1$ESP_15N1);max(lab_15n1$ESP_15N1)
# 0.1;100

#FE
lab_13c1_fe <- LAB[which(LAB$LAB_METH_CODE == '13C1_Fe' & !is.na(LAB$NUMERIC_VALUE)), 
                   c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_CODE','NUMERIC_VALUE')]
names(lab_13c1_fe)[7] <- "FE_13C1"
# use mean value if multiple values available for one horizon 
lab_13c1_fe<-  aggregate(FE_13C1~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_13c1_fe,mean)
min(lab_13c1_fe$FE_13C1);max(lab_13c1_fe$FE_13C1)
# 0;19.8

lab_4a1 <- LAB[which(LAB$LAB_METH_CODE == '4A1' & !is.na(LAB$NUMERIC_VALUE)), 
               c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_CODE','NUMERIC_VALUE')]
names(lab_4a1)[7] <- "PH_4A1"
# use mean value if multiple values available for one horizon 
lab_4a1<-  aggregate(PH_4A1~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_4a1,mean)
min(lab_4a1$PH_4A1);max(lab_4a1$PH_4A1)
# 0;11

lab_2z2_clay <- LAB[which(LAB$LAB_METH_CODE == '2Z2_Clay' & !is.na(LAB$NUMERIC_VALUE)), 
                    c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_CODE','NUMERIC_VALUE')]
names(lab_2z2_clay)[7] <- "Clay_2Z2"
# use mean value if multiple values available for one horizon 
lab_2z2_clay<-  aggregate(Clay_2Z2~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_2z2_clay,mean)
min(lab_2z2_clay$Clay_2Z2);max(lab_2z2_clay$Clay_2Z2)
#0;503

lab_6b_6a <- LAB[which(LAB$LAB_METH_CODE %in% c('6A1','6B2a','6B2b','6B2c','6B4') & !is.na(LAB$NUMERIC_VALUE)), 
                 c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','SAMPLE_NO','LAB_CODE','NUMERIC_VALUE')]
names(lab_6b_6a)[7] <- "CARBON_6B_6A"
# use mean value if multiple values available for one horizon 
lab_6b_6a<-  aggregate(CARBON_6B_6A~PROJECT_CODE+SITE_ID+OBS_NO+HORIZON_NO,lab_6b_6a,mean)
min(lab_6b_6a$CARBON_6B_6A);max(lab_6b_6a$CARBON_6B_6A)
#0;46

# + generate  datasets ############################################
ord <- 
  HOR_split[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO',
               'UPPER_DEPTH','LOWER_DEPTH','BOUND_DISTINCT','SOIL_WATER_STAT',
               'HOR_PREFIX','HOR_MASTER','HOR_SUBHOR','HOR_SUFFIX', 'TEXTURE_CODE')] %>%
  full_join(.,FTS,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  full_join(.,lab_13c1_fe,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  full_join(.,lab_15n1,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  full_join(.,lab_6b_6a,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  full_join(.,lab_4a1,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  full_join(.,lab_2z2_clay,by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  full_join(.,HCU[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','CUTAN_TYPE')],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  full_join(.,HST[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','PEDALITY_TYPE','PEDALITY_GRADE')],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  full_join(.,HSG[,c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO','NATURE')],
            by = c('PROJECT_CODE','SITE_ID','OBS_NO','HORIZON_NO')) %>%
  left_join(.,OBS[,c('PROJECT_CODE','SITE_ID','OBS_NO','DRAINAGE')],
            by = c("PROJECT_CODE", "SITE_ID", "OBS_NO"))  %>%
  left_join(.,OSC[,c('PROJECT_CODE','SITE_ID','OBS_NO','STATUS')],
            by = c("PROJECT_CODE", "SITE_ID", "OBS_NO")) %>%
  left_join(.,SIT[,c('PROJECT_CODE','SITE_ID','ELEM_TYPE_CODE')],
            by = c("PROJECT_CODE", "SITE_ID")) %>%
  left_join(.,OCL,by=c('PROJECT_CODE','SITE_ID'))
# only use min(OBS_NO) if there are multiple observations for unique site
ord <-  aggregate(OBS_NO~PROJECT_CODE+SITE_ID,ord,min) %>%
  left_join(.,ord,by=c("PROJECT_CODE" , "SITE_ID","OBS_NO"))
# "-" to NA, ASC_CONFIDENCE : NA -> 0
ord[ord=='-']=NA
ord$ASC_CONFIDENCE[is.na(ord$ASC_CONFIDENCE)]=0
# check "sn" attribute
ord$DRAINAGE = as.integer(ord$DRAINAGE)
# Exchange ordinal data
# exchange one ordinal attribute in a dataframe into numeric 
# based on the list in "l"
# input: x: single attribute of a dataframe, e.g.: A$a
#        l: list of unique attribute values and their replace number
#            example of list "l" : list(c('a','b','c'), c(1,2,3))
#        na: na: the value for NA, default as 0 ? or NA
# output: the numeric attribute
ex_ordinal <- function(x,l,na = NA) {
  if (length(l[[1]]) == length(l[[2]])) {
    x[is.na(x)] = na
    for (i in 1 : length(l[[1]])) {
      x[x==l[[1]][i]] = l[[2]][i]
    }
    return(as.integer(x))
  } else { print('Error, the length of the list not matched.')}
}
ord$SOIL_WATER_STAT = ex_ordinal(ord$SOIL_WATER_STAT,list(c('D','T','M','W'),1:4))
ord$BOUND_DISTINCT = ex_ordinal(ord$BOUND_DISTINCT,list(c('S','A','C','G','D'),1:5))
ord$PEDALITY_GRADE = ex_ordinal(ord$PEDALITY_GRADE,list(c('G','V','W','M','S'),1:5))

# idx = c('PRODUCT_CODE','SITE_ID','OBS_NO','HORIZON_NO')
# sn = c('DRAINAGE')
# sc = c('ELEM_TYPE_CODE','STATUS')
# vn = c('UPPER_DEPTH','LOWER_DEPTH','SOIL_WATER_STAT','FTS_PH','BOUND_DISTINCT','FE_13C1',
#        'ESP_15N1','CARBON_6B_6A','PH_4A1','Clay_2Z2','PEDALITY_GRADE','HOR_PREFIX')
# vc = c('PEDALITY_TYPE','NATURE','CUTAN_TYPE','TEXTURE_CODE','HOR_MASTER','HOR_SUBHOR','HOR_SUFFIX')
# tag = c('ASC_ORD')
# normalization <- function(x,incl,e=0.01) {
#   # Normalization x by columns in c, use e as adjusted eps
#   # formula: y = e+(1-2e)*(x-min(x))/(max(x)-min(x))
#   for (i in names(x)) {
#     if ( i %in% incl){
#       x[[i]] = e+(1-2*e)*(x[[i]]-min(x[[i]][which(!is.na(x[[i]]))]))/
#         (max(x[[i]][which(!is.na(x[[i]]))])-min(x[[i]][which(!is.na(x[[i]]))]))
#     }
#   }
#   return(x)
# }
# # normalization on sn,vn
# ord = normalization(ord,c(sn,vn))


# ord_tr <- right_join(ord,OCL_tr[,c('PROJECT_CODE','SITE_ID','ASC_ORD')])
# ord_ts <- right_join(ord,OCL_ts[,c('PROJECT_CODE','SITE_ID','ASC_ORD')])
# ord_nd <- ord %>% 
#   anti_join(.,ord_tr,by = c('PROJECT_CODE','SITE_ID')) %>%
#   anti_join(.,ord_ts,by = c('PROJECT_CODE','SITE_ID'))

# + empty rate#############################
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
  rate$EMPTY_RATE = label_percent(accuracy =0.1)(rate$EMPTY_RATE)
  return(rate)
}
empty_rate(ord[,5:26])
# name EMPTY_RATE EMPTY_. NON_EMPTY_.   TOTAL
# 9          FE_13C1      99.9%  191040         132  191172
# 11    CARBON_6B_6A      96.9%  185250        5922  191172
# 13        Clay_2Z2      96.5%  184559        6613  191172
# 10        ESP_15N1      95.6%  182839        8333  191172
# 19      CUTAN_TYPE      89.7%  171542       19630  191172
# 14      HOR_PREFIX      88.9%  169970       21202  191172
# 12          PH_4A1      86.5%  165380       25792  191172
# 17      HOR_SUFFIX      84.0%  160582       30590  191172
# 22          NATURE      75.4%  144188       46984  191172
# 7  SOIL_WATER_STAT      67.0%  128154       63018  191172
# 20   PEDALITY_TYPE      48.4%   92525       98647  191172
# 16      HOR_SUBHOR      46.6%   89044      102128  191172
# 6   BOUND_DISTINCT      42.8%   81751      109421  191172
# 1         DRAINAGE      26.5%   50602      140570  191172
# 8           FTS_PH      20.8%   39754      151418  191172
# 3           STATUS      15.8%   30154      161018  191172
# 21  PEDALITY_GRADE      12.4%   23722      167450  191172
# 2   ELEM_TYPE_CODE      11.5%   22032      169140  191172
# 18    TEXTURE_CODE       5.0%    9499      181673  191172
# 15      HOR_MASTER       0.9%    1640      189532  191172
# 4      UPPER_DEPTH       0.3%     514      190658  191172
# 5      LOWER_DEPTH       0.3%     514      190658  191172
# 23         By_rows     100.0%  191172           0  191172
# 24           Total      50.5% 2125255     2080529 4205784

# remove TE from the dataset
# ord_tr = filter(ord_tr,ASC_ORD !='TE')



# + Write datasets to files#############################
write_rds(ord,"./00_ord/ord_not_normalize.rds")
write.csv(ord,file ="./00_ord/ord.csv",row.names = FALSE)
