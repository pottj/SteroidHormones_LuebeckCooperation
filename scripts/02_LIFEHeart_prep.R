#' ---
#' title: "LIFE-Heart - data preparation"
#' subtitle: "Luebeck Cooperation"
#' author: "Janne Pott"
#' date: "Last compiled on `r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   html_document:
#'     toc: true
#'     number_sections: true
#'     toc_float: true
#'     code_folding: show
#' ---
#'
#' # Introduction ####
#' ***
#' In this script, I want to load all the phenotype data I have for LIFE-Heart and bring it to the format I can use throughout the project. 
#' 
#' # Initialize ####
#' ***
rm(list = ls())
time0<-Sys.time()
server = "angmar"

source("../SourceFile.R")
.libPaths()

#' # Load derivatives data ####
#' ***
data = list.files(path = path_LIFEHeart, pattern = ".xlsx")

#' Okay, there are 9 excel files. I start with D00242, because all samples must have sex and age. 
#' 
D00242 = data.table(read_excel(paste0(path_LIFEHeart,data[grepl("D00242",data)])))
names(D00242)

myTab = copy(D00242)
names(myTab) = gsub("HEART_PROB_","",names(myTab))
myTab[,dumID := paste(SIC,GRUPPE,sep="_")]
table(duplicated(myTab$SIC))
table(duplicated(myTab$dumID))

setnames(myTab,"GENDER","SEX")

#' Now I can add the aliquot ID from the blood sample
T00991 = data.table(read_excel(paste0(path_LIFEHeart,data[grepl("T00991",data)])))
names(T00991)
T00991[,dumID := paste(SIC,GRUPPE,sep="_")]
table(duplicated(T00991$SIC))
table(duplicated(T00991$dumID))
matched = match(myTab$dumID,T00991$dumID)
table(is.na(matched))
T00991 = T00991[matched,]
table(T00991$dumID == myTab$dumID)
table(T00991$HEART_BE_EDAT == myTab$EDAT)
myTab[,ALIQUOT := T00991$HEART_BE_NR]
myTab[,timeBloodDraw := T00991[,HEART_BE_ZEIT]]
myTab[,timeBloodDraw := gsub(":.*","",timeBloodDraw)]
myTab[,timeBloodDraw := as.numeric(timeBloodDraw)]
myTab[,timeFasting := T00991[,HEART_BE_KARENZZEIT]]

#' Okay, now I add a flag for genetic data available
#' 
T00959 = data.table(read_excel(paste0(path_LIFEHeart,data[grepl("T00959",data)])))
load(path_LIFEHeart_GeneticPCs)
myTab[,genetics := F]
myTab[SIC %in% T00959$SIC, genetics := T]
myTab[,table(genetics)]
matched = match(myTab$SIC,T00959$SIC)
table(is.na(matched))
myTab[,ALIQUOT_genetics := T00959[matched,B3_SNP_SAMPLINGID]]
matched = match(myTab$ALIQUOT_genetics,SamplesAsImputedB3$Aliquot_Genetik)
table(is.na(matched))
myTab[,SEX_genetics := SamplesAsImputedB3[matched,"sex"]]
myTab = cbind(myTab,SamplesAsImputedB3[matched,12:21])

#' Okay, now I simply match all the other variables (non-blood by SIC, blood parameters by aliquot, must match G)
D00228 = data.table(read_excel(paste0(path_LIFEHeart,data[grepl("D00228",data)])))
D00228[,G03 := grepl("#G03",HEART_MEDA_H_ATC)]
D00228[,H02AB := grepl("#H02AB",HEART_MEDA_H_ATC)]
D00228[,table(G03,H02AB)]
matched = match(myTab$SIC,D00228$SIC)
D00228 = D00228[matched,]
myTab[,meds_G03 := D00228[,G03]]
myTab[,meds_H02AB := D00228[,H02AB]]

D00157 = data.table(read_excel(paste0(path_LIFEHeart,data[grepl("D00157",data)])))
matched = match(myTab$SIC,D00157$SIC)
D00157 = D00157[matched,]
myTab[,BMI := D00157[,BIOM_ANT_BMI]]

D00212 = data.table(read_excel(paste0(path_LIFEHeart,data[grepl("D00212",data)])))
matched = match(myTab$SIC,D00212$pseudonym)
D00212 = D00212[matched,]
myTab[,ageLastMenst := D00212[,nobcad_gender.d00212_fem15]]
myTab[,removedUterus := D00212[,nobcad_gender.d00212_fem05]]
myTab[,removedOvaries := D00212[,nobcad_gender.d00212_fem07]]
myTab[,med_antibaby := D00212[,nobcad_gender.d00212_fem09]]
myTab[,med_HRT := D00212[,nobcad_gender.d00212_fem11]]

#' # Menopausal status ####
#' ***
#' 
#' get years since last menstruation 
myTab[,yearsLastMenst := AGE - ageLastMenst]
hist(myTab$yearsLastMenst)
myTab[yearsLastMenst < 1, yearsLastMenst := NA]
hist(myTab$yearsLastMenst)

#' 
#' -	Post-menopausal definition: 
#'    - Days since last menstruation >365 & age >50
#'    - Days since last menstruation missing & age >60
#' - Remaining women
#'   

myTab[SEX==1,group := "men"]

myTab[SEX==2 & !is.na(yearsLastMenst) & yearsLastMenst>=1 & AGE>50,group := "postmenopausal"]
myTab[SEX==2 & is.na(yearsLastMenst) & AGE>60,group := "postmenopausal"]

myTab[SEX==2 & is.na(group) & AGE<60,group := "premenopausal"]

myTab[,table(group)]

#' # Load blood parameter
#' ***

data2 = data[grepl("T0117",data)]

#' **IMPORTANT**: During the LIFE-Heart hormone measurements, there was an update of the LC-MSMS device. Hence, batch 1-4 have different LODs. To make my live simpler, I just kick them all out. 
#' 
dumTab = foreach(i=1:length(data2))%do%{
  #i=1
  data3 = data.table(read_excel(paste0(path_LIFEHeart,data2[i])))
  if(grepl("DATUM",names(data3)[2])==T){
    parameter1 = gsub("_DATUM","",names(data3)[2])
  }else if(grepl("EDAT",names(data3)[2])==T){
    parameter1 = gsub("_EDAT","",names(data3)[2])
  }
  names(data3) = gsub(paste0(parameter1,"_"),"",names(data3))
  data3 = data3[BATCH>=5,]
  matched = match(myTab$ALIQUOT,data3$SAMPLING_ID)
  myTab[,value := data3[matched,NUM_VALUE]]
  myTab[,value := gsub(",",".",value)]
  myTab[,value := as.numeric(value)]
  setnames(myTab,"value",parameter1)
  
}
names(myTab)
myTab[,table(genetics,!is.na(CORT_LCMS))]
setnames(myTab,"TESTO_LSMS","TESTO_LCMS")
setnames(myTab,"ANDRO_LSMS","ANDRO_LCMS")
setnames(myTab,"OHP_LSMS","OHP17_LCMS")
setnames(myTab,"ESTR_LCMS","E2_LCMS")

save(myTab, file = paste0(path_LIFEprepped,"LuebeckCooperation/02_LIFEHeart_unfiltered.RData"))
load(paste0(path_LIFEprepped,"LuebeckCooperation/02_LIFEHeart_unfiltered.RData"))

#' # Sample exclusion criteria
#' ***
#' Check for samples missing all hormones
#' 
filt = is.na(myTab$CORT_LCMS) & is.na(myTab$DHEAS_LCMS) & is.na(myTab$TESTO_LCMS) & is.na(myTab$E2_LCMS) & is.na(myTab$OHP17_LCMS) & is.na(myTab$PROG_LCMS) & is.na(myTab$ANDRO_LCMS) & is.na(myTab$ALDO_LCMS) 
table(filt)
myTab = myTab[!filt,]

#' Check for covariables (age, group, BMI, time of blood sampling, genetics) 
#' 
myTab[,goodSample := T]
myTab[is.na(group), goodSample := F]
myTab[is.na(BMI), goodSample := F]
myTab[is.na(timeBloodDraw), goodSample := F]
myTab[timeBloodDraw %in% c(13), goodSample := F]
myTab[is.na(genetics), goodSample := F]
myTab[genetics == F, goodSample := F]
myTab[,table(goodSample,group)]
myTab = myTab[goodSample == T,]

#' Check for removal of uterus or ovaries (only relevant in pre-menopausal group)
#' 
myTab[group != "men",removedUO := 0]
myTab[group != "men" & (removedUterus==1 | removedOvaries==1),removedUO := 1]
myTab[group=="premenopausal" & (removedUterus==1 | removedOvaries==1), goodSample := F]
myTab[,table(goodSample,group)]
myTab = myTab[goodSample == T,]

#' Check for medication (G03, H02AB, antibaby pill, hormone replacement therapy HRT)
#' 
myTab[,meds := 0]
myTab[meds_G03==T | meds_H02AB==T | med_antibaby==1 | med_HRT==1,meds := 1]
myTab[,table(meds,group)]
myTab = myTab[meds == 0,]

#' Check for remaining duplicates
myTab[,table(duplicated(SIC))]

#' Save filtered data set
#' 
save(myTab, file = paste0(path_LIFEprepped,"LuebeckCooperation/02_LIFEHeart_filtered.RData"))

#' # Check hormone levels
#' ***
#' Check hormone levels for outlier. 
#' 
myTab[,QC_ok := T]
myTab[,reasonToX := ""]

#' ## E2 ####
plot5 = ggplot(myTab, aes(x=AGE, y=E2_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("E2 levels") 
plot5

#' ## TT ####
plot5 = ggplot(myTab, aes(x=AGE, y=TESTO_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("TT levels") 
plot5

myTab[group == "postmenopausal" & TESTO_LCMS>7,]
myTab[TESTO_LCMS>7 & group=="postmenopausal",QC_ok := F]
myTab[TESTO_LCMS>7 & group=="postmenopausal",reasonToX:="high TT values"]

plot5 = ggplot(myTab[QC_ok==T,], aes(x=AGE, y=TESTO_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("TT levels") 
plot5

#' ## CORT ####
plot5 = ggplot(myTab, aes(x=AGE, y=CORT_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("Cortisol levels") 
plot5

plot5 = ggplot(myTab, aes(x=as.factor(timeBloodDraw), y=CORT_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_boxplot() +
  theme_bw(base_size = 15) + 
  xlab("Time of blood collection") + ylab("Cortisol levels") 
plot5

#' ## A4 ####
plot5 = ggplot(myTab, aes(x=AGE, y=ANDRO_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("Androstenedione levels") 
plot5

#' ## 17-OHP ####
plot5 = ggplot(myTab, aes(x=AGE, y=OHP17_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("17-OHP levels") 
plot5

myTab[OHP17_LCMS>15 & QC_ok==F,reasonToX := paste(reasonToX,"high 17-OHP values",sep=", ")]
myTab[OHP17_LCMS>15 & QC_ok==T,reasonToX := "high 17-OHP values"]
myTab[OHP17_LCMS>15,QC_ok:=F]

plot5 = ggplot(myTab[QC_ok==T], aes(x=AGE, y=OHP17_LCMS)) +
  facet_wrap(~ as.factor(group),scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("17-OHP levels") 
plot5

#' ## ALDO ###
plot5 = ggplot(myTab, aes(x=AGE, y=ALDO_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("Aldosterone levels") 
plot5

myTab[ALDO_LCMS>1200,]
myTab[ALDO_LCMS>1200 ,QC_ok:=F]
myTab[ALDO_LCMS>1200 ,reasonToX := "high ALDO values"]

plot5 = ggplot(myTab[QC_ok==T], aes(x=AGE, y=ALDO_LCMS)) +
  facet_wrap(~ as.factor(group),scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("ALDO levels") 
plot5

#' ## DHEAS ###
plot5 = ggplot(myTab, aes(x=AGE, y=DHEAS_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("DHEA-S levels") 
plot5

#' ## P4 ###
plot5 = ggplot(myTab, aes(x=AGE, y=PROG_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("Progesterone levels") 
plot5

myTab[group == "men" & PROG_LCMS>4,QC_ok := F]
myTab[group == "men" & PROG_LCMS>4,reasonToX := "extreme P4 value"]

plot5 = ggplot(myTab[QC_ok==T], aes(x=AGE, y=PROG_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("Progesterone levels") 
plot5

#' # Summary ####
#' ***
myTab[, table(reasonToX,group)]
myTab = myTab[QC_ok==T,]

myHormones = names(myTab)[c(34:41,6,26)]

dumTab = foreach(i = 1:length(myHormones))%do%{
  #i=1
  message("Sample sizes for ", myHormones[i])
  #print(myTab[,table(!is.na(get(myHormones[i])),group)])
  x1 = myTab[!is.na(get(myHormones[i])),mean(get(myHormones[i])),group]
  x2 = myTab[!is.na(get(myHormones[i])),sd(get(myHormones[i])),group]
  x3 = myTab[!is.na(get(myHormones[i])),.N,group]
  
  res = data.table(group = x1$group,
                   mean = round(x1$V1,3),
                   sd = round(x2$V1,3),
                   n = x3$N)
  res$hormone = myHormones[i]
  res
}
dumTab2 = rbindlist(dumTab)
dumTab3 = dcast(dumTab2,hormone ~ group, value.var = c("mean","sd","n"))
dumTab3 = dumTab3[,c(1,8,2,5,9,3,6,10,4,7)]
dumTab3

#' # Save data ####
#' ***
save(myTab, file = paste0(path_LIFEprepped,"LuebeckCooperation/02_LIFEHeart_QC.RData"))

fwrite(dumTab3,file = "../results/02_descriptiveTable_hormones.txt",col.names = T,row.names = F, quote=F,sep="\t",dec=".")
save(dumTab3,file = "../results/02_descriptiveTable_hormones.RData")

#' # Session Info ####
#' ***
sessionInfo()
message("\nTOTAL TIME : " ,round(difftime(Sys.time(),time0,units = "mins"),3)," minutes")

