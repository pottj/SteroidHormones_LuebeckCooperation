#' ---
#' title: "LIFE-Adult - data preparation"
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
#' In this script, I want to load all the phenotype data I have for LIFE-Adult and bring it to the format I can use throughout the project. 
#' 
#' According to the analysis plan: 
#' 
#' - all 8 steroid hormones and SHBG, LH and FSH
#' - sex, age, BMI, time of blood collection, smoking status, medication with steroids, genetic principal components 
#' - female-specific: menopausal status, days since last menstruation, surgical removal of uterus or ovaries 
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
data = list.files(path = path_LIFEAdult, pattern = ".xlsx")

#' Okay, there are 25 excel files. I start with D00153, because all samples must have sex and age. 
#' 
D00153 = data.table(read_excel(paste0(path_LIFEAdult,data[grepl("D00153",data)])))
names(D00153)

myTab = copy(D00153)
names(myTab) = gsub("ADULT_PROB_","",names(myTab))
myTab[,dumID := paste(SIC,GRUPPE,sep="_")]
table(duplicated(myTab$SIC))
table(duplicated(myTab$dumID))

setnames(myTab,"GENDER","SEX")

#' Now I can add the aliquot ID from the blood sample
D00126 = data.table(read_excel(paste0(path_LIFEAdult,data[grepl("D00126",data)])))
names(D00126)
D00126[,dumID := paste(SIC,GRUPPE,sep="_")]
table(duplicated(D00126$SIC))
table(duplicated(D00126$dumID))
matched = match(myTab$dumID,D00126$dumID)
table(is.na(matched))
D00126 = D00126[matched,]
table(D00126$dumID == myTab$dumID)
table(D00126$ENTNAHME_EDAT == myTab$EDAT)
myTab[,ALIQUOT := D00126$ENTNAHME_SAMPLING_ID]
myTab[,timeBloodDraw := D00126[,ENTNAHME_ZP0_START]]
myTab[,timeBloodDraw := gsub(":.*","",timeBloodDraw)]
myTab[,timeBloodDraw := as.numeric(timeBloodDraw)]
myTab[,timeFasting := D00126[,ENTNAHME_FASTED_HOURS]]

#' Okay, now I add a flag for genetic data available
#' 
D00364 = data.table(read_excel(paste0(path_LIFEAdult,data[grepl("D00364",data)])))
load(path_LIFEAdult_GeneticPCs)
myTab[,genetics := F]
myTab[SIC %in% D00364$SIC, genetics := T]
myTab[,table(genetics)]
matched = match(myTab$SIC,D00364$SIC)
table(is.na(matched))
myTab[,ALIQUOT_genetics := D00364[matched,ADULT_SNP_SAMPLING_ID]]
matched = match(myTab$ALIQUOT_genetics,SamplesAsImputed$Aliquot)
table(is.na(matched))
myTab[,SEX_genetics := SamplesAsImputed[matched,sex]]
myTab = cbind(myTab,SamplesAsImputed[matched,8:17])

#' Okay, now I simply match all the other variables (non-blood by SIC, blood parameters by aliquot)
D00038 = data.table(read_excel(paste0(path_LIFEAdult,data[grepl("D00038",data)])))
D00038[,G03 := grepl("#G03",ADULT_MEDA_H_ATC)]
D00038[,H02AB := grepl("#H02AB",ADULT_MEDA_H_ATC)]
D00038[,table(G03,H02AB)]
matched = match(myTab$SIC,D00038$SIC)
D00038 = D00038[matched,]
myTab[,meds_G03 := D00038[,G03]]
myTab[,meds_H02AB := D00038[,H02AB]]

D00074 = data.table(read_excel(paste0(path_LIFEAdult,data[grepl("D00074",data)])))
matched = match(myTab$SIC,D00074$SIC)
D00074 = D00074[matched,]
myTab[,BMI := D00074[,BMI_BMI]]

D00133 = data.table(read_excel(paste0(path_LIFEAdult,data[grepl("D00133",data)])))
D00133[ADULT_GENDER_GEN01==2,gender := 1]
D00133[ADULT_GENDER_GEN01==1,gender := 2]
matched = match(myTab$SIC,D00133$SIC)
D00133 = D00133[matched,]
myTab[,SEX_FSQ := D00133[,gender]]
myTab[,daysLastMenst := D00133[,ADULT_GENDER_FEM02]]
myTab[,monthsLastMenst := D00133[,ADULT_GENDER_FEM03]]
myTab[,yearsLastMenst := D00133[,ADULT_GENDER_FEM04]]
myTab[,removedUterus := D00133[,ADULT_GENDER_FEM11]]
myTab[,removedOvaries := D00133[,ADULT_GENDER_FEM13]]
myTab[,med_antibaby := D00133[,ADULT_GENDER_FEM17]]
myTab[,med_HRT := D00133[,ADULT_GENDER_FEM20]]

#' # Menopausal status ####
#' ***
#' 
#' I want to get the time since last menstruation: 
#' 
#' - days last menstruation should be between 0-31, -1 is set to NA
#' - years last menstruation should be between 1-x, -1 and 0 is set to NA
#' 
#' -	Post-menopausal definition: 
#'    - Days since last menstruation >365 & age >50
#'    - Days since last menstruation missing & age >60
#' -	Pre-menopausal definition: 
#'    - Days since last menstruation within 0-31 & age <60
#'    

myTab[SEX==1,group := "men"]

myTab[,table(daysLastMenst)]
myTab[daysLastMenst == -1, daysLastMenst := NA]
myTab[,table(yearsLastMenst)]
myTab[yearsLastMenst == -1, yearsLastMenst := NA]

myTab[SEX==2 & !is.na(yearsLastMenst) & yearsLastMenst>=1 & AGE>50,group := "postmenopausal"]
myTab[SEX==2 & is.na(yearsLastMenst) & AGE>60,group := "postmenopausal"]

myTab[SEX==2 & !is.na(daysLastMenst) & is.na(group) & AGE<50,group := "premenopausal"]

#' # Load blood parameter
#' ***
#' 
data2 = data[grepl("T",data)]
data2 = data2[c(2,3,4,5,6,7,8,11,15,16,18)]

for(i in 1:length(data2)){
  #i=1
  data3 = data.table(read_excel(paste0(path_LIFEAdult,data2[i])))
  if(grepl("DATUM",names(data3)[2])==T){
    parameter1 = gsub("_DATUM","",names(data3)[2])
  }else if(grepl("EDAT",names(data3)[2])==T){
    parameter1 = gsub("_EDAT","",names(data3)[2])
  }
  names(data3) = gsub(paste0(parameter1,"_"),"",names(data3))
  matched = match(myTab$ALIQUOT,data3$SAMPLING_ID)
  myTab[,value := data3[matched,NUM_VALUE]]
  setnames(myTab,"value",parameter1)
}

myTab[,table(!is.na(ALIQUOT_genetics),!is.na(CORT_S))]
setnames(myTab,"ES_V1","E2_S")
setnames(myTab,"PROG","PROG_LCMS")
setnames(myTab,"X17OHP_LCMS","OHP17_LCMS")

save(myTab, file = paste0(path_LIFEprepped,"LuebeckCooperation/01_LIFEAdult_unfiltered.RData"))
load(paste0(path_LIFEprepped,"LuebeckCooperation/01_LIFEAdult_unfiltered.RData"))

#' # Sample exclusion criteria
#' ***
#' Check for samples missing all hormones
#' 
filt = is.na(myTab$CORT_S) & is.na(myTab$DHEAS_S) & is.na(myTab$TESTO_S) & is.na(myTab$E2_S) & is.na(myTab$OHP17_LCMS) & is.na(myTab$PROG_LCMS) & is.na(myTab$ANDRO_LCMS) & is.na(myTab$ALDO_LCMS) & is.na(myTab$SHBG_S) & is.na(myTab$FSH_S) & is.na(myTab$LH_S)
table(filt)
myTab = myTab[!filt,]

#' Check for covariables (age, group, BMI, time of blood sampling, genetics) 
#' 
myTab[,goodSample := T]
myTab[is.na(group), goodSample := F]
myTab[is.na(BMI), goodSample := F]
myTab[is.na(timeBloodDraw), goodSample := F]
myTab[timeBloodDraw %in% c(0,11,12), goodSample := F]
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
duplicateSICs = myTab[duplicated(SIC),SIC]
myTab_dups = myTab[SIC %in% duplicateSICs, ]
myTab_dups

#' Okay, I will remove the ones in GRUPPE "A1_HAUPT01_EXCEPT01"
#' 
myTab = myTab[!is.element(dumID,c(myTab_dups[GRUPPE=="A1_HAUPT01_EXCEPT01",dumID]))]

#' Save filtered data set
#' 
save(myTab, file = paste0(path_LIFEprepped,"LuebeckCooperation/01_LIFEAdult_filtered.RData"))

#' # Check hormone levels
#' ***
#' Check hormone levels for outlier. 
#' 

myTab[,QC_ok := T]
myTab[,reasonToX := ""]

#' ## E2 ####
plot5 = ggplot(myTab, aes(x=AGE, y=E2_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("E2 levels") 
plot5

#' I will only remove the men with high values (unplausible) 
myTab[group == "men" & E2_S>1500,QC_ok := F]
myTab[group == "men" & E2_S>1500,reasonToX := "extreme E2 value"]

plot5 = ggplot(myTab[QC_ok==T], aes(x=AGE, y=E2_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("E2 levels") 
plot5

#' ## P4 ###
plot5 = ggplot(myTab, aes(x=AGE, y=PROG_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("Progesterone levels") 
plot5

#' I will remove the man amd the four post-menopausal women with high values (unplausible) 
myTab[group == "men" & PROG_LCMS>4,QC_ok := F]
myTab[group == "men" & PROG_LCMS>4,reasonToX := "extreme P4 value"]

myTab[group == "postmenopausal" & PROG_LCMS>10 & QC_ok==F ,reasonToX := paste(reasonToX,"extreme P4 value",sep=", ")]
myTab[group == "postmenopausal" & PROG_LCMS>10 & QC_ok==T,reasonToX := "extreme P4 value"]
myTab[group == "postmenopausal" & PROG_LCMS>10,QC_ok := F]

plot5 = ggplot(myTab[QC_ok==T], aes(x=AGE, y=PROG_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("Progesterone levels") 
plot5

#' ## TT ####
plot5 = ggplot(myTab, aes(x=AGE, y=TESTO_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("TT levels") 
plot5

#' I will remove the two men with high values (unplausible, value > mean + 6SD)
myTab[group == "men" & TESTO_S>60,]
myTab[group == "men" & TESTO_S>60,QC_ok := F]
myTab[group == "men" & TESTO_S>60,reasonToX := "extreme TT value"]

plot5 = ggplot(myTab[QC_ok==T,], aes(x=AGE, y=TESTO_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("TT levels") 
plot5

#' ## CORT ####
plot5 = ggplot(myTab, aes(x=AGE, y=CORT_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("Cortisol levels") 
plot5

plot5 = ggplot(myTab, aes(x=as.factor(timeBloodDraw), y=CORT_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_boxplot() +
  theme_bw(base_size = 15) + 
  xlab("Time of blood collection") + ylab("Cortisol levels") 
plot5

#' ## Androstenedione ####
plot5 = ggplot(myTab, aes(x=AGE, y=ANDRO_LCMS)) +
  facet_wrap(~ as.factor(group),scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("Androstenedione levels") 
plot5

#' I will remove the one women with high values (unplausible, value > mean + 6SD)
myTab[ANDRO_LCMS>10 & SEX == 2,]
myTab[ANDRO_LCMS>10 & SEX == 2,QC_ok:=F]
myTab[ANDRO_LCMS>10 & SEX == 2,reasonToX := "high ANDRO values"]

plot5 = ggplot(myTab[QC_ok==T], aes(x=AGE, y=ANDRO_LCMS)) +
  facet_wrap(~ as.factor(SEX),scales = "free") +
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

myTab[OHP17_LCMS>20 & QC_ok==F,reasonToX := paste(reasonToX,"high 17-OHP values",sep=", ")]
myTab[OHP17_LCMS>20 & QC_ok==T,reasonToX := "high 17-OHP values"]
myTab[OHP17_LCMS>20,QC_ok:=F]

plot5 = ggplot(myTab[QC_ok==T], aes(x=AGE, y=OHP17_LCMS)) +
  facet_wrap(~ as.factor(SEX),scales = "free") +
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
  facet_wrap(~ as.factor(SEX),scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("ALDO levels") 
plot5

#' ## DHEAS ###
plot5 = ggplot(myTab, aes(x=AGE, y=DHEAS_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("DHEA-S levels") 
plot5

#' ## SHBG ###
plot5 = ggplot(myTab, aes(x=AGE, y=SHBG_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("SHBG levels") 
plot5

#' ## FSH ###
plot5 = ggplot(myTab, aes(x=AGE, y=FSH_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("FSH levels") 
plot5

#' ## LH ###
plot5 = ggplot(myTab, aes(x=AGE, y=LH_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("age") + ylab("LH levels") 
plot5

#' # Reproductive hormones per cycle day
#' ***
#' 
plot5 = ggplot(myTab[group=="premenopausal"], aes(x=daysLastMenst, y=LH_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("days since last menstruation") + ylab("LH levels") 
plot5

plot5 = ggplot(myTab[group=="premenopausal"], aes(x=daysLastMenst, y=FSH_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("days since last menstruation") + ylab("FS levels") 
plot5

plot5 = ggplot(myTab[group=="premenopausal"], aes(x=daysLastMenst, y=PROG_LCMS)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("days since last menstruation") + ylab("Progesterone levels") 
plot5

plot5 = ggplot(myTab[group=="premenopausal"], aes(x=daysLastMenst, y=E2_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("days since last menstruation") + ylab("Estradiol levels") 
plot5

plot5 = ggplot(myTab[group=="premenopausal"], aes(x=E2_S, y=LH_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("Estradiol levels") + ylab("LH levels") 
plot5

plot5 = ggplot(myTab[group=="premenopausal"], aes(x=FSH_S, y=LH_S)) +
  facet_wrap(~ group,scales = "free") +
  geom_point() +
  theme_bw(base_size = 15) + 
  xlab("FSH levels") + ylab("LH levels") 
plot5

#' # Summary ####
#' ***
myTab[, table(reasonToX,group)]
myTab = myTab[QC_ok==T,]

myHormones = names(myTab)[c(35:45,5,25)]

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
save(myTab, file = paste0(path_LIFEprepped,"LuebeckCooperation/01_LIFEAdult_QC.RData"))

fwrite(dumTab3,file = "../results/01_descriptiveTable_hormones.txt",col.names = T,row.names = F, quote=F,sep="\t",dec=".")
save(dumTab3,file = "../results/01_descriptiveTable_hormones.RData")

#' # Session Info ####
#' ***
sessionInfo()
message("\nTOTAL TIME : " ,round(difftime(Sys.time(),time0,units = "mins"),3)," minutes")

