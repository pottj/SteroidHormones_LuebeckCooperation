#' ---
#' title: "Boxplots of hormone levels by age"
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
#' 
#' # Initialize ####
#' ***
rm(list = ls())
time0<-Sys.time()
server = "angmar"

source("../SourceFile.R")
.libPaths()

#' # Load data ####
#' ***
#' 
load(file = paste0(path_LIFEprepped,"LuebeckCooperation/01_LIFEAdult_QC.RData"))
myTabA1 = copy(myTab)

load(file = paste0(path_LIFEprepped,"LuebeckCooperation/02_LIFEHeart_QC.RData"))
myTabB3 = copy(myTab)

myTabA1[,study := "LIFE-Adult"]
myTabB3[,study := "LIFE-Heart"]

names(myTabA1)
names(myTabB3)

myTabA1_2 = myTabA1[,c(1,2,51,7,11,34,4,5,25,8,13:22,35,39,41,37,42:45,38,36,40)]
myTabB3_2 = myTabB3[,c(1,2,47,8,12,33,5,6,26,9,14:23,41,39,34,37,38,36,35,40)]

names(myTabA1_2) = gsub("_S","",names(myTabA1_2))
names(myTabA1_2) = gsub("_LCMS","",names(myTabA1_2))
names(myTabB3_2) = gsub("_LCMS","",names(myTabB3_2))

myTab = rbind(myTabA1_2,myTabB3_2,fill=T)

#' define age groups
#' 
myTab[AGE<=40, age3 := "20-40"]
myTab[AGE<=45 & AGE>40, age3 := "40-45"]
myTab[AGE<=50 & AGE>45, age3 := "46-50"]
myTab[AGE<=55 & AGE>50, age3 := "51-55"]
myTab[AGE<=60 & AGE>55, age3 := "56-60"]
myTab[AGE<=65 & AGE>60, age3 := "61-65"]
myTab[AGE<=70 & AGE>65, age3 := "66-70"]
myTab[AGE<=75 & AGE>70, age3 := "71-75"]
myTab[ AGE>75, age3 := "76-80"]
myTab[,table(age3,study)]
myTab[,table(age3,group)]

#' log-transformation 
#' 
myTab[,ALDO := log(ALDO)]
myTab[ALDO>4.418841,ALDO_sens := ALDO]
myTab[,ANDRO := log(ANDRO)]
myTab[,CORT := log(CORT)]
myTab[,DHEAS := log(DHEAS)]
myTab[,E2 := log(E2)]
myTab[E2>2.912352,E2_sens := E2]
myTab[,FSH := log(FSH)]
myTab[,LH := log(LH)]
myTab[,OHP17 := log(OHP17)]
myTab[,PROG := log(PROG)]
myTab[,SHBG := log(SHBG)]
myTab[,TESTO := log(TESTO)]
myTab[TESTO>-2.441847,TESTO_sens := TESTO]

#' define relevant hormones
#' 
myTraits = names(myTab)[c(21:31,33:35)]
myTraits2 = c("Cortisol","DHEA-S","Testosterone","Estradiol","Progesterone","17-OHP","Androstenedione","Aldosterone","SHBG","FSH","LH","Aldosterone - sensitivity","Estradiol - sensitivity","Testosterone - sensitivity")

for(i in 1:length(myTraits)){
  #i=1
  myTab[,trait := get(myTraits[i])]
  #myTab[,group2 := paste(study, group, sep=" - ")]
  
  plot1 = ggplot(myTab, aes(x=age3, y= trait, fill = group)) +
    facet_wrap(~ study,nrow = 2,scales = "free") +
    geom_boxplot(position=position_dodge(1)) +
    scale_fill_manual(values = c("#5B9BD5","#70AD47","#E94D36")) +
    labs(x="Age (grouped)",
         y=myTraits2[i], 
         fill="Group") +
    theme_bw(base_size = 15) +
    theme(legend.position = "none")
  
  print(plot1)
  
  filename = paste0("../results/05_Boxplots/",myTraits[i],".png")
  png(filename = filename,width = 2500, height = 1500, res=200)
  plot(plot1)
  dev.off()
  
}


#' # Session Info ####
#' ***
sessionInfo()
message("\nTOTAL TIME : " ,round(difftime(Sys.time(),time0,units = "mins"),3)," minutes")

