#' ---
#' title: "GWAS in LIFE"
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
#' Here, I want to run my GWAS analyses. 
#' 
#' First, I create per study and subgroup a phenotype file with all hormone traits. I will check the distribution, and transform the data if necessary. 
#' 
#' Second, I create one covariate file per study (will always use the same file). 
#' 
#' Third, I will generate the plink calls using the minimal filtered pgen files (TOPMed imputed data)
#' 
#'  
#' # Initialize ####
#' ***
rm(list = ls())
time0<-Sys.time()
server = "angmar"

source("../SourceFile.R")
.libPaths()

#' # Check hormonal distribution ####
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

#' I want a data table with 
#' 
#' - SIC, EDAT, ALIQUOT, group, SEX, age, BMI, timeBloodDraw, genetic PCs, 
#' - hormones
#' 
myTabA1_2 = myTabA1[,c(1,2,51,7,11,34,4,5,25,8,13:22,35,39,41,37,42:45,38,36,40)]
myTabB3_2 = myTabB3[,c(1,2,47,8,12,33,5,6,26,9,14:23,41,39,34,37,38,36,35,40)]

names(myTabA1_2) = gsub("_S","",names(myTabA1_2))
names(myTabA1_2) = gsub("_LCMS","",names(myTabA1_2))
names(myTabB3_2) = gsub("_LCMS","",names(myTabB3_2))

myTab = rbind(myTabA1_2,myTabB3_2,fill=T)

#' Now plot distribution per study and subgroup for BMI and all hormones
#' 
myTraits = names(myTab)[c(9,21:31)]
myTraits2 = c("BMI","Cortisol","DHEA-S","Testosterone","Estradiol","Progesterone","17-OHP","Androstenedione","Aldosterone","SHBG","FSH","LH")

for(i in 1:length(myTraits)){
  #i=1
  myTab[,trait := get(myTraits[i])]
  myTab[,group2 := paste(study, group, sep=" - ")]
  
  plot1 = ggplot(myTab, aes(x=trait,fill = group)) +
    facet_wrap(~ group2,scales = "free") +
    geom_histogram(col="grey") +
    theme_bw(base_size = 15) + 
    xlab(myTraits2[i]) + 
    scale_fill_manual(values = c("#5B9BD5","#70AD47","#E94D36")) +
    theme(legend.position = "none") 
  #print(plot1)
  
  filename = paste0("../results/03_Histograms/",myTraits[i],"_raw.png")
  png(filename = filename,width = 2500, height = 1500, res=200)
  plot(plot1)
  dev.off()
  
  plot2 = ggplot(myTab, aes(x=log(trait),fill = group)) +
    facet_wrap(~ group2,scales = "free") +
    geom_histogram(col="grey") +
    theme_bw(base_size = 15) + 
    xlab(paste0("log(",myTraits2[i],")")) + 
    scale_fill_manual(values = c("#5B9BD5","#70AD47","#E94D36")) +
    theme(legend.position = "none") 
  #print(plot2)
  
  filename = paste0("../results/03_Histograms/",myTraits[i],"_log.png")
  png(filename = filename,width = 2500, height = 1500, res=200)
  plot(plot2)
  dev.off()
  
}

#' Visual inspection of the histograms:
#' 
#' - Aldosterone: log & run sensitivity without samples below LOD
#' - Androstenedione: log
#' - BMI: log
#' - Cortisol: log
#' - DHEA-S: log
#' - Estradiol: log & run sensitivity without samples below LOD
#' - FSH: log for men and premenopausal women, raw for postmenopausal women --> keep it harmonized
#' - LH: log for men and premenopausal women, raw for postmenopausal women --> keep it harmonized
#' - 17-OHP: log
#' - Progesterone: log
#' - SHBG: log
#' - Testosterone: log in women, raw for men & run sensitivity without samples below LOD --> keep it harmonized
#' 
myTab[,ALDO := log(ALDO)]
myTab[ALDO>4.418841,ALDO_sens := ALDO]
myTab[,ANDRO := log(ANDRO)]
myTab[,BMI := log(BMI)]
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

myTraits = names(myTab)[c(34:36)]
myTraits2 = c("Aldosterone - sensitivity","Estradiol - sensitivity","Testosterone - sensitivity")

for(i in 1:length(myTraits)){
  #i=15
  myTab[,trait := get(myTraits[i])]
  myTab[,group2 := paste(study, group, sep=" - ")]
  
  plot3 = ggplot(myTab, aes(x=trait,fill = group)) +
    facet_wrap(~ group2,scales = "free") +
    geom_histogram(col="grey") +
    theme_bw(base_size = 15) + 
    xlab(myTraits2[i]) + 
    scale_fill_manual(values = c("#5B9BD5","#70AD47","#E94D36")) +
    theme(legend.position = "none") 
  #print(plot3)
  
  filename = paste0("../results/03_Histograms/",myTraits[i],"_sens.png")
  png(filename = filename,width = 2500, height = 1500, res=200)
  plot(plot3)
  dev.off()

}

#' # Create PLINK input files
#' ***
myTab[,IID := ALIQUOT_genetics]
myTab[,FID := ALIQUOT_genetics]

#' ## Phenotype files
#' 
#' One per study and subgroup
#' 
write.table(myTab[study=="LIFE-Adult" & group=="men",c(38,37,21:31,34:36)],
            file=paste0(path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEAdult_men.txt"),
            sep=" ",col.names = T,row.names = F,quote=F)

write.table(myTab[study=="LIFE-Adult" & group=="postmenopausal",c(38,37,21:31,34:36)],
            file=paste0(path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEAdult_postmenopausal.txt"),
            sep=" ",col.names = T,row.names = F,quote=F)

write.table(myTab[study=="LIFE-Adult" & group=="premenopausal",c(38,37,21:25,29:31,35:36)],
            file=paste0(path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEAdult_premenopausal.txt"),
            sep=" ",col.names = T,row.names = F,quote=F)

write.table(myTab[study=="LIFE-Heart" & group=="men",c(38,37,21:28,34:36)],
            file=paste0(path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEHeart_men.txt"),
            sep=" ",col.names = T,row.names = F,quote=F)

write.table(myTab[study=="LIFE-Heart" & group=="postmenopausal",c(38,37,21:28,34:36)],
            file=paste0(path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEHeart_postmenopausal.txt"),
            sep=" ",col.names = T,row.names = F,quote=F)

write.table(myTab[study=="LIFE-Heart" & group=="premenopausal",c(38,37,21:28,34:36)],
            file=paste0(path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEHeart_premenopausal.txt"),
            sep=" ",col.names = T,row.names = F,quote=F)

#' ## Covariate files
#' 
#' age, BMI, time of blood sampling, genetic PCs
#' 
write.table(myTab[study=="LIFE-Adult",c(38,37,8:20)],
            file=paste0(path_LIFEprepped,"LuebeckCooperation/CovarFile_LIFEAdult.txt"),
            sep=" ",col.names = T,row.names = F,quote=F)

write.table(myTab[study=="LIFE-Heart",c(38,37,8:20)],
            file=paste0(path_LIFEprepped,"LuebeckCooperation/CovarFile_LIFEHeart.txt"),
            sep=" ",col.names = T,row.names = F,quote=F)


#' # PLINK call ####
#' ***
#' 
mycall1 = paste0(path_plink2,
                 " --pfile ",path_LIFEAdult_Genetics,
                 " --glm hide-covar firth-fallback",
                 " cols=chrom,pos,ref,alt,firth,test,nobs,machr2,a1freq,a1freqcc,a1countcc,orbeta,se,ci,tz,p",
                 " --pheno ",path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEAdult_men.txt",
                 " --covar ",path_LIFEprepped,"LuebeckCooperation/CovarFile_LIFEAdult.txt",
                 " --covar-variance-standardize",
                 " --threads 20",
                 " --mach-r2-filter 0.8 2 --maf 0.01",
                 " --out ../results/03_PLINK_GLM/LIFEAdult_men")
mycall1
system(mycall1)

mycall2 = paste0(path_plink2,
                 " --pfile ",path_LIFEAdult_Genetics,
                 " --glm hide-covar firth-fallback",
                 " cols=chrom,pos,ref,alt,firth,test,nobs,machr2,a1freq,a1freqcc,a1countcc,orbeta,se,ci,tz,p",
                 " --pheno ",path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEAdult_postmenopausal.txt",
                 " --covar ",path_LIFEprepped,"LuebeckCooperation/CovarFile_LIFEAdult.txt",
                 " --covar-variance-standardize",
                 " --threads 20",
                 " --mach-r2-filter 0.8 2 --maf 0.01",
                 " --out ../results/03_PLINK_GLM/LIFEAdult_postmenopausal")
mycall2
system(mycall2)

mycall3 = paste0(path_plink2,
                 " --pfile ",path_LIFEAdult_Genetics,
                 " --glm hide-covar firth-fallback",
                 " cols=chrom,pos,ref,alt,firth,test,nobs,machr2,a1freq,a1freqcc,a1countcc,orbeta,se,ci,tz,p",
                 " --pheno ",path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEAdult_premenopausal.txt",
                 " --covar ",path_LIFEprepped,"LuebeckCooperation/CovarFile_LIFEAdult.txt",
                 " --covar-variance-standardize",
                 " --threads 20",
                 " --mach-r2-filter 0.8 2 --maf 0.01",
                 " --out ../results/03_PLINK_GLM/LIFEAdult_premenopausal")
mycall3
system(mycall3)

mycall4 = paste0(path_plink2,
                 " --pfile ",path_LIFEHeart_Genetics,
                 " --glm hide-covar firth-fallback",
                 " cols=chrom,pos,ref,alt,firth,test,nobs,machr2,a1freq,a1freqcc,a1countcc,orbeta,se,ci,tz,p",
                 " --pheno ",path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEHeart_men.txt",
                 " --covar ",path_LIFEprepped,"LuebeckCooperation/CovarFile_LIFEHeart.txt",
                 " --covar-variance-standardize",
                 " --threads 20",
                 " --mach-r2-filter 0.8 2 --maf 0.01",
                 " --out ../results/03_PLINK_GLM/LIFEHeart_men")
mycall4
system(mycall4)

mycall5 = paste0(path_plink2,
                 " --pfile ",path_LIFEHeart_Genetics,
                 " --glm hide-covar firth-fallback",
                 " cols=chrom,pos,ref,alt,firth,test,nobs,machr2,a1freq,a1freqcc,a1countcc,orbeta,se,ci,tz,p",
                 " --pheno ",path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEHeart_postmenopausal.txt",
                 " --covar ",path_LIFEprepped,"LuebeckCooperation/CovarFile_LIFEHeart.txt",
                 " --covar-variance-standardize",
                 " --threads 20",
                 " --mach-r2-filter 0.8 2 --maf 0.01",
                 " --out ../results/03_PLINK_GLM/LIFEHeart_postmenopausal")
mycall5
system(mycall5)

mycall6 = paste0(path_plink2,
                 " --pfile ",path_LIFEHeart_Genetics,
                 " --glm hide-covar firth-fallback",
                 " cols=chrom,pos,ref,alt,firth,test,nobs,machr2,a1freq,a1freqcc,a1countcc,orbeta,se,ci,tz,p",
                 " --pheno ",path_LIFEprepped,"LuebeckCooperation/PhenoFile_LIFEHeart_premenopausal.txt",
                 " --covar ",path_LIFEprepped,"LuebeckCooperation/CovarFile_LIFEHeart.txt",
                 " --covar-variance-standardize",
                 " --threads 20",
                 " --mach-r2-filter 0.8 2 --maf 0.01",
                 " --out ../results/03_PLINK_GLM/LIFEHeart_premenopausal")
mycall6
system(mycall6)

#' # Session Info ####
#' ***
sessionInfo()
message("\nTOTAL TIME : " ,round(difftime(Sys.time(),time0,units = "mins"),3)," minutes")

