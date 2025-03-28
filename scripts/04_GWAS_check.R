#' ---
#' title: "GWAS in LIFE - checks"
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
#' I want to create two plots (QQ-Plot, Manhattan Plot) per study and hormone and setting, and I want a table with 
#' 
#' - sample size 
#' - total number of SNPs
#' - lambda
#' - number of SNPs with p<5e-8
#' - number of SNPs with p<1e-6 
#' - number of SNPs with p<0.05
#' 
#' # Initialize ####
#' ***
rm(list = ls())
time0<-Sys.time()
server = "angmar"

source("../SourceFile.R")
.libPaths()
source("../helperfunctions/gg_qqplot.R")

#' # Check GWAS results ####
#' ***
#' 
myFiles = list.files(path = "../results/03_PLINK_GLM/",pattern = ".linear")

ToDoList = data.table(file = myFiles)
ToDoList[,study := gsub("_.*","",file)]
ToDoList[,study := gsub("LIFE","LIFE-",study)]
ToDoList[,subgroup := gsub("[.].*","",file)]
ToDoList[,subgroup := gsub("LIFEAdult_","",subgroup)]
ToDoList[,subgroup := gsub("LIFEHeart_","",subgroup)]
ToDoList[,hormone := gsub(".glm.linear","",file)]
ToDoList[,hormone := gsub(".*[.]","",hormone)]

head(ToDoList)
ToDoList[,table(study,subgroup)]
ToDoList[,table(study,hormone)]

registerDoParallel(10)

dumTab1 = foreach(i = 1:dim(ToDoList)[1])%dopar%{
  #i=2
  myRow = ToDoList[i,]
  message("Working on trait ",myRow$hormone," in ",myRow$subgroup," (study: ",myRow$study,")")
  
  gwas = fread(paste0("../results/03_PLINK_GLM/",myRow$file))
  
  # Filter data for MAF>=0.01, mach r2 >=0.8 and statistics available (no NA)
  gwas = gwas[A1_FREQ >= 0.01 & A1_FREQ<= 0.99,]
  gwas = gwas[MACH_R2 >= 0.8 & !is.na(MACH_R2),]
  gwas = gwas[!is.na(BETA),]
  
  # QQ-Plot using all SNPs
  gwas[,stat := T_STAT^2]
  lambda = gwas[,median(stat)/qchisq(0.5, 1)]
  gwas[,maf := A1_FREQ]
  gwas[maf>0.5,maf := 1-A1_FREQ]
  
  qq_title = paste0(myRow$hormone," in ",myRow$subgroup," (study: ",myRow$study,")")
  qq_subtitle = paste("After extraction Filter: lambda = ",  round(lambda,4) )

  plot_qq =  gg_qqplot(pvalues = gwas$P, maf = gwas$maf, info = gwas$MACH_R2) +
    ggtitle(qq_title, qq_subtitle)

  png_fn_qq = paste0("../results/04_QQPlots/",myRow$study,"_",myRow$subgroup,"_",myRow$hormone, ".png")
  ggsave(filename= png_fn_qq, plot= plot_qq, width=30, height=20, units="cm", dpi=300)
  
  # Manhattan plot using SNPs with logP>2
  setnames(gwas,"#CHROM","CHR")
  gwas[CHR=="X",CHR := 23]
  gwas[,CHR := as.numeric(CHR)]
  
  png_fn_manhattan = paste0("../results/04_ManhattanPlots/",myRow$study,"_",myRow$subgroup,"_",myRow$hormone, ".png")
  png(png_fn_manhattan, 30,20, units = "cm", res = 300)
  
  manhattan(gwas[P<0.01,],
    chr = "CHR",
    bp = "POS",
    p = "P",
    snp = "ID",
    col = c("gray10", "gray60"),
    chrlabs = NULL,
    suggestiveline = -log10(1e-06),
    genomewideline = -log10(5e-08),
    highlight = NULL,
    logp = TRUE,
    annotatePval = NULL,
    annotateTop = TRUE)
  
  dev.off()
  
  # Create result table
  myRow[, n := gwas[,max(OBS_CT)]]
  myRow[, lambda := lambda]
  myRow[, SNPs := dim(gwas)[1]]
  myRow[, SNPs_gw := dim(gwas[P<5e-8])[1]]
  myRow[, SNPs_sug := dim(gwas[P<1e-6])[1]]
  myRow[, SNPs_nom := dim(gwas[P<0.05])[1]]
  myRow
}

SNPInfo = rbindlist(dumTab1)
fwrite(SNPInfo,file = "../results/04_SNPNumbers.txt",col.names = T,row.names = F, quote=F,sep="\t",dec=".")
save(SNPInfo,file = "../results/04_SNPNumbers.RData")


#' # Session Info ####
#' ***
sessionInfo()
message("\nTOTAL TIME : " ,round(difftime(Sys.time(),time0,units = "mins"),3)," minutes")
