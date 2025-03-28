# Steroid hormones analyses for the Luebeck Cooperation

last updated: 28/03/2025

## General aim

This analysis is part of the project M08 of the Collaborative Research Center “Sexdiversity – determinants, meanings and implications of gender diversity in sociocultural, medical and biological contexts” (CRC 1665). This project will use a hormone-based quantitative trait score for sex stratification in genome-wide association studies (GWAS) to enhance the likelihood of identifying sex-specific genetic risk factors for coronary artery disease (CAD).

Here, we will estimate the genetic effects on hormones in the LIFE study. These summary statistics will then be used to calculate polygenic scores and applied to a CAD GWAS. 

## Contact

Team Leipzig (LIFE data):
-	Markus Scholz  (markus.scholz@imise.uni-leipzig.de)
-	Janne Pott (janne.pott@mrc-bsu.cam.ac.uk) 

Team Lübeck: 
-	Rédouane Aherrahrou (r.aherrahrou@uni-luebeck.de) 
-	Cherng Yang (cherng.yang@uni-luebeck.de)

## Analyzed hormones

-	Testosterone (TESTO)
-	Progesterone (PROG) 
-	Estradiol (E2)
-	Androstenedione (ANDRO)
-	Dehydroepiandrosterone sulfate (DHEAS)
-	17-OH-Progesterone (OHP17) 
-	Cortisol (CORT)
-	Aldosterone (ALDO)
-	Sex-hormone binding globulin (SHBG)
-	Luteinizing hormone (LH)
-	Follicle-stimulating hormone (FSH) 

QC: 

- LIFE-Heart: remove batches 1-4 due to update of the LC-MSMS device
- restrict time of blood sampling:
    - LIFE-Adult: between 7:00 and 10:59
    - LIFE-Heart: between 6:00 and 12:59
 
## Exclusion criteria

- Steroid medication (ATC codes starting with “G03” or “H02AB”)
- Women without uterus or ovaries

## Menopause definition

-	Post-menopausal definition:
    -	Days since last menstruation >365 & age >50
    - Days since last menstruation missing & age >60
-	Pre-menopausal definition: 
    - Days since last menstruation within 0-31 & age <60

## GWAS 

-	Tool: PLINK2
-	Linear regression model with additive SNP effect
-	Covariates: age, logBMI, time of blood sampling and genetic PC 
-	Chromosome X with total X-inactivation
-	SNP filtering: MAF >0.01, machr2 >0.8
