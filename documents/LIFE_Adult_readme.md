# Read Me for LIFE-Adult

##	Authors and Acknowledgments 

- Janne Pott (1,2)
- Markus Scholz (1,3)
- Uta Ceglarek (3,4)

1 Institute for Medical Informatics, Statistics, and Epidemiology, University of Leipzig, 04107 Leipzig, Germany; 
2 MRC Biostatistics Unit, University of Cambridge, Cambridge, UK
3 LIFE Research Center for Civilization Diseases, University of Leipzig, 04103 Leipzig, Germany;
4 Institute of Laboratory Medicine, Clinical Chemistry, and Molecular Diagnostics, University Hospital, 04103 Leipzig, Germany

Acknowledgments:

We thank Kerstin Wirkner very much for running the LIFE study center. LIFE-Adult genotyping (round 3) was done at the Cologne Center for Genomics (University of Cologne, Peter Nürnberg and Mohammad R. Toliat). For LIFE-Adult genotype imputation, compute infrastructure provided by ScaDS (Dresden/Leipzig Competence Center for Scalable Data Services and Solutions) at the Leipzig University Computing Center was used.  

## Study name and brief details/references

LIFE-Adult (DOI https://doi.org/10.1186/s12889-015-1983-z)

LIFE-Adult is a population-based cohort with 10,000 randomly selected participants from Leipzig, Germany. Recruitment started in 2011, and a 10-year follow-up is ongoing. The primary focus of this study was on civilization diseases such as obesity, vascular disease, and mental health, and which genetic or environmental risk factors affect these.

LIFE-Adult meets the ethical standards of the Declaration of Helsinki, and the Ethics Committee of the Medical Faculty of the University Leipzig (Leipzig, Germany) has approved the study (registration no. 263-2009-14122009). Written informed consent, including agreement with genetic analyses, was obtained from all participants.

## Phenotype (method of testing, assay etc).

Estradiol, testosterone, cortisol, DHEA-S, SHBG, FSH, and LH were measured by an electrochemiluminescence immunoassay (ECLIA; Roche Cobas). 

Aldosterone, androstenedione, progesterone, and 17-OHP were measured by liquid chromatography-tandem mass spectrometry (LC-MSMS). 

We excluded individuals using hormone therapy (anatomical therapeutic chemical code starting with "G03" or "H02AB"), or with removed ovaries or uterus.

## Genotype QC

In LIFE-Adult, 7838 individuals were genotyped using the genome-wide single-nucleotide polymorphism (SNP) array Affymetrix Axiom CEU1. 
For genotype calling, the software Affymetrix Power Tools (version 1.20.6) was used and quality control (QC) of calling was performed following the best practice steps of Affymetrix guidelines.

Subject QC comprised analysis of dish QC, call rate, heterozygosity, sex mismatch, cryptic relatedness, and irregularities of X/Y intensity plots (for X-chromosomal analyses only). Genetic heterogeneity was assessed with principal component analysis. Outliers of >6 SDs in any of the first 10 principal components were removed.

SNP QC comprised call rate (>=97%), parameters of cluster plot irregularities according to Affymetrix recommendations, violation of Hardy–Weinberg equilibrium [p<1e-6 in exact test; for X chromosomal SNPs, only females were tested, with p<1e-4], and plate associations (p<1e-7). 

After QC there was a final set of 7669 LIFE-Adult probands with 528,517 high-quality autosomal SNPs. For chromosome X, 7660 samples and 13,460 SNPs were available. 

## Imputation program/method.

Imputation was performed on the TOPMed Imputation Server with default settings. 

## GWAS analysis program 

- PLINK 2.0
- Imputation quality measure: INFO
- covariates: age, log(BMI), genetic PCs 1-10
