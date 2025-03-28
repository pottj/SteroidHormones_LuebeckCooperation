# ReadMe for LIFE-Heart

##	Authors and Acknowledgments 

- Janne Pott (1,2)
- Markus Scholz (1,3)
- Uta Ceglarek (3,4)

1 Institute for Medical Informatics, Statistics, and Epidemiology, University of Leipzig, 04107 Leipzig, Germany; 
2 MRC Biostatistics Unit, University of Cambridge, Cambridge, UK
3 LIFE Research Center for Civilization Diseases, University of Leipzig, 04103 Leipzig, Germany;
4 Institute of Laboratory Medicine, Clinical Chemistry, and Molecular Diagnostics, University Hospital, 04103 Leipzig, Germany

Acknowledgments:

We thank Sylvia Henger very much for data quality control. We thank Kay Olischer and Annegret Unger very much for technical assistance.

## Study name and brief details/references

LIFE-Heart (DOI https://doi.org/10.1093/ije/dyaa075)

LIFE-Heart is a cohort of patients with suspected or confirmed stable coronary artery disease or myocardial infarction collected at the Heart Center of the University of Leipzig, Germany. A total of 6,997 patients were recruited, and all underwent coronary angiography and vascular phenotyping. 

LIFE-Heart meets the ethical standards of the Declaration of Helsinki. The study is approved by the Ethics Committee of the Medical Faculty of the University Leipzig, Germany (Reg. No 276-2005) and is registered at ClinicalTrials.gov (NCT00497887). Written informed consent including agreement with genetic analyses was obtained from all participants. Patients with myocardial infarction were excluded from the present analysis. 

##	Phenotype (method of testing, assay etc).

All eight hormones were measured by liquid chromatography-tandem mass spectrometry (LC-MSMS). After batch 4, there was an change in method of the measurement, leading to an lower LOD (e.g. LOD of Estradiol batch 1-4 110 pmol/L; batch 5-45 11.1 pmol/L). Hence, values of batch 1-4 were discarded from analysis. 

We excluded individuals using hormone therapy (anatomical therapeutic chemical code starting with "G03" or "H02AB"), or with removed ovaries or uterus.

## Genotype QC

In LIFE-Heart, 5869 individuals were genotyped using the genome-wide single-nucleotide polymorphism (SNP) array Affymetrix Axiom CEU1 or Affymetrix Axiom CADLIFE containing CEU1 as genome-wide backbone plus 62,500 SNPs from known CAD risk loci. For genotype calling, the software Affymetrix Power Tools (version 1.17.0) was used and quality control (QC) of calling was performed following the best practice steps of Affymetrix guidelines.

Subject QC comprised analysis of dish QC, call rate, heterozygosity, sex mismatch, cryptic relatedness, and irregularities of X/Y intensity plots (for X-chromosomal analyses only). Genetic heterogeneity was assessed with principal component analysis. Outliers of >6 SDs in any of the first 10 principal components were removed.

SNP QC comprised call rate (>=97%), parameters of cluster plot irregularities according to Affymetrix recommendations, violation of Hardy–Weinberg equilibrium [p<1e-6 in exact test; for X chromosomal SNPs, only females were tested, with p<1e-4], and plate associations (p<1e-7). 

After QC, we harmonized the two arrays of LIFE-Heart by using the intersection of high-quality SNPs on both arrays. There was a final set of 5700 LIFE-Heart patients with 500,576 high-quality autosomal SNPs. For chromosome X, 5688 samples and 12,741 SNPs were available. 

## Imputation program/method.

Imputation was performed on the TOPMed Imputation Server with default settings. 

## GWAS analysis program 

- PLINK 2.0
- Imputation quality measure: INFO
- covariates: age, log(BMI), genetic PCs 1-10
