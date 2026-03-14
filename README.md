# MRD × PET–CT concordance in multiple myeloma
Code and aggregated study-level data for the systematic review and meta-analysis “Minimal concordance but complementary prognostic value of bone–marrow MRD and PET–CT in multiple myeloma.”

The repository reproduces analyses quantifying:
- concordance between bone-marrow minimal residual disease (MRD) and PET–CT
- directional discordance between MRD–/PET–CT+ and MRD+/PET–CT– states
- the prognostic impact of dual MRD–/PET–CT– negativity on progression-free survival (PFS)

The primary dataset includes 10 cohorts contributing 1,138 paired MRD/PET–CT assessments. The main pooled results are an observed agreement of 60.0%, pooled Cohen’s κ of 0.14 (95% CI 0.03–0.25), pooled directional discordance log-odds of −0.83 (95% CI −1.87 to 0.21), and a pooled hazard ratio for PFS of 0.34 (95% CI 0.22–0.51) for dual-negative MRD–/PET–CT– versus all other categories.


## Contents
- `Multiple Myeloma Concordance.Rmd` – main R Markdown pipeline (runs end-to-end).
- `Multiple Myeloma Concordance.html` – rendered analytical report.
- `R/` – helper functions and figure-generation code.
- `data/` – aggregated study-level datasets for concordance, survival, risk of bias, and screening records.
- `README.md` – repository overview and reproducibility notes.


## Reproducing the analysis
1. Install R (version 4.5.0 or later recommended) and, optionally, RStudio.
2. Install required R packages:

r
   install.packages(c(
     "tidyverse", "here", "metafor", "ggplot2", "patchwork", "DescTools", "ggrepel", "DiagrammeR", "DiagrammeRsvg", "rsvg", "ragg", "robvis"
   ))
   
3. Open and knit the Multiple Myeloma Concordance.Rmd file.


## Analysis overview
The analytical workflow reconstructs paired 2×2 MRD/PET–CT contingency tables from published studies using the canonical ordering:

- a = MRD– / PET–CT+
- b = MRD– / PET–CT–
- c = MRD+ / PET–CT–
- d = MRD+ / PET–CT+

Agreement metrics include observed agreement, Cohen’s κ, and directional discordance. Pooled estimates are obtained using random-effects meta-analysis with restricted maximum likelihood (REML) and Knapp–Hartung adjustment. Sensitivity analyses include strict time-pairing (≤30 days between MRD and PET–CT assessments) and risk-of-bias exclusions. Prognostic analyses pool hazard ratios for progression-free survival comparing dual-negative MRD–/PET–CT– status with all other response categories.


## Data sources
All data used in this repository are aggregated study-level values extracted from published studies. The `data/` directory contains:

- `mrd_petct_data.csv` – reconstructed MRD/PET–CT contingency tables  
- `Screening_Log.xlsx` – PRISMA screening log  
- `QUADAS2.csv` – risk-of-bias assessment for concordance outcomes  
- `QUIPS.csv` – risk-of-bias assessment for prognostic outcomes  

No individual patient-level data are included.

## Reproducibility
The analytical workflow follows PRISMA 2020 reporting standards and the preregistered study protocol available on the Open Science Framework (OSF):
https://doi.org/10.17605/OSF.IO/3CH9E
See: https://archive.org/details/osf-registrations-3ch9e-v1

## Contact
Max Mendez-Lopez  
Department of Medical Oncology / Haematology  
HOCH Ostschweiz, St Gallen, Switzerland  
Email: Max.MendezLopez@h-och.ch

## Citation
Please cite the associated publication:
Mendez-Lopez M, Talarico M, Driessen C. Minimal concordance but complementary prognostic value of bone–marrow minimal residual disease and PET–CT in multiple myeloma: a systematic review and meta-analysis. eClinicalMedicine. 2026.

Repository:
https://github.com/MaxMendezL/MRD-PET-CT-Concordance-in-Multiple-Myeloma
