# Senescence in Carinascincus ocellatus: Analysis Code

Code repository accompanying the manuscript:

**[Seasonality drives population-specific senescence trajectories in a reptile]**  
[LJ Fitzpatrick*, H Cayuela, M Olsson, GM While and E Wapstra]  
*Functional Ecology* (pre submission)

---

## Overview

This repository contains the R Markdown analysis workflows for a comparative 
study of actuarial and reproductive senescence in spotted snow skinks 
(*Carinascincus ocellatus*) across thermally contrasting highland and lowland 
populations in Tasmania, using 25 years of mark-recapture data.

---

## Repository structure
```
scripts/     R Markdown workflows (numbered in manuscript order)
data/        Subsetted data files required by each script
outputs/     Generated figures
```

## Scripts

Run scripts 01–03 before 04 (figures depend on prior outputs).
Scripts 01–03 can otherwise be run independently.

| Script | Description |
|--------|-------------|
| 01_Actuarial_senescence_BaSTA.Rmd | Survival and mortality modelling |
| 02_Reproductive_senescence_BaFTA.Rmd | Reproductive senescence modelling |
| 03_Growth_curve.Rmd | Von Bertalanffy growth curve analysis |
| 04_Manuscript_figures.Rmd | 2 Multipanel figures |

---

## Data

The full 25-year mark-recapture dataset is held by the Behavioural and Evolutionary Ecology research group at the University of Tasmania and is available on reasonable request. This repository contains only the processed data subsets required to reproduce each analysis.

---

## Software

R version [4.5.1]  
Key packages: [fill these in at the end — e.g. BaSTA, lme4, ggplot2]

---

## Contact

Luisa Fitzpatrick  
luisa.fitzpatrick@utas.edu.au  
University of Tasmania