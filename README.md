# Senescence in Carinascincus ocellatus: Analysis Code

Code repository accompanying the manuscript:

**[Manuscript title]**  
[Author list]  
*Functional Ecology* (in review)

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

Run scripts 01–04 before 05–06 (figures depend on prior outputs).
Scripts 01–04 can otherwise be run independently.

| Script | Description |
|--------|-------------|
| 01_growth_curves.Rmd | Von Bertalanffy growth curve analysis |
| 02_actuarial_senescence.Rmd | BaSTA survival and mortality modelling |
| 03_reproductive_senescence.Rmd | Reproductive senescence BaFTA modelling |
| 04_proxy_dead_supplementary.Rmd | Proxy-for-dead supplementary analysis |
| 05_figure_multipanel_species.Rmd | Multipanel figure including species photo |
| 06_figure_basta_bafta.Rmd | Four-panel growth curve repro and BaSTA/BaFTA output figure |

---

## Data

The full 25-year mark-recapture dataset is held by the Olsson/Wapstra 
research groups at the University of Tasmania and is available on 
reasonable request. This repository contains only the processed data 
subsets required to reproduce each analysis.

---

## Software

R version [x.x.x]  
Key packages: [you can fill these in at the end — e.g. BaSTA, lme4, ggplot2]

---

## Contact

Luisa Fitzpatrick  
luisa.fitzpatrick@utas.edu.au  
University of Tasmania