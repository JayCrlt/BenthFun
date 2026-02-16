# Unravelling marine benthic functioning shifts under ocean acidification

![GitHub](https://img.shields.io/badge/GitHub-39457E?style=for-the-badge&logo=github&logoColor=white)
![Gitlab](https://img.shields.io/badge/GitLab-FFA500?style=for-the-badge&logo=gitlab&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=RStudio&logoColor=white)
![Google Drive](https://img.shields.io/badge/Google%20Drive-FCD535?style=for-the-badge&logo=googledrive&logoColor=white)

![alt text](https://github.com/JayCrlt/BenthFun/blob/main/Meetings_and_Medias/Pictures/Incubations.JPG)

**Authors**: J. Carlot, S. Comeau, A. Chiarore, A. Mirasole, S. Alliouane, F. Micheli, C. L. Hurd, J.-P. Gattuso, N. Teixidó

**Abstract**: Ocean acidification (OA) driven by increasing atmospheric CO₂ is altering marine biodiversity. However, impacts of OA on ecosystem functioning at the community level, including calcification, primary production, and nutrient uptake, remain largely unknown. Here, we conducted community transplant experiments at natural CO₂ vents to assess how declining pH affects marine community species composition, biomass, and key ecosystem processes over time. Our results indicate that community shifts caused by declining pH lead to decreased biomass and calcification rates, while photosynthesis and nutrient uptake rates increased. By leveraging OA field model systems and in situ measurements of ecosystem functioning, this study provides critical insights into how OA-induced biodiversity loss reshapes the structure and functioning of temperate marine coastal ecosystems.

---

This repository hosts the main `BenthFun project` documents. The fieldwork will be split into two campaigns 🤿 (i.e., spring 🍃 2023 and fall 🍂 2023).

⚠️ `DISCLAIMER #1`: The raw observational data supporting this study cannot be released until the completion of the Beatriu de Pinos grant 2024BP00106 (expected 31/08/2027). To ensure reproducibility, we provide the fully processed datasets and model outputs used for all analyses and figures, along with annotated code detailing all steps from raw data to final results. Upon completion of the grant, the raw data will be made publicly available in this repository.

⚠️ `DISCLAIMER #2`: All figures and figure panels presented in the manuscript were post-processed in Keynote (Apple). Consequently, the plots generated using the code provided here may have different formatting, or arrangements compared to those shown in the published manuscript. Nevertheless, the provided code reproduces all figures exactly in their raw form, enabling full replication of the underlying analyses.

➡️ The repository is organized into 3 main folders as follows:

📁 [`Data`](https://github.com/JayCrlt/BenthFun/tree/main/Data_Online) is the folder where you might find the data needed to reproduce the figures ✏️.\
• [Figure 1](https://github.com/JayCrlt/BenthFun/blob/main/Data_Online/Data_Figure_1.xlsx) • [Figure 2](https://github.com/JayCrlt/BenthFun/blob/main/Data_Online/Data_Figure_2.xlsx) • [Figure 3](https://github.com/JayCrlt/BenthFun/blob/main/Data_Online/Data_Figure_3.xlsx) • [Figure 4](https://github.com/JayCrlt/BenthFun/blob/main/Data_Online/Data_Figure_4.xlsx) • 

📁 [`Outputs`](https://github.com/JayCrlt/BenthFun/tree/main/Outputs) hosts the main figures 📊.\
• [Figure 1](https://github.com/JayCrlt/BenthFun/blob/main/Outputs/Figures/Final_Figures/PNG/Figure_1.png) • [Figure 2](https://github.com/JayCrlt/BenthFun/blob/main/Outputs/Figures/Final_Figures/PNG/Figure_2.png) • [Figure 3](https://github.com/JayCrlt/BenthFun/blob/main/Outputs/Figures/Final_Figures/PNG/Figure_3.png) • [Figure 4](https://github.com/JayCrlt/BenthFun/blob/main/Outputs/Figures/Final_Figures/PNG/Figure_4.png) • 

📁 [`R_Script`](https://github.com/JayCrlt/BenthFun/tree/main/R_Script) hosts the scripts used for the current analyses 💻.\
• [Figure 1](https://github.com/JayCrlt/BenthFun/blob/main/R_Script/Figure_1_Script.R) • [Figure 2](https://github.com/JayCrlt/BenthFun/blob/main/R_Script/Figure_2_Script.R) • [Figure 3](https://github.com/JayCrlt/BenthFun/blob/main/R_Script/Figure_3_Script.R) • [Figure 4](https://github.com/JayCrlt/BenthFun/blob/main/R_Script/Figure_4_Script.R) • 

---
System informations

```
─ Session info ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.4.3 (2025-02-28)
 os       macOS 26.2
 system   aarch64, darwin20
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       Europe/Madrid
 date     2026-02-15
 rstudio  2026.01.0+392 Apple Blossom (desktop)
 pandoc   NA
 quarto   1.8.25 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto

─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 package        * version  date (UTC) lib source
 abind            1.4-8    2024-09-12 [1] CRAN (R 4.4.1)
 backports        1.5.0    2024-05-23 [1] CRAN (R 4.4.1)
 bayesplot        1.11.1   2024-02-15 [1] CRAN (R 4.4.0)
 bridgesampling   1.1-2    2021-04-16 [1] CRAN (R 4.4.0)
 brms           * 2.22.0   2024-09-23 [1] CRAN (R 4.4.1)
 Brobdingnag      1.2-9    2022-10-19 [1] CRAN (R 4.4.0)
 callr            3.7.6    2024-03-25 [1] CRAN (R 4.4.0)
 cellranger       1.1.0    2016-07-27 [1] CRAN (R 4.4.0)
 checkmate        2.3.2    2024-07-29 [1] CRAN (R 4.4.0)
 cli              3.6.5    2025-04-23 [1] CRAN (R 4.4.1)
 coda             0.19-4.1 2024-01-31 [1] CRAN (R 4.4.1)
 codetools        0.2-20   2024-03-31 [1] CRAN (R 4.4.3)
 colorspace       2.1-1    2024-07-26 [1] CRAN (R 4.4.1)
 crayon           1.5.3    2024-06-20 [1] CRAN (R 4.4.1)
 curl             7.0.0    2025-08-19 [1] CRAN (R 4.4.1)
 distributional   0.5.0    2024-09-17 [1] CRAN (R 4.4.1)
 dplyr          * 1.1.4    2023-11-17 [1] CRAN (R 4.4.0)
 emmeans          1.10.7   2025-01-31 [1] CRAN (R 4.4.1)
 estimability     1.5.1    2024-05-12 [1] CRAN (R 4.4.1)
 farver           2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
 forcats        * 1.0.0    2023-01-29 [1] CRAN (R 4.4.0)
 generics         0.1.4    2025-05-09 [1] CRAN (R 4.4.1)
 ggplot2        * 4.0.0    2025-09-11 [1] CRAN (R 4.4.1)
 ggridges       * 0.5.6    2024-01-23 [1] CRAN (R 4.4.0)
 glue             1.8.0    2024-09-30 [1] CRAN (R 4.4.1)
 gridExtra        2.3      2017-09-09 [1] CRAN (R 4.4.1)
 gtable           0.3.6    2024-10-25 [1] CRAN (R 4.4.1)
 hms              1.1.3    2023-03-21 [1] CRAN (R 4.4.0)
 inline           0.3.21   2025-01-09 [1] CRAN (R 4.4.1)
 jsonlite         2.0.0    2025-03-27 [1] CRAN (R 4.4.1)
 labeling         0.4.3    2023-08-29 [1] CRAN (R 4.4.1)
 lattice          0.22-6   2024-03-20 [1] CRAN (R 4.4.3)
 lifecycle        1.0.4    2023-11-07 [1] CRAN (R 4.4.1)
 loo              2.8.0    2024-07-03 [1] CRAN (R 4.4.0)
 lubridate      * 1.9.4    2024-12-08 [1] CRAN (R 4.4.1)
 magrittr         2.0.3    2022-03-30 [1] CRAN (R 4.4.1)
 Matrix           1.7-2    2025-01-23 [1] CRAN (R 4.4.3)
 matrixStats      1.5.0    2025-01-07 [1] CRAN (R 4.4.1)
 mvtnorm          1.3-3    2025-01-10 [1] CRAN (R 4.4.1)
 nlme             3.1-167  2025-01-27 [1] CRAN (R 4.4.3)
 patchwork      * 1.3.0    2024-09-16 [1] CRAN (R 4.4.1)
 pillar           1.10.2   2025-04-05 [1] CRAN (R 4.4.1)
 pkgbuild         1.4.7    2025-03-24 [1] CRAN (R 4.4.3)
 pkgconfig        2.0.3    2019-09-22 [1] CRAN (R 4.4.1)
 posterior        1.6.1    2025-02-27 [1] CRAN (R 4.4.1)
 processx         3.8.6    2025-02-21 [1] CRAN (R 4.4.1)
 ps               1.9.1    2025-04-12 [1] CRAN (R 4.4.1)
 purrr          * 1.0.4    2025-02-05 [1] CRAN (R 4.4.1)
 QuickJSR         1.7.0    2025-03-31 [1] CRAN (R 4.4.1)
 R6               2.6.1    2025-02-15 [1] CRAN (R 4.4.1)
 ragg             1.3.3    2024-09-11 [1] CRAN (R 4.4.1)
 RColorBrewer     1.1-3    2022-04-03 [1] CRAN (R 4.4.1)
 Rcpp           * 1.1.0    2025-07-02 [1] CRAN (R 4.4.1)
 RcppParallel     5.1.10   2025-01-24 [1] CRAN (R 4.4.1)
 readr          * 2.1.5    2024-01-10 [1] CRAN (R 4.4.0)
 readxl         * 1.4.4    2025-02-27 [1] CRAN (R 4.4.1)
 rJava            1.0-11   2024-01-26 [1] CRAN (R 4.4.1)
 rlang            1.1.6    2025-04-11 [1] CRAN (R 4.4.1)
 rstan            2.32.7   2025-03-10 [1] CRAN (R 4.4.1)
 rstantools       2.4.0    2024-01-31 [1] CRAN (R 4.4.1)
 rstudioapi       0.17.1   2024-10-22 [1] CRAN (R 4.4.1)
 S7               0.2.0    2024-11-07 [1] CRAN (R 4.4.1)
 scales           1.4.0    2025-04-24 [1] CRAN (R 4.4.1)
 sessioninfo      1.2.3    2025-02-05 [1] CRAN (R 4.4.1)
 StanHeaders      2.32.10  2024-07-15 [1] CRAN (R 4.4.1)
 stringi          1.8.7    2025-03-27 [1] CRAN (R 4.4.1)
 stringr        * 1.5.1    2023-11-14 [1] CRAN (R 4.4.0)
 systemfonts      1.3.1    2025-10-01 [1] CRAN (R 4.4.1)
 tensorA          0.36.2.1 2023-12-13 [1] CRAN (R 4.4.1)
 textshaping      1.0.0    2025-01-20 [1] CRAN (R 4.4.1)
 tibble         * 3.2.1    2023-03-20 [1] CRAN (R 4.4.0)
 tidyr          * 1.3.1    2024-01-24 [1] CRAN (R 4.4.1)
 tidyselect       1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
 tidyverse      * 2.0.0    2023-02-22 [1] CRAN (R 4.4.0)
 timechange       0.3.0    2024-01-18 [1] CRAN (R 4.4.1)
 tzdb             0.5.0    2025-03-15 [1] CRAN (R 4.4.1)
 utf8             1.2.5    2025-05-01 [1] CRAN (R 4.4.1)
 V8               6.0.1    2025-02-02 [1] CRAN (R 4.4.1)
 vctrs            0.6.5    2023-12-01 [1] CRAN (R 4.4.0)
 withr            3.0.2    2024-10-28 [1] CRAN (R 4.4.1)
 xlsx             0.6.5    2020-11-10 [1] CRAN (R 4.4.0)
 xlsxjars         0.6.1    2014-08-22 [1] CRAN (R 4.4.1)
 xtable           1.8-4    2019-04-21 [1] CRAN (R 4.4.1)

 [1] /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library
 * ── Packages attached to the search path.
```

---

**Author contributions**: All authors have agreed to the submission of this manuscript and take responsibility for the integrity, accuracy, and ethics of the work. J.C. and N.T. are responsible for the overall integrity of the manuscript. All authors made substantial intellectual contributions and meet the authorship criteria of Ecology Letters.
Conceptualization: J.C., S.C., J-P.G. and N.T. Data curation: J.C., A.C., A.M., S.A. and N.T. Formal analysis: J.C. Investigation: J.C., A.C., A.M. and N.T. Writing (first draft): J.C. Writing (review and editing): J.C., S.C., A.C., A.M., S.A., C.L.H., F.M., J-P.G. and N.T. Funding: J.C & N.T. The authors declare no competing interests.