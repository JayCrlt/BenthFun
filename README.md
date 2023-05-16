# BenthFun

This repository hosts the main `BenthFun project` documents. So far, it is organized as follows:

1️⃣ [`Data`](https://github.com/JayCrlt/BenthFun/tree/main/Data) folder hosts the data Because the project will be split into two temporal seasons (i.e., between summer and after summer), you might find one subfolder for[`spring`](https://github.com/JayCrlt/BenthFun/tree/main/Data/Spring_2023) and another for fall (*coming soon...*). The spring season understands three majors experiments:

\- [`Transplant experiment`](https://github.com/JayCrlt/BenthFun/tree/main/Data/Spring_2023/Transplants) hosts mostly all the data. You might find 2 crucial documents that we will use almost along this campaign (*i.e.,* 🧪 [`Alkalinity analyses`](https://github.com/JayCrlt/BenthFun/tree/main/Data/Spring_2023/Transplants/Alkalinity) and 🤿 [`Diving_log`](https://github.com/JayCrlt/BenthFun/tree/main/Data/Spring_2023))

\- `Historic tiles` *coming soon...*

\- `PI Curves` *coming soon...*

2️⃣ [`Outputs`](https://github.com/JayCrlt/BenthFun/tree/main/Outputs) hosts the main outputs for further analyse.s \
Once again, you might find different sub-folders (i.e., main interesting figures, summary tables and intermediate outputs such as tables)

3️⃣ [`R_Script`](https://github.com/JayCrlt/BenthFun/tree/main/R_Script) hosts the scripts used for the current analyses.\
So far, there are 4 scripts. The first one I worked on (*i.e.*,`Quality_Check_O2_Sensors.R`) allows me to check quickly if something is wrong with the oxygen measures. The two following scripts (*i.e.*,`Alkalinity – T0.R` and `MiniDots. – T0.R`) do not deserved to be open. They will be sourced directly in the last script (*i.e.*, `Viz – T0.R`) and be run silently to provides useful plots

---
System informations

```
R version 4.2.1 (2022-06-23)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Monterey 12.2.1

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1]  readxl_1.4.2    patchwork_1.1.2 lubridate_1.9.2 forcats_1.0.0   stringr_1.5.0   
[6]  dplyr_1.1.0     purrr_1.0.1     readr_2.1.4     tidyr_1.3.0     tibble_3.2.0    
[11] ggplot2_3.4.1   tidyverse_2.0.0
```

---
Main collaborators: Samir Alliouane, Jordi Boada, Jeremy Carlot, Antonia Chiarore, Steeve Comeau, Jean-Pierre Gattuso, Alice Mirasole, Melissa Palmisciano, Nuria Teixido