# BenthFun

This repository hosts the main `BenthFun project` documents. The fieldwork will be split in two campaigns 🤿 (i.e., spring 🍃 2023 and fall 🍂 2023) and the repository is organized in 3 folders as follows:

📁 [`Data`](https://github.com/JayCrlt/BenthFun/tree/main/Data) is the folder where you might find the data needed to run the analysis.
This folder is organized itself as follows:
0️⃣ [Fieldwork documents to print](https://github.com/JayCrlt/BenthFun/tree/main/Data/0.%20Fieldwork%20documents%20to%20print) – This sub-folder is used to store the lab and underwater document to print. \n
1️⃣ [Diving log](https://github.com/JayCrlt/BenthFun/tree/main/Data/1.%20Diving%20log) – This sub-folder hosts the dates and hours of each dive. It is the cornerstone for each script written. \n
2️⃣ [Incubations](https://github.com/JayCrlt/BenthFun/tree/main/Data/2.%20Incubations) – This sub-folder contains the 3 main experiments folders. Each of them contains O2 and light data organized by incubation day. \n
The three mains experiments are: a) Transplants, b) Historic and c) PI Curves. More information will be added on this later. \n
3️⃣ [Alkalinity](https://github.com/JayCrlt/BenthFun/tree/main/Data/3.%20Alkalinity) – This sub-folderhas been used to determine the total alkalinity of each sample. The protocole is coming from the [IMEV – LOV](https://lov.imev-mer.fr/web/) \n
4️⃣ [Visual census](https://github.com/JayCrlt/BenthFun/tree/main/Data/4.%20Visual%20census) – this last sub-folder contains information about tile biodiversity and cover, an xlsx file to convert cover to biomass regarding the species observed and a masterclass led by Nuria Teixido and Antonia Chiarore in order to ID benthic species.

📁 [`Outputs`](https://github.com/JayCrlt/BenthFun/tree/main/Outputs) hosts the main outputs for further analyses. 
You might find the main figures, summary table and intermediate tables defined from analyses to generate summaries and figures.

📁 [`R_Script`](https://github.com/JayCrlt/BenthFun/tree/main/R_Script) hosts the scripts used for the current analyses.\
Several scripts have been written so far:

**O2 Data**
- `Quality_Check_O2_Sensors` is the first script to use. It allows us to check the O2 data quality from each incubation and to extract intermediate tables in the Outputs folder.
- `MiniDots` will be used then to summarize O2 data for each experiment (e.g., Transplants at T0, Transplants at T1)

**Calcification**
- `Titration_alkalinity` is used to defined the total alkalinity (TA) from the titration done in the lab
- `Alkalinity` is used to convert TA to calcification or dissolution rates

**Light influence**
- `PI_Photo` is used to look at the PAR profile during the PI curve experiment
- `PAR_Profiles` is used to define the PI curves and to vizualise them

**Viz**
- `Viz` will be used to load each script and to provides the figures

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
[1]  readxl_1.4.2    patchwork_1.1.2   lubridate_1.9.2   forcats_1.0.0   stringr_1.5.0   
[6]  dplyr_1.1.0     purrr_1.0.1       readr_2.1.4       tidyr_1.3.0     tibble_3.2.0    
[11] ggplot2_3.4.1   tidyverse_2.0.0
```

---

**Main collaborators**: Samir Alliouane, Jordi Boada, Jérémy Carlot, Antonia Chiarore, Steeve Comeau, Jean-Pierre Gattuso, Alice Mirasole, Melissa Palmisciano, Nuria Teixido