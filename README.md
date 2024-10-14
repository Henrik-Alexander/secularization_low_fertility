 October 2024

# Secularization and low fertility
This folder contains the code to fully reproduce the result of the paper **Secularization and low fertility: How declining church membership affects couples and their childbearing**. All code on which this analysis is based was written in the [**R**](https://www.r-project.org/) statistical programming language.


## Software and hardware
The analysis were executed in [**R**](https://www.r-project.org/) version 4.2.1 (2022-06-23 ucrt). The computing unit was platform x86_64-w64-mingw32/x64 (64-bit).
The program was running under Windows Server x64 (build 17763)

### Packages
This work would not have been possible with the scientific and programming contributions of people who developed packages and made them available free of use on [**R-Cran**](https://cran.r-project.org/). I list the packages used in this project to acknowledge the contribution of the authors and to ensure that people can download the required packages in order to fully reproduce the results. Furthermore, the interested reader can follow the link on the package name to read the vignettes.

- [`stargazer`](https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf) by Marek Hlavac
- [`feisr`](https://cran.r-project.org/web/packages/feisr/index.html) by Tobias Rüttenauer
- [`tidyverse`](https://cran.r-project.org/web/packages/tidyverse/index.html) by Hadley Wickham
- [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) by Matt Dowle et al.
- [`zoo`](https://cran.r-project.org/web/packages/zoo/index.html) by Achim Zeileis et al.
- [`reshape2`](https://cran.r-project.org/web/packages/reshape2/index.html) by Hadley Wickham
- [`usdata`](https://cran.rstudio.com/web/packages/usdata/index.html>) by Mine  Çetinkaya-Rundel et al.
- [`plm`](https://cran.r-project.org/web/packages/plm/plm.pdf) by Yves Croissant et al.
- [`clusterSEs`](https://cran.r-project.org/web/packages/clusterSEs/index.html) by Justin Esarey
- [`lmtest`](https://cran.r-project.org/web/packages/lmtest/index.html) by Torsten Hothorn et al.
- [`starpolisher`](https://github.com/ChandlerLutz/starpolishr) by Chandler Lutz
- [`aTSA`](https://cran.r-project.org/web/packages/aTSA/aTSA.pdf) by Debin Qiu
- [`readxl`](https://cran.r-project.org/web/packages/readxl/index.html) by Jennifer Bryan
- [`quantreg`](https://cran.r-project.org/web/packages/quantreg/index.html) by Roger Koenker
- [`SparseM`](https://cran.r-project.org/web/packages/SparseM/index.html) by Roger Koenker et al.
- [`rqpd`](https://r-forge.r-project.org/projects/rqpd/) by Roger Koenker and Stefan Holst Bache
- [`patchwork`](https://cran.r-project.org/web/packages/patchwork/index.html) by Thomas Lin Pedersen
- [`ggrepel`](https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html) by Kamil Slowikowski
- [`bea.R`](https://cran.r-project.org/web/packages/bea.R/bea.R.pdf) by Andrea Batch

## Reproduction Procedures

1. Register at the following websites: 
    * [US Mortality DataBase](https://usa.mortality.org/mp/auth.pl)
    * [Global Data Lab](https://globaldatalab.org/register/) 
    * [BEA](https://apps.bea.gov/API/signup/)

2. Download the data
    
    2.1. **Automatically:** Include the account information from [US Mortality DataBase](https://usa.mortality.org/mp/auth.pl), [Global Data Lab](https://globaldatalab.org/register/) and the API-key for [BEA](https://apps.bea.gov/API/signup/) in the `meta-file.R` script, set `down_type` = "automatic" and run the meta-file
    
    2.2. **Manually:** Visit the websites provided in the section raw_data below, download the data from the websites and store under the names provided in the description
3. Run the scripts `01.` to `10.`


## Directory structure:
The structure of the repository is as follows:

*   `\Code`: This folder contains all script files
*   `\Data`: This folder contains all data that was created in the code  
*   `\Raw_data`: This folder contains the raw, unmanipulated data
*   `\Figures`: This folder contains all the created figures
*   `\Results`: This folder includes tabular outputs
*   `\Functions`: This folder contains all the functions used in the code 
*   `\meta_file.R`: This script calls all the scripts. Requires that the user is registered on [US Mortality DataBase](https://usa.mortality.org/mp/auth.pl), [Global Data Lab](https://globaldatalab.org/register/) and has an API-key for [BEA](https://apps.bea.gov/API/signup/)


### Code:
*  "00 - Load the data.R" 
*  "01 - Edit birth register files.R"                       
*  "02 - Combine birth register files.R"     
*  "03 - Edit and aggregate pop counts.R"                   
*  "04 - Births by Parity.R"                 
*  "05 - Impute and calculate ASFRs.R"                      
*  "06 - Calculate TFRs etc.R"            
*  "07 - Development indicators.R"                          
*  "08 - Analysis.R"                           
*  "08.01 Regression diagnostics.R"                         
* "08.02 - Regression with controls.R"         
* "08.03 - Regression with mean age of childbearing.R"     
* "09.02 - Smoothing.R"                           
* "09.03 - longitudinal quantile regression V.R"           
* "09.04.- Sensitivity analysis; year and state omission.R" "10. Print the results.R" 
*   "10. - Print the results"

### Raw_data:

The raw_data subdirectory consists already of most required files to reproduce the results,,
except for 1) the birth files from the national bureau of economic research and 2) the population
data from . 

1) **Birth data** can be obtained from:
[from https://www.nber.org/data/vital-statistics-natality-data.html}

Please download the data for the years 1969 to 2018,
 and save the files in the folder "Raw_data/Births" in the following way:
   "natl1969.csv" - "natl2018.csv" 

2) Download the **population data** from NBER: Select the file single year, 1969-2018, county level data.
[https://www.nber.org/data/seer_u.s._county_population_data.html],
 and save the file as "Raw_data/Population/uswbosingleages.csv"
 
3) Loading the **HDI** data from [**Global Data Lab**](https://globaldatalab.org/). First, in order to download the data the user is required to register at Global Data Lab. Afterwards, choose in the top console under `Instruments` the categery `development indicators`, then select data tables and reduce the countries to the United States by filtering `Countries`. Save the data under `Controls/GDL-Sub-national-HDI-data.csv`.

5) Loading the **life tables** from [**United States Mortality DataBase**](https://usa.mortality.org/). First, register on the website. Second, select on the homepage `lifetables.zip`. This should download the complete data from the website. Under downloads, select the `zip-file` and export it to `Raw_data\controls\`.

4) Loading the data on **economic structure**. In order to execute the analysis on the mechanisms, data on the economic structure has to be downloaded from the [**Bureau of Economic Analysis**](https://www.bea.gov/). Follow the links underneath, click on the buttom `Download` in the top right corner of the webpage and save the data in the folder `~ Controls/` under the respective name. The file names used in the Code are state infront of the blue icon.

   - `Raw_data/Controls/economic_structure.xlsx` -> [**Service jobs 1969-2001**](https://apps.bea.gov/iTable/?reqid=70&step=30&isuri=1&year_end=-1&acrdn=4&classification=naics&state=0&yearbegin=-1&unit_of_measure=levels&major_area=0&area=12000&year=2019&tableid=31&category=431&area_type=0&statistic=1004#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyNCwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbInRhYmxlaWQiLCI0Il0sWyJDbGFzc2lmaWNhdGlvbiIsIlNJQyJdLFsiTWFqb3JfQXJlYSIsIjAiXSxbIlN0YXRlIixbIjAiXV0sWyJBcmVhIixbIjAxMDAwIiwiMDIwMDAiLCIwNDAwMCIsIjA1MDAwIiwiMDYwMDAiLCIwODAwMCIsIjA5MDAwIiwiMTAwMDAiLCIxMTAwMCIsIjEyMDAwIiwiMTMwMDAiLCIxNTAwMCIsIjE2MDAwIiwiMTcwMDAiLCIxODAwMCIsIjE5MDAwIiwiMjAwMDAiLCIyMTAwMCIsIjIyMDAwIiwiMjMwMDAiLCIyNDAwMCIsIjI1MDAwIiwiMjYwMDAiLCIyNzAwMCIsIjI4MDAwIiwiMjkwMDAiLCIzMDAwMCIsIjMxMDAwIiwiMzIwMDAiLCIzMzAwMCIsIjM0MDAwIiwiMzUwMDAiLCIzNjAwMCIsIjM3MDAwIiwiMzgwMDAiLCIzOTAwMCIsIjQwMDAwIiwiNDEwMDAiLCI0MjAwMCIsIjQ0MDAwIiwiNDUwMDAiLCI0NjAwMCIsIjQ3MDAwIiwiNDgwMDAiLCI0OTAwMCIsIjUwMDAwIiwiNTEwMDAiLCI1MzAwMCIsIjU0MDAwIiwiNTUwMDAiLCI1NjAwMCJdXSxbIlN0YXRpc3RpYyIsWyI1MDAiLCI2MTAiLCI2MjAiLCI3MDAiLCI4MDAiLCI5MDAiXV0sWyJVbml0X29mX21lYXN1cmUiLCJMZXZlbHMiXSxbIlllYXIiLFsiLTEiXV0sWyJZZWFyQmVnaW4iLCItMSJdLFsiWWVhcl9FbmQiLCItMSJdXX0=)
   - `Raw_data/Controls/economic_structure2.xlsx` ->  [**Service jobs 2001-2018**](https://apps.bea.gov/iTable/?reqid=70&step=30&isuri=1&year_end=-1&acrdn=4&classification=naics&state=0&yearbegin=-1&unit_of_measure=levels&major_area=0&area=12000&year=2019&tableid=31&category=431&area_type=0&statistic=1004#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyNCwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbInRhYmxlaWQiLCIzMCJdLFsiQ2xhc3NpZmljYXRpb24iLCJOQUlDUyJdLFsiTWFqb3JfQXJlYSIsIjAiXSxbIlN0YXRlIixbIjAiXV0sWyJBcmVhIixbIjAxMDAwIiwiMDIwMDAiLCIwNDAwMCIsIjA1MDAwIiwiMDYwMDAiLCIwODAwMCIsIjA5MDAwIiwiMTAwMDAiLCIxMTAwMCIsIjEyMDAwIiwiMTMwMDAiLCIxNTAwMCIsIjE2MDAwIiwiMTcwMDAiLCIxODAwMCIsIjE5MDAwIiwiMjAwMDAiLCIyMTAwMCIsIjIyMDAwIiwiMjMwMDAiLCIyNDAwMCIsIjI1MDAwIiwiMjYwMDAiLCIyNzAwMCIsIjI4MDAwIiwiMjkwMDAiLCIzMDAwMCIsIjMxMDAwIiwiMzIwMDAiLCIzMzAwMCIsIjM0MDAwIiwiMzUwMDAiLCIzNjAwMCIsIjM3MDAwIiwiMzgwMDAiLCIzOTAwMCIsIjQwMDAwIiwiNDEwMDAiLCI0MjAwMCIsIjQ0MDAwIiwiNDUwMDAiLCI0NjAwMCIsIjQ3MDAwIiwiNDgwMDAiLCI0OTAwMCIsIjUwMDAwIiwiNTEwMDAiLCI1MzAwMCIsIjU0MDAwIiwiNTUwMDAiLCI1NjAwMCJdXSxbIlN0YXRpc3RpYyIsWyI4MCIsIjYwMCIsIjcwMCIsIjgwMCIsIjkwMCIsIjEwMDAiLCIxMjAwIiwiMTMwMCIsIjE0MDAiLCIxNTAwIiwiMTYwMCIsIjE3MDAiLCIxODAwIiwiMTkwMCIsIjIwMDAiXV0sWyJVbml0X29mX21lYXN1cmUiLCJMZXZlbHMiXSxbIlllYXIiLFsiMjAxOCIsIjIwMTciLCIyMDE2IiwiMjAxNSIsIjIwMTQiLCIyMDEzIiwiMjAxMiIsIjIwMTEiLCIyMDEwIiwiMjAwOSIsIjIwMDgiLCIyMDA3IiwiMjAwNiIsIjIwMDUiLCIyMDA0IiwiMjAwMyIsIjIwMDIiLCIyMDAxIl1dLFsiWWVhckJlZ2luIiwiLTEiXSxbIlllYXJfRW5kIiwiLTEiXV19)
    - `Raw_data/Controls/total_jobs.xlsx` -> [**Overall jobs 1969-2001**](https://apps.bea.gov/iTable/?reqid=70&step=30&isuri=1&year_end=-1&acrdn=4&classification=naics&state=0&yearbegin=-1&unit_of_measure=levels&major_area=0&area=12000&year=2019&tableid=31&category=431&area_type=0&statistic=1004#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyNCwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbInRhYmxlaWQiLCIzMCJdLFsiQ2xhc3NpZmljYXRpb24iLCJOQUlDUyJdLFsiTWFqb3JfQXJlYSIsIjAiXSxbIlN0YXRlIixbIjAiXV0sWyJBcmVhIixbIjAxMDAwIiwiMDIwMDAiLCIwNDAwMCIsIjA1MDAwIiwiMDYwMDAiLCIwODAwMCIsIjA5MDAwIiwiMTAwMDAiLCIxMTAwMCIsIjEyMDAwIiwiMTMwMDAiLCIxNTAwMCIsIjE2MDAwIiwiMTcwMDAiLCIxODAwMCIsIjE5MDAwIiwiMjAwMDAiLCIyMTAwMCIsIjIyMDAwIiwiMjMwMDAiLCIyNDAwMCIsIjI1MDAwIiwiMjYwMDAiLCIyNzAwMCIsIjI4MDAwIiwiMjkwMDAiLCIzMDAwMCIsIjMxMDAwIiwiMzIwMDAiLCIzMzAwMCIsIjM0MDAwIiwiMzUwMDAiLCIzNjAwMCIsIjM3MDAwIiwiMzgwMDAiLCIzOTAwMCIsIjQwMDAwIiwiNDEwMDAiLCI0MjAwMCIsIjQ0MDAwIiwiNDUwMDAiLCI0NjAwMCIsIjQ3MDAwIiwiNDgwMDAiLCI0OTAwMCIsIjUwMDAwIiwiNTEwMDAiLCI1MzAwMCIsIjU0MDAwIiwiNTUwMDAiLCI1NjAwMCJdXSxbIlN0YXRpc3RpYyIsWyIxMCJdXSxbIlVuaXRfb2ZfbWVhc3VyZSIsIkxldmVscyJdLFsiWWVhciIsWyItMSJdXSxbIlllYXJCZWdpbiIsIi0xIl0sWyJZZWFyX0VuZCIsIi0xIl1dfQ==)
   - `Raw_data/Controls/total_jobs.xlsx` -> [**Overall jobs 2001-2018**](https://apps.bea.gov/iTable/?reqid=70&step=30&isuri=1&year_end=-1&acrdn=4&classification=naics&state=0&yearbegin=-1&unit_of_measure=levels&major_area=0&area=12000&year=2019&tableid=31&category=431&area_type=0&statistic=1004#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyNCwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbInRhYmxlaWQiLCI0Il0sWyJDbGFzc2lmaWNhdGlvbiIsIlNJQyJdLFsiTWFqb3JfQXJlYSIsIjAiXSxbIlN0YXRlIixbIjAiXV0sWyJBcmVhIixbIjAxMDAwIiwiMDIwMDAiLCIwNDAwMCIsIjA1MDAwIiwiMDYwMDAiLCIwODAwMCIsIjA5MDAwIiwiMTAwMDAiLCIxMTAwMCIsIjEyMDAwIiwiMTMwMDAiLCIxNTAwMCIsIjE2MDAwIiwiMTcwMDAiLCIxODAwMCIsIjE5MDAwIiwiMjAwMDAiLCIyMTAwMCIsIjIyMDAwIiwiMjMwMDAiLCIyNDAwMCIsIjI1MDAwIiwiMjYwMDAiLCIyNzAwMCIsIjI4MDAwIiwiMjkwMDAiLCIzMDAwMCIsIjMxMDAwIiwiMzIwMDAiLCIzMzAwMCIsIjM0MDAwIiwiMzUwMDAiLCIzNjAwMCIsIjM3MDAwIiwiMzgwMDAiLCIzOTAwMCIsIjQwMDAwIiwiNDEwMDAiLCI0MjAwMCIsIjQ0MDAwIiwiNDUwMDAiLCI0NjAwMCIsIjQ3MDAwIiwiNDgwMDAiLCI0OTAwMCIsIjUwMDAwIiwiNTEwMDAiLCI1MzAwMCIsIjU0MDAwIiwiNTUwMDAiLCI1NjAwMCJdXSxbIlN0YXRpc3RpYyIsWyIxMCJdXSxbIlVuaXRfb2ZfbWVhc3VyZSIsIkxldmVscyJdLFsiWWVhciIsWyItMSJdXSxbIlllYXJCZWdpbiIsIi0xIl0sWyJZZWFyX0VuZCIsIi0xIl1dfQ==)

### Functions: 
The subdirectory Functions should contain an R file called `~ /Functions/Functions.R`
