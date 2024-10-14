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

## Directory structure:
The structure of the repository is as follows:

*   `\Code`: This folder contains all script files
*   `\Functions`: This folder contains all the functions used in the code 


### Code:
*  "01_basic.R" 
*  "02_tax_registers.R"                       
*  "03_preparation.R"     
*  "04_demographics.R"                   
*  "05_relationships.R"                 
*  "06_descriptives.R"                      
*  "07_analysis.R"            
*  "08_simulation.R"                          
*  "09_spatial.R"                           
*  "10_twin_comparison.R"                         

### Data note
The Finnish administrative register data used in this analysis were obtained from Statistics Finland under special contractual arrangements. They were analyzed under the permission TK-53-1119-17. These data are not available from the authors. Persons interested in obtaining the data should contact Statistics Finland at \url{tutkijapalvelut@stat.fi}.
### Functions: 
The subdirectory Functions should contain an R file called `~ /functions/functions.R`
