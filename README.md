# COVID-19 vaccination in Ontario: Exploring intra-provincial variations within Health Regions and socio-economic strata

## Paper repository

Organization:

This repository contains all the files, code, and documents used to create the paper and the Supplementary Materials (Appendix).

The structure is as follows:

- `code`:  this directory contains the R Scripts used to prepare the data, perform corrections (via raking), and run the regression model.

- `data`: This directory contains the  data files.
    - **FieldsBehaviouralSurveyRawDataV2V3.xlsx**: This is the original file that I found in the dropbox link that I was provided, it contains the dataset (Sheet 1), a data dictorionary, and some preliminary analyses that Fields ran regarding demographic differences between the dataset and the Ontario Census.
    - **Fields_data.csv**: This is a file above converted to csv format to import it in R.
    - **clean_dataset.csv**: File used for analysis obtained after all the cleaning steps.
    
- `data_cleaning`: This folder contains preliminary analyses and all the steps used to clean the original dataset.`Data_cleaning_Feb_17.qmd` contains the main steps for cleaning, whereas file "municipalities.qmd" has information about the use of Municipality data in the clean dataset.

The qmd files can be rendered to PDF (the same way you would render a RMarkdown file), for the time being I have left PDF versions of the files in place. If needed, instructions regarding rendering `qmd` files can be found in [this link](https://quarto.org/docs/get-started/).

- `manuscript`: This folder contains all the files used to create the main paper and the appendix. The main paper is `main.pdf`, and the appendix is `appendix.pdf`.
