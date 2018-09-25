# IMPORTANT
This replication file is correct but the README is a bit out of date. 

# Social mobility analysis
***
This is a project I've been working at the Pompeu Fabra University on the estimation of upwards mobility for the PIAAC and IALS data. This README reproduces the analysis until now. This was my first R project so the analysis is very dirty.

***

#### Downloading and preparing the data

Let's start with the files you will need. This analysis consists of two scripts: one in which the data is downloaded and prepared for analysis and another in which I carry out the analysis. Until this point in time, nearly all of the analysis has been done for the PIAAC dataset and not on the IALS data.

The script that downloads and prepares the data is an adaption of a [script](https://github.com/ajdamico/asdfree/blob/master/Programme%20for%20the%20International%20Assessment%20of%20Adult%20Competencies/download%20import%20and%20design.R) by [@adjdamico](https://github.com/ajdamico) and its called "Download_prepare_data_survey.R". You can [acces it here](https://github.com/cimentadaj/social_mobility_analysis/blob/master/Download_prepare_data_survey.R).

This script accesses the OECD website, downloads the PIAAC data separately for each country, does a lot of data manipulation over each dataset, and finally, creates a complex survey object with the data and its weights.  

_Note that each time you run this file it will download each file again, and this take some time(around 15-20 minutes each time). If you're interested in rerunning the file only for data management purposes, be sure to have the data files in the ./data/ folder (ignored and not present in the remote repo but automatically created by this R script) and comment out this section:_

```{r eval=F echo=F}
 links <- character(0)
 countrylist <- list()
 for (i in 1:length(csv.fns)) {
     links <- c(links, paste0(oecd.csv.website, csv.fns[i], ".csv"))
     download.file(links[i], destfile =csv.fns[i])
     countrylist[[i]] <-  read.csv(csv.fns[i], stringsAsFactors = FALSE)
 }
 names(countrylist) <- csv.fns
 save(countrylist, file="countrylist.Rda")
 ```
 _Also note that MOST, if not ALL of the data management of each data frame, is done in this file BEFORE the complex survey design is created. That is because editing a complex survey object is complicated, specially if its an imputated list such as these ones. This is a bit frustrating because each time you want to change something in a variable you need to rerun this file excluding the downloading of the data._
 
 ***
 
#### Analysing PIAAC data

In this [script](https://github.com/cimentadaj/social_mobility_analysis/blob/master/Analysis_PIAAC_survey.R), I do all of the analysis on the PIAAC data set. This section loads the .Rda data from your working directory and runs the data analysis, outputting tables in `.docx` format in the `./Tables/` directory. Additionally, there are other scripts that perform specific tasks such as estimating some complex interactions. Each script should have comments which are self-explanatory.
***
If you open the .Rproj from the repository zip, you will have all the scripts opened together.
