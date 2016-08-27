# Social mobility analysis
***
This is a project I've been working at the Pompeu Fabra University on the estimation of upwards mobility for the PIAAC and IALS data. This readme is my attempt to leave a detailed description of all the steps I've taken through the data processing section and the analysis section. I also add some caveats and things you should consider if you'd like to reproduce the analysis.

***
#### Downloading and preparing the data
Let's start with the files you will need. This analysis consists of two scripts: one in which the data is downloaded and prepared for analysis and another in which I carry out the analysis. Until this point in time, nearly all of the analysis has been done for the PIAAC dataset and not on the IALS data.

The script that downloads and prepares the data is an adaption of a [script](https://github.com/ajdamico/asdfree/blob/master/Programme%20for%20the%20International%20Assessment%20of%20Adult%20Competencies/download%20import%20and%20design.R) by [@adjdamico](https://github.com/ajdamico) and its called "Download_prepare_data_survey.R". You can [acces it here](https://github.com/cimentadaj/social_mobility_analysis/blob/master/Download_prepare_data_survey.R).

This script acceses the OECD website, downloads the PIAAC data separetely for each country, does a lot of data manipulation over each dataset, and finally, creates a complex survey object with the data and its weights.  

_Note that each time you run this file it will download each file again, and this take some time(around 15-20 minutes each time). If you're interested in reruning the file only for data management purposes, be sure to have the data files in your working directory and comment out this section:_

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
 _Also note that MOST if not ALL of the data management of each data frame is done in this file BEFORE the complex survey design is created. That is because editing a complex survey object is complicated, specially if its a imputated list such as these ones. This is a bit frustrating because each time you want to change something in a variable you need to re run this file excluding the downloading of the data._
 
 ***
 
#### Analysing PIAAC data
In this [script](https://github.com/cimentadaj/social_mobility_analysis/blob/master/Analysis_PIAAC_survey.R), I do all of the analysis on the PIAAC data set. This section loads the .Rda data from your working directory and runs logistic regressions for each country separately, taking into account its complex design. The output is two html tables per country saved to your working directory containing the logistic regression results.

*** 

#### Multilevel models for PIAAC and IALS
In this [script] (https://github.com/cimentadaj/social_mobility_analysis/blob/master/Multilevel_PIAAC_IALS.R), I pool the IALS and PIAAC countries together(those which are not repeated) into a single data frame. I parse macro level indicators of the size of the welfare state for all countries based on the time of the survey plus the Employment Protection Legislation index (EPL) of each country and the first age at tracking for each country. I start with some basic models and explore relationships(this script is the most disorganized and its still in its early stages. I need to revise it and no conclusions have been made from these analysis).

The IALS data needs to be requested to Statistics Canada separately. _Note: This analysis won't work without the IALS data, you can look at how to download the data [here] (http://www5.statcan.gc.ca/olc-cel/olc.action?ObjId=89M0014X&ObjType=2&lang=en&limit=0)_

***
If you open the .Rproj from the repository zip, you will have all the scripts opened together.
