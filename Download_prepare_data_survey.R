
# This script was originally from anthony joseph damico - ajdamico@gmail.com
# If you want to find out more, please visit:
# analyze survey data for free (http://asdfree.com) with the r language
# each and every available file hooray

# remove the # in order to run this install.packages line only once
# install.packages( c( "survey" , "mitools" , "downloader" , "digest", "dplyr", "car", "psych", "matrixStats" ) )

# set your PIAAC data directory
# after downloading and importing
# all multiply-imputed, replicate-weighted complex-sample survey designs
# will be stored here
# use forward slashes instead of back slashes

### THIS IS WHERE YOU INTRODUCE YOUR WORKING DIRECTORY ###
setwd("/Users/cimentadaj/Google Drive/Gosta project/PIAAC2/social_mobility_analysis")


library(survey)			# load survey package (analyzes complex design surveys)
library(mitools) 		# load mitools package (analyzes multiply-imputed data)
library(downloader)			# downloads and then runs the source() function on scripts from github
library(dplyr)
library(car)
library(psych)
library(matrixStats)

# 
# # # load the download_cached and related functions
# # # to prevent re-downloading of files once they've been downloaded.
source_url( 
    "https://raw.githubusercontent.com/ajdamico/asdfree/master/Download%20Cache/download%20cache.R" , 
    prompt = FALSE , 
    echo = FALSE 
)
# 
# 
# # designate the oecd public use file page
 oecd.csv.website <- 'http://vs-web-fs-1.oecd.org/piaac/puf-data/CSV/'
# 
# # download the contents of that page
 csv.page <- readLines( oecd.csv.website )
# 
# # figure out all lines on that page with a hyperlink
 csv.links <- unlist( strsplit( csv.page , "<A HREF=\"" ) )
# 
# # further refine the links to only the ones containing the text `CSV/[something].csv`
 csv.texts <- csv.links[ grep( "(.*)CSV/(.*)\\.csv\">(.*)" , csv.links ) ]
# 
# # figure out the base filename of each csv on the website
 csv.fns <- gsub( "(.*)CSV/(.*)\\.csv\">(.*)" , "\\2" , csv.texts )
# 
# # initiate a temporary file on the local computer
 tf <- tempfile()
# 
# # R will exactly match SUDAAN results and Stata with the MSE option results
# options( survey.replicates.mse = TRUE )
# # otherwise if it is commented out or set to FALSE
#######
 links <- character(0)
 countrylist <- list()
 for (i in 1:length(csv.fns)) {
     links <- c(links, paste0(oecd.csv.website, csv.fns[i], ".csv"))
     download.file(links[i], destfile =csv.fns[i])
     countrylist[[i]] <-  read.csv(csv.fns[i], stringsAsFactors = FALSE)
 }
 names(countrylist) <- csv.fns
#####
 save(countrylist, file="countrylist.Rda")
 load("countrylist.Rda")
vars <- c("ISCO1C","J_Q07b","J_Q06b","J_Q08","J_Q07a","J_Q06a","B_Q01a",
          "VEMETHOD","CNTRYID","GENDER_R","AGE_R","AGEG5LFS","I_Q04l","I_Q04j",
          "I_Q04m",paste0(rep("PVNUM",10),1:10),paste0(rep("PVLIT",10),1:10),
          paste0(rep("PVPSL",10),1:10), paste0(rep("SPFWT",80),0:80))

sapply(countrylist, function(x) setdiff(vars, colnames(x)))

data.management <- function(x) {
    
    x <- x[vars]
    x <- tbl_df(x) %>% rename(isco=ISCO1C,dadedu=J_Q07b,momedu=J_Q06b,
                  numbooks=J_Q08,dadimmigrant=J_Q07a,
                  momimmigrant=J_Q06a,
                  eduattain=B_Q01a,gender=GENDER_R,
                  age=AGE_R,age_categories=AGEG5LFS,cntry=CNTRYID,
                  bottomthings=I_Q04j, differentideas=I_Q04l,
                  additinfo=I_Q04m)

numtrans <- function(x) {
    x <- as.numeric(as.character(x))
}

vars <- c("bottomthings","differentideas","additinfo","isco",
          "momedu","dadedu","numbooks","dadimmigrant","eduattain")

x[,vars] <- lapply(x[,vars], numtrans)

factor <- fa(x[,c("bottomthings","differentideas","additinfo")], nfactors = 1,rotate="none", fm="pa", score=T)

x$VEMETHOD <- as.character(x$VEMETHOD)
x$non.cognitive <- as.numeric(factor$scores)

quant <- quantile(scale(x$PVNUM1), probs = c(0.25,0.75), na.rm=T)
quant2 <- quantile(scale(x$non.cognitive), probs = c(0.25,0.75), na.rm=T)


x$highab <- as.numeric(scale(x$PVNUM1) >= quant[2][[1]] & scale(x$non.cognitive) >= quant2[2][[1]]) ## Both high quantiles of cognitive and non-cognitive
x$lowab <- as.numeric(scale(x$PVNUM1) <= quant[1][[1]] & scale(x$non.cognitive) <= quant2[1][[1]]) ## Both low quantiles of cognitive and non-cognitive
x$midcoghigh <- as.numeric(scale(x$PVNUM1) >= quant[2][[1]] & scale(x$non.cognitive) <= quant2[1][[1]]) ## High cognitive, low non-cognitive
x$midnonhigh <- as.numeric(scale(x$PVNUM1) <= quant[1][[1]] & scale(x$non.cognitive) >= quant2[2][[1]])## Low cognitive, high non-cognitive

x$age <- as.numeric(x$age)
x <- as.data.frame(x)
## Recoding dependent var into dummy: 1= service class, 0 = all else,
## I exclude ppl who didn't work in the last 5 years(code 9)

#         Armed forces	                                    0       EGP
#         Legislators, senior officials and managers	    1       1
#         Professionals	                                    2       1    
#         Technicians and associate professionals	        3       2
#         Clerks	                                        4       3
#         Service workers and shop and market sales workers	5       3
#         Skilled agricultural and fishery workers	        6       10
#         Craft and related trades workers	                7       8
#         Plant and machine operators and assemblers	    8       9
#         Elementary occupations	                        9       9
#         No paid work for past 5 years	                    9995
#         Valid skip	                                    9996
#         Don't know	                                    9997
#         Refused	                                        9998
#         Not stated or inferred	                        9999


x$isco <- recode(x$isco, "c(9995,9996,9997,9998,9999,0)=NA")
x$serviceclass <- as.numeric(as.character(x$isco))
x$serviceclass <- recode(x$serviceclass, "c(1,2)=1; c(3,4,5,6,7,8,9)=0")

x$middleclass <- 0
x$middleclass[x$isco %in% c(3,4,5)] <- 1
x$lowerclass <- as.numeric(x$isco %in% c(6,7,8,9))

## Recoding fathers education into two levels instead of 3
x$origin12 <- x$dadedu
x$origin12 <- as.numeric(x$dadedu == 1)
## Recoding fathers education into two levels instead of 3
x$origin56 <- as.numeric(x$dadedu == 3)
x$lowmidisced <- as.numeric(x$dadedu %in% c(2,1))
## Education homogamy dummies
x$highorigin <- as.numeric(x$dadedu == 3 & x$momedu == 3)
x$loworigin <- as.numeric(x$dadedu == 1 & x$momedu == 1)

## recoding dad and mother immigration
## Was your father or male guardian born in #CountryName? 1=Yes 2=No
x$dadimmigrant <- recode(x$dadimmigrant,"1=0;2=1")

#x$momimmigrant <- recode(x$momimmigrant,"'2'='1';'1'=0;c('','V','D','R','N')=NA")
#x$momimmigrant <- as.numeric(as.character(x$momimmigrant))

## standardizing cognitive score
#x$cognitive <- scale(x$cognitive)

## recoding gender, 1=men 0=women
x$gender <- as.numeric(x$gender)
x$gender <- recode(x$gender, "2=0")
x <- x[!is.na(x$gender),]

# recoded the education into three levels: low, mid, high
x$eduattain <- recode(x$eduattain, "1:3 = 1; 4:10=2; 11:16=3")

## Recoding the DV(3 levels) into two levels. Comparing the highly educated
## vs the middly and low educated
x$dest56 <- x$eduattain
x$dest56 <- recode(x$dest56, "3=1; c(2,1)=0")

## Creating of highest education indicator
## This evaluates both parents education and takes the highest

x$dadedu[is.na(x$dadedu)] <- 0
x$momedu[is.na(x$momedu)] <- 0

x$highedu <- rowMaxs(as.matrix(x[c("dadedu","momedu")]))

x$dadedu[x$dadedu == 0] <- NA
x$momedu[x$momedu == 0] <- NA
x$highedu[x$highedu == 0] <- NA

x$highisced <- as.numeric(x$highedu == 3)
x$lowisced  <- as.numeric(x$highedu == 1)
x$lowmidisced2 <- as.numeric(x$highedu == 1 | x$highedu == 2) ## For the USA
return(x)
}

usable.country2 <- lapply(countrylist, data.management)

rm(list=ls()[!ls() %in% c("countrylist","usable.country2", "csv.fns","csv.links","csv.page","csv.texts")])


# specify which variables are plausible values (i.e. multiply-imputed)
pvals <- c( 'pvlit' , 'pvnum' , 'pvpsl' )
# loop through each downloadable file..
    for ( i in 1:length(csv.fns) ) {
        
        # create a filename object, containing the lowercase of the csv filename
        fn <- tolower( csv.fns[i] )
        
        # create a design object name, still just a string.
        design.name <- paste0( fn , ".design" )
        
        # convert all column names to lowercase
        names( usable.country2[[i]] ) <- tolower( names( usable.country2[[i]] ) )
        
        
        # paste together all of the plausible value variables with the numbers 1 through 10
        pvars <- outer( pvals , 1:10 , paste0 ) 
        
        # figure out which variables in the `x` data.frame object
        # are not plausible value columns
        non.pvals <- names( usable.country2[[i]] )[ !( names( usable.country2[[i]] ) %in% pvars ) ]
        
        
        
        # loop through each of the ten plausible values..
        for ( k in 1:10 ){
            
            # create a new `y` data.frame object containing only the
            # _current_ plausible value variable (for example: `pvlit4` and `pvnum4` and `pvpsl4`)
            # and also all of the columns that are not plausible value columns
            y <- usable.country2[[i]][ , c( non.pvals , paste0( pvals , k ) ) ]
            
            
            
            # inside of that loop..
            # loop through each of the plausible value variables
            for ( j in pvals ){
                
                # within this specific `y` data.frame object
                
                # get rid of the number on the end, so
                # first copy the `pvlit4` to `pvlit` etc. etc.
                y[ , j ] <- y[ , paste0( j , k ) ]
                
                
                
                # then delete the `pvlit4` variable etc. etc.
                y[ , paste0( j , k ) ] <- NULL
                
                
            }
            
            # save the current `y` data.frame object as `x#` instead.
            assign( paste0( 'usable.country2' , k  ) , y )
            
            # remove `y` from working memory
            rm( y )
            
            # clear up RAM
            gc()
            
        }
        
        # smush all ten of these data.frame objects into one big list object
        w <- list( usable.country21, usable.country22 , usable.country23 , usable.country24 , usable.country25
                 , usable.country26 , usable.country27 , usable.country28 , usable.country29 , usable.country210 )
        
        # remove the originals from memory
        rm( list = paste0( "usable.country2" , 1:10 ) )
        
        # clear up RAM
        gc()
        
        # note: the piaac requires different survey designs for different countries.  quoting their technical documentation:
        # "The variable VEMETHOD denoting whether it is the JK1 or JK2 formula that is applicable to different countries must be in the dataset"
        
        # figure out jackknife method to use from the original `x` data.frame object
        
        # determine the unique values of the `vemethod` column in the current data.frame object
        jk.method <- unique( usable.country2[[i]]$vemethod )
        
        # confirm that they are all the same value.  if there are more than one unique values, this line will crash the program.
        stopifnot( length( jk.method ) == 1 )
        
        # confirm that the jackknife method is one of these.  if it's not, again, crash the program.
        stopifnot( jk.method %in% c( 'JK1' , 'JK2' ) )
        
        # where oecd statisticians say `JK2` the survey package needs a `JKn` instead
        if ( jk.method == 'JK2' ) jk.method <- 'JKn'
        
        # construct the full multiply-imputed, replicate-weighted, complex-sample survey design object
        z <-
            svrepdesign( 	
                weights = ~spfwt0 , 
                repweights = "spfwt[1-9]" ,
                rscales = rep( 1 , 80 ) ,
                scale = ifelse( jk.method == 'JKn' , 1 , 79 / 80 ) ,
                type = jk.method ,
                data = imputationList( w ) ,
                mse = TRUE
            )
        
        # save the originally imported data.frame object `x` to a data.frame named after the original filename
        assign( fn , usable.country2[[i]] )
        
        # save this new survey design object `z` to a survey design named after the original filename
        assign( design.name , z )
        
        # save both objects together into a single `.rda` file
        save( list = c( fn , design.name ) , file = paste0( fn , ".rda" ) )
        
        # now that you've got what you came for, remove everything else from working memory
        rm( list = c( fn , design.name , "w" , "z" ) )
        
        # clear up RAM
        gc()
        
    }

# remove the temporary file - where everything's been downloaded - from the hard disk
file.remove( tf )


# the current working directory should now contain one r data file (.rda)
# for each multiply-imputed, replicate-weighted complex-sample survey design object
# that's one for each available country


# print a reminder: set the directory you just saved everything to as read-only!
message( paste0( "all done.  you should set the directory " , getwd() , " read-only so you don't accidentally alter these tables." ) )
