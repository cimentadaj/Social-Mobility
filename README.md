# IMPORTANT

These are the replication files to reproduce the paper 'Esping-Andersen, G. & Cimentada, J. (2018).  Ability and mobility:  The relative influence of skills and social origin on social mobility.  Social Science Research, Volume 75, Pages 13-31.'. It can be accessed freely [here](https://www.researchgate.net/publication/325919720_Ability_and_mobility_The_relative_influence_of_skills_and_social_origin_on_social_mobility)

Below is a description of each of the files used to reproduce the paper.

1. Set your R working directory to the root of this repository.
2. Open the script `./download_data.R` and install the packages that are loaded at the beginning.
3. Run `download_data.R`, which downloads all PIAAC data automatically from the web to the folder `./data/` , which is created automatically. Note: this script was adapted from this [script](https://github.com/ajdamico/asdfree/blob/master/Programme%20for%20the%20International%20Assessment%20of%20Adult%20Competencies/download%20import%20and%20design.R) by [@adjdamico](https://github.com/ajdamico)
4. Open the script `./analysis_1.R` and install the packages that are loaded at the beginning. Be sure to install them properly because some are not easy to install such as `ReporteRs`.
5. Run `./analysis_1.R`. This file will produce descriptives 1-2, table 1-5 and figure 1-2
6. Install all packages from script `./analysis_2.R` and run it to get figures 3-5. Read the top of the script because there is things you need to change to get all combinations of the graphs.
7. Install all packages from script `./analysis_3.R` and run it to get tables 7-9.

All tables (saved as `.docx`) and plots will be in the `./Tables` folder.

DISCLAIMER: This was our first project in R and the code is really messy. Hopefully anyone who is interested can still reproduce this. We are sorry for the messy code.