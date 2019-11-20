#' ---
#' title: "Donor Competition Experiment Dataset"
#' author: "Gento Kato"
#' date: "November 20, 2019"
#' ---


#+ echo=FALSE

## Clear Workspace
rm(list = ls())

## Import Required Package
library(readstata13)
library(rprojroot)
library(knitr)

## Set Working Directory (Automatically) ##
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
#cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)


## Set Original Data Path
doloc <- "../Revised Data/181130 HS180326-6464cases variables for analysis.dta"

## Import Data

# Original
do <- read.dta13(doloc, convert.factors = FALSE)

# Exclude US Treatment Cases
do <- do[do$pat_q08_q11%in% c(1,2,3,5),]

## Initiate New Data

# New Data
d <- data.frame(id = do$record_id)

#' # Meta Data
#' 
#' ## <code>id</code>
#' * *Description*: Respondent ID number
#' * *Values*: Raw values

#+ echo = FALSE
kable(t(summary(d$id)),align="c")

#' # Pre-treatment Covariates
#' 
#' ## <code>issint</code>
#' * *Original*: <code>q01</code>
#' * *Description*: Interests toward international issues
#' * *Values*:
#' 0 Not interested; 
#' 1 Somewhat interested; 
#' 2 Very interested

#+ echo = FALSE
#table(do$q01) # Few "Not Interested At All"
d$issint <- ifelse(do$q01!=9,3-do$q01,NA)
d$issint[d$issint==-1] <- 0
d$issint <- as.factor(d$issint)
kable(t(table(d$issint)),align="c")

#' ## <code>threat.MMR</code>
#' * *Original*: <code>q04_8</code>
#' * *Description*: Threat posed by Myanmar
#' * *Values*:
#' 0 Not threatened; 
#' 1 Neither; 
#' 2 Threatened

#+ echo = FALSE
#table(do$q04_8) # MMR
d$threat.MMR <- as.factor(ifelse(do$q04_8!=9,
                                 ifelse(do$q04_8==3,1,
                                        ifelse(do$q04_8%in%c(4,5),2,0)
                                 ),NA
))
kable(t(table(d$threat.MMR)),align="c")

#' ## <code>threat.PHL</code>
#' * *Original*: <code>q04_3</code>
#' * *Description*: Threat posed by Philippines
#' * *Values*:
#' 0 Not threatened; 
#' 1 Neither; 
#' 2 Threatened

#+ echo = FALSE
#table(do$q04_3) # PHL
d$threat.PHL <- as.factor(ifelse(do$q04_3!=9,
                                 ifelse(do$q04_3==3,1,
                                        ifelse(do$q04_3%in%c(4,5),2,0)
                                 ),NA
))
kable(t(table(d$threat.PHL)),align="c")


#' ## <code>imp.MMR</code>
#' * *Original*: <code>q05_8</code>
#' * *Description*: Importance of Myanmar for Japan
#' * *Values*:
#' 0 Not important; 
#' 1 Neither; 
#' 2 Important

#+ echo = FALSE
#table(do$q05_8) # MMR
d$imp.MMR <- as.factor(ifelse(do$q05_8!=9,
                              ifelse(do$q05_8==3,1,
                                     ifelse(do$q05_8%in%c(4,5),2,0)
                              ),NA
))
kable(t(table(d$imp.MMR)),align="c")

#' ## <code>imp.PHL</code>
#' * *Original*: <code>q05_3</code>
#' * *Description*: Importance of Philippines for Japan
#' * *Values*:
#' 0 Not important; 
#' 1 Neither; 
#' 2 Important

#+ echo = FALSE
#table(do$q05_3) # PHL
d$imp.PHL <- as.factor(ifelse(do$q05_3!=9,
                              ifelse(do$q05_3==3,1,
                                     ifelse(do$q05_3%in%c(4,5),2,0)
                              ),NA
))
kable(t(table(d$imp.PHL)),align="c")

#' ## <code>odaimp</code>
#' * *Original*: <code>q06</code>
#' * *Description*: Importance of ODA
#' * *Values*:
#' 0 Neither/Not important; 
#' 1 Important

#+ echo=FALSE
## ODA Importance (1=Important, 0=Not)
#table(do$q06)
d$odaimp <- ifelse(do$q06!=9,ifelse(do$q06%in%c(1,2,3),1,0),NA)
kable(t(table(d$odaimp)),align="c")

#' ## <code>potential.MMR</code>
#' * *Original*: <code>q07_5</code>
#' * *Description*: Future economic potential of Myanmar for Japan
#' * *Values*: ; 
#' 0 Low ; 
#' 1 Moderate/DK; 
#' 2 High

#+ echo = FALSE
table(do$q07_5) # MMR
d$potential.MMR <- as.factor(ifelse(do$q07_5!=9,
                                    ifelse(do$q07_5%in%c(3,7),1,
                                           ifelse(do$q07_5%in%c(4,5),2,0)
                                    ),NA
))
kable(t(table(d$potential.MMR)),align="c")

#' ## <code>potential.PHL</code>
#' * *Original*: <code>q07_3</code>
#' * *Description*: Future economic potential of Philippines for Japan
#' * *Values*:
#' 0 Low; 
#' 1 Moderate/DK; 
#' 2 High

#+ echo = FALSE
#table(do$q07_3) # PHL
d$potential.PHL <- as.factor(ifelse(do$q07_3!=9,
                                    ifelse(do$q07_3%in%c(3,7),1,
                                           ifelse(do$q07_3%in%c(4,5),2,0)
                                    ),NA
))
kable(t(table(d$potential.PHL)),align="c")

#' # Pre-treatment Moderator
#' 
#' ## <code>threat.CHN</code>
#' * *Original*: <code>q04_2</code>
#' * *Description*: Threat posed by China (Binary)
#' * *Values*: 
#' 0 Neither/Not Threatened; 
#' 1 Threatened

#+ echo = FALSE
# Moderator
## China Threat Perception (Binary)
#table(do$q04_2) # CHN
d$threat.CHN <- ifelse(do$q04_2!=9,ifelse(do$q04_2 %in% c(4,5),1,0),NA)
kable(t(table(d$threat.CHN)),align="c")

#' ## <code>threat.CHN.3cat</code>
#' * *Original*: <code>q04_2</code>
#' * *Description*: Threat posed by China (3 categories)
#' * *Values*: 
#' 0 Neither/Not Threatened; 
#' 1 Somewhat Threatened; 
#' 2 Highly Threatened

#+ echo = FALSE
## China Threat Perception (3 Category)
#table(do$q04_2) # CHN
d$threat.CHN.3cat <- d$threat.CHN
d$threat.CHN.3cat[do$q04_2==5] <- 2
d$threat.CHN.3cat <- as.factor(d$threat.CHN.3cat)
kable(t(table(d$threat.CHN)),align="c")

#' # Treatment Variables
#' 
#' ## <code>treatment</code>
#' * *Original*: <code>pat_q08_q11</code>
#' * *Description*: Treatment ID Number 
#' * *Values*: Raw values

#+ echo = FALSE
d$treatment <- do$pat_q08_q11
kable(t(table(d$treatment)),align="c")

#' ## <code>treat_China</code>
#' * *Original*: <code>pat_q08_q11</code>
#' * *Description*: Treatment (China as the alternative donor) 
#' * *Values*: 1 Treated; 0 Control

#+ echo = FALSE
d$treat_China <- ifelse(do$pat_q08_q11 %in% c(2,3,4),1,0)
kable(t(table(d$treat_China)),align="c")

#' ## <code>treat_MMR</code>
#' * *Original*: <code>pat_q08_q11</code>
#' * *Description*: Recipient country (Myanmar) 
#' * *Values*: 1 Myanmar is recipient; 0 Philippines is recipient

#+ echo = FALSE
d$treat_MMR <- ifelse(do$pat_q08_q11 %in% c(1,2),1,0)   
kable(t(table(d$treat_MMR)),align="c")

#' ## <code>treat_PHL</code>
#' * *Original*: <code>pat_q08_q11</code>
#' * *Description*: Recipient country (Philippines) 
#' * *Values*: 1 Philippines is recipient; 0 Myanmar is recipient 

#+ echo = FALSE
d$treat_PHL <- ifelse(do$pat_q08_q11 %in% c(3,4,5,6),1,0)   
kable(t(table(d$treat_PHL)),align="c")

#' # Mediator Variables
#' 
#' ## <code>med_econ</code>
#' * *Original*: <code>q09a</code>
#' * *Description*: Damage to national economic interests 
#' * *Values*: 
#' 1 Promote interests greatly; 
#' 2 Promote interests a little; 
#' 3 No change; 
#' 4 Damage interests a little; 
#' 5 Damage interests greatly

#+ echo = FALSE
d$med_econ <- as.factor(ifelse(do$q09a!=9,6 - do$q09a,NA)) 
kable(t(table(d$med_econ)),align="c")

#' ## <code>med_secu</code>
#' * *Original*: <code>q09b</code>
#' * *Description*: Damage to national security interests 
#' * *Values*: 
#' 1 Promote interests greatly; 
#' 2 Promote interests a little; 
#' 3 No change; 
#' 4 Damage interests a little; 
#' 5 Damage interests greatly

#+ echo = FALSE
d$med_secu <- as.factor(ifelse(do$q09b!=9,6 - do$q09b,NA)) 
kable(t(table(d$med_secu)),align="c")

#' ## <code>med_repu</code>
#' * *Original*: <code>q09c</code>
#' * *Description*: Damage to international reputation 
#' * *Values*:
#' 1 Improve greatly; 
#' 2 Improve a little; 
#' 3 No change; 
#' 4 Worsen a little; 
#' 5 Worsen greatly

#+ echo = FALSE
d$med_repu <- as.factor(ifelse(do$q09c!=9,6 - do$q09c,NA)) 
kable(t(table(d$med_repu)),align="c")

#' ## <code>med_effi</code>
#' * *Original*: <code>q09d</code>
#' * *Description*: Effectiveness of aid cancellation to reduce human rights violations 
#' * *Values*: 
#' 1 Reduce violations greatly; 
#' 2 Reduce violations a little; 
#' 3 No change; 
#' 4 Increase violations a little; 
#' 5 Increase violations greatly

#+ echo = FALSE
d$med_effi <- as.factor(ifelse(do$q09d!=9,do$q09d,NA))
kable(t(table(d$med_effi)),align="c")

#' # Outcome Variable
#' 
#' ## <code>cancel_aid</code>
#' * *Original*: <code>q10</code>, <code>q10sq1</code>, <code>q10sq2</code>
#' * *Description*: Opinion on aid cancellation (9p scale)
#' * *Values*: 
#' 1 Should not cancel (very strong); 
#' 2 Should not cancel (somewhat strong); 
#' 3 Should not cancel (not so strong); 
#' 4 Should not cancel, if forced (after probed); 
#' 5 Difficult to say, even if forced (after probed); 
#' 6 Should cancel, if forced (after probed); 
#' 7 Should cancel (not so strong); 
#' 8 Should cancel (somewhat strong); 
#' 9 Should cancel (very strong)

#+ echo = FALSE
# Outcome (9pt scale) ^cancel
d$cancel_aid <- ifelse(do$q10==1,
                       ifelse(do$q10sq1!=9,10-do$q10sq1,NA),
                       ifelse(do$q10==2,
                              ifelse(do$q10sq2==3,5,
                                     ifelse(do$q10sq2!=9,(1.5-do$q10sq2)*2 + 5,NA)
                              ),
                              ifelse(do$q10==3,
                                     ifelse(do$q10sq3!=9,4-do$q10sq3,NA),NA
                              )
                       )
)
d$cancel_aid <- as.factor(d$cancel_aid)
kable(t(table(d$cancel_aid)),align="c")

#' ## <code>cancel_aid_3cat</code>
#' * *Original*: <code>q10</code>
#' * *Description*: Opinion on aid cancellation (3p scale) 
#' * *Values*: 
#' 1 Should not cancel; 
#' 2 Difficult to say; 
#' 3 Should cancel

#+ echo = FALSE
# Outcome (Three Category Scale)
d$cancel_aid_3cat <- as.factor(ifelse(do$q10!=9,4-do$q10,NA))
#d$cancel_aid_3cat[do$q10sq2==1] <- 3
#d$cancel_aid_3cat[do$q10sq2==2] <- 1
kable(t(table(d$cancel_aid_3cat)),align="c")

#' ## <code>cancel_aid_2cat</code>
#' * *Original*: <code>q10</code>
#' * *Description*: Opinion on aid cancellation (binary) 
#' * *Values*: ; 
#' 0 Should not cancel; 
#' 1 Difficult to say/Should cancel

#+ echo = FALSE
# Outcome (Binary Scale. Cancel/DK=1, Keep=0)
d$cancel_aid_2cat <- ifelse(do$q10!=9,ifelse(do$q10==3,0,1),NA)
#d$cancel_aid_2cat[do$q10sq2==2] <- 0
kable(t(table(d$cancel_aid_2cat)),align="c")

#' # Demographic and Ideology Variables
#' 
#' ## <code>fem</code>
#' * *Original*: <code>f01</code>
#' * *Description*: Respondent gender 
#' * *Values*: 0 Male; 1 Female

#+ echo = FALSE
## Gender (Female)
#table(do$f01)
d$fem <- ifelse(do$f01==2,1,0)
kable(t(table(d$fem)),align="c")

#' ## <code>age</code>
#' * *Original*: <code>f02</code>
#' * *Description*: Respondent age 
#' * *Values*: Raw values

#+ echo = FALSE
## Age
d$age <- do$f02
kable(t(summary(d$age)),align="c")

## Education (Turns Out Not to be Related with Mediators)
#table(do$f04)
#d$edu <- as.factor(ifelse(do$f04==1,1,do$f04-1))
#table(d$edu)

#' ## <code>right</code>
#' * *Original*: <code>q25</code>
#' * *Description*: Right-wing ideology (binary) 
#' * *Values*: 1 Right-wing; 0 Not

#+ echo = FALSE
## Right-wing Ideology (Binary)
#table(do$q25)
d$right <- ifelse(do$q25==99,NA,ifelse(do$q25%in%c(6,7,8,9,10),1,0))
kable(t(table(d$right)),align="c")

#' ## <code>ide3</code>
#' * *Original*: <code>q25</code>
#' * *Description*: Right-wing ideology (3 categories) 
#' * *Values*:
#' 0 Left; 
#' 1 Moderate; 
#' 2 Right

#+ echo = FALSE
## Right-wing Ideology (3 Category)
#table(do$q25)
d$ide3 <- d$right*2
d$ide3[do$q25==5] <- 1
d$ide3 <- as.factor(d$ide3)
kable(t(table(d$ide3)),align="c")

#' # Compliers
#' 
#' ## <code>comply</code>
#' * *Original*: <code>q12</code>, <code>q24</code>
#' * *Description*: Compliers and Non-compliers. 
#' * *Values*: 1 Compliers; Not-1 Non-compliers

#+ echo = FALSE
d$comply <- do$comply
kable(t(table(d$comply)),align="c")

# Drop Cases with missing values
#d <- d[do$comply==1,]

#' # Save Data
#' 
#' The contents of following two files are identical.
#' 
#' ## <code>data/donorexp.csv</code>
#' 
#' CSV file.
#' 
#' ## <code>data/donorexp.rds</code>
#' 
#' RDS file (R data format). This file is used for the analysis.
#' 
#+ eval=FALSE, echo=FALSE
# In CSV
write.csv(d, file = "data/donorexp.csv", row.names = FALSE)
# in RDS (This is used for analysis)
saveRDS(d, "data/donorexp.rds")

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('src/data_recode.R', 'pdf_document', encoding = 'UTF-8')
# rmarkdown::render('src/data_recode.R', 'github_document', clean=FALSE, encoding = 'UTF-8')
# In Terminal, run:
# Rscript -e "rmarkdown::render('src/data_recode.R', 'pdf_document', encoding = 'UTF-8')"
# Rscript -e "rmarkdown::render('src/data_recode.R', 'github_document', clean=FALSE, encoding = 'UTF-8')"
