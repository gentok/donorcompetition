# Donor Competition and Public Support for Foreign Aid Sanctions

[Working paper (2/1/2019)](paper/DonorCompetition_v2.4.pdf)

### Authors 
Masaru Kohno, Gabriella Montinola, Matthew Winters, and Gento Kato

### Abstract
Are there scenarios in which the public will not support foreign aid sanctions against aid-receiving states engaged in human rights violations? We argue that public opinion on aid sanctions can vary with the international environment. Specifically, we examine whether the threat of competition with others donors for influence in an aid-receiving country affects public support in a donor country for aid sanctions against a repressive regime. In studying how voters consider the international environment, we speak to general theories about how the public views foreign aid policy. In particular, we study how support for aid sanctions varies within a sample of Japanese adults.  We find that information about the possibility that another donor will substitute for Japanese aid slightly reduces support for sanctions.  We provide evidence that this effect runs through a pathway where donor competition triggers concerns for Japanese security.  We also find that the effect is larger among those who have pre-existing concerns about the other donor.

### Dataset

* [Questionnaire](data/Questionnaire_English.pdf). Questionaire (translated to English) of experiment. The dataset contains only converted variables, so check Codebook for practical use.

* [Codebook (4/12/2019)](src/data_recode.md). Also in [PDF](src/data_recode.pdf). The R code used to transform variables can be checked [here](src/data_recode.R).

* Dataset in CSV format. Run in R: <br> <code>d <- read.csv("https://raw.githubusercontent.com/gentok/donorexp/master/data/donorexp.csv")</code> <br> (*This works after the repository becoming public*)

* Dataset in RDS format. Run in R: <br> <code>d <- readRDS("https://raw.githubusercontent.com/gentok/donorexp/master/data/donorexp.rds")</code> <br> (*This works after the repository becoming public*)

### Code Files (in R)

To replicate analysis, clone (or download and extract the zip folder of) repository and run each file (working directory will be automatically set for most of editors and order does not matter):

* <code>**analysis0_functions.R**</code> ([Code](src/analysis0_functions.R)). Contains functions and generic objects used in the analysis. Sourced in analysis files. Make sure to install R packages required for this code.
* <code>**cl.mlogit.R**</code> ([Code](src/cl.mlogit.R)). Supplemental codes to calculate robust standard errors of Multionomial Logit.
* <code>**analysis1_descriptive.R**</code> ([Code](src/analysis1_descriptive.R); [Code/Output](src/analysis1_descriptive.md)). Exports following descriptive statistics under <code>out</code> directory:
    * [<code>outdist.png</code>](out/outdist.png). The distribution of outcome variable (aid cancellation)
    * [<code>perrecip.png</code>](out/perrecip.png). The distribution of selected pre-treatment covariates (threat, importance and potential perception of recipients).
    * [<code>threatCHN.png</code>](out/threatCHN.png). The distribution of moderator (threat perception of China).
    * [<code>medoutrel.png</code>](out/medoutrel.png). The relationship between mediators and outcome.
    * [<code>balance_ALL.png</code>](out/balance_ALL.png). Covariate balance between treatment and control groups.
* <code>**analysis2_ate.R**</code> ([Code](src/analysis2_ate.R); [Code/Output](src/analysis2_ate.md)). Contains the analysis of average treatment effect (ATE). Exports following results under <code>out</code> directory:
    * [<code>olsres.doc</code>](out/olsres.doc). OLS regression table of ATE.
    * [<code>olsplot.png</code>](out/olsplot.png). OLS regression coefficients plot of ATE.
    * [<code>olsres_std.doc</code>](out/olsres_std.doc). OLS regression table of ATE, with outcome variable scale standardized by control group SD.
    * [<code>olsplot_std.png</code>](out/olsplot_std.png). OLS regression coefficients plot of ATE, with outcome variable scale standardized by control group SD.
    * [<code>mlogitres.doc</code>](out/mlogitres.doc). Multinomial Logit coefficients table, with three category outcome variable ("Continue","Neither","Cancel" with "Continue" as the reference category).
    * [<code>mlogitres_onlyATE.doc</code>](out/mlogitres_onlyATE.doc). Multinomial Logit coefficients table, only showing coefficients for treatment variable.
    * [<code>mlogitplot.png</code>](out/mlogitplot.png). Predicted probabilities of outcome variable responses in treatment and control group, simulated by using results of Multinomial Logit.
    * [<code>olsres_secu.doc</code>](out/olsres_secu.doc). OLS regression table of treatment effect on mediator (damage to national security, 5p scale).
    * [<code>olsres_econ.doc</code>](out/olsres_econ.doc). OLS regression table of treatment effect on mediator (damage to economy, 5p scale).
    * [<code>olsres_repu.doc</code>](out/olsres_repu.doc). OLS regression table of treatment effect on mediator (damage to international reputation, 5p scale).
    * [<code>olsres_effi.doc</code>](out/olsres_effi.doc). OLS regression table of treatment effect on mediator (aid un-efficacy, 5p scale).
    * [<code>dvmedplot_ols.png</code>](out/dvmedplot_ols.png). Summarized coefficient plot of treatment effect on mediators in 5p scale.
    * [<code>logitres_secu.doc</code>](out/logitres_secu.doc). Logistic regression table of treatment effect on mediator (damage to national security, 2p scale).
    * [<code>logitres_econ.doc</code>](out/logitres_econ.doc). Logistic regression table of treatment effect on mediator (damage to economy, 2p scale).
    * [<code>logitres_repu.doc</code>](out/logitres_repu.doc). Logistic regression table of treatment effect on mediator (damage to international reputation, 2p scale).
    * [<code>logitres_effi.doc</code>](out/logitres_effi.doc). Logistic regression table of treatment effect on mediator (aid un-efficacy, 2p scale).
    * [<code>dvmedplot_logit.png</code>](out/dvmedplot_logit.png). Summarized odds ratio plot of treatment effect on mediators in 2p scale.
* <code>**analysis3_mediation.R**</code> ([Code](src/analysis3_mediation.R); [Code/Output](src/analysis3_mediation.md)). Contains the analysis of causal mediation analysis. Exports following results under <code>out</code> directory:
    * [<code>med.out.main.plot.png</code>](out/med.out.main.plot.png). Causal mediation analysis plot with 2p mediators (Logit) and 9p outcome (OLS).
    * [<code>med.out.sub.plot.png</code>](out/med.out.main.plot.png). Causal mediation analysis plot with 5p mediators (OLS) and 9p outcome (OLS).
    * [<code>med.mod.out.main.1.plot.png</code>](out/med.mod.out.main.1.plot.png). Moderated causal mediation analysis plot with 2p moderator, 2p mediators (Logit) and 9p outcome (OLS).
    * [<code>med.mod.out.main.1.secu.wtotal.plot.png</code>](out/med.mod.out.main.1.secu.wtotal.plot.png). Moderated causal mediation analysis plot with 2p moderator, 2p mediators (Logit) and 9p outcome (OLS). Only showing results for national security mediator, including total treatment effect.
    * [<code>med.mod.out.main.1.secu.wototal.plot.png</code>](out/med.mod.out.main.1.secu.wototal.plot.png). Moderated causal mediation analysis plot with 2p moderator, 2p mediators (Logit) and 9p outcome (OLS). Only showing results for national security mediator, excluding total treatment effect.
    * [<code>med.mod.out.sub.1.plot.png</code>](out/med.mod.out.sub.1.plot.png). Moderated causal mediation analysis plot with 2p moderator, 5p mediators (OLS) and 9p outcome (OLS).
    * [<code>med.mod.out.sub.1.secu.wtotal.plot.png</code>](out/med.mod.out.sub.1.secu.wtotal.plot.png). Moderated causal mediation analysis plot with 2p moderator, 5p mediators (OLS) and 9p outcome (OLS). Only showing results for national security mediator, including total treatment effect.
    * [<code>med.mod.out.sub.1.secu.wototal.plot.png</code>](out/med.mod.out.sub.1.secu.wototal.plot.png). Moderated causal mediation analysis plot with 2p moderator, 5p mediators (OLS) and 9p outcome (OLS). Only showing results for national security mediator, excluding total treatment effect.
    * [<code>med.mod.out.main.3.plot.png</code>](out/med.mod.out.main.3.plot.png). Moderated causal mediation analysis plot with 3p moderator, 2p mediators (Logit) and 9p outcome (OLS).
    * [<code>med.mod.out.main.3.secu.wtotal.plot.png</code>](out/med.mod.out.main.3.secu.wtotal.plot.png). Moderated causal mediation analysis plot with 3p moderator, 2p mediators (Logit) and 9p outcome (OLS). Only showing results for national security mediator, including total treatment effect.
    * [<code>med.mod.out.main.3.secu.wototal.plot.png</code>](out/med.mod.out.main.3.secu.wototal.plot.png). Moderated causal mediation analysis plot with 3p moderator, 2p mediators (Logit) and 9p outcome (OLS). Only showing results for national security mediator, excluding total treatment effect.
    * [<code>med.mod.out.sub.3.plot.png</code>](out/med.mod.out.sub.3.plot.png). Moderated causal mediation analysis plot with 3p moderator, 5p mediators (OLS) and 9p outcome (OLS).
    * [<code>med.mod.out.sub.3.secu.wtotal.plot.png</code>](out/med.mod.out.sub.3.secu.wtotal.plot.png). Moderated causal mediation analysis plot with 3p moderator, 5p mediators (OLS) and 9p outcome (OLS). Only showing results for national security mediator, including total treatment effect.
    * [<code>med.mod.out.sub.3.secu.wototal.plot.png</code>](out/med.mod.out.sub.3.secu.wototal.plot.png). Moderated causal mediation analysis plot with 3p moderator, 5p mediators (OLS) and 9p outcome (OLS). Only showing results for national security mediator, excluding total treatment effect.

### Project Structure

* <code>src</code>: Contains code files and their direct ouputs.
* <code>src/processing</code>: Currently empty. When you run analysis codes, this folder stores temporary files used during the analysis.
* <code>data</code>: Contains datasets and questionnaire.
* <code>out</code>: Contains output plots and tables.
* <code>paper</code>: Contains working paper.