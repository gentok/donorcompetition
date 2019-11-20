# Donor Competition and Public Support for Foreign Aid Sanctions

<!--
[Working paper (6/4/2019)](paper/DonorCompetition_v5-2.pdf)
-->

### Authors 
Masaru Kohno, Gabriella Montinola, Matthew Winters, and Gento Kato

### Abstract
Previous research suggests that ideology, material interests, and moral values drive citizens� preferences over foreign aid policy. Little attention has been paid to how perceptions of the international environment affects these preferences. We examine the extent to which citizens in a traditional donor country consider donor competition when deciding whether to impose aid sanctions on governments engaged in human rights violations. Employing an information experiment conducted among Japanese adults, we find that the prospect of another donor ready to act as a substitute aid-provider reduces support for the use of aid sanctions. This effect runs most strongly through a pathway privileging security concerns, and the effect is larger among respondents who have pre-existing concerns about the other donor. These results highlight the way in which public desires for foreign aid to bring about material returns can hinder a government�s ability to use aid to promote good governance ends.

### Dataset

* [Questionnaire](data/PQR_Questionnaire_Eng_Jpn.pdf). Questionnaire (Original Japanese and English Translation) of experiment. The dataset contains only converted variables, so check Codebook for practical use.

* [Codebook (4/12/2019)](src/data_recode.md). Also in [PDF](src/data_recode.pdf). The R code used to transform variables can be checked [here](src/data_recode.R).

* Dataset in CSV format. Run in R: <br> <code>d <- read.csv("https://raw.githubusercontent.com/gentok/donorexp/master/data/donorexp.csv")</code> <br>

* Dataset in RDS format. Run in R: <br> <code>d <- readRDS("https://raw.githubusercontent.com/gentok/donorexp/master/data/donorexp.rds")</code> <br>

### Code Files (in R)

To replicate analysis, clone (or download and extract the zip folder of) repository and run each file (working directory will be automatically set for most of editors and order does not matter):

* <code>**analysis0_functions.R**</code> ([Code](src/analysis0_functions.R)). Contains functions and generic objects used in the analysis. Sourced in analysis files. Make sure to install R packages required for this code.
* <code>**cl.mlogit.R**</code> ([Code](src/cl.mlogit.R)). Supplemental codes to calculate robust standard errors for Multionomial Logit.
* <code>**analysis1_descriptive.R**</code> ([Code](src/analysis1_descriptive.R); [Code/Output](src/analysis1_descriptive.md)). Exports following descriptive statistics under <code>out</code> directory:
    * [<code>outdist.png</code>](out/outdist.png). FIGURE 2: The distribution of outcome variable (aid cancellation)
    * [<code>perrecip.png</code>](out/perrecip.png). The distribution of selected pre-treatment covariates (threat, importance and potential perception of recipients).
    * [<code>threatCHN.png</code>](out/threatCHN.png). FIGURE 6: The distribution of moderator (threat perception of China).
    * [<code>meddist.png</code>](out/meddist.png). FIGURE 4: Distributions of mediators.
    * [<code>medoutrel.png</code>](out/medoutrel.png). The relationship between mediators and outcome.
    * [<code>balance_ALL.png</code>](out/balance_ALL.png). APPENDIX I: Covariate balance between treatment and control groups.
* <code>**analysis2_ate.R**</code> ([Code](src/analysis2_ate.R); [Code/Output](src/analysis2_ate.md)). Contains the analysis of average treatment effect (ATE). Exports following results under <code>out</code> directory:
    * [<code>olsres.doc</code>](out/olsres.doc). APPENDIX III: OLS regression table of ATE.
    * [<code>olsplot.png</code>](out/olsplot.png). FIGURE 3: OLS regression coefficients plot of ATE.
    * [<code>mlogitres.doc</code>](out/mlogitres.doc). Multinomial Logit coefficients table, with three category outcome variable ("Continue","Neither","Cancel" with "Continue" as the reference category).
    * [<code>mlogitres_onlyATE.doc</code>](out/mlogitres_onlyATE.doc). Multinomial Logit coefficients table, only showing coefficients for treatment variable.
    * [<code>mlogitplot.png</code>](out/mlogitplot.png). APPENDIX II: Predicted probabilities of outcome variable responses in treatment and control group, simulated by using results of Multinomial Logit.
* <code>**analysis3_mediation.R**</code> ([Code](src/analysis3_mediation.R); [Code/Output](src/analysis3_mediation.md)). Contains the analysis of causal mediation analysis implemented by <code>mediation</code> package. Exports following results under <code>out</code> directory:
    * [<code>med.out.main.plot.woDE.png</code>](out/med.out.main.plot.woDE.png). FIGURE 5: Causal mediation analysis plot with 2p mediators (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.out.main.plot.wDE.png</code>](out/med.out.main.plot.wDE.png) for direct effects.
    * [<code>med.out.sub.plot.woDE.png</code>](out/med.out.sub.plot.png). APPENDIX V: Causal mediation analysis plot with 5p mediators (OLS) and 9p outcome (OLS). Without direct effects. Check [<code>med.out.sub.plot.wDE.png</code>](out/med.out.sub.plot.wDE.png) for direct effects.
    * [<code>med.mod.out.main.1.secu.plot.woDE.png</code>](out/med.mod.out.main.1.secu.plot.woDE.png). Moderated causal mediation analysis plot with 2p moderator, 2p security mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.main.1.secu.plot.wDE.png</code>](out/med.mod.out.main.1.secu.plot.wDE.png) for direct effects; [<code>med.mod.out.main.1.secu.plot.wTE.png</code>](out/med.mod.out.main.1.secu.plot.wTE.png) for total effects.
    * [<code>med.mod.out.main.1.econ.plot.woDE.png</code>](out/med.mod.out.main.1.econ.plot.woDE.png). Moderated causal mediation analysis plot with 2p moderator, 2p economy mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.main.1.econ.plot.wDE.png</code>](out/med.mod.out.main.1.secu.plot.wDE.png) for direct effects; [<code>med.mod.out.main.1.econ.plot.wTE.png</code>](out/med.mod.out.main.1.econ.plot.wTE.png) for total effects.
    * [<code>med.mod.out.main.1.repu.plot.woDE.png</code>](out/med.mod.out.main.1.repu.plot.woDE.png). Moderated causal mediation analysis plot with 2p moderator, 2p reputation mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.main.1.repu.plot.wDE.png</code>](out/med.mod.out.main.1.repu.plot.wDE.png) for direct effects; [<code>med.mod.out.main.1.repu.plot.wTE.png</code>](out/med.mod.out.main.1.repu.plot.wTE.png) for total effects.
    * [<code>med.mod.out.main.1.effi.plot.woDE.png</code>](out/med.mod.out.main.1.effi.plot.woDE.png). Moderated causal mediation analysis plot with 2p moderator, 2p efficacy mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.main.1.effi.plot.wDE.png</code>](out/med.mod.out.main.1.effi.plot.wDE.png) for direct effects; [<code>med.mod.out.main.1.effi.plot.wTE.png</code>](out/med.mod.out.main.1.effi.plot.wTE.png) for total effects.
    * [<code>med.mod.out.sub.1.secu.plot.woDE.png</code>](out/med.mod.out.sub.1.secu.plot.woDE.png). Moderated causal mediation analysis plot with 2p moderator, 5p security mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.sub.1.secu.plot.wDE.png</code>](out/med.mod.out.sub.1.secu.plot.wDE.png) for direct effects; [<code>med.mod.out.sub.1.secu.plot.wTE.png</code>](out/med.mod.out.sub.1.secu.plot.wTE.png) for total effects.
    * [<code>med.mod.out.sub.1.econ.plot.woDE.png</code>](out/med.mod.out.sub.1.econ.plot.woDE.png). Moderated causal mediation analysis plot with 2p moderator, 5p economy mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.sub.1.econ.plot.wDE.png</code>](out/med.mod.out.sub.1.secu.plot.wDE.png) for direct effects; [<code>med.mod.out.sub.1.econ.plot.wTE.png</code>](out/med.mod.out.sub.1.econ.plot.wTE.png) for total effects.
    * [<code>med.mod.out.sub.1.repu.plot.woDE.png</code>](out/med.mod.out.sub.1.repu.plot.woDE.png). Moderated causal mediation analysis plot with 2p moderator, 5p reputation mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.sub.1.repu.plot.wDE.png</code>](out/med.mod.out.sub.1.repu.plot.wDE.png) for direct effects; [<code>med.mod.out.sub.1.repu.plot.wTE.png</code>](out/med.mod.out.sub.1.repu.plot.wTE.png) for total effects.
    * [<code>med.mod.out.sub.1.effi.plot.woDE.png</code>](out/med.mod.out.sub.1.effi.plot.woDE.png). Moderated causal mediation analysis plot with 2p moderator, 5p efficacy mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.sub.1.effi.plot.wDE.png</code>](out/med.mod.out.sub.1.effi.plot.wDE.png) for direct effects; [<code>med.mod.out.sub.1.effi.plot.wTE.png</code>](out/med.mod.out.sub.1.effi.plot.wTE.png) for total effects.  * [<code>med.mod.out.main.3.secu.plot.woDE.png</code>](out/med.mod.out.main.3.secu.plot.woDE.png). FIGURE 7: Moderated causal mediation analysis plot with 3p moderator, 2p security mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.main.3.secu.plot.wDE.png</code>](out/med.mod.out.main.3.secu.plot.wDE.png) for direct effects; [<code>med.mod.out.main.3.secu.plot.wTE.png</code>](out/med.mod.out.main.3.secu.plot.wTE.png) for total effects.
    * [<code>med.mod.out.main.3.econ.plot.woDE.png</code>](out/med.mod.out.main.3.econ.plot.woDE.png). Moderated causal mediation analysis plot with 3p moderator, 2p economy mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.main.3.econ.plot.wDE.png</code>](out/med.mod.out.main.3.secu.plot.wDE.png) for direct effects; [<code>med.mod.out.main.3.econ.plot.wTE.png</code>](out/med.mod.out.main.3.econ.plot.wTE.png) for total effects.
    * [<code>med.mod.out.main.3.repu.plot.woDE.png</code>](out/med.mod.out.main.3.repu.plot.woDE.png). Moderated causal mediation analysis plot with 3p moderator, 2p reputation mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.main.3.repu.plot.wDE.png</code>](out/med.mod.out.main.3.repu.plot.wDE.png) for direct effects; [<code>med.mod.out.main.3.repu.plot.wTE.png</code>](out/med.mod.out.main.3.repu.plot.wTE.png) for total effects.
    * [<code>med.mod.out.main.3.effi.plot.woDE.png</code>](out/med.mod.out.main.3.effi.plot.woDE.png). Moderated causal mediation analysis plot with 3p moderator, 2p efficacy mediator (Logit) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.main.3.effi.plot.wDE.png</code>](out/med.mod.out.main.3.effi.plot.wDE.png) for direct effects; [<code>med.mod.out.main.3.effi.plot.wTE.png</code>](out/med.mod.out.main.3.effi.plot.wTE.png) for total effects.
    * [<code>med.mod.out.sub.3.secu.plot.woDE.png</code>](out/med.mod.out.sub.3.secu.plot.woDE.png). APPENDIX VII: Moderated causal mediation analysis plot with 3p moderator, 5p security mediator (OLS) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.sub.3.secu.plot.wDE.png</code>](out/med.mod.out.sub.3.secu.plot.wDE.png) for direct effects; [<code>med.mod.out.sub.3.secu.plot.wTE.png</code>](out/med.mod.out.sub.3.secu.plot.wTE.png) for total effects.
    * [<code>med.mod.out.sub.3.econ.plot.woDE.png</code>](out/med.mod.out.sub.3.econ.plot.woDE.png). Moderated causal mediation analysis plot with 3p moderator, 5p economy mediator (OLS) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.sub.3.econ.plot.wDE.png</code>](out/med.mod.out.sub.3.secu.plot.wDE.png) for direct effects; [<code>med.mod.out.sub.3.econ.plot.wTE.png</code>](out/med.mod.out.sub.3.econ.plot.wTE.png) for total effects.
    * [<code>med.mod.out.sub.3.repu.plot.woDE.png</code>](out/med.mod.out.sub.3.repu.plot.woDE.png). Moderated causal mediation analysis plot with 3p moderator, 5p reputation mediator (OLS) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.sub.3.repu.plot.wDE.png</code>](out/med.mod.out.sub.3.repu.plot.wDE.png) for direct effects; [<code>med.mod.out.sub.3.repu.plot.wTE.png</code>](out/med.mod.out.sub.3.repu.plot.wTE.png) for total effects.
    * [<code>med.mod.out.sub.3.effi.plot.woDE.png</code>](out/med.mod.out.sub.3.effi.plot.woDE.png). Moderated causal mediation analysis plot with 3p moderator, 5p efficacy mediator (OLS) and 9p outcome (OLS). Without direct effects. Check [<code>med.mod.out.sub.3.effi.plot.wDE.png</code>](out/med.mod.out.sub.3.effi.plot.wDE.png) for direct effects; [<code>med.mod.out.sub.3.effi.plot.wTE.png</code>](out/med.mod.out.sub.3.effi.plot.wTE.png) for total effects.
* <code>**analysis4_medflex.R**</code> ([Code](src/analysis4_medflex.R); [Code/Output](src/analysis4_medflex.md)). Contains the analysis of causal mediation analysis implemented by imputation-based approach in <code>medflex</code> package. This package allows estimation of joint causal mediation effect (which is not possible in <code>mediation</code> package), but through quite different (but theoretically equivalent) estimation methods. Since estimation procedure is different, the mediation analysis in the previous parts is re-implemented to check robustness. Exports following results under <code>out</code> directory:
    * [<code>medflex.med.out.main.plot.woDE.png</code>](out/medflex.med.out.main.plot.woDE.png). APPENDIX VI-A: Causal mediation analysis plot with 2p mediators (Logit) and 9p outcome (OLS) with additional output of joint causal mediation effect.
    * [<code>medflex.med.out.sub.plot.png</code>](out/medflex.med.out.sub.plot.png). APPENDIX VI-B: Causal mediation analysis plot with 5p mediators (OLS) and 9p outcome (OLS) with additional output of joint causal mediation effect.
* <code>**analysis5_additional.R**</code> ([Code](src/analysis5_additional.R); [Code/Output](src/analysis5_additional.md)). Contains additional analytical codes. Make sure to run <code>**analysis3_mediation.R**</code> prior to running this code file. Exports following results under <code>out</code> directory:
    * [<code>nodifacme.png</code>](out/nodifacme.png). Assess difference in causal mediation effects between Philippines and Myanmar. Uses the analytical results used for exporting FIGURE 5/[<code>med.out.main.plot.woDE.png</code>](out/med.out.main.plot.woDE.png). 
    * [<code>med.out.ALL.main.plot.woDE.png</code>](out/med.out.ALL.main.plot.woDE.png). Causal mediation analysis plot with 2p mediators (Logit) and 9p outcome (OLS) using combined data of Philippines and Myanmar. Without direct effects. Check [<code>med.out.ALL.main.plot.wDE.png</code>](out/med.out.ALL.main.plot.wDE.png) for direct effects.
    * [<code>med.out.ALL.sub.plot.woDE.png</code>](out/med.out.ALL.sub.plot.png). Causal mediation analysis plot with 5p mediators (OLS) and 9p outcome (OLS). Without direct effects using combined data of Philippines and Myanmar. Check [<code>med.out.ALL.sub.plot.wDE.png</code>](out/med.out.ALL.sub.plot.wDE.png) for direct effects.

### Project Structure

* <code>src</code>: Contains code files and their direct outputs.
* <code>src/processing</code>: Currently empty. When you run analysis codes, this folder stores temporary files used during the analysis.
* <code>data</code>: Contains datasets and questionnaire.
* <code>out</code>: Contains output plots and tables.
* <code>paper</code>: Contains working paper.