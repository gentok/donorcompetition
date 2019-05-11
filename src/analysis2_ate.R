#' ---
#' title: "Analysis 2: Average Treatment Effect"
#' author: "Gento Kato"
#' date: "April 13, 2019"
#' ---

#' # Preparation

## Clear Workspace
rm(list = ls())

## Set Working Directory (Automatically) ##
require(rprojroot); require(rstudioapi)
if (rstudioapi::isAvailable()==TRUE) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); 
} 
projdir <- find_root(has_file("thisishome.txt"))
#cat(paste("Working Directory Set to:\n",projdir))
setwd(projdir)

## Required Functions & Packages
source("src/analysis0_functions.R")
source("src/cl.mlogit.R")

## Load Data
do <- readRDS("data/donorexp.rds")
d <- do[do$comply==1,] # only compliers

## Subset Data
# MMR
d.MMR <- d[d$treatment %in% c(1,2),]
d.MMR$threat <- d.MMR$threat.MMR
d.MMR$imp <- d.MMR$imp.MMR
d.MMR$potential <- d.MMR$potential.MMR
# PHL
d.PHL <- d[d$treatment %in% c(3,5),]
d.PHL$threat <- d.PHL$threat.PHL
d.PHL$imp <- d.PHL$imp.PHL
d.PHL$potential <- d.PHL$potential.PHL

#' # Average Treatment Effect
#' 
#' ## Simple OLS

# Define Outcome Variable and Drop Missing Values
d.MMR$out <- d.MMR$cancel_aid
d.PHL$out <- d.PHL$cancel_aid
vars <- c("out","cancel_aid",
          "treat_China",
          "threat","imp","potential",  
          "issint","odaimp","fem","age","ide3",
          "med_econ","med_secu","med_repu","med_effi")
d.MMR.sub <- na.omit(d.MMR[,vars])
d.PHL.sub <- na.omit(d.PHL[,vars])

# Analysis
m0.MMR <- lm(as.numeric(out)~treat_China, data = d.MMR.sub)
m1.MMR <- lm(update(as.numeric(out)~treat_China,fcv), data = d.MMR.sub)
m0.PHL <- lm(as.numeric(out)~treat_China, data = d.PHL.sub)
m1.PHL <- lm(update(as.numeric(out)~treat_China,fcv), data = d.PHL.sub)

#+ eval = FALSE
# Export Table
table_coef(list(m0.MMR,m1.MMR,m0.PHL,m1.PHL), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE,
           caption = "Table 2: Treatment Effect on the Support for Cancelling Aid",
           custom.footnote = "Estimated by ordinary least square (OLS) regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/olsres")
adjpval("out/olsres.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/olsres.doc"))))

#'
#' Export Coefficient Plot (Dropping Intercept)
#' 
ftset <- paste0("Estimated by ordinary least square (OLS) regression with HC1 robust standard errors.",
                " Intercept omitted from analysis.",
                "\n Myanmar models N = ", nobs(m1.MMR), "and Philippines models N = ", nobs(m1.PHL), ".")
p <- plot_coef(list(m0.MMR,m1.MMR,m0.PHL,m1.PHL), 
           vcov.est = "robust", # Stata HC1 Robust SE
           overlap.names = rep(c("Baseline","Full"),2),
           overlap.linetype.index = c(2,1),
           overlap.shape.index = c(17,19),
           facet.names = rep(c("Myanmar","Philippines"), each=2),
           drop.intercept = TRUE,
           custom.variable.names = basevn[-1],
           title = "Treatment Effect on the Support for Cancelling Aid",
           custom.footnote = ftset, footnote.caption = TRUE)
#+ eval=FALSE
png_save(p,w=800,h=600,file="out/olsplot.png")

#' ## Simple OLS (with Standardized DV)


# Define Outcome Variable and Drop Missing Values
d.MMR$out <- d.MMR$cancel_aid
d.PHL$out <- d.PHL$cancel_aid
vars <- c("out","treat_China",
          "threat","imp","potential",  
          "issint","odaimp","fem","age","ide3",
          "med_econ","med_secu","med_repu","med_effi")
d.MMR.sub <- na.omit(d.MMR[,vars])
d.PHL.sub <- na.omit(d.PHL[,vars])

## Standardize DV by Control Group SD
d.MMR.sub$out <- as.numeric(d.MMR.sub$out)/
  sd(as.numeric(d.MMR.sub$out)[d.MMR.sub$treat_China==0]) #- 
  #mean(as.numeric(d.MMR.sub$out)[d.MMR.sub$treat_China==0])
d.PHL.sub$out <- as.numeric(d.PHL.sub$out)/
  sd(as.numeric(d.PHL.sub$out)[d.PHL.sub$treat_China==0]) #- 
  #mean(as.numeric(d.PHL.sub$out)[d.MMR.sub$treat_China==0])

# Analysis
m0.MMR <- lm(as.numeric(out)~treat_China, data = d.MMR.sub)
m1.MMR <- lm(update(as.numeric(out)~treat_China,fcv), data = d.MMR.sub)
m0.PHL <- lm(as.numeric(out)~treat_China, data = d.PHL.sub)
m1.PHL <- lm(update(as.numeric(out)~treat_China,fcv), data = d.PHL.sub)
#+ eval=FALSE
# Export Table
table_coef(list(m0.MMR,m1.MMR,m0.PHL,m1.PHL), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE,
           caption = "Table 2: Treatment Effect on the Support for Cancelling Aid",
           custom.footnote = "Estimated by ordinary least square (OLS) regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/olsres_std", show.table=TRUE)
adjpval("out/olsres_std.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/olsres_std.doc"))))

#'
#' Export Coefficient Plot (Dropping Intercept)
#' 
ftset <- paste0("Estimated by ordinary least square (OLS) regression with HC1 robust standard errors.",
                " Intercept omitted from analysis.",
                "\n Myanmar models N = ", nobs(m1.MMR), "and Philippines models N = ", nobs(m1.PHL), ".")
p <- plot_coef(list(m0.MMR,m1.MMR,m0.PHL,m1.PHL), 
               vcov.est = "robust", # Stata HC1 Robust SE
               overlap.names = rep(c("Baseline","Full"),2),
               overlap.linetype.index = c(2,1),
               overlap.shape.index = c(17,19),
               facet.names = rep(c("Myanmar","Philippines"), each=2),
               drop.intercept = TRUE,
               custom.variable.names = basevn[-1],
               title = "Treatment Effect on the Support for Cancelling Aid",
               custom.footnote = ftset, footnote.caption = TRUE)
#+ eval=FALSE
png_save(p,w=800,h=600,file="out/olsplot_std.png")

#' ## Multinomial Logit

# Define Outcome Variable and Drop Missing Values
d.MMR$out <- d.MMR$cancel_aid_3cat
d.PHL$out <- d.PHL$cancel_aid_3cat
vars <- c("out","treat_China","id",
          "threat","imp","potential",  
          "issint","odaimp","fem","age","ide3",
          "med_econ","med_secu","med_repu","med_effi")
d.MMR.sub <- na.omit(d.MMR[,vars])
d.PHL.sub <- na.omit(d.PHL[,vars])

# Transform to "mlogit" style dagta
d.MMR.sub.mlogit <- mlogit.data(d.MMR.sub,shape="wide",
                                choice="out",
                                id.var="id")
d.PHL.sub.mlogit <- mlogit.data(d.PHL.sub,shape="wide",
                                choice="out",
                                id.var="id")

# Run Multinomial Logit
m0.MMR <- mlogit(out ~ 1 | treat_China, 
             data = d.MMR.sub.mlogit, reflevel="1")
m1.MMR <- mlogit(update(mFormula(out ~ 1 | treat_China), fcvml), 
             data = d.MMR.sub.mlogit, reflevel="1")
m0.PHL <- mlogit(out ~ 1 | treat_China, 
                 data = d.PHL.sub.mlogit, reflevel="1")
m1.PHL <- mlogit(update(mFormula(out ~ 1 | treat_China), fcvml), 
                 data = d.PHL.sub.mlogit, reflevel="1")

# Variable Names
basevn.mlogit <- paste(c("Neither:","Cancel:"), rep(basevn,each=2))

#+ eval = FALSE
table_coef(list(m0.MMR,m1.MMR,m0.PHL,m1.PHL), 
           vcov.est = list(cl.mlogit.vcov(m0.MMR,d.MMR.sub$id),
                           cl.mlogit.vcov(m1.MMR,d.MMR.sub$id),
                           cl.mlogit.vcov(m0.PHL,d.PHL.sub$id),
                           cl.mlogit.vcov(m1.PHL,d.PHL.sub$id)), 
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn.mlogit,
           single.row=FALSE,
           caption = "Table 2: Treatment Effect on the Support for Cancelling Aid",
           custom.footnote = "Estimated by ordinary least square (OLS) regression. Robust standard errors in parentheses.",
           format="doc", file.name="out/mlogitres", show.table=TRUE)
adjpval("out/mlogitres.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/mlogitres.doc"))))

#'
#' Table with Only ATE
#'
#+ eval = FALSE
table_coef(list(m0.MMR,m1.MMR,m0.PHL,m1.PHL), 
           vcov.est = list(cl.mlogit.vcov(m0.MMR,d.MMR.sub$id),
                           cl.mlogit.vcov(m1.MMR,d.MMR.sub$id),
                           cl.mlogit.vcov(m0.PHL,d.PHL.sub$id),
                           cl.mlogit.vcov(m1.PHL,d.PHL.sub$id)), 
           m.names = paste0(rep(c("Myanmar","Philippines"), each=2),
                           rep(c("(Baseline)","(Full)"), 2)),
           drop.variable.names = names(m1.MMR$coefficients)[-c(3,4)],
           custom.variable.names = basevn.mlogit[c(3,4)],
           single.row=FALSE,
           caption = "Table 2: Treatment Effect on the Support for Cancelling Aid",
           custom.footnote = "Estimated by ordinary least square (OLS) regression. Robust standard errors in parentheses. \n",
           format="doc", file.name="out/mlogitres_onlyATE", show.table=TRUE)
adjpval("out/mlogitres_onlyATE.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/mlogitres_onlyATE.doc"))))

#'
#' Simulation of Predicted Probability
#'
#+ eval=FALSE

# This takes a while, so not executed in github output
simu.m1.MMR.mlogit <- 
  simu_mlogit_ate(m1.MMR, cl.mlogit.vcov(m1.MMR,d.MMR.sub$id),
                  d.MMR.sub.mlogit, "treat_China")
simu.m1.PHL.mlogit <- 
  simu_mlogit_ate(m1.PHL, cl.mlogit.vcov(m1.PHL,d.PHL.sub$id),
                  d.PHL.sub.mlogit, "treat_China")

#+ eval=FALSE, echo=FALSE
# Save This Relatively Heavy Simulations
save(simu.m1.MMR.mlogit,simu.m1.PHL.mlogit,
     file=paste0(projdir,"/src/processing/simu.m1.mlogit.RData"))

#+ include=FALSE
# Import pre-simulated data
load(paste0(projdir,"/src/processing/simu.m1.mlogit.RData"))

#+ 
# Combine Data
simu.m1.mlogit <- rbind(simu.m1.MMR.mlogit$summary_control,
                        simu.m1.MMR.mlogit$summary_treated,
                        simu.m1.PHL.mlogit$summary_control,
                        simu.m1.PHL.mlogit$summary_treated)
colnames(simu.m1.mlogit) <- c("Mean", "lowerCI", "upperCI")
simu.m1.mlogit$treat <- factor(rep(rep(c("Control","Treated"),each=3),2))
simu.m1.mlogit$outcat <- 
  factor(rep(c("Continue","Neither","Cancel"),2),
                   levels=c("Continue","Neither","Cancel"))
simu.m1.mlogit$outrec <- 
  factor(rep(c("Myanmar","Philippines"),each=6),
         levels=c("Myanmar","Philippines"))

# Plot Predicted Probabilities
p <- plot_simu(simu.m1.mlogit, "outcat", 
          type.est="point",
          name.facet.x = "outrec",
          name.color = "treat", name.shape = "treat",
          label.shape = "Treatment",
          label.color = "Treatment",
          label.y = "Predicted Probability\n (with 95% Confidence Interval)",
          label.x = "Aid Cancellation Preference") + 
  scale_color_manual(name="Treatment",values=c("black","black"))
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals estimated from quasi-Bayesian Monte Carlo method based on normal \n         approximation using robust standard errors. The model is estimated by Multinomial Logistic regression.",
                   bottom.expand.rate = 3, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8.5, fig.height=6
grid.draw(p)

#+ eval=FALSE
png_save(p,w=850,h=600,file="out/mlogitplot.png")


#' ## The Effect of Treatment on Mediator (OLS)

# Define Outcome Variable and Drop Missing Values
d.MMR$out <- d.MMR$cancel_aid
d.PHL$out <- d.PHL$cancel_aid
vars <- c("out","cancel_aid",
          "treat_China",
          "threat","imp","potential",  
          "issint","odaimp","fem","age","ide3",
          "med_econ","med_secu","med_repu","med_effi")
d.MMR.sub <- na.omit(d.MMR[,vars])
d.PHL.sub <- na.omit(d.PHL[,vars])

#'
#' Analysis (secu)
#' 
m0.MMR.secu <- lm(as.numeric(med_secu)~treat_China, data = d.MMR.sub)
m1.MMR.secu <- lm(update(as.numeric(med_secu)~treat_China,fcv), data = d.MMR.sub)
m0.PHL.secu <- lm(as.numeric(med_secu)~treat_China, data = d.PHL.sub)
m1.PHL.secu <- lm(update(as.numeric(med_secu)~treat_China,fcv), data = d.PHL.sub)

#+ eval=FALSE
# Export Table (secu)
table_coef(list(m0.MMR.secu,m1.MMR.secu,m0.PHL.secu,m1.PHL.secu), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE,
           caption = "Treatment Effect on Mediator (Damage to Security Interests)",
           custom.footnote = "Estimated by ordinary least square (OLS) regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/olsres_secu")
adjpval("out/olsres_secu.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/olsres_secu.doc"))))

#'
#' Analysis (econ)
#' 
m0.MMR.econ <- lm(as.numeric(med_econ)~treat_China, data = d.MMR.sub)
m1.MMR.econ <- lm(update(as.numeric(med_econ)~treat_China,fcv), data = d.MMR.sub)
m0.PHL.econ <- lm(as.numeric(med_econ)~treat_China, data = d.PHL.sub)
m1.PHL.econ <- lm(update(as.numeric(med_econ)~treat_China,fcv), data = d.PHL.sub)

#+ eval = FALSE
# Export Table (econ)
table_coef(list(m0.MMR.econ,m1.MMR.econ,m0.PHL.econ,m1.PHL.econ), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE,
           caption = "Treatment Effect on Mediator (Damage to Economic Interests)",
           custom.footnote = "Estimated by ordinary least square (OLS) regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/olsres_econ")
adjpval("out/olsres_econ.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/olsres_econ.doc"))))

#'
#' Analysis (repu)
#' 
m0.MMR.repu <- lm(as.numeric(med_repu)~treat_China, data = d.MMR.sub)
m1.MMR.repu <- lm(update(as.numeric(med_repu)~treat_China,fcv), data = d.MMR.sub)
m0.PHL.repu <- lm(as.numeric(med_repu)~treat_China, data = d.PHL.sub)
m1.PHL.repu <- lm(update(as.numeric(med_repu)~treat_China,fcv), data = d.PHL.sub)

#+ eval = FALSE
# Export Table (repu)
table_coef(list(m0.MMR.repu,m1.MMR.repu,m0.PHL.repu,m1.PHL.repu), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE,
           caption = "Treatment Effect on Mediator (Damage to International Reputation)",
           custom.footnote = "Estimated by ordinary least square (OLS) regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/olsres_repu")
adjpval("out/olsres_repu.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/olsres_repu.doc"))))

#'
#' Analysis (effi)
#' 
m0.MMR.effi <- lm(as.numeric(med_effi)~treat_China, data = d.MMR.sub)
m1.MMR.effi <- lm(update(as.numeric(med_effi)~treat_China,fcv), data = d.MMR.sub)
m0.PHL.effi <- lm(as.numeric(med_effi)~treat_China, data = d.PHL.sub)
m1.PHL.effi <- lm(update(as.numeric(med_effi)~treat_China,fcv), data = d.PHL.sub)

#+ eval = FALSE
# Export Table (effi)
table_coef(list(m0.MMR.effi,m1.MMR.effi,m0.PHL.effi,m1.PHL.effi), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE,
           caption = "Treatment Effect on Mediator (Effecitiveness of Cancelling Aid)",
           custom.footnote = "Estimated by ordinary least square (OLS) regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/olsres_effi")
adjpval("out/olsres_effi.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/olsres_effi.doc"))))

#'
#' ### Plot the Treatment Effect on Mediator (OLS)
#' 

# Data for Plotting
estlist <- list(m1.MMR.secu,m1.MMR.econ,m1.MMR.repu,m1.MMR.effi,
                m1.PHL.secu,m1.PHL.econ,m1.PHL.repu,m1.PHL.effi)
dOLS <- as.data.frame(t(sapply(estlist, estrow, meth="asis")))
dOLS$pcat <- ifelse(dOLS$p10==0,"p >= .1", ifelse(dOLS$p05==0,"p < .1","p < .05"))
dOLS$pcat <- factor(dOLS$pcat,levels=c("p < .05","p < .1", "p >= .1"))
dOLS$med <- rep(c("Security","Economy","Reputation","Efficacy"),2)
dOLS$med <- factor(dOLS$med, levels=unique(dOLS$med))
dOLS$eff <- "OLS (5p)"
dOLS$country <- rep(c("Myanmar","Philippines"), each=4)

p <- 
  genplot(dOLS, "", include.eff = c("OLS (5p)")) + 
  ylab("OLS Coefficient") + ggtitle(NULL) + xlab(NULL) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size=12, face="bold"))
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated from OLS regression with robust standard errors. \n         Mediator variables are in five point scale and each model includes full set of control variables.",
                   bottom.expand.rate = 8, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8, fig.height=4
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, file=c("out/dvmedplot_ols.png"))

#' ## The Effect of Treatment on Mediator (Logit)

# Define Outcome Variable and Drop Missing Values
d.MMR$out <- d.MMR$cancel_aid
d.PHL$out <- d.PHL$cancel_aid
vars <- c("out","cancel_aid",
          "treat_China",
          "threat","imp","potential",  
          "issint","odaimp","fem","age","ide3",
          "med_econ","med_secu","med_repu","med_effi")
d.MMR.sub <- na.omit(d.MMR[,vars])
d.PHL.sub <- na.omit(d.PHL[,vars])

#'
#' Analysis (secu)
#' 
m0.MMR.secu <- glm(as.numeric(med_secu)>3~treat_China, data = d.MMR.sub, family=binomial("logit"))
m1.MMR.secu <- glm(update(as.numeric(med_secu)>3~treat_China,fcv), data = d.MMR.sub, family=binomial("logit"))
m0.PHL.secu <- glm(as.numeric(med_secu)>3~treat_China, data = d.PHL.sub, family=binomial("logit"))
m1.PHL.secu <- glm(update(as.numeric(med_secu)>3~treat_China,fcv), data = d.PHL.sub, family=binomial("logit"))

#+ eval = FALSE
# Export Table (secu)
table_coef(list(m0.MMR.secu,m1.MMR.secu,m0.PHL.secu,m1.PHL.secu), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE, 
           caption = "Treatment Effect on Mediator (Damage to Security Interests)",
           custom.footnote = "Estimated by Logitstic regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/logitres_secu")
adjpval("out/logitres_secu.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/logitres_secu.doc"))))

#'
#' Analysis (econ)
#' 

m0.MMR.econ <- glm(as.numeric(med_econ)>3~treat_China, data = d.MMR.sub, family=binomial("logit"))
m1.MMR.econ <- glm(update(as.numeric(med_econ)>3~treat_China,fcv), data = d.MMR.sub, family=binomial("logit"))
m0.PHL.econ <- glm(as.numeric(med_econ)>3~treat_China, data = d.PHL.sub, family=binomial("logit"))
m1.PHL.econ <- glm(update(as.numeric(med_econ)>3~treat_China,fcv), data = d.PHL.sub, family=binomial("logit"))

#+ eval = FALSE
# Export Table (econ)
table_coef(list(m0.MMR.econ,m1.MMR.econ,m0.PHL.econ,m1.PHL.econ), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE, 
           caption = "Treatment Effect on Mediator (Damage to Economic Interests)",
           custom.footnote = "Estimated by Logitstic regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/logitres_econ")
adjpval("out/logitres_econ.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/logitres_econ.doc"))))

#'
#' Analysis (repu)
#' 
m0.MMR.repu <- glm(as.numeric(med_repu)>3~treat_China, data = d.MMR.sub, family=binomial("logit"))
m1.MMR.repu <- glm(update(as.numeric(med_repu)>3~treat_China,fcv), data = d.MMR.sub, family=binomial("logit"))
m0.PHL.repu <- glm(as.numeric(med_repu)>3~treat_China, data = d.PHL.sub, family=binomial("logit"))
m1.PHL.repu <- glm(update(as.numeric(med_repu)>3~treat_China,fcv), data = d.PHL.sub, family=binomial("logit"))

#+ eval = FALSE
# Export Table (repu)
table_coef(list(m0.MMR.repu,m1.MMR.repu,m0.PHL.repu,m1.PHL.repu), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE, 
           caption = "Treatment Effect on Mediator (Damage to International Reputation)",
           custom.footnote = "Estimated by Logitstic regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/logitres_repu")
adjpval("out/logitres_repu.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/logitres_repu.doc"))))

#'
#' Analysis (effi)
#' 
m0.MMR.effi <- glm(as.numeric(med_effi)>3~treat_China, data = d.MMR.sub, family=binomial("logit"))
m1.MMR.effi <- glm(update(as.numeric(med_effi)>3~treat_China,fcv), data = d.MMR.sub, family=binomial("logit"))
m0.PHL.effi <- glm(as.numeric(med_effi)>3~treat_China, data = d.PHL.sub, family=binomial("logit"))
m1.PHL.effi <- glm(update(as.numeric(med_effi)>3~treat_China,fcv), data = d.PHL.sub, family=binomial("logit"))

#+ eval = FALSE
# Export Table (effi)
table_coef(list(m0.MMR.effi,m1.MMR.effi,m0.PHL.effi,m1.PHL.effi), 
           vcov.est = "robust", # Stata HC1 Robust SE
           m.names = rep(c("Myanmar","Philippines"), each=2),
           custom.variable.names = basevn,
           single.row=FALSE, 
           caption = "Treatment Effect on Mediator (Effecitiveness of Cancelling Aid)",
           custom.footnote = "Estimated by Logitstic regression. HC1 robust standard errors in parentheses.",
           format="doc", file.name="out/logitres_effi")
adjpval("out/logitres_effi.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/logitres_effi.doc"))))

#'
#' ### Plot the Treatment Effect on Mediator (Logit)
#' 

# Data for Plotting
estlist <- list(m1.MMR.secu,m1.MMR.econ,m1.MMR.repu,m1.MMR.effi,
                m1.PHL.secu,m1.PHL.econ,m1.PHL.repu,m1.PHL.effi)
dLogit <- as.data.frame(t(sapply(estlist, estrow, meth="odds")))
dLogit$pcat <- ifelse(dLogit$p10==0,"p >= .1", ifelse(dLogit$p05==0,"p < .1","p < .05"))
dLogit$pcat <- factor(dLogit$pcat,levels=c("p < .05","p < .1", "p >= .1"))
dLogit$med <- rep(c("Security","Economy","Reputation","Efficacy"),2)
dLogit$med <- factor(dLogit$med, levels=unique(dLogit$med))
dLogit$eff <- "Logit (2p)"
dLogit$country <- rep(c("Myanmar","Philippines"), each=4)

p <- 
  genplot(dLogit, "", include.eff = c("Logit (2p)"), odds = TRUE) + 
  ylab("Odds Ratio") + ggtitle(NULL) + xlab(NULL) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size=12, face="bold"))
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated from Logistic regression with robust standard errors. \n         Mediator variables are in two point scale and each model includes full set of control variables.",
                   bottom.expand.rate = 8, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8, fig.height=4
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, file=c("out/dvmedplot_logit.png"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('src/analysis2_ate.R', rmarkdown::github_document(toc=TRUE), clean=FALSE, encoding = 'UTF-8')
# In Terminal, run:
# Rscript -e "rmarkdown::render('analysis2_ate.R', rmarkdown::github_document(toc=TRUE), clean=FALSE, encoding = 'UTF-8')"
