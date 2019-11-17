#' ---
#' title: "Analysis 2: Average Treatment Effect"
#' author: "Gento Kato"
#' date: "November 17, 2019"
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

#'
#' ### Regression Table (Appendix III)
#'
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
#' ### Coefficient Plot without Intercept (Figure 3)
#' 

ftset <- paste0("Note: Estimated by ordinary least square (OLS) regression with robust standard errors.",
                "\nIntercepts are omitted from the output.",
                " Myanmar: N = ", nobs(m1.MMR), ". Philippines: N = ", nobs(m1.PHL), ".")

tmp0a <- matrix_coefci(m0.MMR, vcov.est="robust")
tmp0a$mod <- "Treatment Only"
tmp0a$country <- "Myanmar"
tmp1a <- matrix_coefci(m1.MMR, vcov.est="robust")
tmp1a$mod <- "With Covariates"
tmp1a$country <- "Myanmar"
tmp0b <- matrix_coefci(m0.PHL, vcov.est="robust")
tmp0b$mod <- "Treatment Only"
tmp0b$country <- "Philippines"
tmp1b <- matrix_coefci(m1.PHL, vcov.est="robust")
tmp1b$mod <- "With Covariates"
tmp1b$country <- "Philippines"
tmp <- rbind(tmp0a,tmp1a,tmp0b,tmp1b)
tmp$mod <- factor(tmp$mod, levels=unique(tmp$mod))
tmp$country <- factor(tmp$country, levels=unique(tmp$country))
tmp$vn <- factor(c(basevn[1:2],basevn,basevn[1:2],basevn),
                 levels=rev(basevn))
tmp <- tmp[tmp$vn!="(Intercept)",]

p <- ggplot(tmp, aes(x=vn, y=CF, ymin=lowerCI, ymax=upperCI)) + 
  geom_hline(aes(yintercept=0), linetype=2, color="red1") + 
  geom_point() + 
  geom_errorbar(width=0.3) + 
  facet_grid(mod~country, scales="free_y", space="free_y", switch = "y") + 
  coord_flip() + 
  ylab("Coefficient with 95% Confidence Interval") + xlab(NULL) +
  #labs(caption=ftset) + 
  theme_bw() + 
  theme(legend.position="bottom", 
        legend.title = element_blank(),
        axis.text.y = element_text(face="bold"),
        strip.text.x = element_text(face="bold", size=11),
        strip.text.y = element_text(face="bold", size=11, angle=180),
        strip.background.y = element_rect(color="black",fill=NA),
        strip.placement = "outside",
        panel.border = element_rect(color="black"),
        plot.caption = element_text(hjust=-1))
p <- plot_footnote(p, ftset, fontsize=9, bottom.expand.rate=7, align="right",
              distance.from.side = 0.02)



#+ fig.width=9, fig.height=5.5
grid.draw(p)

#+ eval=FALSE
png_save(p,w=900,h=550,file="out/olsplot.png")

#'
#' ## Multinomial Logit (Appendix II)
#' 

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

#'
#' ### Regression Table
#'
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
           single.row=TRUE,
           caption = "Treatment Effect on the Support for Cancelling Aid",
           custom.footnote = "Estimated by multinomial logit. Robust standard errors in parentheses.",
           format="doc", file.name="out/mlogitres", show.table=TRUE)
adjpval("out/mlogitres.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/mlogitres.doc"))))

#'
#' ### Table with Only ATE
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
           caption = "Treatment Effect on the Support for Cancelling Aid",
           custom.footnote = "Estimated by multinomial logit. Robust standard errors in parentheses. \n",
           format="doc", file.name="out/mlogitres_onlyATE", show.table=TRUE)
adjpval("out/mlogitres_onlyATE.doc")

#+ results="asis", echo=FALSE
cat(as.character(read_html(paste0(projdir,"/out/mlogitres_onlyATE.doc"))))

#'
#' ### Simulation of Predicted Probability
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
simu.m1.mlogit$treat <- factor(rep(rep(c(" Control Group "," Treated Group "),each=3),2))
simu.m1.mlogit$outcat <- 
  factor(rep(c("Continue","Neither","Cancel"),2),
                   levels=c("Continue","Neither","Cancel"))
simu.m1.mlogit$outrec <- 
  factor(rep(c("Myanmar","Philippines"),each=6),
         levels=c("Myanmar","Philippines"))

#'
#' ### Plot of Predicted Probabilities (Appendix II)
#'

# Plot Predicted Probabilities
p <- plot_simu(simu.m1.mlogit, "outcat", 
          type.est="point",
          name.facet.x = "outrec",
          name.color = "treat", name.shape = "treat",
          label.shape = "Treatment",
          label.color = "Treatment",
          label.y = "Predicted Probability\n (with 95% Confidence Interval)",
          label.x = "Aid Cancellation Preference") + 
  scale_color_manual(name="Treatment",values=c("black","black")) + 
  theme_bw() + 
  theme(legend.position="bottom", 
        legend.title = element_blank(),
        axis.text.y = element_text(face="bold"),
        strip.text = element_text(face="bold", size=11),
        plot.caption = element_text(hjust=0)) + 
  labs(caption=
"Note: Lines represent 95% confidence intervals estimated from Monte Carlo method based on normal 
approximation using robust standard errors. The model is estimated by Multinomial Logistic regression.")

#+ fig.width=8.5, fig.height=6
grid.draw(p)

#+ eval=FALSE
png_save(p,w=850,h=600,file="out/mlogitplot.png")

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('src/analysis2_ate.R', rmarkdown::github_document(toc=TRUE), clean=FALSE, encoding = 'UTF-8')
# In Terminal, run:
# Rscript -e "rmarkdown::render('analysis2_ate.R', rmarkdown::github_document(toc=TRUE), clean=FALSE, encoding = 'UTF-8')"
