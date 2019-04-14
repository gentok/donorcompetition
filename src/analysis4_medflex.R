#' ---
#' title: "Analysis 4: Joint Causal Mediation Analysis Using medflex Package"
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

d$cancel_aid <- as.numeric(d$cancel_aid)
d$med_econ <- as.numeric(d$med_econ)
d$med_secu <- as.numeric(d$med_secu)
d$med_repu <- as.numeric(d$med_repu)
d$med_effi <- as.numeric(d$med_effi)
d$med_econ_2cat <- ifelse(d$med_econ>3,1,0)
d$med_secu_2cat <- ifelse(d$med_secu>3,1,0)
d$med_repu_2cat <- ifelse(d$med_repu>3,1,0)
d$med_effi_2cat <- ifelse(d$med_effi>3,1,0)

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

# Drop Cases with Missing Values in Relevant Variables
vars <- c("cancel_aid","treat_China","threat","imp","potential",  
          "issint","odaimp","fem","age","ide3",
          "med_econ","med_secu","med_repu","med_effi",
          "med_econ_2cat","med_secu_2cat","med_repu_2cat","med_effi_2cat")
d.MMR.sub <- na.omit(d.MMR[,vars])
d.PHL.sub <- na.omit(d.PHL[,vars])

#' # Joint Causal Mediation Analysis
#' 
#' This analysis is based on VanderWeele and Vansteelandt (2013) which discusses 
#' the estimation of "joint" mediation effect under potential outcome framework.
#' <code>mediation</code> package does not provide this functionality, thus 
#' <code>medflex</code> packages is used here. 
#' 

library(medflex)

#' 
#' ## Check Single Mediators 
#' 
#' Check if the one-by-one mediation analysis looks the same with the 
#' results from <code>mediation</code> package (it is!).
#' 
#' ### Myanmar
#' 

# (secu)
# 2 cat mediator
di.MMR.main.secu <- neImpute(update(cancel_aid ~ treat_China + med_secu_2cat, fcv),
               family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.main.secu <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
             family = "gaussian", expData = di.MMR.main.secu, se = "robust")
# 5 cat mediator
di.MMR.sub.secu <- neImpute(update(cancel_aid ~ treat_China + med_secu, fcv),
                         family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.sub.secu <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                       family = "gaussian", expData = di.MMR.sub.secu, se = "robust")

# (econ)
# 2 cat mediator
di.MMR.main.econ <- neImpute(update(cancel_aid ~ treat_China + med_econ_2cat, fcv),
                         family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.main.econ <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                       family = "gaussian", expData = di.MMR.main.econ, se = "robust")
# 5 cat mediator
di.MMR.sub.econ <- neImpute(update(cancel_aid ~ treat_China + med_econ, fcv),
                        family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.sub.econ <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                      family = "gaussian", expData = di.MMR.sub.econ, se = "robust")

# (repu)
# 2 cat mediator
di.MMR.main.repu <- neImpute(update(cancel_aid ~ treat_China + med_repu_2cat, fcv),
                         family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.main.repu <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                       family = "gaussian", expData = di.MMR.main.repu, se = "robust")
# 5 cat mediator
di.MMR.sub.repu <- neImpute(update(cancel_aid ~ treat_China + med_repu, fcv),
                        family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.sub.repu <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                      family = "gaussian", expData = di.MMR.sub.repu, se = "robust")

# (effi)
# 2 cat mediator
di.MMR.main.effi <- neImpute(update(cancel_aid ~ treat_China + med_effi_2cat, fcv),
                         family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.main.effi <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                       family = "gaussian", expData = di.MMR.main.effi, se = "robust")
# 5 cat mediator
di.MMR.sub.effi <- neImpute(update(cancel_aid ~ treat_China + med_effi, fcv),
                        family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.sub.effi <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                      family = "gaussian", expData = di.MMR.sub.effi, se = "robust")

#' ### Philippines
#' 

# (secu)
# 2 cat mediator
di.PHL.main.secu <- neImpute(update(cancel_aid ~ treat_China + med_secu_2cat, fcv),
                             family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.main.secu <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                           family = "gaussian", expData = di.PHL.main.secu, se = "robust")
# 5 cat mediator
di.PHL.sub.secu <- neImpute(update(cancel_aid ~ treat_China + med_secu, fcv),
                            family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.sub.secu <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                          family = "gaussian", expData = di.PHL.sub.secu, se = "robust")

# (econ)
# 2 cat mediator
di.PHL.main.econ <- neImpute(update(cancel_aid ~ treat_China + med_econ_2cat, fcv),
                             family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.main.econ <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                           family = "gaussian", expData = di.PHL.main.econ, se = "robust")
# 5 cat mediator
di.PHL.sub.econ <- neImpute(update(cancel_aid ~ treat_China + med_econ, fcv),
                            family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.sub.econ <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                          family = "gaussian", expData = di.PHL.sub.econ, se = "robust")

# (repu)
# 2 cat mediator
di.PHL.main.repu <- neImpute(update(cancel_aid ~ treat_China + med_repu_2cat, fcv),
                             family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.main.repu <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                           family = "gaussian", expData = di.PHL.main.repu, se = "robust")
# 5 cat mediator
di.PHL.sub.repu <- neImpute(update(cancel_aid ~ treat_China + med_repu, fcv),
                            family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.sub.repu <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                          family = "gaussian", expData = di.PHL.sub.repu, se = "robust")

# (effi)
# 2 cat mediator
di.PHL.main.effi <- neImpute(update(cancel_aid ~ treat_China + med_effi_2cat, fcv),
                             family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.main.effi <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                           family = "gaussian", expData = di.PHL.main.effi, se = "robust")
# 5 cat mediator
di.PHL.sub.effi <- neImpute(update(cancel_aid ~ treat_China + med_effi, fcv),
                            family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.sub.effi <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                          family = "gaussian", expData = di.PHL.sub.effi, se = "robust")

#' ## Joint Causal Mediation Analysis

# 2p Mediator and 9p Outcome
di.MMR.main <- neImpute(update(cancel_aid ~ treat_China + med_secu_2cat * med_econ_2cat * med_repu_2cat * med_effi_2cat, fcv),
               family = "gaussian", nMed = 4, data = d.MMR.sub)
m.MMR.main <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
             family = "gaussian", expData = di.MMR.main, se = "robust")
summary(m.MMR.main)
di.PHL.main <- neImpute(update(cancel_aid ~ treat_China + med_secu_2cat * med_econ_2cat * med_repu_2cat * med_effi_2cat, fcv),
                        family = "gaussian", nMed = 4, data = d.PHL.sub)
m.PHL.main <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                      family = "gaussian", expData = di.PHL.main, se = "robust")
summary(m.PHL.main)

# 5p Mediator and 9p Outcome
di.MMR.sub <- neImpute(update(cancel_aid ~ treat_China + med_secu * med_econ * med_repu * med_effi, fcv),
                        family = "gaussian", nMed = 4, data = d.MMR.sub)
m.MMR.sub <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                      family = "gaussian", expData = di.MMR.sub, se = "robust")
summary(m.MMR.sub)
di.PHL.sub <- neImpute(update(cancel_aid ~ treat_China + med_secu * med_econ * med_repu * med_effi, fcv),
                        family = "gaussian", nMed = 4, data = d.PHL.sub)
m.PHL.sub <- neModel(update(cancel_aid ~ treat_China0 + treat_China1, fcv), 
                      family = "gaussian", expData = di.PHL.sub, se = "robust")
summary(m.PHL.sub)

#' ## Plotting Mediation Analysis Results

# 2p Mediator and 9p Outcome
mainls <- list(m.MMR.main.secu,m.MMR.main.econ,m.MMR.main.repu,m.MMR.main.effi,m.MMR.main,
               m.PHL.main.secu,m.PHL.main.econ,m.PHL.main.repu,m.PHL.main.effi,m.PHL.main)
dmain0 <- as.data.frame(t(sapply(mainls, estrow2, at=2)))
dmain1 <- as.data.frame(t(sapply(mainls, estrow2, at=3)))
dmain <- rbind(dmain0,dmain1)
dmain$pcat <- ifelse(dmain$p10==0,"p >= .1", ifelse(dmain$p05==0,"p < .1","p < .05"))
dmain$pcat <- factor(dmain$pcat,levels=c("p < .05","p < .1", "p >= .1"))
dmain$med <- rep(c("Security","Economy","Reputation","Efficacy","JOINT"),4)
dmain$med <- factor(dmain$med, levels=unique(dmain$med))
dmain$eff <- rep(c("Direct","Mediated"), each = 10)
dmain$eff <- factor(dmain$eff, levels=c("Mediated","Direct","Total"))
dmain$country <- rep(rep(c("Myanmar","Philippines"), each=5),2)

p <- genplot(dmain, "", include.eff = c("Mediated","Direct")) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with binary mediators and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package. 'JOINT' is the joint mediation effect of all mediators.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8, fig.height=4
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, file=c("out/medflex.med.out.main.plot.png"))

#+
# 5p Mediator and 9p Outcome
subls <- list(m.MMR.sub.secu,m.MMR.sub.econ,m.MMR.sub.repu,m.MMR.sub.effi,m.MMR.sub,
               m.PHL.sub.secu,m.PHL.sub.econ,m.PHL.sub.repu,m.PHL.sub.effi,m.PHL.sub)
dsub0 <- as.data.frame(t(sapply(subls, estrow2, at=2)))
dsub1 <- as.data.frame(t(sapply(subls, estrow2, at=3)))
dsub <- rbind(dsub0,dsub1)
dsub$pcat <- ifelse(dsub$p10==0,"p >= .1", ifelse(dsub$p05==0,"p < .1","p < .05"))
dsub$pcat <- factor(dsub$pcat,levels=c("p < .05","p < .1", "p >= .1"))
dsub$med <- rep(c("Security","Economy","Reputation","Efficacy","JOINT"),4)
dsub$med <- factor(dsub$med, levels=unique(dsub$med))
dsub$eff <- rep(c("Direct","Mediated"), each = 10)
dsub$eff <- factor(dsub$eff, levels=c("Mediated","Direct","Total"))
dsub$country <- rep(rep(c("Myanmar","Philippines"), each=5),2)

p <- genplot(dsub, "", include.eff = c("Mediated","Direct")) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with 5p mediators and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package. 'JOINT' is the joint mediation effect of all mediators.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8, fig.height=4
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, file=c("out/medflex.med.out.sub.plot.png"))

#' ## Moderated Mediation of Security Mediator

# Drop Cases with Missing Values in Relevant Variables
vars <- c("cancel_aid","treat_China","threat","imp","potential",  
          "issint","odaimp","fem","age","ide3",
          "threat.CHN","threat.CHN.3cat",
          "med_econ","med_secu","med_repu","med_effi",
          "med_econ_2cat","med_secu_2cat","med_repu_2cat","med_effi_2cat")
d.MMR.sub <- na.omit(d.MMR[,vars])
d.PHL.sub <- na.omit(d.PHL[,vars])

#' ### 2p Moderator, 2p Mediator and 9p Outcome
di.MMR.main <- neImpute(update(cancel_aid ~ treat_China + med_secu_2cat + treat_China:threat.CHN + med_secu_2cat:threat.CHN, fcv),
                        family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.main <- neModel(update(cancel_aid ~ treat_China0 + treat_China1 + treat_China0:threat.CHN + treat_China1:threat.CHN, fcv), 
                      family = "gaussian", expData = di.MMR.main, se = "robust")
di.PHL.main <- neImpute(update(cancel_aid ~ treat_China + med_secu_2cat + treat_China:threat.CHN + med_secu_2cat:threat.CHN, fcv),
                        family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.main <- neModel(update(cancel_aid ~ treat_China0 + treat_China1 + treat_China0:threat.CHN + treat_China1:threat.CHN, fcv), 
                      family = "gaussian", expData = di.PHL.main, se = "robust")

# Data
de.main <- rbind(modex(m.MMR.main, "Myanmar", v=1),
                 modex(m.PHL.main, "Philippines", v=1))

# Plot (with Total Effect)
p <- genplot2(de.main,"(Mediator: Security Interests)",interact=TRUE) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with binary mediator and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8.5, fig.height=5
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, dpi=300, file=c("out/medflex.med.mod.out.main.1.secu.wtotal.plot.png"))

#+
# Data
de.main.wototal <- de.main[de.main$eff != "Total",]

# Plot (without Total Effect)
p <- genplot2(de.main.wototal,"(Mediator: Security Interests)",interact=TRUE) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with binary mediator and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8.5, fig.height=5
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, dpi=300, file=c("out/medflex.med.mod.out.main.1.secu.wototal.plot.png"))

#' ### 3p Moderator, 2p Mediator and 9p Outcome
di.MMR.main2 <- neImpute(update(cancel_aid ~ treat_China + med_secu_2cat + treat_China:threat.CHN.3cat + med_secu_2cat:threat.CHN.3cat, fcv),
                        family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.main2 <- neModel(update(cancel_aid ~ treat_China0 + treat_China1 + treat_China0:threat.CHN.3cat + treat_China1:threat.CHN.3cat, fcv), 
                      family = "gaussian", expData = di.MMR.main2, se = "robust")
di.PHL.main2 <- neImpute(update(cancel_aid ~ treat_China + med_secu_2cat + treat_China:threat.CHN.3cat + med_secu_2cat:threat.CHN.3cat, fcv),
                        family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.main2 <- neModel(update(cancel_aid ~ treat_China0 + treat_China1 + treat_China0:threat.CHN.3cat + treat_China1:threat.CHN.3cat, fcv), 
                      family = "gaussian", expData = di.PHL.main2, se = "robust")

# Data
de.main2 <- rbind(modex(m.MMR.main2, "Myanmar", v=2),
                 modex(m.PHL.main2, "Philippines", v=2))

# Plot (with Total Effect)
p <- genplot2(de.main2,"(Mediator: Security Interests)",interact=TRUE) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with binary mediator and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8.5, fig.height=5
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, dpi=300, file=c("out/medflex.med.mod.out.main.3.secu.wtotal.plot.png"))

#+
# Data
de.main2.wototal <- de.main2[de.main2$eff != "Total",]

# Plot (without Total Effect)
p <- genplot2(de.main2.wototal,"(Mediator: Security Interests)",interact=TRUE) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with binary mediator and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8.5, fig.height=5
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, dpi=300, file=c("out/medflex.med.mod.out.main.3.secu.wototal.plot.png"))

#' ### 2p Moderator, 5p Mediator and 9p Outcome
di.MMR.sub <- neImpute(update(cancel_aid ~ treat_China + med_secu + treat_China:threat.CHN + med_secu:threat.CHN, fcv),
                        family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.sub <- neModel(update(cancel_aid ~ treat_China0 + treat_China1 + treat_China0:threat.CHN + treat_China1:threat.CHN, fcv), 
                      family = "gaussian", expData = di.MMR.sub, se = "robust")
di.PHL.sub <- neImpute(update(cancel_aid ~ treat_China + med_secu + treat_China:threat.CHN + med_secu:threat.CHN, fcv),
                        family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.sub <- neModel(update(cancel_aid ~ treat_China0 + treat_China1 + treat_China0:threat.CHN + treat_China1:threat.CHN, fcv), 
                      family = "gaussian", expData = di.PHL.sub, se = "robust")

# Data
de.sub <- rbind(modex(m.MMR.sub, "Myanmar", v=1),
                 modex(m.PHL.sub, "Philippines", v=1))

# Plot (with Total Effect)
p <- genplot2(de.sub,"(Mediator: Security Interests)",interact=TRUE) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with 5p mediators and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8.5, fig.height=5
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, dpi=300, file=c("out/medflex.med.mod.out.sub.1.secu.wtotal.plot.png"))

#+
# Data
de.sub.wototal <- de.sub[de.sub$eff != "Total",]

# Plot (without Total Effect)
p <- genplot2(de.sub.wototal,"(Mediator: Security Interests)",interact=TRUE) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with 5p mediators and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8.5, fig.height=5
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, dpi=300, file=c("out/medflex.med.mod.out.sub.1.secu.wototal.plot.png"))

#' ### 3p Moderator, 5p Mediator and 9p Outcome
di.MMR.sub2 <- neImpute(update(cancel_aid ~ treat_China + med_secu + treat_China:threat.CHN.3cat + med_secu:threat.CHN.3cat, fcv),
                         family = "gaussian", nMed = 1, data = d.MMR.sub)
m.MMR.sub2 <- neModel(update(cancel_aid ~ treat_China0 + treat_China1 + treat_China0:threat.CHN.3cat + treat_China1:threat.CHN.3cat, fcv), 
                       family = "gaussian", expData = di.MMR.sub2, se = "robust")
di.PHL.sub2 <- neImpute(update(cancel_aid ~ treat_China + med_secu + treat_China:threat.CHN.3cat + med_secu:threat.CHN.3cat, fcv),
                         family = "gaussian", nMed = 1, data = d.PHL.sub)
m.PHL.sub2 <- neModel(update(cancel_aid ~ treat_China0 + treat_China1 + treat_China0:threat.CHN.3cat + treat_China1:threat.CHN.3cat, fcv), 
                       family = "gaussian", expData = di.PHL.sub2, se = "robust")

# Data
de.sub2 <- rbind(modex(m.MMR.sub2, "Myanmar", v=2),
                  modex(m.PHL.sub2, "Philippines", v=2))

# Plot (with Total Effect)
p <- genplot2(de.sub2,"(Mediator: Security Interests)",interact=TRUE) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with 5p mediators and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8.5, fig.height=5
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, dpi=300, file=c("out/medflex.med.mod.out.sub.3.secu.wtotal.plot.png"))

#+
# Data
de.sub2.wototal <- de.sub2[de.sub2$eff != "Total",]

# Plot (without Total Effect)
p <- genplot2(de.sub2.wototal,"(Mediator: Security Interests)",interact=TRUE) + 
  theme(strip.text = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x = element_text(vjust=-0.5)) + 
  ggtitle(NULL) + xlab("The Decomposition of Total Effect")
p <- plot_footnote(p, "Note: Lines represent 95% confidence intervals calculated using robust standard errors. The mediator model\n         are estimated with 5p mediators and the outcome model is estimated with linear regression through\n         imputation-based apporach in 'medflex' R package.",
                   bottom.expand.rate = 11, align="left", caption=FALSE, show.plot = FALSE)

#+ fig.width=8.5, fig.height=5
grid.draw(p)

#+ eval=FALSE
png_save(p, w=850, h=500, dpi=300, file=c("out/medflex.med.mod.out.sub.3.secu.wototal.plot.png"))

#+ eval=FALSE, echo=FALSE
# Exporting HTML File
# In R Studio
# rmarkdown::render('src/analysis4_medflex.R', rmarkdown::github_document(toc=TRUE), clean=FALSE, encoding = 'UTF-8')
# In Terminal, run:
# Rscript -e "rmarkdown::render('analysis4_medflex.R', rmarkdown::github_document(toc=TRUE), clean=FALSE, encoding = 'UTF-8')"
