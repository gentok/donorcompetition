#' ---
#' title: "Functions used for analysis"
#' author: "Gento Kato"
#' date: "November 20, 2019"
#' ---

#' # Prepare Functions

## Packages
library(readstata13)
library(pbapply)
library(mediation)
library(brant)
library(xtable)
library(ggplot2)
library(estvis) 
## This is Gento's Custom Package (Under development)
## Use devtools::install_github("gentok/estvis", force=TRUE)
library(grid)
library(lmtest)
library(sandwich)
library(texreg)
library(ggpubr)
library(mlogit)
library(xml2)
library(multcomp)

#' # Set Formulas

# Mediation
fm.econ <- formula(med_econ ~ treat_China)
fm.secu <- formula(med_secu ~ treat_China)
fm.repu <- formula(med_repu ~ treat_China)
fm.effi <- formula(med_effi ~ treat_China)
# Outcome
fo.econ <- formula(out ~ med_econ + treat_China)
fo.secu <- formula(out ~ med_secu + treat_China)
fo.repu <- formula(out ~ med_repu + treat_China)
fo.effi <- formula(out ~ med_effi + treat_China)
# Outcome - mediator interaction
fox.econ <- formula(out ~ med_econ * treat_China)
fox.secu <- formula(out ~ med_secu * treat_China)
fox.repu <- formula(out ~ med_repu * treat_China)
fox.effi <- formula(out ~ med_effi * treat_China)
# Covariates
fcv <- formula(. ~ . + threat + imp + potential + issint + odaimp + fem + age + ide3)
fcv2 <- formula(. ~ . + threat + imp + potential + issint + odaimp + fem + age)
fcvml <- mFormula(. ~ .|. + threat + imp + potential + issint + odaimp + fem + age + ide3)
fcvml2 <- mFormula(. ~ .|. + threat + imp + potential + issint + odaimp + fem + age)
# Moderated Models
fm.mod.econ <- formula(med_econ ~ treat_China * mod)
fm.mod.secu <- formula(med_secu ~ treat_China * mod)
fm.mod.repu <- formula(med_repu ~ treat_China * mod)
fm.mod.effi <- formula(med_effi ~ treat_China * mod)
fo.mod.econ <- formula(out ~ med_econ * mod + treat_China * mod)
fo.mod.secu <- formula(out ~ med_secu * mod + treat_China * mod)
fo.mod.repu <- formula(out ~ med_repu * mod + treat_China * mod)
fo.mod.effi <- formula(out ~ med_effi * mod + treat_China * mod)

## Variable Names
basevn <- c("(Intercept)",
            "Treatment (Donor Competition)",
            "Recipient's Threat (Moderate)",
            "Recipient's Threat (High)",
            "Recipient's Importance (Moderate)",
            "Recipient's Importance (High)",
            "Recipient's Potential (Moderate)",
            "Recipient's Potential (High)",
            "Political Interest (Moderate)",
            "Political Interest (High)",
            "ODA Important",
            "Gender (Female)",
            "Age",
            "Ideology (Moderate)",
            "Ideology (Right)")

#' # Adjust P-value (delete ***p<.001)

adjpval <- function(filename) {
  tmp <- read_html(filename)
  tmp <- as.character(tmp)
  tmp <- sub('<sup style="vertical-align: 0px;">***</sup>p &lt; 0.001, ',
             '', tmp, fixed = TRUE)
  tmp <- gsub('***','**', tmp, fixed = TRUE)
  tmp <- read_html(tmp)
  write_html(tmp, filename, encoding = "CP1252")
}

#' # Mediation Analysis Function

gen.med.out <- function(d.sub, outvar, simsn, filename,
                        chosen_med = c(1,2,3,4),
                        models = c("logit","gaussian"),
                        medcats = 5, # or 3 or 2
                        interaction = FALSE,
                        moderated = FALSE, 
                        modvar = "threat.CHN",
                        modfix = NULL){ 
  if (interaction==TRUE) {
    fo.econ <- fox.econ
    fo.secu <- fox.secu
    fo.repu <- fox.repu
    fo.effi <- fox.effi
  } else if (moderated==TRUE) {
    fo.econ <- fo.mod.econ
    fo.secu <- fo.mod.secu
    fo.repu <- fo.mod.repu
    fo.effi <- fo.mod.effi
    fm.econ <- fm.mod.econ
    fm.secu <- fm.mod.secu
    fm.repu <- fm.mod.repu
    fm.effi <- fm.mod.effi
    if (modvar %in% c("ide3","right")) fcv <- fcv2
  }
  
  vars <- c(all.vars(update(fm.econ,fcv)),
            "out","med_secu","med_repu","med_effi")
  d.sub$out <- d.sub[,outvar]
  if (moderated==TRUE) {
    d.sub$mod <- d.sub[,modvar]
  }
  d.sub <- d.sub[,vars]
  d.sub <- na.omit(d.sub)
  
  if (medcats==3) {
    d.sub$med_econ <- as.factor(ifelse(d.sub$med_econ!=3,
                                       ifelse(d.sub$med_econ%in%c(1,2),1,3),2))
    d.sub$med_secu <- as.factor(ifelse(d.sub$med_secu!=3,
                                       ifelse(d.sub$med_secu%in%c(1,2),1,3),2))
    d.sub$med_repu <- as.factor(ifelse(d.sub$med_repu!=3,
                                       ifelse(d.sub$med_repu%in%c(1,2),1,3),2))
    d.sub$med_effi <- as.factor(ifelse(d.sub$med_effi!=3,
                                       ifelse(d.sub$med_effi%in%c(1,2),1,3),2))
  } else if (medcats==2) {
    d.sub$med_econ <- ifelse(d.sub$med_econ%in%c(4,5),1,0)
    d.sub$med_secu <- ifelse(d.sub$med_secu%in%c(4,5),1,0)
    d.sub$med_repu <- ifelse(d.sub$med_repu%in%c(4,5),1,0)
    d.sub$med_effi <- ifelse(d.sub$med_effi%in%c(4,5),1,0)
  }
  
  if(models[1]=="ologit"){
    med.m1 <- polr(update(fm.econ,fcv), data=d.sub, method="logistic",Hess=TRUE)
    med.m2 <- polr(update(fm.secu,fcv), data=d.sub, method="logistic",Hess=TRUE)
    med.m3 <- polr(update(fm.repu,fcv), data=d.sub, method="logistic",Hess=TRUE)
    med.m4 <- polr(update(fm.effi,fcv), data=d.sub, method="logistic",Hess=TRUE)
  } else if (models[1]=="gaussian") {
    d.sub$med_econ <- as.numeric(as.character(d.sub$med_econ))
    d.sub$med_secu <- as.numeric(as.character(d.sub$med_secu))
    d.sub$med_repu <- as.numeric(as.character(d.sub$med_repu))
    d.sub$med_effi <- as.numeric(as.character(d.sub$med_effi))
    med.m1 <- lm(update(fm.econ,fcv), data=d.sub)
    med.m2 <- lm(update(fm.secu,fcv), data=d.sub)
    med.m3 <- lm(update(fm.repu,fcv), data=d.sub)
    med.m4 <- lm(update(fm.effi,fcv), data=d.sub)
  } else if (models[1]=="logit") {
    med.m1 <- glm(update(fm.econ,fcv), data=d.sub, family=binomial("logit"))
    med.m2 <- glm(update(fm.secu,fcv), data=d.sub, family=binomial("logit"))
    med.m3 <- glm(update(fm.repu,fcv), data=d.sub, family=binomial("logit"))
    med.m4 <- glm(update(fm.effi,fcv), data=d.sub, family=binomial("logit"))
  }
  
  if(models[2]=="ologit"){
    out.m1 <- polr(update(fo.econ,fcv), data=d.sub, method="logistic",Hess=TRUE)
    out.m2 <- polr(update(fo.secu,fcv), data=d.sub, method="logistic",Hess=TRUE)
    out.m3 <- polr(update(fo.repu,fcv), data=d.sub, method="logistic",Hess=TRUE)
    out.m4 <- polr(update(fo.effi,fcv), data=d.sub, method="logistic",Hess=TRUE)
  } else if (models[2]=="gaussian") {
    d.sub$out <- as.numeric(as.character(d.sub$out))
    out.m1 <- lm(update(fo.econ,fcv), data=d.sub)
    out.m2 <- lm(update(fo.secu,fcv), data=d.sub)
    out.m3 <- lm(update(fo.repu,fcv), data=d.sub)
    out.m4 <- lm(update(fo.effi,fcv), data=d.sub)
  } else if (models[2]=="logit") {
    out.m1 <- glm(update(fo.econ,fcv), data=d.sub, family=binomial("logit"))
    out.m2 <- glm(update(fo.secu,fcv), data=d.sub, family=binomial("logit"))
    out.m3 <- glm(update(fo.repu,fcv), data=d.sub, family=binomial("logit"))
    out.m4 <- glm(update(fo.effi,fcv), data=d.sub, family=binomial("logit"))
  }
  
  
  mediators <- c("med_econ","med_secu","med_repu","med_effi")
  med.list <- list(med.m1,med.m2,med.m3,med.m4)
  out.list <- list(out.m1,out.m2,out.m3,out.m4)
  save(med.list,out.list,file=filename)
  cat("\n Original Models Saved \n")
  
  med.out.list <- list()
  cat("\n Conducting Mediation Analysis... \n")
  bootval <- FALSE
  if (models[2]=="ologit") bootval = TRUE
  pb <- timerProgressBar(min = 0, max = length(chosen_med), style = 3)
  #setTxtProgressBar(pb, 0)
  if (is.null(modfix)) {
    for(i in chosen_med){
      set.seed(56789)
      med.out.list[[i]] <- mediate(med.list[[i]], out.list[[i]],
                                   treat = "treat_China", mediator = mediators[[i]],
                                   robustSE=TRUE, boot = bootval, sims = simsn, type="HC1")
      save(med.list,out.list,med.out.list,file=filename)
      Sys.sleep(0.1)
      setTimerProgressBar(pb, which(chosen_med==i))
    }
  } else {
    for(i in chosen_med){
      set.seed(56789)
      med.out.list[[i]] <- mediate(med.list[[i]], out.list[[i]],
                                   treat = "treat_China", mediator = mediators[[i]],
                                   robustSE=TRUE, boot = bootval, sims = simsn,
                                   covariates = list(mod =modfix), type="HC1")
      save(med.list,out.list,med.out.list,file=filename)
      Sys.sleep(0.1)
      setTimerProgressBar(pb, which(chosen_med==i))
    }
  }
  
  
  return(list(med.out=med.out.list, med=med.list[chosen_med], out=out.list[chosen_med]))
}

#' # Generating Data from Mediation Analysis Object
gendata <- function(med.out.MMR,med.out.PHL,mod=FALSE,modN=NULL,modval=NULL,
                    treated = TRUE) {
  
  gendata1 <- function(med.out,country,mod,modN,modval) {
    
    # Initiate Data
    res <- data.frame(est=numeric(),loCI=numeric(),upCI=numeric(),p=numeric(),
                      med=character(),eff=character())
    
    ## Treatment -> Mediator ##
    # Securirty
    tmp <- coeftest(med.out$med[[2]], vcov.=vcovHC(med.out$med[[2]]))[2,]
    if (mod==TRUE) {
      if (modN==2 & modval==1) {
        tmp <- coeftest(glht(med.out$med[[2]],
                            linfct = c("treat_China + treat_China:mod = 0"),
                            vcov.=vcovHC(med.out$med[[2]])))[1,]
      } else if (modN>=3 & modval>=1) {
        tmp <- coeftest(glht(med.out$med[[2]],
                             linfct = c(paste0("treat_China + treat_China:mod", modval, " = 0")),
                             vcov.=vcovHC(med.out$med[[2]])))[1,]      
      }
    }
    res <- rbind(res,
                 data.frame(est=tmp[1],
                            loCI=tmp[1]+qnorm(0.025)*tmp[2],
                            upCI=tmp[1]+qnorm(0.975)*tmp[2],
                            p=tmp[4],
                            med="Security",
                            eff="Treatment -> Mediator"))
    # Economy
    tmp <- coeftest(med.out$med[[1]], vcov.=vcovHC(med.out$med[[1]]))[2,]
    if (mod==TRUE) {
      if (modN==2 & modval==1) {
        tmp <- coeftest(glht(med.out$med[[1]],
                             linfct = c("treat_China + treat_China:mod = 0"),
                             vcov.=vcovHC(med.out$med[[1]])))[1,]
      } else if (modN>=3 & modval>=1) {
        tmp <- coeftest(glht(med.out$med[[1]],
                             linfct = c(paste0("treat_China + treat_China:mod", modval, " = 0")),
                             vcov.=vcovHC(med.out$med[[1]])))[1,]      
      }
    }
    res <- rbind(res,
                 data.frame(est=tmp[1],
                            loCI=tmp[1]+qnorm(0.025)*tmp[2],
                            upCI=tmp[1]+qnorm(0.975)*tmp[2],
                            p=tmp[4],
                            med="Economy",
                            eff="Treatment -> Mediator"))
    # Reputation
    tmp <- coeftest(med.out$med[[3]], vcov.=vcovHC(med.out$med[[3]]))[2,]
    if (mod==TRUE) {
      if (modN==2 & modval==1) {
        tmp <- coeftest(glht(med.out$med[[3]],
                             linfct = c("treat_China + treat_China:mod = 0"),
                             vcov.=vcovHC(med.out$med[[3]])))[1,]
      } else if (modN>=3 & modval>=1) {
        tmp <- coeftest(glht(med.out$med[[3]],
                             linfct = c(paste0("treat_China + treat_China:mod", modval, " = 0")),
                             vcov.=vcovHC(med.out$med[[3]])))[1,]      
      }
    }
    res <- rbind(res,
                 data.frame(est=tmp[1],
                            loCI=tmp[1]+qnorm(0.025)*tmp[2],
                            upCI=tmp[1]+qnorm(0.975)*tmp[2],
                            p=tmp[4],
                            med="Reputation",
                            eff="Treatment -> Mediator"))
    # Efficacy
    tmp <- coeftest(med.out$med[[4]], vcov.=vcovHC(med.out$med[[4]]))[2,]
    if (mod==TRUE) {
      if (modN==2 & modval==1) {
        tmp <- coeftest(glht(med.out$med[[4]],
                             linfct = c("treat_China + treat_China:mod = 0"),
                             vcov.=vcovHC(med.out$med[[4]])))[1,]
      } else if (modN>=3 & modval>=1) {
        tmp <- coeftest(glht(med.out$med[[4]],
                             linfct = c(paste0("treat_China + treat_China:mod", modval, " = 0")),
                             vcov.=vcovHC(med.out$med[[4]])))[1,]      
      }
    }
    res <- rbind(res,
                 data.frame(est=tmp[1],
                            loCI=tmp[1]+qnorm(0.025)*tmp[2],
                            upCI=tmp[1]+qnorm(0.975)*tmp[2],
                            p=tmp[4],
                            med="Efficacy",
                            eff="Treatment -> Mediator"))
    
    ## Mediator -> Outcome ##
    # Security
    tmp <- coeftest(med.out$out[[2]], vcov.=vcovHC(med.out$out[[2]]))[2,]
    if (mod==TRUE) {
      if (modN==2 & modval==1) {
        tmp <- coeftest(glht(med.out$out[[2]],
                             linfct = c("med_secu + med_secu:mod = 0"),
                             vcov.=vcovHC(med.out$out[[2]])))[1,]
      } else if (modN>=3 & modval>=1) {
        tmp <- coeftest(glht(med.out$out[[2]],
                             linfct = c(paste0("med_secu + med_secu:mod", modval, " = 0")),
                             vcov.=vcovHC(med.out$out[[2]])))[1,]      
      }
    }
    res <- rbind(res,
                 data.frame(est=tmp[1],
                            loCI=tmp[1]+qnorm(0.025)*tmp[2],
                            upCI=tmp[1]+qnorm(0.975)*tmp[2],
                            p=tmp[4],
                            med="Security",
                            eff="Mediator -> Outcome"))
    # Economy
    tmp <- coeftest(med.out$out[[1]], vcov.=vcovHC(med.out$out[[1]]))[2,]
    if (mod==TRUE) {
      if (modN==2 & modval==1) {
        tmp <- coeftest(glht(med.out$out[[1]],
                             linfct = c("med_econ + med_econ:mod = 0"),
                             vcov.=vcovHC(med.out$out[[1]])))[1,]
      } else if (modN>=3 & modval>=1) {
        tmp <- coeftest(glht(med.out$out[[1]],
                             linfct = c(paste0("med_econ + med_econ:mod", modval, " = 0")),
                             vcov.=vcovHC(med.out$out[[1]])))[1,]      
      }
    }
    res <- rbind(res,
                 data.frame(est=tmp[1],
                            loCI=tmp[1]+qnorm(0.025)*tmp[2],
                            upCI=tmp[1]+qnorm(0.975)*tmp[2],
                            p=tmp[4],
                            med="Economy",
                            eff="Mediator -> Outcome"))
    # Reputation
    tmp <- coeftest(med.out$out[[3]], vcov.=vcovHC(med.out$out[[3]]))[2,]
    if (mod==TRUE) {
      if (modN==2 & modval==1) {
        tmp <- coeftest(glht(med.out$out[[3]],
                             linfct = c("med_repu + med_repu:mod = 0"),
                             vcov.=vcovHC(med.out$out[[3]])))[1,]
      } else if (modN>=3 & modval>=1) {
        tmp <- coeftest(glht(med.out$out[[3]],
                             linfct = c(paste0("med_repu + med_repu:mod", modval, " = 0")),
                             vcov.=vcovHC(med.out$out[[3]])))[1,]      
      }
    }
    res <- rbind(res,
                 data.frame(est=tmp[1],
                            loCI=tmp[1]+qnorm(0.025)*tmp[2],
                            upCI=tmp[1]+qnorm(0.975)*tmp[2],
                            p=tmp[4],
                            med="Reputation",
                            eff="Mediator -> Outcome"))
    # Efficacy
    tmp <- coeftest(med.out$out[[4]], vcov.=vcovHC(med.out$out[[4]]))[2,]
    if (mod==TRUE) {
      if (modN==2 & modval==1) {
        tmp <- coeftest(glht(med.out$out[[4]],
                             linfct = c("med_effi + med_effi:mod = 0"),
                             vcov.=vcovHC(med.out$out[[4]])))[1,]
      } else if (modN>=3 & modval>=1) {
        tmp <- coeftest(glht(med.out$out[[4]],
                             linfct = c(paste0("med_effi + med_effi:mod", modval, " = 0")),
                             vcov.=vcovHC(med.out$out[[4]])))[1,]      
      }
    }
    res <- rbind(res,
                 data.frame(est=tmp[1],
                            loCI=tmp[1]+qnorm(0.025)*tmp[2],
                            upCI=tmp[1]+qnorm(0.975)*tmp[2],
                            p=tmp[4],
                            med="Efficacy",
                            eff="Mediator -> Outcome"))
    
    if (treated==TRUE) {
      ## Mediated Effect ##
      # Security
      tmp <- with(med.out$med.out[[2]], data.frame(d1,t(d1.ci),d1.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Security"; tmp$eff <- "Treatment -> Med. -> Out."
      res <- rbind(res,tmp)
      # Economy
      tmp <- with(med.out$med.out[[1]], data.frame(d1,t(d1.ci),d1.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Economy"; tmp$eff <- "Treatment -> Med. -> Out."
      res <- rbind(res,tmp)
      # Reputation
      tmp <- with(med.out$med.out[[3]], data.frame(d1,t(d1.ci),d1.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Reputation"; tmp$eff <- "Treatment -> Med. -> Out."
      res <- rbind(res,tmp)
      # Efficacy
      tmp <- with(med.out$med.out[[4]], data.frame(d1,t(d1.ci),d1.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Efficacy"; tmp$eff <- "Treatment -> Med. -> Out."
      res <- rbind(res,tmp)
      
      ## Direct Effect ##
      # Security
      tmp <- with(med.out$med.out[[2]], data.frame(z1,t(z1.ci),z1.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Security"; tmp$eff <- "Treatment -> Outcome"
      res <- rbind(res,tmp)
      # Economy
      tmp <- with(med.out$med.out[[1]], data.frame(z1,t(z1.ci),z1.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Economy"; tmp$eff <- "Treatment -> Outcome"
      res <- rbind(res,tmp)
      # Reputation
      tmp <- with(med.out$med.out[[3]], data.frame(z1,t(z1.ci),z1.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Reputation"; tmp$eff <- "Treatment -> Outcome"
      res <- rbind(res,tmp)
      # Efficacy
      tmp <- with(med.out$med.out[[4]], data.frame(z1,t(z1.ci),z1.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Efficacy"; tmp$eff <- "Treatment -> Outcome"
      res <- rbind(res,tmp)
    } else {
      ## Mediated Effect ##
      # Security
      tmp <- with(med.out$med.out[[2]], data.frame(d0,t(d0.ci),d0.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Security"; tmp$eff <- "Treatment -> Med. -> Out."
      res <- rbind(res,tmp)
      # Economy
      tmp <- with(med.out$med.out[[1]], data.frame(d0,t(d0.ci),d0.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Economy"; tmp$eff <- "Treatment -> Med. -> Out."
      res <- rbind(res,tmp)
      # Reputation
      tmp <- with(med.out$med.out[[3]], data.frame(d0,t(d0.ci),d0.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Reputation"; tmp$eff <- "Treatment -> Med. -> Out."
      res <- rbind(res,tmp)
      # Efficacy
      tmp <- with(med.out$med.out[[4]], data.frame(d0,t(d0.ci),d0.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Efficacy"; tmp$eff <- "Treatment -> Med. -> Out."
      res <- rbind(res,tmp)
      
      ## Direct Effect ##
      # Security
      tmp <- with(med.out$med.out[[2]], data.frame(z0,t(z0.ci),z0.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Security"; tmp$eff <- "Treatment -> Outcome"
      res <- rbind(res,tmp)
      # Economy
      tmp <- with(med.out$med.out[[1]], data.frame(z0,t(z0.ci),z0.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Economy"; tmp$eff <- "Treatment -> Outcome"
      res <- rbind(res,tmp)
      # Reputation
      tmp <- with(med.out$med.out[[3]], data.frame(z0,t(z0.ci),z0.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Reputation"; tmp$eff <- "Treatment -> Outcome"
      res <- rbind(res,tmp)
      # Efficacy
      tmp <- with(med.out$med.out[[4]], data.frame(z0,t(z0.ci),z0.p))
      names(tmp) <- c("est","loCI","upCI","p")
      tmp$med <- "Efficacy"; tmp$eff <- "Treatment -> Outcome"
      res <- rbind(res,tmp)
    }
  
    ## Total Effect ##
    # Security
    tmp <- with(med.out$med.out[[2]], data.frame(tau.coef,t(tau.ci),tau.p))
    names(tmp) <- c("est","loCI","upCI","p")
    tmp$med <- "Security"; tmp$eff <- "Total"
    res <- rbind(res,tmp)
    # Economy
    tmp <- with(med.out$med.out[[1]], data.frame(tau.coef,t(tau.ci),tau.p))
    names(tmp) <- c("est","loCI","upCI","p")
    tmp$med <- "Economy"; tmp$eff <- "Total"
    res <- rbind(res,tmp)
    # Reputation
    tmp <- with(med.out$med.out[[3]], data.frame(tau.coef,t(tau.ci),tau.p))
    names(tmp) <- c("est","loCI","upCI","p")
    tmp$med <- "Reputation"; tmp$eff <- "Total"
    res <- rbind(res,tmp)
    # Efficacy
    tmp <- with(med.out$med.out[[4]], data.frame(tau.coef,t(tau.ci),tau.p))
    names(tmp) <- c("est","loCI","upCI","p")
    tmp$med <- "Efficacy"; tmp$eff <- "Total"
    res <- rbind(res,tmp)
    
    # Convert Character into Factor
    res$med <- factor(res$med,levels=rev(c("Security","Economy","Reputation","Efficacy")))
    res$eff <- factor(res$eff,levels=unique(res$eff))
    
    # P-value Variable
    res$p05 <- ifelse(res$p <= 0.05,1,0)
    res$p10 <- ifelse(res$p <= 0.1,1,0)
    res$pcat <- "p >= .1"
    res$pcat[res$p10==1] <- "p < .1"
    res$pcat[res$p05==1] <- "p < .05"
    res$pcat <- factor(res$pcat,levels=c("p < .05","p < .1", "p >= .1"))
    
    # Country
    res$country <- country
    
    return(res)
    
  }
  
  res <- rbind(gendata1(med.out.MMR, "Myanmar",mod,modN,modval),
               gendata1(med.out.PHL, "Philippines",mod,modN,modval))
  
  res$country <- factor(res$country,levels=unique(res$country))
  
  return(res)
  
  
}

#' # Generating Plots

# run this code before calling ggplot2 function
guides_merge <- function(gdefs) {
  gdefs <- lapply(gdefs, function(g) { g$hash <- paste(g$order, g$hash, sep = "z"); g})
  tapply(gdefs, sapply(gdefs, function(g)g$hash), function(gs)Reduce(guide_merge, gs))
}
environment(guides_merge) <- environment(ggplot)
assignInNamespace("guides_merge", guides_merge, pos = "package:ggplot2")

genplot <- function(med.out.v0.data, 
                    titletxt=NULL,
                    captiontxt=NULL,
                    odds=FALSE, #interact=FALSE,
                    include.eff = c("Treatment -> Mediator",
                                    "Mediator -> Outcome",
                                    "Treatment -> Med. -> Out."),
                    est.type = c("Logit Coefficient",
                                 "OLS Coefficient",
                                            "Av. Causal Mediation Effect")){
  
  if(length(include.eff)!=length(est.type)) stop("include.eff and est.type must have the same length!")
  
  facet_formula <- formula(.~country)
  # if (interact==TRUE) {
  #   facet_formula <- formula(tcond~country)
  # }
  fixhline <- 0
  if (odds==TRUE) {
    fixhline <- 1
  }
  med.out.v0.data <- med.out.v0.data[med.out.v0.data$eff%in%include.eff,]
  pvallist <- which(as.vector(table(med.out.v0.data$pcat))>0)
  med.out.v0.data$efflab <- as.character(med.out.v0.data$eff)
  for (i in 1:length(include.eff)) {
    med.out.v0.data$efflab[med.out.v0.data$eff==include.eff[i]] <- 
      paste0(include.eff[i],"\n(",est.type[i],")")
  }
  med.out.v0.data$efflab <- factor(med.out.v0.data$efflab, levels=paste0(include.eff,"\n(",est.type,")"))
  collen <- length(table(med.out.v0.data$med))
  p <- ggplot(med.out.v0.data, aes(x=med,y=est)) + 
    geom_hline(yintercept=fixhline, linetype=2) + 
    geom_errorbar(aes(ymin=loCI,ymax=upCI,
                      color=med, linetype=pcat),
                  size = 0.7, width=0.3,
                  position = position_dodge(0.9)) + 
    geom_point(aes(shape=med),
               size = 2, 
               position = position_dodge(0.9)) + 
    scale_shape_manual(name="Mediators",values=rev(c(16,17,15,3,7,8)[seq(1,length(unique(med.out.v0.data$med)))])) + 
    scale_color_manual(name="Mediators", values=rep("Black",collen)) + 
    scale_linetype_manual(name="Significance", values=pvallist) +
    guides( color = guide_legend(order = 1, reverse=TRUE,nrow=2,byrow=TRUE),
            shape = guide_legend(order = 1, reverse=TRUE,nrow=2,byrow=TRUE),
            linetype = guide_legend(order = 2,nrow=2,byrow=TRUE)) +
    theme_bw() + 
    ylab("(Lines Represent 95% Confidence Intervals)") + 
    xlab(NULL) + 
    ggtitle(titletxt) + 
    facet_grid(country~efflab, scales="free", switch="y") + coord_flip() +
    theme(plot.title=element_text(hjust=0.5),
          strip.text.x = element_text(face="bold"),
          strip.text.y = element_text(angle=270,hjust=0.5,face="bold",size=11),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background.y = element_blank(),
          panel.border = element_rect(colour = "black"),
          legend.position="top") + 
    labs(caption=captiontxt) + 
    theme(plot.caption = element_text(hjust=0))
  
  return(p)
} 

#' # Generating Plots (for only plotting security plots)

genplot2 <- function(med.out.v0.data, 
                     titletxt=NULL,
                     captiontxt=NULL,
                     legendrow=2,
                     odds=FALSE, #interact=FALSE,
                    include.eff = c("Treatment -> Mediator",
                                    "Mediator -> Outcome",
                                    "Treatment -> Med. -> Out."),
                    est.type = c("Logit Coefficient",
                                 "OLS Coefficient",
                                 "Av. Causal Mediation Effect")){
  
  if(length(include.eff)!=length(est.type)) stop("include.eff and est.type must have the same length!")
  
  facet_formula <- formula(.~country)
  # if (interact==TRUE) {
  #   facet_formula <- formula(tcond~country)
  # }
  fixhline <- 0
  if (odds==TRUE) {
    fixhline <- 1
  }
  med.out.v0.data <- med.out.v0.data[med.out.v0.data$eff%in%include.eff,]
  pvallist <- which(as.vector(table(med.out.v0.data$pcat))>0)
  med.out.v0.data$efflab <- as.character(med.out.v0.data$eff)
  for (i in 1:length(include.eff)) {
    med.out.v0.data$efflab[med.out.v0.data$eff==include.eff[i]] <- 
      paste0(include.eff[i],"\n(",est.type[i],")")
  }
  med.out.v0.data$efflab <- factor(med.out.v0.data$efflab, levels=paste0(include.eff,"\n(",est.type,")"))
  collen <- length(table(med.out.v0.data$tcond))
  p <- ggplot(med.out.v0.data, aes(x=med,y=est)) + 
    geom_hline(yintercept=fixhline, linetype=2) + 
    geom_errorbar(aes(ymin=loCI,ymax=upCI,
                      color=tcond, linetype=pcat),
                  size = 0.7, width=0.3,
                  position = position_dodge(0.9)) + 
    geom_point(aes(shape=tcond),
               size = 2, 
               position = position_dodge(0.9)) + 
    scale_shape_manual(name="China\nThreat",values=rev(c(16,17,15,3,7,8)[seq(1,length(unique(med.out.v0.data$tcond)))])) + 
    scale_color_manual(name="China\nThreat", values=rep("Black",collen)) + 
    scale_linetype_manual(name="Significance", values=pvallist) +
    guides( color = guide_legend(order = 1, reverse=TRUE,nrow=legendrow,byrow=TRUE),
            shape = guide_legend(order = 1, reverse=TRUE,nrow=legendrow,byrow=TRUE),
            linetype = guide_legend(order = 2,nrow=legendrow,byrow=TRUE)) +
    theme_bw() + 
    ylab("(Lines Represent 95% Confidence Intervals)") + 
    xlab(NULL) + 
    ggtitle(titletxt) + 
    facet_grid(country~efflab, scales="free", switch="y") + coord_flip() +
    theme(plot.title=element_text(hjust=0.5),
          strip.text.x = element_text(face="bold"),
          strip.text.y = element_text(angle=270,hjust=0.5,face="bold",size=11),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background.y = element_blank(),
          panel.border = element_rect(colour = "black"),
          legend.position="top") + 
    labs(caption=captiontxt) + 
    theme(plot.caption = element_text(hjust=0))
  
  return(p)
} 

#' # Checking Balance
checkbal <- function(dtlist, dtnames, robust=FALSE){
  
  restab <- list()
  datasub <- list()
  for(i in 1:length(dtlist)){
    
    dt <- dtlist[[i]]
    
    dmod <- data.frame(
      threat0 = ifelse(dt$threat==0,1,0),
      threat1 = ifelse(dt$threat==1,1,0),
      threat2 = ifelse(dt$threat==2,1,0),
      imp0 = ifelse(dt$imp==0,1,0),
      imp1 = ifelse(dt$imp==1,1,0),
      imp2 = ifelse(dt$imp==2,1,0),
      potential0 = ifelse(dt$potential==0,1,0),
      potential1 = ifelse(dt$potential==1,1,0),
      potential2 = ifelse(dt$potential==2,1,0),
      issint0 = ifelse(dt$issint==0,1,0),
      issint1 = ifelse(dt$issint==1,1,0),
      issint2 = ifelse(dt$issint==2,1,0),
      odaimp0 = ifelse(dt$odaimp==0,1,0),
      odaimp1 = dt$odaimp,
      fem0 = ifelse(dt$fem==0,1,0),
      fem1 = dt$fem,
      age = dt$age/10,
      ide30 = ifelse(dt$ide3==0,1,0),
      ide31 = ifelse(dt$ide3==1,1,0),
      ide32 = ifelse(dt$ide3==2,1,0)
    )
    
    if (robust==TRUE){
      res1 <- apply(dmod, 2, function(x) coeftest(lm(x~dt$treat_China),vcov.=vcovHC(lm(x~dt$treat_China),type="HC1")))
    } else {
      res1 <- apply(dmod, 2, function(x) coeftest(lm(x~dt$treat_China)))
    }
    
    
    res2 <- as.data.frame(t(apply(res1,2,function(x) c(x[2],x[4],x[8]))))
    colnames(res2) <- c("coef","se","pval")
    res2$lCI <- res2$coef - 1.96*res2$se
    res2$uCI <- res2$coef + 1.96*res2$se
    
    res3 <- data.frame(Variable=
                         rep(
                           c("Recipient's Threat (Low)",
                             "Recipient's Threat (Moderate)",
                             "Recipient's Threat (High)",
                             "Recipient's Importance (Low)",
                             "Recipient's Importance (Moderate)",
                             "Recipient's Importance (High)",
                             "Recipient's Potential (Low)",
                             "Recipient's Potential (Moderate)",
                             "Recipient's Potential (High)",
                             "Political Interest (Low)",
                             "Political Interest (Moderate)",
                             "Political Interest (High)",
                             "ODA Not Important",
                             "ODA Important",
                             "Gender (Male)",
                             "Gender (Female)",
                             "Age (By 10 Years)",
                             "Ideology (Left)",
                             "Ideology (Moderate)",
                             "Ideology (Right)"),2),
                       stat = c(res2$coef,res2$pval),
                       lCI = c(res2$lCI,rep(NA,nrow(res2))),
                       uCI = c(res2$uCI,rep(NA,nrow(res2))),
                       val = rep(c("Treated - Control","p-value"),each=20)
    )
    res3$Variable <- factor(res3$Variable,levels=rev(unique(res3$Variable)))
    res3$val <- factor(res3$val,levels=unique(res3$val))
    restab[[i]] <- res3
    
  }
  
  if(length(dtlist)==1){
    restab <- restab[[1]]
  } else {
    restabtemp <- restab[[1]]
    restabtemp$recipient <- dtnames[1]
    for (i in 2:length(dtlist)){
      restabtemp2 <- restab[[i]]
      restabtemp2$recipient <- dtnames[i]
      restabtemp <- rbind(restabtemp,restabtemp2)
    }
    restab <- restabtemp
  }
  
  data2 <- data.frame(val=c("Treated - Control","p-value"),
                      vloc1=c(NA,0),
                      vloc2=c(0,0.1))
  data2$val <- factor(data2$val,levels=unique(data2$val))
  
  p <- ggplot(restab, aes(x=Variable,y=stat)) + 
    geom_errorbar(aes(ymin=lCI,ymax=uCI, color=recipient),width=0.5, 
                  position=position_dodge(width=-0.7)) +
    geom_point(aes(shape=recipient, color=recipient), 
               position=position_dodge(width=-0.7)) + 
    geom_hline(data=data2, aes(yintercept=vloc1), linetype=1) +
    geom_hline(data=data2, aes(yintercept=vloc2), linetype=2) +
    scale_y_continuous(breaks=c(-0.1,0,0.1,0.3,0.6,0.9)) +
    scale_shape_discrete(name="Recipient") + 
    scale_color_manual(name="Recipient", values=c(1,2)) +
    facet_grid(.~val, scales = "free_x") + coord_flip() + 
    xlab(NULL) + ylab(NULL) + 
    theme_bw() + 
    theme(legend.position = "bottom")
  
  return(p)
}

#' # Simulate Multionomial Logit ATE Predictions
simu_mlogit_ate <- function(m, vcov_est, d.mlogit, treat, 
                            ndraws=1000, cilevel=0.95) {
  
  # Draw Coefficients
  coef_est <- coef(m)
  betadraw <- mvrnorm(ndraws, coef_est, vcov_est)
  
  # Define Profile
  prof.1 <- prof.0 <- d.mlogit
  prof.1[,treat] <- 1
  prof.0[,treat] <- 0
  
  # Making predictions
  eachrun <- function(beta) {
    mx <- m
    mx$coefficients <- beta
    ctrpred <- predict(mx, prof.0)
    trtpred <- predict(mx, prof.1)
    atepred <- trtpred - ctrpred
    pred <- as.data.frame(rbind(colMeans(ctrpred),
                                colMeans(trtpred),
                                colMeans(atepred)))
    row.names(pred) <- c("Control","Treated","ATE")
    return(pred)
  }
  
  # Generating Results
  res <- pbapply(betadraw, 1, eachrun)
  ctrpred <- sapply(res, function(k) as.numeric(k[1,]))
  trtpred <- sapply(res, function(k) as.numeric(k[2,]))
  atepred <- sapply(res, function(k) as.numeric(k[3,]))
  
  # Summarize Results
  ciset <- c((1 - cilevel)/2, 1 - (1 - cilevel)/2) # Confidence Interval Range
  ctrsum <- data.frame(mean = rowMeans(ctrpred),
                       loCI = apply(ctrpred, 1, function(k) quantile(k, probs = ciset[1])),
                       upCI = apply(ctrpred, 1, function(k) quantile(k, probs = ciset[2])))
  rownames(ctrsum) <- colnames(res[[1]])
  trtsum <- data.frame(mean = rowMeans(trtpred),
                       loCI = apply(trtpred, 1, function(k) quantile(k, probs = ciset[1])),
                       upCI = apply(trtpred, 1, function(k) quantile(k, probs = ciset[2])))
  rownames(trtsum) <- colnames(res[[1]])
  atesum <- data.frame(mean = rowMeans(atepred),
                       loCI = apply(atepred, 1, function(k) quantile(k, probs = ciset[1])),
                       upCI = apply(atepred, 1, function(k) quantile(k, probs = ciset[2])))
  rownames(atesum) <- colnames(res[[1]])
  
  resfin <- list(summary_ate = atesum,
                 summary_control = ctrsum,
                 summary_treated = trtsum,
                 simu_ate = atepred, 
                 simu_control = ctrpred,
                 simu_treated = trtpred)
  
  return(resfin)  
}

#' # Function to Simplify the Process of Treatment Effect on Mediator
estrow <- function(m, meth="asis", at=2) {
  a <- as.numeric(coeftest(m, vcov.=vcovHC(m, type="HC1"))[at,])
  b <- as.numeric(coefci(m, vcov.=vcovHC(m, type="HC1"))[at,])
  res <- c(a[1],b, a[4], (a[4]<.05)*1, (a[4]<.10)*1)
  names(res) <- c("est","loCI","upCI","p","p05","p10")
  if (meth == "odds") res[c(1,2,3)] <- exp(res[c(1,2,3)])
  return(res)
}

#' # Function to Simplify the analysis of joint mediation analysis
estrow2 <- function(m, meth="asis", at) {
  a <- as.numeric(coeftest(m)[at,])
  b <- as.numeric(coefci(m)[at,])
  res <- c(a[1],b, a[4], (a[4]<.05)*1, (a[4]<.10)*1)
  names(res) <- c("est","loCI","upCI","p","p05","p10")
  if (meth == "odds") res[c(1,2,3)] <- exp(res[c(1,2,3)])
  return(res)
}

#' # Function to export medflex moderation analysis results (deprecated)
# modex <- function(m, cname, v=1) {
#   
#   v1comp <- c("treat_China1 = 0",
#               "treat_China0 = 0",
#               "treat_China0 + treat_China1 = 0",
#               "treat_China1 + treat_China1:threat.CHN = 0",
#               "treat_China0 + treat_China0:threat.CHN = 0",
#               "treat_China0 + treat_China0:threat.CHN + treat_China1 + treat_China1:threat.CHN = 0")
#   v2comp <- c("treat_China1 = 0",
#               "treat_China0 = 0",
#               "treat_China0 + treat_China1 = 0",
#               "treat_China1 + treat_China1:threat.CHN.3cat1 = 0",
#               "treat_China0 + treat_China0:threat.CHN.3cat1 = 0",
#               "treat_China0 + treat_China0:threat.CHN.3cat1 + treat_China1 + treat_China1:threat.CHN.3cat1 = 0",
#               "treat_China1 + treat_China1:threat.CHN.3cat2 = 0",
#               "treat_China0 + treat_China0:threat.CHN.3cat2 = 0",
#               "treat_China0 + treat_China0:threat.CHN.3cat2 + treat_China1 + treat_China1:threat.CHN.3cat2 = 0")
#   if (v==1) {
#     vcomp <- v1comp
#     effrep <- 2
#     g1n.MMR <- 1285
#     g0n.MMR <- 280
#     g1n.PHL <- 1283
#     g0n.PHL <- 331
#     t1 <- paste("Threatened \n  (N=", g1n.MMR, " for Myanmar) \n  (N=", g1n.PHL, " for Philippines)",sep="")
#     t0 <- paste("\nNot Threatened \n  (N=", g0n.MMR, " for Myanmar) \n  (N=", g0n.PHL, " for Philippines)",sep="")
#     tset <- c(t0,t1)
#   } else if (v==2) {
#     vcomp <- v2comp
#     effrep <- 3
#     g2n.MMR <- 789
#     g1n.MMR <- 496
#     g0n.MMR <- 280
#     g2n.PHL <- 749
#     g1n.PHL <- 534
#     g0n.PHL <- 331
#     t2 <- paste("High \n  (N=", g2n.MMR, " for Myanmar) \n  (N=", g2n.PHL, " for Philippines)",sep="")
#     t1 <- paste("\nModerate \n  (N=", g1n.MMR, " for Myanmar) \n  (N=", g1n.PHL, " for Philippines)",sep="")
#     t0 <- paste("\nLow/None \n  (N=", g0n.MMR, " for Myanmar) \n  (N=", g0n.PHL, " for Philippines)",sep="")
#     tset <- c(t0,t1,t2)
#   } else {
#     stop("invalid v")
#   }
#   
#   lht <- neLht(m,  linfct = vcomp)
#   a <- as.data.frame(cbind(coef(lht), coefci(lht),coeftest(lht)[,4]))
#   colnames(a) <- c("est","loCI","upCI","p")
#   rownames(a) <- seq(1, nrow(a), 1)
#   a$p05 <- ifelse(a$p<.05,1,0)
#   a$p10 <- ifelse(a$p<.1,1,0)
#   a$pcat <- ifelse(a$p10==0,"p >= .1", ifelse(a$p05==0,"p < .1","p < .05"))
#   a$pcat <- factor(a$pcat,levels=c("p < .05","p < .1", "p >= .1"))
#   a$med <- "Security"
#   a$eff <- factor(rep(c("Mediated","Direct","Total"),effrep), levels=c("Mediated","Direct","Total"))
#   a$country <- cname
#   a$tcond <- factor(rep(tset, each=3), levels=tset)
# 
#   return(a)
# }
