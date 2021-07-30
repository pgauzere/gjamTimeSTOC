################################################################################
# PLOT OUTPUT
################################################################################
load("outputs/gjamOutput.RData")
 
library(viridis)
library(tidyverse)
library(gjam)
library(see)


nspecies = length(output$fit$rmspeBySpec)
specColor <- cividis(nspecies+2)[1:nspecies]

# #### all plots
plotPars <- list(GRIDPLOTS=F, PLOTALLY=F, SAVEPLOTS = F)
gjamPlot(output, plotPars)


plotPars <- list(specColor = specColor, GRIDPLOTS=T, PLOTALLY=T, SAVEPLOTS = T,
                 outFolder = 'gjamOutput', width = 8, height = 6)
gjamPlot(output, plotPars)


#### RMSE
output$fit$DIC
output$fit$rmspeBySpec %>% as.tibble() %>% mutate("name" = names(output$fit$rmspeBySpec)) %>%
  ggplot() +
  geom_bar(aes(y = name, x = value), stat = "identity", fill = specColor)+
  theme_modern()

####### equilibrium abundances #########""
# Equilibrium abundances can be evaluated here:
wstar <- .wrapperEquilAbund(output, covars = 'temp',
                            nsim = 100, ngrid = 10, BYFACTOR = T,
                            verbose = T)
save(wstar, file = "wstarSTOC_temp.Rdata")
# load("wstarSTOC.Rdata")

mean <- as.tibble(wstar$ccMu)
mean$temp <- wstar$x[, "temp"]
sd   <- as.tibble(wstar$ccSd)
sd$temp <- wstar$x[, "temp"]


test <- pivot_longer(mean, cols = -temp, values_to = "eq_abundance")
test2 <- pivot_longer(sd, cols = -temp , values_to = "eq_abundanceSD")
test$eq_abundanceSD <- test2$eq_abundanceSD

ggplot(test, aes(y = eq_abundance, x = temp, ymin = eq_abundance - eq_abundanceSD,
                 ymax = eq_abundance + eq_abundanceSD, col = name))+
  geom_pointrange()+
  geom_hline(yintercept = 0, linetype = 3)+
  geom_line()+
  scale_color_viridis_d(guide=F)+
  facet_wrap(~name, scales = "free_y")


###### interaction coefficients ###########
alpha.estimates <- output$parameters$alphaTable
alpha.estimates$to <- NA
alpha.estimates$from <- NA
alpha.estimates[, c("to", "from")] <- stringr::str_split(alpha.estimates$`alpha_{to, from}`, pattern = ", ", simplify = T)
  
ggplot(data=alpha.estimates,aes(y=from,x=to))+
  geom_tile(aes(fill=Estimate, alpha = -SE))+
  scale_fill_viridis_c("Alpha estimate")+
  scale_alpha_continuous(guide = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_fixed()

ggplot(data=alpha.estimates, aes(x = Estimate, fill=..x..))+
  geom_histogram(bins = 50)+
  scale_fill_viridis_c()

###### beta coefficients ###########
beta.estimates <- output$parameters$betaStandXTable[-c(1:nspecies),]
## OR
# beta.estimates <- output$parameters$betaTable[-c(1:nspecies),]
## OR
# beta.estimates <- output$parameters$betaStandXWTable


beta.estimates$species <- NA
beta.estimates$predictor <- NA
beta.estimates[, c("species", "predictor")] <- 
  str_split(rownames(beta.estimates), pattern = "_", simplify = T)

ggplot(beta.estimates, aes(y = species, x = Estimate, xmin = CI_025, xmax = CI_975))+
  geom_pointrange(col = rep(specColor, length(unique(beta.estimates$predictor))))+
  geom_vline(xintercept = 0, linetype = 2)+
  facet_wrap(~predictor, ncol = 1)

###### rho coefficients ###########
rho.estimates <- output$parameters$rhoStandXTable
## OR
# rho.estimates <- output$parameters$rhoTable

rho.estimates$species <- NA
rho.estimates$predictor <- NA  
rho.estimates[, c("species", "predictor")] <- str_split(rho.estimates$`rho_{to, from}`, pattern = ", ", simplify = T)

ggplot(rho.estimates, aes(y = species, x = Estimate, xmin = CI_025, xmax = CI_975))+
  geom_pointrange(col = rep(specColor, each = length(unique(rho.estimates$predictor))))+
  geom_vline(xintercept = 0, linetype = 2)+
  facet_wrap(~predictor, ncol = 1)


###### rho coefficients ###########


