# library(tidyverse)
library(devtools)
library(gjam)
# library(see)
# theme_set(theme_modern())

# library(repmis)
# library(maps)
# library(DescTools)
# d <- "https://github.com/jimclarkatduke/gjam/blob/master/gjamTimeFunctions.R?raw=True"
# source_url(d)

source("gjamTimeFunctions.R")
load("data_for_model_run.Rdata")

################################################################################
# SET PRIORS
################################################################################

#------------------------------------------------------------------------------
# Beta Priors: Density-Independent Environment Effects ( B * x * x' )
#------------------------------------------------------------------------------
# Beta parameter estimates the amount of em-/immigration into/out of each plot
# that is affected by each covariate.

# FORMULAE
formulaB <- as.formula(~ temp)

# PRIORS
priorB <- list(lo = list(temp =-0.1),
               hi = list(temp =0.5) )

#------------------------------------------------------------------------------
# Rho Priors: Density-Dependent Environment-Species Effects ( R * v * v' )
#------------------------------------------------------------------------------
# Rho parameter estimates the amount of population growth that is affected by 
# each covariate. Here, we expect temperature to 
# have a positive effect on species growth.

formulaR <- as.formula(~ temp)
priorR  <- list(lo = list(temp = -0.05),
                hi = list(temp = 0.05))

#------------------------------------------------------------------------------
# Alpha Priors: Density-Independent Species Interactions ( A * w * w' )
#------------------------------------------------------------------------------
# Alpha parameter is a matrix of parameters that estimate the interaction between 
# species, i.e the effect that abundance of species i have on the abundance of 
# species j 

alphaSign <- alpha
alphaSign[alphaSign > -0.2 ] <- 0
alphaSign[alphaSign <= -0.2 ] <- -1

################################################################################
# MODEL PARAMETERS
################################################################################

priorList <- list( formulaBeta = formulaB, formulaRho = formulaR, 
                   betaPrior = priorB, rhoPrior = priorR, alphaSign = alpha)
priors    <- gjamTimePrior(xdata, ydata, edata, priorList)
timeList  <- mergeList(tlist, priors)

# Try setting the ng to 2000 and burnin to 500 for a longer run.
effort <- list(columns = 1:ncol(edata), values = edata)
modelList <- list( typeNames = 'DA', ng = 100, burnin = 10,  
                   timeList = timeList, effort = effort ) 
################################################################################
# RUN THE MODEL
################################################################################
output <- gjam(formulaB, xdata, ydata, modelList)
save(output, file='gjamOutput.RData')

################################################################################
# PLOT OUTPUT
################################################################################
# load("gjamOutputBBS/outputBBS.rdata")
# load("outputSTOC.Rdata")
# outputSTOC <- outputSTOC_2
# 
# library(viridis)
# library(tidyverse)
# #### all plots
# nspecies = ncol(ydata)
# specColor <- viridis(nspecies)
# plotPars <- list(specColor = specColor, GRIDPLOTS=T, PLOTALLY=T, SAVEPLOTS = T,
#                  outFolder = 'gjamOutput', width = 8, height = 6)
# gjamPlot(output, plotPars)
# 
# save(output, file='gjamOutput/gjamOutput.RData')

#### RMSE
# output$fit$DIC
# 
# output$fit$rmspeBySpec %>% as.tibble() %>% mutate("name" = names(output$fit$rmspeBySpec)) %>%
#   ggplot() + 
#   geom_bar(aes(y = name, x = value, fill = name), stat = "identity")+
#   scale_fill_viridis_d(guide = F)
# 
# output$inputs$factorBeta$eCont

####### equilibrium abundances #########""

# Equilibrium abundances can be evaluated here:
# wstar <- .wrapperEquilAbund(outputSTOC, covars = 'temp',
#                             nsim = 100, ngrid = 10, BYFACTOR = T, 
#                             verbose = T)
# 
# save(wstar, file = "wstarSTOC_temp.Rdata")
# # load("wstarSTOC.Rdata")
# 
# mean <- as.tibble(wstar$ccMu)
# mean$temp <- wstar$x[, "temp"]
# sd   <- as.tibble(wstar$ccSd)
# sd$temp <- wstar$x[, "temp"]
# 
# 
# test <- pivot_longer(mean, cols = -temp, values_to = "eq_abundance")
# test2 <- pivot_longer(sd, cols = -temp , values_to = "eq_abundanceSD")
# test$eq_abundanceSD <- test2$eq_abundanceSD
# 
# ggplot(test, aes(y = eq_abundance, x = temp, ymin = eq_abundance - eq_abundanceSD,
#                  ymax = eq_abundance + eq_abundanceSD, col = name))+
#   geom_pointrange()+
#   geom_hline(yintercept = 0, linetype = 3)+
#   geom_line()+
#   scale_color_viridis_d(guide=F)+
#   facet_wrap(~name, scales = "free_y")




