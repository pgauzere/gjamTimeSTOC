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
formulaB <- as.formula(~ temp + precip)

# PRIORS
priorB <- list(lo = list(temp =-0.05),
               hi = list(temp =0.05))

#------------------------------------------------------------------------------
# Rho Priors: Density-Dependent Environment-Species Effects ( R * v * v' )
#------------------------------------------------------------------------------
# Rho parameter estimates the amount of population growth that is affected by 
# each covariate.

formulaR <- as.formula(~ temp + precip + cover.Agricultural + 
                         cover.Artificial + cover.Forest + cover.Open)
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
                   betaPrior = priorB, rhoPrior = priorR, alphaSign = alphaSign)
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





