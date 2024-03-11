# sensitivity_examples.R
# Examples of sensitivity analysis using an emulator
# Doug McNeall

# Ref article:
# https://dougmcneall.com/2017/02/17/sensitivity-analysis-with-r/

# -----------------------------------------------------------
# 0. load data and packages
# -----------------------------------------------------------

library(DiceKriging) # emulator package

# Load the forest fraction data from the climate model FAMOUS
giturl = 'https://github.com/dougmcneall/famous-git/raw/master/famous_forest_fraction.RData'
load(url(giturl))

# input design matrix X
X = full_frac[, 2:8]

# -----------------------------------------------------------
# 1. Marginal plots
# -----------------------------------------------------------

# You can get a good idea of the influence of parameters by 
# plotting the output against each parameter (margin) in turn.
# Here is the Amazon as an example.

pdf(file = 'margins_amazon.pdf', width = 8, height = 6)
par(mfrow = c(2,4), las = 1, mar = c(5,3,2,1), oma = c(0.1,3,0.1,0.1) )

for(i in 1:ncol(X)){
  plot(X[, i], full_frac$AMAZ_MOD_FRAC, ylim = c(0,1),
       xlab = colnames(X)[i],
       bty = 'n')
}
mtext('Amazon forest fraction', side = 2, line = 1, col = 'black', outer = TRUE, las = 0)
dev.off()


# -----------------------------------------------------------
# 2. One-at-a-time SA with an emulator
# -----------------------------------------------------------
# Using the emulator allows you to approximate what would happen if you
# only adjusted one parameter at a time, holding all the others constant
# (often at their mean, or 'best' values).

# First, build an emulator using DiceKriging
# The ~. means we use a linear model as a prior, with the
# Gaussian process modelling the deviations.
fit_amazon = km(~., design = X, response = full_frac$AMAZ_MOD_FRAC)

# We making a function that samples from the input space across
# a single parameter at a time, and then predict the outcome.

oaat.design = function(design, n, med=TRUE, hold=NULL){
  # function for creating one-at-a-time design matrix
  # INPUTS:
  # design .... original design (e.g. a latin hypercube or output from expand.grid)
  # n ......... number of design points in each dimension 
  # med ....... Use median rather than mean?
  # hold ...... Use supplied value to hold the non-changing points
  #  
  # OUTPUTS:
  # ........... (n x nd) rows, nd columns design matrix, sweeping through parameter space
  
  oamat = NULL
  nd = ncol(design)
  
  if(med){
    meandes = apply(design, 2, median)	
  }
  else{
    meandes = apply(design, 2, mean)
  }
  
  if(is.null(hold) == FALSE){
    meandes = hold
  }
  
  mindes = apply(design, 2, min)
  maxdes = apply(design, 2, max)
  
  for (j in 1:nd){
    # base matrix of 'best' values to put the sweep values into
    basemat = matrix(meandes, nrow=n, ncol=nd , byrow=TRUE)
    vec = seq(from=mindes[j], to=maxdes[j], length.out=n)
    basemat[ ,j] = vec
    oamat = rbind(oamat, basemat)
  }
  oamat
}


n = 21 # number of points in to vary
X_oaat = oaat.design(design=X, n = n, hold = X.standard)
colnames(X_oaat) = colnames(X)

pred_amazon_oaat = predict(fit_amazon, newdata = X_oaat, type = 'UK')

# Now we can plot the output in a similar way to the original parameters.
# Remember, in each plot, we just want the block where the parameter in
# question is varied.

pdf(file = 'oat_amazon.pdf', width = 8, height = 6)
par(mfrow = c(2,4), las = 1, mar = c(5,3,2,1), oma = c(0.1,3,0.1,0.1))

for(i in 1:ncol(X_oaat)){
  # just the points where the parameter varies
  ix <- seq(from = ((i*n) - (n-1)), to =  (i*n), by = 1)
  plot(X_oaat[ix, i], pred_amazon_oaat$mean[ix], ylim = c(0, 1), bty = 'n',
       ylab = '', type = 'l', lwd = 2,
       xlab = colnames(X)[i]
  )
  lines(X_oaat[ix, i], pred_amazon_oaat$upper95[ix], lty = 'dashed')
  lines(X_oaat[ix, i], pred_amazon_oaat$lower95[ix], lty = 'dashed')
}
legend('topright', lty = c('solid', 'dashed'), lwd = c(2,1), legend = c('mean','95% CI'))
mtext('Amazon forest fraction', side = 2, line = 1, col = 'black', outer = TRUE, las = 0)
dev.off()


# How does this compare with the sensitivity of one of the other outputs?
# have a look at the South East Asian forest fraction
# Plot both on the same axes, and use transparent solid polygons to 
# represent uncertainty

fit_congo = km(~., design = X, response = full_frac$CONGO_MOD_FRAC)
pred_congo_oaat = predict(fit_congo, newdata = X_oaat, type = 'UK')

pdf(file = 'oat_compare.pdf', width = 8, height = 6)
par(mfrow = c(2,4), las = 1, mar = c(5,3,2,1), oma = c(0.1,3,0.1,0.1) )

for(i in 1:ncol(X_oaat)){
  # just the points where the parameter varies
  ix <- seq(from = ((i*n) - (n-1)), to =  (i*n), by = 1)
  
  plot(X_oaat[ix, i], pred_amazon_oaat$mean[ix], ylim = c(0, 1), bty = 'n',
       ylab = '', type = 'l', lwd = 2,
       xlab = colnames(X)[i])
  
  col.transp = adjustcolor('black', alpha = 0.4)
  polygon(x = c(X_oaat[ix, i], rev(X_oaat[ix, i])),
          y =c(pred_amazon_oaat$lower95[ix], rev(pred_amazon_oaat$upper95[ix])),
          col = col.transp, border = col.transp)
  
  col.transp = adjustcolor('darkgreen', alpha = 0.4)
  points(X_oaat[ix, i], pred_congo_oaat$mean[ix], ylim = c(0, 1), bty = 'n',
         ylab = '', type = 'l', lwd = 2,
         xlab = colnames(X)[i],
         col = 'darkgreen')
  
  polygon(x = c(X_oaat[ix, i], rev(X_oaat[ix, i])),
          y =c(pred_congo_oaat$lower95[ix], rev(pred_congo_oaat$upper95[ix])),
          col = col.transp, border = col.transp)
}
legend('top', legend = c('Amazon', 'Congo'), col = c('black','darkgreen'),
       lty = 'solid', lwd = 1, pch = NA, bty = 'n',
       text.col = 'black', 
       fill = adjustcolor(c('black', 'darkgreen'), alpha = 0.4), border = NA, cex = 1.2)
mtext('Forest fraction', side = 2, line = 1, col = 'black', outer = TRUE, las = 0)
dev.off()
