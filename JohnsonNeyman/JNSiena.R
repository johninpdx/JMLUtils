
JNSiena <- function(siena07out, # siena07 output
                    theta1, #number of the first parameter involved in the
                    # interaction - check the model results
                    theta2, # number of the 2nd parameter
                    thetaInt, # number of the interaction
                    theta1vals, # range of the statistics corresponding the 1st parameter
                    theta2vals,  # range for the statistics corresponding the 2nd parameter
                    alpha = 0.05) { # significance range,
                        #all p-values are automatically bonferroni holmes adjusted!!!
  if (class(siena07out) != 'sienaFit') {
    stop('sienaout needs to be a sienaFit object created by siena07().')
  }

  if (theta1 > length(siena07out$effects$effectName) ||
      theta2 > length(siena07out$effects$effectName)) {
    cat('theta1 and theta2 need to be the number of the effect in the sienaFit')
    cat('object. The provided numbers exceed the existing parameters.')
    stop()
  }
  #Get the effectName strings for the effects with numbers theta1 & theta2
  # (e.g. "RF ego", "RF ego x RF alter", etc.).
  # 'effects' is the effects table subset of model-included rows
  theta1n <- siena07out$effects$effectName[theta1]
  theta2n <- siena07out$effects$effectName[theta2]

  #Vector of g1+g3 multiplied by each bespoke moderator (x2) value.
  theta1s <- siena07out$theta[theta1] + siena07out$theta[thetaInt] * theta2vals
  #For each bespoke value of x2 in 'theta2vals' (the moderator vals),
  # calculate the SE for the parameter g1 (the main predictor).
  # See eq.(13) in Bauer&Curran(2005). It's the SE of the 'simple slope' w1 of
  # g1+g3*x2.
  seT1 <- sqrt(siena07out$covtheta[theta1,theta1] +
                 theta2vals * 2 * siena07out$covtheta[thetaInt,theta1] +
                 theta2vals ^ 2 * siena07out$covtheta[thetaInt,thetaInt])

  #Calculate a t-ratio for g1 at each bespoke value of x2.
  z1 <- theta1s/seT1

  #For each element of theta2vals, calculate the lesser of
  # the upper or lower end of the N distribution.
  # NOTICE that this is basically the pick-a-point method for
  # establishing conditional significance of theta1 depending
  # on the value of theta2 (or vice versa). But by selecting
  # different sets of values of theta1vals and theta2vals,
  # you can infer the critical value (s) of a moderator, for
  # which the relationship between the primary predictor becomes
  # a significant predictor (e.g. of tie formation)...(and of course
  # you can designate either predictor as primary or moderator).
  # Then you can
  p1 <- c()
  for (i in 1:length(theta2vals)) {
    #Trick to select the correct double-sided critical region
    p1[i] <-  2 * pmin(pnorm(z1[[i]]), (1 - pnorm(z1[[i]])))
  }

  p1O <- p1[order(p1)]
  #This is the Bonferroni-Holm adjustment:
  bhT1 <- alpha * c(1:length(theta2vals))/length(theta2vals)
  sig1 <- p1O < bhT1 #Are the t's significant at the BH-adjusted levels?
  sig11 <- vector(length = length(theta2vals))
  for (i in 1:length(theta2vals)) {
    sig11[order(p1)[i]] <- sig1[i]
  }
#The dataframe of x1 as primary and x2 as moderator
  t1d <- data.frame(theta      = rep(theta1n,length(theta2vals)),
                    moderator  = rep(theta2n,length(theta2vals)),
                    mod_values = round(theta2vals,4),
                    thetavals  = round(theta1s,4),
                    thetase    = round(seT1,4),
                    thetap     = round(p1,3),
                    significance_adjusted = sig11)

#-----------------------------------------------------------------
#Same calc as above, except that now x2 is the primary predictor,
# and x1 is the moderator
#-----------------------------------------------------------------
  theta2s <- siena07out$theta[theta2] + siena07out$theta[thetaInt] * theta1vals
  seT2 <- sqrt(siena07out$covtheta[theta2,theta2] +
                 theta1vals * 2 * siena07out$covtheta[thetaInt,theta2] +
                 theta1vals ^ 2 * siena07out$covtheta[thetaInt,thetaInt])

  z2 <- theta2s/seT2
  p2 <- c()
  for (i in 1:length(theta1vals)) {
    p2[i] <-  2 * pmin(pnorm(z2[[i]]), (1 - pnorm(z2[[i]])))
  }

  p2O <- p2[order(p2)]
  bhT2 <- alpha * c(1:length(theta1vals))/length(theta1vals)
  sig2 <- p2O < bhT2
  sig22 <- vector(length = length(theta1vals))
  for (i in 1:length(theta1vals)) {
    sig22[order(p2)[i]] <- sig2[i]
  }


  t2d <- data.frame(theta      = rep(theta2n,length(theta1vals)),
                    moderator  = rep(theta1n,length(theta1vals)),
                    mod_values = round(theta1vals,4),
                    theta_vals = round(theta2s,4),
                    theta_se   = round(seT2,4),
                    theta_p    = round(p2,3),
                    significance_adjusted = sig22)

  return(list(t1d,t2d))
}
