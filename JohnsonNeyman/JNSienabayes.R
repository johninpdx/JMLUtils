

JNSienaBayes <- function(sbo, # sienaBayes output
                         theta1, #number of the first parameter involved in the 
                         # interaction - check the model results
                         theta2, # number of the 2nd parameter  
                         thetaInt, # number of the interaction
                         theta1vals, # range of the statistics for the 1st parameter
                         theta2vals,  # range of the statistics for the 2nd parameter 
                         burnIn = NULL, # how many iterations should be cut as burn in, NULL = nwarm
                         thin = 1, #should iterations be thinned
                         hyperOnly = TRUE) { # if any of the parameters is random should only the hyper parameter mu be used (otherwise plots and tables for all groups included!)
  eff <- sbo$effects
  deps <- unique(eff$name)
  
  if (is.null(burnIn)) {
    burnIn <- sbo$nwarm
  }
  
  first_rates <- c()
  for (i in 1:length(deps)) {
    first_rates <- c(first_rates,
                     which(eff$name == deps[i] & 
                             eff$functionName == 'Amount of network change in period 1'))
  }
  
  rates <- sbo$basicRate
  rates[first_rates] <- FALSE
  
  eff <- eff[!rates,]
  
  
  theta1n <- eff$effectName[theta1]
  theta2n <- eff$effectName[theta2]
  theta1n <- gsub('\\^','',theta1n)
  theta1n <- gsub('\\/','o',theta1n)
  theta1n <- gsub(':','',theta1n)
  theta2n <- gsub('\\^','',theta2n)
  theta2n <- gsub(':','',theta2n)
  theta2n <- gsub('\\/','o',theta2n)
  
  jnb_support <- function(theta,
                          group,
                          sbo = sbo,
                          theta1 = theta1, 
                          theta2 = theta2, 
                          theta1n = theta1n, 
                          theta2n = theta2n, 
                          thetaInt = thetaInt, 
                          theta1vals = theta1vals, 
                          theta2vals = theta2vals) { 
    
    t1d <- data.frame(theta           = rep(theta1n,length(theta2vals)),
                      moderator       = rep(theta2n,length(theta2vals)),
                      mod_values      = round(theta2vals,4),
                      thetaPostMean   = NA,
                      thetaPostSD     = NA,
                      thetaPostBayesP = NA,
                      thetaPost2.5    = NA,
                      thetaPost97.5   = NA,
                      group = group)
    
    t1plotData <- matrix(NA,0,2)
    
    for (i in 1:length(theta2vals)) {
      theta1x <- theta[,1] + theta[,3] * theta2vals[i]
      t1d$thetaPostMean[i]   <- mean(theta1x)
      t1d$thetaPostSD[i]     <- sd(theta1x)
      t1d$thetaPostBayesP[i] <- sum(theta1x > 0) / length(theta1x)
      t1d$thetaPost2.5[i]    <- quantile(theta1x,0.025)
      t1d$thetaPost97.5[i]   <- quantile(theta1x,0.975)
      
      t1plotData <- rbind(t1plotData, cbind(round(theta2vals[i],3),theta1x))
    }
    t1plotData <- as.data.frame(t1plotData)
    t1plotData <- cbind(t1plotData, group)
    names(t1plotData) <- c('modValue','parameter', 'group')
    t1plotData[,1] <- as.factor(t1plotData[,1])
    
    t2d <- data.frame(theta           = rep(theta2n,length(theta1vals)),
                      moderator       = rep(theta1n,length(theta1vals)),
                      mod_values      = round(theta1vals,4),
                      thetaPostMean   = NA,
                      thetaPostSD     = NA,
                      thetaPostBayesP = NA,
                      thetaPost2.5    = NA,
                      thetaPost97.5   = NA,
                      group = group)
    
    
    t2plotData <- matrix(NA,0,2)
    
    for (i in 1:length(theta1vals)) {
      theta2x <- theta[,2] + theta[,3] * theta1vals[i]
      t2d$thetaPostMean[i]   <- mean(theta2x)
      t2d$thetaPostSD[i]     <- sd(theta2x)
      t2d$thetaPostBayesP[i] <- sum(theta2x > 0) / length(theta2x)
      t2d$thetaPost2.5[i]    <- quantile(theta2x,0.025)
      t2d$thetaPost97.5[i]   <- quantile(theta2x,0.975)
      
      t2plotData <- rbind(t2plotData, cbind(theta1vals[i],theta2x))
    }
    
    t2plotData <- as.data.frame(t2plotData)
    t2plotData <- cbind(t2plotData, group)
    names(t2plotData) <- c('modValue','parameter','group')
    t2plotData[,1] <- as.factor(t2plotData[,1])
    
    
    library(ggplot2)
    
    
    g1 <- ggplot(t1plotData, aes(x = parameter)) +
      geom_vline(xintercept = 0) +
      geom_density(alpha = 0.1, aes(fill = modValue, color = modValue)) +
      theme_bw()
    
    if (nchar(theta2n) < 26) {
      g1 <- g1 + 
        labs(title = paste0('Posterior density for effect of ',
                            theta1n,
                            '\n moderated by ',
                            theta2n,
                            ' (',
                            group,
                            ')'),
             x = theta1n,
             y = 'Posterior Density',
             color = theta2n,
             fill = theta2n)
    } else {
      g1 <- g1 + 
        labs(title = paste0('Posterior density for effect of ',
                            theta1n,
                            '\n moderated by ',
                            theta2n,
                            ' (',
                            group,
                            ')'),
             x = theta1n,
             y = 'Posterior Density',
             color = 'Moderator',
             fill = 'Moderator')
    }
    
    
    g2 <- ggplot(t2plotData, aes(x = parameter)) +
      geom_vline(xintercept = 0) +
      geom_density(alpha = 0.1, aes(fill = modValue, color = modValue))  +
      theme_bw()
    
    if (nchar(theta1n) < 26) {
      g2 <- g2 + 
        labs(title = paste0('Posterior density for effect of ',
                            theta2n,
                            '\n moderated by ',
                            theta1n,
                            ' (',
                            group,
                            ')'),
             x = theta2n,
             y = 'Posterior Density',
             color = theta1n,
             fill = theta1n)
    } else {
      g2 <- g2 + 
        labs(title = paste0('Posterior density for effect of ',
                            theta2n,
                            '\n moderated by ',
                            theta1n,
                            ' (',
                            group,
                            ')'),
             x = theta2n,
             y = 'Posterior Density',
             color = 'Moderator',
             fill = 'Moderator') 
    }
    
    if (grepl('group',group)) {
      g1n <- paste0('JNplots per group/Posterior density for effect of ',
                    theta2n,
                    ' moderated by ',
                    theta1n,
                    ' (',
                    group,
                    ')',
                    '.png')
      
      g2n <- paste0('JNplots per group/Posterior density for effect of ',
                    theta1n,
                    ' moderated by ',
                    theta2n,
                    ' (',
                    group,
                    ')',
                    '.png')
    } else {
      g1n <- paste0('Posterior density for effect of ',
                    theta2n,
                    ' moderated by ',
                    theta1n,
                    ' (',
                    group,
                    ')',
                    '.png')
      
      g2n <- paste0('Posterior density for effect of ',
                    theta1n,
                    ' moderated by ',
                    theta2n,
                    ' (',
                    group,
                    ')',
                    '.png')
      
      plot(g1)
      plot(g2)
    }
    
    
    
    ggsave(g1n, plot = g1, dpi = 600)
    ggsave(g2n, plot = g2, dpi = 600)
    
    returnList <- list(t1d,t2d,g1,g2)
    names(returnList) <- c(paste(theta1n, '_table'),
                           paste(theta2n, '_table'),
                           paste(theta1n, '_ggplot'),
                           paste(theta2n, '_ggplot'))
    return(returnList)
  }
  
  
  if (!any(eff$randomEffects[c(theta1, theta2, thetaInt)])) {
    theta <- sbo$ThinParameters[seq(burnIn + 1,
                                      nrow(sbo$ThinParameters),
                                      thin),1,c(theta1, theta2, thetaInt)]
    returnList <- jnb_support(theta = theta,
                              group = 'Eta',
                              sbo = sbo,
                              theta1 = theta1, 
                              theta2 = theta2, 
                              theta1n = theta1n, 
                              theta2n = theta2n, 
                              thetaInt = thetaInt, 
                              theta1vals = theta1vals, 
                              theta2vals = theta2vals)
    } else {
      rownames(eff) <- 1:nrow(eff)
      eff_ran <- eff[eff$randomEffects,]
      
      if (eff$randomEffects[theta1]) {
        theta1mu <- sbo$ThinPosteriorMu[seq(burnIn + 1,
                                            nrow(sbo$ThinPosteriorMu),
                                            thin),
                                        which(as.numeric(
                                          rownames(eff_ran)) == theta1)]
      } else {
        theta1mu <- sbo$ThinParameters[seq(burnIn + 1,
                                           nrow(sbo$ThinParameters),
                                           thin),1,theta1]
      }
      
      if (eff$randomEffects[theta2]) {
        theta2mu <- sbo$ThinPosteriorMu[seq(burnIn + 1,
                                            nrow(sbo$ThinPosteriorMu),
                                            thin),
                                        which(as.numeric(
                                          rownames(eff_ran)) == theta2)]
      } else {
        theta2mu <- sbo$ThinParameters[seq(burnIn + 1,
                                           nrow(sbo$ThinParameters),
                                           thin),1,theta2]
      }
      
      if (eff$randomEffects[theta1]) {
        thetaIntMu <- sbo$ThinPosteriorMu[seq(burnIn + 1,
                                              nrow(sbo$ThinPosteriorMu),
                                              thin),
                                          which(as.numeric(
                                            rownames(eff_ran)) == thetaInt)]
      } else {
        thetaIntMu <- sbo$ThinParameters[seq(burnIn + 1,
                                             nrow(sbo$ThinParameters),
                                             thin),1,thetaInt]
      }
      theta <- cbind(theta1mu,
                     theta2mu,
                     thetaIntMu)
      returnList <- jnb_support(theta = theta,
                                group = 'Mu',
                                sbo = sbo,
                                theta1 = theta1, 
                                theta2 = theta2, 
                                theta1n = theta1n, 
                                theta2n = theta2n, 
                                thetaInt = thetaInt, 
                                theta1vals = theta1vals, 
                                theta2vals = theta2vals)
      
      if (!hyperOnly) {
        randomResults <- vector(mode = 'list', length = sbo$nGroup)
        names(randomResults) <- paste0('group',1:sbo$nGroup)
        for (i in 1:sbo$nGroup) {
          theta <- sbo$ThinParameters[seq(burnIn + 1,
                                          nrow(sbo$ThinParameters),
                                          thin),
                                      i,
                                      c(theta1, theta2, thetaInt)]
          randomResults[[i]] <- jnb_support(theta = theta,
                                            group = paste0('group',i),
                                            sbo = sbo,
                                            theta1 = theta1, 
                                            theta2 = theta2, 
                                            theta1n = theta1n, 
                                            theta2n = theta2n, 
                                            thetaInt = thetaInt, 
                                            theta1vals = theta1vals, 
                                            theta2vals = theta2vals)
        }
        returnList <- list(returnList,randomResults)
        names(returnList) <- c('Mu','random_groups_effects')
      } 
    }
  
  return(returnList)
}
