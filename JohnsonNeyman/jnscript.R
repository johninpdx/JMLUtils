# se_b1|x2 = sqrt(var(b_1) + 2 * x_2 * cov(b_1, b_2) + x_2 ^ 2 * var(b_2))

library(RSiena)
library(parallel)
load("Glasgow_data/Glasgow-friendship.RData")
load("Glasgow_data/Glasgow-substances.RData")
load("Glasgow_data/Glasgow-demographic.RData")


friendship.1[friendship.1 == 2] <- 1
friendship.2[friendship.2 == 2] <- 1
friendship.3[friendship.3 == 2] <- 1


friendship <- sienaDependent(array(
  c(friendship.1,friendship.2,friendship.2),
  dim = c(160,160,3)), allowOnly = FALSE)

smoke <- varCovar(tobacco[,1:2],centered = TRUE)
alcohol <- varCovar(alcohol[,1:2],centered = TRUE)
gender <- coCovar(sex.F,centered = TRUE)


netDynamics <- sienaDataCreate(friendship,smoke,alcohol,gender)


first.model <- getEffects(netDynamics)

first.model <- includeEffects(first.model,
                               egoX,altX,simX, interaction1 = 'smoke')
first.model <- includeEffects(first.model,
                              egoX,altX,simX, interaction1 = 'alcohol')
first.model <- includeEffects(first.model,simX, interaction1 = 'gender')

first.model <- includeEffects(first.model, 
                              reciAct, 
                              transTrip, 
                              transRecTrip)


first.model <- includeInteraction(first.model, egoX,recip, 
                                  interaction1 = c('alcohol',''))
first.model <- includeInteraction(first.model, egoX,transTrip, 
                                  interaction1 = c('alcohol',''))


estimation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                           cond = FALSE,
                                           seed = 1402, 
                                           n3 = 5000,
                                           MaxDegree = c(friendship = 6))

first.results <- siena07(estimation.options,
                          data = netDynamics,
                          effects = first.model,
                          batch = FALSE,verbose = FALSE,
                          returnDeps = TRUE,
                          nbrNodes = 10,
                          useCluster = TRUE)
(first.results <- siena07(estimation.options,
                          data = netDynamics,
                          effects = first.model,
                          batch = FALSE,verbose = FALSE,
                          returnDeps = TRUE,
                          nbrNodes = 10,
                          useCluster = TRUE,
                          prevAns = first.results))


clust <- makePSOCKcluster(10) 

gof1.indegrees <- sienaGOF(first.results,IndegreeDistribution,
                            verbose = TRUE,
                           varName = "friendship",
                           cumulative = FALSE,
                           cluster = clust)
plot(gof1.indegrees) 

gof1.outdegrees <- sienaGOF(first.results,OutdegreeDistribution, 
                            verbose = TRUE,
                            varName = "friendship",
                            cumulative = FALSE,
                            cluster = clust)
plot(gof1.outdegrees) 


gof1.triads <- sienaGOF(first.results,TriadCensus,
                         verbose = TRUE,varName = "friendship",
                        cluster = clust)
plot(gof1.triads,center = TRUE,scale = TRUE) 
stopCluster(cl = clust)


trans <- 0:5

thetaTwR1 <- first.results$theta[5] + first.results$theta[6]
seTwR1    <- sqrt(first.results$covtheta[5,5] + 
                    2 * 1 * first.results$covtheta[6,5] + 
                    1 ^ 2 * first.results$covtheta[6,6])
thetaTwR1/seTwR1
pnorm(thetaTwR1/seTwR1, lower.tail = FALSE)

thetaRwT <- first.results$theta[4] + first.results$theta[6] * trans
seRwT    <- sqrt(first.results$covtheta[4,4] + 
                    2 * trans * first.results$covtheta[6,4] + 
                   trans ^ 2 * first.results$covtheta[6,6])
thetaRwT/seRwT

round(pnorm(thetaRwT/seRwT, lower.tail = FALSE),3)



smokeVals <- na.omit(unique(netDynamics$vCovars$alcohol))[order(
                na.omit(unique(netDynamics$vCovars$alcohol)))]

thetaSwT <- first.results$theta[13] + first.results$theta[15] 
seSwT    <- sqrt(first.results$covtheta[13,13] + 
                    2 * 1 * first.results$covtheta[13,15] + 
                    1 ^ 2 * first.results$covtheta[15,15])
thetaSwT/seSwT

pnorm(thetaSwT/seSwT, lower.tail = FALSE)

thetaTwS <- first.results$theta[4] + first.results$theta[15] * smokeVals
seTwS    <- sqrt(first.results$covtheta[4,4] + 
                   2 * smokeVals * first.results$covtheta[4,15] + 
                   smokeVals ^ 2 * first.results$covtheta[15,15])
thetaTwS/seTwS

round(pnorm(thetaTwS/seTwS, lower.tail = FALSE),3)



attr(netDynamics$vCovars$alcohol, "mean")


a <- JNSiena(siena07out = first.results,
            theta1 = 4, 
            theta2 = 5, 
            thetaInt = 6,
            theta1vals = 0:1, 
            theta2vals = 0:6)

JNSiena(siena07out = first.results,
        theta1 = 5, 
        theta2 = 13, 
        thetaInt = 16,
        theta1vals = 0:6, 
        theta2vals = na.omit(unique(netDynamics$vCovars$alcohol))[order(
          na.omit(unique(netDynamics$vCovars$alcohol)))])
