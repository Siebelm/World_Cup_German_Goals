# This is the Solution file for the Final Project of course:
# Bayesian Methods for Data Science (DATS 6450 - 11, Summer 2018)
# Data Science @ George Washington University
# Author: Michael Siebel

source("DBDA2E-utilities.R")


#===============================================================================
### Create Charts ###
#German Team Rank Chart
chart1 = function( rank , comparison1 , comparison2 ) {
  library(plyr)
  rank = ddply(rank, .(country_full, rank_date), summarize,  rank=mean(rank))
  rank[rank$country_full=="Germany",]
  library(ggplot2)
  chart = ggplot(data=rank[(rank$country_full=="Germany" | rank$country_full==comparison1 | rank$country_full==comparison2),], 
                  aes(x=rank_date, y=rank, group=country_full, color=country_full)) + 
    ylab("Rank") + scale_x_continuous("Year", breaks=seq(1993,2018,5)) +
    geom_line() + theme(legend.position="bottom") 
  chart = chart + geom_line(data=rank[(rank$country_full=="Germany"),], size=1.5) 
  chart = chart + scale_colour_discrete(name  ="Country:",
                                          breaks=c("Germany", comparison1, comparison2),
                                          labels=c("Germany", comparison1, comparison2))
  return( chart )
}

#Goal Differential Chart
chart2 = function( datFrm , goal_diff , friendly ) {
  chart = ggplot(datFrm, aes(goal_diff, fill = friendly)) +
    geom_histogram(binwidth=.75, position="dodge") + 
    ylab("Count") + xlab("Goal Differential") + theme(legend.position="bottom") 
  return( chart )
}

#===============================================================================
#### Create prior distributions ###
# Define Prior based on average goals scored per match by Germany since 1950
prior = function( datFrm ) {
  library(plyr)
  prior_goals = datFrm
  prior_goals = ddply(prior_goals, .(opponent), summarize,  goals_mean=mean(goals), goals_std=sd(goals))
  datFrm = (merge(datFrm, prior_goals, by.x = "opponent", by.y = "opponent", all.x = TRUE, no.dups = FALSE))
  
  # Assume normal distribution in 14 cases of missing
  datFrm$goals_std = ifelse(is.na(datFrm$goals_std) | datFrm$goals_std==0,1,datFrm$goals_std) 
  
  return( datFrm )
}

#===============================================================================

genMCMC = function( datFrm , goals_mean , goals_std , yName="y" , 
                    x1Name="x1" , x2Name="x2" , x3Name="x3" ,
                    numSavedSteps=15000 , thinSteps=1 , saveName=NULL ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault ) { 
  
  #------------------------------------------------------------------------------
  # THE DATA.
  # Convert data file columns to generic x,y variable names for model:
  y = as.numeric(datFrm[,yName])
  
  x1 = as.numeric(as.factor(datFrm[,x1Name]))
  x1levels = levels(as.factor(datFrm[,x1Name]))
  x2 = as.numeric(as.factor(datFrm[,x2Name]))
  x2levels = levels(as.factor(datFrm[,x2Name]))
  x3 = as.numeric(as.factor(datFrm[,x3Name]))
  x3levels = levels(as.factor(datFrm[,x3Name]))
  Nx1Lvl = length(unique(x1))
  Nx2Lvl = length(unique(x2))
  Nx3Lvl = length(unique(x3))
  Ncell = length(y) 
  
  # For prior distributions:
  yMean = median(goals_mean) 
  ySD = median(goals_std)
  agammaShRa = unlist( gammaShRaFromModeSD( mode=yMean , sd=2*ySD ) )
  showSDprior = TRUE
  if ( showSDprior ) {
    show( agammaShRa )
    openGraph(height=5,width=7)
    xv = seq(0,ySD/2+2*2*ySD,length=501)
    plot( xv , dgamma( xv , shape=agammaShRa[1] , rate=agammaShRa[2] ) ,
          xlab="SD" , ylab="p(SD)" , main="Prior on SD parameters" , type="l" ,
          col="skyblue" , lwd=3 )
  }
  
  # Specify the data in a list for sending to JAGS:
  dataList = list(
    y = y ,
    x1 = x1 ,
    x2 = x2 ,
    x3 = x3 ,
    Ncell = Ncell ,
    Nx1Lvl = Nx1Lvl ,
    Nx2Lvl = Nx2Lvl ,
    Nx3Lvl = Nx3Lvl ,
    ySum = sum(y) ,
    yMean = yMean ,
    ySD = ySD ,
    agammaShRa = agammaShRa 
  )

  #------------------------------------------------------------------------------
  # THE MODEL.
  #------------------------------------------------------------------------------
  modelstring = " # open quote for modelstring
  model {
    for ( i in 1:Ncell ) {
      y[i] ~ dpois( lambda[i] )
      lambda[i] <- exp( a0 + a1[x1[i]] + a2[x2[i]] + a3[x3[i]] )
    }
    
    a0 ~ dnorm( yMean , 1/(ySD*2)^2 )

    for ( j1 in 1:Nx1Lvl ) { 
      a1[j1] ~ dnorm( 0.0 , 1/a1SD^2 ) 
    }
    a1SD ~ dgamma(agammaShRa[1],agammaShRa[2]) 
    
    for ( j2 in 1:Nx2Lvl ) { 
      a2[j2] ~ dnorm( 0.0 , 1/a2SD^2 ) 
    }
    a2SD ~ dgamma(agammaShRa[1],agammaShRa[2])
    
    for ( j3 in 1:Nx3Lvl ) { 
      a3[j3] ~ dnorm( 0.0 , 1/a3SD^2 ) 
    }
    a3SD ~ dgamma(agammaShRa[1],agammaShRa[2])
    
    # Convert a0,a1[],a2[],a3[,] to sum-to-zero b0,b1[],b2[],b3[,] :
    for ( j1 in 1:Nx1Lvl ) { 
      for ( j2 in 1:Nx2Lvl ) {
        for ( j3 in 1:Nx3Lvl ) {
          m[j1,j2,j3] <- a0 + a1[j1] + a2[j2] + a3[j3] # cell means
        }
      } 
    }

    b0 <- mean( m[1:Nx1Lvl,1:Nx2Lvl,1:Nx3Lvl] )
    
    for ( j1 in 1:Nx1Lvl ) { 
      b1[j1] <- mean( m[j1,1:Nx2Lvl,1:Nx3Lvl] ) - b0 
    }
    
    for ( j2 in 1:Nx2Lvl ) { 
      b2[j2] <- mean( m[1:Nx1Lvl,j2,1:Nx3Lvl] ) - b0 
    }
    
    for ( j3 in 1:Nx3Lvl ) {
      b3[j3] <- mean( m[1:Nx1Lvl,1:Nx2Lvl,j3] ) - b0  
    }  
    
    # Compute predicted probabilities:
    for ( j1 in 1:Nx1Lvl ) { 
      for ( j2 in 1:Nx2Lvl ) {
        for ( j3 in 1:Nx3Lvl ) {
          expm[j1,j2,j3] <- exp(m[j1,j2,j3])
          ppx1x2x3p[j1,j2,j3] <- expm[j1,j2,j3]
        }
      } 
    }
    
    for ( j1 in 1:Nx1Lvl ) { 
      ppx1p[j1] <- sum(ppx1x2x3p[j1,1:Nx2Lvl,1:Nx3Lvl]) 
    }
    
    for ( j2 in 1:Nx2Lvl ) { 
      ppx2p[j2] <- sum(ppx1x2x3p[1:Nx1Lvl,j2,1:Nx3Lvl]) 
    }
    
    for ( j3 in 1:Nx3Lvl ) { 
      ppx3p[j3] <- sum(ppx1x2x3p[1:Nx1Lvl,1:Nx2Lvl,j3]) 
    }
  }
  " # close quote for modelstring
  writeLines(modelstring,con="model.txt")
  
  #------------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Let JAGS it automatically...
  #------------------------------------------------------------------------------
  # RUN THE CHAINS
  require(rjags)
  require(runjags)
  parameters = c( "ppx1x2x3p" , "ppx1p" , "ppx2p" , "ppx3p" , 
                  "b0" ,  "b1" ,  "b2" ,  "b3" , "m" ,  
                  "a1SD" , "a2SD" , "a3SD" )
  ## Using runjags:
  adaptSteps = 1000 
  burnInSteps = 2000 
  runJagsOut <- run.jags( method=runjagsMethod ,
                          model="model.txt" , 
                          monitor=parameters , 
                          data=dataList ,  
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps , 
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  codaSamples = as.mcmc.list( runJagsOut )
  
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
  
}

#===============================================================================
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
smryMCMC = function(  codaSamples , 
                      datFrm=NULL , x1Name=NULL , x2Name=NULL , x3Name=NULL ,
                      saveName=NULL ) {
  
  # All single parameters:
  parameterNames = varnames(codaSamples) 
  x1levels = levels(as.factor(datFrm[,x1Name]))
  x2levels = levels(as.factor(datFrm[,x2Name]))
  x3levels = levels(as.factor(datFrm[,x3Name]))
  
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  for ( parName in parameterNames ) {
    summaryInfo = rbind( summaryInfo , summarizePost( mcmcMat[,parName] ) )
    thisRowName = parName
    # For row name, extract numeric digits from parameter name. E.g., if
    # parameter name is "b1[11,86,2]" then pull out b1, 11, 86, and 2:
    strparts = unlist( strsplit( parName , "\\[|,|\\]"  ) )
    # if there are only the param name and a single index:
    if ( length(strparts)==2 ) { 
      # if param name refers to factor 1:
      if ( substr(strparts[1],nchar(strparts[1]),nchar(strparts[1]))=="1" ) { 
        thisRowName = paste( thisRowName , x1levels[as.numeric(strparts[2])] )
      }
      # if param name refers to factor 2:
      if ( substr(strparts[1],nchar(strparts[1]),nchar(strparts[1]))=="2" ) { 
        thisRowName = paste( thisRowName , x2levels[as.numeric(strparts[2])] )
      }
      # if param name refers to factor 3:
      if ( substr(strparts[1],nchar(strparts[1]),nchar(strparts[1]))=="3" ) { 
        thisRowName = paste( thisRowName , x3levels[as.numeric(strparts[2])] )
      }
    }
    # if there are the param name and two indices:
    if ( length(strparts)==3 ) { 
      thisRowName = paste( thisRowName , x1levels[as.numeric(strparts[2])], 
                           x2levels[as.numeric(strparts[3])] )
    }
    rownames(summaryInfo)[NROW(summaryInfo)] = thisRowName
  }

  # Save results:
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  return( summaryInfo )
}


#===============================================================================
# Display posterior information:

plotMCMC = function( codaSamples , x1levs , x2levs , x3levs ,
                     datFrm , yName="y" , x1Name="x1" , x2Name="x2" , x3Name="x3" ,
                     saveName=NULL , saveType="jpg" ) {
  
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  y = datFrm[,yName]
  x1 = as.numeric(as.factor(datFrm[,x1Name]))
  x1levels = levels(as.factor(x1levs)) 
  Nx1levels = length(x1levels)
  x2 = as.numeric(as.factor(datFrm[,x2Name]))
  x2levels = levels(as.factor(x2levs))
  Nx2levels = length(x2levels)
  x3 = as.numeric(as.factor(datFrm[,x3Name]))
  x3levels = levels(as.factor(x3levs)) 
  Nx3levels = length(x3levels)
  
  # Display data with posterior predictive distributions
  openGraph(width=2.25*Nx2levels,height=1.5*Nx1levels)
  par( mar=c(3.5,2.5,3.0,1.5) , mgp=c(2,0.7,0) )
  layout(matrix(1:(Nx1levels*Nx2levels),nrow=Nx1levels,byrow=TRUE))
  xLim = range(mcmcMat[,grep("^ppx1x2p",colnames(mcmcMat))])
  for ( x1Idx in 1:Nx1levels ) {
    for ( x2Idx in 1:Nx2levels ) {
      for ( x3Idx in 1:Nx3levels ) {
        paramCol = paste0("ppx1x2x3p[",x1Idx,",",x2Idx,",",x3Idx,"]")
        plotPost( mcmcMat[,paramCol] , xlab="Predicted Count" , cex.lab=1.0 ,
                  main=paste0( x1Name,":",x1levels[x1Idx] ,"  ",
                               x2Name,":",x2levels[x2Idx] ,"\n",
                               x2Name,"False" ) , cenTend="mode" ,
                  border="skyblue" , HDItextPlace=0.9 )
      }
    }
  }
  if ( !is.null(saveName) ) {
    saveGraph( file=paste0(saveName,"PostPred"), type=saveType)
  }
}
#------------------------------------------------------------------------------- 

frequentist_mex = function( win_ratio ) {
  ###Model###
  summary(model_goal <- glm(goals ~ win_mean +
                            friendly + opponent, family="poisson", data=df))
  
  # Mexico #
  assumptions_mex_goals <- data.frame(win_mean = win_ratio,
                                      opponent = "Mexico",
                                      friendly = FALSE)
  
  goals_mex <- predict(model_goal, assumptions_mex_goals, type="response", se.fit=TRUE)
  return(paste("Germany is expected to score",round(goals_mex$`fit`,2),"goals against Mexico when the win ratio is", win_ratio,", confidence interval:",round(goals_mex$`fit`-goals_mex$se.fit,2),round(goals_mex$`fit`+goals_mex$se.fit,2)))
  
}

frequentist_swe = function( win_ratio ) {
  ###Model###
  summary(model_goal <- glm(goals ~ win_mean +
                              friendly + opponent, family="poisson", data=df))
  # Sweden #
  assumptions_swe_goals <- data.frame(win_mean = win_ratio,
                                      opponent = "Sweden",
                                      friendly = FALSE)
  
  goals_swe <- predict(model_goal, assumptions_swe_goals, type="response", se.fit=TRUE)
  return(paste("Germany is expected to score",round(goals_swe$`fit`,2),"goals against Sweden when the win ratio is", win_ratio,", confidence interval",round(goals_swe$`fit`-goals_swe$se.fit,2),round(goals_swe$`fit`+goals_swe$se.fit,2)))
}
