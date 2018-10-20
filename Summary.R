#===============================================================================


plotMCMC = function( codaSamples , x1levs , x2levs , x3levs ,
                     datFrm , yName="y" , x1Name="x1" , x2Name="x2" , x3Name="x3" ,
                     x1contrasts=NULL , 
                     x2contrasts=NULL , 
                     x3contrasts=NULL ,
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
        cellN = datFrm[ datFrm[,x1Name]==x1levels[x1Idx] 
                        & datFrm[,x2Name]==x2levels[x2Idx] 
                        & datFrm[,x3Name]==x3levels[x3Idx] , yName ]
        totalN = sum(y)
        paramCol = paste0("ppx1x2x3p[",x1Idx,",",x2Idx,",",x3Idx,"]")
        plotPost( mcmcMat[,paramCol] , xlab="Predicted Count" , cex.lab=1.0 ,
                  main=paste0( x1Name,":",x1levels[x1Idx] ,"  ",
                               x2Name,":",x2levels[x2Idx] ) , cenTend="mode" ,
                               border="skyblue" , HDItextPlace=0.9 )
        #points(  ,  0 , pch=17 , col="red" , cex=2 )
      }
    }
  }
  if ( !is.null(saveName) ) {
    saveGraph( file=paste0(saveName,"PostPred"), type=saveType)
  }

}

x1levs = c(0.25, 0.5, 0.75)
x2levs = c(51, 76)
x3levs = c(2)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=df , x1levs , x2levs , x3levs ,
          yName=yName , x1Name=x1Name , x2Name=x2Name , x3Name=x3Name ,
          x1contrasts=x1contrasts , 
          x2contrasts=x2contrasts , 
          x3contrasts=x3contrasts , 
          saveName=fileNameRoot , saveType=graphFileType )