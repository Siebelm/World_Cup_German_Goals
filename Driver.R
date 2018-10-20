# Driver.R
# This is the Solution file for the Final Project of course:
# Bayesian Methods for Data Science (DATS 6450 - 11, Summer 2018)
# Data Science @ George Washington University
# Author: Michael Siebel

setwd("C:\\Users\\Siebelm\\Documents\\3 GWU\\4 DATS 6450\\Project")
source("DBDA2E-utilities.R")


### Datasets Used ####
results = "results.csv"
tournament = "World Cup 2018 Dataset.csv"
fifarank = "fifa_ranking.csv"

# Use data cleaning file before Solution.R to prepare dataset
source("Data_Cleaning.R")

#-----------------------------------------------------------------------------
# Clean Data

# Data for Chart 1
rank_df = suppData( fifarank )

# Main Dataset
df = mainData( tournament, results )

#==============================================================================
source("Solution.R")

#-----------------------------------------------------------------------------
# Exploratory Analysis Graphs

comparison1 = "Brazil"
comparison2 = "England"

# German Team Rank Chart
chart1( rank_df , comparison1 , comparison2 ) 

chart2( df , goal_diff , friendly )

#==============================================================================
#Load The data file 
#.............................................................................
yName="goals" 
x1Name="win_mean" 
x2Name="opponent"  
x3Name="friendly" 
fileNameRoot = "goals"
graphFileType = "pdf"

#-----------------------------------------------------------------------------
# Define Prior
df = prior( df )

#-----------------------------------------------------------------------------
# Run model and save output
mcmcCoda = genMCMC( df , df$goals_mean , df$goals_std , yName , x1Name , x2Name , x3Name ,
                    numSavedSteps=15000 , thinSteps=1 , saveName=NULL ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault )

#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters
parameterNames = varnames(mcmcCoda) 
show( parameterNames ) # show all parameter names, for reference
for ( parName in c("b0","b1[1]","b2[1]","b3[1]","ppx1x2x3p[1,1,1]",
                   "a1SD", "a2SD", "a3SD") ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}

#===============================================================================
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , datFrm=df ,
                        x1Name=x1Name , x2Name=x2Name , x3Name=x3Name ,
                        saveName=fileNameRoot )

# Load predicted probabilities information
x1levs = c(0.25, 0.5, 0.75) # Win ratios
x2levs = c(51, 76) # Teams (51 corresponds to Mexico; 76 corresponds to Sweden)
x3levs = c(2)

# Display posterior information:
plotMCMC( mcmcCoda , datFrm=df , x1levs , x2levs , x3levs ,
          yName=yName , x1Name=x1Name , x2Name=x2Name , x3Name=x3Name ,
          saveName=fileNameRoot , saveType=graphFileType )

#===============================================================================
# Frequentist model

# Set win ratio
win_ratio = c(0.5)

# Goals against Mexico
frequentist_mex( win_ratio )

# Goals against Sweden
frequentist_swe( win_ratio )

#===============================================================================