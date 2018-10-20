# This is the Data_Cleaning file for the Final Project of course:
# Bayesian Methods for Data Science (DATS 6450 - 11, Summer 2018)
# Data Science @ George Washington University
# Author: Michael Siebel

suppData = function( fifarank ) {
  
  ### Load Supplementary Datasets ###
  rank_df = read.csv(fifarank)
  rank_df = subset(rank_df, select=c(rank, country_full, rank_date))
  rank_df$rank_date = as.Date(rank_df$rank_date, tryFormats = "%m/%d/%Y")
  rank_df$rank_date = as.numeric(substr(rank_df$rank_date,1,4))
  
  return( rank_df )
  
}

mainData = function( tournament, results ) {
  
  ### Load Tournament Teams ###
  tnmt = read.csv(tournament)
  # Make vector of 32 World Cup teams
  teams = as.vector(tnmt$Team)
  
  ### Load Main Data ###
  # Load data
  df_total = read.csv(results) 
  # Subset to current World Cup Teams
  df_teams = subset(df_total, home_team %in% teams | away_team %in% teams) 
  df_teams$year = as.numeric(substr(df_teams$date,1,4))
  # Subset data to 1950-2018
  df_teams = df_teams[df_teams$year>=1950,] 
  
  
  ### Clean and Create Variables ###
  df = df_teams
  df$city = NULL
  df$country = NULL
  # Subset dataset to only include Germany matches 
  df = df[df$home_team=="Germany" | df$away_team=="Germany",]
  # Define Goals Scored
  df$goals    = ifelse(df$home_team=="Germany", df$home_score, ifelse(df$away_team=="Germany", df$away_score, NA))
  # Define Goals Conceded
  df$conceded = ifelse(df$home_team=="Germany", df$away_score, ifelse(df$away_team=="Germany", df$home_score, NA))
  # Define Goal Differential
  df$goal_diff = df$goals-df$conceded
  # Define Opponent Team
  df$home_team = as.character(df$home_team)
  df$away_team = as.character(df$away_team)
  df$opponent = ifelse(df$home_team=="Germany", df$away_team, 
                           ifelse(df$away_team=="Germany", df$home_team, NA))
  opponents = as.vector(unique(sort(df$opponent)))
  # Define Home Games 
  df$home = as.logical(df$home_team=="Germany")
  # Define German Wins
  df$win = as.logical(df$goal_diff >0)
  # Define German Ties
  df$tie = as.logical(df$goal_diff==0)
  # Define German Loses
  df$lose = as.logical(df$goal_diff<0)
  # Define Friendlies 
  df$friendly = as.logical(df$tournament=="Friendly")
  # Define Era's or Period's of World Cup Compaigns (8 years; two WC compaigns)
  df$era[df$year>"2014"] = "era 1" # Include current WC compaign by itself
  df$era[df$year>"2006" & df$year<="2014"] = "era 2"
  df$era[df$year>"1998" & df$year<="2006"] = "era 3"
  df$era[df$year>"1990" & df$year<="1998"] = "era 4"
  df$era[df$year>"1982" & df$year<="1990"] = "era 5"
  df$era[df$year>"1974" & df$year<="1982"] = "era 6"
  df$era[df$year>"1966" & df$year<="1974"] = "era 7"
  df$era[df$year>"1958" & df$year<="1966"] = "era 8"
  df$era[df$year<="1958"] = "era 9"
  # Create constant to be used as count variable latter
  df$match = 1
  
  
  ### Create Match Record Data ###
  # Collapse data to find Win, Lose, and Tie ratios and Match counts by opponent
  library(plyr)
  record = ddply(df, .(opponent, era), summarize,  win_mean=mean(win), 
                 lose_mean=mean(lose), tie_mean=mean(tie), match_count=sum(match))
  record$merge = paste(record$opponent,record$era)
  df$merge = paste(df$opponent,df$era)
  df = (merge(df, record, by.x = "merge", by.y = "merge", all.x = TRUE, no.dups = FALSE))
  # Clean resulting data
  df$opponent = df$opponent.x
  df$era = df$era.x
  df$opponent.x = NULL
  df$era.x = NULL
  df$opponent.y = NULL
  df$era.y = NULL
  
  return( df )
}





