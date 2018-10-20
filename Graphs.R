###Create Charts###
#German Team Rank Chart
library(plyr)
rank = ddply(rank, .(country_full, rank_date), summarize,  rank=mean(rank))
rank[rank$country_full=="Germany",]
library(ggplot2)
chart1 = ggplot(data=rank[(rank$country_full=="Germany" | rank$country_full=="Brazil" | rank$country_full=="England"),], 
                aes(x=rank_date, y=rank, group=country_full, color=country_full)) + 
  ylab("Rank") + scale_x_continuous("Year", breaks=seq(1993,2018,5)) +
  geom_line() + theme(legend.position="bottom") 
chart1 = chart1 + geom_line(data=rank[(rank$country_full=="Germany"),], size=1.5) 
chart1 = chart1 + scale_colour_discrete(name  ="Country:",
                                        breaks=c("Germany", "Brazil", "England"),
                                        labels=c("Germany", "Brazil", "England"))
chart1 

#Goal Differential Chart
chart2 = ggplot(df, aes(goal_diff, fill = friendly)) +
  geom_histogram(binwidth=.75, position="dodge") + 
  ylab("Count") + xlab("Goal Differential") + theme(legend.position="bottom") 
chart2
mean(df$goal_diff)
mean(df$goal_diff[df$era==1])

#Win Ratio Chart
chart3 = ggplot(data=df[(df$opponent=="Brazil" | df$opponent=="England"),], 
                aes(x=era, y=win_mean, group=opponent, color=opponent)) + 
  ylab("Win Ratio") + scale_x_discrete("Year") +
  geom_line() + theme(legend.position="bottom") 
chart3 = chart3 + scale_colour_discrete(name  ="Country:",
                                        breaks=c("Brazil", "England"),
                                        labels=c("Brazil", "England"))
chart3 

