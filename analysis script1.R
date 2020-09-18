install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

squid.rawdata = read.csv("kremer csv squidpop data.csv", header = TRUE)
summary(squid.data)

squid = squid.rawdata %>% mutate(consump = absent/reclaimed)

plot(squid$dist_from_bags, squid$consump)

squid %>% 
  ggplot(aes(x = dist_from_bags, y = consump, color = site)) + 
  geom_point() 

site.avgbydist = squid %>% 
    group_by(site, dist_from_bags)%>% 
    summarise(mean(consump)) %>% rename(avg_consump = 'mean(consump)')

site.avgbydist %>% 
  ggplot(aes(x = dist_from_bags, y = avg_consump, color = site)) + 
  geom_point()
