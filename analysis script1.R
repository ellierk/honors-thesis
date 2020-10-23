
library(dplyr)
library(ggplot2)
library(quantreg)
library(quantreg)

squid.rawdata = read.csv("kremer consump assay data.csv", header = TRUE)
summary(squid.rawdata)

squid = squid.rawdata %>% slice(6:65)

squid %>% 
  ggplot(aes(x = dist_from_bags, y = consump, color = site)) + 
  geom_point() 


boxplot(squid$consump)
boxplot((squid%>% filter(site == "mera"))$consump)
boxplot((squid%>% filter(site == "styron"))$consump)

site.avgbydist = squid %>% 
    group_by(site, dist_from_bags)%>% 
    summarise(mean(consump)) %>% rename(avg_consump = 'mean(consump)')

site.avgbydist %>% 
  ggplot(aes(x = dist_from_bags, y = avg_consump, color = site)) + 
  geom_point()

# with outliers
plot(density(logit(squid$consump)))

#without outliers
plot(density((squid %>% filter(consump < .7))$consump))

#linreg 
consumpByDist <- lm(consump ~ dist_from_bags, data = squid)
AIC(consumpByDist)
plot(resid(consumpByDist))

# quantile regression attempts
plot(squid$dist_from_bags, squid$consump)
taus <- c(.05,.1,.25,.75,.90,.95)
fit1 <- rq(consump ~ dist_from_bags, tau = .5, data = squid)
abline(fit1)
for(i in 1:length(taus)) {
  abline(rq(consump ~ dist_from_bags, tau = taus[i], data = squid))
}
