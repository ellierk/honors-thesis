
library(dplyr)
library(ggplot2)
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
Q <- quantile(squid$consump, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(squid$consump)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
squidElim<- subset(squid,
                   squid$consump > (Q[1] - 1.5*iqr) & squid$consump < (Q[2]+1.5*iqr))
plot(density(squidElim$consump))

#linreg 
consumpByDist <- lm(consump ~ dist_from_bags, data = squidElim)
AIC(consumpByDist)
plot(resid(consumpByDist))

# quantile regression attempts
plot(squidElim$dist_from_bags, squidElim$consump)
taus <- c(.05,.1,.25,.75,.90,.95)
fit1 <- rq(consump ~ dist_from_bags, tau = .5, data = squidElim)
abline(fit1)
for(i in 1:length(taus)) {
  abline(rq(consump ~ dist_from_bags, tau = taus[i], data = squidElim))
}

# glm model here
