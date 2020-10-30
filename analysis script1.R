
library(dplyr)
library(ggplot2)
library(quantreg)
library(quantreg)
library(lme4)

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

#removing outliers
Q <- quantile(squid$consump, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(squid$consump)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
squidElim<- subset(squid,
                   squid$consump > (Q[1] - 1.5*iqr) & squid$consump < (Q[2]+1.5*iqr))
# data distribution without outliers
plot(density(squidElim$consump))

# avg point without outliers
squid.sd = squidElim %>% 
  group_by(site, dist_from_bags)%>% 
  summarise(mean(consump), sd(consump)) %>% 
  rename(avg_consump = 'mean(consump)', std_dev = 'sd(consump)') 
  
squid.sd %>% ggplot(aes(x = dist_from_bags, y = avg_consump, color = site)) + 
  geom_point()

# barplot
squid.sd %>%
  ggplot(aes(x = dist_from_bags, y = avg_consump, fill = site)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_errorbar(aes(ymin = avg_consump, ymax = avg_consump+std_dev), width=.2,
                position=position_dodge(2)) +
  xlab("Distance from bags (m)") + ylab("Average % Consumption") +
  labs(title = "Average % Consumption of Squidpops by Lease and Treatment", )

# dot plot without outliers
ggplot(data = squidElim, aes(x=dist_from_bags, y = consump, col = site)) +
  geom_point() + xlab("Distance from bags (m)") + ylab("% Consumption") + 
  labs(title = "% Consumption by Distance")
#+ geom_smooth(method = "lm", formula = y ~ x)

# quantile regression 

ggplot(data = squidElim, aes(x=dist_from_bags, y = consump)) +
  geom_point() +geom_quantile(quantiles = c(.1, .25, .5, .75, .95)) +
  xlab("Distance from bags (m)") + ylab("% Consumption") + 
  labs(title = "% Consumption by Distance")


#linreg 
consumpByDist <- lm(consump ~ dist_from_bags, data = squidElim)
AIC(consumpByDist)
plot(resid(consumpByDist))

# glm model here

consumpMulti <- glm(consump ~ dist_from_bags + site +
                      temp_avg + sal_avg + 
                      spacing_m + soak_start, family = binomial,
                    data = squidElim)
summary(consumpMulti)

# glmer
#consumpMixed <- glmer(consump ~ dist_from_bags + 
 #                       temp_avg + sal_avg + 
  #                      spacing_m + soak_start + (1|site),
   #                   family = binomial,
    #                  data = squidElim)
