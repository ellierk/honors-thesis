df1 <- squid %>% select(site, date, spacing_m, temp_avg, sal_avg, dist_from_bags, reclaimed, absent)
df2 <- df1[1,] %>% mutate(consumed = "1")
v = df1$reclaimed
i = 1
l=1

while(i < length(v)) {
  l=1
  while(l < v[i]) {
    ifelse(l < df1$absent[i], 
           (df2 = df2 %>% add_row(site = df1$site[i], date = df1$date[i], spacing_m = df1$spacing_m[i], temp_avg= df1$temp_avg[i],
                                  sal_avg= df1$sal_avg[i], dist_from_bags = df1$dist_from_bags[i], reclaimed = df1$reclaimed[i],
                                      absent = df1$absent[i], consumed = "1")), 
           (df2 = df2 %>% add_row(site = df1$site[i], date = df1$date[i], spacing_m = df1$spacing_m[i], temp_avg= df1$temp_avg[i],
                                  sal_avg= df1$sal_avg[i], dist_from_bags = df1$dist_from_bags[i], reclaimed = df1$reclaimed[i],
                     absent = df1$absent[i], consumed = "0"))
           )
    
    l = l+1
  }
  i = i+1
}

write.csv(df2, "logreg.csv")
