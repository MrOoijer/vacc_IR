# get and clean data script
# unfortunately, only download manually seems possible

df1<- read.csv("./input/cases-among-vaccinated-134.csv") # post vaccination validation
df2<- read.csv("./input/event-among-vaccinated-79.csv") # Hosp and Mortality of the vaccinated
df3<- read.csv("./input/vaccinated-per-day-2021-09-28.csv") # Ages of the vaccinated

df3$date<- as.Date(df3[,1])
require(lubridate)
df3$weekstart <- df3$date+1-wday(df3$date)

df3$first_dose[df3$first_dose == "<15"] <- "8"
df3$first_dose<- as.integer(df3$first_dose)

df3$second_dose[df3$second_dose == "<15"] <- "8"
df3$second_dose<- as.integer(df3$second_dose)

df3$third_dose[df3$third_dose == "<15"] <- "8"
df3$third_dose<- as.integer(df3$third_dose)

df1[df1== "<5"] <- "3"
for(i in 3:15) df1[,i] <- as.integer(df1[, i])
df1[is.na(df1)] <- 0
df1$weekstart<- as.Date(substr(df1[,1], 1, 10))

#miising 90+ in weeks: 
M<- c("2021-05-09", "2021-05-30",
      "2021-06-06", "2021-06-13")
Z<- head(df1, 4)
Z[,1]<- "xx"
Z[,2]<- "90+"
Z[, 3:15] <- 0
Z[, 16] <- as.Date(M)
df1<- rbind(df1, Z)
df1<- df1[order(df1$weekstart, df1$Age_group),]

population<- read.csv("./input/populatie.csv")
##################################

# per week vaccinated

vacc_per_week<- aggregate(.~ weekstart+age_group, df3[,-c(1,6)], sum)
age_group<- unique(vacc_per_week$age_group)

fully_vac<- data.frame(weekstart=unique(vacc_per_week$weekstart))
for(a in age_group){
  tmp<- vacc_per_week[vacc_per_week$age_group == a, c(1,4)]
  fully_vac<- cbind(fully_vac, cumsum(tmp[,2]))
}
names(fully_vac)[-1] <- age_group

triple_vac<- data.frame(weekstart=unique(vacc_per_week$weekstart))
for(a in age_group){
  tmp<- vacc_per_week[vacc_per_week$age_group == a, c(1,5)]
  triple_vac<- cbind(triple_vac, cumsum(tmp[,2]))
}
names(triple_vac)[-1] <- age_group


single_vac<- data.frame(weekstart=unique(vacc_per_week$weekstart))
for(a in age_group){
  tmp<- vacc_per_week[vacc_per_week$age_group == a, c(1,3)]
  single_vac<- cbind(single_vac, cumsum(tmp[,2]))
}
names(single_vac)[-1] <- age_group

not_vac <- data.frame(weekstart=unique(single_vac$weekstart))
for(a in age_group){
  tmp<- population$size[population$age_group == a]-
    single_vac[, a]
  not_vac<- cbind(not_vac, tmp)
}
names(not_vac)[-1] <- age_group

single_vac[, -1] <- single_vac[, -1] - fully_vac[, -1]

save(df1, df2, df3, file="./data/df.Rdata")
save(triple_vac, fully_vac, single_vac, not_vac, population, vacc_per_week, 
     file="./data/vacc.Rdata")
