#Read data
vehic_all<-read.csv('vehicles.csv')

#create new dataframe with just vehicles of interest
vehic<-subset(vehic_all, make=='Chevrolet'|make=='Nissan')
vehic<-droplevels(vehic)

#compute summary statistics for combined MPG for 
#Chevrolet and Nissan vehicles
vehic_chev<-subset(vehic,make=='Chevrolet')
vehic_nis<-subset(vehic,make=='Nissan')
sum_stats<-data.frame('make'=c('Chevrolet','Nissan'))
sum_stats$count<-c(nrow(vehic_chev),nrow(vehic_nis))
sum_stats<-cbind(sum_stats,(rbind(
  summary(vehic_chev$comb08),summary(vehic_nis$comb08))))
sum_stats$sd<-c(sd(vehic_chev$comb08),
                       sd(vehic_nis$comb08))
sum_stats$Mean<-round(sum_stats$Mean,digits=2)
sum_stats$sd<-round(sum_stats$sd,digits=2)

sum_stats

#create boxplot
boxplot(comb08~make,
        horizontal=TRUE,
        data=vehic,
        xlab='Combined MPG',
        ylab='')

#two sided t-test for Chevrolet vs. Nissan combined MPG
t.test(subset(vehic,make=='Chevrolet')$comb08,
       subset(vehic,make=='Nissan')$comb08,
       alternative='two.sided')

#examine outliers
ol_mpg<-sum_stats$`3rd Qu.`+
  1.5*(sum_stats$`3rd Qu.`-sum_stats$`1st Qu.`)
outliers_chev<-subset(vehic_chev,comb08>ol_mpg[1])
outliers_nis<-subset(vehic_nis,comb08>ol_mpg[1])

atv_freq<-rbind(table(outliers_chev$atvType),
                table(outliers_nis$atvType))

barplot(atv_freq)

#repeat with only cars and wagons
table(vehic$VClass)
cars<-subset(vehic,
        (regexpr('Cars',vehic$VClass,ignore.case=TRUE)>=0)|
          regexpr('Wagon',vehic$VClass,ignore.case=TRUE)>=0)
levels(droplevels(cars$VClass))

#compute summary statistics for combined MPG for 
#Chevrolet and Nissan cars and station wagons
cars_chev<-subset(cars,make=='Chevrolet')
cars_nis<-subset(cars,make=='Nissan')
sum_stats_cars<-data.frame('make'=c('Chevrolet','Nissan'))
sum_stats_cars$count<-c(nrow(cars_chev),nrow(cars_nis))
sum_stats_cars<-cbind(sum_stats_cars,(rbind(
  summary(cars_chev$comb08),summary(cars_nis$comb08))))
sum_stats_cars$sd<-c(sd(cars_chev$comb08),
                sd(cars_nis$comb08))
sum_stats_cars$Mean<-round(sum_stats_cars$Mean,digits=2)
sum_stats_cars$sd<-round(sum_stats_cars$sd,digits=2)

sum_stats_cars

#create boxplot
boxplot(comb08~make,
        horizontal=TRUE,
        data=cars,
        xlab='Combined MPG, Cars and Station Wagons',
        ylab='')

#two sided t-test for Chevrolet vs. Nissan combined MPG
t.test(subset(cars,make=='Chevrolet')$comb08,
       subset(cars,make=='Nissan')$comb08,
       alternative='two.sided')

#examine outliers



