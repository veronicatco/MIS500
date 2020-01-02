df<-read.csv('R/vehicles.csv')

my_vars=c('make','model','year','atvType','VClass','city08')
my_data<-df[my_vars]

cars.chevrolet<-my_data[which(my_data$make=='Chevrolet'),]
cars.nissan<-my_data[which(my_data$make=='Nissan'),]

cars.sub<-my_data[which(my_data$VClass=='Subcompact Cars'),]
cars.mid<-my_data[which(my_data$VClass=='Midsize Cars'),]

boxplot(cars.nissan$city08,cars.chevrolet$city08,
        horizontal = TRUE,
        names=c('Nissan','Chevrolet'))

boxplot(cars.sub$city08,cars.mid$city08,
        horizontal=TRUE,
        names=c('Subcompact','Midsize Cars'))

#boxplot(my_data~make,subset=c('Nissan','Chevrolet'))

#cars.chevrolet[which(cars.chevrolet$city08>80),]

#library(dplyr)
#group_by(my_data,make)%>%
#  summarise(
#    count=n(),
#    mean=mean(city08,na.rm=TRUE),
#    sd=sd(city08,na.rm=TRUE)
#  )
     