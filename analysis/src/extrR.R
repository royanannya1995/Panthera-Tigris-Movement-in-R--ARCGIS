my1df<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/extinction rate.csv")
summary(my1df)
ext_rate<-0
e1<-my1df$Net_score_grid
e2<-my1df$Area
e3<-my1df$Birth_rate
e4<-my1df$Death_rate
for(i in 1:length(my1df$Net_score_grid))
{
  ext_rate[i]=((e1[i]*e2[i])+e3[i]-e4[i])/(e1[i]*e2[i])
}
print(ext_rate)
#my1df$extrate<-ext_rate
#str(my1df)
#write.csv(my1df,file="extinction rate.csv")

plot(my1df$extrate~my1df$Area,pch=1,col="Black",xlab="Extinction Rate",ylab="Area Occupancy",title="The probable migration rate of Tigers")