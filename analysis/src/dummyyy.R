#module 1 and 2 and 3

library(leaflet)
m <- leaflet() %>% setView(lng = 77.0589, lat = 23.3601, zoom = 4)
m %>% addTiles()


library(leaflet)
newdf1<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/tigerss.csv")
m <- leaflet(data =newdf1[2:19,]) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions(),popup=~Reserves
)%>%setView(75.0589,23.3601,4)%>%addGraticule(interval = 2, style = list(color = "Black", weight = 1))
m


library(leaflet)
newdf1<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/tigerss.csv")
m <- leaflet(data =newdf1[2:19,]) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions(),popup=~Reserves
)%>%setView(75.0589,23.3601,4)%>%addGraticule(interval = 2, style = list(color = "Black", weight = 1))%>% addRectangles(
  lng1=70.00, lat1=16.00,
  lng2=90.00, lat2=30.00,
  color="red",
  fillColor = "blue"
)
m

#module 4 and 5
newdf2<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/decvar.csv")
summary(newdf2)

p1<-newdf2$Hoax
p2<-newdf2$PreyBase
p3<-newdf2$Predators
p4<-newdf2$ForestCover_Water
p5<-newdf2$HumanDisturbance
p6<-newdf2$OtherSpecies

h<-"hawk"
d<-"dove"
ht<-"x"
hp<-"y"

thr_tiger<-5.0
thr_prey<-2.6
thr_predator<-5.5
thr_forest<-3.2
thr_human<-4.0
thr_other<-3.5

tt<-c(thr_prey,thr_predator,thr_forest,thr_human,thr_other)
tt

k<-1
t<-1
for(j in 5:9)
{
  p<-newdf2[,j]
  n<-length(p)
  pay<-0
  for(i in 1:n)
  {
    if(thr_tiger>=p[i])
    {
      ht[i]<-h
    }
    else ht[i]<-d
    
    if(p[i]>=tt[k])
    {
      hp[i]<-h
    }
    else hp[i]<-d
  }
  k=k+1
  for(ii in 1:n)
  {
    if(identical(ht[ii],h) && identical(hp[ii],h))
      pay[ii]=1
    
    if(identical(ht[ii],h) && identical(hp[ii],d))
      pay[ii]=6
    
    if(identical(ht[ii],d) && identical(hp[ii],h))
      pay[ii]=-6
    
    if(identical(ht[ii],d) && identical(hp[ii],d))
      pay[ii]=3
  }
  print(pay)
  plot(pay~p,pch=1,col="blue",xlab="Individual Scores of Decision Variables",ylab="Payoff",title="Rank vs Payoff in Hawk and Dove GT")
}


Ndata<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/decvar.csv")
summary(Ndata)
nx<-length(Ndata$p_prey)
x<-0
nnd<-0
for(i in 1:nx)
{
  sumdata<-0
  
  for(j in 20:24)
  {
    p2<-Ndata[i,j]
    x<-p2
    sumdata=sumdata+x
    
  }
  sumdata=sumdata*thr_tiger
  nnd[i]=sumdata
}
print(nnd)
plot(nnd~Ndata$GridNo,pch=10,col="black",xlab="Habitat Grid",ylab="Score of Grid")

#module 6 and 7

rr1<-rank(-nnd)
print(rr1)

plot(Ndata$Rank~Ndata$GridNo,pch=7,col="green",xlab="Habitat Grid",ylab="Rank of Habitat Grid",title="Score vs Rank in Hawk and Dove GT")

#add rank to data frame
#Ndata$Rank=rr1
#str(Ndata)
#write.csv(Ndata,file="decvar.csv")

mycolor1<-"xx"
for(i in 1:n)
{
  if(rr1[i]>=0 && rr1[i]<30)
    mycolor1[i]="Green"
  if(rr1[i]>=30 && rr1[i]<60)
    mycolor1[i]="Orange"
  if(rr1[i]>=60 && rr1[i]<90)
    mycolor1[i]="Brown"
  if(rr1[i]>=90 && rr1[i]<120)
    mycolor1[i]="Pink"
  if(rr1[i]>=120 && rr1[i]<150)
    mycolor1[i]="Blue"
  if(rr1[i]>=150 && rr1[i]<180)
    mycolor1[i]="Purple"
  if(rr1[i]>=180 && rr1[i]<210)
    mycolor1[i]="Yellow"
  if(rr1[i]>=210 && rr1[i]<240)
    mycolor1[i]="Black"
  if(rr1[i]>=240 && rr1[i]<270)
    mycolor1[i]="Grey"
  if(rr1[i]>=270 && rr1[i]<300)
    mycolor1[i]="Red"
}

print(mycolor1)

#add color to data frame
#Ndata$mycolor=mycolor1
#str(Ndata)
#write.csv(Ndata,file="decvar.csv")


xdf1<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/my1.csv")

Ndata<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/decvar.csv")
new1<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/mat.csv")
str(new1)
colm1<-Ndata$GridNo
colm2<-Ndata$Latitude
colm3<-Ndata$Longitude
colm4<-Ndata$mycolor

for(i in 2:235)
{
  if(identical(colm4[i],"Black"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid=x1
    new1$lat=x2
    new1$lng=x3
    str(new1)
    write.csv(new1,file="mat.csv") 
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Blue"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid1=x1
    new1$lat1=x2
    new1$lng1=x3
    str(new1)
    write.csv(new1,file="mat.csv") 
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Brown"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid2=x1
    new1$lat2=x2
    new1$lng2=x3
    str(new1)
    write.csv(new1,file="mat.csv")   
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Green"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid3=x1
    new1$lat3=x2
    new1$lng3=x3
    str(new1)
    write.csv(new1,file="mat.csv")   
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Orange"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid4=x1
    new1$lat4=x2
    new1$lng4=x3
    str(new1)
    write.csv(new1,file="mat.csv")    
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Pink"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid5=x1
    new1$lat5=x2
    new1$lng5=x3
    str(new1)
    write.csv(new1,file="mat.csv")   
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Purple"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid6=x1
    new1$lat6=x2
    new1$lng6=x3
    str(new1)
    write.csv(new1,file="mat.csv")   
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Yellow"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid7=x1
    new1$lat7=x2
    new1$lng7=x3
    str(new1)
    write.csv(new1,file="mat.csv")    
  }
}

summary(xdf1)

leaflet(xdf1) %>% addTiles() %>%addGraticule(interval = 2, style = list(color = "Black", weight = 1))%>% addRectangles(
  lng1=70.00, lat1=16.00,
  lng2=90.00, lat2=30.00,
  color="Red",
  fillColor = "transparent"
)%>% addCircles(lng = xdf1$lng, lat = xdf1$lat, weight = 1,
                radius = sqrt(xdf1$Pop) * 20,popup = ~xdf1$mygrid, color="black"
)%>% addCircles(lng = xdf1$lng1, lat = xdf1$lat1, weight = 1,
                radius = sqrt(xdf1$Pop1) * 20,popup = ~xdf1$mygrid1, color="blue"
)%>% addCircles(lng = xdf1$lng2, lat = xdf1$lat2, weight = 1,
                radius = sqrt(xdf1$Pop2) * 20,popup = ~xdf1$mygrid2, color="brown"
)%>% addCircles(lng = xdf1$lng3, lat = xdf1$lat3, weight = 1,
                radius = sqrt(xdf1$Pop3) * 20,popup = ~xdf1$mygrid3, color="green"
)%>% addCircles(lng = xdf1$lng4, lat = xdf1$lat4, weight = 1,
                radius = sqrt(xdf1$Pop4) * 20,popup = ~xdf1$mygrid4, color="Orange"
)%>% addCircles(lng = xdf1$lng5, lat = xdf1$lat5, weight = 1,
                radius = sqrt(xdf1$Pop5) * 20,popup = ~xdf1$mygrid5, color="pink"
)%>%addCircles(lng = xdf1$lng6, lat = xdf1$lat6, weight = 1,
               radius = sqrt(xdf1$Pop6) * 20,popup = ~xdf1$mygrid6, color="purple"
)%>%addCircles(lng = xdf1$lng7, lat = xdf1$lat7, weight = 1,
               radius = sqrt(xdf1$Pop7) * 20,popup = ~xdf1$mygrid7, color="yellow"
)


#module 8

mmcount<-0
myhabitat<-0
for(i in 1: n)
{  
  if(identical(mycolor1[i],"Green"))
  {
    myhabitat[i]=100
    mmcount=mmcount+1
  }
  else myhabitat[i]=-1
  
}

#add habitat to data frame
#Ndata$habitat=myhabitat
#str(Ndata)
#write.csv(Ndata,file="decvar.csv")

Ndata<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/decvar.csv")
summary(Ndata)
new1<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/mat.csv")
str(new1)
colm1<-Ndata$GridNo
colm2<-Ndata$Latitude
colm3<-Ndata$Longitude
colm4<-Ndata$mycolor

for(i in 2:235)
{
  if(identical(colm4[i],"Black"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid=x1
    new1$lat=x2
    new1$lng=x3
    str(new1)
    write.csv(new1,file="mat.csv") 
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Blue"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid1=x1
    new1$lat1=x2
    new1$lng1=x3
    str(new1)
    write.csv(new1,file="mat.csv") 
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Brown"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid2=x1
    new1$lat2=x2
    new1$lng2=x3
    str(new1)
    write.csv(new1,file="mat.csv")   
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Green"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid3=x1
    new1$lat3=x2
    new1$lng3=x3
    str(new1)
    write.csv(new1,file="mat.csv")   
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Orange"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid4=x1
    new1$lat4=x2
    new1$lng4=x3
    str(new1)
    write.csv(new1,file="mat.csv")    
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Pink"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid5=x1
    new1$lat5=x2
    new1$lng5=x3
    str(new1)
    write.csv(new1,file="mat.csv")   
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Purple"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid6=x1
    new1$lat6=x2
    new1$lng6=x3
    str(new1)
    write.csv(new1,file="mat.csv")   
  }
}
for(i in 2:235)
{
  if(identical(colm4[i],"Yellow"))
  {
    x1[i]<-colm1[i]
    x2[i]<-colm2[i]
    x3[i]<-colm3[i]
    print(colm1[i],colm2[i],colm3[i])
    new1$mygrid7=x1
    new1$lat7=x2
    new1$lng7=x3
    str(new1)
    write.csv(new1,file="mat.csv")    
  }
}

neww<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/decvar.csv")
summary(neww)
gethabitat<-neww$habitat
mygrid<-200
mylat<-0
mylong<-0
myscore<-0
myid<-0
pp<-0
for(i in 2:234)
{
  if(gethabitat[i]>0)
  { mygrid[pp]<-neww$GridNo[i]
  mylat[pp]<-neww$Latitude[i]
  mylong[pp]<-neww$Longitude[i]
  myscore[pp]<-neww$Net_score_grid[i]
  myid[pp]<-neww$Identity[i]
  pp=pp+1}
  
}
xdf<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/griddata2.csv")
summary(xdf)
#add habitat to data frame
#xdf$mygrid=mygrid
#xdf$mylat=mylat
#xdf$mylong=mylong
#xdf$myscore=myscore
#xdf$id<-myid
#str(xdf)
#write.csv(xdf,file="griddata2.csv")


leaflet(xdf) %>% addTiles() %>%addGraticule(interval = 2, style = list(color = "Black", weight = 1))%>% addRectangles(
  lng1=70.00, lat1=16.00,
  lng2=90.00, lat2=30.00,
  color="Red",
  fillColor = "transparent"
)%>% addCircles(lng = xdf$mylong, lat = xdf$mylat, weight = 1,
             radius = sqrt(xdf$Pop) * 20,popup = ~xdf$mygrid, color="green"
  )



#module 9

library(igraph) 
D <- read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/adjmat.csv")
D<-D[,-1]
G <- graph.adjacency(as.matrix(D), weighted=TRUE) 

## Some graphical parameters 
V(G)$label <- V(G)
V(G)$shape <- "circle" 
V(G)$color <- "yellow" 
V(G)$size <- 29

## MST and plot 
mst <- minimum.spanning.tree(G) 

lay <- layout.reingold.tilford(G,params=list(root=1))
plot(mst, layout=lay)


#module 10

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

plot(my1df$extrate~my1df$Area,pch=1,col="Black",xlab="Area Occupancy",ylab="Extinction Rate ",title="The probable migration rate of Tigers")