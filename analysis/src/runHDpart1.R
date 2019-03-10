#module 1 and 2 and 3

library(leaflet)
m <- leaflet() %>% setView(lng = 77.0589, lat = 23.3601, zoom = 4)
m %>% addTiles()


library(leaflet)
newdf1<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/tigerss.csv")
m <- leaflet(data =newdf1[2:19,]) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions(),popup=~Reserves
)%>%setView(75.0589,23.3601,4)%>%addGraticule(interval = 2, style = list(color = "Black", weight = 1))%>% addRectangles(
  lng1=70.00, lat1=16.00,
  lng2=90.00, lat2=30.00,
  color="red",
  fillColor = "red"
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
  plot(pay,pch=1,col="blue",xlab="Rank",ylab="Payoff",title="Rank vs Payoff in Hawk and Dove GT")
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


#module 6 and 7

rr1<-rank(-nnd)
print(rr1)

plot(neww$Net_score_grid~neww$Rank,pch=1:2,col=c("blue","green"),xlab="Rank",ylab="Score",title="Score vs Rank in Hawk and Dove GT")

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
  {
    mygrid[pp]<-neww$GridNo[i]
    mylat[pp]<-neww$Latitude[i]
    mylong[pp]<-neww$Longitude[i]
    myscore[pp]<-neww$Net_score_grid[i]
    myid[pp]<-neww$Identity[i]
    pp=pp+1
  }
  
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
V(G)$color <- "black" 
V(G)$size <- 29

## MST and plot 
mst <- minimum.spanning.tree(G) 

lay <- layout.reingold.tilford(G,params=list(root=1))
plot(mst, layout=lay)


