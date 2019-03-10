xdff<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/newIndia.csv")
summary(xdff)
gg<-xdff$mygrid
a<-xdff$mylat
b<-xdff$mylong
id<-xdff$id
RR<-0
Record<-matrix(data=0,nrow=length(gg),ncol=length(gg),byrow=FALSE)
index<-0
recursive.myhab<-function(RR,a,b)
{
for(i in 2:29)
{
  for(k in 2:29)
  {
    j<-2
    index<-id[i]
    {
      if(index>=1)
      {
       Record[i][k]<-gg[i] 
    recursive.myhab(RR,a[j+1],b)
    recursive.myhab(RR,a,b[j+1])
    recursive.myhab(RR,a[j-1],b[j+1])
    recursive.myhab(RR,a[j+1],b[j-1])
    recursive.myhab(RR,a[j+1],b[j+1])
    recursive.myhab(RR,a[j-1],b[j-1])
    recursive.myhab(RR,a[j-1],b)
    recursive.myhab(RR,a,b[j-1])
      }
      else
      {
        Record[i][j]=0
        return(Record)
      }
      
    }
  }
  }
}
mmm1<-read.csv("C:/Users/varsha/Desktop/wildlife metapopulation dynamics analysis/analysis/adjmat.csv")
Record<-as.matrix(mmm1)
