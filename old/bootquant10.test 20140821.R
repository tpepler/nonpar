#********************************************************************
### BOOTQUANT10.TEST FUNCTION

bootquant10.test<-function(x, y, reps=1000){
  # x: first sample (as a vector)
  # y: second sample (as a vector)
  # reps: number of bootstrap replications
  
  x.n<-length(x)
  y.n<-length(y)
  bound<-quantile(x,probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  indvec<-rep(1,times=length(x))
  indvec[((x>bound[1]) & (x<=bound[2]))]<-2
  indvec[((x>bound[2]) & (x<=bound[3]))]<-3
  indvec[((x>bound[3]) & (x<=bound[4]))]<-4
  indvec[((x>bound[4]) & (x<=bound[5]))]<-5
  indvec[((x>bound[5]) & (x<=bound[6]))]<-6
  indvec[((x>bound[6]) & (x<=bound[7]))]<-7
  indvec[((x>bound[7]) & (x<=bound[8]))]<-8
  indvec[((x>bound[8]) & (x<=bound[9]))]<-9
  indvec[x>bound[9]]<-10
  
  changeprop<-rep(NA,times=reps)
  for(i in 1:reps){
    bootsamp<-x[sample(1:x.n,size=y.n,replace=TRUE)]
    bootbound<-quantile(c(x,bootsamp),probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
    bootindvec<-rep(1,times=x.n)
    bootindvec[((x>bootbound[1]) & (x<=bootbound[2]))]<-2
    bootindvec[((x>bootbound[2]) & (x<=bootbound[3]))]<-3
    bootindvec[((x>bootbound[3]) & (x<=bootbound[4]))]<-4
    bootindvec[((x>bootbound[4]) & (x<=bootbound[5]))]<-5
    bootindvec[((x>bootbound[5]) & (x<=bootbound[6]))]<-6
    bootindvec[((x>bootbound[6]) & (x<=bootbound[7]))]<-7
    bootindvec[((x>bootbound[7]) & (x<=bootbound[8]))]<-8
    bootindvec[((x>bootbound[8]) & (x<=bootbound[9]))]<-9
    bootindvec[x>bootbound[9]]<-10
    changevec<-bootindvec-indvec
    changeprop[i]<-(x.n-length(changevec[changevec==0]))/x.n
  }
  
  newbound<-quantile(c(x,y),probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  newindvec<-rep(1,times=x.n)
  newindvec[((x>newbound[1]) & (x<=newbound[2]))]<-2
  newindvec[((x>newbound[2]) & (x<=newbound[3]))]<-3
  newindvec[((x>newbound[3]) & (x<=newbound[4]))]<-4
  newindvec[((x>newbound[4]) & (x<=newbound[5]))]<-5
  newindvec[((x>newbound[5]) & (x<=newbound[6]))]<-6
  newindvec[((x>newbound[6]) & (x<=newbound[7]))]<-7
  newindvec[((x>newbound[7]) & (x<=newbound[8]))]<-8
  newindvec[((x>newbound[8]) & (x<=newbound[9]))]<-9
  newindvec[x>newbound[9]]<-10
  newchangevec<-newindvec-indvec
  newchangeprop<-(x.n-length(newchangevec[newchangevec==0]))/x.n
  p.value<-length(changeprop[changeprop>=newchangeprop])/reps # p-value for null hypothesis of no change
  
  return(list(statistic=newchangeprop,crit.val=quantile(changeprop,prob=0.95),p.value=p.value))
}
#********************************************************************