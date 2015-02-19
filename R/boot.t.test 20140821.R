#********************************************************************
### BOOTSTRAP T-TEST FUNCTION

boot.t.test<-function(x,y,reps=1000,alternative="two.sided")
{
  nx<-length(x)
  ny<-length(y)
  t.obs<-(mean(x)-mean(y))/sqrt(var(x)/nx+var(y)/ny)
  x.c<-x-mean(x)+mean(c(x,y))
  y.c<-y-mean(y)+mean(c(x,y))
  t.boot<-rep(NA,times=reps)
  for(r in 1:reps){
    bootx<-x.c[sample(1:nx,size=nx,replace=T)]
    booty<-y.c[sample(1:ny,size=ny,replace=T)]
    t.boot[r]<-(mean(bootx)-mean(booty))/sqrt(var(bootx)/nx+var(booty)/ny)
  }
  if(alternative=="two.sided"){pval<-length(t.boot[abs(t.boot)>abs(t.obs)])/reps}
  else{pval<-length(t.boot[t.boot>t.obs])/reps}
  return(pval)
}

#********************************************************************