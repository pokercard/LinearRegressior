#For simple linear regressor
#The observed values
X=c(40,50,60,70,80,90,40,60,80,50)
Y=c(69,175,272,335,490,415,72,265,492,180)



#Using Least Square
SimpleLR<-function(x,y){
  ybar<-mean(y)
  xbar<-mean(x)
  b1<-sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
  b0<-ybar-b1*xbar
  return(c(b0=b0,b1=b1))
}
est_values<-SimpleLR(X,Y)

#Estimate sigma
#Using ML, MLE is equal to LSE 
Est_Sigma<-function(x,y,est_values){
  b0<-est_values['b0']
  b1<-est_values['b1']
  n<-length(x)
  Unbiased_Sigma<-mean((y-(b0+b1*x))^2)
  Biased_Sigma<-sum((y-(b0+b1*x))^2)/(n-2)
  return(c(Unbiased=Unbiased_Sigma,Biased=Biased_Sigma))
}
est_sigmas<-Est_Sigma(X,Y,est_values)

#To conduct several test
SS_values<-function(x,y,est_values){
  ybar<-mean(y)
  b0<-est_values['b0']
  b1<-est_values['b1']
  SSTO<-sum((y-ybar)^2)
  SSR<-sum((y-(b0+b1*x))^2)
  SSE<-SSTO-SSR
  return(c(SSR=SSR,SSE=SSE,SSTO=SSTO))
}

ss_values<-SS_values(X,Y,est_values)

#Goodness of fit test







#Residual Plots