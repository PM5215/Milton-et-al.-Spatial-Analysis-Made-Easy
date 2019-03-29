set.seed(1234)
# Generate the epidemic data
n=200
x_big=seq(0,100,length.out=n)
y_big=1+ x_big + 0.2*x_big^2+rnorm(n,0,150)
y_big[y_big<0] <- 0 # make sure that we cant have nagtive infection No.
plot(x_big,y_big,pch=16,col='black',cex=1,ylab="Number of Cases",xlab="Number of Days")

# Train of a subset of the data
n_small=20
sm<-sort(sample(1:n,n_small))
x<-x_big[sm]
y<-y_big[sm]
plot(x,y,pch=16,col='black',cex=1,ylab="Number of Cases",xlab="Number of Days")

data_small=data.frame(x=x,y=y)
data_big=data.frame(x=x_big,y=y_big)

# 1st order polynomial - linear
plot(x,y,pch=16,col='black',cex=1,ylab="Number of Cases",xlab="Number of Days", xlim=c(0,100), ylim=c(0,2500))
l1=lm(y~x,data=data_small)
lines(x,l1$fitted.values,col='red',lwd=2)

# 2nd order polynomial 
l2=lm(y~x+I(x^2),data=data_small)
lines(x,l2$fitted.values,col='green',lwd=2)

# 5th order polynomial 
l5=lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5),data=data_small)
lines(x,l5$fitted.values,col='blue',lwd=2)


# Compute MSE on the testing data
error1=mean((y_big-predict(l1,newdata=data_big))^2)
error2=mean((y_big-predict(l2,newdata=data_big))^2)
error5=mean((y_big-predict(l5,newdata=data_big))^2)
error1
error2
error5


plot(x_big,y_big,pch=16, cex=1,ylab="Number of Cases",xlab="Number of Days", col="grey", xlim=c(0,100), ylim=c(0,2500))
lines(x,l1$fitted.values,col='red',lwd=2)

l1=lm(y~x+I(x^2),data=data_small)
lines(x,l2$fitted.values,col='green',lwd=2)

l1=lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5),data=data_small)
lines(x,l5$fitted.values,col='blue',lwd=2)
