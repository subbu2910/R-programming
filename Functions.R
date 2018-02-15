
#Addition
add <- function(a,b)
{
  return(a+b)
}

#Sum of Square

Sumsqt <-function(x)
{
  sum((x^2))
}

##Variance
Var <-function(x,about=mean(x))
{
  x=x[!is.na(x)]#Handling Null values in the Variable selection
  y=sum((x - about)^2)
  return(y)
}

#std
std <-function(x,about=mean(x))
{
  x=x[!is.na(x)]#Handling Null values in the Variable selection
  y=sum((x - about)^2)
  z=y^1/2
  return(z)
}

#Mean Absolute Error
MAE <-function(actual,predicted)
{
  mean(abs(actual-predicted))
}

#Ordering Categorical and Numerical Variables
orderVar <-function(data)
{
  type <-vector()
  for(i in 1:ncol(mydata))
  {
    type[i] <-class(mydata[,i])
  }
  
  varType <-cbind(name=names(data),type)
  #View(varType)
  varType<-varType[order(type),]
  data<-subset(data,select = varType[,"name"])
  return(data)
}

### T-test for all numeric variable

tTest <- function(data,varindx,responseindx)
{
  pvalue<-numeric()
  for(i in varindx)
  {
    if(is.factor(data[,i]))
    {
      stop(paste("The variable is not numeric"))
    }
    else
    {
      pvalue[i] <-t.test(data[,i]~data[,responseindx])$p.value
    }
  }
  pvalue<-pvalue[!is.na(pvalue)]
  names(pvalue)<-names(data[,varindx])
  pvalue<-as.data.frame(pvalue)
  colnames(pvalue)<-"p=value"
  return(round(pvalue,8))
}



### chi-test for all categorical variable

chiTest <- function(data,varindx,responseindx)
{
  pvalue<-numeric()
  for(i in varindx)
  {
    if(!is.factor(data[,i]))
    {
      stop(paste("The variable is not categorical"))
    }
    else
    {
      pvalue[i] <-chisq.test(table(data[,i],data[,responseindx]))$p.value
    }
  }
  pvalue<-pvalue[!is.na(pvalue)]
  names(pvalue)<-names(data[,varindx])
  pvalue<-as.data.frame(pvalue)
  colnames(pvalue)<-"p=value"
  return(round(pvalue,8))
}

