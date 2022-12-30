

{
  
rm(df)
  
library(stringi)
  
#some other operations:
  
imp <- function(p,q){
  return(paste("!(",p,")|(",q,")"))
}
bic <- function(p,q){
  return(paste("(",imp(p,q),")","&","(",imp(q,p),")"))
}

cat("\n\n")
x=readline("Enter the statement form involving variables and connectives (&,|,!):   ")
#x=imp("q|a","b")
#x=bic("a","b")
Y=unique(unlist(strsplit(x, split = "[^a-zA-Z]+",fixed=FALSE)))
y=c("z")

if(Y[1]==""){
  
  for (i in 2:(length(Y)))y[i-1]=Y[i]
  
}

else {
  y=Y
}

F=rep(list(c(FALSE, TRUE)), length(y))
df=expand.grid(F)
names(df)=y




df[ , "RESULT"] <- with(df, eval(parse(text = x)))

View(df)


cat("\n\nDNF:\n\n")
n=ncol(df)-1
cont=TRUE
for(i in 1:nrow(df)){
  if(df[i,ncol(df)]==TRUE){
    if(cont){
      cat("(",sep="")
      cont=FALSE
    }
    else{
      cat(" | (",sep="")
    }
    if(n>=2)
    for(j in 1:(n-1)){
      if(df[i,j]==TRUE){
        cat(c(y[j],"&"),sep="")
      }
      if(df[i,j]==FALSE){
        cat(c("!",y[j],"&"),sep="")
      }
    }
    if(df[i,n]==TRUE){
      cat(c(y[n]),sep="")
    }
    if(df[i,n]==FALSE){
      cat(c("!",y[n]),sep="")
    }
    cat(")",sep="")
  }
}
if(cont){
  cat("(!",y[1],"&",y[1],")","&",sep="")
  if(n>=2)
  for(i in 2:(n-1)){
    cat(c(y[i]," & "),sep="")
  }
  if(n>=2)
  cat(c(y[n]),sep="")
}

cat("\n\n\nCNF:\n\n")
n=ncol(df)-1
cont=TRUE
for(i in 1:nrow(df)){
  if(df[i,ncol(df)]==FALSE){
    if(cont){
      cat("(",sep="")
      cont=FALSE
    }
    else{
      cat(" & (",sep="")
    }
    if(n>=2)
    for(j in 1:(n-1)){
      if(df[i,j]==TRUE){
        cat(c("!",y[j],"|"),sep="")
      }
      if(df[i,j]==FALSE){
        cat(c(y[j],"|"),sep="")
      }
    }
    if(df[i,n]==TRUE){
      cat(c("!",y[n]),sep="")
    }
    if(df[i,n]==FALSE){
      cat(c(y[n]),sep="")
    }
    cat(")",sep="")
  }
}
if(cont){
  cat("(!",y[1],"|",y[1],")",sep="")
  if(n>=2)
  for(i in 2:(n-1)){
    cat(c(y[i]," | "),sep="")
  }
  if(n>=2)
  cat(c(y[n]),sep="")
}
cat("\n\n")
}
