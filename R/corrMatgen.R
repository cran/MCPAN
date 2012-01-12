`corrMatgen` <-
function(CM,varp)

      {

#print("var")      
#print(varp)
      k <- ncol(CM)
#print("k")      
#print(k)
      if( length(varp) != k )
       {stop("number of columns and length of var must be the same")}


    l <- nrow(CM)

      cor <- matrix(0, ncol=l, nrow=l)

    for(i in 1:l)
    {
      for(j in 1:l)
      {
        cor[i,j]<-sum( CM[i,]*CM[j,]*varp ) / sqrt( sum( (CM[i,]^2)*varp ) * sum( (CM[j,]^2)*varp ) )
       }

    }

      return(cor)
    }

