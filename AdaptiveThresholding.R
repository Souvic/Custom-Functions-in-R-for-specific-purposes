integrate=function(mat){
  for(i in 2:nrow(mat)){
    mat[i,]=mat[(i-1),]+mat[i,]
    
  }
  for(i in 2:ncol(mat)){
    mat[,i]=mat[,(i-1)]+mat[,i]
    
  }
  return(mat)
}
thresholds=function(mat,s){
  av=mat*0
  mat=integrate(mat)
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      xmx=(i+s>nrow(mat))
      xmx=xmx*(nrow(mat))+(1-xmx)*(i+s)
      xmn=(i>s)
      xmn=xmn*(i-s)+(1-xmn)*i
      ymx=(j+s>ncol(mat))
      ymx=ymx*(ncol(mat))+(1-ymx)*(j+s)
      ymn=(j>s)
      ymn=ymn*(j-s)+(1-ymn)*j
      
      av[i,j]=(mat[xmx,ymx]+mat[xmn,ymn]-mat[xmx,ymn]-mat[xmn,ymx])/((ymx-ymn+1)*(xmx-xmn+1))
    }
    
  }
  return(av)
}

mat=readTIFF("sd.tif",convert=T)[,,1]
th=thresholds(mat,10)
res=(1.05*(th)>1)
res=res*(1.05*(th))
res=1*(mat>res)
writeTIFF(res,"sdres.tiff")




