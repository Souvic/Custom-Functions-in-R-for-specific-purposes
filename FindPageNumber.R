
Pagenumber <- function(textFileConten) {
  #print(length(textFileConten[[1]]))
  if(textFileConten[[1]]=="")
  {
    return(0)
  }
  #print("CALLING THE PageNumber FUNCTION")
  textFileConten<-tolower(textFileConten)
  assq<-strsplit(textFileConten[[1]]," ")[[1]]
  assq<-paste(assq, collapse="")
  if(assq=="")
  {
    return(0)
  }
  pageNumPattern2 <- regexpr("p[a|@][c|g|8|&]e[t|a|l|i|!|b|g|1|2|3|4|5|6|7|8|9|o|s](.*)/[o|s|t|a|l|!|i|b|g|1|2|3|4|5|6|7|8|9|0](.*)", assq, ignore.case=T, perl=T)[1]
  #print(pageNumPattern2)
  if(pageNumPattern2>0){
    newassq<-strsplit(assq,"")[[1]]
    newassq<-newassq[pageNumPattern2:length(newassq)]
    
    newassq<-paste(newassq,collapse="")
    qnewassq<-(unlist(strsplit(newassq, "/")))
    qnewassq1<-unlist(strsplit(qnewassq[1], ""))
    qnewassq1<-qnewassq1[5:length(qnewassq1)]
    qnewassq[1]<-paste(qnewassq1,collapse="")
    qnewassq[2]<-(strsplit(gsub("[^0-9]", " ",qnewassq[2])," ")[[1]])[1]
    newassq<-qnewassq
    
    newassq<-as.numeric(newassq)
    if(!is.na(newassq[1]))
    {
      if(!is.na(newassq[2]))
      {
        if(newassq[1]==1)
        {
          return(1)
        }else{
          if(newassq[1]==newassq[2])
          {
            return(3)
          }else{
            if(newassq[1]<newassq[2]){
              return(2)
            }
          }
        }
      }else{if(newassq[1]==1)
      {
        return(1)
      }
      }
    }
  }
  pgn1<-0
  spl<-assq
  
  pageNumPattern2<-0
  # Rule 1: The page contains "Page #1" on the top and "Page 1 of x" in the bottom
  for(i in 1:100){
    hhs1<-regexpr("p[a|@][c|g|8]e(.)(.)of[o|s|i|t|!|1|2|3|4|5|6|7|8|9|0|l|a|b|g]", spl, ignore.case=T, perl=T)
    hhs<-regexpr("p[a|@][c|g|8]e(.)of[o|s|!|i|1|2|t|3|4|5|6|7|8|9|0|l|a|b|g]", spl, ignore.case=T, perl=T)
    if(hhs[1]==-1){hhs<-hhs1}
    pageNumPattern2 <- hhs[1]
    if(pageNumPattern2>0){
      #print("blue")
      pgn1<-pgn1+pageNumPattern2
      spl<-strsplit(spl[[1]],"")[[1]]
      break;
    }else{break;}
  }
  #print(pgn1)
  if(pageNumPattern2>0){pageNumPattern2<-pgn1}
  if(pageNumPattern2 > 0){
    
    #print("If statement valid")
    assq<-strsplit(assq[[1]],"")[[1]]
    if(length(assq)>pageNumPattern2+20)
    {
      assq<-assq[pageNumPattern2:(pageNumPattern2+20)]
    }else{assq<-assq[pageNumPattern2:length(assq)]}
    
    assq<-assq[5:length(assq)]
    assq<-paste(assq,collapse='')
    q<-unlist((unlist(strsplit(tolower(assq), "of"))))
    q<-q[1:2]
    #print("i")
    #print(q)
    q.111<-strsplit(q[1],"")[[1]]
    
    
    q.111[which(q.111=="g")]<-8
    q.111[which(q.111=="b")]<-8
    q.111[which(q.111=="a")]<-4
    q.111[which(q.111=="t")]<-1
    q.111[which(q.111=="l")]<-1
    q.111[which(q.111=="i")]<-1
    q.111[which(q.111=="!")]<-1
    
    q[1]<-paste(q.111,collapse="")
    q.112<-strsplit(q[2],"")[[1]]
    if(!is.na(q.112[1])){
      if(q.112[1]=="g"){q.112[1]<-8}
      if(q.112[1]=="b"){q.112[1]<-8}
      if(q.112[1]=="a"){q.112[1]<-4}
      if(q.112[1]=="t"){q.112[1]<-1}
      if(q.112[1]=="l"){q.112[1]<-1}
      if(q.112[1]=="!"){q.112[1]<-1}
      if(q.112[1]=="t"){q.112[1]<-1}
      if(q.112[1]=="i"){q.112[1]<-1}
    }
    
    #q.112[which(q.112=="b")]<-8
    #q.112[which(q.112=="a")]<-4
    #q.112[which(q.111=="t")]<-1
    #q.112[which(q.112=="l")]<-1
    #q.112[which(q.112=="i")]<-1
    #q.112[which(q.111=="!")]<-1
    
    q[2]<-paste(q.112,collapse="")
    qnew<-q
    #print(qnew)
    wwr<-as.numeric(strsplit(gsub("[^0-9]", " ", unlist(q[2]))," ")[[1]])
    if(is.na(wwr[1])){qnew[1]=as.numeric(qnew[1])
    if(is.numeric(qnew[1]))
    {
      if(qnew[1]==1)
      {return(1)}else{return(Pagenumber4(textFileConten))}
      
    }
    }
    wwr<-wwr[which(!is.na(wwr))]
    qnew[2]<-wwr[1]
    qnew=as.numeric(qnew)
    ##print(qnew)
    
    ##print(is.na(qnew[1]))
    if(is.na(qnew[1]))
    {
      return(Pagenumber4(textFileConten))
    }
    if(is.na(qnew[2]))
    {
      return(Pagenumber4(textFileConten))
    }
    
    if(qnew[1]==1)
    { return(1)
    }else{
      if(qnew[1]==qnew[2])
      {
        return(3)
      }else{if(qnew[1]<qnew[2]){return(2)}}
    }
  }else{
    e=0
    f=0
    #print("If Statement invalid")
    
    assq<-strsplit(assq,"")[[1]]
    if(length(assq)>100)
    {#print(">100")
      assq<-assq[(length(assq)-100):length(assq)]
    }
    assq<-paste(assq,collapse='')
    
    rrr<-regexpr("(.)of(.)", assq, ignore.case=T, perl=T)[1]
    if(rrr>0)
    {
      
      assq<-strsplit(assq[[1]],"of")[[1]]
      assq<-rev(assq)
      assq<-rev(assq[1:2])
      assq1<-strsplit(assq[[1]],"")[[1]]
      assq2<-strsplit(assq[[2]],"")[[1]]
      if(length(assq1)>1)
      {
        assq1=assq1[(length(assq1)-1):length(assq1)];
      }
      if(length(assq2)>1)
      {
        assq2=assq2[1:2]
      }
      #print(assq1)
      #print(assq2)
      qnew<-c()
      q<-c()
      if((length(assq1)>=1)&(length(assq2)>=1))
      {
        q.111<-assq1
        
        q[1]<-paste(q.111,collapse="")
        q.112<-assq2
        
        q[2]<-paste(q.112,collapse="")
        #print(q)
        q111<-as.numeric(strsplit(gsub("[^0-9]", " ", unlist(q[1]))," ")[[1]])
        
        q111<-q111[length(q111)]
        q[1]<-q111
        
        q111<-as.numeric(strsplit(gsub("[^0-9]", " ", unlist(q[2]))," ")[[1]])
        q111<-q111[1]
        q[2]<-q111
        q<-as.numeric(q)
        qnew<-q
        #print(qnew)
        
        
        
        
        if((length(which(!is.na(qnew[1])))!=0)&(length(which(!is.na(qnew[2])))!=0))
        {
          #print("blah")
          
          if(is.numeric(qnew[1])&is.numeric(qnew[2]))
          {
            
            if(qnew[1]==1)
            { return(1)
            }else{
              if(qnew[1]==qnew[2])
              {
                return(3)
              }else{return(2)}
            }
          }
        }
        
      }
    }
    assq<-strsplit(textFileConten[[1]],"")[[1]]
    
    assq=assq[which(assq!=" ")]
    if(length(assq)>6){
      assq=assq[(length(assq)-6):length(assq)]
    }
    if((Pagenumber4(textFileConten))==0)
    {
      
      assq<-paste(assq,collapse="")
      #print(assq)
      assq<-as.numeric(strsplit(gsub("[^0-9]", " ", assq)," ")[[1]])
      #print(assq)
      assq<-assq[length(assq)]
      
      if(is.na(assq))
      {
        return(Pagenumber4(textFileConten))
      }else{
        
        
        assq=as.numeric(assq)
        if(assq%/%200==0)
        {
          
          if(assq==1)
          {
             return(Pagenumber4(textFileConten))
            
          }else{return(Pagenumber4(textFileConten))}
        }
      }
    }
    return((Pagenumber4(textFileConten)))
    
  }
  
  return (Pagenumber4(textFileConten))
}


Pagenumber4<-function(textFileConten) {
  #print("CALLING THE Pagenumber FUNCTION")
  textFileConten<-tolower(textFileConten)
  assq<-strsplit(textFileConten[[1]]," ")[[1]]
  assq<-paste(assq, collapse="")
  if(assq=="")
  {
    return(0)
  }
  pageNumPattern2 <- regexpr("p[a|@][c|g|8]e#", assq, ignore.case=T, perl=T)[1]
  
  #print(assq)
  #print(pageNumPattern2)
  if(pageNumPattern2!=-1)
  {
    assq<-strsplit(assq,"")[[1]]
    if(length(assq)>=(pageNumPattern2+5)){
      assq<-assq[(pageNumPattern2+5):(pageNumPattern2+6)]}else{
        if(length(assq)>=(pageNumPattern2+4)){
          assq<-assq[pageNumPattern2+5]}else{assq=c()}
      }
    q.111<-assq[1]
    q.112<-assq[2]
    q.111[which(q.111=="g")]<-8
    q.111[which(q.111=="b")]<-8
    q.111[which(q.111=="a")]<-4
    q.111[which(q.111=="l")]<-1
    q.111[which(q.111=="i")]<-1
    q.111[which(q.111=="t")]<-1
    q.111[which(q.111=="o")]<-0
    q.111[which(q.111=="!")]<-1
    
    assq[1]<-q.111
    assq[2]<-q.112
    
    #print(assq[2])
    
    assq33<-as.numeric(assq[2])
    if(is.na(assq33))
    {#print("ok")
      #print(assq[1])
      assq22<-as.numeric(assq[1])
      #print(assq22)
      if(!is.na(assq22))
      {
        if(assq22==1)
        {return(1)}
      }
    }else{
      return (0)
    }
  }
  return(0)
}
