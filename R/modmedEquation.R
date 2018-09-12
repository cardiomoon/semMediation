X="time.c";M="pubs";Y="jobs"
moderator=list(name=c("alex.c","moon.c"),site=list(c("a","b","c"),c("a","c")))
moderator


X="X";M="M";Y="Y"
moderator=list(name=c("z1","z2"),site=list(c("a","b","c"),c("a","c")))
moderator


#'make interaction equation
#'@param x character vector
#'@param prefix prefix
interactStr=function(x,prefix="a"){
    res=c()
    count=1
    for(i in seq_along(x)){
        temp=paste0(prefix,count,"*",x[i])
        res=c(res,temp)
        count=count+1
        if(i>1){
            temp=paste0(prefix,count,"*",x[i],":",x[1])
            res=c(res,temp)
            count=count+1
        }
    }
    res
}


modmedEquation=function(X="",M="",Y="",moderator=list(),labels=NULL){
      (XM=moderator$name[str_detect(moderator$site,"a")])
      (MY=moderator$name[str_detect(moderator$site,"b")])
      (XY=moderator$name[str_detect(moderator$site,"c")])

      # Regression of Moderator
      XM=c(X,XM)
      XMstr=interactStr(XM,prefix="a")
      equation=paste(M,"~",stringr::str_flatten(XMstr,"+"),"\n")
      MY=c(M,MY)
      XY=c(X,XY)
      MYstr=interactStr(MY,prefix="b")
      XYstr=interactStr(XY,prefix="c")
      temp=paste(Y,"~",stringr::str_flatten(XYstr,"+"),"+",
                 stringr::str_flatten(MYstr,"+"),"\n")
      equation=paste0(equation,temp)
      cat(equation)

      XMstr
      MYstr

      XYstr1=stringr::str_replace_all(XYstr,":","*")
      XYstr1
      MEqui=stringr::str_flatten(XMstr,"+")
      MEqui
      MYstr1=str_replace(MYstr,M,paste0("(",MEqui,")"))
      MYstr1=stringr::str_replace_all(MYstr1,":","*")
      MYstr1

      for(i in seq_along(moderator$name)){
          name=moderator$name[i]
          temp=paste0(name," ~ ",name,".mean*1\n")
          temp=paste0(temp,name," ~~ ",name,".var*",name,"\n")
          equation=paste0(equation,temp)
      }

}

x=MYstr1[1]
x
require(stringr)
removeBracket=function(x){
   x=str_replace(x,fixed(")"),"")
   x=unlist(str_split(x,fixed("*(")))
   x=unlist(str_split(x,fixed("+")))
   x
   res=c()
   for(i in seq_along(x)){
       if(i==1){
           temp=NULL
       } else {
           temp=paste0(x[1],"*",x[i])
       }
       res=c(res,temp)
   }
   res
}

removeBracket(x)

cat(makeEquation(X="X",M="M",Y="Y"))
