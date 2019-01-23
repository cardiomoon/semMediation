#' draw conceptual diagram of process macro model
#' @param no process macro model number
#' @export
pmacroModel=function(no=1){
    i=which(pmacro$no==no)
    if(length(i)<1) {
        cat("Currently, Model number ",no," is not supported.")
    } else{
        if((no>=6)&(no<7)){
            result=statisticalDiagram(no,arrowlabel = FALSE)
        } else{
    name=list()
    sites=list()
    moderator=list()
    if(pmacro$modName[i]!=""){
        name=unlist(strsplit(pmacro$modName[i],":"))
        name
        sites=unlist(strsplit(pmacro$modSite[i],":"))
        x=strsplit(sites,",")
        latent=rep(FALSE,length(name))
        pos=as.numeric(unlist(strsplit(pmacro$pos[i],",")))
        if(length(pos)==0) pos=3
        moderator=list(name=name,label=name,site=x,latent=latent,pos=pos)
        moderator

    }
    M=pmacro$M[i]
    if(M=="") M=c()
    if(no==74) result=conceptDiagram2(xb=TRUE)
    else result=conceptDiagram2(X=pmacro$X[i],M=M,Y=pmacro$Y[i],moderator = moderator)

        }
    }
    result
}
