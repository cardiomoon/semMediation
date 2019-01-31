#' draw conceptual diagram of process macro model
#' @param no process macro model number
#' @param labels A character list
#' @param covar A optional list of covariates
#' @export
#' @examples
#' covar=list(name=c("C1","C2","C3"),label=c("ese","sex","tenure"),site=list("M",c("M","Y"),c("Y")))
#' pmacroModel(4,covar=covar)
#' pmacroModel(1,covar=covar)
#' pmacroModel(1)
pmacroModel=function(no=1,labels=list(),covar=list()){
    # no=76
    # labels=list("X"="age","M"="educ","Y"="interest","W"="policy","Z"="male")
    #labels=list()
    i=which(pmacro$no==no)
    i
    if(length(i)<1) {
        cat("Currently, Model number ",no," is not supported.")
    } else{
        if(no %in% c(4.2,6.0,6.3,6.4)){
            statisticalDiagram(no,arrowlabel = FALSE,covar=covar)
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
        pos
        if(length(pos)==0) pos=3
        labels

        label=c()
        for(j in 1:length(name)){
            if(!is.null(labels[[name[j]]])) {
                label=c(label,labels[[name[j]]])
            } else {
                label=c(label,name[j])
            }
        }
        moderator=list(name=name,label=label,site=x,latent=latent,pos=pos)
        moderator

    }
    i
    M=pmacro$M[i]
    if(M=="") {
        M=c()
    } else{
        if(!is.null(labels$M)){
            M=labels$M
        }
    }
    M
    X=pmacro$X[i]
    if(!is.null(labels$X)){
        X=labels$X
    }
    Y=pmacro$Y[i]
    if(!is.null(labels$Y)){
        Y=labels$Y
    }

    if(no==74) {
        conceptDiagram2(X=X,M=M,Y=Y,xb=TRUE,covar=covar)
    } else {
        conceptDiagram2(X=X,M=M,Y=Y,moderator = moderator,covar=covar)
    }

        }
    }

}

