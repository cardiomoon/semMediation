library(shiny)
library(semMediation)
library(stringr)
library(DT)
library(editData)
library(shinyWidgets)
library(lavaan)
library(flextable)
library(semTools)
library(jtools)
library(ggplot2)

dataFiles=list.files(path="data","*.csv")
dataNames=str_extract(dataFiles,"[^.]*")
# for(i in seq_along(dataNames)){
#     data=read.csv(paste0("data/",dataFiles[i]),stringsAsFactors = FALSE)
#     assign(dataNames[i],data)
# }

actionBttn3=function(...){
   div(style="display:inline-block;",actionBttn(...))
}

pickerInput3=function(...){
    div(style="display:inline-block;",pickerInput(...))
}

ui=fluidPage(

    h2("Select Data"),
    fluidRow(
        column(3,
               fileInput("file","Upload File or"),
               radioButtons("dataname","Select example",choices=dataNames),
               textInput("mydata","Data Name")
        ),
        column(9,
               DTOutput('table')

        )
    ),
    h2("Select Process Macro Model Number"),
    fluidRow(
        column(2,
               selectInput("modelno","Model No",choices=pmacro$no,selectize=FALSE,size=28)
               ),
        column(10,

               radioGroupButtons(
                   inputId = "plotChoice",
                   label = "Select Plot",
                   choices = c("Conceptual Diagram"=1, "Statistical Diagram"=2),
                   status = "primary"
               ),

               plotOutput("modelPlot",height="500px",width="700px")
        )
    ),
    fluidRow(
        column(6,
            h2("Assign Variables"),
            uiOutput("Vars")),
         column(6,
            h2("Add Covariates"),
            actionButton('insertBtn', 'Add Covariate',icon=icon("plus")),
            actionButton('removeBtn', 'Remove Covariate',icon=icon("trash-alt")),
            actionButton('resetBtn', 'Reset All',icon=icon("ban")),
            tags$div(id = 'placeholder'))
    ),
    fluidRow(
        h2("Make Equation"),
        column(2,actionButton("makeEq","make Equation",width="150px"),
               hr(),
               actionButton("resetEq","reset Equation",width="150px")
        ),
        column(4,
               textAreaInput("equation",NULL,rows=10,placeholder="You can edit equation.")
        )
    ),
    h2("Analysis"),
    actionButton("Analysis","Analysis"),
    uiOutput("result")
)

server=function(input,output,session){

    RV=reactiveValues(number=0)


    output$modelPlot=renderPlot(
        if(input$plotChoice==1) {
            pmacroModel(as.numeric(input$modelno))
        } else{
                statisticalDiagram(as.numeric(input$modelno))
            }

    )

    observeEvent(input$file,{

        updateTextInput(session,"mydata",value="uploaded")

    })

    observeEvent(input$dataname,{
        updateTextInput(session,"mydata",value=input$dataname)
    })

   data=reactive({
        if(input$mydata=="uploaded") {
            data<-myimport(input$file$datapath)
        } else if(input$mydata == "mtcars"){
            data<-mtcars
        } else{
             data<-read.csv(paste0("data/",input$dataname,".csv"),stringsAsFactors = FALSE)
            }
        data
    })

    mylist=reactive({
        i=as.numeric(input$modelno)
        select=pmacro$no==i
        mylist=pmacro$X[select]
        if(pmacro$M[select]!="") {
            mediators=unlist(strsplit(pmacro$M[select],":"))
            mylist=c(mylist,mediators)
        }
        mylist=c(mylist,pmacro$Y[select])
        mylist=setdiff(mylist,"")
        if(pmacro$modName[select]!="") {
            moderators=unlist(strsplit(pmacro$modName[select],":"))
            mylist=c(mylist,moderators)
        }

        RV$varsNo=length(mylist)

        mylist
    })

    output$table=renderDT(
        data()
    )

    output$Vars=renderUI({

        output$assignVars=renderUI({
            inputlist=list()
            for(i in 1:length(mylist())){
                inputlist[[4*i-3]]=actionBttn3(paste0("addVar",i),NULL,style="simple",color="success",icon=icon("arrow-right"))
                inputlist[[4*i-2]]=label3(mylist()[i],width=20)
                inputlist[[4*i-1]]=pickerInput3(mylist()[i],NULL,
                                                choices=c("",colnames(data())),selected="",
                                                width="150px",options=list(title="Select..."))

                inputlist[[4*i]]=p("")
            }

            do.call(tagList,inputlist)

        })

         tagList(


            fluidRow(
                column(4,selectInput("chooser",NULL,
                                     choices=colnames(data()),
                                     selectize=FALSE,
                                     size=min(10,length(data())))),
                column(8,uiOutput("assignVars"))
            )
        )
    })

    choices1=function(){
        count=length(mylist())
        selected=c()
        for(i in 1:count){
            selected=c(selected,input[[mylist()[i]]])
        }
        result=setdiff(setdiff(colnames(data()),selected),getCovNames())
        result
    }



    lapply(1:7,FUN=function(i){
        observeEvent(input[[paste0("addVar",i)]],{
            updateSelectInput(session,mylist()[i],selected=input$chooser)
        })
    })

    lapply(1:7,FUN=function(i){
        observeEvent(input[[mylist()[i]]],{
            updateSelectInput(session,"chooser",choices=choices1())
        })
    })

    lapply(1:10,FUN=function(i){
        observeEvent(input[[paste0("cov",i)]],{
            updateSelectInput(session,"chooser",choices=choices1())
        })
    })

    observeEvent(c(input$insertBtn,input$resetBtn,input$removeBtn),{
        updateSelectInput(session,"chooser",choices=choices1())
    })



    observeEvent(input$resetEq,{
        updateTextAreaInput(session,"equation",value="")
    })


    observeEvent(input$makeEq,{
        i=as.numeric(input$modelno)
        select=pmacro$no==i
        if(i %in% c(4.2,6,6.3,6.4)){
            temp=unlist(strsplit(pmacro$M[select],":"))
            mediators=c()
            for(j in 1:length(temp)){
                mediators=c(mediators,input[[temp[j]]])
            }
            add=ifelse(i==4.2,FALSE,TRUE)
            cat("i=",i,",add=",add,"\n")
            model=makeEquation(X=input$X,M=mediators,Y=input$Y,add2ndMediation = add,
                               covar=getCovariates2())
        } else{
        select=pmacro$no==i
        #select=3
        if(pmacro$modName[select]!=""){
            name=unlist(strsplit(pmacro$modName[select],":"))
            name
            modname=c()
            for(i in 1:length(name)){
                modname=c(modname,input[[name[i]]])
            }
            modname
            temp=unlist(strsplit(pmacro$modSite[select],":"))
            temp
            sites=list()
            for(i in 1:length(temp)){
              sites[[i]]=unlist(strsplit(temp[i],","))
            }
            sites
           # moderator=list(name=name,site=sites)
            moderator=list(name=modname,site=sites)
            #str(moderator)

        } else{
            moderator=NULL
        }

        pmacro$M[select]
        if(pmacro$M[select]==""){
            model=modmedEquation(X=input$X,Y=input$Y,moderator=moderator,
                                 covar=getCovariates())
        } else {
            moderator
           # model=modmedEquation(X="X",M="M",Y="Y",moderator=moderator)
            #model
            model=modmedEquation(X=input$X,M=input$Mi,Y=input$Y,moderator=moderator,
                                 covar=getCovariates())
        }
        }
        #cat(model)

        updateTextAreaInput(session,"equation",value=model)
    })

    output$result=renderUI({

        req(input$Analysis)


       fit=sem(model=isolate(input$equation),data=data())

       if(input$modelno==1){
           probs=c(0.16,0.5,0.84)
           if(length(unique(data()[[input$W]]))==2){
               temp=sort(unique(data()[[input$W]]))
           } else{
               temp=quantile(data()[[input$W]],probs)
           }
           modValues=paste(temp,collapse=",")
       }


        output$text=renderPrint({


                if(input$equation!=""){
                cat("model='",input$equation,"'\n")
                cat("fit=sem(model=model,data=",input$mydata,")\n")
                cat("summary(fit)\n\n")

                summary(fit)
                cat("parameterEstimates(fit)\n\n")
                print(parameterEstimates(fit))
                cat("\n\n")
                if(!is.null(discriminantValidityTable(fit))){
                    cat("\n\ndiscriminantValidityTable(fit)\n\n")
                    print(discriminantValidityTable(fit))
                }
                if(!is.null(reliabilityTable(fit))){
                cat("reliablityTable(fit)\n\n")
                print(reliabilityTable(fit))
                }
                }

        })

        output$estimateTable=renderUI({



            if(input$equation!=""){

            estimatesTable2(fit,digits=as.numeric(input$digits),vanilla=input$vanilla) %>%
                htmltools_value()
            }

        })

        output$corTable=renderUI({
                if(input$equation!=""){

                    corTable2(fit,vanilla=input$vanilla) %>%
                        htmltools_value()
                }
        })

        output$corPlot=renderPlot({
            if(input$equation!=""){

                corPlot(fit)
            }
        })

        output$reliabilityTable=renderUI({
            if(input$equation!=""){
                reliabilityTable2(fit,vanilla=input$vanilla)  %>%
                    htmltools_value()
            }
        })

        output$discriminantValidityTable=renderUI({
            if(input$equation!=""){

                discriminantValidityTable2(fit,vanilla=input$vanilla) %>%
                    htmltools_value()
            }
        })

        output$modelFitTable=renderUI({
            if(input$equation!=""){

                modelFitTable2(fit,vanilla=input$vanilla) %>%
                    htmltools_value()
            }
        })


        # output$diagram=renderGrViz({
        #
        #
        #         if(input$equation!=""){
        #             semDiagram(fit)
        #         }
        #
        # })

        output$concept=renderPlot({
            if(input$equation!=""){


            names<-mylist()
            labels=list()
            for(i in 1:length(names)){
                labels[[names[i]]]=input[[names[i]]]
            }
            pmacroModel(no=as.numeric(input$modelno),labels=labels,covar=getCovariates())
            }
        })
        output$statDiagram=renderPlot({

            input$makeEq

            names<-mylist()
            labels=list()


            for(i in 1:length(names)){
                labels[[names[i]]]=input[[names[i]]]
            }
            table1=estimatesTable(fit,digits=as.numeric(input$digits))
            statisticalDiagram(no=as.numeric(input$modelno),labels=labels,
                               whatLabel = input$whatLabel,estimateTable=table1,
                               radx=as.numeric(input$radx),
                               covar=getCovariates())


        })


        tagList(
            checkboxInput("vanilla","vanilla table",value=FALSE),
            verbatimTextOutput("text"),
            h2("Conceptual Diagram"),
            plotOutput("concept",height="500px",width="700px"),
            h2("Estimates Table"),
            uiOutput("estimateTable"),
            h2("Statistical Diagram"),
            selectInput3("whatLabel","whatLabel",choices=c("est","std","name")),
            selectInput3("digits","digits",choices=c(2,3,4),selected=3),
            selectInput3("radx","box width",
                         choices=c("0.04","0.06","0.08","0.10","0.12","0.14"),
                         selected="0.10"),
            plotOutput("statDiagram",height="500px",width="700px"),
            h2("Correlation Table"),
            uiOutput("corTable"),
            h2("Correlation Plot"),
            plotOutput("corPlot"),
            h2("Model Fit Table"),
            uiOutput("modelFitTable"),
            if(input$modelno==1) h2("Moderation Effect"),

            if(input$modelno==1) textInput3("probs","probs",value="",placeholder="0.16,0.5,0.84",width=150),
            if(input$modelno==1) textInput3("modxvalues","moderator values",value="",placeholder=modValues,width=150),
            if(input$modelno==1) checkboxInput3("showeffect","show effect",value=TRUE,width=120),
            if(input$modelno==1) checkboxInput3("plotpoints","show points",value=FALSE,width=120),
            if(input$modelno==1) checkboxInput3("interval","show interval",value=FALSE,width=120),
            if(input$modelno==1) pickerInput3("inttype","type",choices=c("confidence","prediction"),width=120),
            if(input$modelno==1) numericInput3("intwidth","width",value=0.95,min=0.1,max=1,step=0.01),
            if(input$modelno==1) checkboxInput3("switchMod2","switch moderator",value=FALSE,width=200),
            if(input$modelno==1) checkboxInput3("linearity","linearity check",value=FALSE,width=200),
            if(input$modelno==1) plotOutput("moderationPlot"),
            if(input$modelno==1) verbatimTextOutput("JNText"),
            if(input$modelno==1) numericInput3("alpha","alpha",value=0.05,min=0.01,max=1,step=0.01),
            if(input$modelno==1) checkboxInput3("switchMod","switch moderator",value=FALSE,width=200),
            if(input$modelno==1) plotOutput("JNPlot"),
            verbatimTextOutput("regEquation")
            # h2("Reliability Table"),
            # uiOutput("reliabilityTable"),
            # h2("Discriminant Validity Table"),
            # uiOutput("discriminantValidityTable")



        )

    })

    observeEvent(input$switchMod2,{
        probs=c(0.16,0.5,0.84)
        if(input$switchMod2) {
            if(length(unique(data()[[input$X]]))==2){
                temp=sort(unique(data()[[input$X]]))
            } else{
                temp=quantile(data()[[input$X]],probs)
            }
        } else{
            if(length(unique(data()[[input$W]]))==2){
                temp=sort(unique(data()[[input$W]]))
            } else{
                temp=quantile(data()[[input$W]],probs)
            }

        }
        modValues=paste(temp,collapse=",")
        updateTextInput(session,"modxvalues",value="",placeholder = modValues)
    })

    observeEvent(input$linearity,{
        if(input$linearity) {
            updateCheckboxInput(session,"showeffect",value=FALSE)
        }
    })

    observeEvent(input$showeffect,{
        if(input$showeffect) {
            updateCheckboxInput(session,"linearity",value=FALSE)
        }
    })

   observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        id <- length(RV$inserted)/3+1
        if(input$modelno %in% c(1,2,6.3,6.4)) {
            covchoices="Y"
        } else if(input$modelno %in% c(4.2,6)) {
            covchoices=c("M1,M2,Y","M1,Y","M2,Y","Y")
        } else if(input$modelno == "6.3"){
            covchoices=c("M1,M2,M3,Y","Y")
        } else if(input$modelno == "6.4"){
            covchoices=c("M1,M2,M3,M4,Y","Y")
        } else{
            covchoices=c("Mi,Y","Mi","Y")
        }

        insertUI(
            selector = '#placeholder',
            ## wrap element in a div with id for ease of removal
            ui = tagList(
                tags$div(h4(paste0("Covariate ",id,"    Site")),id=paste0(id)),
                selectInput3(paste0("cov",id),NULL,choices=choices1()),
                selectInput3(paste0("site",id),NULL,choices=covchoices)
            )
        )
        RV$inserted <-c(paste0("div:has(> #cov",id,")"),paste0('#',id),paste0("div:has(> #site", id,")"),RV$inserted)
        RV$number=as.numeric(RV$number)+1

    })

    observeEvent(input$removeBtn, {


        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[1]
        )
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[2]
        )
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[3]
        )

        RV$inserted <- RV$inserted[-(1:3)]
        RV$number=ifelse(RV$number>0,as.numeric(RV$number)-1,0)
    })

    observeEvent(input$resetBtn, {
        while(RV$number>0){
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[1]
        )
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[2]
        )
        removeUI(
            ## pass in appropriate div id
            selector = RV$inserted[3]
        )

        RV$inserted <- RV$inserted[-(1:3)]
        RV$number=ifelse(RV$number>0,as.numeric(RV$number)-1,0)
        }


    })

    getCovNames=reactive({
        count=RV$number

        temp=c()
        if(count) for(i in 1:count){
            temp=c(temp,input[[paste0("cov",i)]])
        }
        temp
    })

    getCovSites=reactive({
        count=RV$number

        result=list()
        if(count>0) for(i in 1:count){
            temp=input[[paste0("site",i)]]
            result[[i]]<-unlist(str_split(temp,","))
        }
        result
    })

    getCovSites2=reactive({
        count=RV$number

        result=list()
        if(count>0) for(i in 1:count){
            temp=input[[paste0("site",i)]]
            temp1<-unlist(str_split(temp,","))
            temp2=c()
            for(j in 1:length(temp1)){
                temp2=c(temp2,input[[temp1[j]]])
            }
            result[[i]]<-temp2

        }
        result
    })

    getCovariates=reactive({
        result<-list()
        if(RV$number>0) {
            result$name=getCovNames()
            result$site=getCovSites()
        }
        result
    })

    getCovariates2=reactive({
        result<-list()
        if(RV$number>0) {
            result$name=getCovNames()
            result$site=getCovSites2()
        }
        result
    })

    getModerator=reactive({
    i=as.numeric(input$modelno)
    select=pmacro$no==i
    if(i %in% c(4.2,6,6.3,6.4)){
        # temp=unlist(strsplit(pmacro$M[select],":"))
        # mediators=c()
        # for(j in 1:length(temp)){
        #     mediators=c(mediators,input[[temp[j]]])
        # }
        # add=ifelse(i==4.2,FALSE,TRUE)
        # cat("i=",i,",add=",add,"\n")
        # model=makeEquation(X=input$X,M=mediators,Y=input$Y,add2ndMediation = add,
        #                    covar=getCovariates2())
        moderator=NULL
    } else{
        select=pmacro$no==i
        #select=3
        if(pmacro$modName[select]!=""){
            name=unlist(strsplit(pmacro$modName[select],":"))
            name
            modname=c()
            for(i in 1:length(name)){
                modname=c(modname,input[[name[i]]])
            }
            modname
            temp=unlist(strsplit(pmacro$modSite[select],":"))
            temp
            sites=list()
            for(i in 1:length(temp)){
                sites[[i]]=unlist(strsplit(temp[i],","))
            }
            sites
            # moderator=list(name=name,site=sites)
            moderator=list(name=modname,site=sites)
            #str(moderator)

        } else{
            moderator=NULL
        }
        moderator
    }
    })

    getRegEq=reactive({
         regEquation(X=input$X,Y=input$Y,moderator=getModerator(),covar=getCovariates2())
    })

    output$moderationPlot=renderPlot({
        data1<-data()
        temp=paste0("lm(",getRegEq(),",data=data1)")
        print(temp)
        fit=eval(parse(text=temp))
        summary(fit)
        probs<-modx.values<-NULL
        if(input$probs!="") probs=as.numeric(unlist(strsplit(input$probs,",")))
        if(input$modxvalues!="") modx.values=as.numeric(unlist(strsplit(input$modxvalues,",")))
        condEffect(fit=fit,pred=input$X,modx=input$W,show.Effect=input$showeffect,
                   switchVars=input$switchMod2,probs=probs,modx.values=modx.values,
                   plot.points=input$plotpoints,interval=input$interval,int.type=input$inttype,int.width=input$intwidth,
                   linearity.check=input$linearity)
    })

    output$JNText=renderPrint({
        data1<-data()
        fit=eval(parse(text=paste0("lm(",getRegEq(),",data=data1)")))
        pred=ifelse(input$switchMod,input$W,input$X)
        modx=ifelse(input$switchMod,input$X,input$W)
        johnson_neyman(fit,pred=pred,modx=modx,alpha=input$alpha,plot=FALSE,digits=3)

    })
    output$JNPlot=renderPlot({
        data1<-data()
        fit=eval(parse(text=paste0("lm(",getRegEq(),",data=data1)")))
        pred=ifelse(input$switchMod,input$W,input$X)
        modx=ifelse(input$switchMod,input$X,input$W)

        XM=paste(input$X,input$W,sep=":")
        label=paste0("italic(theta) [italic(X) %->% italic(Y)] == ",
                     sprintf("%.03f",fit$coef[pred]),"+",sprintf("%.03f",fit$coef[XM]),"*italic(W)")

        result=johnson_neyman(fit,pred=pred,modx=modx,plot=FALSE,alpha=input$alpha)
        pos=relpos(result$plot)
        result$plot+
            annotate("text",x=result$bounds,y=-Inf,label=round(result$bounds,3),
                     vjust=-0.5,hjust=-0.1)+
            annotate("text",x=pos[1],y=pos[2],label=label,parse=TRUE)


    })

    output$regEquation=renderPrint({

        eq=getRegEq()
        # print(eq)
        fit=eval(parse(text=paste0("lm(",eq,",data=data())")))

        summary(fit)
    })

}

shinyApp(ui,server)

