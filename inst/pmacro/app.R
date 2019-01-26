library(shiny)
library(semMediation)
library(stringr)
library(DT)
library(editData)
library(shinyWidgets)
library(lavaan)
library(flextable)
library(semTools)

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
    h2("Assign Variables"),
    uiOutput("Vars"),
    actionButton("Analysis","Analysis"),
    uiOutput("result")
)

server=function(input,output,session){


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
                column(2,selectInput("chooser",NULL,
                                     choices=colnames(data()),
                                     selectize=FALSE,
                                     size=min(10,length(data())))),
                column(3,uiOutput("assignVars")),
                column(2,actionButton("makeEq","make Equation",width="150px"),
                       hr(),
                       actionButton("resetEq","reset Equation",width="150px")
                       ),
                column(4,
                       textAreaInput("equation",NULL,rows=10,placeholder="You can edit equation.")
                       )
            )
        )
    })

    choices1=function(){
        count=length(mylist())
        selected=c()
        for(i in 1:count){
            selected=c(selected,input[[mylist()[i]]])
        }
        result=setdiff(colnames(data()),selected)
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


    observeEvent(input$resetEq,{
        updateTextAreaInput(session,"equation",value="")
    })


    observeEvent(input$makeEq,{
        i=as.numeric(input$modelno)
        select=pmacro$no==i
        if((i>=6) & (i<7)){
            temp=unlist(strsplit(pmacro$M[select],":"))
            mediators=c()
            for(i in 1:length(temp)){
                mediators=c(mediators,input[[temp[i]]])
            }
            model=makeEquation(X=input$X,M=mediators,Y=input$Y)
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
            model=modmedEquation(X=input$X,Y=input$Y,moderator=moderator)
        } else {
            moderator
           # model=modmedEquation(X="X",M="M",Y="Y",moderator=moderator)
            #model
            model=modmedEquation(X=input$X,M=input$Mi,Y=input$Y,moderator=moderator)
        }
        }
        #cat(model)

        updateTextAreaInput(session,"equation",value=model)
    })

    output$result=renderUI({

        req(input$Analysis)

        isolate({




       fit=sem(model=input$equation,data=data())


        output$text=renderPrint({


                if(input$equation!=""){
                cat("model='",input$equation,"'\n")
                cat("fit=sem(model=model,data=",input$mydata,")\n")
                cat("summary(fit)\n\n")

                summary(fit)
                cat("parameterEstimates(fit)\n\n")
                parameterEstimates(fit)
                cat("discriminantValidityTable(fit)\n\n")
                discriminantValidityTable(fit)
                cat("reliablityTable(fit)\n\n")
                reliabilityTable(fit)
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

            names<-mylist()
            labels=list()
            for(i in 1:length(names)){
                labels[[names[i]]]=input[[names[i]]]
            }
            pmacroModel(no=as.numeric(input$modelno),labels=labels)

        })
        output$statDiagram=renderPlot({

            names<-mylist()
            labels=list()
            for(i in 1:length(names)){
                labels[[names[i]]]=input[[names[i]]]
            }
            table1=estimatesTable(fit,digits=as.numeric(input$digits))
            statisticalDiagram(no=as.numeric(input$modelno),labels=labels,
                               whatLabel = input$whatLabel,estimateTable=table1,
                               radx=as.numeric(input$radx))

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
            uiOutput("modelFitTable")
            # h2("Reliability Table"),
            # uiOutput("reliabilityTable"),
            # h2("Discriminant Validity Table"),
            # uiOutput("discriminantValidityTable"),



        )
        })
    })


}

shinyApp(ui,server)

