library(shiny)
library(semMediation)
library(stringr)
library(DT)
library(webrSub)
library(editData)
library(shinyWidgets)
library(lavaan)
library(flextable)
library(DiagrammeR)

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
               selectInput("dataname","Data",choices=dataNames,selectize=FALSE,size=7)
        ),
        column(9,
               DTOutput('table')

        )
    ),
    h2("Select Process Macro Model Number"),
    fluidRow(
        column(3,
               selectInput("modelno","Model No",choices=pmacro$no,selectize=FALSE,size=18)
               ),
        column(9,
               h4("Concept Diagram"),
               plotOutput("modelPlot")
        )
    ),
    h2("Assign Variables"),
    uiOutput("Vars"),
    actionButton("Analysis","Analysis"),
    uiOutput("result")
)

server=function(input,output,session){
    output$modelPlot=renderPlot(
        pmacroModel(as.numeric(input$modelno))
    )

    data=reactive({
        data=read.csv(paste0("data/",input$dataname,".csv"),stringsAsFactors = FALSE)
        data
    })

    mylist=reactive({
        i=as.numeric(input$modelno)
        select=pmacro$no==i
        mylist=c(pmacro$X[select],pmacro$M[select],pmacro$Y[select])
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
                inputlist[[4*i-2]]=label3(mylist()[i],width=15)
                inputlist[[4*i-1]]=pickerInput3(mylist()[i],NULL,choices=colnames(data()),selected="",
                                              multiple=TRUE,width="90px",options=list(title="Select...",style="btn-warning"))

                inputlist[[4*i]]=p("")
            }

            do.call(tagList,inputlist)

        })

         tagList(


            fluidRow(
                column(4,chooserUI("chooser")),
                column(3,uiOutput("assignVars")),
                column(2,actionButton("makeEq","make Equation",width="150px"),
                       br(),
                       actionButton("resetEq","reset Equation",width="150px")
                       ),
                column(3,
                       textAreaInput("equation",NULL,rows=10,placeholder="You can edit equation.")
                       )
            )
        )
    })

    choices1=reactive({
        count=length(mylist())
        selected=c()
        for(i in 1:count){
            selected=c(selected,input[[mylist()[i]]])
        }
        result=setdiff(colnames(data()),selected)
        result
    })

    result=callModule(chooser,"chooser",leftChoices=choices1,width=reactive(105))

    observeEvent(input$addVar1,{
        updatePickerInput(session,mylist()[1],selected=result()$right)
    })

    observeEvent(input$addVar2,{
        updatePickerInput(session,mylist()[2],selected=result()$right)
    })

    observeEvent(input$addVar3,{
        updatePickerInput(session,mylist()[3],selected=result()$right)
    })

    observeEvent(input$addVar4,{
        updatePickerInput(session,mylist()[4],selected=result()$right)
    })
    observeEvent(input$addVar5,{
        updatePickerInput(session,mylist()[5],selected=result()$right)
    })

    observeEvent(input$addVar6,{
        updatePickerInput(session,mylist()[6],selected=result()$right)
    })

    observeEvent(input$resetEq,{
        updateTextAreaInput(session,"equation",value="")
    })


    observeEvent(input$makeEq,{
        i=as.numeric(input$modelno)
        select=pmacro$no==i

        if(pmacro$modName[select]!=""){
            name=unlist(strsplit(pmacro$modName[select],":"))
            modname=c()
            for(i in 1:length(name)){
                modname=c(modname,input[[name[i]]])
            }

            sites=strsplit(pmacro$modSite[select],":")

            moderator=list(name=modname,site=sites)

       }
        if(pmacro$M[select]=="") model=modmedEquation(X=input$X,Y=input$Y,moderator=moderator)
        else model=modmedEquation(X=input$X,M=input$M,Y=input$Y,moderator=moderator)

        updateTextAreaInput(session,"equation",value=model)
    })

    output$result=renderUI({

        if(input$equation!=""){

            fit=sem(model=input$equation,data=data())
        }

        output$text=renderPrint({

          input$Analysis

            isolate({
                if(input$equation!=""){
                cat("model=",input$equation,"\n")
                cat("fit=sem(model=model,data=",input$dataname,")\n")
                cat("summary(fit)\n\n")

                summary(fit)
                cat("parameterEstimates(fit)\n\n")
                parameterEstimates(fit)}
            })
        })

        output$estimateTable=renderUI({

            input$Analysis

            isolate({
            if(input$equation!=""){

            estimatesTable2(fit) %>%
                htmltools_value()
            }
            })
        })

        output$diagram=renderGrViz({

            input$Analysis

            isolate({
                if(input$equation!=""){
                    semDiagram(fit)
                }
            })
        })

        tagList(
            verbatimTextOutput("text"),
            h3("Estimates Table"),
            uiOutput("estimateTable"),
            h3("Statistical Diagram"),
            grVizOutput("diagram")
        )
    })


}

shinyApp(ui,server)
