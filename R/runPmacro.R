#' Run process macro shiny app
#' @importFrom shiny runApp
runPmacro=function(){
    shiny::runApp(system.file('pmacro',package='semMediation'))
}
