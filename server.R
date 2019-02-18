# Server funtion for the State of the Salmon Synoptic Status Evaluation Tool
# Developed by B. MacDonald
# Feb 13, 2019

list.of.packages <- c("shiny", 
                      "shinydashboard",
                      "tibble",
                      "devtools",
                      "ggplot2",
                      "dplyr",
                      "GGally",
                      "DT",
                      "htmltools",
                      "shinyWidgets",
                      "plotly",
                      "forcats",
                      "rsconnect",
                      "brew",
                      "ini",
                      "xfun",
                      "readxl",
                      "markdown")                      

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#if(!"parcoords" %in% installed.packages()[,"Package"]) devtools::install_github("timelyportfolio/parcoords")
devtools::install_github("brigitte-dorner/parcoords")
#if(!"ezR" %in% installed.packages()[,"Package"]) devtools::install_github("jerryzhujian9/ezR")
library(parcoords)
library(shinydashboard)
library(forcats)
#library("ezR")

# ==========Define server components ================


# Define server logic 
function(input, output,session){

  #--------- ---------- Helper functions ------------------
  # assemble the id of a shiny input widget or a variable name from a prefix and a postfix, e.g. widget.1 
  sId <- function(pre, post) {paste(pre, post, sep=".")}
  # sum columns using select_if function of dplyr to remove empty columns
  sumfun <- function(x){sum(!is.na(x)) > 0}
  #------------------- Data processing ------------------
  
  # data.start <- readxl::read_excel("data/FR SK metrics.xls")
  # data.start$Lower.Ratio <- suppressWarnings(as.double(data.start$Lower.Ratio))
  # data.start$Upper.Ratio <- suppressWarnings(as.double(data.start$Upper.Ratio))
  # data.start$Recent.ER <- suppressWarnings(as.double(data.start$Recent.ER))
  # data.start$WSP.status <- factor(data.start$WSP.status, levels =c("UD", "R", "RA", "A", "AG", "G"), ordered=T)
  # data.start$Management.Timing <- factor(data.start$Management.Timing, levels =c("Estu", "Early_Summer", "Summer", "Late"), ordered=T)
  data.new <- reactive({
    req(input$selected_species, input$selected_watershed, input$selected_year)
    df <- data.start  %>% filter(Base.Unit.Species %in% input$selected_species) %>%
                          #dplyr::select_if(colSums(!is.na(.)) > 0)
                          dplyr::select_if(sumfun)
    
    if(input$selected_watershed != "All"){
      df <- df %>% filter(BaseUnit.Watershed %in% input$selected_watershed) %>%
                   #dplyr::select_if(colSums(!is.na(.)) > 0)
                   dplyr::select_if(sumfun)
    }
    
    if(input$select_change == "Change" ){                     ########## WILL NEED TO SET THIS UP SO IT UPDATES METRICS AUTOMATICALLY WITHOUT CHANGING THIS - LOOK TO METRICS FILE FOR LIST OF NAMES
      func <- function(x){x-dplyr::lag(x, default=dplyr::first(x))}
      
      df <- df %>% group_by(Base.Unit.CU.ShortName) %>%
        filter(Year %in% c(input$selected_changeyear_1, input$selected_changeyear_2)) %>%
        arrange(Year, .by_group=TRUE) %>%
        mutate(WSP.numeric = as.numeric(WSP.status)) %>%
        dplyr::mutate_at(.vars = vars(Recent.Total, Lower.Ratio, Upper.Ratio, LongTerm.Ratio, ShortTerm.Trend, Recent.ER, WSP.numeric), .funs= func) %>%
        filter(Year== max(Year)) %>%
        select(-WSP.status)
    } 
    if(input$select_change=="Annual"){
      df <- df %>% filter(Year %in% input$selected_year)
    }
    df <- df %>% select(-Year)
    as.data.frame(df)
  })
  
 
  observeEvent(                            # will need to update selections once we have more than 2 years so both cannot be same year
    {input$select_changeyear_1},{ 
      updateSelectInput(session, "selected_changeyear_2", choices=levels(as.factor( unique(data.start$Year[data.start$Year > input$selected_changeyear_1]))), 
                        selected= levels(as.factor( unique(data.start$Year[data.start$Year > input$selected_changeyear_1])))[1] )  
    })    
  
  observeEvent(                            # will need to update selections once we have more than 2 years so both cannot be same year
    {input$select_changeyear_2},{ 
      updateSelectInput(session, "selected_changeyear_1", choices=levels(as.factor( unique(data.start$Year[data.start$Year < input$selected_changeyear_2]))), 
                        selected= levels(as.factor( unique(data.start$Year[data.start$Year < input$selected_changeyear_2])))[1] )  
    }) 
  
  
  #------------------- Parallel Coordinate Plot ------------------
  
  # Create data for the parallel plot (add row names and reorder columns and rows)
  data.par <- reactive({
    df <- as.data.frame(data.new())   
    rownames(df) <- df[,1]  
    df <- df %>% select(-dplyr::one_of("Base.Unit.CU.ShortName", "Base.Unit.Species", "BaseUnit.Watershed")) %>%  # one_of allows you to provide a list including names that may not be there
      select(-Management.Timing, Management.Timing) %>%
      select(-FAZ, FAZ)
    # Must re-order rows so most full rows are first and also have a full row as the first (no NAs)
    df[order(rowSums(is.na(df))),]
  })

  # create dimensions list with auxiliary information on numeric metrics to pass on to parcoords
  # each element in dims is a list with a set of parameters specific to dims[[metric]], where 'metric'
  # is one of the metrics included in the parcoords dataset
  dims <- reactive({
    dataset <- data.par()
    metrics <- names(dataset)
    names(metrics) <- metrics
    lapply(metrics, 
           function(m) {
             d <- list() # add any information on metric m here that we want to pass on to javascript
             # if there is a checkbox for this dim; allow it to set visibility, otherwise make it always visible
             d[['hide']] <- ifelse (any(names(input) == sId("visible", m)), !input[[sId("visible", m)]], FALSE) 
             if (m %in% numericMetrics) {
               d[['nullValue']] <- median(dataset[, m], na.rm = T) # change this to "top" or "bottom" to show nulls above or below chart
               d[['min']] <- min(dataset[, m], na.rm = T)
               d[['max']] <- max(dataset[, m], na.rm = T)
               if (sId("yrange", m) %in% names(input)) { # if there is an input widget for this dim, allow for it to set the ylims
                 d[['ymin']] <- input[[sId("yrange", m)]][1]
                 d[['ymax']] <- input[[sId("yrange", m)]][2]
               } else { # ylims not under user control
                 d[['ymin']] <- d[['min']]
                 d[['ymax']] <- d[['max']]
               }
             }
             d
           })
  })
  
  observeEvent({
    input$reset_brush
    data.par()
  },{
 
    output$parcoords <- renderParcoords({ parcoords(data=data.par(),
                                                    autoresize=TRUE,
                                                    color= list(colorScale=htmlwidgets::JS("d3.scale.category10()"), colorBy="Management.Timing"),
                                                    rownames=T,
                                                    alpha=0.6, 
                                                    alphaOnBrushed = 0,
                                                    brushMode="1D-axes-multi",
                                                    brushPredicate="and",
                                                    reorderable = TRUE, 
                                                    dimensions=dims(),
                                                    nullValueSeparator="nullValue",
                                                    tasks = list(
                                                      htmlwidgets::JS(sprintf(
                                                        "
                                                        function(){
                                                        debugger
                                                        this.parcoords.dimensions()['WSP.status']
                                                        .yscale = d3.scale.ordinal()
                                                        .domain([%s])
                                                        .rangePoints([
                                                        1,
                                                        this.parcoords.height()-this.parcoords.margin().top - this.parcoords.margin().bottom
                                                        ])
                                                        
                                                        this.parcoords.removeAxes();
                                                        this.parcoords.render();
                                                        // duplicated from the widget js code
                                                        //  to make sure reorderable and brushes work
                                                        // if( this.x.options.reorderable ) {
                                                        this.parcoords.reorderable();
                                                        
                                                        //
                                                        
                                                        
                                                        if( this.x.options.brushMode ) {
                                                        // reset the brush with None
                                                        this.parcoords.brushMode('None')
                                                        this.parcoords.brushMode(this.x.options.brushMode);
                                                        this.parcoords.brushPredicate(this.x.options.brushPredicate);
                                                        }
                                                        
                                                        
                                                        
                                                        
                                                        // delete title from the rownames axis
                                                        d3.select('#' + this.el.id + ' .dimension .axis > text').remove();
                                                        this.parcoords.render()
                                                        
                                                        }
                                                        "     ,
                                                        paste0(shQuote(rev(levels(data.par()$WSP.status))),collapse=",")     
                                                      ))
                                                    )
                                          )
                        })
  })
  
  # Create a block with miscellaneous controls for the parcoords plot
  output$parcoordsControls <- renderUI({
    metrics <- names(data.par())
    names(metrics) <- metrics 
    d <- dims()
    # control widgets for categorical metrics
    catWidgets <- lapply(metrics[!(metrics %in% numericMetrics)], 
                         function(m) { 
                           column(2,tags$div(title=metricInfo[[m]],
                                             checkboxInput(inputId = sId("visible", m), 
                                                           label=m,
                                                           value=!d[[m]][['hide']], width='20px'))) })
    # control widgets for numerical metrics
    numWidgets <- lapply(metrics[metrics %in% numericMetrics], 
                         function(m) { 
                           column(2,tags$div(title=metricInfo[[m]],
                                             checkboxInput(inputId = sId("visible", m), 
                                                           label=m,
                                                           value=!d[[m]][['hide']], width='20px'),
                                             sliderInput(inputId = sId("yrange", m),
                                                         label = NULL,
                                                         min = d[[m]][['min']],
                                                         max = d[[m]][['max']],
                                                         value = c(d[[m]][['ymin']],d[[m]][['ymax']]), 
                                                         width='80px')))})
    fluidRow(do.call(tagList, c(numWidgets, catWidgets)))})

  
  observeEvent({input$select_change}, {
    df <- data.par()
    for (m in names(df)) {
      if (sId("yrange", m) %in% names(input)) {
        updateSliderInput(session, sId("yrange", m), 
                          value = c(min(df[, m], na.rm=T),max(df[, m], na.rm=T)))
      }
    }
  })
  
  # leave this out for now; this triggers a call to parcoords, which resets the brushing 
  # which is not the desired beharviour. need to add option to draw parcoords with pre-selected data
  # to make this work.  
  #  observeEvent({input$scale_to_selected}, {
  #    df <- data.par()
  #    for (m in names(df)) {
  #      if (sId("yrange", m) %in% names(input)) { # adjust the sliders so ylims correspond to range of selected data
  #        updateSliderInput(session, sId("yrange", m), 
  #                          value = c(min(df[input$parcoords_brushed_row_names, m], na.rm=T),
  #                                    max(df[input$parcoords_brushed_row_names, m], na.rm=T)))
  #      }
  #    }
  #  })
  
  #------------------- Radar Plots ------------------
  
  # update available selections for the radar plot metrics
  data.metrics <- reactive({
    df <- as.data.frame(data.par()) 
    if(input$select_change=="Annual") {df2 <- df %>% select(-c(WSP.status, FAZ, Recent.ER, Management.Timing))}
    if(input$select_change == "Change") {df2 <- df %>% select(-c(WSP.numeric, FAZ, Recent.ER, Management.Timing))}
    df2
  })
  
  # Re-scale function adapted from ezR package (devtools::install_github("jerryzhujian9/ezR")
  ez.rescale02 = function (x) {
    if (is.numeric(x)) {
      # get rid of negative values
      if (min(x, na.rm=TRUE)<0) {x = x - min(x, na.rm=TRUE)}
      # scale all postive to max of 1
      if (max(x, na.rm=TRUE)!=0) {x = x/max(x, na.rm=TRUE)}
      result = abs(x-1)
    } else {
      result = x
    }
    return(result)
  }
  
  
  # Radar coordinates Helper Funciton so does not need to access ezR package
  coord_radar <- function (theta = "x", start = 0, direction = 1)
  {
    theta <- match.arg(theta, c("x", "y"))
    r <- ifelse (theta == "x", "y", "x")
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
            direction = sign(direction),
            is_linear = function(coord) TRUE)
  }
  
  observe({ 
    updateSelectInput(session, "selected_metric_1", choices=colnames(data.metrics()), selected=colnames(data.metrics())[1])
  })
  
  observeEvent(
    {input$selected_metric_1
      data.metrics()},
    {
      choices_2 <- data.metrics() %>% select(-dplyr::one_of(input$selected_metric_1)) %>%
        colnames()
      updateSelectInput(session, "selected_metric_2", choices=choices_2, selected = choices_2[1])
    })
  
  observeEvent(
    {input$selected_metric_1
      input$selected_metric_2
      data.metrics()},
    {
      choices_3 <- data.metrics() %>% select(-dplyr::one_of(input$selected_metric_1,input$selected_metric_2)) %>%
        colnames()
      updateSelectInput(session, "selected_metric_3", choices=choices_3, selected=choices_3[1])
    })
  
  # Pull selected metrics data
  metrics_subset <- reactive({
    req(input$selected_metric_1, input$selected_metric_2, input$selected_metric_3)
    df <- brushed.data() %>% select(dplyr::one_of("Base.Unit.CU.ShortName", input$selected_metric_1,
                                                  input$selected_metric_2, input$selected_metric_3))
    
    # Filter our CUs with fewer than 3 metrics for Radar plot
    df <-  df[rowSums(!is.na(df)) >= 4 ,]             
    
    #2) rescale
    #print(df)
    data.frame(lapply(df, ez.rescale02))
  })
  # 
  
  #  # Radar Plots Code of Brushed CUs
  observeEvent(
    {input$faceted
      metrics_subset()},{
        
        # Long format
        data.radar <- metrics_subset()
        
        #  select(-Management.Timing)
        
        df.long <- tidyr::gather(data.radar, variable,value,-"Base.Unit.CU.ShortName",factor_key = T)
        
        if(input$faceted == FALSE){
          p <- ggplot(df.long,  aes(x = variable, y = value,
                                    group = Base.Unit.CU.ShortName,
                                    color=Base.Unit.CU.ShortName,
                                    fill=Base.Unit.CU.ShortName,
                                    linetype=Base.Unit.CU.ShortName)) +
            geom_polygon(aes(), alpha=0.4, size = 1, show.legend = FALSE) +
            xlab("") + ylab("") +
            coord_radar()+
            scale_linetype_manual(values=rep("solid",nlevels(factor(df.long$Base.Unit.CU.ShortName))))+
            theme(axis.text.x = element_text(size = rel(0.8), angle = 0),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank()) +
            guides(color = guide_legend(ncol=2)) +
            geom_line(aes(), size = 1)
        }
        if(input$faceted == TRUE){
          p <- ggplot(df.long,  aes(x = variable, y = value,
                                    group = Base.Unit.CU.ShortName,
                                    color=Base.Unit.CU.ShortName,
                                    fill=Base.Unit.CU.ShortName,
                                    linetype=Base.Unit.CU.ShortName)) +
            geom_polygon(aes(), alpha=0.4, size = 1, show.legend = FALSE) +
            xlab("") + ylab("") +
            coord_radar()+
            scale_linetype_manual(values=rep("solid",nlevels(factor(df.long$Base.Unit.CU.ShortName))))+
            theme(strip.text.x = element_text(size = rel(1)),
                  axis.ticks.x = element_blank(),
                  axis.text.x = element_text(size = rel(0.8), angle = 0),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank()) +
            guides(color = "none") +
            facet_wrap(~Base.Unit.CU.ShortName)
        }
        
        output$radarPlot <- renderPlot({p})
        
      })
  
  
  #------------------- All Data Tab ------------------
  
  # Create another data object to show in the All Data tab
  data.show <- eventReactive(data.new(),{
    req(input$selected_species, input$selected_watershed, input$selected_year)
    
    df <- data.start  %>% filter(Base.Unit.Species %in% input$selected_species) %>%
                          #dplyr::select_if(colSums(!is.na(.)) > 0)
                          dplyr::select_if(sumfun)
    if(input$selected_watershed != "All"){
      df <- df %>% filter(BaseUnit.Watershed %in% input$selected_watershed) %>%
                   #dplyr::select_if(colSums(!is.na(.)) > 0)
                   dplyr::select_if(sumfun)
    }
    #df <- as.data.frame(df)
    #rownames(df) <- df[,1]
    df <- df %>% select(-dplyr::one_of("Base.Unit.Species", "BaseUnit.Watershed")) %>%  # one_of allows you to provide a list including names that may not be there
      select(-Management.Timing, Management.Timing) %>%
      select(-FAZ, FAZ)
    # Must re-order rows so most full rows are first and also have a full row as the first (no NAs)
    #na.order <- order(rowSums(is.na(df)))
    #df <- df[na.order,]
  })
  
  
  output$AllData <- DT::renderDataTable({
    DT::datatable(data.show())
  })
  
  #------------------- Extracted Data Tab ------------------
  
  brushed.data <- reactive({
    if(length(input$parcoords_brushed_row_names)>0){
      df <- data.new() %>% filter(Base.Unit.CU.ShortName %in% input$parcoords_brushed_row_names)
    }
    else{df <- data.new()}
    # write.csv(df, "brushed.data.csv")
    df
  })
  
  output$SelectedData <-  DT::renderDataTable({
    #ids <- rownames(data.par()) %in% input$parcoords_brushed_row_names
    df <- brushed.data() 
    rownames(df) <- df[,1] 
    df <- df[-c(1:3)]
    DT::datatable(df)
  })
  
  
  
  #------------------- Areas Tab ------------------
  
  
  output$Areas <- DT::renderDataTable({
    df <- metrics_subset()
    
    # Each will be the sum of 3 triangles
    a <- df[,2]
    b <- df[,3]
    c <- df[,4]
    
    side1 <- a*b*sin(pi/5)/2
    side2 <- b*c*sin(pi/5)/2
    side3 <- c*a*sin(pi/5)/2
    area <- side1 + side2 + side3
    df <- cbind(df, Area=area)
    DT::datatable(df)
  })
  
  #------------------- Summary Plots Tab ------------------
  
  # Summary Plots tab
  summary.prep <- function(variable=variable, type=input$selected_type, change=input$select_change){ # type = "Proportion" or "Number"
    #   subset_data <-subset(data.start, !duplicated(Base.Unit.CU.ShortName) )
    subset_data <- data.new()
    brushed_data <- brushed.data()
    
    if(variable == "Recent.ER"){
      if(change == "Annual"){
        breaks <- c( 0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
        names <- c("Below 10%","10-20%","20-30%","30%-40%","40-50%", "50%-60%","60-70%","70%-80%","80-90%","Above 90%")
      }
      if(change =="Change"){
        breaks <- c(-1, -0.1, -0.05, -0.01,0.01, 0.05, 0.1, 1)
        names <- c(">10% decrease", "5%-10% decrease", "0-5% decrease","No Change", "0-5% increase", "5-10% increase",">10 increase")
      }
      subset_data[,variable] <- cut(subset_data[,variable], breaks=breaks, labels=names)
      brushed_data[,variable] <- cut(brushed_data[,variable], breaks=breaks, labels=names)
    }
    
    subset_data[,variable] <- factor(subset_data[,variable], exclude=NULL)
    # subset_data[,variable] <- addNA(subset_data[,variable], ifany=TRUE)
    # brushed_data[,variable] <- addNA(brushed_data[,variable], ifany=TRUE)
    y <- factor(levels(subset_data[,variable]), ordered=TRUE, levels=levels(subset_data[,variable]), exclude=NULL)
    total <- as.vector(table(subset_data[,variable], useNA="ifany"))
    
    brush_full_selection <- factor(brushed_data[,variable], levels = levels(subset_data[,variable]), exclude=NULL)
    selected <- as.vector(table(brush_full_selection,useNA = "ifany"))
    not.selected <- total-selected
    perc <- selected/total
    
    if(type == "Proportion")  data.sum <- data.frame(y, selected=perc, unselected =(1-perc), text=c(paste("Total # of CUs:",as.character(total))))
    if(type =="Number")       data.sum <- data.frame(y, selected, unselected=not.selected, text=c(paste("Total # of CUs:",as.character(total))))
    #print(data.sum)
    #browser()
    data.sum$y <- forcats::fct_explicit_na(data.sum$y, "Unknown")
    
    # Create plot
    p <- plot_ly(data.sum, x = ~selected, y = ~y, text=~text, type = 'bar', orientation = 'h', name = 'Selected', 
                 marker = list(color = "darkred")) %>% 
      add_trace(x = ~unselected, name = 'Unselected', 
                marker = list(color = "rgba(128,128,128,0.6)")) %>%        # last number is transparenct;  colours for plotly @ https://reeddesign.co.uk/test/namedcolors.html
      layout(barmode = 'stack', xaxis = list(title = c(paste(input$selected_type, "of CUs"))), 
             yaxis = list(title = variable), margin = list(l = 130, r = 50, b = 50, t = 50, pad = 4))
    
  }
  
  # Percentage.Plots <- function(variable="Management.Timing"){
  observeEvent({
    brushed.data()
    input$selected_type},{
      p.1 <- summary.prep(variable="Management.Timing", type=input$selected_type, change=input$select_change)
      
      output$summaryPlot_MT <- renderPlotly({p.1})
      
      p.2 <- summary.prep(variable="FAZ", type=input$selected_type,change=input$select_change)
      output$summaryPlot_FAZ <- renderPlotly({p.2})
      
      if(input$select_change=="Annual") p.3 <- summary.prep(variable="WSP.status", type=input$selected_type, change=input$select_change)
      if(input$select_change=="Change") p.3 <- summary.prep(variable="WSP.numeric", type=input$selected_type, change=input$select_change)
      output$summaryPlot_WSP <- renderPlotly({p.3})
      
      p.4 <-  summary.prep(variable="Recent.ER", type=input$selected_type,change=input$select_change)
      
      output$summaryPlot_ER <- renderPlotly({p.4})
    }
  )
  
} # end server function
#   

# 
# # Add observer to write to screen if metric = NA
#  incomplete <- reactive({
#        metrics_subset() %>%  filter(!complete.cases(.)) %>%
#                         select(Base.Unit.CU.ShortName)
#  })    
#  
# observeEvent(incomplete(),{
#     if(length(incomplete()$Base.Unit.CU.ShortName) == 0 ){
#                     output$incomplete_plots <- renderText({paste("")})
#     }
#     if(length(incomplete()$Base.Unit.CU.ShortName) > 0){
#                     output$incomplete_plots <- renderText({
#                                               paste("The following CUs are missing metric values for",
#                                                     input$selected_metric,
#                                                     ":",
#                                                     toString(incomplete()$Base.Unit.CU.ShortName) )
#                      })
#     }
# })
# # 
