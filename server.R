library(shiny)
library(bs4Dash)
library(ggplot2)
library(magrittr)
library(dplyr)
library(ggrepel)
library(thematic)
library(tercen)
library(waiter)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

server <- function(input, output, session) {
  
  ## For plot theme to switch with dashboard theme
  # useAutoColor()
  
  ## For loading spinner in plot when plot is rendered
  w <- Waiter$new(id = "coolplot")
  
  
  # Session variable - initialize defaults
  genelist.selected <- ""
  
  ###### DATA INPUT ###################
  
  getValues <- function(session){
    ctx <- getCtx(session)
    labs <- ctx$labels[[1]]
    values <- list()
    vars <- c(".x", ".y", labs)
    values$data <- ctx$select(vars)
    return(values)
  }
  
  getData <- reactive({
    getValues(session)[["data"]]
  })
  
  #### DISPLAY UPLOADED DATA (as provided) ##################
  # this part removed as the data are displayed in the Tercen crosstab
  
  ################ Select top hits #########
  df_top  <- reactive({
    df <- df_filtered()
    
    if (input$direction == "increased") {
      df <- df %>% filter(Change == "Increased")
    } else if (input$direction == "decreased") {
      df <- df %>% filter(Change == "Decreased")
    } else if (input$direction == "significant") {
      df <- df %>% filter(Change != "Unchanged")
    }
    
    if (input$criterion == "manh") {
      df <-
        df %>%
        mutate(`Manhattan distance` = abs(`Significance`) + abs(`Fold change (log2)`)) %>%
        arrange(desc(`Manhattan distance`))
      
      df_out <-
        df %>%
        top_n(input$top_x, `Manhattan distance`) %>%
        select(Name,
               Change,
               `Fold change (log2)`,
               `Significance`,
               `Manhattan distance`)
    } else if (input$criterion == "euclid") {
      df <-
        df %>%
        mutate(`Euclidean distance` = sqrt((`Significance`) ^ 2 + (`Fold change (log2)`) ^2)) %>%
        arrange(desc(`Euclidean distance`))
      
      df_out <-
        df %>%
        top_n(input$top_x, `Euclidean distance`) %>%
        select(Name,
               Change,
               `Fold change (log2)`,
               `Significance`,
               `Euclidean distance`)
      
    } else if (input$criterion == "fc") {
      df <- df %>% arrange(desc(abs(`Fold change (log2)`)))
      df_out <-
        df %>% top_n(input$top_x, abs(`Fold change (log2)`)) %>%
        select(Name, Change, `Fold change (log2)`, `Significance`)
    } else if (input$criterion == "sig") {
      df <- df %>% arrange(desc(`Significance`))
      df_out <-
        df %>% top_n(input$top_x, `Significance`) %>%
        select(Name, Change, `Fold change (log2)`, `Significance`)
    }
    
    #Add user selected hits, but remove them when already present
    df_out <- bind_rows(df_out, df_user()) %>% 
      distinct(Name, .keep_all = TRUE)
    
    # observe({print(df_out)})
    return(df_out)
  })
  
  ################ List of user-selected hits #########
  df_user <- reactive({
    df <- as.data.frame(df_filtered())
    
    #select based on text input
    usr_selection <- input$user_gene_list
    df_selected_by_name <-
      df %>% filter(Name %in% usr_selection)
    
    #Select rows from DT
    table_selection <- input$uploaded_rows_selected
    # observe({print(table_selection)})
    if (length(table_selection) >= 1) {
      df_selected_by_tab <- df %>% slice(table_selection)
      df_selected_by_name <-
        df_selected_by_name %>% bind_rows(df_selected_by_tab)
    }
    
    return(df_selected_by_name)
  })
  
  ################ SELECT COLUMNS AND ANNOTATE CHANGES #########
  df_filtered <- reactive({
    
    df <- getData()
    
    koos <-
      df %>% select(
        `Fold change (log2)` = .x,
        `Significance` = .y,
        Name = gene_name
      )
    #Remove  names after semicolon for hits with multiple names, seperated by semicolons, e.g.: POLR2J3;POLR2J;POLR2J2
    koos <- koos %>% mutate(Name = gsub(';.*', '', Name))
    
    #Update the gene list for user selection
    updateSelectizeInput(session,
                         "user_gene_list",
                         choices = koos$Name,
                         server = TRUE,
                         selected = genelist.selected)
    
    foldchange_min = input$fc_cutoff[1]
    foldchange_max = input$fc_cutoff[2]
    
    pvalue_tr = input$p_cutoff
    
    if (input$direction == "decreased") {
      koos <- koos %>% mutate(
        Change = case_when(
          `Fold change (log2)` < foldchange_min &
            `Significance` > pvalue_tr ~ "Decreased",
          TRUE ~ "Unchanged"
        )
      )
    } else if (input$direction == "increased") {
      koos <- koos %>% mutate(
        Change = case_when(
          `Fold change (log2)` > foldchange_max &
            `Significance` > pvalue_tr ~ "Increased",
          TRUE ~ "Unchanged"
        )
      )
    } else {
      koos <- koos %>% mutate(
        Change = case_when(
          `Fold change (log2)` > foldchange_max &
            `Significance` > pvalue_tr ~ "Increased",
          `Fold change (log2)` < foldchange_min &
            `Significance` > pvalue_tr ~ "Decreased",
          TRUE ~ "Unchanged"
        )
      )
    }
    
    return(koos)
  })
  
  plot_data <- reactive({
    
    ############## Adjust X-scaling if necessary ##########
    
    #Adjust scale if range for x (min,max) is specified
    if (input$range_x != "" &&  input$change_scale == TRUE) {
      rng_x <- as.numeric(strsplit(input$range_x, ",")[[1]])
      observe({
        print(rng_x)
      })
    } else if (input$range_x == "" ||
               input$change_scale == FALSE) {
      rng_x <- c(NULL, NULL)
    }
    
    ############## Adjust Y-scaling if necessary ##########
    
    #Adjust scale if range for y (min,max) is specified
    if (input$range_y != "" &&  input$change_scale == TRUE) {
      rng_y <- as.numeric(strsplit(input$range_y, ",")[[1]])
    } else if (input$range_y == "" ||
               input$change_scale == FALSE) {
      rng_y <- c(NULL, NULL)
    }
    
    df <- as.data.frame(df_filtered())
    #Convert 'Change' to a factor to keep this order, necessary for getting the colors right
    df$Change <-
      factor(df$Change, levels = c("Unchanged", "Increased", "Decreased"))
    
    p <-  ggplot(data = df) +
      aes(x = `Fold change (log2)`) +
      aes(y = `Significance`) +
      geom_point(
        alpha = input$alphaInput,
        size = input$pointSize,
        shape = 16
      ) +
      aes(color = Change) +
      theme_bw()
    
    #Indicate cut-offs with dashed lines
    if (input$direction != "decreased")
      p <-
      p + geom_vline(
        xintercept = input$fc_cutoff[2],
        linetype = "dashed"
      )
    if (input$direction != "increased")
      p <-
      p + geom_vline(
        xintercept = input$fc_cutoff[1],
        linetype = "dashed"
      )
    
    p <-
      p + geom_hline(
        yintercept = input$p_cutoff,
        linetype = "dashed"
      )
    
    # if log-scale checked specified
    if (input$scale_log_10)
      p <- p + scale_y_log10()
    
    #remove gridlines (if selected)
    if (input$add_grid == FALSE) {
      p <- p + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
    }
    
    ########## User defined labeling
    if (input$hide_labels == FALSE) {
      p <-
        p + geom_point(
          data = df_top(),
          aes(x = `Fold change (log2)`, y = `Significance`),
          shape = 1,
          size = (input$pointSize)
        ) +
        geom_text_repel(
          data = df_top(),
          aes(label = Name),
          size = input$fnt_sz_cand,
          nudge_x = 0.2,
          nudge_y = 0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3 + input$pointSize * 0.1, "lines"),
          show.legend = F
        )
    }
    
    p <-
      p + coord_cartesian(xlim = c(rng_x[1], rng_x[2]),
                          ylim = c(rng_y[1], rng_y[2]))
    #### If selected, rotate plot 90 degrees CW ####
    if (input$rotate_plot == TRUE) {
      p <-
        p + coord_flip(xlim = c(rng_x[1], rng_x[2]),
                       ylim = c(rng_y[1], rng_y[2]))
    }
    
    ########## Do some formatting of the lay-out ##########
    
    # if title specified
    if (!is.null(input$title)) {
      #Add line break to generate some space
      title <- paste(input$title, "\n", sep = "")
      p <- p + labs(title = title)
    } else if (input$sheet != " ") {
      title <- paste(input$sheet, "\n", sep = "")
      # observe({print('yay')})
      p <- p + labs(title = title)
    }
    
    # # if labels specified
    if (input$label_axes)
    {
      p <- p + labs(x = input$lab_x, y = input$lab_y)
    }
    else {
      p <-
        p + labs(
          x = bquote('Fold Change (' * Log[2] * ')'),
          y = bquote('Significance (' * -Log[10] * ')')
        )
    }
    
    # # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size = input$fnt_sz_ax))
      p <-
        p + theme(axis.title = element_text(size = input$fnt_sz_labs))
      p <-
        p + theme(plot.title = element_text(size = input$fnt_sz_title))
    }
    
    #remove legend (if selected)
    if (input$add_legend == FALSE) {
      p <- p + theme(legend.position = "none")
    }
    
    p
  })
  
  
  ##### Render the plot ############
  
  ##### Set width and height of the plot area
  width <- reactive ({
    input$plot_width
  })
  height <- reactive ({
    input$plot_height
  })
  
  output$coolplot <- renderPlot(width = width, height = height, {
    w$show()
    w$hide()
    
    df <- as.data.frame(df_filtered())
    #Convert 'Change' to a factor to keep this order, necessary for getting the colors right
    df$Change <-
      factor(df$Change, levels = c("Unchanged", "Increased", "Decreased"))
    
    ############## Adjust X-scaling if necessary ##########
    
    #Adjust scale if range for x (min,max) is specified
    if (input$range_x != "" &&  input$change_scale == TRUE) {
      rng_x <- as.numeric(strsplit(input$range_x, ",")[[1]])
      observe({
        print(rng_x)
      })
    } else if (input$range_x == "" ||
               input$change_scale == FALSE) {
      rng_x <- c(NULL, NULL)
    }
    
    ############## Adjust Y-scaling if necessary ##########
    
    #Adjust scale if range for y (min,max) is specified
    if (input$range_y != "" &&  input$change_scale == TRUE) {
      rng_y <- as.numeric(strsplit(input$range_y, ",")[[1]])
    } else if (input$range_y == "" ||
               input$change_scale == FALSE) {
      rng_y <- c(NULL, NULL)
    }
    
    p <-  ggplot(data = df) +
      aes(x = `Fold change (log2)`) +
      aes(y = `Significance`) +
      geom_point(
        alpha = input$alphaInput,
        size = input$pointSize,
        shape = 16
      ) +
      aes(color = Change) +
      theme_bw()
    
    #Indicate cut-offs with dashed lines
    if (input$direction != "decreased")
      p <-
      p + geom_vline(
        xintercept = input$fc_cutoff[2],
        linetype = "dashed"
      )
    if (input$direction != "increased")
      p <-
      p + geom_vline(
        xintercept = input$fc_cutoff[1],
        linetype = "dashed"
      )
    
    p <-
      p + geom_hline(
        yintercept = input$p_cutoff,
        linetype = "dashed"
      )
    
    # if log-scale checked specified
    if (input$scale_log_10)
      p <- p + scale_y_log10()
    
    #remove gridlines (if selected)
    if (input$add_grid == FALSE) {
      p <- p + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
    }
    
    ########## User defined labeling
    if (input$hide_labels == FALSE) {
      p <-
        p + geom_point(
          data = df_top(),
          aes(x = `Fold change (log2)`, y = `Significance`),
          shape = 1,
          size = input$pointSize
        ) +
        geom_text_repel(
          data = df_top(),
          aes(label = Name),
          size = input$fnt_sz_cand,
          nudge_x = 0.2,
          nudge_y = 0.2,
          box.padding = unit(0.9, "lines"),
          point.padding = unit(.3 + input$pointSize * 0.1, "lines"),
          show.legend = F
        )
      
    }
    #
    # ########## Top hits labeling
    # if (input$hide_labels == FALSE) {
    #   p <- p + geom_text_repel(
    #     data = df_top(),
    #     aes(label = Name),
    #     size = input$fnt_sz_cand,
    #     nudge_x = 0.2,
    #     nudge_y=-0.2,
    #     # check_overlap = TRUE,
    #     box.padding = unit(0.35, "lines"),
    #     point.padding = unit(0.3+input$pointSize*0.1, "lines"),
    #     show.legend=F
    #   )
    #
    # }
    p <-
      p + coord_cartesian(xlim = c(rng_x[1], rng_x[2]),
                          ylim = c(rng_y[1], rng_y[2]))
    #### If selected, rotate plot 90 degrees CW ####
    if (input$rotate_plot == TRUE) {
      p <-
        p + coord_flip(xlim = c(rng_x[1], rng_x[2]),
                       ylim = c(rng_y[1], rng_y[2]))
    }
    ########## Do some formatting of the lay-out ##########
    # if title specified
    
    if (!is.null(input$title)) {
      #Add line break to generate some space
      title <- paste(input$title, "\n", sep = "")
      p <- p + labs(title = title)
    } else if (input$sheet != " ") {
      title <- paste(input$sheet, "\n", sep = "")
      # observe({print('yay')})
      p <- p + labs(title = title)
    }
    
    # # if labels specified
    if (input$label_axes)
    {
      p <- p + labs(x = input$lab_x, y = input$lab_y)
    }
    else {
      p <-
        p + labs(
          x = bquote('Fold Change (' * Log[2] * ')'),
          y = bquote('Significance (' * -Log[10] * ')')
        )
    }
    
    # # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size = input$fnt_sz_ax))
      p <-
        p + theme(axis.title = element_text(size = input$fnt_sz_labs))
      p <-
        p + theme(plot.title = element_text(size = input$fnt_sz_title))
    }
    
    #remove legend (if selected)
    if (input$add_legend == FALSE) {
      p <- p + theme(legend.position = "none")
    }
    
    p
  })
  
  ###### From: https://gitlab.com/snippets/16220 ########
  output$hover_info <- renderUI({
    req(df_filtered())
    df <- as.data.frame(df_filtered())
    
    hover <- input$plot_hover
    point <-
      nearPoints(
        df,
        hover,
        threshold = 10,
        maxpoints = 1,
        addDist = FALSE
      )
    if (nrow(point) == 0)
      return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <-
      (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <-
      (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <-
      hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0(
      "position:absolute;
                  padding: 5px;
                  z-index:100; background-color: rgba(200, 200, 245, 0.65); ",
      "left:",
      left_px + 20,
      "px; top:",
      top_px + 32,
      "px;"
    )
    
    # actual tooltip created as wellPanel
    wellPanel(style = style,
              p(HTML(
                paste0(
                  "<b> Name: </b>",
                  point$Name,
                  "<br/>",
                  "<b> Fold change: </b>",
                  round(point[1], 2),
                  "<br/>",
                  "<b> Significance: </b>",
                  round(point[2], 2),
                  "<br/>",
                  # "<b> Number: </b>", rownames(point), "<br/>",
                  # top_px,
                  NULL
                )
              )))
  })
  
  
  ######### DEFINE DOWNLOAD BUTTONS FOR ORDINARY PLOT ###########
  
  output$downloadPlotPDF <- downloadHandler(filename <- function() {
    paste("VolcaNoseR_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    pdf(file,
        width = input$plot_width / 72,
        height = input$plot_height / 72)
    plot(plot_data())
    
    dev.off()
  },
  contentType = "application/pdf" # MIME type of the image
  )
  
  
  output$downloadPlotPNG <- downloadHandler(filename <- function() {
    paste("VolcaNoseR_", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    png(
      file,
      width = input$plot_width * 4,
      height = input$plot_height * 4,
      res = 300
    )
    plot(plot_data())
    
    dev.off()
  },
  contentType = "application/png" # MIME type of the image
  )
  
  ######## The End; close server ########################
} #Close server
