library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# VolcaNoseR - A Shiny app for for nosing around in volcano plots
# Created by Joachim Goedhart (@joachimgoedhart), first version 2019
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Joachim Goedhart (C) 2019
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Makes sure the plot theme changes 
# with the dashboad skin (dark, light)
# thematic::thematic_shiny(font = "Source Sans Pro")

# in 1
#####
in_1 <- conditionalPanel(
  condition = "input.side == 'xy'",
  bs4Dash::box(
    selectInput(
      inputId = "x_var",
      label = "X-axis; Effect (fold change)",
      choices = "",
      selected = NULL
    ),
    selectInput(
      inputId = "y_var",
      label = "Y-axis; Significance (p-value)",
      choices = "",
      selected = NULL
    ),
    selectInput(
      inputId = "g_var",
      label = "Select column with names",
      choices = "",
      selected = NULL
    ))
)
#####  

# in 2
#####

in_2 <- conditionalPanel(
  condition = "input.side == 'aesth'",
  bs4Dash::box(
    title = "Plot aesthetics",
    width = 12,
    status = "primary",
    collapsible = FALSE,
    solidHeader = TRUE,
    
    sliderInput(
      inputId = "pointSize",
      label = "Point size",
      value = 4,
      min = 0,
      max = 10,
      step =  1
    ),
    sliderInput(
      inputId = "alphaInput",
      label = "Transparency (alpha)",
      value = 0.8,
      min = 0,
      max = 1,
      step = 0.05
    )
  )
)

#####

# in_3
#####

in_3 <- conditionalPanel(
  condition = "input.side == 'hits'",
  bs4Dash::box(
    title = "Selection & Annotation",
    width = 12,
    status = "primary",
    collapsible = FALSE,
    solidHeader = TRUE,
    
    sliderInput(
      inputId = "fc_cutoff",
      label = "Fold Change threshold",
      min = -5,
      max = 5,
      step = 0.1,
      value = c(-1.5, 1.5)
    ),
    
    sliderInput(
      inputId = "p_cutoff",
      label = "Significance threshold",
      min = 0,
      max = 5,
      step = 0.1,
      value = 2
    ),
    selectInput(
      inputId = "direction",
      label = "Use thresholds to annotate:",
      choices = list(
        "All (ignores thresholds)" = "all",
        "Changed (and significant)" = "significant",
        "Increased (and significant)" = "increased",
        "Decreased (and significant)" = "decreased"
      ),
      selected = "significant"
    ),
    
    selectInput(
      "criterion",
      label = "Criterion for ranking hits:",
      choices = list(
        "Manhattan distance" = "manh",
        "Euclidean distance" = "euclid",
        "Fold change" = "fc",
        "Significance" = "sig"
      ),
      selected = "manh"
    ),
    
    numericInput("top_x", "Number of top hits (0 to hide):", value = 10),
    
    selectizeInput(
      inputId = 'user_gene_list',
      label = "User selected hits:",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(create = TRUE)
    ),
    
    checkboxInput(
      inputId = "hide_labels",
      label = "Hide labels in the plot",
      value = FALSE
    )#,
    
    # radioButtons(
    #   inputId = "adjustcolors",
    #   label = "Color (Unchanged,Increased,Decreased)",
    #   choices =
    #     list(
    #       "Grey, Red, Blue" = 1,
    #       "Grey, Blue, Green" = 3,
    #       "Grey, Cyan, Purple" = 4,
    #       "User defined" = 5
    #     ),
    #   selected =  1
    # ),
    # conditionalPanel(
    #   condition = "input.adjustcolors == 5",
    #   textInput("user_color_list", "List of names or hexadecimal codes", value = "turquoise2,#FF2222,lawngreen")
    # )
  )
)

#####

# in_4
#####

in_4 <- conditionalPanel(
  condition = "input.side == 'scale'",
  bs4Dash::box(
    title = "Scaling & Transformation",
    width = 12,
    status = "primary",
    collapsible = FALSE,
    solidHeader = TRUE,
    checkboxInput(
      inputId = "rotate_plot",
      label = "Rotate plot 90 degrees",
      value = FALSE
    ),
    checkboxInput(
      inputId = "add_grid",
      label = "Add gridlines",
      value = FALSE
    ),
    checkboxInput(
      inputId = "change_scale",
      label = "Change scale",
      value = FALSE
    ),
    conditionalPanel(
      condition = "input.change_scale == true",
      textInput("range_x", "Range x-axis (min,max)", value = "")
    ),
    conditionalPanel(
      condition = "input.change_scale == true",
      textInput("range_y", "Range y-axis (min,max)", value = ""),
      checkboxInput(
        inputId = "scale_log_10",
        label = "Log10 scale on y-axis",
        value = FALSE
      )
    ),
    numericInput("plot_height", "Plot height (# pixels): ", value = 600),
    numericInput("plot_width", "Plot width (# pixels):", value = 750)
  )
)

#####

# in_5
#####
in_5 <- conditionalPanel(
  condition = "input.side == 'label'",
  bs4Dash::box(
    title = "Labels",
    width = 12,
    status = "primary",
    collapsible = FALSE,
    solidHeader = TRUE,
    checkboxInput(
      inputId = "add_title",
      label = "Add title",
      value = FALSE
    ),
    conditionalPanel(condition = "input.add_title == true",
                     textInput("title", "Title:", value = "")),
    checkboxInput(
      inputId = "label_axes",
      label = "Change axis labels",
      value = FALSE
    ),
    conditionalPanel(
      condition = "input.label_axes == true",
      textInput("lab_x", "X-axis:", value = ""),
      textInput("lab_y", "Y-axis:", value = "")
    ),
    checkboxInput(
      inputId = "adj_fnt_sz",
      label = "Change font size",
      value = FALSE
    ),
    conditionalPanel(
      condition = "input.adj_fnt_sz == true",
      numericInput("fnt_sz_title", "Plot title:", value = 20),
      numericInput("fnt_sz_labs", "Axis titles:", value = 16),
      numericInput("fnt_sz_ax", "Axis labels:", value = 14),
      numericInput("fnt_sz_cand", "Labels of hits:", value = 6)
    ),
    
    checkboxInput(
      inputId = "add_legend",
      label = "Add legend",
      value = FALSE
    )
  ))
#####

# in_6
#####
in_6 <- conditionalPanel(
  condition = "input.side == 'about'",
  bs4Dash::box(
    title = "About VolcaNoseR",
    width = 12,
    status = "primary",
    collapsible = FALSE,
    solidHeader = TRUE,
    
    includeHTML("about.html")
  )
)
#####

# Main UI
#####

ui <- dashboardPage(dark = FALSE,
  title = "Tercen|Volcano plot",
  preloader = list(html = spin_1(), color = "#333e48"),
  header = dashboardHeader(
    dashboardBrand(title = "VolcaNoseR - Exploring volcano plots",
                   href = "https://github.com/JoachimGoedhart/VolcaNoseR"),
    title = dashboardBrand(
      title = "Tercen::Volcano",
      href = "https://github.com/teofiln/volcano_shiny_operator",
      image = "tercen.png"
    )
  ),
  sidebar = dashboardSidebar(skin = "light",
    width = "15%",
    status = "primary",
    sidebarMenu(
      id = "side",
      compact = TRUE,
      # menuItem(tabName = 'xy', text = "Fold change and significance", icon = icon("chart-bar")),
      menuItem(
        tabName = 'about',
        text = "About",
        icon = icon("user")
      ),
      menuItem(
        tabName = "hits",
        text = "Selection & Annotation",
        icon = icon("dna")
      ),
      menuItem(
        tabName = "scale",
        text = "Scaling & Transformation",
        icon = icon("ruler")
      ),
      menuItem(
        tabName = "aesth",
        text = "Aesthetics",
        icon = icon("adjust")
      ),
      menuItem(
        tabName = "label",
        text = "Labels",
        icon = icon("tags")
      )
    )
  ),
  
  body = dashboardBody(
    
    # to prevent timeout 
    shinyjs::useShinyjs(),
    tags$script(HTML('setInterval(function(){ $("#hiddenButton").click(); }, 1000*4);')),
    tags$footer(shinyjs::hidden(actionButton(inputId = "hiddenButton", label="hidden"))),
    
    fluidRow(
      column(width = 4,
             # in_1,
             in_2,
             in_3,
             in_4,
             in_5,
             in_6),
      column(
        width = 8,
        bs4Dash::box(
          title = "Volcano Plot",
          width = 12,
          height = 700,
          status = "primary",
          collapsible = FALSE,
          solidHeader = TRUE,
          elevation = 2,
          downloadButton("downloadPlotPDF", "Download pdf"),
          downloadButton("downloadPlotPNG", "Download png"),
          div(style="height: 4px;"),
          div(
            style = "position:relative",
            plotOutput(
              outputId = "coolplot",
              hover = hoverOpts("plot_hover", delay = 10, delayType = "debounce")
            ),
            uiOutput("hover_info")
          )
        )
      )

    )))
