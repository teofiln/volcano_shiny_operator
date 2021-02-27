
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


library(shiny)

ui <- fluidPage(
  # Application title
  titlePanel("VolcaNoseR - Exploring volcano plots"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabs=='Plot'",
        
        checkboxInput(
          "xy",
          label = h4('Select X & Y variables'),
          value = FALSE
        ),
        conditionalPanel(
          condition = 'input[["xy"]] == true',
          
          selectInput("x_var", label = "X-axis; Effect (fold change)", choices = NULL),
          selectInput("y_var", label = "Y-axis; Significance (p-value)", choices = NULL),
          selectInput("g_var", label = "Select column with names", choices = NULL)
        ),
        
        checkboxInput("aesth", label = h4("Aesthetics"), value = FALSE),
        conditionalPanel(
          condition = 'input[["aesth"]] == true',
          sliderInput("pointSize", "Size of the datapoints", 0, 10, 4),
          sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.8)
        ),
        
        checkboxInput(
          "hits",
          label = h4("Selection & Annotation of hits"),
          value = FALSE
        ),
        conditionalPanel(
          condition = 'input[["hits"]] == true',
          
          sliderInput(
            "fc_cutoff",
            "Fold Change threshold:",-5,
            5,
            step = 0.1,
            value = c(-1.5, 1.5)
          ),
          sliderInput(
            "p_cutoff",
            "Significance threshold:",
            0,
            5,
            step = 0.1,
            value = 2
          ),
          selectInput(
            "direction",
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
            # allow for multiple inputs
            options = list(create = TRUE)
          ),
          # if TRUE, allows newly created inputs
          
          checkboxInput(
            inputId = "show_table",
            label = "Show table with hits",
            value = FALSE
          ),
          checkboxInput(
            inputId = "hide_labels",
            label = "Hide labels in the plot",
            value = FALSE
          ),
          
          radioButtons(
            "adjustcolors",
            "Color (Unchanged,Increased,Decreased)",
            choices =
              list(
                "Grey, Red, Blue" = 1,
                "Grey, Blue, Green" = 3,
                "Grey, Cyan, Purple" = 4,
                "User defined" = 5
              ),
            selected =  1
          ),
          conditionalPanel(
            condition = "input.adjustcolors == 5",
            textInput("user_color_list", "List of names or hexadecimal codes", value = "turquoise2,#FF2222,lawngreen")
          ),
          checkboxInput(
            inputId = "dark",
            label = "Dark Theme",
            value = FALSE
          )
        ),
        
        checkboxInput(
          "scale",
          label = h4("Transformation & Scaling"),
          value = FALSE
        ),
        conditionalPanel(
          condition = 'input[["scale"]] == true',
          
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
          numericInput("plot_width", "Plot width (# pixels):", value = 800)
        ),
        
        checkboxInput("labels", label = h4("Labels"), value = FALSE),
        conditionalPanel(
          condition = 'input[["labels"]] == true',
          
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
            numericInput("fnt_sz_title", "Plot title:", value = 24),
            numericInput("fnt_sz_labs", "Axis titles:", value = 24),
            numericInput("fnt_sz_ax", "Axis labels:", value = 18),
            numericInput("fnt_sz_cand", "Labels of hits:", value = 6)
            
          ),
          
          checkboxInput(
            inputId = "add_legend",
            label = "Add legend",
            value = FALSE
          ),
          
          NULL
        ),
        
        conditionalPanel(condition = "input.tabs=='iPlot'", h4("iPlot"))
      ),
      
      conditionalPanel(
        condition = "input.tabs=='About'",
        h4("Find our other dataViz apps at:"),
        a("https://huygens.science.uva.nl/", href = "https://huygens.science.uva.nl/")
      )
    ),
    #Close sidebarPanel
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      id = "tabs",
      tabPanel(
        "Plot",
        h3("Volcano Plot"),
        downloadButton("downloadPlotPDF", "Download pdf-file"),
        downloadButton("downloadPlotPNG", "Download png-file"),
        div(
          style = "position:relative",
          plotOutput(
            "coolplot",
            hover = hoverOpts("plot_hover", delay = 10, delayType = "debounce")
          ),
          uiOutput("hover_info")
        )
      ),
      tabPanel("About", includeHTML("about.html"))
    ))   #Close mainPanel
  ) #Close sidebarLayout
) #Close fluidPage