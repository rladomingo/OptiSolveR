# Load R packages
if (!require(shiny)) install.packages('shiny')
if (!require(shinythemes)) install.packages('shinythemes')
if (!require(shinyMatrix)) install.packages('shinyMatrix')

# Define UI
ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage(
    title = h3("OptiSolveR"),
    tabPanel(
      title = h3("Quadratic Spline Interpolation"),
      sidebarPanel(
        tags$head( # To design the buttons
          tags$style(
            HTML("#qsi {background-color: #4c9be8; font-weight: bold} #qsi_example {background-color: #abb6c2; font-weight: bold}")
          )
        ),
        HTML(paste("
          <p class=\"lead\"><b>Description:</b></p>
          <p>This is a generic solver for quadratic spline interpolation.</p>
          <hr/>
          <p class=\"lead\"><b>Syntax:</b></p>
          <p><b class=\"text-info\">Spaces</b> separate the x- and y- values of a point. 
          <b class=\"text-info\">Newline</b> distinguishes the next point.
          The <b class=\"text-info\">x-values</b> must be typed at the left side, 
          while the <b class=\"text-info\">y-values</b> must be typed at the right. 
          Click the button below to see an example.</p>
         ")),
        actionButton(
          inputId = "qsi_example",
          label = "Show example",
        ),
        hr(),
        textAreaInput(
          inputId = "qsi_data",
          label = "Type the set of points here:",
        ),
        textInput(
          inputId = "qsi_x",
          label = "Value to be approximated:"
        ),
        actionButton(
          inputId = "qsi",
          label = "Solve",
        ),
        htmlOutput(outputId = "qsi_warning")
      ), # sideBarPanel
      mainPanel(
        htmlOutput(outputId = "qsi_result"),
        htmlOutput(outputId = "qsi_fxns_text"),
        verbatimTextOutput(outputId = "qsi_fxns"),
        htmlOutput(outputId = "qsi_y_text"),
        htmlOutput(outputId = "qsi_y")
      ) # mainPanel
    ), # tabPanel
    tabPanel(
      title = h3("Simplex Method"),
      sidebarPanel(
        tags$head( # To design the buttons
          tags$style(
            HTML("#simplex_generate {background-color: #4c9be8; font-weight: bold} #simplex_example {background-color: #abb6c2; font-weight: bold}")
          )
        ),
        HTML(paste("
          <p class=\"lead\"><b>Description:</b></p>
          <p>This is a generic solver for the simplex method.</p>
          <hr/>
          <p class=\"lead\"><b>Instructions:</b></p>
          <p> To generate a table to input your data in, you must first choose the mode (<span class=\"text-info\"><b>maximization</b></span> or 
          <span class=\"text-info\"><b>minimization</b></span>), and type the number of the constraints and variables of your linear programming problem.</p>
          <p>The first line of the generated table must be the objective function, written in the form of 
          <b class=\"text-info\">c<sub>1</sub>x<sub>1</sub> + c<sub>2</sub>x<sub>2</sub> + ... + c<sub>n</sub>x<sub>n</sub> = Z</b>.
          In the table, Z (equivalent to the last column in the first row) must be equal to 1.</p>
          <p>The remaining rows are for the constraints (excluding the nonnegativity constraints) in the form of inequalities. 
          If it is a maximization problem, the constraints must be of type <span class=\"text-info\"><b><=</b></span>. Otherwise, 
          they must be of type <span class=\"text-info\"><b>>=</b></span>.</p>
          <p>Click the button to see the example below in table form:</p>
          <details>
          <summary>Maximization Example <i>(Click to expand)</i></summary>
          <p><br/>&emsp;&emsp;Maximize<br/>&emsp;&emsp;&emsp;&emsp;Z = 150x<sub>1</sub> + 175x<sub>2</sub><br/>&emsp;&emsp;subject to</br>
          &emsp;&emsp;&emsp;&emsp;7x<sub>1</sub> + 11x<sub>2</sub> &le; 77</br>
          &emsp;&emsp;&emsp;&emsp;10x<sub>1</sub> + 8x<sub>2</sub> &le; 80</br>
          &emsp;&emsp;&emsp;&emsp;x<sub>1</sub> &le; 9</br>
          &emsp;&emsp;&emsp;&emsp;x<sub>2</sub> &le; 6</br>
          &emsp;&emsp;&emsp;&emsp;x<sub>1</sub>, x<sub>2</sub> &ge; 0</p>
          </details>
          <br/>
        ", sep = "")),
        actionButton(
          inputId = "simplex_example",
          label = "Show example",
        ),
        hr(),
        selectInput(
          inputId = "simplex_mode",
          label = "Choose mode:",
          choices = c("Maximization" = "max", "Minimization" = "min"),
          selected = "max"
        ),
        numericInput(
          inputId = "simplex_constraints",
          label = "Number of constraints (excluding nonnegativity constraints)",
          value = 1,
          min = 1
        ),
        numericInput(
          inputId = "simplex_variables",
          label = "Number of variables",
          value = 1,
          min = 1
        ),
        actionButton(
          inputId = "simplex_generate",
          label = "Generate",
        )
      ), # sideBarPanel
      mainPanel(
        uiOutput(outputId = "simplex_table"),
        htmlOutput(outputId = "simplex_pos_const"),
        uiOutput(outputId = "simplex_btn"),
        uiOutput(outputId = "simplex_divider"),
        htmlOutput(outputId = "simplex_warning"),
        htmlOutput(outputId = "simplex_result"),
        htmlOutput(outputId = "final_tableau_text"),
        tableOutput(outputId = "final_tableau"),
        htmlOutput(outputId = "basic_solution_text"),
        tableOutput(outputId = "basic_solution"),
        htmlOutput(outputId = "opt_val")
      ) # mainPanel
    ), # tabPanel
    tabPanel(
      title = h3("Shipping Costs Minimization"),
      sidebarPanel(
        HTML(paste("
            <p class=\"lead\"><b>Description:</b></p>
            <p>This is an implementation of the simplex method for minimization.</p>
            <hr/>
            <p class=\"lead\"><b>Instructions:</b></p>
            <p>Fill in the Supply column with the number of supplies available at each plant.</p>
            <p>Fill in the Demand row with the demands of every warehouse.</p>
            <p>The cell intersected by the Demand row and the Supply column must have <b class=\"text-info\">0</b> as its value.</p>
            <p>Fill in the rest of table with the shipping costs from the selected plant to the selected warehouse.</p>
            <p>Click the button to confirm and solve.</p>
            <hr/>
            <p class=\"lead\"><b>Legend:</b></p>
            <p><b>DEN, PHO, DAL</b> - Plants in Denver, Phoenix, and Dallas, respectively</p>
            <p><b>SAC, SL, ALB, CHI, NYC</b> - Warehouses in Sacramento, Salt Lake City, Albuquerque, Chicago, and New York City, respectively</p>
        ", sep = ""))
      ),
      mainPanel(
        tags$head( # To design the button
          tags$style(
            HTML("#problem {background-color: #4c9be8; font-weight: bold}")
          )
        ),
        p(strong("Input needed data:"), class="lead"),
        matrixInput(
          inputId = "shipping_data",
          inputClass = "numeric",
          value = matrix(
            nrow = 4,
            ncol = 6, 
            dimnames = list(c("Demand", "DEN", "PHO", "DAL"), c("Supply", "SAC", "SL", "ALB", "CHI", "NYC"))
          ),
          rows = list(
            extend = FALSE
          ),
          cols = list(
            names = TRUE
          )
        ), # matrixInput
        actionButton(
          inputId = "problem",
          label = "Confirm and solve"
        ),
        hr(),
        htmlOutput(outputId = "problem_result"),
        htmlOutput(outputId = "min_cost"),
        htmlOutput(outputId = "problem_result_text"),
        tableOutput(outputId = "shipping_num")
      ) # mainPanel
    ) # tabPanel
  ) # navbarPage
) # fluidPage