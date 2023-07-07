# Source solver functions
source("src/qsi.R")
source("src/simplex.R")

# Define server function  
server <- function(input, output, session) {
  # Show example for qsi
  observeEvent(
    input$qsi_example, {
      updateTextAreaInput(
        session = session,
        inputId = "qsi_data",
        value = "1.6 2\n2 8\n2.5 14"
      )
      
      updateTextInput(
        session = session,
        inputId = "qsi_x",
        value = "2.2"
      )
    }
  )
  
  # Solve qsi
  observeEvent(
    input$qsi, {
      # To clear all the (past) outputs
      output$qsi_warning <- NULL
      output$qsi_result <- NULL
      output$qsi_fxns_text <- NULL
      output$qsi_fxns <- NULL
      output$qsi_y_text <- NULL
      output$qsi_y <- NULL
      
      suppressWarnings({
        qsi.x <- as.numeric(input$qsi_x)
        if (is.na(qsi.x)) { # To check if user inputted a non-numeric value
          output$qsi_warning <- renderUI({
            HTML(paste("<br/><p>Please follow the correct syntax.</p>"))
          })
        } else {
          data <- GetValues(input$qsi_data)
          if (any(is.na(data))) {
            output$qsi_warning <- renderUI({
              HTML(paste("<br/><p>Please follow the correct syntax.</p>"))
            })
          } else {
            result <- poly.qsi(data, qsi.x)
            if (any(is.na(result))) {
              output$qsi_warning <- renderUI({
                HTML(paste("<br/><p>Please recheck your inputs.</p>"))
              })
            } else {
              output$qsi_result <- renderUI({
                HTML(paste("<p class=\"lead\"><b>Result:</b></p>"))
              })
              
              output$qsi_fxns_text <- renderUI({
                HTML(paste("<h4>The given set of data points produce the following set of polynomial functions:</h4>"))
              })
              
              sorted.x <- sort(data$x)
              
              output$qsi_fxns <- renderPrint({
                for (i in 1:length(result$qsi.fxns)) {
                  f <- unlist(deparse(result$qsi.fxns[[i]]))
                  cat(paste(paste(f, collapse = ""), ",", sep = ""), sorted.x[i], "<= x <=", sorted.x[i + 1], "\n")
                }
              })
              
              output$qsi_y_text <- renderUI({
                HTML(paste("<br/><h4>The estimated value:</h4>"))
              })
              
              output$qsi_y <- renderUI({
                HTML(paste("<h5><b>f(", qsi.x, ") = ", result$y, "</b></h5>", sep = ""))
              })
            }
          }
        }
      })
    }
  )
  
  # Show example for simplex
  observeEvent(
    input$simplex_example, {
      # To clear all (past) outputs
      output$simplex_divider <- NULL
      output$simplex_warning <- NULL
      output$simplex_result <- NULL
      output$final_tableau_text <- NULL
      output$final_tableau <- NULL
      output$basic_solution_text <- NULL
      output$basic_solution <- NULL
      output$opt_val <- NULL
      
      updateSelectInput(
        session = session,
        inputId = "simplex_mode",
        selected = "max"
      )
      
      updateNumericInput(
        session = session,
        inputId = "simplex_constraints",
        value = 4
      )
      
      updateNumericInput(
        session = session,
        inputId = "simplex_variables",
        value = 2
      )
      
      output$simplex_table <- renderUI({
        matrixInput(
          inputId = "simplex_data",
          inputClass = "numeric",
          value = matrix(
            data = c(150, 175, "=", 1,
                     7, 11, "<=", 77,
                     10, 8, "<=", 80,
                     1, 0, "<=", 9,
                     0, 1, "<=", 6),
            byrow = TRUE,
            nrow = 5,
            ncol = 4,
            dimnames = list(NULL, c("x1", "x2", "<=, =>, or =", ""))
          ),
          rows = list(
            extend = FALSE,
            names = FALSE
          ),
          cols = list(
            names = TRUE
          )
        )
      })
      
      output$simplex_pos_const <- renderUI({
        HTML(paste("<p>and ", paste(c("x1", "x2"), collapse = ", "), " >= 0</p>", sep = ""))
      })
      
      output$simplex_btn <- renderUI({
        actionButton(
          inputId = "simplex",
          label = "Solve"
        )
      })
    }
  )
  
  # Generate simplex table
  observeEvent(
    input$simplex_generate, {
      # To clear all (past) outputs
      output$simplex_divider <- NULL
      output$simplex_warning <- NULL
      output$simplex_result <- NULL
      output$final_tableau_text <- NULL
      output$final_tableau <- NULL
      output$basic_solution_text <- NULL
      output$basic_solution <- NULL
      output$opt_val <- NULL
      
      constraints.num <- input$simplex_constraints
      variables.num <- input$simplex_variables
      cols <- paste("x", 1:variables.num, sep = "")
      
      output$simplex_table <- renderUI({
        matrixInput(
          inputId = "simplex_data",
          inputClass = "numeric",
          value = matrix(
            nrow = constraints.num + 1,
            ncol = variables.num + 2,
            dimnames = list(NULL, c(cols, "<=, =>, or =", ""))
          ),
          rows = list(
            extend = FALSE,
            names = FALSE
          ),
          cols = list(
            names = TRUE
          )
        )
      })
      
      output$simplex_pos_const <- renderUI({
        HTML(paste("<p>and ", paste(cols, collapse = ", "), " >= 0</p>", sep = ""))
      })
      
      output$simplex_btn <- renderUI({
        actionButton(
          inputId = "simplex",
          label = "Solve"
        )
      })
    }
  )
  
  # Solve simplex
  observeEvent(
    input$simplex, {
      # To clear all (past) outputs
      output$simplex_divider <- NULL
      output$simplex_warning <- NULL
      output$simplex_result <- NULL
      output$final_tableau_text <- NULL
      output$final_tableau <- NULL
      output$basic_solution_text <- NULL
      output$basic_solution <- NULL
      output$opt_val <- NULL
      
      suppressWarnings({
        r <- nrow(input$simplex_data)
        c <- ncol(input$simplex_data)
        z.coeffs <- as.numeric(input$simplex_data[1, 1:(c - 2)])
        z.rhs <- as.numeric(input$simplex_data[1, c])
        const.coeffs <- as.numeric(input$simplex_data[2:r, 1:(c - 2)])
        const.rhs <- as.numeric(input$simplex_data[2:r, c])
        const.type <- input$simplex_data[2:r, c - 1]
        mode <- input$simplex_mode
        
        if ((mode == "max" && !all(const.type == "<=")) || (mode == "min" && !all(const.type == ">=")) || input$simplex_data[1, c - 1] != "=" 
            ||any(is.na(z.coeffs)) || any(is.na(z.rhs)) || any(is.na(const.coeffs)) || any(is.na(const.rhs)) || input$simplex_data[1, c] != 1) {
          output$simplex_warning <- renderUI({
            HTML(paste("<br/><p>Please follow the instructions.</p>"))
          })
        } else {
          output$simplex_divider <- renderUI({
            hr()
          })
          
          if (mode == "max") {
            result <- simplex(GenericInitialTableau(r, c, z.coeffs, z.rhs, const.coeffs, const.rhs, mode), TRUE, FALSE)
          } else {
            result <- simplex(GenericInitialTableau(r, c, z.coeffs, z.rhs, const.coeffs, const.rhs, mode), FALSE, FALSE)
          }
          if (any(result == "No feasible solution")) {
            output$simplex_result <- renderUI({
              HTML(paste("<p class = \"lead\"><b>Result:</b></p><p>There is no feasible solution.</p>", sep=""))
            })
          } else {
            output$simplex_result <- renderUI({
              HTML(paste("<p class = \"lead\"><b>Result:</b></p>", sep=""))
            })
            
            output$final_tableau_text <- renderUI({
              HTML(paste("<p>The final tableau:</p>", sep=""))
            })
            
            output$final_tableau <- renderTable(
              result$final.tableau,
              align = "c",
              rownames = FALSE,
              colnames = TRUE,
              digits = 4
            )
            
            output$basic_solution_text <- renderUI({
              HTML(paste("<p>The final solution:</p>", sep=""))
            })
            
            output$basic_solution <- renderTable(
              result$basic.solution,
              align = "c",
              rownames = TRUE,
              colnames = TRUE,
              digits = 4
            )
            
            output$opt_val <- renderUI({
              HTML(paste("<p>The optimum value is <b>", result$opt.val,"</b>.</p>", sep=""))
            })
          }
        }
      })
    }
  )
  
  # Solve shipping costs minimization problem
  observeEvent(
    input$problem, {
      # To clear all the (past) outputs
      output$problem_result <- NULL
      output$min_cost <- NULL
      output$problem_result_text <- NULL
      output$shipping_num <- NULL
      
      if(any(is.na(input$shipping_data)) || input$shipping_data[1, 1] != "0") {
        output$problem_result_text <- renderUI({
          HTML(paste("<p>Please follow the instructions.</p>"))
        })
      } else {
        suppressWarnings({
          temp.matrix <- matrix(
            data = as.numeric(input$shipping_data),
            nrow = 4,
            ncol = 6,
            dimnames = list(c("Demand", "DEN", "PHO", "DAL"), c("Supply", "SAC", "SL", "ALB", "CHI", "NYC"))
          )
          
          if (any(is.na(temp.matrix))) { # To check if user inputted non-numeric values
            output$problem_result_text <- renderUI({
              HTML(paste("<p>Please follow the instructions.</p>"))
            })
          } else {
            temp.matrix[1, 1] <- NA
            result <- simplex(ProblemInitialTableau(temp.matrix), FALSE, TRUE)
            
            output$problem_result <- renderUI({
              HTML(paste("<p class=\"lead\"><b>Result:</b></p>"))
            })
            
            if (any(result == "No feasible solution")) {
              output$problem_result_text <- renderUI({
                HTML(paste("<p>There is no feasible solution.</p>"))
              })
            } else {
              output$min_cost <- renderUI({
                HTML(paste("<p>The minimum cost is<b> $", result$opt.val,"</b>.</p>", sep=""))
              })
              
              output$problem_result_text <- renderUI({
                HTML(paste("<p>The number of items to ship from plant to warehouse:</p>"))
              })
              
              output$shipping_num <- renderTable(
                result$shipping.num,
                align = "c",
                rownames = TRUE,
                colnames = TRUE,
                digits = 0
              )
            }
          }
        })
      }
    }
  )
}