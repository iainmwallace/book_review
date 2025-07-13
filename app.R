library(shiny)
library(httr)
library(jsonlite)
library(DT)

# Function to fetch book data from Google Books API
get_book_data <- function(isbn) {
  url <- paste0("https://www.googleapis.com/books/v1/volumes?q=isbn:", isbn)
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      if (data$totalItems > 0) {
        book_info <- data$items[1, ]$volumeInfo
        
        return(list(
          title = book_info$title %||% "Unknown",
          author = paste(book_info$authors, collapse = ", ") %||% "Unknown",
          description = book_info$description %||% "No description available",
          publisher = book_info$publisher %||% "Unknown",
          published_date = book_info$publishedDate %||% "Unknown",
          page_count = book_info$pageCount %||% NA,
          success = TRUE
        ))
      } else {
        return(list(success = FALSE, message = "No book found with this ISBN"))
      }
    } else {
      return(list(success = FALSE, message = "API request failed"))
    }
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

# UI
ui <- fluidPage(
  titlePanel("Book Review App"),
  
  # Add some custom CSS for read-only styling
  tags$head(
    tags$style(HTML("
      .readonly-text {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 4px;
        padding: 6px 12px;
        font-family: inherit;
        font-size: 14px;
        line-height: 1.42857143;
        color: #555;
        margin-bottom: 10px;
      }
      .readonly-label {
        font-weight: bold;
        margin-bottom: 5px;
        display: block;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Add Book Review"),
      
      textInput("isbn", "Enter ISBN:", 
                placeholder = "e.g., 9780132350884"),
      
      actionButton("fetch_book", "Fetch Book Info", 
                   class = "btn-primary"),
      
      br(), br(),
      
      # Display book information as formatted text instead of inputs
      conditionalPanel(
        condition = "output.book_loaded",
        div(
          span("Title:", class = "readonly-label"),
          div(textOutput("book_title_display"), class = "readonly-text"),
          
          span("Author:", class = "readonly-label"),
          div(textOutput("book_author_display"), class = "readonly-text"),
          
          span("Description:", class = "readonly-label"),
          div(textOutput("book_description_display"), class = "readonly-text", 
              style = "height: 80px; overflow-y: auto;")
        )
      ),
      
      br(),
      
      numericInput("rating", "Your Rating (1-5):", 
                   value = 5, min = 1, max = 5, step = 1),
      
      textAreaInput("review_text", "Your Review:", 
                    rows = 4, 
                    placeholder = "Write your review here..."),
      
      actionButton("submit_review", "Submit Review", 
                   class = "btn-success"),
      
      br(), br(),
      
      actionButton("clear_form", "Clear Form", 
                   class = "btn-warning")
    ),
    
    mainPanel(
      h3("Book Reviews"),
      
      conditionalPanel(
        condition = "output.has_reviews",
        DT::dataTableOutput("reviews_table")
      ),
      
      conditionalPanel(
        condition = "!output.has_reviews",
        div(
          h4("No reviews yet"),
          p("Add your first book review using the form on the left."),
          style = "text-align: center; color: #666; margin-top: 50px;"
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store reviews and book data
  reviews <- reactiveVal(data.frame(
    ISBN = character(),
    Title = character(),
    Author = character(),
    Rating = numeric(),
    Review = character(),
    Date = character(),
    stringsAsFactors = FALSE
  ))
  
  # Reactive values for book information
  book_info <- reactiveValues(
    title = "",
    author = "",
    description = "",
    loaded = FALSE
  )
  
  # Fetch book information
  observeEvent(input$fetch_book, {
    if (nchar(input$isbn) > 0) {
      showModal(modalDialog(
        title = "Fetching book information...",
        "Please wait while we retrieve the book details.",
        footer = NULL
      ))
      
      book_data <- get_book_data(input$isbn)
      
      removeModal()
      
      if (book_data$success) {
        book_info$title <- book_data$title
        book_info$author <- book_data$author
        book_info$description <- book_data$description
        book_info$loaded <- TRUE
        
        showNotification("Book information loaded successfully!", 
                        type = "default")
      } else {
        book_info$loaded <- FALSE
        showNotification(book_data$message, type = "error")
      }
    } else {
      showNotification("Please enter an ISBN", type = "warning")
    }
  })
  
  # Display book information
  output$book_title_display <- renderText({
    book_info$title
  })
  
  output$book_author_display <- renderText({
    book_info$author
  })
  
  output$book_description_display <- renderText({
    book_info$description
  })
  
  output$book_loaded <- reactive({
    book_info$loaded
  })
  outputOptions(output, "book_loaded", suspendWhenHidden = FALSE)
  
  # Submit review
  observeEvent(input$submit_review, {
    if (nchar(input$isbn) > 0 && 
        book_info$loaded && 
        nchar(input$review_text) > 0) {
      
      new_review <- data.frame(
        ISBN = input$isbn,
        Title = book_info$title,
        Author = book_info$author,
        Rating = input$rating,
        Review = input$review_text,
        Date = Sys.Date(),
        stringsAsFactors = FALSE
      )
      
      current_reviews <- reviews()
      updated_reviews <- rbind(current_reviews, new_review)
      reviews(updated_reviews)
      
      showNotification("Review submitted successfully!", type = "default")
      
      # Clear form
      updateTextInput(session, "isbn", value = "")
      updateTextAreaInput(session, "review_text", value = "")
      updateNumericInput(session, "rating", value = 5)
      book_info$loaded <- FALSE
      
    } else {
      showNotification("Please fetch book information and fill in your review", type = "warning")
    }
  })
  
  # Clear form
  observeEvent(input$clear_form, {
    updateTextInput(session, "isbn", value = "")
    updateTextAreaInput(session, "review_text", value = "")
    updateNumericInput(session, "rating", value = 5)
    book_info$loaded <- FALSE
  })
  
  # Render reviews table
  output$reviews_table <- DT::renderDataTable({
    DT::datatable(reviews(), 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Check if there are reviews
  output$has_reviews <- reactive({
    nrow(reviews()) > 0
  })
  outputOptions(output, "has_reviews", suspendWhenHidden = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)