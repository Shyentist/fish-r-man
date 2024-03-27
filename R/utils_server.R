#' server
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# quick description and all important links should go here

intro.message <- function () {

  showModal(
    modalDialog(
      size = "l",
      title = "Welcome to fishRman",
      tags$span(
        tags$b("fishRman"),
        "is a dashboard to help you explore and analyse",
        tags$b("Global Fishing Watch Data."),
        "Learn more about the software, the data, and the people behind them via:"
      ),
      tags$br(),
      tags$br(),
      tags$ul(
        tags$li(
          "fishRman's ",
          tags$b(
            tags$a(

              "paper",
              target = "_blank",
              rel = "noreferrer noopener",
              href = "https://doi.org/10.21105/joss.03467"
            )
          ),
          ", also available at the bottom of the page, under 'References';"
        ),
        tags$li(
          "fishRman's instructions for use, the ",
          tags$b(
            tags$a(

              "Handbook",
              target = "_blank",
              rel = "noreferrer noopener",
              href = "https://raw.githubusercontent.com/Shyentist/fish-r-man/main/www/doc/Handbook.pdf"
            )
          ),
          ", also available at the top of the page;"
        ),
        tags$li(
          "fishRman's ",
          tags$b(
            tags$a(

              "GitHub repository",
              target = "_blank",
              rel = "noreferrer noopener",
              href = "https://github.com/Shyentist/fish-r-man"
            )
          ),
          ", also available at the bottom left corner of the page, clicking the logo;"
        ),
        tags$li(
          tags$b(
            tags$a(

              "Global Fishing Watch's website",
              target = "_blank",
              rel = "noreferrer noopener",
              href = "https://globalfishingwatch.org/"
            )
          ),
          ", also available at the bottom of the page, under 'References';"
        )
      ),
      tags$br(),
      tags$b("How to quote (References)"),
      tags$br(),
      tags$br(),
      tags$ul(
        tags$li(
          "Software: Buonomo P. [2021]. fishRman: A Shiny R Dashboard improving Global Fishing Watch data availability. Journal of Open Source Software.",
          tags$b(
            tags$a("https://doi.org/10.21105/joss.03467",
                   target = "_blank",
                   rel = "noreferrer noopener",
                   href = "https://doi.org/10.21105/joss.03467"
            )
          )
        ),
        tags$li(
          "Software: Buonomo P. [2023]. fishRman: The Fisheries Scientist's Toolbox. CRAN.",
          tags$b(
            tags$a("https://doi.org/10.5281/zenodo.5582567",
                   target = "_blank",
                   rel = "noreferrer noopener",
                   href = "https://doi.org/10.5281/zenodo.5582567"
            )
          )
        ),
        tags$li(
          "Data: Global Fishing Watch. [2021].",
          tags$b(
            tags$a("https://globalfishingwatch.org/",
                   target = "_blank",
                   rel = "noreferrer noopener",
                   href = "https://globalfishingwatch.org/"
            )
          )
        )
      )
    )
  )}


