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
        ),
        tags$li(
          tags$b(
            tags$a(

              "OSMOS's website",
              target = "_blank",
              rel = "noreferrer noopener",
              href = "https://osmos.xyz/"
            )
          ),
          ", also available at the bottom right corner of the page, under 'Sponsor'. Open-Source for Marine and Ocean Sciences (OSMOS) is our research group. If you like our projects, and would like a more interactive role, consider joining our",
          tags$b(
            tags$a(

              "Discord server",
              target = "_blank",
              rel = "noreferrer noopener",
              href = "https://discord.com/invite/W2unKxKbp7"
            )
          ),
          "or",
          tags$b(
            tags$a(

              "donating.",
              target = "_blank",
              rel = "noreferrer noopener",
              href = "https://www.buymeacoffee.com/osmos"
            )
          )
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

# this is a function to count the length of a vector necessary until it reaches an amount
# used for cumulative distribution
## source: https://stackoverflow.com/questions/8540143/add-consecutive-elements-of-a-vector-until-a-value

length.until <- function(x, max = 10) {
  s <- 0
  len <- length(x) # modified this to avoid function looping forever in case of bad
  # rounding or similar minor issues. This way, length is, at most, equal to the entire dataset
  start <- 1
  j <- 1
  for (i in seq_along(x)) {
    s <- s + x[i]
    while (s >= max) {
      if (i - j + 1 < len) {
        len <- i - j + 1
        start <- j
      }
      s <- s - x[j]
      j <- j + 1
    }
  }

  # commented the list below because we only need len
  # list(start = start, length = len)

  len
}
