#' ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

footer <- function() {
  fluidRow(
    column(
      12,
      tags$div(
        class = "footer",
        column(
          3,
          tags$p("Software"),
          tags$a(
            target = "_blank",
            rel = "noreferrer noopener",
            tags$img(
              src = "www/img/fishrman_banner.png",
              height = "auto",
              width = "100%"
            ),
            href = "https://github.com/Shyentist/fish-r-man"
          )
        ),
        column(
          6,
          tags$p("References"),
          "Software: Buonomo P. [2021]. fishRman: A Shiny R Dashboard improving Global Fishing Watch data availability. Journal of Open Source Software.",
          tags$a("https://doi.org/10.21105/joss.03467",
                 target = "_blank",
                 rel = "noreferrer noopener",
                 href = "https://doi.org/10.21105/joss.03467",
                 style = "color:#000000"
          ),
          tags$br(),
          tags$br(),
          "Data: Global Fishing Watch. [2021].",
          tags$a("https://globalfishingwatch.org/",
                 target = "_blank",
                 rel = "noreferrer noopener",
                 href = "https://globalfishingwatch.org/",
                 style = "color:#000000"
          ),
          tags$br(),
          tags$br(),
          tags$p("Contacts"),
          "E-mail:",
          tags$a("pasqualebuonomo@hotmail.it",
                 target = "_blank",
                 rel = "noreferrer noopener",
                 href = "mailto:pasqualebuonomo@hotmail.it",
                 style = "color:#000000"
          )
        ),
        column(
          3,
          tags$p("Sponsor"),
          tags$a(
            target = "_blank",
            rel = "noreferrer noopener",
            tags$img(
              src = "www/img/OSMOS_logo.png",
              height = "auto",
              width = "100%"
            ),
            href = "https://osmos.xyz/"
          )
        )
      )
    )
  )
}

