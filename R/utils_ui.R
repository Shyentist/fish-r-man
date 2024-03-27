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
          9,
          tags$p("References"),

          tags$ul(
            tags$li(
          "Software: Buonomo P. [2021]. fishRman: A Shiny R Dashboard improving Global Fishing Watch data availability. Journal of Open Source Software.",
          tags$a("https://doi.org/10.21105/joss.03467",
                 target = "_blank",
                 rel = "noreferrer noopener",
                 href = "https://doi.org/10.21105/joss.03467",
                 style = "color:#000000"
          )),
          tags$li(
          "Software: Buonomo P. [2023]. fishRman: The Fisheries Scientist's Toolbox. CRAN.",
          tags$a("https://doi.org/10.5281/zenodo.5582567",
                 target = "_blank",
                 rel = "noreferrer noopener",
                 href = "https://doi.org/10.5281/zenodo.5582567",
                 style = "color:#000000"
          )),
          tags$li(
          "Data: Global Fishing Watch. [2021].",
          tags$a("https://globalfishingwatch.org/",
                 target = "_blank",
                 rel = "noreferrer noopener",
                 href = "https://globalfishingwatch.org/",
                 style = "color:#000000"
          ))),
          tags$p("Contacts"),
          "E-mail:",
          tags$a("pasqualebuonomo@hotmail.it",
                 target = "_blank",
                 rel = "noreferrer noopener",
                 href = "mailto:pasqualebuonomo@hotmail.it",
                 style = "color:#000000"
          )
        )
      )
    )
  )
}

