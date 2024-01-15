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
        "is an R package and dashboard to help you explore and analyse",
        tags$b("Global Fishing Watch Data."),
        "Learn more about the software, the data, and the people behind them via:"
      ),
      tags$br(),
      tags$br(),
      tags$ul(
        tags$li(
          "The official documentation, which includes the paper published on the ",
          tags$b(
            tags$a(
              "Journal of Open Source Software",
              target = "_blank",
              rel = "noreferrer noopener",
              href = "https://doi.org/10.21105/joss.03467"
            )
          ),
          "and the R package manual published on ",
          tags$b(
            tags$a(
              "CRAN",
              target = "_blank",
              rel = "noreferrer noopener",
              href = "https://cran.r-project.org/web/packages/fishRman/fishRman.pdf"
            )
          ),
          ", both available at the top of the page under 'Docs', together with fishRman's instructions for use, the ",
          tags$b("Handbook"),
          ";"
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
          ", also available at the bottom right corner of the page, under 'Sponsor'. Open-Source for Marine and Ocean Sciences (OSMOS) is our research group. If you like our projects, consider joining our",
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
      )
    )
  )}


