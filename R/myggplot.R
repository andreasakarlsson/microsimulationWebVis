#' Make a random plot
#'
#' This function creates a random histogram plot.
#'
#' @export
#' @param n numer of random values
#' @param screen screening scenario
myggplot <- function(n, screen=c("randomScreen50to70",
               "twoYearlyScreen50to70", "fourYearlyScreen50to70",
               "screen50", "screen60", "screen70", "screenUptake",
               "stockholm3_goteborg", "stockholm3_risk_stratified",
               "goteborg", "risk_stratified", "mixed_screening",
               "regular_screen", "single_screen")){

  ## input validation
  screen <- match.arg(screen)
  stopifnot(n <= 1e5)

    library(plotly)
    p <- plot_ly(mtcars, x = ~mpg, y = ~disp, mode = "markers")
    htmlwidgets::saveWidget(p, "rates.html", selfcontained = TRUE)


    require(microsimulation)
    require(ggplot2)
    sim1 <- callFhcrc(n=n, screen=screen, mc.cores = 3)
    p1 <- ggplot(predict(sim1, group = "age", type="testing.rate"), aes(x=age, y=rate)) +
        xlim(40, 90) + geom_line() +
        ylab("PSA testing rate") + xlab("Age (years)")

        ## htmlwidgets::saveWidget(ggplotly(p1), "rates.html", selfcontained = FALSE)

    print(p1)

  #return nothing
  invisible();
}
