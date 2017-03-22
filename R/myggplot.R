#' Make a random plot
#'
#' This function creates a random histogram plot.
#'
#' @export
#' @param n numer of random values
#' @param screen screening scenario
myggplot <- function(n,
                    screen=c("randomScreen50to70",
                             "twoYearlyScreen50to70", "fourYearlyScreen50to70",
                             "screen50", "screen60", "screen70", "screenUptake",
                             "stockholm3_goteborg", "stockholm3_risk_stratified",
                             "goteborg", "risk_stratified", "mixed_screening",
                             "regular_screen", "single_screen"),
                    type = c("incidence.rate", "testing.rate",
                             "biopsy.rate", "metastasis.rate",
                             "pc.mortality.rate",
                             "allcause.mortality.rate")){

  ## input validation
  screen <- match.arg(screen)
  type <- match.arg(type)
  stopifnot(n <= 1e5)

    require(microsimulation)
    require(ggplot2)
    n = 1e4
    screen = "screenUptake"
    type = "testing.rate"
    sub("^\\.","",type, perl = TRUE)
    sim1 <- callFhcrc(n=n, screen=screen, mc.cores = 3)
    p1 <- ggplot(predict(sim1, group = "age", type = type),
                aes(x=age, y = rate)) +
        xlim(40, 90) + geom_line() +
        ylab(type) + xlab("Age (years)")
    print(p1)

  #return nothing
  invisible();
}
