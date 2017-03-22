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
                    type = c("prevalence", "incidence.rate", "testing.rate",
                             "biopsy.rate", "metastasis.rate",
                             "pc.mortality.rate",
                             "allcause.mortality.rate"),
                    time = c("age", "year")){

  ## input validation
  screen <- match.arg(screen)
  type <- match.arg(type)
  time <- match.arg(time)
  stopifnot(n <= 1e5)

    require(microsimulation)
    require(ggplot2)

    n = 1e4
    screen = "screenUptake"
    ## type = "testing.rate"
    ## type = "prevalence"
    ## type = "pc.mortality.rate"
    sim1 <- callFhcrc(n=n, screen=screen, mc.cores = 3)

    ## TODO
    ## Should predict() be nested or passed as _df_?
    ## 0. Fix so testing.rate under noScreening returns zeros and not an error
    ## 1. Force append a time scale to group
    ## 2. Suitable x-limits depending on time scale
    ## 3. Add group option
    ## 4. Allow for rate ratios, see the multi-option list
    ## 5. Cache simulations in package
    ## 6. Use plotly to make plots nicer
    ## 7. Use D3 tree graph e.g. for life-histories

    ## Fix this (doesn't work'):
    xaxis <- function(time){
        switch(time,
               age = ggplot() + xlim(40, 90) + xlab("Age (years)"),
               year = ggplot() + xlim(1990, 2020) + xlab("Calendar period (years)"))
    }

    makeplot <- function(sim, type, x, y) {
        switch(x,
               age = ggplot(predict(sim, group = x, type = type),
                            aes_string(x = x, y = y)) +
                   geom_line() +
                   ylab(type) +
                   theme_bw() +
                   xlim(40, 90) +
                   xlab("Age (years)"),
               year = ggplot(predict(sim, group = x, type = type),
                             aes_string(x = x, y = y)) +
                   geom_line() +
                   ylab(type) +
                   theme_bw() +
                   xlim(1990, 2020) +
                   xlab("Calendar period (years)"))
    }


  print(makeplot(sim1, type, x=time, y=gsub("\\w*[[:punct:]]", "", type) ))

  #return nothing
  invisible();
}
