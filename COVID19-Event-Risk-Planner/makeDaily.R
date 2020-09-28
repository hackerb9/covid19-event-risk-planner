get_token()

pcrit <- function(x) {
  0.01 / x
}

calc_risk <- function(I, n, pop) {
  p_I <- I / pop
  1 - (1 - p_I)**n
}

# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
roundUpNice <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {
  if (length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

`%nin%` <- Negate(`%in%`)
current_fh <- tail(list.files("states_current/", full.names = TRUE), 1)
current_time <- gsub(".csv", "", basename(current_fh))
dir.create(paste0('www/daily_risk_plots/', current_time))

daily_fh <- tail(list.files("states_daily/", full.names = TRUE), 1)
daily_time <- gsub(".csv", "", basename(daily_fh))
state_current <<- read.csv(current_fh, stringsAsFactors = F)
states <- state_current %>% filter(state %nin% c('AS', 'MP', 'VI', 'GU', 'PR', 'DC'))
states = states$state
cur_date <- gsub("-", "", Sys.Date())
past_date <- ymd(cur_date) - 14
states_historic <- read.csv(daily_fh, stringsAsFactors = F) %>%
    filter(ymd(date) == past_date) %>% 
    arrange(state)

state_pops <- read.delim("state_pops.tsv", header = T, sep = "\t", stringsAsFactors = F)
state_data <- state_current %>%
    select(state, positive) %>%
    arrange(state)
state_data$C_i <- state_data$positive - states_historic$positive


xblock <- c(10, 50, 100, 1000, 10000)
state <- "GA"
rl = list()
ci_list = list()
for (state in states){
    pop <- as.numeric(state_pops[state_pops$state == state, "pop"])
    C_i <- as.numeric(state_data[state_data$state == state, "C_i"])
    ci_list[[state]] = data.frame("state" = state, "ci" = C_i/pop, "realci" = C_i)
    nvec <- c(C_i, 5 * C_i, 10 * C_i, 20 * C_i)
    event_size = c(10, 100, 1000, 1000)
    risk <- calc_risk(nvec, event_size, pop)

    n <- logspace(0, 6, 100)
    pcrit_val <- pcrit(n)
    numcrit <- pcrit_val * pop
    sizevec <- xblock
    risk_vals <- c(0.01, 0.02, 0.1, 0.5, 0.9)
    
    risk_vals_list <- list()
    sizes = logspace(0, 6, 50)
        for (i in 1:length(nvec)) {
            p_equiv <- nvec[i] / pop
            risk_vals_I <- round(100 * (1 - (1 - p_equiv)**sizes), 2)
            risk_vals_list[[i]] <- data.frame("nvec" = nvec[i], "svec" = sizes, "risk" = risk_vals_I, "state" = state)
        }
        
        risk.df <- do.call(rbind.data.frame, risk_vals_list)
              
        risk.df$nvec <- factor(risk.df$nvec, levels=c(C_i, 5 * C_i, 10 * C_i, 20 * C_i))
    
          
    rl[[state]] = risk.df
    
    
}

ci = do.call(rbind.data.frame, ci_list)
ci$rank = rank(ci$ci)

ci = ci[order(ci$rank),]






