Sys.setenv(PATH = with_path('/projects/covid19/bin', Sys.getenv("PATH")))

pcrit <- function(x) {
  0.01 / x
}

calc_risk <- function(I, n, USpop) {
  p_I <- (I / USpop) * (10.0/14.0)
  r <- 1 - (1 - p_I)**n
  round(100 * r, 1)
}

roundUpNice <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {
  if (length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

get_data <- function() {
  current_fh <- tail(list.files("states_current/", full.names = TRUE), 1)
  current_time <<- gsub(".csv", "", basename(current_fh))
  daily_fh <- tail(list.files("states_daily/", full.names = TRUE), 1)
  daily_time <<- gsub(".csv", "", basename(daily_fh))
  state_current <<- read.csv(current_fh, stringsAsFactors = F)
  states <<- state_current$state
  cur_date <- gsub("-", "", Sys.Date())
  past_date <- ymd(cur_date) - 14
  states_historic <<- read.csv(daily_fh, stringsAsFactors = F)
  states_historic <<- subset(states_historic, ymd(date) == past_date) %>% arrange(state)
  state_pops <<- read.delim("state_pops.tsv", header = T, sep = "\t", stringsAsFactors = F)
  state_data <<- state_current %>%
    select(state, positive) %>%
    arrange(state)
  state_data$C_i <<- state_data$positive - states_historic$positive
}


# maps <- readRDS("daily_risk_map/riskmaps.rds")
# county_geo = read.csv('map_data/ctcenter.csv', stringsAsFactors=F)



  pred_plot <- ""
  output$plot_us <- renderPlot({
    xblock <- c(10, 100, 1000, 10**4, 10**5)
    yblock <- c(10, 100, 1000, 10000, 10**5, 4 * 10**5, 10**6, 2 * 10**6, 8 * 10**6)

    names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game")
    names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million")
    if (8 * 10**6 < values_pred$event_size) {
      yblock <- yblock + c(values_pred$event_size)
      names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million", format(values_pred$event_size, big.mark = ","))
    }
    use_state <- values_pred$use_state
    state <- values_pred$state
    if (use_state) {
      USpop <- as.numeric(state_pops[state_pops$state == state, "pop"])
      xblock <- c(10, 100, 1000, 10**4, 10**5)
      nvec <- round(c(.01 * USpop, .05 * USpop, .25 * USpop))
      yblock <- sapply(c(10, 100, 1000, .05 * USpop, .1 * USpop, .25 * USpop, USpop), roundUpNice)
      names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game")
      names(yblock) <- format(sapply(c(10, 100, 1000, .05 * USpop, .1 * USpop, .25 * USpop, USpop), roundUpNice), big.mark = ",")
      ylimits <- c(10, max(yblock, 10 * 10^ceiling(log10(max(yblock)))))
    } else {
      USpop <- 330 * 10^6
      stata <- "US"
      nvec <- c(8000000, 400000, 2000000)
      xblock <- c(10, 100, 1000, 10**4, 10**5)
      yblock <- c(10, 100, 1000, 10000, 10**5, 4 * 10**5, 10**6, 2 * 10**6, 8 * 10**6)
      names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game")
      names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million")
      ylimits <- c(10**4, 3 * 10**7)
    }

    # cat(state, "\t", USpop, "\n")
    n <- logspace(0, 6, 100)
    pcrit_val <- pcrit(n)
    numcrit <- pcrit_val * USpop
    sizevec <- c(10, 100, 1000, 10000, 100000, 10**7)
    risk_vals <- c(0.01, 0.02, 0.1, 0.5, 0.9)
    pcrit_risk_list <- list()
    for (i in 1:length(risk_vals)) {
      pcrit_risk <- 1 - (1 - risk_vals[i])**(1 / n)
      pcrit_risk <- pcrit_risk * USpop
      pcrit_risk_list[[i]] <- data.frame("risk" = risk_vals[i], "y" = pcrit_risk, "x" = n)
    }

    ytarget <- 100000
    pcrit_label <- ytarget / USpop
    pcrit_lab_list <- list()
    for (i in 1:length(risk_vals)) {
      nlabel <- log(1 - risk_vals[i]) / log(1 - pcrit_label)
      pcrit_lab_list[[i]] <- data.frame("risk" = risk_vals[i], "x" = nlabel, y = ytarget * 1.4)
    }


    risk_vals_list <- list()
    for (i in 1:length(nvec)) {
      p_equiv <- nvec[i] / USpop
      risk_vals_I <- round(100 * (1 - (1 - p_equiv)**sizevec), 2)
      risk_vals_list[[i]] <- data.frame("nvec" = nvec[i], "svec" = sizevec, "risk" = risk_vals_I)
    }

    pcrit.df <- do.call(rbind.data.frame, pcrit_risk_list)
    pcrit_lab.df <- do.call(rbind.data.frame, pcrit_lab_list)
    risk.df <- do.call(rbind.data.frame, risk_vals_list)

    infect <- values_pred$infect
    event_size <- values_pred$event_size
    shiny::validate(
      need(is.numeric(event_size), "Event size must be a number"),
      need(event_size >= 5, "Event size must be >=5"),
      need(event_size <= 100000, "Event size must be <= 100,000")
    )
    shiny::validate(
      need(is.numeric(infect), "Number of active cases must be a number"),
      need(infect >= 10, "Number of active cases must be >=10"),
      need(infect < 0.5 * USpop, paste("Number of active cases must less than 10% of population <", round(USpop * .5)))
    )
    risk <- calc_risk(infect, event_size, USpop)
    risk <- case_when(risk < .1 ~ "<0.1", risk > 99 ~ ">99", TRUE ~ as.character(risk))

    pred_plot <<- ggplot() +
      geom_area(data = pcrit_risk_list[[1]], aes(x = x, y = y), alpha = .5) +
      # geom_text(data = pcrit_lab.df, aes(x=x, y = y, label=paste(risk * 100, "% Chance")), angle=angle, size=6) +
      geom_hline(yintercept = risk.df$nvec, linetype = 2) +
      geom_path(data = pcrit.df, aes(x = x, y = y, group = risk, color = as.factor(100 * risk)), size = 1) +
      scale_color_manual(values = c("black", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")) +
      # geom_segment(data=pcrit.df, aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
      geom_label(data = risk.df, aes(x = svec, y = nvec, label = paste(risk, "% Chance")), nudge_y = .1, size = 5, fill = "blue", alpha = .5, color = "white") +
      geom_vline(xintercept = event_size, linetype = 3) +
      geom_hline(yintercept = infect, linetype = 3) +
      geom_point(aes(x = event_size, y = infect), size = 4, color = "red") +
      geom_point(data = risk.df, aes(x = svec, y = nvec), size = 3) +
      geom_label_repel(aes(x = event_size, y = infect, label = paste(risk, "% chance an attendee\n has COVID-19.")), size = 5) +
      # geom_polygon(aes(x=c(0, 0, 100), y=c(pcrit.df[1,]$ystart, 0, 0), group=c(1,1,1)), fill="grey", alpha = 0.5) +
      theme_clean() +
      # coord_cartesianxlim(1, 10**5) + ylim(ylimits)
      scale_x_continuous(name = "Number of people at event", breaks = xblock, labels = names(xblock), trans = "log10", expand = c(.1, .1)) +
      scale_y_continuous(name = paste0("Number of circulating cases in ", state), breaks = yblock, labels = names(yblock), trans = "log10", expand = c(.1, .1)) +
      annotation_logticks(scaled = T) +
      # # geom_vline(xintercept = 10**5, linetype=2) +
      coord_cartesian(ylim = ylimits, xlim = c(10, 100001)) +
      theme(
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        plot.caption = element_text(hjust = 0, face = "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5)
      ) +
      guides(color = guide_legend(title = "% Chance"), override.aes = list(size = 2), label.position = "bottom") +
      labs(
        caption = paste0("© CC-BY-4.0\tChande, A.T., Gussler, W., Harris, M., Lee, S., Rishishwar, L., Jordan, I.K., Andris, C.M., and Weitz, J.S. 'Interactive COVID-19 Event Risk Assessment Planning Tool'\nhttp://covid19risk.biosci.gatech.edu\nRisk estimates made:  ", today(), "\nReal-time COVID19 data comes from the COVID Tracking Project: https://covidtracking.com/api/\nUS 2019 population estimate data comes from the US Census: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html"),
        title = paste0("COVID-19 Event Risk Assessment Planner - ", state, " - Exploratory"),
        subtitle = "Estimated chance that one or more individuals are COVID-19 positive at an event\ngiven event size (x-axis) and current case prevalence (y-axis)"
      )
    pred_plot
  })
  dd_inputs <- reactive({
    list(input$states_dd, input$event_dd, input$use_state_dd)
  })
  #
  dd_plot <- ""
  states_dd <- "US"
  observeEvent(dd_inputs(), {
    xblock <- c(10, 100, 1000, 10**4, 10**5)
    names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game")
    # cat("218 ", values_dd$use_state, "\n")
    use_state <- input$use_state_dd
    state <- input$states_dd
    states_dd <<- state
    # cat("220\t", state, "\t",use_state, "\n")
    if (use_state) {
      USpop <- as.numeric(state_pops[state_pops$state == state, "pop"])
      # cat(USpop)
      pcrit_label_x <- c(-9, -20, -200, -2000, -7000)
      C_i <- as.numeric(state_data[state_data$state == state, "C_i"])
      yblock <- c(10, 100, 1000, C_i, 5 * C_i, 10 * C_i, 10 * 10^ceiling(log10(10 * C_i)))
      names(yblock) <- c("10", "100", "1,000", format(c(C_i, 5 * C_i, 10 * C_i, 10 * 10^ceiling(log10(10 * C_i))), big.mark = ","))
      ylimits <- c(10, max(yblock))
    } else {
      states_dd <<- "US"
      USpop <- 330 * 10^6
      pcrit_label_x <- c(9, 20, 200, 2000, 7000)
      C_i <- sum(as.numeric(state_data$C_i))
      yblock <- c(10, 100, 1000, 10000, 10**5, 4 * 10**5, 10**6, 2 * 10**6, 8 * 10**6)
      names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million")
      ylimits <- c(10**4, 3 * 10**7)
    }
    nvec <- c(C_i, 5 * C_i, 10 * C_i)
    event_size <- as.numeric(gsub("[ ,-]", "", isolate(input$event_dd)))
    risk <- calc_risk(nvec, event_size, USpop)
    risk <- case_when(risk < .1 ~ "<0.1", risk > 99 ~ ">99", TRUE ~ as.character(risk))


    output$dd_text <- renderUI({
      HTML(paste0(
        "<p style='font-size: 18px;'><br/><strong>C<sub>I</sub> = Current reported incidence</strong><br/>Chance someone is COVID19 positive at C<sub>I</sub>  (", format(nvec[1], big.mark = ","), "): ", risk[1], "%<br/>",
        "Chance someone is COVID19 positive at 5x C<sub>I</sub> (", format(nvec[2], big.mark = ","), "): ", risk[2], "%<br/>",
        "Chance someone is COVID19 positive at 10x C<sub>I</sub> (", format(nvec[3], big.mark = ","), "): ", risk[3], "%</p>"
      ))
    })

    output$dd_current_data <- renderUI({
      HTML(
        paste0(
          "Real-time data last updated at: ", ymd_hms(current_time, tz = ""),
          "<br/>Historic data last updated at: ", ymd_hms(daily_time, tz = "")
        )
      )
    })

    output$plot_dd <- renderPlot({
      req(input$states_dd)
      req(input$event_dd)
      req(USpop)
      # cat("state: ", state, "\tpop: ", USpop, "\n")
      n <- logspace(0, 6, 100)
      pcrit_val <- pcrit(n)
      numcrit <- pcrit_val * USpop
      sizevec <- c(1, 10, 100, 1000, 10000, 100000, 10**7)
      risk_vals <- c(0.01, 0.02, 0.1, 0.5, 0.9)
      pcrit_risk_list <- list()
      for (i in 1:length(risk_vals)) {
        pcrit_risk <- 1 - (1 - risk_vals[i])**(1 / n)
        pcrit_risk <- pcrit_risk * USpop
        pcrit_risk[is.infinite(pcrit_risk)] <- USpop
        pcrit_risk_list[[i]] <- data.frame("risk" = risk_vals[i], "y" = pcrit_risk, "x" = n)
      }
      ytarget <- 100000
      # cat("USpop\t", USpop, "\n")
      pcrit_label <- ytarget / USpop
      pcrit_lab_list <- list()
      for (i in 1:length(risk_vals)) {
        # cat("rv\t", risk_vals[i],"pc\t", pcrit_label, "\n")
        nlabel <- log(1 - risk_vals[i]) / log(1 - pcrit_label)
        pcrit_lab_list[[i]] <- data.frame("risk" = risk_vals[i], "x" = nlabel, y = ytarget * 1.4)
      }

      risk_vals_list <- list()
      # cat("before risk_vals\n")
      for (i in 1:length(nvec)) {
        p_equiv <- nvec[i] / USpop
        risk_vals_I <- round(100 * (1 - (1 - p_equiv)**sizevec), 2)
        risk_vals_list[[i]] <- data.frame("nvec" = nvec[i], "svec" = sizevec, "risk" = risk_vals_I)
      }
      # cat("after risk_vals\n")



        mutate(risk = case_when(
          risk > 99 ~ ">99",
          risk <= 0.1 ~ "<0.1",
          TRUE ~ as.character(risk)
        ))

