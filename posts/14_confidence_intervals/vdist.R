# school colors
princeton_orange <- "#E77500"
princeton_black  <- "#121212"

# binomial distribution
vbinom <- function(k_obs, n, p, labels = TRUE){
  # make data frame
  k_vals <- 0:n
  pk     <- dbinom(k_vals, n, p)
  k_bool <- k_vals %in% k_obs
  df_binom <- data.frame(k_vals, pk, k_bool)
  
  # compute requested probability
  answer_prob = round(sum(dbinom(k_obs, n, p)), 4)
  
  # define bar plot
  this_plot <- if(labels){
    df_binom |>
      ggplot(aes(x = factor(k_vals), y = pk, color = k_bool, fill = k_bool)) +
      geom_bar(stat = "identity") +
      geom_label(aes(x = factor(k_vals), y = pk, label = round(pk, 4)),
                 color = "black", fill = "white") +
      labs(subtitle = paste0("n = ", n, ", k = ", list(k_obs), ", p = ", p, ", P(k = ", list(k_obs), ") = ", answer_prob),
           caption = "SML 201",
           y = "probability") +
      scale_color_manual(values = c("gray70", "#121212")) +
      scale_fill_manual(values = c("gray70", "#E77500")) +
      theme(
        legend.position = "bottom",
        panel.background = element_blank()
      )
  } else{
    df_binom |>
      ggplot(aes(x = factor(k_vals), y = pk, color = k_bool, fill = k_bool)) +
      geom_bar(stat = "identity") +
      labs(subtitle = paste0("n = ", n, ", k = ", list(k_obs), ", p = ", p, ", P(k = ", list(k_obs), ") = ", answer_prob),
           caption = "SML 201",
           y = "probability") +
      scale_color_manual(values = c("gray70", "#121212")) +
      scale_fill_manual(values = c("gray70", "#E77500")) +
      theme(
        legend.position = "bottom",
        panel.background = element_blank()
      )
  }
  
  # plot bar chart
  this_plot
}

# normal distribution
vnorm <- function(x, mu = 0, sigma = 1, section = "lower"){
  
  # bell curve
  x_vals <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 201)
  y_vals <- dnorm(x_vals, mu, sigma)
  df_for_graph <- data.frame(x_vals, y_vals)
  
  # outline shaded regions
  if(length(x) == 1){
    shade_left <- rbind(c(x[1],0), df_for_graph |>
                          filter(x_vals < x[1]))
    shade_right <- rbind(c(x[1],0), df_for_graph |>
                           filter(x_vals > x[1]))
  }
  if(length(x) == 2){
    shade_between <- rbind(c(x[1],0),
                           df_for_graph |>
                             filter(x_vals > x[1] &
                                      x_vals < x[2]),
                           c(x[2],0))
    shade_tails <- rbind(df_for_graph |>
                           filter(x_vals < x[1]),
                         c(x[1],0),
                         c(x[2],0),
                         df_for_graph |>
                           filter(x_vals > x[2]))
  }
  if(section %in% c("less", "lower")){
    bell_curve <- df_for_graph |>
      ggplot(aes(x_vals, y_vals)) +
      geom_polygon(aes(x = x_vals, y = y_vals),
                   data = shade_left,
                   fill = "#E77500",) +
      geom_line(color = "gray50", linewidth = 2)
    prob_val <- round(pnorm(x,mu,sigma), 4)
  }
  if(section %in% c("greater", "upper")){
    bell_curve <- df_for_graph |>
      ggplot(aes(x_vals, y_vals)) +
      geom_polygon(aes(x = x_vals, y = y_vals),
                   data = shade_right,
                   fill = "#E77500",) +
      geom_line(color = "gray50", linewidth = 2)
    prob_val <- 1 - round(pnorm(x,mu,sigma), 4)
  }
  if(section == "between"){
    bell_curve <- df_for_graph |>
      ggplot(aes(x_vals, y_vals)) +
      geom_polygon(aes(x = x_vals, y = y_vals),
                   data = shade_between,
                   fill = "#E77500",) +
      geom_line(color = "gray50", linewidth = 2)
    prob_val <- round(diff(pnorm(x,mu,sigma)), 4)
  }
  if(section %in% c("tails", "two.sided", "two_sided")){
    bell_curve <- df_for_graph |>
      ggplot(aes(x_vals, y_vals)) +
      geom_polygon(aes(x = x_vals, y = y_vals),
                   data = shade_tails,
                   fill = "#E77500",) +
      geom_line(color = "gray50", linewidth = 2)
    prob_val <- round(1 - diff(pnorm(x,mu,sigma)), 4)
  }
  
  # plot bell curve
  bell_curve + 
    labs(subtitle = paste0("Probability: ", prob_val),
         caption = "SML 201", y = "") +
    theme_minimal()
}