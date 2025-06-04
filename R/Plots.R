#' Plot prior vs posterior distribution
#'
#' Plot leading to compare conflict or problems on the calibration
#'
#' @param DF_prior_posterior data frame, results of `Estimation` function givin prior vs posterior information
#'
#' @return ggplot,  histogram of prior vs posterior distribution
#' @import ggplot2
#' @export
Plot_prior_posterior <- function(DF_prior_posterior) {
  plot_conflicts <- ggplot(DF_prior_posterior, aes(x = value, fill = Distributions)) +
    geom_density(alpha = 0.4, show.legend = TRUE) +
    facet_wrap(~id, scales = "free", ncol = 3) +
    theme_bw() +
    theme(legend.position = "top", legend.key.size = unit(0.7, "cm")) +
    scale_y_continuous(name = "Probability density function")

  return(plot_conflicts)
}


#' Plot of the discharge simulation
#'
#' @param Q_observed data frame, discharge measurements
#' @param Q_simulated list or data frame, discharge simulation with their uncertainties
#'
#' @return plot_Q: ggplot, discharge simulation the associated uncertainties
#' @details
#' Function need results from `Prediction_Q_Qmec` to plot of the discharge simulation
#'
#' @export
plot_Q_sim_Qmec <- function(Q_observed,
                            Q_simulated) {
  # Plot simulation:
  plot_Q <- ggplot()
  if (length(Q_simulated) == 1) { # MaXPost only
    plot_Q <- plot_Q +
      geom_line(
        data = Q_simulated[[1]],
        aes(
          x = date,
          y = V1,
          color = factor(id)
        ),
        linewidth = 1
      )
  } else { # list, need to iterate depending on the type of uncertainties

    for (i in 1:length(Q_simulated)) {
      if (i == length(Q_simulated)) { # Plot MaxPost simulation

        plot_Q <- plot_Q +
          geom_line(
            data = Q_simulated[[i]],
            aes(
              x = date,
              y = V1,
              color = factor(id)
            ),
            linewidth = 1
          )
      } else {
        plot_Q <- plot_Q +
          geom_ribbon(
            data = Q_simulated[[i]],
            aes(
              x = date,
              ymin = q2.5,
              ymax = q97.5,
              fill = factor(id)
            ),
            alpha = 0.5
          )
      }
    }
  }

  # Plot discharge measurements:
  plot_Q <- plot_Q +
    geom_point(
      data = Q_observed,
      aes(
        x = date,
        y = Q,
        color = "Gaugings"
      ),
      size = 1
    )

  plot_Q <- plot_Q +
    labs(x = "Time", y = "Discharge (m3/s)") +
    scale_color_manual(
      name = "Discharge",
      values = c("MaxPost" = "black", "Gaugings" = "green"),
      labels = c("Simulated", "Observed")
    ) +
    scale_fill_manual(
      name = "Uncertainty",
      values = c("totalU" = "red", "paramU" = "yellow"),
      labels = c("Total", "Parametric")
    ) +
    guides(color = guide_legend(override.aes = list(
      shape = c(NA, 16),
      linetype = c(1, 0)
    ))) +
    theme_bw() +
    theme(legend.title = element_text(hjust = 0.5, size = 12))

  return(plot_Q)
}


#' Plot 1D Shallow Water (1D SW) components
#'
#' @param pressure_SW list or data frame, pressure gradient obtained by using `Prediction_Q_Qmec` function
#' @param friction_SW list or data frame, friction obtained by using `Prediction_Q_Qmec` function
#' @param advection_SW list or data frame, advection obtained by using `Prediction_Q_Qmec` function
#' @param MaxPostOnly logical, if `TRUE`, only plot of MaxPost estimation of all components of the 1D Shallow Water
#'
#' @return plot_SW: list of ggplots, plots of 1D SW components over time with uncertainties and their sum
#' @export
plot_shallow_water <- function(pressure_SW,
                               friction_SW,
                               advection_SW,
                               MaxPostOnly = FALSE) {
  # Plot simulation:
  plot_SW <- list()
  combined_SW <- list()

  if (MaxPostOnly) {
    combined_SW <- data.frame(
      date = pressure_SW$MaxPost$date,
      min = c(pressure_SW$MaxPost$V1 +
        friction_SW$MaxPost$V1 +
        advection_SW$MaxPost$V1),
      max = c(pressure_SW$MaxPost$V1 +
        friction_SW$MaxPost$V1 +
        advection_SW$MaxPost$V1),
      id = "Sum"
    )

    plot_SW <- ggplot() +
      geom_line(
        data = pressure_SW$MaxPost,
        aes(
          x = date,
          y = V1,
          color = factor(id)
        ),
        linewidth = 1
      ) +
      geom_line(
        data = friction_SW$MaxPost,
        aes(
          x = date,
          y = V1,
          color = factor(id)
        ),
        linewidth = 1
      ) +
      geom_line(
        data = advection_SW$MaxPost,
        aes(
          x = date,
          y = V1,
          color = factor(id)
        ),
        linewidth = 1
      ) +
      scale_color_manual(
        name = "1D Shallow Water \ncomponents",
        values = c(
          "MaxPostPressure" = "blue",
          "MaxPostFriction" = "green",
          "MaxPostAdvection" = "red",
          "Sum" = "purple"
        ),
        labels = c(
          "Pressure",
          "Friction",
          "Advection",
          "Sum"
        )
      ) +
      labs(
        title = "Maximum posterior estimation",
        x = "Time",
        y = expression("Discharge gradient (" * m^3 * "/" * s^2 * ")")
      ) +
      theme_bw() +
      theme(
        legend.title = element_text(hjust = 0.5, size = 12),
        plot.title = element_text(hjust = 0.5, size = 12)
      )

    return(plot_SW)
  } else { # Uncertainties

    # First loop: All components of SWE with a same type of uncertainty or MaxPost
    for (i in 1:length(pressure_SW)) {
      plot_SW[[i]] <- local({
        if (names(pressure_SW)[i] == "MaxPost") { # Plot MaxPost simulation
          ggplot() +
            geom_line(
              data = pressure_SW$MaxPost,
              aes(
                x = date,
                y = V1,
                color = factor(id)
              ),
              linewidth = 1
            ) +
            geom_line(
              data = friction_SW$MaxPost,
              aes(
                x = date,
                y = V1,
                color = factor(id)
              ),
              linewidth = 1
            ) +
            geom_line(
              data = advection_SW$MaxPost,
              aes(
                x = date,
                y = V1,
                color = factor(id)
              ),
              linewidth = 1
            ) +
            scale_color_manual(
              name = "1D Shallow Water \ncomponents",
              values = c(
                "MaxPostPressure" = "blue",
                "MaxPostFriction" = "green",
                "MaxPostAdvection" = "red",
                "Sum" = "purple"
              ),
              labels = c(
                "Pressure",
                "Friction",
                "Advection",
                "Sum"
              )
            ) +
            labs(
              title = "Maximum posterior estimation",
              x = "Time",
              y = expression("Discharge gradient (" * m^3 * "/" * s^2 * ")")
            ) +
            theme_bw() +
            theme(
              legend.title = element_text(hjust = 0.5, size = 12),
              plot.title = element_text(hjust = 0.5, size = 12)
            )
        } else {
          combined_SW[[i]] <- data.frame(
            date = pressure_SW[[i]]$date,
            min = c(pressure_SW[[i]]$q2.5 +
              friction_SW[[i]]$q2.5 +
              advection_SW[[i]]$q2.5),
            max = c(pressure_SW[[i]]$q97.5 +
              friction_SW[[i]]$q97.5 +
              advection_SW[[i]]$q97.5),
            id = "Sum"
          )

          if (names(pressure_SW)[i] == "TotalU") {
            values_color <- c(
              "totalPressure" = "blue",
              "totalFriction" = "green",
              "totalAdvection" = "red",
              "Sum" = "purple"
            )

            title_customized <- "Total uncertainty"
          } else {
            values_color <- c(
              "paramPressure" = "blue",
              "paramFriction" = "green",
              "paramAdvection" = "red",
              "Sum" = "purple"
            )

            title_customized <- "Parametric uncertainty"
          }

          ggplot() +
            geom_ribbon(
              data = pressure_SW[[i]],
              aes(
                x = date,
                ymin = q2.5,
                ymax = q97.5,
                fill = factor(id)
              ),
              alpha = 0.5
            ) +
            geom_ribbon(
              data = friction_SW[[i]],
              aes(
                x = date,
                ymin = q2.5,
                ymax = q97.5,
                fill = factor(id)
              ),
              alpha = 0.5
            ) +
            geom_ribbon(
              data = advection_SW[[i]],
              aes(
                x = date,
                ymin = q2.5,
                ymax = q97.5,
                fill = factor(id)
              ),
              alpha = 0.5
            ) +
            geom_ribbon(
              data = combined_SW[[i]],
              aes(
                x = date,
                ymin = min,
                ymax = max,
                fill = factor(id)
              ),
              alpha = 0.5
            ) +
            scale_fill_manual(
              name = "1D Shallow Water \ncomponents",
              values = values_color,
              labels = c(
                "Pressure",
                "Friction",
                "Advection",
                "Sum"
              )
            ) +
            labs(
              title = title_customized,
              x = "Time",
              y = expression("Discharge gradient (" * m^3 * "/" * s^2 * ")")
            ) +
            theme_bw() +
            theme(
              legend.title = element_text(hjust = 0.5, size = 12),
              plot.title = element_text(hjust = 0.5, size = 12)
            )
        }
      })
      names(plot_SW)[[i]] <- names(pressure_SW)[i]
    }
  }
  return(plot_SW)
}
