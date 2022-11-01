# Packages ----
{
  library(mc2d)
  library(tidyverse)
  library(scales)
  library(ggExtra)
  library(gridExtra)
  library(kableExtra)
  library(ggpmisc)
}


# Functions ----
{
  ## Simulations ----
  {
    fn_cost.refurb <- function(
      refurb.cost.mats, refurb.cost.labor, refurb.cost.outage, refurb.L, 
      new.cost.mats, new.cost.labor, new.cost.outage, new.L,
      coincidental.cost.labor, coincidental.cost.outage, 
      rate.discount, median.start.year = 2027
    ){
      
      {
        # refurb.cost.mats <- wg.refurb.cost.materials[2]
        # refurb.cost.labor <- wg.refurb.cost.labor[2]
        # refurb.cost.outage <- wg.refurb.cost.outage[2]
        # refurb.L <- wg.refurb.life[2]
        # 
        # coincidental.cost.labor <- sunk.cost.labor[2]
        # coincidental.cost.outage <- sunk.cost.outage[2]
        # 
        # new.cost.mats <- wg.new.cost.materials[2]
        # new.cost.labor <- wg.new.cost.labor[2]
        # new.cost.outage <- wg.new.cost.outage[2]
        # new.L <- wg.new.life[2]
      }
      
      # if(refurb.cost.labor < coincidental.cost.labor) refurb.cost.labor <- coincidental.cost.labor
      # if(refurb.cost.outage < coincidental.cost.outage) refurb.cost.outage <- coincidental.cost.outage
      
      # Costs at T=0
      {
        costs.upfront <- refurb.cost.mats #+ refurb.cost.labor + refurb.cost.outage - coincidental.cost.labor - coincidental.cost.outage
      }
      
      # Costs at T=L
      {
        (costs.future <- new.cost.mats + new.cost.labor + new.cost.outage)
        costs.future.pv <- (costs.future)*(1+rate.discount)^(-refurb.L)
      }
      
      # Costs in Perpetuity
      {
        costs.perpetuity <- costs.future / ((1 + rate.discount)^(new.L) - 1)
        costs.perpetuity.pv <- costs.perpetuity * (1+rate.discount)^(-refurb.L)
      }
      
      # Total PV Cost
      cost.pv <- (costs.upfront + costs.future.pv + costs.perpetuity.pv) / (1 + rate.discount)^(median.start.year - as.integer(format(Sys.Date(), "%Y")))
      
      return(cost.pv)
    }
    
    
    fn_cost.replace <- function(
      #refurb.cost.mats, refurb.cost.labor, refurb.cost.outage, refurb.L, 
      new.cost.mats, new.cost.labor, new.cost.outage, new.L,
      coincidental.cost.labor, coincidental.cost.outage, 
      rate.discount, median.start.year = 2027
    ){
      
      {
        # refurb.cost.mats <- wg.refurb.cost.materials[2]
        # refurb.cost.labor <- wg.refurb.cost.labor[2]
        # refurb.cost.outage <- wg.refurb.cost.outage[2]
        # refurb.L <- wg.refurb.life[2]
        # 
        # coincidental.cost.labor <- sunk.cost.labor[2]
        # coincidental.cost.outage <- sunk.cost.outage[2]
        # 
        # new.cost.mats <- wg.new.cost.materials[2]
        # new.cost.labor <- wg.new.cost.labor[2]
        # new.cost.outage <- wg.new.cost.outage[2]
      }
      
      # if(new.cost.labor < coincidental.cost.labor) new.cost.labor <- coincidental.cost.labor
      # if(new.cost.outage < coincidental.cost.outage) new.cost.outage <- coincidental.cost.outage
      
      
      # Costs at T=0
      {
        costs.upfront <- new.cost.mats #+ new.cost.labor + new.cost.outage - coincidental.cost.labor - coincidental.cost.outage
      }
      
      # Costs in Perpetuity
      {
        costs.future <- new.cost.mats + new.cost.labor + new.cost.outage
        costs.perpetuity <- costs.future / ((1 + rate.discount)^(new.L) - 1)
      }
      
      # Total PV Cost
      cost.pv <- (costs.upfront + costs.perpetuity) / (1 + rate.discount)^(median.start.year - as.integer(format(Sys.Date(), "%Y")))
      
      return(cost.pv)
    }
    
    fn_vector_to_dist <- function(n, vector, dist){
      # vector <- wg.refurb.cost.labor
      # dist <- rbeta
      
      if(identical(dist, rpert)){
        
        rpert(n, min = vector[1], mode = vector[2], max = vector[3]) 
        
      } else if(identical(dist, rbeta)){
        
        term <- mean(vector) * (1 - mean(vector))/var(vector) - 1
        alpha <- mean(vector) * term
        beta <- (1 - mean(vector)) * term
        if (var(vector) >= mean(vector) * (1 - mean(vector))) stop("var must be less than mean * (1 - mean)")
        
        rbeta(n, alpha, beta) 
        
      }  else if(identical(dist, rnorm)){
        
        rnorm(n, mean = mean(vector), sd = sd(vector)/3) 
      }  else if(identical(dist, rgamma)){
        
        theta <- sd(vector)^2/mean(vector)
        kappa <- mean(vector)/theta
        
        rgamma(n, shape = kappa, scale = theta)
      }
      
      
    }
  }
  
  # Charts ----
  {
    # Marginal Plots
    {
      fn_chart.marginal.scatter <- function(
        sim.all, x.var, y.var = "cost.net", size.var = NULL, alpha.var = NULL, x.label = NULL
      ){
        
        {
          # sim.all <- sim.pert.all
          # x.var <- "refurb.L"
          # y.var <- "cost.net"
          # size.var = NULL
          # alpha.var = NULL
          # x.label <- NULL
        }
        
        if(is.null(size.var)){
          
          (gg <- sim.all %>% 
            ggplot(aes_string(
              x = x.var, y = y.var, 
              alpha = alpha.var
            )) +
            # geom_hline(yintercept = 0, linetype = "dashed") +
            geom_point(
              aes_string(fill = "optimal.choice"),
              shape = 21, color = "gray40", size = 4
            ) +
            # geom_smooth(method = "lm", color = "gray30") +
            # geom_quantile(linetype = "dashed", size = 2, quantiles = .5) +
            scale_x_continuous(x.label, labels = label_comma()) +
            scale_y_continuous(labels = scales::label_dollar()) +
            theme_bw() +
            theme(legend.position = "bottom"))
          
        } else{
          
          gg <- sim.all %>% 
            ggplot(aes_string(
              x = x.var, y = y.var, fill = "optimal.choice", 
              size = size.var,
              alpha = alpha.var
            )) +
            # geom_hline(yintercept = 0, linetype = "dashed") +
            geom_point(shape = 21, color = "gray40") +
            scale_x_continuous(x.label, labels = label_comma()) +
            scale_y_continuous(labels = scales::label_dollar()) +
            # scale_size_manual(values = c(.5, 4)) +
            theme_bw() +
            theme(legend.position = "bottom")
          
        }
        
        # gg <- gg +
        #   annotate(
        #     geom = "table", x = Inf, y = Inf, 
        #     label = list(tbl.refurb.L), vjust = 1
        #   )
        
        # ggMarginal(gg, type = "histogram", margins = "x", size = 10)
        
        return(gg)
      }
      
      fn_chart.marginal.frequency <- function(sim.all, x.var, x.label = NULL){
        sim.all %>% 
          ggplot(aes_string(x = x.var, color = "optimal.choice")) +
          geom_freqpoly(size = 1, bins = 30) +
          scale_x_continuous(x.label, labels = label_comma()) +
          theme_bw() +
          theme(
            legend.position = "bottom",
            panel.grid.major.x = element_line(color = "gray80"),
            panel.grid.minor.x = element_line(color = "gray90"),
            panel.grid.major.y = element_line(color = "gray90")
          )
      }
    }
    
    
    # Cost Distributions
    {
      # fn_chart.cost.net <- function(sim.pert.all){
      #   
      #   dens <- density(sort(sim.pert.all$cost.net))
      #   probs <- seq(0,1,.25)
      #   df <- data.frame(x = dens$x, y = dens$y) %>% 
      #     mutate(
      #       optimal.choice = ifelse(x >= 0, "replace", "refurb"),
      #       quantile = factor(findInterval(x, quantile(sim.pert.all$cost.net, probs)))
      #     )
      #   
      #   df %>% 
      #     ggplot(aes(x = x, y = y)) +
      #     geom_ribbon(
      #       aes(ymin = 0, ymax = y, fill = quantile), 
      #       alpha = .6, color = "white"
      #     ) +
      #     geom_text(
      #       data = data.frame(
      #         quants = quantile(sim.pert.all$cost.net, seq(.125, .875, .25)),
      #         label = paste(c("First", "Second", "Third", "Fourth"), "Quartile", sep = "\n")
      #       ) %>% 
      #         mutate(
      #           height = df$y[c(c(.2, .25, .25, .2)*nrow(df))]
      #         ),
      #       inherit.aes = FALSE,
      #       aes(x = quants, y = height, label = label),
      #       hjust = .5, vjust = .5, size = 2.5
      #     ) +
      #     geom_line(size = 1) +
      #     geom_vline(xintercept = 0, linetype = "dashed") +
      #     geom_text(
      #       data = sim.pert.all %>% 
      #         summarize(
      #           avg = mean(cost.net),
      #           sd = sd(cost.net)
      #         ),
      #       aes(
      #         x = quantile(sim.pert.all$cost.net, 0), y = Inf, 
      #         label = paste0(
      #           "Mean: ", label_dollar()(avg), "\n",
      #           "Standard Deviation: ", label_dollar()(sd)
      #         )
      #       ), vjust = 1, hjust = 0
      #     ) +
      #     annotate(
      #       "text", x = -100000, y = 0, label = "← Refurbish is optimal", size = 3,
      #       hjust = 1, vjust = 1
      #     ) +
      #     annotate(
      #       "text", x = 100000, y = 0, label = "Replace is optimal →", size = 3,
      #       hjust = 0, vjust = 1
      #     ) +
      #     scale_x_continuous("Values", labels = scales::label_comma()) +
      #     scale_fill_brewer(guide="none") +
      #     ggtitle("Simulated Cost to Replace Now Less the Cost to Refurbish", "Per Unit") +
      #     theme_bw() +
      #     theme(
      #       axis.text.y = element_blank(),
      #       axis.ticks.y = element_blank(),
      #       axis.title.y = element_blank(),
      #       panel.grid.minor = element_line(color = "gray85"),
      #       legend.position = "bottom"
      #     )
      #   
      # }
      # 
      
      
      fn_chart.shaded.density <- function(values){
        
        dens <- density(sort(values))
        probs <- seq(0,1,.25)
        df <- data.frame(x = dens$x, y = dens$y) %>% 
          mutate(
            quantile = factor(findInterval(x, quantile(values, probs)))
          )
        
        df %>% 
          ggplot(aes(x = x, y = y)) +
          geom_ribbon(
            aes(ymin = 0, ymax = y, fill = quantile), 
            alpha = .6, color = "white"
          ) +
          geom_text(
            data = data.frame(
              quants = quantile(values, seq(.125, .875, .25)),
              label = paste(c("First", "Second", "Third", "Fourth"), "Quartile", sep = "\n")
            ) %>% 
              mutate(
                height = df$y[c(c(.2, .25, .25, .2)*nrow(df))]
              ),
            inherit.aes = FALSE,
            aes(x = quants, y = height/2, label = label),
            hjust = .5, vjust = .5, size = 2.5
          ) +
          geom_line(size = 1) +
          geom_text(
            data = data.frame(
              avg = mean(values),
              sd = sd(values)
            ),
            aes(
              x = quantile(values, 0), y = Inf, 
              label = paste0(
                "\nMean: ", label_dollar()(avg), "\n",
                "Standard Deviation: ", label_dollar()(sd)
              )
            ), vjust = 1, hjust = 0
          ) +
          # annotate(
          #   "text", x = -100000, y = 0, label = "← Refurbish is optimal", size = 3,
          #   hjust = 1, vjust = 1
          # ) +
          # annotate(
          #   "text", x = 100000, y = 0, label = "Replace is optimal →", size = 3,
          #   hjust = 0, vjust = 1
          # ) +
          scale_x_continuous(
            "NPV Cost", 
            labels = scales::label_dollar()
          ) +
          scale_fill_brewer(guide="none") +
          theme_bw() +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.minor = element_line(color = "gray85"),
            legend.position = "bottom"
          )
        
      }
    }
  }
  
  
  # Breakeven Analysis ----
  {
    fn_fit.logit <- function(simulation, rate.discount){
      
      {
        # simulation <- sim.pert.all
      }
      
      suppressWarnings({
        fit.glm <- simulation %>% 
          mutate(optimal.choice.binary = ifelse(optimal.choice == "replace", 1, 0)) %>% 
          glm(
            data = ., 
            optimal.choice.binary ~ refurb.cost.mats 
            + refurb.L 
            + new.cost.mats
            + I(new.cost.mats * (1 + rate.discount)^refurb.L)
            + I(new.cost.labor * (1 + rate.discount)^refurb.L)
            + I(new.cost.outage * (1 + rate.discount)^refurb.L)
            
            , family = binomial
          )#; summary(fit.glm)
      })
      
      
      # par(mfrow = c(2,2)); plot(fit.glm)
      # 1 - fit.glm$deviance / fit.glm$null.deviance
      # car::crp(fit.glm)
      
      prob <- exp(predict(fit.glm, newdata = simulation)) / (1 + exp(predict(fit.glm, newdata = simulation)))
      
      return(prob)      
    }
    
    fn_get.breakeven <- function(series, probabilities, quantiles = c(.5)){
      
      {
        # series <- sim.pert.all$refurb.L
        # probabilities <- prob
        # quantiles <- .5
      }
      
      log.odds <- log(quantiles / (1 - quantiles))
      
      suppressWarnings({
        logit <- glm(
          probabilities ~ series
          , family = binomial
        )
      })#; summary(logit)
      
      betas <- coef(logit)
      
      breakeven <- (-(as.numeric((betas[1] - log.odds) / betas[2])))
      names(breakeven) <- label_percent(accuracy = 1)(quantiles)
      # breakeven
      
      return(breakeven)
    }
    
    fn_chart.logit <- function(
      simulation, probabilities, quantile.vec = c(.25,.5,.75), 
      x.var = "refurb.L", fn_format = label_comma(accuracy = .1)
    ){
      
      {
        # fn_format = label_comma(accuracy = .1)
        # quantile.vec = c(.1, .25,.5,.75, .9)
        # x.var <- "new.cost.outage"
        # simulation <- sim.pert.all
        # probabilities <- fn_fit.logit(simulation = simulation, rate.discount = rate.discount)
        # breakeven <- fn_get.breakeven(sim.pert.all[,x.var], probabilities, .5)
      }
      
      quantiles <- data.frame(
        probability = quantile.vec, 
        x = fn_get.breakeven(simulation[,x.var], probabilities, quantile.vec)
      ) %>% 
        filter(x >= 0)
      
      min.val <- min(min(simulation[,x.var]), min(quantiles$x))
      
      
      simulation %>%
        mutate(probability = probabilities) %>%
        ggplot(aes_string(x = x.var, y = "probability")) +
        geom_point(aes(color = optimal.choice)) +
        geom_point(
          data = quantiles,
          aes(x = x, y = probability), inherit.aes = F,
          color = "gray20", size = 4
        ) +
        geom_segment(
          data = quantiles,
          aes(
            y = probability, yend = probability,
            x = min.val, xend = x
          ),
          linetype = "dotted"
        ) +
        geom_smooth(
          method = "glm", method.args = list(family = "binomial"),
          fullrange = TRUE, se = T, size = 1.25
        ) +
        annotate(
          "text", x = min.val*.95, y = .4, label = "← Refurbish is optimal", size = 4,
          hjust = 1, vjust = 0, angle = 90, fontface = "bold"
        ) +
        annotate(
          "text", x = min.val*.95, y = .6, label = "Replace is optimal →", size = 4,
          hjust = 0, vjust = 0, angle = 90, fontface = "bold"
        ) +
        geom_label(
          data = quantiles, #inherit.aes = FALSE,
          aes(x = x, y = probability + .01, label = fn_format(x)),
          vjust = 0, hjust = 1, fontface = "bold", nudge_x = .1
        ) +
        scale_y_continuous(
          "Estimated Probability", labels = label_percent(),
          breaks = c(0, quantile.vec, 1)
        ) +
        theme_bw() +
        theme(
          legend.position = c(.11,.11),
          legend.background = element_rect(fill = "white", color = "black")
        )
      
      
      # ggMarginal(gg, type = "histogram", margins = "x", size = 10)
      
    }
  }
}