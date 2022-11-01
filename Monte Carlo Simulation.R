{
  source("Functions.R")
  options(scipen = 999, digits = 5)
}

# Inputs ----
{
  # Costs incurred for other projects at time 0
  sunk.cost.labor <- c(2500, 3500, 4500)*1000
  sunk.cost.outage <- c(300, 400, 600)*1000
  
  
  # Refurbish
  wg.refurb.life <- c(15,30,45)
  wg.refurb.cost.materials <- c(500, 800, 3000)*1000
  wg.refurb.cost.labor <- c(2500, 3500, 4500)*1000
  wg.refurb.cost.outage <- c(0, 924, 1927.8)*1000
  
  # New
  wg.new.life <- c(90, 100, 110)
  wg.new.cost.materials <- c(1800, 2200, 3000)*1000
  wg.new.cost.labor <- c(2500, 3500, 4500)*1000
  wg.new.cost.outage <- c(0, 924, 1927.8)*1000
  
  
  n <- 10000
  n.units <- 7
  rate.discount <- (1.07 / 1.025) - 1
}



# Simulations ----
{
  ## PERT Distribution ----
  {
    sim.pert <- sapply(
      list(
        wg.refurb.cost.materials, wg.refurb.cost.labor, wg.refurb.cost.outage, wg.refurb.life,
        wg.new.cost.materials, wg.new.cost.labor, wg.new.cost.outage, wg.new.life,
        sunk.cost.labor, sunk.cost.outage
      ), 
      \(x){ fn_vector_to_dist(n, x, rpert)}
    ) %>% 
      as_tibble() %>% 
      rename_all(~ c(
        "refurb.cost.mats", "refurb.cost.labor", "refurb.cost.outage", "refurb.L", 
        "new.cost.mats", "new.cost.labor", "new.cost.outage", "new.L",
        "coincidental.cost.labor", "coincidental.cost.outage"
      )) %>% 
      mutate(across(-contains("L", ignore.case = F), \(x){x * n.units}))
    
    
    cost.refurb.pert <- sim.pert %>% 
      mutate(rate.discount = rate.discount) %>% 
      pmap_dbl(., .f = fn_cost.refurb)
    
    
    cost.replace.pert <- sim.pert %>% 
      select(contains("new") | contains("coin")) %>% 
      mutate(rate.discount = rate.discount) %>% 
      pmap_dbl(., .f = fn_cost.replace)
    
    sim.pert.all <- cbind(sim.pert, cost.refurb.pert, cost.replace.pert) %>% 
      mutate(
        cost.net = cost.refurb.pert - cost.replace.pert, 
        optimal.choice = ifelse(cost.net >= 0, "replace", "refurb")
      )
  }
  
  
  # Normal Distribution ----
  {
    sim.norm <- sapply(
      list(
        wg.refurb.cost.materials, wg.refurb.cost.labor, wg.refurb.cost.outage, wg.refurb.life,
        wg.new.cost.materials, wg.new.cost.labor, wg.new.cost.outage, wg.new.life,
        sunk.cost.labor, sunk.cost.outage
      ), 
      \(x){ fn_vector_to_dist(n, x, rnorm)}
    ) %>% 
      as_tibble() %>% 
      rename_all(~ c(
        "refurb.cost.mats", "refurb.cost.labor", "refurb.cost.outage", "refurb.L", 
        "new.cost.mats", "new.cost.labor", "new.cost.outage", "new.L",
        "coincidental.cost.labor", "coincidental.cost.outage"
      ))
    
    
    cost.refurb.norm <- sim.norm %>% 
      pmap_dbl(., .f = fn_cost.refurb)
    
    
    cost.replace.norm <- sim.norm %>% 
      select(contains("new") | contains("coin")) %>% 
      pmap_dbl(., .f = fn_cost.replace)
    
    
    sim.norm.all <- cbind(sim.norm, cost.refurb.norm, cost.replace.norm) %>% 
      mutate(
        cost.net = cost.refurb.norm - cost.replace.norm, 
        optimal.choice = ifelse(cost.net >= 0, "replace", "refurb")
      )
  }
  
  
  # Gamma Distribution ----
  {
    sim.gamma <- sapply(
      list(
        wg.refurb.cost.materials, wg.refurb.cost.labor, wg.refurb.cost.outage, wg.refurb.life,
        wg.new.cost.materials, wg.new.cost.labor, wg.new.cost.outage, wg.new.life,
        sunk.cost.labor, sunk.cost.outage
      ), 
      \(x){ fn_vector_to_dist(n, x, rgamma)}
    ) %>% 
      as_tibble() %>% 
      rename_all(~ c(
        "refurb.cost.mats", "refurb.cost.labor", "refurb.cost.outage", "refurb.L", 
        "new.cost.mats", "new.cost.labor", "new.cost.outage", "new.L",
        "coincidental.cost.labor", "coincidental.cost.outage"
      )) 
    
    cost.refurb.gamma <- sim.gamma %>% 
      pmap_dbl(., .f = fn_cost.refurb)
    
    
    cost.replace.gamma <- sim.gamma %>% 
      select(contains("new") | contains("coin")) %>% 
      pmap_dbl(., .f = fn_cost.replace)
    
    
    sim.gamma.all <- cbind(sim.gamma, cost.refurb.gamma, cost.replace.gamma) %>% 
      mutate(
        cost.net = cost.refurb.gamma - cost.replace.gamma, 
        optimal.choice = ifelse(cost.net >= 0, "replace", "refurb")
      )
  }
  
}

# Distribution stats
{
  # Overall
  {
    
    sim.pert.all %>% 
      select(-optimal.choice) %>%
      mutate(
        `Refurbished Labor Cost` = refurb.cost.labor - coincidental.cost.labor,
        `Refurbished Outage Cost` = refurb.cost.outage - coincidental.cost.outage,
        `Replace Labor Cost` = new.cost.labor - coincidental.cost.labor,
        `Replace Outage Cost` = new.cost.outage - coincidental.cost.outage,
        
        .keep = "unused"
      ) %>% 
      relocate(starts_with("refurb"), starts_with("new"), starts_with("Replace"), contains("cost")) %>% 
      rename(
        `Refurbished Materials Cost` = refurb.cost.mats, 
        `Replace Materials Cost` = new.cost.mats, 
        `Refurbished Unit Life` = refurb.L,
        `PV Cost - Refurbished` = cost.refurb.pert,
        `PV Cost - Replace` = cost.replace.pert,
        `NPV Cost` = cost.net
      ) %>% 
      select(-contains("coin"), -new.L) %>% 
      summarize(
        across(everything(), .fns = mean)
      ) %>% 
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean") %>% 
      mutate(
        Mean = label_comma()(Mean)
      ) %>% 
      kableExtra::kbl(align = c("l", "r"))
    
    choice.proportions <- sim.pert.all %>% 
      count(optimal.choice) %>% 
      mutate(
        percent = n / sum(n)
      )
    
    
    
    mean(sim.pert.all$cost.refurb.pert)
    sd(sim.pert.all$cost.refurb.pert)
    
    mean(sim.pert.all$cost.replace.pert)
    sd(sim.pert.all$cost.replace.pert)
    
  }
  
  # Marginal
  {
    
    sim.pert.all %>% 
      select(refurb.L, cost.refurb.pert, cost.replace.pert, cost.net) %>%
      group_by()
      summarize(
        across(everything(), .fns = mean)
      ) %>% 
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean") %>% 
      mutate(
        Mean = label_comma()(Mean)
      ) %>% 
      kableExtra::kbl(align = c("l", "r"))
    
    choice.proportions <- sim.pert.all %>% 
      count(optimal.choice) %>% 
      mutate(
        percent = n / sum(n)
      )
    
    
    
    mean(sim.pert.all$cost.refurb.pert)
    sd(sim.pert.all$cost.refurb.pert)
    
    mean(sim.pert.all$cost.replace.pert)
    sd(sim.pert.all$cost.replace.pert)
    
  }
}

# Breakeven Analysis ----
{
  {
    prob <- fn_fit.logit(simulation = sim.pert.all, rate.discount = rate.discount)  
    
    # Refurbished Life
    
    fn_get.breakeven(sim.pert.all$refurb.L, prob, c(.37,.42,.45,.49,.55,.6,.64,.71,.76,.82,.87,.9,.94))
    refurb.L.breakeven <- fn_get.breakeven(sim.pert.all$refurb.L, prob, .5)
    
    sim.pert.all %>% 
      ggplot(aes(x = refurb.L, fill = optimal.choice)) +
      geom_histogram(position = position_dodge()) +
      scale_x_continuous(limits = c(15, 50), breaks = seq(0, 100, 5)) +
      theme(
        legend.position = "none"
      )
    
    fn_chart.logit(sim.pert.all, prob, quantile.vec = c(.1,.25,.5,.75, .9), x.var = "refurb.L") %>%
      ggMarginal(., type = "histogram", margins = "both", size = 10, groupFill = F)
      # ggMarginal(., type = "histogram", margins = "both", size = 10, groupFill = T, position = position_dodge())
    
    # New Mat Cost
    (fn_chart.logit(
      sim.pert.all, prob, quantile.vec = c(.1,.25,.5,.75, .9), 
      x.var = "new.cost.mats", 
      fn_format = label_dollar(accuracy = 1)
    ) +
      scale_x_continuous(labels = label_dollar(), expand = c(.1, 0, .05, 0))) %>% 
      ggMarginal(., type = "histogram", margins = "x", size = 10)
    
    # Refurb Mat Cost
    (fn_chart.logit(
      sim.pert.all, prob, quantile.vec = c(.25,.5,.75, .9), 
      x.var = "refurb.cost.mats", 
      fn_format = label_dollar(accuracy = 1)
    ) +
      scale_x_continuous(labels = label_dollar(), expand = c(.12, 0, .05, 0))) %>% 
      ggMarginal(., type = "histogram", margins = "x", size = 10)
    
    # Refurb Labor Cost
    fn_chart.logit(
      sim.pert.all, prob, quantile.vec = c(.1,.25,.5,.75, .9), 
      x.var = "refurb.cost.labor", 
      fn_format = label_dollar(accuracy = 1)
    ) +
      scale_x_continuous(labels = label_dollar(), trans = "log")
    
    # Future Replace Outage Cost
    fn_chart.logit(
      sim.pert.all, prob, quantile.vec = c(.1,.25,.5,.75, .9), 
      x.var = "new.cost.outage", 
      fn_format = label_dollar(accuracy = 1)
    ) +
      scale_x_continuous(labels = label_dollar())
  }
  
  
  # Old
  {
    fit.glm <- sim.pert.all %>% 
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
      ); summary(fit.glm)
    
    par(mfrow = c(2,2)); plot(fit.glm)
    1 - fit.glm$deviance / fit.glm$null.deviance
    
    car::crp(fit.glm)
    
    # Estimated Probabilities
    prob <- exp(predict(fit.glm, newdata = sim.pert.all)) / (1 + exp(predict(fit.glm, newdata = sim.pert.all)))
    
    refurb.L.breakeven <- fn_get.breakeven(sim.pert.all$refurb.L, prob, .5)
    
    # Marginal Plot
    sim.pert.all %>% 
      mutate(probability = prob) %>% #View()
      ggplot(aes(x = refurb.L, y = probability)) +
      geom_hline(yintercept = .5, linetype = "dashed") +
      geom_vline(xintercept = refurb.L.breakeven, size = 1) +
      geom_point(aes(color = optimal.choice)) +
      geom_smooth(method = "glm", method.args = list(family = "binomial")) +
      geom_text(
        data = as.data.frame(refurb.L.breakeven), inherit.aes = FALSE,
        aes(
          x = refurb.L.breakeven, y = .51,
          label = label_comma(accuracy = .1)(refurb.L.breakeven)
        ),
        vjust = 0, hjust = 0, fontface = "bold", nudge_x = .1
      ) +
      ylab("Estimated Probability") +
      xlab("Refurbished Life") +
      theme_bw() +
      theme(
        # legend.position = "bottom"
        legend.position = c(.11,.11),
        legend.background = element_rect(fill = "white", color = "black")
      )
  }
}

# Distribution Charts
{
  fn_chart.shaded.density(sim.pert.all$cost.refurb.pert)
  fn_chart.shaded.density(sim.pert.all$cost.replace.pert)
  fn_chart.shaded.density(sim.pert.all$cost.net)
}


# Marginal Plots ----
{
  colnames(sim.norm.all)
  
  grid.arrange(
    fn_chart.marginal.scatter(sim.pert.all, "refurb.L"),
    fn_chart.marginal.frequency(sim.pert.all, "refurb.L")
  )
  
  fn_chart.marginal.scatter(sim.pert.all, "refurb.L")
  
  fn_chart.marginal.scatter(sim.pert.all, "refurb.L")
  fn_chart.marginal.frequency(sim.pert.all, "refurb.L")
  
  fn_chart.marginal.scatter(sim.pert.all, "refurb.cost.mats", size.var = NULL, alpha.var = "refurb.L")
  fn_chart.marginal.scatter(sim.pert.all, "refurb.cost.mats", "new.cost.mats") 
  fn_chart.marginal.frequency(sim.pert.all, "refurb.cost.mats")+
    scale_x_continuous(breaks = seq(0, 5000000, 500000), labels = label_comma())
  
  
  fn_chart.marginal.scatter(sim.pert.all, "refurb.cost.labor", size.var = "refurb.L", alpha.var = "refurb.L")
  fn_chart.marginal.frequency(sim.pert.all, "refurb.cost.labor")
  
  
  fn_chart.marginal.scatter(sim.pert.all, "new.cost.mats")
  fn_chart.marginal.frequency(sim.pert.all, "new.cost.mats")
  
  
  fn_chart.marginal.scatter(sim.pert.all, "new.cost.labor")
  fn_chart.marginal.frequency(sim.pert.all, "new.cost.labor")
  
  
  fn_chart.marginal.scatter(sim.pert.all, "new.cost.outage")
  fn_chart.marginal.frequency(sim.pert.all, "new.cost.outage")
  
  
  {
    fn_chart.marginal.scatter(sim.pert.all, "refurb.cost.mats", "new.cost.mats") 
    
    sim.pert.all %>% 
      select(refurb.cost.mats, new.cost.mats, cost.net) %>% 
      mutate(
        # refurb = cut(
        #   refurb.cost.mats, 
        #   round(quantile(sim.pert.all$refurb.cost.mats, seq(0,1,.1))), 
        #   dig.lab = 9, 
        # ),
        # replace = cut(new.cost.mats, round(quantile(sim.pert.all$new.cost.mats, seq(0,1,.1))), dig.lab = 9)
        
        refurb = cut_interval(refurb.cost.mats, n = 10, dig.lab = 9),
        replace = cut_interval(new.cost.mats, n = 10, dig.lab = 9)
      ) %>% 
      group_by(refurb, replace) %>% 
      summarize(cost.net = mean(cost.net)) %>% 
      mutate(
        refurb.cost.mats = str_sub(refurb, 2, -2) %>% str_extract(., ".*(?=,)") %>% as.numeric(),
        new.cost.mats = str_sub(replace, 2, -2) %>% str_extract(., ".*(?=,)") %>% as.numeric()
      ) %>% 
      ggplot(aes(x = refurb, y = replace, fill = cost.net)) +
      geom_raster(interpolate = TRUE) +
      scale_fill_gradient2(
        low = "red3", high = "blue3"
      ) +
      # scale_x_continuous(labels = label_dollar()) +
      # scale_y_continuous(labels = scales::label_dollar()) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    
     
    "(500195,646200]" %>%
      str_sub(., 2, -2) %>% 
      str_extract(., ".*(?=,)")
     
  }
  
  tbl.refurb.L <- sim.pert.all %>% 
      as_tibble() %>% 
      mutate(
        refurb.L = round(refurb.L)
      ) %>% 
      group_by(refurb.L) %>% 
      summarise(
        # n = n(),
        across(c(cost.refurb.pert, cost.replace.pert, cost.net), mean)
      ) %>% 
      filter(between(refurb.L, refurb.L.breakeven - 5, refurb.L.breakeven + 5)) %>% 
      mutate(across(c(cost.refurb.pert, cost.replace.pert, cost.net), \(x){label_dollar(accuracy = .01, scale = 1/1000000, suffix = "M")(x)})) %>% 
      rename_with(~c("Refurbished Life", "Avg Refurb Cost", "Avg Replace Cost", "Avg Diff")) # , "Modeled Refurb Cost"

}


# Linear Estimation ----
{
  fit.lm <- sim.pert.all %>% 
    lm(
      data = .,
      cost.refurb.pert ~ #refurb.cost.mats 
      + refurb.L 
      # + new.cost.mats
      # + I(new.cost.mats * (1 + rate.discount)^refurb.L)
      # + I(new.cost.labor * (1 + rate.discount)^refurb.L)
      # + I(new.cost.outage * (1 + rate.discount)^refurb.L)
    ); summary(fit.lm); par(mfrow = c(2,2)); plot(fit.lm); 
  
  tibble(refurb.L = seq(25, 40, 1)) %>% 
    mutate(
      predicted = label_dollar()(predict(fit.lm, as.data.frame(refurb.L)))
    ) %>% 
    # summarize(mean(predicted))
    View()
  
  refurb.L.breakeven <- fn_get.breakeven(sim.pert.all$refurb.L, prob, .5)
  
  sim.pert.all %>% 
    # as_tibble() %>% 
    mutate(
      refurb.L = round(refurb.L)
    ) %>% 
    group_by(refurb.L) %>% 
    summarise(
      # n = n(),
      across(c(cost.refurb.pert, cost.replace.pert, cost.net), mean, .names = "{.col}.mean"),
      # across(c(cost.refurb.pert, cost.replace.pert, cost.net), median, .names = "{.col}.median")
    ) %>% 
    # mutate(
    #   cost.net.calc.median = cost.refurb.pert.median - cost.replace.pert.median,
    #   across(contains("cost"), \(x){label_dollar()(x)})
    # ) %>% 
    # filter(between(refurb.L, refurb.L.breakeven - 5, refurb.L.breakeven + 5)) %>% 
    # rename_with(~c("Refurbished Life", "Count", "Avg Refurb Cost", "Avg Replace Cost", "Avg Diff"))
    select(-contains("net")) %>% 
    pivot_longer(-refurb.L, names_to = "cost", values_to = "amount") %>% 
    ggplot(aes(x = refurb.L, y = amount, color = cost)) +
    geom_line() +
    scale_y_continuous(
      labels = label_dollar()
    ) +
    theme_bw()
  
  m <- mean(sim.pert.all$new.cost.mats)
  l <- mean(sim.pert.all$new.cost.labor)
  o <- mean(sim.pert.all$new.cost.outage)
  r <- rate.discount
  L <- mean(sim.pert.all$refurb.L)
  
  cost.refurb_partial.L <- function(L, m, l, o, r){
    -(m+l+o)*log(1+r)*(1+r)^(-L) 
  }
  
  tibble(refurb.L = seq(25, 50, 1)) %>% 
    mutate(
      mc = cost.refurb_partial.L(L = refurb.L, m, l, o, r)*7,
      c = c(19236547, rep(0, 25)), #16717655
      adj = 4819292,
      c = cumsum(c) + cumsum(mc) + adj
    ) %>% 
    select(-adj) %>% 
    View()
}



x <- 15866667

(x / 100)*60
