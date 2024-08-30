#library 
library(tidyverse)


par_plot_func <- function(x, vars, vars_type = NULL){
  
  df_box <- list("vector", length(vars))
  for (i in (1:length(vars))){
    df_box[[i]] <- treezy::partial_dependence(x, vars[[i]])
  }
  
  df <- dplyr::bind_rows(df_box)
  
  if(str_detect(vars_type, "o2_0m")){
    df <- df %>% 
      mutate(variable = as.factor(variable), 
             variable = fct_relevel(variable, c("o2_mean_0m", "o2_mean_0m_seas", "o2_mean_0m_ann")))
  }
  if(str_detect(vars_type, "o2_250m")){
    df <- df %>% 
      mutate(variable = as.factor(variable), 
             variable = fct_relevel(variable, c("o2_mean_250m", "o2_mean_250m_seas", "o2_mean_250m_ann")))
  }
  if(str_detect(vars_type, "AGI_0m")){
    df <- df %>% 
      mutate(variable = as.factor(variable), 
             variable = fct_relevel(variable, c("AGI_0m", "AGI_0m_seas", "AGI_0m_ann")))
  }
  if(str_detect(vars_type, "AGI_250m")){
    df <- df %>% 
      mutate(variable = as.factor(variable), 
             variable = fct_relevel(variable, c("AGI_250m", "AGI_250m_seas", "AGI_250m_ann")))
  }
  
  df_mean <-
    df %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(mean = mean(value))
  
  if(vars_type == "o2_0m" | vars_type == "o2_250m"){
    par_plot <- ggplot(data = df, aes(x = value, y = fitted_function, color = variable)) + 
      geom_line(linewidth = 2)+
      labs(x = bquote("Dissolved oxygen"~(mmol/m^3)), 
           y = "Probability of presence", 
           color = "Temporal resolution") + 
      scale_color_manual(values = MetBrewer::met.brewer("OKeeffe2", n = length(vars), direction = -1), labels = c("Daily", "Seasonal", "Annual"))+
      theme_minimal()+
      theme(axis.title = element_text(size = 16), 
            axis.text = element_text(size = 14), 
            legend.title = element_text(size = 16), 
            legend.text = element_text(size = 14), 
            panel.grid = element_blank())
    }

  if(vars_type == "AGI_0m"){
    par_plot <- ggplot(data = df, aes(x = value, y = fitted_function, color = variable)) + 
      geom_line(linewidth = 2)+
      facet_wrap(~variable, scales = "free_x")+
      labs(x = "AGI", 
           y = "Probability of presence", 
           color = "Temporal resolution") + 
      scale_color_manual(values = MetBrewer::met.brewer("OKeeffe2", n = length(vars), direction = -1), labels = c("Daily", "Seasonal", "Annual"))+
      theme_minimal()+
      theme(strip.text = element_blank(),
            axis.title = element_text(size = 16), 
            axis.text = element_text(size = 14), 
            legend.title = element_text(size = 16), 
            legend.text = element_text(size = 14), 
            panel.grid = element_blank())
  }
  
  if(vars_type == "AGI_250m"){
    par_plot <- ggplot(data = df, aes(x = value, y = fitted_function, color = variable)) + 
      geom_line(linewidth = 2)+
      labs(x = "AGI", 
           y = "Probability of presence", 
           color = "Temporal resolution") + 
      scale_color_manual(values = MetBrewer::met.brewer("OKeeffe2", n = length(vars), direction = -1), labels = c("Daily", "Seasonal", "Annual"))+
      theme_minimal()+
      theme(axis.title = element_text(size = 16), 
            axis.text = element_text(size = 14), 
            legend.title = element_text(size = 16), 
            legend.text = element_text(size = 14), 
            panel.grid = element_blank())
  }
  
  
  return(par_plot)
}
