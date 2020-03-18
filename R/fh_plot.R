# generic function for plotting
fh_plot = function(data, covs_list) {
  data %>%
    select(qrtl, one_of(covs_list)) %>%
    gather(variable, value, -qrtl) %>%
    ggplot(aes(x = value,
               color = qrtl,
               fill = qrtl)) +
    geom_density(alpha = 0.3) +
    theme_bw() +
    labs(x = "Value",
         y = "Density",
         color = "Category",
         fill = "Category") +
    facet_wrap( ~ variable,
                scales = "free")
}
