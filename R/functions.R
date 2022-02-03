#`Plots a firm's cost curves given its total cost function in terms of quantity supplied.`
#' @export
Cost_curves <- function(TC_formula="(q^2) + 100",qmin = 0,qmax = 25) {
  TC_formula <- parse(text = TC_formula)
  MC_formula <- D(parse(text =TC_formula),"q")
  z = 0
  FC <- eval(parse(text = gsub("q","z",TC_formula)))
  data <- dplyr::mutate(data.frame(q = qmin:qmax),TC = eval(TC_formula),AC = TC/q, MC = eval(MC_formula),AVC = TC-TC[1])
    ggplot2::ggplot() + ggplot2::geom_line(data=data,ggplot2::aes(x = q, y = TC, col = "Total Cost"),size = 2) +
    ggplot2::geom_line(data=data,ggplot2::aes(x = q, y = AC, col = "Average Cost"),size = 1.5) +
    ggplot2::geom_line(data=data,ggplot2::aes(x = q, y = MC, col = "Marginal Cost"), size = 1.5) +
    ggplot2::geom_line(data=data,ggplot2::aes(x = q, y = AVC, col = "Average Variable Cost"), size = 1.5) +
    ggplot2::scale_color_manual(values = c("red","blue","green","black")) +
    ggplot2::labs(title = "Cost Curves",subtitle = paste("Total Cost function:",TC_formula)) +
    ggplot2::ylab("Cost ($)") + ggplot2::xlab("Quantity Produced (q)") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5),plot.subtitle = ggplot2::element_text(hjust = .5)) +
    ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0)
}

#`Plots a set of indifference curves given a utility function of 2 variables`
#' @export
Utility_curves <- function(utility_formula="(x*y)^.5",xmin=1,xmax=100,ymin=1,ymax=100) {
  utility_formula <- parse(text = utility_formula)
  data <- dplyr::mutate(expand.grid(data.frame(
    x = seq(from = xmin, to = xmax,by = .01),
    y = seq(from = ymin, to = ymax,by = .01)
  )), u = round(eval(utility_formula),2))
  ggplot2::ggplot(data = dplyr::filter(data,u %in% data$u[c(250,500,750,1000)])) + ggplot2::geom_line(ggplot2::aes(x = x, y = y, group = u)) + ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0) + ggplot2::labs(title = "Indifference Curves",subtitle = paste(utility_formula)) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5),plot.subtitle = ggplot2::element_text(hjust=.5))
}

