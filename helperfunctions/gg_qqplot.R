gg_qqplot = function (pvalues, ci = 0.95, maf = NULL, info = NULL, mafcutoff = 0.05, infocutoff = 0.8) {
  df = data.table::data.table(pvalues, maf, info)
  data.table::setorder(df, pvalues)
  n <- nrow(df)
  df[, `:=`(observed, -log10(pvalues))]
  df[, `:=`(expected, -log10(ppoints(n)))]
  df[, `:=`(clower, -log10(qbeta(p = (1 - ci)/2, shape1 = 1:n, 
                                 shape2 = n:1)))]
  df[, `:=`(cupper, -log10(qbeta(p = (1 + ci)/2, shape1 = 1:n, 
                                 shape2 = n:1)))]
  log10Pe <- expression(paste("Expected -log"[10], plain(P)))
  log10Po <- expression(paste("Observed -log"[10], plain(P)))
  p1pre = ggplot2::ggplot(df) + ggplot2::geom_ribbon(mapping = ggplot2::aes(x = expected, 
                                                                            ymin = clower, ymax = cupper), alpha = 0.3)
  if (is.null(maf) == F & is.null(info) == F) 
    p2pre = p1pre + ggplot2::geom_point(ggplot2::aes(expected, 
                                                     observed, col = maf < 0.05, pch = info < 0.8), size = 1)
  if (is.null(maf) == F & is.null(info) == T) 
    p2pre = p1pre + ggplot2::geom_point(ggplot2::aes(expected, 
                                                     observed, col = maf < 0.05), size = 1)
  if (is.null(maf) == T & is.null(info) == F) 
    p2pre = p1pre + ggplot2::geom_point(ggplot2::aes(expected, 
                                                     observed, pch = info < 0.8), size = 1)
  if (is.null(maf) == T & is.null(info) == T) 
    p2pre = p1pre + ggplot2::geom_point(ggplot2::aes(expected, 
                                                     observed), size = 1)
  p3 = p2pre + ggplot2::geom_abline(intercept = 0, slope = 1, 
                                    alpha = 0.5) + ggplot2::xlab(log10Pe) + ggplot2::ylab(log10Po) + 
    ggplot2::theme_classic(base_size = 14) + ggplot2::theme(axis.ticks = ggplot2::element_line(size = 0.5), 
                                                            panel.grid = ggplot2::element_blank(), legend.position = c(0.2, 
                                                                                                                       0.7)) + ggplot2::scale_color_manual(values = c("black", 
                                                                                                                                                                      "red")) + ggplot2::scale_shape_manual(values = c(1, 2)) + 
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) + 
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6))
  p3
}
