lavplot <- lav_plot <- function(
  model = NULL,
  infile = NULL,
  varlv = FALSE,
  placenodes = NULL,
  edgelabelsbelow = NULL,
  common_opts = list(sloped_labels = TRUE,
                     mlovcolors = c("lightgreen", "lightblue")),
  rplot = list(outfile = "",
               addgrid = TRUE),
  tikz = list(outfile = "",
              cex = 1.3,
              standalone = FALSE),
  svg = list(outfile = "",
             strokeWidth = 2L,
             svgFontSize = 20L,
             svgIdxFontSize = 15L,
             svgDy = 5L,
             standalone = FALSE)
) {
  tmp <- lav_get_model_info(model,
                            infile = infile,
                            varlv = varlv)
  tmp <- lav_position_nodes(tmp,
                            placenodes = placenodes,
                            edgelabelsbelow = edgelabelsbelow)
  mc <- match.call()
  create_rplot <- !is.null(mc$rplot) || (is.null(mc$tikz) && is.null(mc$svg))
  if (create_rplot)
    do.call(lav_make_rplot, c(list(nodes_edges = tmp),
                                   common_opts,
                                   rplot))
  if (!is.null(mc$tikz))
    do.call(lav_make_tikz, c(list(nodes_edges = tmp),
                             common_opts,
                             tikz))
  if (!is.null(mc$svg))
    do.call(lav_make_svg, c(list(nodes_edges = tmp),
                             common_opts,
                             svg))
}
