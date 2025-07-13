lavplot <- function(model = NULL,
                    infile = NULL,
                    varlv = FALSE,
                    allowbottom = FALSE,
                    cex = 1.75,
                    outfile = NULL,
                    outformat = c("tikz", "svg", "diagram")) {
  outformat <- match.arg(outformat)
  tmp <- lvp_get_model_info(model, infile = infile, varlv = varlv)
  nodes <- lvp_position_nodes(tmp$nodes, tmp$edges, allowbottom = allowbottom)
  if (outformat == "tikz") {
    lvp_make_tikz(nodes, tmp$edges, cex, outfile)
  } else if (outformat == "svg") {
    lvp_make_tikz(nodes, tmp$edges, outfile)
  } else {
    lvp_make_diagram(nodes, tmp$edges, outfile)
  }
}
