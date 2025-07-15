lavplot <- function(model = NULL,
                    infile = NULL,
                    varlv = FALSE,
                    allowbottom = FALSE,
                    cex = 1.3,
                    outfile = NULL,
                    outformat = c("tikz", "svg", "diagram"),
                    placenodes = NULL,
                    edgelabelsbelow = NULL,
                    verbose = FALSE) {
  outformat <- match.arg(outformat)
  tmp <- lvp_get_model_info(model, infile = infile, varlv = varlv)
  nodes <- lvp_position_nodes(tmp$nodes, tmp$edges, allowbottom = allowbottom)
  edges <- tmp$edges
  if (verbose) {
    print(nodes, row.names = FALSE)
    print(edges, row.names = FALSE)
  }
  if (!is.null(placenodes)) {
    for (nn in names(placenodes)) {
      w <- which(nodes$naam == nn)
      if (length(w) == 0) {
        warning("placenodes: node name", nn, "not found!")
      }
      nodes$rij[w] <- placenodes[[nn]][1L]
      nodes$kolom[w] <- placenodes[[nn]][2L]
    }
  }
  if (!is.null(edgelabelsbelow)) {
    for (i in seq_along(edgelabelsbelow)) {
      n1 <- which(nodes$naam == edgelabelsbelow[[i]][1L])
      if (length(n1) == 0) {
        warning("edgelabelsbelow: node name", edgelabelsbelow[[i]][1L], "not found!")
      }
      n2 <- which(nodes$naam == edgelabelsbelow[[i]][2L])
      if (length(n2) == 0) {
        warning("edgelabelsbelow: node name", edgelabelsbelow[[i]][2L], "not found!")
      }
      ed <- which(edges$van == nodes$id[n1] & edges$naar == nodes$id[n2])
      if (length(ed) == 0L) {
        ed <- which(edges$naar == nodes$id[n1] & edges$van == nodes$id[n2])
      }
      if (length(ed) == 0L) {
        warning("edgelabelsbelow: edge", nodes$naam[n1], "--", nodes$naam[n2], "not found!")
      }
      edges$labelbelow[ed] <- TRUE
    }
  }
  if (outformat == "tikz") {
    lvp_make_tikz(nodes, edges, cex, outfile)
  } else if (outformat == "svg") {
    lvp_make_tikz(nodes, edges, outfile)
  } else {
    lvp_make_diagram(nodes, edges, outfile)
  }
}
