lavplot <- function(model = NULL,
                    infile = NULL,
                    varlv = FALSE,
                    allowbottom = FALSE,
                    cex = 1.3,
                    texfile = NULL,
                    standalone = FALSE,
                    pngfile = NULL,
                    htmlfile = NULL,
                    sloped_labels = TRUE,
                    placenodes = NULL,
                    edgelabelsbelow = NULL,
                    verbose = FALSE) {
  tmp <- lvp_get_model_info(model, infile = infile, varlv = varlv)
  tmp <- lvp_position_nodes(tmp, allowbottom = allowbottom)
  if (verbose) {
    print(tmp$nodes, row.names = FALSE)
    print(tmp$edges, row.names = FALSE)
  }
  if (!is.null(placenodes)) {
    for (nn in names(placenodes)) {
      w <- which(tmp$nodes$naam == nn)
      if (length(w) == 0) {
        warning("placenodes: node name", nn, "not found!")
      }
      tmp$nodes$rij[w] <- placenodes[[nn]][1L]
      tmp$nodes$kolom[w] <- placenodes[[nn]][2L]
      edg <- which((tmp$edges$van == tmp$nodes$id[w] |
                      tmp$edges$naar == tmp$nodes$id[w]) &
                     tmp$edges$tiepe == "~")
      if (length(edg) > 0L) tmp$edges$vananker[edg] <- NA_character_
    }
    tmp$edges <- complete_anchors(tmp$nodes, tmp$edges)
  }
  if (!is.null(edgelabelsbelow)) {
    for (i in seq_along(edgelabelsbelow)) {
      n1 <- which(tmp$nodes$naam == edgelabelsbelow[[i]][1L])
      if (length(n1) == 0) {
        warning("edgelabelsbelow: node name", edgelabelsbelow[[i]][1L], "not found!")
      }
      n2 <- which(tmp$nodes$naam == edgelabelsbelow[[i]][2L])
      if (length(n2) == 0) {
        warning("edgelabelsbelow: node name", edgelabelsbelow[[i]][2L], "not found!")
      }
      ed <- which(tmp$edges$van == tmp$nodes$id[n1] & tmp$edges$naar == tmp$nodes$id[n2])
      if (length(ed) == 0L) {
        ed <- which(tmp$edges$naar == tmp$nodes$id[n1] & tmp$edges$van == tmp$nodes$id[n2])
      }
      if (length(ed) == 0L) {
        warning("edgelabelsbelow: edge", tmp$nodes$naam[n1], "--", tmp$nodes$naam[n2], "not found!")
      }
      tmp$edges$labelbelow[ed] <- TRUE
    }
  }
  addgrid <- TRUE
  if (!is.null(pngfile)) addgrid <- FALSE
  lvp_plot(tmp, sloped_labels = sloped_labels, pngfile = pngfile,
           addgrid = addgrid)
  if (!is.null(texfile))
    lvp_make_tikz(tmp, texfile, cex, sloped_labels, standalone)
  if (!is.null(htmlfile))
    lvp_make_svg(tmp, sloped_labels = sloped_labels, htmlfile)
}
