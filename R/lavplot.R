lavplot <- function(model = NULL,
                    infile = NULL,
                    varlv = FALSE,
                    allowbottom = FALSE,
                    cex = 1.3,
                    texfile = NULL,
                    standalone = FALSE,
                    pngfile = NULL,
                    svgfile = NULL,
                    sloped_labels = TRUE,
                    placenodes = NULL,
                    edgelabelsbelow = NULL) {
  tmp <- lvp_get_model_info(model, infile = infile, varlv = varlv)
  tmp <- lvp_position_nodes(tmp,
                            allowbottom = allowbottom,
                            placenodes = placenodes,
                            edgelabelsbelow = edgelabelsbelow)
  addgrid <- TRUE
  if (!is.null(pngfile)) addgrid <- FALSE
  lvp_make_rplot(tmp, sloped_labels = sloped_labels, pngfile = pngfile,
           addgrid = addgrid)
  if (!is.null(texfile))
    lvp_make_tikz(tmp, texfile, cex, sloped_labels, standalone)
  if (!is.null(svgfile))
    lvp_make_svg(tmp, sloped_labels = sloped_labels, svgfile,
                 standalone = standalone)
}
