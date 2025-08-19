lavplot <- function(model = NULL, infile = NULL,
                    varlv = FALSE,
                    texfile = NULL,
                    cex = 1.3,
                    pngfile = NA_character_,
                    svgfile = NULL,
                    sloped_labels = TRUE,
                    standalone = FALSE,
                    placenodes = NULL,
                    edgelabelsbelow = NULL) {
  tmp <- lvp_get_model_info(model, infile = infile, varlv = varlv)
  tmp <- lvp_position_nodes(tmp,
                            placenodes = placenodes,
                            edgelabelsbelow = edgelabelsbelow)
  addgrid <- is.na(pngfile)
  if (!is.null(pngfile))
    lvp_make_rplot(tmp, sloped_labels = sloped_labels, pngfile = pngfile,
           addgrid = addgrid)
  if (!is.null(texfile))
    lvp_make_tikz(tmp, texfile, cex, sloped_labels, standalone)
  if (!is.null(svgfile))
    lvp_make_svg(tmp, sloped_labels = sloped_labels, svgfile,
                 standalone = standalone)
}
