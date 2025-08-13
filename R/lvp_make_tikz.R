lvp_make_tikz <- function(nodes_edges, outfile, cex = 1.3,
                          sloped_labels = TRUE, standalone = FALSE) {
  nodenaam <- function(nm, blk) {
    if (blk > 0L) return(gsub("_", "", paste0("B", blk, nm)))
    return(gsub("_", "", nm))
    }
  mlrij <- nodes_edges$mlrij
  if (is.null(mlrij)) stop("nodes_edges hasn't been processed by position_nodes!")
  nodes <- nodes_edges$nodes
  edges <- nodes_edges$edges
  if (is.character(outfile)) {
    zz <- file(outfile, open = "w")
    closezz <- TRUE
  } else {
    zz <- outfile
    closezz <- FALSE
  }
  if (standalone) writeLines(c(
    "\\documentclass{article}",
    "\\usepackage{amsmath, amssymb}",
    "\\usepackage{amsfonts}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage[english]{babel}",
    "\\usepackage{xcolor}",
    "\\usepackage{color}",
    "\\usepackage{tikz}"), zz)
  commstyle <- paste0("draw, minimum size=", round(6 * cex), "mm")
  writeLines (c(
    "\\usetikzlibrary {shapes.geometric}",
    "\\tikzset{",
    "bend angle=45,",">=stealth,",
    paste0("x={(", cex, "cm,0cm)}, y={(0cm,", cex, "cm)},"),
    paste0("lv/.style={circle, ", commstyle, ", thick},"),
    paste0("varlv/.style={circle, draw, minimum size=", round(4 * cex), "mm, semithick},"),
    paste0("cv/.style={regular polygon, regular polygon sides=6, ", commstyle, ", thick},"),
    paste0("ov/.style={rectangle, ", commstyle,", thick},"),
    paste0("av/.style={rectangle, fill=black!10, ", commstyle,", thick},"),
    paste0("wov/.style={rectangle, rounded corners, ", commstyle, ", thick},"),
    paste0("bov/.style={rectangle, rounded corners, ", commstyle, ", thick},"),
    paste0("const/.style={regular polygon, regular polygon sides=3, ", commstyle, ", thick}"),
    "}"), zz)
  if (standalone) writeLines("\\begin{document}", zz)
  writeLines("\\begin{tikzpicture}", zz)
  maxrij <- max(nodes$rij)
  maxcol <- max(nodes$kolom)
  if (mlrij > 0L) {
    writeLines(paste("\\draw (0, ", maxrij - mlrij, ") -- (", maxcol, ",", maxrij - mlrij,
    ");", sep = ""), zz)
  }
  for (j in seq.int(nrow(nodes))) {
    xpos <- nodes$kolom[j]
    ypos <- maxrij - nodes$rij[j]
    writeLines(paste(
      "\\node[", nodes$tiepe[j], "] (", nodenaam(nodes$naam[j], nodes$blok[j]),
      ") at (", xpos, ",", ypos, ") {",
      lvp_format_label(nodes$naam[j], show = FALSE)$tikz, "};", sep = ""), zz)
  }
  varlv <-any(nodes$tiepe == "varlv")
  for (j in seq.int(nrow(edges))) {
    van <- which(nodes$id == edges$van[j])
    vannaam <- nodenaam(nodes$naam[van], nodes$blok[van])
    naar <- which(nodes$id == edges$naar[j])
    naarnaam <- nodenaam(nodes$naam[naar], nodes$blok[naar])
    nodelabel <- lvp_format_label(edges$label[j], show = FALSE)$tikz
    if (van == naar) { # self
      if (nodes$kolom[van] == 1L) {
        writeLines(paste("\\path[<->] (", vannaam,
                         ") edge [in=160, out=-160, looseness=8] node[right] {",
                         nodelabel, "} (",
                         vannaam, ");",
                         sep = ""), zz)
      } else if (nodes$rij[van] == maxrij) {
        writeLines(paste("\\path[<->] (", vannaam,
                         ") edge [in=-110, out=-70, looseness=8] node[above] {",
                         nodelabel, "} (",
                         vannaam, ");",
                         sep = ""), zz)
      } else if (nodes$kolom[van] == maxcol) {
        writeLines(paste("\\path[<->] (", vannaam,
                         ") edge [in=20, out=-20, looseness=8] node[left] {",
                         nodelabel, "} (",
                         vannaam, ");",
                         sep = ""), zz)
      } else {
        writeLines(paste("\\path[<->] (", vannaam,
                         ") edge [in=110, out=70, looseness=8] node[below] {",
                         nodelabel, "} (",
                         vannaam, ");",
                         sep = ""), zz)
      }
    } else {
      bending <- " "
      anchorv <- anchorn <- ""
      if (edges$tiepe[j] %in% c("=~", "<~")) {
        edges$tiepe[j] <- "p"
        if (nodes$voorkeur[van] %in% c("l", "r") ||
             nodes$voorkeur[naar] %in% c("l", "r")) {
        if ((nodes$kolom[van] <= 1L + varlv ||           # composite left
             nodes$kolom[naar] >= maxcol - varlv)) {     # LV right
          anchorv <- ".east"
          anchorn <- ".west"
        } else if (nodes$kolom[naar] <= 1L + varlv ||        # LV left
                    nodes$kolom[van] >= maxcol - varlv) {    # composite right
          anchorv <- ".west"
          anchorn <- ".east"
        }} else if (nodes$voorkeur[van] %in% c("m", "b") ||
                    nodes$voorkeur[naar] %in% c("m", "b")) {
          if (nodes$rij[van] <= 1L + varlv ||           # composite left
                   nodes$rij[naar] >= maxrij - varlv) { # LV right
          anchorv <- ".south"
          anchorn <- ".north"
        } else if (nodes$rij[naar] <= 1L + varlv ||       # LV left
                   nodes$rij[van] >= maxrij - varlv) {    # composite right
          anchorv <- ".north"
          anchorn <- ".south"
        }}
      } else if (nodes$tiepe[van] == nodes$tiepe[naar] && edges$tiepe[j] == "~~")  {
        if (nodes$kolom[van] == nodes$kolom[naar] && nodes$kolom[van] %in% c(1L, maxcol)) {
          if (nodes$kolom[van] == 1L) anchorv <- anchorn <- ".west"
          if (nodes$kolom[van] == maxcol) anchorv <- anchorn <- ".east"
          if ((nodes$kolom[van] == 1L) == (nodes$rij[van] < nodes$rij[naar])) {
            bending <- " [bend right] "
          } else {
            bending <- " [bend left] "
          }
        } else if (nodes$rij[van] == nodes$rij[naar] &&
                  nodes$rij[van] %in% c(1L, maxrij)) {
          if (nodes$rij[van] == 1) anchorv <- anchorn <- ".north"
          if (nodes$rij[van] == maxcol) anchorv <- anchorn <- ".south"
          if ((nodes$rij[van] < 3L) == (nodes$kolom[van] < nodes$kolom[naar])) {
            bending <- " [bend left] "
          } else {
            bending <- " [bend right] "
          }
        }
      }
      thelabel <- lvp_format_label(edges$label[j], show = FALSE)$tikz
      if (thelabel != "") {
        thelabel <- paste0("node[" ,
                           ifelse(edges$labelbelow[j], "below", "above"),
                           ifelse(sloped_labels, ",sloped", ""),
                           "] {", thelabel, "} ")
      }
      pijl <- ifelse(edges$tiepe[j] %in% c("~~", "~~~"), "<->", "->")
      writeLines(paste("\\path[", pijl, "] (", vannaam, anchorv, ") edge",
                bending, thelabel, "(", naarnaam, anchorn, ");", sep = ""), zz)
    }
  }
  writeLines("\\end{tikzpicture}", zz)
  if(standalone) writeLines("\\end{document}", zz)
  if (closezz) close(zz)
  return(invisible(NULL))
}
