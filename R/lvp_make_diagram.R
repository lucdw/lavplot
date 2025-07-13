lvp_make_diagram <- function(nodes, edges, outfile = "") {
  A <- matrix(NA, nrow = nrow(nodes), ncol = nrow(nodes))
  rijen <- max(nodes$rij)
  kolommen <- max(nodes$kolom)
  boxsize <- 0.2 / max(rijen, kolommen)
  for (j in seq.int(nrow(edges))) {
     van <- which(nodes$id == edges$naar[j])
     naar <- which(nodes$id == edges$van[j])
     A[van, naar] <- edges$label[j]
     if (edges$label[j] == "") A[van, naar] <- ""
     if (edges$tiepe[j] == "d") A[naar, van] <- ""
  }
  pos <- matrix(0, nrow = nrow(nodes), ncol = 2L)
  boxtypes <- character(nrow(nodes))
  boxcols <- character(nrow(nodes))
  for (j in seq.int(nrow(nodes))) {
    pos[j, 2L] <- 1 - nodes$rij[j] / (max(nodes$rij) + 1)
    pos[j, 1L] <- nodes$kolom[j] / (max(nodes$kolom) + 1)
    boxtypes[j] <- switch(nodes$tiepe[j],
                          lv = "circle",
                          ov = "rect",
                          wov = "rect",
                          bov = "rect",
                          cv = "hexa",
                          const = "multi")
    boxcols[j] <- switch(nodes$tiepe[j],
                          lv = "white",
                          ov = "white",
                          wov = "lightgreen",
                          bov = "lightblue",
                          cv = "white",
                          const = "white")
  }
  namen <- sub("_([[:digit:]]*)$", "[\\1]", nodes$naam)
  namen <- str2expression(namen)
  cat(
  "library(diagram)\n",
  "A <- ", deparse(A), "\n",
  "pos <- ", deparse(pos), "\n",
  "boxcols <- ", deparse(boxcols), "\n",
  "namen <- ", deparse(namen), "\n",
  "boxtypes <- ", deparse(boxtypes), "\n",
  "plotmat(A, pos=pos, lwd=1, curve=0, box.lwd=2, cex.txt=0.8, box.col=boxcols,\n",
  "        name = namen, box.cex=0.8, box.size = ", boxsize, ",arr.length=0.2,\n",
  "        arr.pos = 0.55, box.type = boxtypes, shadow.size = 0,\n",
  "        nr = 3, main='SEM diagram', box.prop = 1, self.cex = 0.4)\n",
  sep = "", file = outfile)
  return(invisible(NULL))
}
