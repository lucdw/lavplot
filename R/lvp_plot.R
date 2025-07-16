lvp_plot <- function(nodes, edges) {
  noderadius <- 0.25
  arrowangle <- pi * 30 / 180
  arrowlength <- noderadius / 3
  vecrotate <- function(vec, angle) {
    c(cos(angle)*vec[1]+sin(angle)*vec[2],
      -sin(angle)*vec[1]+cos(angle)*vec[2])
  }
  plot_edge <- function(van, naar, label = "", dubbel = FALSE, bend = 0) {
    unitvec <- (naar - van) / sqrt(sum((naar - van) * (naar - van)))
    if (bend == 0) {
      vanpt <- van + unitvec * noderadius
      naarpt <- naar - unitvec * noderadius
      if (dubbel) {
        args <- rbind(vanpt, vanpt + vecrotate(unitvec*arrowlength, arrowangle),
                      vanpt + vecrotate(unitvec*arrowlength, -arrowangle),
                      vanpt, naarpt,
                      naarpt + vecrotate(-unitvec*arrowlength, arrowangle),
                      naarpt + vecrotate(-unitvec*arrowlength, -arrowangle),
                      naarpt)
      } else {
        args <- rbind(vanpt, naarpt,
                      naarpt + vecrotate(-unitvec*arrowlength, arrowangle),
                      naarpt + vecrotate(-unitvec*arrowlength, -arrowangle),
                      naarpt)
      }
      lines(args, lwd = 2)
    } else {
      # gebogen lijn (cirkelsegment) door van en naar met raaklijn in
      # van die hoek van bend °  maakt met unitvec

    }
  }
  rijen <- max(nodes$rij)
  kolommen <- max(nodes$kolom)
  namen <- sub("_([[:digit:]]*)$", "[\\1]", nodes$naam)
  namen <- str2expression(namen)
  opar <- par(mar = c(1L,1L, 1L,1L) + 0.1)
  plot.default(x = c(0, kolommen + 1), c(0, rijen + 1), type="n",
               xlab = "", ylab = "", axes = FALSE, asp = 1)
  grid()
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
                          varlv = "circle",
                          ov = "rect",
                          wov = "rect",
                          bov = "rect",
                          cv = "hexa",
                          const = "multi")
    boxcols[j] <- switch(nodes$tiepe[j],
                          lv = "white",
                          varlv = "lightgrey",
                          ov = "white",
                          wov = "lightgreen",
                          bov = "lightblue",
                          cv = "white",
                          const = "white")
  }
  # plotmat(A, pos=pos, lwd=1, curve=0, box.lwd=2, cex.txt=0.8, box.col=boxcols,
  #        name = namen, box.cex=0.8, box.size = ", boxsize, ",arr.length=0.2,
  #        arr.pos = 0.55, box.type = boxtypes, shadow.size = 0,
  #        nr = 3, main='SEM diagram', box.prop = 1, self.cex = 0.4)
  return(invisible(NULL))
}
