node_elements_svg <- function(nodetiepe, noderadius, waar) {
  # define form, color and anchors for a node
  localradius <- noderadius
  if (nodetiepe == "varlv") localradius <- noderadius * .8
  ovxy <- as.integer(localradius * sqrt(0.5))
  cvxy <- as.integer(localradius * c(0.5, sqrt(0.75)))
  constxy <- cvxy
  drawit <- switch(nodetiepe,
                   lv = ,
                   varlv = paste0('<circle cx="', waar[1], '" cy="', waar[2],
                                  '" r="', localradius,
                                  '" stroke-width="2" stroke="black" fill="white"/>'),
                   ov = paste0('<rect width="', 2 * ovxy, '" height="',
                               2 * ovxy, '" x="', waar[1] - ovxy, '" y="',
                               waar[2] - ovxy,
                               '" stroke-width="2" stroke="black" fill="white" />'),
                   wov =  paste0('<rect width="', 2 * ovxy, '" height="',
                                 2 * ovxy, '" x="', waar[1] - ovxy, '" y="',
                                 waar[2] - ovxy,
                                 '" stroke-width="2" stroke="black" fill="lightblue" />'),
                   bov =  paste0('<rect width="', 2 * ovxy, '" height="',
                                 2 * ovxy, '" x="', waar[1] - ovxy, '" y="',
                                 waar[2] - ovxy,
                                 '" stroke-width="2" stroke="black" fill="lightgreen" />'),
                   cv = paste0('<polygon points="',
                               waar[1] - cvxy[1], ',', waar[2] - cvxy[2], ' ',
                               waar[1] + cvxy[1], ',', waar[2] - cvxy[2], ' ',
                               waar[1] + localradius, ',', waar[2], ' ',
                               waar[1] + cvxy[1], ',', waar[2] + cvxy[2], ' ',
                               waar[1] - cvxy[1], ',', waar[2] + cvxy[2], ' ',
                               waar[1] - localradius, ',', waar[2],
                               '" stroke-width="2" stroke="black" fill="none" />'),
                   const = paste0('<polygon points="',
                          waar[1], ',', waar[2] - localradius, ' ',
                          waar[1] + constxy[2], ',', waar[2] + constxy[1], ' ',
                          waar[1] - constxy[2], ',', waar[2] + constxy[1],
                                '" stroke-width="2" stroke="black" fill="none" />')
  )
  n <- as.integer(c(waar[1], switch(nodetiepe,
                   lv = , varlv = , const = waar[2] - localradius,
                   ov = , wov = , bov = waar[2] - ovxy,
                   cv = waar[2] - cvxy[2])))
  s <- as.integer(c(waar[1], switch(nodetiepe,
                   lv = , varlv = waar[2] + localradius,
                   ov = , wov = , bov = waar[2] + ovxy,
                   cv = waar[2]  + cvxy[2],
                   const = waar[2] + constxy[1])))
  e <- as.integer(switch(nodetiepe,
              lv = , varlv = , cv = waar + c(localradius, 0),
              ov = , wov = , bov = waar + c(ovxy, 0),
              const = waar + c(constxy[2], constxy[1])))
  w <- as.integer(switch(nodetiepe,
                         lv = , varlv = , cv = waar + c(-localradius, 0),
                         ov = , wov = , bov = waar + c(-ovxy, 0),
                         const = waar + c(-constxy[2], constxy[1])))
  ne <- as.integer(switch(nodetiepe,
               lv = , varlv = , ov = , wov = ,
               bov = waar + ovxy * c(1, -1),
               cv = waar + c(cvxy[1], -cvxy[2]),
               const = e))
  nw <- as.integer(switch(nodetiepe,
                          lv = , varlv = , ov = , wov = ,
                          bov = waar + ovxy * c(-1, -1),
                          cv = waar + c(-cvxy[1], -cvxy[2]),
                          const = w))
  se <- as.integer(switch(nodetiepe,
               lv = , varlv = , ov = , wov = ,
               bov = waar + ovxy * c(1, 1),
               cv = waar + cvxy,
               const = e))
  sw <- as.integer(switch(nodetiepe,
                          lv = , varlv = , ov = , wov = ,
                          bov = waar + ovxy * c(-1, 1),
                          cv = waar + c(-cvxy[1L], cvxy[2L]),
                          const = w))
  list(drawit = drawit, n = n, ne = ne, e = e,
       se = se, s = s, sw = sw, w = w, nw = nw)
}
lvp_make_svg <- function(nodes_edges, sloped_labels, outfile = "") {
  mlrij <- nodes_edges$mlrij
  if (is.null(mlrij))
    stop("nodes_edges hasn't been processed by lvp_position_nodes!")
  if (is.character(outfile)) {
    zz <- file(outfile, open = "w")
    closezz <- TRUE
  } else {
    zz <- outfile
    closezz <- FALSE
  }
  nodes <- nodes_edges$nodes
  edges <- nodes_edges$edges
  nodedist <- 100
  noderadius <- 0.3
  rijen <- max(nodes$rij)
  kolommen <- max(nodes$kolom)

  writeLines(c(
    '<!DOCTYPE html>',
    '<html>',
    '<body>',
    '<h2>SVG diagram created by lavplot R package</h2>',
    paste0('<svg width="', (kolommen + 1) * nodedist, '" height="',
           (rijen + 1) * nodedist,
           '" version="1.1" xmlns="http://www.w3.org/2000/svg">'),
    '<defs>',
    '  <marker id="arrow" markerWidth="12" markerHeight="8"',
    '          refX="11" refY="4" orient="auto">',
    '    <path d="M 0 0 L 12 4 L 0 8 L 4 4 z" fill="black" />',
    '  </marker>',
    '  <marker id="sarrow" markerWidth="12" markerHeight="8"',
    '          refX="1" refY="4" orient="auto">',
    '    <path d="M 0 4 L 12 0 L 8 5 L 12 8 z" fill="black" />',
    '  </marker>',
    '</defs>'),
     zz)
  vecrotate <- function(vec, angle) {
    c(cos(angle)*vec[1]+sin(angle)*vec[2],
      -sin(angle)*vec[1]+cos(angle)*vec[2])
  }
  plot_edge <- function(van, naar, label = "", dubbel = FALSE,
                        bend = 0, below = FALSE, txtcex = 0.9, id = 0) {
    labele <- lvp_format_label(label, show = FALSE)$svg
    unitvec <- (naar - van) / sqrt(sum((naar - van) * (naar - van)))
    theta <- atan2(naar[2] - van[2], naar[1] - van[1])
    if (bend == 0) { # line
      writeLines(paste0('<path id="L', id, '" d="M ', van[1L], ' ', van[2L], ' L ',
                        naar[1L], " ", naar[2L],
                        '" stroke-width="2" stroke="black" ',
                        ifelse(dubbel,'marker-start="url(#sarrow)" ', ''),
                        'marker-end="url(#arrow)" />'), zz)
      midden <- (van + naar) * 0.5
    } else {  # path Q (quadratic Bézier)
      # gebogen lijn (cirkelsegment) door van en naar met raaklijn in
      # van die hoek van bend °  maakt met unitvec
      lengte <- sqrt(sum((naar - van) * (naar - van)))
      orthovec <- c(-unitvec[2], unitvec[1]) # 90° in tegenwijzerzin
      middelpt <- van + lengte * unitvec / 2  -
        lengte * tan(pi/2 - bend) * orthovec / 2
      bezierpunt <- van + lengte * unitvec / 2  +
        lengte * tan(bend) * orthovec / 2
      writeLines(paste0('<path id="L', id, '" d="M ', van[1L], ' ', van[2L], ' Q ',
                        bezierpunt[1L], ' ', bezierpunt[2L], ' ',
                        naar[1L], " ", naar[2L],
                        '" stroke-width="2" stroke="black" fill="none" ',
                        ifelse(dubbel,'marker-start="url(#sarrow)" ', ''),
                        'marker-end="url(#arrow)" />'), zz)
      vantheta <- atan2(van[2] - middelpt[2], van[1] - middelpt[1])
      naartheta <- atan2(naar[2] - middelpt[2], naar[1] - middelpt[1])
      if (abs(naartheta-vantheta) > pi) {
        if (vantheta < naartheta) {
          vantheta <- vantheta + 2 * pi
        } else {
          naartheta <- naartheta + 2 * pi
        }
      }
      theta <- (vantheta + naartheta) / 2
      straal <- veclen(van - middelpt)
      midden <- c(middelpt[1] + cos(theta) * straal,
                  middelpt[2] + sin(theta) * straal)
    }
    if (label != "") {
      if (sloped_labels) {
        writeLines(
          c('<text font-size="25" text-anchor="middle">',
            paste0('<textPath href="#L', id, '" startOffset="50%">',
                   labele, '</textPath>'),
            '</text>'), zz)
      } else {
        if (below) {
        if (theta >= 0 && theta < pi / 2) {
          extra <- 'dy="30"'
        } else if (theta >= pi / 2) {
          extra <- 'dy="30" text-anchor="end"'
        } else if (theta < -pi/2) {
          extra <- 'dy="30"'
        } else {
          extra <- 'dy="0" text-anchor="end"'
        }
      } else {
        if (theta >= 0 && theta < pi / 2) {
          extra <- 'text-anchor="end"'
        } else if (theta >= pi / 2) {
          extra <- ' '
        } else if (theta < -pi/2) {
          extra <- 'text-anchor="end"'
        } else {
          extra <- ' '
        }
        writeLines(paste0('<text x="', midden[1L], '" y="', midden[2L],
                          '" font-size="25" ', extra, '>', labele, '</text>'),
                   zz)
        }
      }
    }
  }
  plot_var <- function(waar, noderadius, label = "", side = "n", txtcex = 0.9) {
    labele <- lvp_format_label(label, show = FALSE)$svg
    thetarange <- c(pi / 6, 11 * pi / 6)
    if (side == "s") thetarange <- thetarange + pi / 2
    if (side == "e") thetarange <- thetarange + pi
    if (side == "n") thetarange <- thetarange + 3 * pi / 2
    localradius <- noderadius * 0.8
    middelpt <- switch(side,
                       n = c(0, localradius),
                       w = c(-localradius, 0),
                       s = c(0, -localradius),
                       e = c(localradius, 0))
    middelpt <- middelpt + waar
    # cirkelsegment
    straal <- localradius
    xs <- middelpt[1] + cos(thetarange) * straal
    ys <- middelpt[2] + sin(thetarange) * straal
    writeLines(paste0(
      '<path d="M ', xs[1L], ' ', ys[1L], ' A ', straal, ' ', straal ,
      ' 0 1,1 ', xs[2L], ' ', ys[2L] , ' stroke-width="2" stroke="black" ',
      ' marker-start="url(#sarrow)" marker-end="url(#arrow)" />'
    ), zz)
    # label
    if (label != "") {
      writeLines(paste0('<text x="', middelpt[1L], '" y="', middelpt[2L],
                        '" text-anchor="middle" font-size="25">', labele,
                        '</text>'), zz)
    }
  }
  plot_node <- function(waar, tiepe, label = "", txtcex = 0.9) {
    labele <- lvp_format_label(label, show = FALSE)$svg
    elems <- node_elements_svg(tiepe, nodedist * noderadius, waar)
    writeLines(c(
      elems$drawit,
      paste0('<text x="', waar[1], '" y="', waar[2], '" fill="black" ',
             'font-size="25" dominant-baseline="central" text-anchor="middle">',
             labele, '</text>')
    ), zz)
  }

  if (mlrij > 0L) {
    writeLines(paste0('<path d="M 1 ', mlrij * nodedist, ' L ',
                      kolommen * nodedist, ' ', mlrij * nodedist, '"/>'),
               zz)
  }
  yrange <- nodedist * range(nodes$rij)
  xrange <- nodedist * range(nodes$kolom)
  midxy <- as.integer(c(mean(xrange), mean(yrange)))
  for (j in seq.int(nrow(edges))) {
    if (edges$naar[j] != edges$van[j]) {
      van <- which(nodes$id == edges$van[j])
      naar <- which(nodes$id == edges$naar[j])
      adrvan <- c(nodedist * nodes$kolom[van], nodedist * nodes$rij[van])
      elems <- node_elements_svg(nodes$tiepe[van], nodedist * noderadius, adrvan)
      adrvan <- elems[[edges$vananker[j]]]
      adrnaar <- c(nodedist * nodes$kolom[naar], nodedist * nodes$rij[naar])
      elems <- node_elements_svg(nodes$tiepe[naar], nodedist * noderadius, adrnaar)
      adrnaar <- elems[[edges$naaranker[j]]]
      if (edges$tiepe[j] != "~~") {
        plot_edge(adrvan, adrnaar, edges$label[j], dubbel = FALSE,
                  below = edges$labelbelow[j], id = j)
      } else {
        thetavan <- atan2(adrvan[2] - midxy[2], adrvan[1] - midxy[1])
        thetanaar <- atan2(adrnaar[2] - midxy[2], adrnaar[1] - midxy[1])
        deltatheta <- thetanaar - thetavan
        if (deltatheta < 0) deltatheta <- deltatheta + 2 * pi
        benddirection <- ifelse(deltatheta < pi, -1, 1)
        plot_edge(adrvan, adrnaar, edges$label[j], dubbel = TRUE,
                  below = edges$labelbelow[j],
                  bend = benddirection * pi/4,
                  id = j
        )
      }
    } else {
      van <- which(nodes$id == edges$van[j])
      adrvan <- c(nodedist * nodes$kolom[van], nodedist * nodes$rij[van])
      elems <- node_elements_svg(nodes$tiepe[van], nodedist * noderadius, adrvan)
      adrvan <- elems[[edges$vananker[j]]]
      plot_var(adrvan, noderadius, edges$label[j], edges$vananker[j])
    }
  }
  for (j in seq.int(nrow(nodes))) {
    plot_node(nodedist * c(nodes$kolom[j], nodes$rij[j]),
              nodes$tiepe[j],
              nodes$naam[j])
  }

  writeLines(c("</svg>", "</body>", "</html>"), zz)
  if (closezz) close(zz)
  return(invisible(NULL))
}
