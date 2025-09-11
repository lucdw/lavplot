BeziersControlPunt <- function(van, naar, wvannaar, maxrij, maxcol) {
  dif <- (abs(van[1L] - naar[1L]) + abs(van[2L] - naar[2L]) - 1) /
    (maxrij + maxcol - 2)
  p <- switch(wvannaar,
              ne = , en = c(1, maxcol) + c(-dif, dif),
              nw = , wn = c(1, 1) + c(-dif, -dif),
              se = , es = c(maxrij, maxcol) + c(dif, dif),
              sw = , ws = c(maxrij, 1) + c(dif, -dif)
  )
  2 * (p - 0.25 * (van + naar))
}
veclen <- function(x) sqrt(sum(x*x))
delta_nodes <- function(node1, node2) {
  if (any(is.na(node2))) return(-1)
  delta <- node2 - node1
  veclen(delta)
}
delta_node_edge <- function(edgevan, edgenaar, node) {
  van <- edgevan - node
  naar <- edgenaar - node
  edgevec <- edgevan - edgenaar
  if (sum(van * edgevec) * sum(naar * edgevec) >= 0) {
    # loodlijn niet op lijnstuk
    return(min(veclen(van), veclen(naar)))
  }
  return(veclen(van) *
           sqrt(1 - sum(edgevec * (-van))**2 /
                  (sum(edgevec*edgevec)*sum(van*van))))
}
search_position <- function(j, nodes, edges) {
  if (all(is.na(nodes$rij))) {
    availcols <- availrows <- c(1L, 3L)
  } else {
    availrows <- range(nodes$rij, na.rm = TRUE)
    availcols <- range(nodes$kolom, na.rm = TRUE)
    availrows[1L] <- availrows[1] - 1L
    availrows[2L] <- availrows[2L] + 1L
    availcols[1L] <- availcols[1] - 1L
    availcols[2L] <- availcols[2L] + 1L
  }
  availpositions <- list((availrows[2L] - availrows[1L] + 1L) *
                           (availcols[2L] - availcols[1L] + 1L))
  positie <- 0L
  nodestocheck <- nodes[!is.na(nodes$rij), ]
  for (trij in seq.int(availrows[1L], availrows[2L])) {
    for (tkol in seq.int(availcols[1L], availcols[2L])) {
      if (any(nodestocheck$rij == trij & nodestocheck$kolom == tkol)) next
      positie <- positie + 1L
      availpositions[[positie]] <- c(trij, tkol)
    }
  }
  availpositions <- availpositions[seq_len(positie)]
  mogelijk <- vapply(availpositions, function(availpos) {
    distances <- vapply(seq_len(nrow(edges)), function(i) {
      if (edges$van[i] != edges$naar[i] &&
          !is.na(nodes$rij[edges$van[i]]) &&
          !is.na(nodes$rij[edges$naar[i]])) {
        afstand <- delta_node_edge(
          c(nodes$rij[edges$van[i]], nodes$kolom[edges$van[i]]),
          c(nodes$rij[edges$naar[i]], nodes$kolom[edges$naar[i]]),
          availpos
        )
        afstand
      } else {
        99
      }
    }, 0.0)

    distances2 <- vapply(seq_len(nrow(edges)), function(i) {
      if (edges$van[i] != edges$naar[i] &&
          !is.na(nodes$rij[edges$van[i]]) &&
          edges$naar[i] == nodes$naam[j]) {
        afstand <- min(vapply(seq_along(nodestocheck$rij), function(k) {
          delta_node_edge(
            c(nodes$rij[edges$van[i]], nodes$kolom[edges$van[i]]),
            availpos,
            c(nodes$rij[k], nodes$kolom[k]))
        }, 0.0)
        )
        afstand
      } else {
        99
      }
    }, 0.0)

    distances3 <- vapply(seq_len(nrow(edges)), function(i) {
      if (edges$van[i] != edges$naar[i] &&
          !is.na(nodes$rij[edges$naar[i]]) &&
          edges$van[i] == nodes$naam[j]) {
        afstand <- min(vapply(seq_along(nodestocheck$rij), function(k) {
          delta_node_edge(
            c(nodes$rij[edges$naar[i]], nodes$kolom[edges$naar[i]]),
            availpos,
            c(nodes$rij[k], nodes$kolom[k]))
        }, 0.0)
        )
        afstand
      } else {
        99
      }
    }, 0.0)
    if (min(c(distances, distances2, distances3)) > 0.5) return(TRUE)
    FALSE
  }, TRUE)
  availpositions <- availpositions[mogelijk]
  meandistances <- vapply(availpositions, function(availpos) {
    distances <- vapply(seq_len(nrow(edges)), function(i) {
      if (edges$van[i] == j && edges$naar[i] != j) {
        return(delta_nodes(availpos, c(nodes$rij[edges$naar[i]],
                                       nodes$kolom[edges$naar[i]])))
      }
      if (edges$naar[i] == j && edges$van[i] != j) {
        return(delta_nodes(availpos, c(nodes$rij[edges$van[i]],
                                       nodes$kolom[edges$van[i]])))
      }
      -1
    }, 0.0)
    if (all(distances < 0)) {
      bestposition <- as.integer(c(mean(availrows), mean(availcols)))
      delta_nodes(availpos, bestposition)
    } else {
      mean(distances[distances > 0])
    }
  }, 0.0)
  mindistance <- min(meandistances)
  k <- which(meandistances == mindistance)[1L]
  availpositions[[k]]
}

complete_anchors <- function(nodes, edges) {
  if (all(!is.na(edges$vananker))) return(edges)
  adaptedges <- which(is.na(edges$vananker))
  breaks <- c(-pi - 0.01, -7 * pi / 8, -5 * pi / 8, -3 * pi / 8, -pi / 8,
              pi / 8, 3 * pi / 8, 5 * pi / 8, 7 * pi / 8, pi + 0.01)
  winds <- c("w", "sw", "s", "se", "e", "ne", "n", "nw", "w")
  for (i in adaptedges) {
    nodevan <- which(nodes$id == edges$van[i])
    nodenaar <- which(nodes$id == edges$naar[i])
    rrij <- range(nodes$rij)
    rkol <- range(nodes$kol)
    if (edges$tiepe[i] == "~~~") {
      if (nodes$kolom[nodevan] == rkol[1L]) {
        edges$vananker[i] <- "w"
        edges$naaranker[i] <- "w"
      } else if (nodes$kolom[nodevan] == rkol[2L]) {
        edges$vananker[i] <- "e"
        edges$naaranker[i] <- "e"
      } else if (nodes$rij[nodevan] == rrij[1L]) {
        edges$vananker[i] <- "n"
        edges$naaranker[i] <- "n"
      } else if (nodes$rij[nodevan] == rrij[2L]) {
        edges$vananker[i] <- "s"
        edges$naaranker[i] <- "s"
      } else {
        edges$vananker[i] <- "n"
        edges$naaranker[i] <- "n"
      }
      next
    }
    if (edges$tiepe[i] == "~~") {
      if (nodes$kolom[nodevan] == nodes$kolom[nodenaar] &&
          nodes$kolom[nodevan] %in% range(nodes$kolom, na.rm=TRUE)) {
        midden <- mean(range(nodes$kolom))
        if (nodes$kolom[nodevan] < midden) {
          edges$vananker[i] <- "w"
          edges$naaranker[i] <- "w"
        } else {
          edges$vananker[i] <- "e"
          edges$naaranker[i] <- "e"
        }
        next
      } else if (nodes$rij[nodevan] == nodes$rij[nodenaar] &&
                 nodes$rij[nodevan] %in% range(nodes$rij, na.rm=TRUE)) {
        midden <- mean(range(nodes$rij))
        if (nodes$rij[nodevan] < midden) {
          edges$vananker[i] <- "n"
          edges$naaranker[i] <- "n"
        } else {
          edges$vananker[i] <- "s"
          edges$naaranker[i] <- "s"
        }
        next
      }
    }
    hoek <- atan2(nodes$rij[nodevan] - nodes$rij[nodenaar],
                  nodes$kolom[nodenaar] - nodes$kolom[nodevan])
    wind <- cut(hoek, breaks, winds)
    edges$vananker[i] <- as.character(wind)
    if (hoek > 0) hoek <- hoek - pi else hoek <- hoek + pi
    wind <- cut(hoek, breaks, winds)
    edges$naaranker[i] <- as.character(wind)
  }
  edges
}
lav_position_nodes <- function(nodes.edges,
                               placenodes = NULL,
                               edgelabelsbelow = NULL,
                               group.covar.indicators = FALSE) {
  #### lav_position_nodes MAIN ####
  nodes <- nodes.edges$nodes
  nodes$rij <- NA_integer_
  nodes$kolom <- NA_integer_
  edges <- nodes.edges$edges
  edges$vananker <- NA_character_
  edges$naaranker <- NA_character_
  edges$controlpt.kol <- NA_real_
  edges$controlpt.rij <- NA_real_
  edges$labelbelow <- FALSE
  if (length(nodes$rij) == 1L) { # Only 1 node !
    nodes$rij[1L] <- 1L
    nodes$kolom[1L] <- 1L
    return(list(nodes = nodes, edges = edges, mlrij = 0L))
  }
  if (any(nodes$blok > 0L)) { # Multilevel, only level:1 and level:2 accepted
    nodes1 <- nodes[nodes$blok >= 2L, ]
    edges1 <- edges[edges$van %in% nodes1$id, ]
    nodes1$blok <- 0L
    nodes2 <- nodes[nodes$blok == 1L, ]
    edges2 <- edges[edges$van %in% nodes2$id, ]
    nodes2$blok <- 0L
    result1 <- lav_position_nodes(list(nodes = nodes1, edges = edges1))
    result2 <- lav_position_nodes(list(nodes = nodes2, edges = edges2))
    rijen1 <- max(result1$nodes$rij)
    result2$nodes$rij <- result2$nodes$rij + rijen1 + 1L
    result1$nodes$blok <- 2L
    result2$nodes$blok <- 1L
    nodes <- rbind(result1$nodes, result2$nodes)
    edges <- rbind(result1$edges, result2$edges)
    return(list(nodes = nodes, edges = edges, mlrij = rijen1 + 1L))
  }
  structnodes <- setdiff(nodes$id, union(edges$naar[edges$tiepe == "=~"],
                                         edges$van[edges$tiepe == "<~"]))
  structnodeindicators <- lapply(structnodes, function(sn) {
    union(edges$naar[edges$tiepe == "=~" & edges$van == sn],
          edges$van[edges$tiepe == "<~" & edges$naar == sn])
  })

  # create partition of structnodes via column sidegroup
  nodes$sidegroup <- rep(0L, length(nodes$naam))
  for (j in seq_along(structnodes)) {
    k <- which(nodes$id == structnodes[j])
    if (j == 1L) {
      nodes$sidegroup[k] <- j
      next
    }
    for (jj in seq.int(1L, j - 1L)) {
      if (any(structnodeindicators[[j]] %in% structnodeindicators[[jj]])) {
        nodes$sidegroup[k] <- nodes$sidegroup[structnodes[jj]]
        next
      }
      if (group.covar.indicators) {
        if (any(outer(structnodeindicators[[j]], structnodeindicators[[jj]],
            function(ind1, ind2) {
              sapply(seq_along(ind1), function(i) {
              length(edges$id[edges$van == ind1[i] & edges$naar == ind2[i]]) +
              length(edges$id[edges$naar == ind1[i] & edges$van == ind2[i]]) >
              0L
              })
        }))) {
          nodes$sidegroup[k] <- nodes$sidegroup[structnodes[jj]]
        }
      }
      if (nodes$sidegroup[k] > 0L) break
    }
    if (nodes$sidegroup[k] == 0L) nodes$sidegroup[k] <- j
  }

  # modify voorkeur to be the same within each partition group
  for (groep in unique(nodes$sidegroup)) {
    if (groep > 0L) {
      if (any(nodes$voorkeur[nodes$sidegroup == groep] == "l")) {
        nodes$voorkeur[nodes$sidegroup == groep] = "l"
      }
      if (any(nodes$voorkeur[nodes$sidegroup == groep] == "r")) {
        nodes$voorkeur[nodes$sidegroup == groep] = "r"
      }
    }
  }
  # number of groups per side (l, m or r)
  sides <- c("l", "m", "r")
  nbgroup <- rep(0L, 3)
  nbindic <- rep(0L, 3)
  for (k in seq_along(sides)) {
    nbgroup[k] <- length(unique(nodes$sidegroup[nodes$voorkeur == sides[k]]))
    welke <- nodes$id[nodes$voorkeur == sides[k]]
    nbindic[k] <-
      length(unique(edges$naar[edges$tiepe=="=~" & edges$van %in% welke])) +
      length(unique(edges$van[edges$tiepe=="<~" & edges$naar %in% welke]))
  }
  # move last 'm' groups to bottom if possible
  bottomoccupied <- FALSE
  if (nbgroup[2L] > 1L && nbindic[2L] > 8L) {
    mgroepen <- nodes$sidegroup[nodes$voorkeur == "m"]
    mgroep <- mgroepen[length(mgroepen)]
    nodes$voorkeur[nodes$sidegroup == mgroep] <- "b"
    bottomoccupied <- TRUE
  } else {
    # move last 'l' group to bottom if possible
    if (nbgroup[1L] > 1L && nbindic[1L] > 8L) {
      lgroepen <- nodes$sidegroup[nodes$voorkeur == "l"]
      lgroep <- lgroepen[length(lgroepen)]
      nodes$voorkeur[nodes$sidegroup == lgroep] <- "b"
      bottomoccupied <- TRUE
    }
    # move first 'r' group to top or last r group to bottom, if possible
    if (nbgroup[3L] > 1L && nbindic[3L] > 8L) {
      rgroepen <- nodes$sidegroup[nodes$voorkeur == "r"]
      if (nbgroup[2L] == 0L) {
        rgroep <- lgroepen[1L]
        nodes$voorkeur[nodes$sidegroup == rgroep] <- "m"
      } else {
        if (!bottomoccupied) {
          rgroep <- lgroepen[length(rgroepen)]
          nodes$voorkeur[nodes$sidegroup == rgroep] <- "b"
          bottomoccupied <- TRUE
        }
      }
    }
  }
  nodes$sidegroup <- NULL
  varlvs <- which(nodes$tiepe == "varlv")
  varlv <- length(varlvs) > 0L
  d_lv <- ifelse(varlv, 3L, 2L)  # distance from border
  d_ind <- ifelse(varlv, 2L, 1L) # depends on presence varlv's
  # structural part + measurement
  lvcvs <- nodes$tiepe %in% c("lv", "cv")
  if (any(lvcvs)) {
    for (side in c("l", "r", "m", "b")) {
      curi <- d_lv
      indicatorsassigned <- integer(0L)
      strucs <- which(nodes$voorkeur == side & lvcvs)
      for (k in strucs) {
        lvs <- edges$tiepe == "=~" & edges$van == nodes$id[k]
        cvs <- edges$tiepe == "<~" & edges$naar ==  nodes$id[k]
        edges$vananker[lvs] <- switch(side, l="w", r="e", m="n", b="s")
        edges$naaranker[lvs] <- switch(side, l="e", r="w", m="s", b="n")
        edges$vananker[cvs] <- switch(side, l="e", r="w", m="s", b="n")
        edges$naaranker[cvs] <- switch(side, l="w", r="e", m="n", b="s")
        indics <- unique(c(edges$naar[lvs], edges$van[cvs]))
        indicstoplace <- setdiff(indics, indicatorsassigned)
        indicatorsassigned <- union(indicatorsassigned, indics)
        if (side == "l" || side == "r") {
          nodes$rij[k] <- curi + as.integer(length(indicstoplace)/2.01)
          nodes$kolom[k] <- ifelse(side=="l", d_lv, 101L - d_lv)
        } else {
          nodes$kolom[k] <- curi + as.integer(length(indicstoplace)/2.01)
          nodes$rij[k] <- ifelse(side=="m", d_lv, 101L - d_lv)
        }
        for (kk in seq_along(indicstoplace)) {
          kkk <- which(nodes$id == indicstoplace[kk])
          if (side == "l" || side == "r") {
            nodes$rij[kkk] <- curi
            nodes$kolom[kkk] <- ifelse(side=="l", d_ind, 101L - d_ind)
          } else {
            nodes$kolom[kkk] <- curi
            nodes$rij[kkk] <- ifelse(side=="m", d_ind, 101L - d_ind)
          }

          if (varlv) { # variances of indicators?
            varlvedges <- which(edges$naar == indicstoplace[kk] &
                                  edges$van %in% varlvs)
            if (length(varlvedges) > 0L) {
              edges$vananker[varlvedges[1L]] <- switch(side, l="e", r="w", m = "s", b = "n")
              edges$naaranker[varlvedges[1L]] <- switch(side, l="w", r="e", m = "n", b = "s")
              lvvarid <- edges$van[varlvedges[1L]]
              nodes$kolom[lvvarid] <- switch(side, l = 1L, r = 100L, m =, b = curi)
              nodes$rij[lvvarid] <- switch(side, m = 1L, b = 100L, l =, r = curi)
            }
          }
          curi <- curi + 1L
        }
      }
    }
  } else {  #### only observed variables ####
    if (any(nodes$voorkeur == "l")) {
      strucs <- which(nodes$voorkeur == "l")
      nodes$rij[strucs] <- 1L + seq.int(length(strucs))
      nodes$kolom[strucs] <- 1L
    }
    if (any(nodes$voorkeur == "r")) {
      strucs <- which(nodes$voorkeur == "r")
      nodes$rij[strucs] <- 1L + seq.int(length(strucs))
      nodes$kolom[strucs] <- 100L
    }
    if (any(nodes$voorkeur == "m")) {
      strucs <- which(nodes$voorkeur == "m")
      nodes$kolom[strucs] <- 1L + seq.int(length(strucs))
      nodes$rij[strucs] <- 1L
    }
    if (any(nodes$voorkeur == "b")) {
      strucs <- which(nodes$voorkeur == "b")
      nodes$kolom[strucs] <- 1L + seq.int(length(strucs))
      nodes$rij[strucs] <- 100L
    }
  }
  #### remove empty rows / cols ####
  if (all(nodes$voorkeur == "")) { # no regressions defined
    # do nothing, should be covered by search_position subroutine
  } else {
    # remove the holes in rows and columns, always include row/col in the middle
    rijen <- sort(unique(c(d_lv + 1L, nodes$rij)))
    nodes$rij <- match(nodes$rij, rijen)
    kolommen <- sort(unique(c(d_lv + 1L, nodes$kolom)))
    nodes$kolom <- match(nodes$kolom, kolommen)
  }
  #### handle nodes which are not yet placed ####
  while (any(is.na(nodes$rij))) {
    j <- which(is.na(nodes$rij))[1L]
    x <- search_position(j, nodes, edges)
    nodes$rij[j] <- x[1L]
    nodes$kolom[j] <- x[2L]
  }
  #### adapt rij, kolom to be 1: ... ####
  minrij <- min(nodes$rij)
  if (minrij != 1L) nodes$rij <- nodes$rij - minrij + 1L
  maxrij <- max(nodes$rij)
  minkol <- min(nodes$kolom)
  if (minkol != 1L) nodes$kolom <- nodes$kolom - minkol + 1L
  maxkol <- max(nodes$kolom)
  #### adapt anchors for covariances in first or last rows/columns ####
  adaptableedges <- which(edges$tiepe == "~~" & edges$van != edges$naar)
  for (i in adaptableedges) {
    nodevan <- which(nodes$id == edges$van[i])
    nodenaar <- which(nodes$id == edges$naar[i])
    if (nodes$rij[nodevan] == nodes$rij[nodenaar]) {
      if (nodes$rij[nodevan] == 1L) {
        edges$vananker[i] <- "n"
        edges$naaranker[i] <- "n"
        edges$controlpt.kol[i] <- (nodes$kolom[nodevan] + nodes$kolom[nodenaar]) / 2
        edges$controlpt.rij[i] <- nodes$rij[nodevan] - 0.3 -
          abs(nodes$kolom[nodevan] - nodes$kolom[nodenaar]) / 2
      } else {
        edges$vananker[i] <- "s"
        edges$naaranker[i] <- "s"
        edges$controlpt.kol[i] <- (nodes$kolom[nodevan] + nodes$kolom[nodenaar]) / 2
        edges$controlpt.rij[i] <- nodes$rij[nodevan] + 0.3 +
          abs(nodes$kolom[nodevan] - nodes$kolom[nodenaar]) / 2
      }
    } else if (nodes$kolom[nodevan] == nodes$kolom[nodenaar]) {
      if (nodes$kolom[nodevan] == 1L) {
        edges$vananker[i] <- "w"
        edges$naaranker[i] <- "w"
        edges$controlpt.rij[i] <- (nodes$rij[nodevan] + nodes$rij[nodenaar]) / 2
        edges$controlpt.kol[i] <- nodes$kolom[nodevan] - 0.3 -
          abs(nodes$rij[nodevan] - nodes$rij[nodenaar]) / 2
      } else {
        edges$vananker[i] <- "e"
        edges$naaranker[i] <- "e"
        edges$controlpt.rij[i] <- (nodes$rij[nodevan] + nodes$rij[nodenaar]) / 2
        edges$controlpt.kol[i] <- nodes$kolom[nodevan] + 0.3 +
          abs(nodes$rij[nodevan] - nodes$rij[nodenaar]) / 2
      }
    } else {
      if (nodes$kolom[nodevan] == 1L) {
        wvan <- "w"
      } else if (nodes$kolom[nodevan] == maxkol) {
        wvan <- "e"
      } else if (nodes$rij[nodevan] == 1L) {
        wvan <- "n"
      } else {
        wvan <- "s"
      }
      if (nodes$kolom[nodenaar] == 1L) {
        wnaar <- "w"
      } else if (nodes$kolom[nodenaar] == maxkol) {
        wnaar <- "e"
      } else if (nodes$rij[nodenaar] == 1L) {
        wnaar <- "n"
      } else {
        wnaar <- "s"
      }
      wvannaar <- paste0(wvan, wnaar)
      edges$vananker[i] <-
        switch(wvannaar,
               nn = , ne = , nw = "n",
               ns = ifelse(nodes$kolom(nodevan) < nodes$kolom(nodenaar), "e", "w"),
               ss = , se = , sw = "s",
               sn = ifelse(nodes$kolom(nodevan) < nodes$kolom(nodenaar), "e", "w"),
               ww = , wn = , ws = "w",
               we = ifelse(nodes$rij(nodevan) < nodes$rij(nodenaar), "s", "n"),
               ee = , en = , es = "e",
               ew = ifelse(nodes$rij(nodevan) < nodes$rij(nodenaar), "n", "s")
        )
      edges$naaranker[i] <-
        switch(wvannaar,
               nn = , en = , wn = "n",
               sn = "s",
               ss = , es = , ws = "s",
               ns = "n",
               ww = , nw = , sw = "w",
               ew = "e",
               ee = , ne = , se = "e",
               we = "w"
        )
      edges$controlpt.kol[i] <-
        switch(wvannaar,
               nn = , ss = (nodes$kolom[nodevan] + nodes$kolom[nodenaar]) / 2,
               ee = nodes$kolom[nodevan] + 0.3 +
                 abs(nodes$rij[nodevan] - nodes$rij[nodenaar]) / 2,
               ww = nodes$kolom[nodevan] - 0.3 -
                 abs(nodes$rij[nodevan] - nodes$rij[nodenaar]) / 2,
               ns = , sn = nodes$kolom[nodenaar],
               we = , ew = nodes$kolom[nodevan],
               BeziersControlPunt(
                 c(nodes$rij[nodevan], nodes$kolom[nodevan]),
                 c(nodes$rij[nodenaar], nodes$kolom[nodenaar]),
                 wvannaar, maxrij, maxkol)[2L]
        )
      edges$controlpt.rij[i] <-
        switch(wvannaar,
               nn =  nodes$rij[nodevan] - 0.3 -
                 abs(nodes$kolom[nodevan] - nodes$kolom[nodenaar]) / 2,
               ss =  nodes$rij[nodevan] + 0.3 +
                 abs(nodes$kolom[nodevan] - nodes$kolom[nodenaar]) / 2,
               ee = , ww = (nodes$rij[nodevan] + nodes$rij[nodenaar]) / 2,
               ew = , we = nodes$rij[nodenaar],
               ns = , sn = nodes$rij[nodevan],
               BeziersControlPunt(
                 c(nodes$rij[nodevan], nodes$kolom[nodevan]),
                 c(nodes$rij[nodenaar], nodes$kolom[nodenaar]),
                 wvannaar, maxrij, maxkol)[1L]
               )
    }
  }
  #### fill anchors structural edges ####
  edges <- complete_anchors(nodes, edges)
  #### place nodes demanded by user ? ####
  if (!is.null(placenodes)) {
    for (nn in names(placenodes)) {
      w <- which(nodes$naam == nn)
      if (length(w) == 0) {
        warning("placenodes: node name", nn, "not found!")
      }
      nodes$rij[w] <- placenodes[[nn]][1L]
      nodes$kolom[w] <- placenodes[[nn]][2L]
      edg <- which((edges$van == nodes$id[w] |
                     edges$naar == nodes$id[w]) &
                     edges$tiepe == "~")
      if (length(edg) > 0L) edges$vananker[edg] <- NA_character_
    }
    edges <- complete_anchors(nodes, edges)
  }
  #### labelsbelow demanded by user ? ####
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

  #### RETURN ####
  return(list(nodes = nodes, edges = edges, mlrij = 0L))
}
