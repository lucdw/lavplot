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
  if (sum(van * edgevec)*sum(naar*edgevec) >= 0) { #loodlijn niet op lijnstuk
    return(min(veclen(van), veclen(naar)))
  }
  return(veclen(van) *
           sqrt(1 - sum(edgevec * (-van))**2 /
                  (sum(edgevec*edgevec)*sum(van*van))))
}
search_position <- function(j, nodes, edges) {
  # TODO: check also edges to placed node don't go over other nodes !
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
    if (min(distances) > 0.5) return(TRUE)
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
      if (nodes$kolom[nodevan] == nodes$kolom[nodenaar]) {
        midden <- mean(range(nodes$kolom))
        if (nodes$kolom[nodevan] < midden) {
          edges$vananker[i] <- "w"
          edges$naaranker[i] <- "w"
        } else {
          edges$vananker[i] <- "e"
          edges$naaranker[i] <- "e"
        }
        next
      } else if (nodes$rij[nodevan] == nodes$rij[nodenaar]) {
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

lvp_position_nodes <- function(nodes_edges, allowbottom = FALSE) {
  #### lvp_position_nodes MAIN ####
  nodes <- nodes_edges$nodes
  edges <- nodes_edges$edges
  if (length(nodes$rij) == 1L) {
    nodes$rij[1L] <- 1L
    nodes$kolom[1L] <- 1L
    return(nodes)
  }
  if (any(nodes$blok > 0L)) { # multilevel, only level:1 and level:2 accepted
    nodes1 <- nodes[nodes$blok >= 2L, ]
    edges1 <- edges[edges$van %in% nodes1$id, ]
    nodes1$blok <- 0L
    nodes2 <- nodes[nodes$blok == 1L, ]
    edges2 <- edges[edges$van %in% nodes2$id, ]
    nodes2$blok <- 0L
    result1 <- lvp_position_nodes(list(nodes = nodes1, edges = edges1))
    result2 <- lvp_position_nodes(list(nodes = nodes2, edges = edges2))
    rijen1 <- max(result1$nodes$rij)
    result2$nodes$rij <- result2$nodes$rij + rijen1 + 1L
    result1$nodes$blok <- 2L
    result2$nodes$blok <- 1L
    nodes <- rbind(result1$nodes, result2$nodes)
    edges <- rbind(result1$edges, result2$edges)
    return(list(nodes = nodes, edges = edges, mlrij = rijen1 + 1L))
  }
  varlvs <- which(nodes$tiepe == "varlv")
  varlv <- length(varlvs) > 0L
  d_lv <- ifelse(varlv, 3L, 2L)  # distance from border
  d_ind <- ifelse(varlv, 2L, 1L) # depends on presence varlv's
  # structural part
  lvcvs <- nodes$tiepe %in% c("lv", "cv")
  if (any(lvcvs)) { # lv's or cv's present
    if (any(nodes$voorkeur == "l" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "l" & lvcvs)
      maxindicatoren <- max(nodes$indicatoren[strucs])
      nodes$rij[strucs] <- d_lv + seq.int(length(strucs)) * maxindicatoren
      nodes$kolom[strucs] <- d_lv
    }
    if (any(nodes$voorkeur == "r" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "r" & lvcvs)
      maxindicatoren <- max(nodes$indicatoren[strucs])
      nodes$rij[strucs] <- d_lv + seq.int(length(strucs)) * maxindicatoren
      nodes$kolom[strucs] <- 101L - d_lv
    }
    if (any(nodes$voorkeur == "m" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "m" & lvcvs)
      if (length(strucs) > 1L && allowbottom) {
        vanaf <- as.integer(1L + length(strucs) / 2)
        nodes$voorkeur[strucs[seq.int(vanaf, length(strucs))]] <- "b"
      }
    }
    if (any(nodes$voorkeur == "m" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "m" & lvcvs)
      maxindicatoren <- max(nodes$indicatoren[strucs])
      nodes$kolom[strucs] <- d_lv + seq.int(length(strucs)) * maxindicatoren
      nodes$rij[strucs] <- d_lv
    }
    if (any(nodes$voorkeur == "b" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "b" & lvcvs)
      maxindicatoren <- max(nodes$indicatoren[strucs])
      nodes$kolom[strucs] <- d_lv + seq.int(length(strucs)) * maxindicatoren
      nodes$rij[strucs] <- 101L - d_lv
    }
    # indicators
    #### LEFT INDICATORS ####
    # allindicators <- nodes$id[nodes$voorkeur == ""] anders, HIER VERDER !!!!
    strucs <- which(nodes$voorkeur == "l" & lvcvs)
    for (j in strucs) {
      cvedges <- edges$naar == nodes$id[j] & edges$tiepe == "<~"
      edges$vananker[cvedges] <- "e"
      edges$naaranker[cvedges] <- "w"
      cvindicatorids <- edges$van[cvedges]
      lvedges <- edges$van == nodes$id[j] & edges$tiepe == "=~"
      edges$vananker[lvedges] <- "w"
      edges$naaranker[lvedges] <- "e"
      lvindicatorids <- edges$naar[lvedges]
      indicatorids <- union(cvindicatorids, lvindicatorids)
      indicators <- which(nodes$id %in% indicatorids)
      rijtje <- nodes$rij[j]
      addedindicators <- 0L
      for (k in indicators) {
        if (is.na(nodes$rij[k])) {
          nodes$rij[k] <- rijtje
          rijtje <- rijtje + 1L
          nodes$kolom[k] <- d_ind
          addedindicators <- addedindicators + 1L
          if (varlv) { # variances of indicators?
            varlvedges <- which(edges$naar == nodes$id[k] &
                                  edges$van %in% varlvs)
            if (length(varlvedges) > 0L) {
              lvvarid <- edges$van[varlvedges[1L]]
              nodes$kolom[lvvarid] <- 1L          # kolom 1
              nodes$rij[lvvarid] <- nodes$rij[k]  # zelfde rij als indicator
              edges$vananker[varlvedges[1L]] <- "e"
              edges$naaranker[varlvedges[1L]] <- "w"
            }
          }
        }
      }
      if (addedindicators > 2) {
        nodes$rij[j] <- nodes$rij[j] + as.integer((addedindicators - 1) / 2)
      }
    }
    #### RIGHT INDICATORS ####
    strucs <- which(nodes$voorkeur == "r" & lvcvs)
    for (j in strucs) {
      cvedges <- edges$naar == nodes$id[j] & edges$tiepe == "<~"
      edges$vananker[cvedges] <- "w"
      edges$naaranker[cvedges] <- "e"
      cvindicatorids <- edges$van[cvedges]
      lvedges <- edges$van == nodes$id[j] & edges$tiepe == "=~"
      edges$vananker[lvedges] <- "e"
      edges$naaranker[lvedges] <- "w"
      lvindicatorids <- edges$naar[lvedges]
      indicatorids <- union(cvindicatorids, lvindicatorids)
      indicators <- which(nodes$id %in% indicatorids)
      rijtje <- nodes$rij[j]
      addedindicators <- 0L
      for (k in indicators) {
        if (is.na(nodes$rij[k])) {
          nodes$rij[k] <- rijtje
          rijtje <- rijtje + 1L
          nodes$kolom[k] <- 101L - d_ind
          addedindicators <- addedindicators + 1L
          if (varlv) { # variances of indicators?
            varlvedges <- which(edges$naar == nodes$id[k] &
                                  edges$van %in% varlvs)
            if (length(varlvedges) > 0L) {
              lvvarid <- edges$van[varlvedges[1L]]
              nodes$kolom[lvvarid] <- 100L        # kolom 100
              nodes$rij[lvvarid] <- nodes$rij[k]  # zelfde rij als indicator
              edges$vananker[varlvedges[1L]] <- "w"
              edges$naaranker[varlvedges[1L]] <- "e"
            }
          }
        }
      }
      if (addedindicators > 2) {
        nodes$rij[j] <- nodes$rij[j] + as.integer((addedindicators - 1) / 2)
      }
    }
    #### TOP INDICATORS ####
    strucs <- which(nodes$voorkeur == "m" & lvcvs)
    for (j in strucs) {
      cvedges <- edges$naar == nodes$id[j] & edges$tiepe == "<~"
      edges$vananker[cvedges] <- "s"
      edges$naaranker[cvedges] <- "n"
      cvindicatorids <- edges$van[cvedges]
      lvedges <- edges$van == nodes$id[j] & edges$tiepe == "=~"
      edges$vananker[lvedges] <- "n"
      edges$naaranker[lvedges] <- "s"
      lvindicatorids <- edges$naar[lvedges]
      indicatorids <- union(cvindicatorids, lvindicatorids)
      indicators <- which(nodes$id %in% indicatorids)
      kolompje <- nodes$kolom[j]
      addedindicators <- 0L
      for (k in indicators) {
        if (is.na(nodes$rij[k])) {
          nodes$kolom[k] <- kolompje
          kolompje <- kolompje + 1L
          nodes$rij[k] <- d_ind
          addedindicators <- addedindicators + 1L
          if (varlv) { # variances of indicators?
            varlvedges <- which(edges$naar == nodes$id[k] &
                                  edges$van %in% varlvs)
            if (length(varlvedges) > 0L) {
              lvvarid <- edges$van[varlvedges[1L]]
              nodes$rij[lvvarid] <- 1L              # rij 1
              nodes$kolom[lvvarid] <- nodes$kolom[k]  # zelfde kol als indicator
              edges$vananker[varlvedges[1L]] <- "s"
              edges$naaranker[varlvedges[1L]] <- "n"
            }
          }
        }
      }
      if (addedindicators > 2) {
        nodes$kolom[j] <- nodes$kolom[j] + as.integer((addedindicators - 1) / 2)
      }
    }
    #### BOTTOM INDICATORS ####
    strucs <- which(nodes$voorkeur == "b" & lvcvs)

    for (j in strucs) {
      cvedges <- edges$naar == nodes$id[j] & edges$tiepe == "<~"
      edges$vananker[cvedges] <- "n"
      edges$naaranker[cvedges] <- "s"
      cvindicatorids <- edges$van[cvedges]
      lvedges <- edges$van == nodes$id[j] & edges$tiepe == "=~"
      edges$vananker[lvedges] <- "s"
      edges$naaranker[lvedges] <- "n"
      lvindicatorids <- edges$naar[lvedges]
      indicatorids <- union(cvindicatorids, lvindicatorids)
      indicators <- which(nodes$id %in% indicatorids)
      kolompje <- nodes$kolom[j]
      addedindicators <- 0L
      for (k in indicators) {
        if (is.na(nodes$rij[k])) {
          nodes$kolom[k] <- kolompje
          kolompje <- kolompje + 1L
          nodes$rij[k] <- 101L - d_ind
          addedindicators <- addedindicators + 1L
          if (varlv) { # variances of indicators?
            varlvedges <- which(edges$naar == nodes$id[k] &
                                  edges$van %in% varlvs)
            if (length(varlvedges) > 0L) {
              lvvarid <- edges$van[varlvedges[1L]]
              nodes$rij[lvvarid] <- 100L              # rij 100
              nodes$kolom[lvvarid] <- nodes$kolom[k]  # zelfde kol als indicator
              edges$vananker[varlvedges[1L]] <- "n"
              edges$naaranker[varlvedges[1L]] <- "s"
            }
          }
        }
      }
      if (addedindicators > 2) {
        nodes$kolom[j] <- nodes$kolom[j] + as.integer((addedindicators - 1) / 2)
      }
    }
  } else {
    #### only observed variables ####
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
      if (length(strucs) > 1L && allowbottom) {
        vanaf <- as.integer(1L + length(strucs) / 2)
        nodes$voorkeur[strucs[seq.int(vanaf, length(strucs))]] <- "b"
      }
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
    # remove the holes in rows and columns
    rijen <- sort(unique(nodes$rij))
    nodes$rij <- match(nodes$rij, rijen)
    kolommen <- sort(unique(nodes$kolom))
    nodes$kolom <- match(nodes$kolom, kolommen)
  }
  #### handle nodes which are not yet placed ####
  while (any(is.na(nodes$rij))) {
    j <- which(is.na(nodes$rij))[1L]
    x <- search_position(j, nodes, edges)
    nodes$rij[j] <- x[1L]
    nodes$kolom[j] <- x[2L]
  }
  #### adapt anchors for covariances in first or last rows/columns ####
  adaptableedges <- which(edges$op == "~~" & edges$van != edges$naar)
  for (i in adaptableedges) {
    nodevan <- which(nodes$id == edges$van[i])
    nodenaar <- which(nodes$id == edges$naar[i])
    if (nodes$rij[nodevan] == nodes$rij[nodenaar]) {
      if (nodes$rij[nodevan] == 1L) {
        edges$vananker[i] <- "n"
        edges$naaranker[i] <- "n"
      } else {
        edges$vananker[i] <- "s"
        edges$naaranker[i] <- "s"
      }
    } else if (nodes$kolom[nodevan] == nodes$kolom[nodenaar]) {
      if (nodes$kolom[nodevan] == 1L) {
        edges$vananker[i] <- "w"
        edges$naaranker[i] <- "w"
      } else {
        edges$vananker[i] <- "e"
        edges$naaranker[i] <- "e"
      }
    }
  }
  #### adapt rij, kolom to be 1: ... ####
  minrij <- min(nodes$rij)
  if (minrij != 1L) nodes$rij <- nodes$rij - minrij + 1L
  minkol <- min(nodes$kolom)
  if (minkol != 1L) nodes$kolom <- nodes$kolom - minkol + 1L
  #### fill anchors structural edges ####
  edges <- complete_anchors(nodes, edges)
  #### RETURN ####
  return(list(nodes = nodes, edges = edges, mlrij = 0L))
}
