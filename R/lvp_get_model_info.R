# extract info from model
lvp_get_model_info <- function(model = NULL, infile = NULL, varlv = FALSE) {
  if (is.null(model) == is.null(infile)) {
      stop("either model or infile must be specified")
  }
  edge_label <- function(label, fix) {
    if (label == "" && fix == "") return("")
    if (label == "") return(fix)
    if (fix == "") return(label)
    return(paste(label, fix, sep = "="))
  }
  if (!is.null(infile)) {
    stopifnot(file.exists(infile))
    model <- readLines(infile)
  }
  if (is.list(model) && !is.null(model$op) && !is.null(model$lhs) &&
        !is.null(model$rhs) && !is.null(model$label) &&
        !is.null(model$fixed)) {
    tbl <- as.data.frame(model)
  } else if (is.character(model)) {
    tbl <- lavParseModelString(model.syntax = model,
                               as.data.frame. = TRUE)
  } else {
    stop("model, or content of infile, not in interpretable form!")
  }
  if (is.null(tbl$block)) {
    tbl$block <- 0L
  } else {
    if (all(tbl$block == tbl$block[1L])) tbl$block <- 0L
  }
  maxedges <- nrow(tbl)
  maxnodes <- 2 * maxedges
  nodes <- data.frame(
    id = integer(maxnodes),
    naam = character(maxnodes),
    tiepe = character(maxnodes), # ov, lv, varlv, cv, wov, bov, const
    # cv: composite; wov = within; bov = between; const = intercept
    blok = integer(maxnodes),
    voorkeur = character(maxnodes), # l = links, r = rechts, m = midden
    indicatoren = integer(maxnodes),
    rij = rep(NA_integer_, maxnodes),
    kolom = rep(NA_integer_, maxnodes),
    tmp = character(maxnodes)
  )
  edges <- data.frame(
    id = integer(maxedges),
    label = character(maxedges),
    van = integer(maxedges),
    naar = integer(maxedges),
    tiepe = character(maxedges), # p = pijl, d = dubbele pijl, s = self,
                                 # ip = indicator pijl
    labelbelow = rep(FALSE, maxedges)
  )
  curnode <- 0L
  curedge <- 0L
  for (i in seq.int(nrow(tbl))) {
    if (tbl$op[i] == "=~") {
      #### =~ : is manifested by ####
      # lhs node
      jl <-match(paste(tbl$block[i], tbl$lhs[i]), nodes$tmp, nomatch = 0L)
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- "lv"
        nodes$blok[curnode] <- tbl$block[i]
        nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
      } else {
        nodes$tiepe[jl] <- "lv"
      }
      # rhs node
      jr <-match(paste(tbl$block[i], tbl$rhs[i]), nodes$tmp, nomatch = 0L)
      nodetype <- "ov"
      if (length(unique(tbl$block[tbl$rhs == tbl$rhs[i]])) > 1L)
        nodetype <- switch(tbl$block[i], "wov", "bov")
      if (jr == 0L) {
        curnode <- curnode + 1L
        jr <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$rhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
      } else {
        if (nodes$tiepe[jr] == "") nodes$tiepe[jr] <- nodetype
      }
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- edge_label(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jl
      edges$naar[curedge] <- jr
      edges$tiepe[curedge] <- "ip"
    } else if (tbl$op[i] == "<~") {
      #### <~ : is a result of ####
      # lhs node
      jl <-match(paste(tbl$block[i], tbl$lhs[i]), nodes$tmp, nomatch = 0L)
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- "cv"
        nodes$blok[curnode] <- tbl$block[i]
        nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
      } else {
        nodes$tiepe[jl] <- "cv"
      }
      # rhs node
      jr <-match(paste(tbl$block[i], tbl$rhs[i]), nodes$tmp, nomatch = 0L)
      nodetype <- "ov"
      if (length(unique(tbl$block[tbl$rhs == tbl$rhs[i]])) > 1L)
        nodetype <- switch(tbl$block[i], "wov", "bov")
      if (jr == 0L) {
        curnode <- curnode + 1L
        jr <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$rhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
      } else {
        if (nodes$tiepe[jr] == "") nodes$tiepe[jr] <- nodetype
      }
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- edge_label(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jr
      edges$naar[curedge] <- jl
      edges$tiepe[curedge] <- "ip"
    } else if (tbl$op[i] == "~") {
      #### ~ : is regressed on ####
      # lhs node
      jl <-match(paste(tbl$block[i], tbl$lhs[i]), nodes$tmp, nomatch = 0L)
      nodetype <- "ov"
      if (length(unique(tbl$block[tbl$rhs == tbl$rhs[i]])) > 1L)
        nodetype <- switch(tbl$block[i], "wov", "bov")
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
        nodes$voorkeur[curnode] <- "r"
      } else {
        if (nodes$tiepe[jl] == "") nodes$tiepe[jl] <- nodetype
        if (nodes$voorkeur[jl] == "") {
          nodes$voorkeur[jl] <- "r"
        } else if (nodes$voorkeur[jl] == "l") {
          nodes$voorkeur[jl] <- "m"
        }
      }
      # rhs node
      jr <-match(paste(tbl$block[i], tbl$rhs[i]), nodes$tmp, nomatch = 0L)
      if (jr == 0L) {
        curnode <- curnode + 1L
        jr <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$rhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
        nodes$voorkeur[curnode] <- "l"
      } else {
        if (nodes$tiepe[jr] == "") nodes$tiepe[jr] <- nodetype
        if (nodes$voorkeur[jr] == "") {
          nodes$voorkeur[jr] <- "l"
        } else if (nodes$voorkeur[jr] == "r") {
          nodes$voorkeur[jr] <- "m"
        }
      }
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- edge_label(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jr
      edges$naar[curedge] <- jl
      edges$tiepe[curedge] <- "p"
    } else if (tbl$op[i] == "~1") {
      #### ~1 : intercept ####
      # lhs node
      jl <-match(paste(tbl$block[i], tbl$lhs[i]), nodes$tmp, nomatch = 0L)
      nodetype <- "ov"
      if (length(unique(tbl$block[tbl$rhs == tbl$lhs[i] |
                                  tbl$lhs == tbl$lhs[i]])) > 1L)
        nodetype <- switch(tbl$block[i], "wov", "bov")
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
        nodes$voorkeur[curnode] <- "r"
      } else {
        if (nodes$tiepe[jl] == "") nodes$tiepe[jl] <- nodetype
        if (nodes$voorkeur[jl] == "") {
          nodes$voorkeur[jl] <- "r"
        } else if (nodes$voorkeur[jl] == "l") {
          nodes$voorkeur[jl] <- "m"
        }
      }
      # rhs node
      jr <- 0L
      curnode <- curnode + 1L
      jr <- curnode
      nodes$id[curnode] <- curnode
      nodes$naam[curnode] <- "1"
      nodes$tiepe[curnode] <- "const"
      nodes$blok[curnode] <- tbl$block[i]
      nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
      nodes$voorkeur[curnode] <- "l"
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- edge_label(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jr
      edges$naar[curedge] <- jl
      edges$tiepe[curedge] <- "p"
    } else if (tbl$op[i] == "~~") {
      #### ~~ : is correlated with ####
      # lhs node
      jl <-match(paste(tbl$block[i], tbl$lhs[i]), nodes$tmp, nomatch = 0L)
      nodetype <- "ov"
      if (length(unique(tbl$block[tbl$rhs == tbl$lhs[i] |
                                  tbl$lhs == tbl$lhs[i]])) > 1L)
        nodetype <- switch(tbl$block[i], "wov", "bov")
      if (jl == 0L) {
        curnode <- curnode + 1L
        jl <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$lhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
      } else {
        if (nodes$tiepe[jl] == "") nodes$tiepe[jl] <- nodetype
      }
      # rhs node
      jr <-match(paste(tbl$block[i], tbl$rhs[i]), nodes$tmp, nomatch = 0L)
      nodetype <- "ov"
      if (length(unique(tbl$block[tbl$rhs == tbl$lhs[i] |
                                  tbl$lhs == tbl$lhs[i]])) > 1L)
        nodetype <- switch(tbl$block[i], "wov", "bov")
      if (jr == 0L) {
        curnode <- curnode + 1L
        jr <- curnode
        nodes$id[curnode] <- curnode
        nodes$naam[curnode] <- tbl$rhs[i]
        nodes$tiepe[curnode] <- nodetype
        nodes$blok[curnode] <- tbl$block[i]
        nodes$tmp[curnode] <- paste(nodes$blok[curnode], nodes$naam[curnode])
      } else {
        if (nodes$tiepe[jr] == "") nodes$tiepe[jr] <- nodetype
      }
      # edge
      curedge <- curedge + 1L
      edges$id[curedge] <- curedge
      edges$label[curedge] <- edge_label(tbl$label[i], tbl$fixed[i])
      edges$van[curedge] <- jr
      edges$naar[curedge] <- jl
      edges$tiepe[curedge] <- ifelse(jl == jr, "s", "d")
    }
  }
  # aanpassingen voor varlv
  if (varlv && any(edges$tiepe == "s")) {
    #       wijzigen varianties
    welke <- which(edges$tiepe == "s")
    varlvnodes <- rep(0L, length(welke))
    lvnodes <- rep(0L, length(welke))
    for (ji in seq_along(welke)) {
      j <- welke[ji]
      curnode <- curnode + 1L
      jj <- curnode
      nodes$id[curnode] <- curnode
      nodes$naam[curnode] <- gsub("=.*$", "", edges$label[j])
      nodes$tiepe[curnode] <- "varlv"
      nodes$blok[curnode] <- nodes$blok[which(nodes$id == edges$van[j])[[1L]]]
      varlvnodes[ji] <- curnode
      lvnodes[ji] <- edges$van[j]
      edges$van[j] <- curnode
      edges$tiepe[j] <- "p"
      edges$label[j] <- ifelse(grepl("=", edges$label[j]),
                               gsub("^.*=","",edges$label[j]), "")
    }
    #       wijzigen covarianties
    for (j1 in seq_along(welke)) {
      for (j2 in seq_along(welke)) {
        if (j1 != j2 && any(edges$tiepe == "d" & edges$van == lvnodes[j1] & edges$naar == lvnodes[j2])) {
          edg <- which(edges$tiepe == "d" &  edges$van == lvnodes[j1] & edges$naar == lvnodes[j2])[[1L]]
          edges$van[edg] <- varlvnodes[j1]
          edges$naar[edg] <- varlvnodes[j2]
        }
      }
    }
  }

  latents <- nodes$id[nodes$tiepe == "lv" | nodes$tiepe == "cv"]
  for (j in latents) {
    nodes$indicatoren[j] <- sum(tbl$lhs == nodes$naam[j] &
                                  (tbl$op == "=~" | tbl$op == "<~"))
  }
  nodes$tmp <- NULL
  nodes$voorkeur[nodes$voorkeur == "" & nodes$tiepe == "lv"] <- "m"
  nodes <- nodes[seq.int(curnode), ]
  edges <- edges[seq.int(curedge), ]
  nodes$voorkeur[nodes$voorkeur == "" & nodes$tiepe == "lv"] <- "l"
  edges$label <- trimws(edges$label)
  return(list(nodes = nodes, edges = edges))
}
