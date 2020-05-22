printz <-
function (x, file = "", col.name.align = "cen", col.txt.align = "right", 
    cell.align = "cen", hsep = "|", vsep = "-", csep = "+", row.names = TRUE, 
    col.names = FALSE, append = FALSE, top.border = TRUE, left.border = TRUE, 
    ...) 
{
    ndimn <- names(dimnames(x))
    rownames <- dimnames(x)[[1]]
    x <- cbind(rownames, x)
    names(dimnames(x)) <- ndimn
    cnam <- dimnames(x)[[2]]
    if (length(ndimn)) 
        cnam[1] <- ndimn[1]
    dimnames(x) <- list(as.character(seq(nrow(x))), cnam)
    names(dimnames(x)) <- ndimn
    pad.left <- function(z, pads) {
        padding <- paste(rep(" ", pads), collapse = "")
        paste(padding, z, sep = "")
    }
    pad.mid <- function(z, pads) {
        padding.right <- paste(rep(" ", pads%/%2), collapse = "")
        padding.left <- paste(rep(" ", pads - pads%/%2), collapse = "")
        paste(padding.left, z, padding.right, sep = "")
    }
    pad.right <- function(z, pads) {
        padding <- paste(rep(" ", pads), collapse = "")
        paste(z, padding, sep = "")
    }
    pad.types <- c("left", "mid", "right")
    names(pad.types) <- c("right", "cen", "left")
    pad.name <- pad.types[col.name.align]
    pad.txt <- pad.types[col.txt.align]
    pad.cell <- pad.types[cell.align]
    pad.char.col.right <- function(y) {
        col.width <- nchar(y)
        biggest <- max(col.width)
        smallest <- min(col.width)
        padding <- biggest - col.width
        out <- NULL
        for (i in seq(y)) out[i] <- pad.right(y[i], pads = padding[i])
        out
    }
    pad.char.col.left <- function(y) {
        col.width <- nchar(y)
        biggest <- max(col.width)
        smallest <- min(col.width)
        padding <- biggest - col.width
        out <- NULL
        for (i in seq(y)) out[i] <- pad.left(y[i], pads = padding[i])
        out
    }
    pad.char.col.mid <- function(y) {
        col.width <- nchar(y)
        biggest <- max(col.width)
        smallest <- min(col.width)
        padding <- biggest - col.width
        out <- NULL
        for (i in seq(y)) out[i] <- pad.mid(y[i], pads = padding[i])
        out
    }
    pad.name.fn <- get(paste("pad.", pad.name, sep = ""))
    pad.txt.fn <- get(paste("pad.char.col.", pad.txt, sep = ""))
    pad.cell.fn <- get(paste("pad.", pad.cell, sep = ""))
    x <- as.data.frame(x)
    fac.col <- names(x)[sapply(x, is.factor)]
    for (i in fac.col) x[, i] <- I(as.character(x[, i]))
    break.list <- list()
    for (i in seq(nrow(x))) {
        x.i <- unlist(x[i, ])
        rows.i <- sapply(strsplit(unlist(x[i, ]), "\n"), length)
        rows.i[rows.i < 1] <- 1
        break.list[[i]] <- rows.i
    }
    break.row <- sapply(break.list, function(x) any(x > 1))
    names(break.row) <- seq(nrow(x))
    xx <- x
    if (any(break.row)) {
        xx <- NULL
        reprow <- lapply(break.list, unique)
        for (k in seq(nrow(x))) {
            x.k <- unlist(x[k, ])
            x.k[x.k == ""] <- " "
            if (break.row[k]) {
                l.k <- strsplit(x.k, "\n")
                add.blanks <- max(break.list[[k]]) - break.list[[k]]
                names(l.k) <- names(add.blanks) <- seq(length(l.k))
                if (any(add.blanks > 0)) {
                  for (kk in names(add.blanks[add.blanks > 0])) l.k[[kk]] <- c(l.k[[kk]], 
                    rep(" ", add.blanks[kk]))
                }
                l.k.df <- as.data.frame(l.k)
                names(l.k.df) <- names(x)
                xx <- rbind(xx, as.matrix(l.k.df))
            }
            else xx <- rbind(xx, x.k)
        }
        row.names(xx) <- paste(rep(row.names(x), sapply(reprow, 
            max)), unlist(reprow), sep = ".")
        rn <- row.names(xx)
        rnb <- strsplit(rn, "\\.")
        rpref <- as.numeric(factor(sapply(rnb, function(z) z[1])))
    }
    else rpref <- seq(nrow(x))
    x <- as.data.frame(xx)
    char.cols <- sapply(x, is.character)
    if (any(char.cols)) 
        x[char.cols] <- sapply(x[char.cols], pad.txt.fn)
    if (any(!char.cols)) 
        x[!char.cols] <- sapply(x[!char.cols], format)
    names.width <- nchar(names(x))
    if (!col.names) 
        names.width <- rep(0, length(names.width))
    cell.width <- sapply(x, function(y) max(nchar(as.character(y))))
    name.pads <- cell.width - names.width
    cell.pads <- -name.pads
    name.pads[name.pads < 0] <- 0
    cell.pads[cell.pads < 0] <- 0
    pad.names <- name.pads > 0
    pad.cells <- cell.pads > 0
    if (any(pad.names)) {
        stretch.names <- names(x)[pad.names]
        for (i in stretch.names) {
            names(x)[names(x) == i] <- pad.name.fn(i, name.pads[i])
        }
    }
    if (any(pad.cells)) {
        stretch.cells <- names(x)[pad.cells]
        for (j in stretch.cells) x[, j] <- pad.cell.fn(x[, j], 
            cell.pads[j])
    }
    if (!row.names) 
        x <- x[-1]
    if (col.names) 
        mat2 <- rbind(names(x), as.matrix(x))
    else mat2 <- as.matrix(x)
    mat.names.width <- nchar(mat2[1, ])
    space.h <- ""
    for (k in seq(along = mat.names.width)) {
        space.h <- c(space.h, rep(vsep, mat.names.width[k]), 
            csep)
    }
    line.sep <- paste(c(ifelse(left.border, csep, ""), space.h), 
        collapse = "")
    if (col.names) 
        rpref <- c(0, rpref, 0)
    else rpref <- c(rpref, 0)
    if (top.border) {
        write(line.sep, file = file, append = append)
        append <- TRUE
    }
    for (i in 1:nrow(mat2)) {
        if (left.border) 
            write(paste(paste(c("", mat2[i, ]), collapse = hsep), 
                hsep, sep = ""), file = file, append = append)
        else write(paste(paste(mat2[i, ], collapse = hsep), hsep, 
            sep = ""), file = file, append = append)
        append <- TRUE
        if (rpref[i] != rpref[i + 1]) 
            write(line.sep, file = file, append = TRUE)
    }
}

