library(data.table)
library(keypress)

## Create the universe. Well, sort of.
dims <- c(49, 49)
universe <- CJ(x = seq(dims[1]), y = seq(dims[2]), k = 1, cell = FALSE)
universe[, cell := sample(c(FALSE, TRUE), prod(dims), TRUE)]

## Neighbourhood to consider for each cell
neighbours <- CJ(xd = -1:1, yd = -1:1, k = 1)[xd != 0 | yd != 0]

## Function to advance the Game of Life one tick
gol_tick <- function(xy, sz, nb) {

  nb[xy, on = .(k), allow.cartesian = TRUE
    ][, nbx := x + xd][, nby := y + yd
    ][nbx >= 1 & nbx <= sz[1] & nby >= 1 & nby <= sz[2]
    ][xy, on = .(nbx = x, nby = y)
    ][, .(nnb = sum(i.cell)), by = .(x, y, cell, k)
    ][!cell & nnb == 3, cell := TRUE
    ][cell & (nnb < 2 | nnb > 3), cell := FALSE
    ][, nnb := NULL]

}

## Run & visualise the game
cat("\033[?25l")

repeat ({

  kp <- keypress(FALSE)

  universe[order(y, x)
    ][, cat(fifelse(.SD$cell, "\033[32;40m◼", "\033[90;40m◌"),
            "\033[K\n"),
      by = y]

  cat("\033[2K\033[33;40m", sum(universe$cell), "\n")

  if (kp == "q") break

  if (kp == "x") {
    new_cells <- sample(c(FALSE, TRUE), prod(dims), TRUE, c(9, 1))
    universe[, cell := cell | new_cells]
  }

  Sys.sleep(0.2)

  cat(paste0("\033[", dims[2] + 1, "A"))

  universe <- gol_tick(universe, dims, neighbours)

})

cat("\033[?25h")