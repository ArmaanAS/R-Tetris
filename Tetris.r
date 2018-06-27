# Initializing packages.
if(require("R.utils")){
    print("R.utils has loaded correctly")
} else {
    print("Trying to install R.utils...")
    install.packages("R.utils")
    if(require("R.utils")){
        print("R.utils installed and loaded!")
    } else {
        stop("Could not install R.utils!")
    }
}
# Initializing packages.
library(grid)

# Variable values
	score 	<<- 0
	width 	<<- 10
	height 	<<- 20
	board 	<<- matrix(rep("-", height * width), ncol = height, nrow = width)
	tetro 	<<- NULL
	tetro1 	<<- NULL
	held	<<- NULL
	swapped <<- FALSE
	level 	<<- 1
	score 	<<- 0
	lns 	<<- 0
	combo	<<- 0
	msg 	<<- ""

# Constants
	tetCol <<- c("grey", "#F5CE2D", "#4897E0", "#9D3AA1", 
					  "Blue", "#F28035", "#3EB551", "#DF1F31")
	tetObj <<- list(
				list(matrix(c(0), 				 1, 1, T), c(1, 1)),
				list(matrix(c(1, 1, 1, 1), 2, 2, T), 	   c(2, 2)),
				list(matrix(c(1, 1, 1, 1),	     4, 1, T), c(3, 1)),
				list(matrix(c(0, 1, 0, 1, 1, 1), 2, 3, T), c(2, 2)),
				list(matrix(c(0, 0, 1, 1, 1, 1), 2, 3, T), c(2, 2)),
				list(matrix(c(1, 0, 0, 1, 1, 1), 2, 3, T), c(2, 2)),
				list(matrix(c(0, 1, 1, 1, 1, 0), 2, 3, T), c(2, 2)),
				list(matrix(c(1, 1, 0, 0, 1, 1), 2, 3, T), c(2, 2))			
			)
	tets <<- c("-", "o", "i", "t",  "l", "j", "s", "z")
	names(tetCol) <- tets
	names(tetObj) <- tets
	
	pwide <<- 0.05
	phigh <<- 0.05
	
rand <- function(max, min = 0) { # Returns a random integer
	return(min + round(runif(1) * (max - min)))
}

rotate <- function(tet, rot = 1) {
	if (rot %% 4 == 0) {
		return(tet)
	}
	
	for (i in 1 : rot) {
		tet = t(apply(tet, 2, rev))
	}
	return(tet)
}

shiftDown <- function(d) {
	for (y in d : (ncol(board) - 1)) {
		for (x in 1 : nrow(board)) {
			setTile(x, y, board[x, y + 1])
		}
	}
	
	for (x in 1 : nrow(board)) {
		setTile(x, ncol(board), "-")
	}
}

holdTet <- function() {
	removeTetromino()
	if (is.null(held)) {
		held <<- c(5, 18, tetro[3], 0)
		setTet()
	} else {
		l = tetro
		tetro <<- c(5, 18, held[3], 0)
		held <<- l
	}
	swapped <<- TRUE
	drawTetromino()
	
	drawHoldBox()
	drawHeldTet()
}

drawHoldText <- function() {
	grid.text(
		"HOLD",
		x = 0.07,
		y = 0.95,
		gp = gpar(
			lwd = 3,
			col = "#333333",
			cex = 2.1,
			font = 2
		)
	)
		
	grid.text(
		"HOLD",
		x = 0.07,
		y = 0.95,
		gp = gpar(
			lwd = 3,
			col = "gold",
			cex = 2
		)
	)
}

drawHoldBox <- function() {
	x = 0.17
	y = 0.9
	rot = 0
	
	grid.rect(
		x = 0.07,
		y = 0.75,
		width = pwide * 4,
		height = phigh * 6,
		gp = gpar(
			lwd = 5,
			fill = "white",
			col = "white"
		)
	)
		
	if (!is.null(held)) {
		tet = held[3]
		
		if (tet == "i") {
			grid.rect(
				x = 0.07,
				y = 0.75,
				width = pwide * 4,
				height = phigh * 6,
				gp = gpar(
					lwd = 5,
					fill = "#222222",
					col = "Gold"
				)
			)
		} else {
			grid.rect(
				x = 0.07,
				y = 0.8,
				width = pwide * 4,
				height = phigh * 4,
				gp = gpar(
					lwd = 5,
					fill = "#222222",
					col = "Gold"
				)
			)
		}
	} else {
		grid.rect(
			x = 0.07,
			y = 0.8,
			width = pwide * 4,
			height = phigh * 4,
			gp = gpar(
				lwd = 5,
				fill = "#222222",
				col = "Gold"
			)
		)
	}	
}

drawHeldTet <- function() {
	x = 0.17
	y = 0.9
	tet = held[3]
	
	if (tet == "i") {
		x = 0.12
		y = 0.875
	} else if (tet == "o") {
		x = 0.145
		y = 0.875
	}
	
	obj = rotate(tetObj[[tet]][[1]], 3)
	
	for (nx in 1 : nrow(obj)) {
		for (ny in 1 : ncol(obj)) {
			if(obj[nx, ny]) {
				a = x - nx * pwide
				b = y - ny * phigh 
				
				drawTile(a, b, tet, TRUE)
			}
		}
	}
}

drawNextText <- function() {
	grid.text(
		"NEXT",
		x = 0.83,
		y = 0.95,
		gp = gpar(
			lwd = 3,
			col = "#333333",
			cex = 2.1,
			font = 2
		)
	)
		
	grid.text(
		"NEXT",
		x = 0.83,
		y = 0.95,
		gp = gpar(
			lwd = 3,
			col = "gold",
			cex = 2
		)
	)
}

drawNextBox <- function() {
	x = 0.83
	y = 0.9
	rot = 0
	
	grid.rect(
		x = 0.83,
		y = 0.75,
		width = pwide * 4,
		height = phigh * 6,
		gp = gpar(
			lwd = 5,
			fill = "white",
			col = "white"
		)
	)
	tet = tetro1[3]
	
	if (tet == "i") {
		grid.rect(
			x = 0.83,
			y = 0.75,
			width = pwide * 4,
			height = phigh * 6,
			gp = gpar(
				lwd = 5,
				fill = "#333333",
				col = "Gold"
			)
		)
	} else {
		grid.rect(
			x = 0.83,
			y = 0.8,
			width = pwide * 4,
			height = phigh * 4,
			gp = gpar(
				lwd = 5,
				fill = "#333333",
				col = "Gold"
			)
		)
	}
}

drawNextTet <- function() {
	x = 0.93
	y = 0.9
	tet = tetro1[3]
	
	if (tet == "i") {
		x = 0.88
		y = 0.875
	} else if (tet == "o") {
		x = 0.905
		y = 0.875
	}
	
	obj = rotate(tetObj[[tet]][[1]], 3)
	
	for (nx in 1 : nrow(obj)) {
		for (ny in 1 : ncol(obj)) {
			if(obj[nx, ny]) {
				a = x - nx * pwide
				b = y - ny * phigh 
				
				drawTile(a, b, tet, TRUE)
			}
		}
	}
}

setTet <- function(x, y = 18, tet = tets[rand(2, 8)], rot = 0) {
	if (missing(x)) {
		x = 5
		if (!is.null(tetro1)) {
			tetro <<- tetro1
			tetro1 <<- c(x, y, tet, rot)
		} else {
			tetro <<- c(x, y, tet, rot)
			tetro1 <<- c(5, 18, tets[rand(2, 8)], 0)
		}
		swapped <<- FALSE
		shiftBoard()
		drawNextBox()
		drawNextTet()
	} else {
		tetro <<- c(x, y, tet, rot)
	}
}

setTetX <- function(x) {
	tetro[1] <<- x
}

setTetY <- function(y) {
	tetro[2] <<- y
}

setTetT <- function(t) {
	tetro[3] <<- t
}

setTetRot <- function(rot) {
	tetro[4] <<- rot
}

getTetX <- function() {
	return(as.numeric(tetro[1]))
}

getTetY <- function() {
	return(as.numeric(tetro[2]))
}

getTetT <- function() {
	return(tetro[3])
}

getTetRot <- function() {
	return(as.numeric(tetro[4]))
}

draw <- function() {
	grid.newpage()
	pushViewport(viewport(width = 0.9, height = 0.9))
	
	drawTiles()
	setTet()
	drawTetromino()
	redraw()
	drawScore()
	drawOverlay()
	
	drawHoldBox()
	drawHoldText()
	
	drawNextBox()
	drawNextText()
	drawNextTet()
}

redraw <- function() {
	redrawTiles()
	drawOverlay()
	drawScore()
}

drawOverlay <- function() {
	grid.lines(
		c(0.2, 0.2, 0.7, 0.7, 0.2),
		c(0, 1, 1, 0, 0), 
		gp = gpar(
			lwd = 5, 
			col = "gold"
		)
	)
}

cols = c("#222222", "#333333")
drawTile <- function(gx, gy, tet, h = FALSE) {
	if (h) {
		grid.rect(
			x = gx,
			y = gy,
			width = pwide,
			height = phigh,
			gp = gpar(
				lwd = 2,
				col = "white",
				fill = tetCol[tet]
			)
		)
		return()
	}
	
	if (tet != "-") {
		grid.rect(
			x = (0.2 - pwide / 2) + pwide * gx,
			y = gy * phigh - pwide / 2,
			width = pwide,
			height = phigh,
			gp = gpar(
				lwd = 2,
				col = "white",
				fill = tetCol[tet]
			)
		)
	} else {
		col = sample(cols, 2)
		grid.rect(
			x = (0.2 - pwide / 2) + pwide * gx,
			y = gy * phigh - pwide / 2,
			width = pwide,
			height = phigh,
			gp = gpar(
				lwd = 2,
				col = col[1],
				fill = col[2]
			)
		)
	}
}

difTiles = matrix(rep(0, width * height), ncol = height, nrow = width)
setTile <- function(x, y, tet) {
	if (board[x, y] != tet) {
		board[x, y] <<- tet
		if (tet != ob[x, y] || tet != "-") {
			difTiles[x, y] <<- tet
		} else {
			difTiles[x, y] <<- 0
		}
	}
}

setBoard <- function(bd) {
	for (x in 1 : nrow(bd)) {
		for (y in 1 : ncol(bd)) {
			if (bd[x, y] != board[x, y]) {
				setTile(x, y, bd[x, y])
			}
		}
	}
}

drawTetromino <- function(h = FALSE) {
	x = getTetX()
	y = getTetY()
	tet = getTetT()
	rot = getTetRot()

	bb = board
	obj = rotate(tetObj[[tet]][[1]], rot)
	
	mx = tetObj[[tet]][[2]][1]
	my = tetObj[[tet]][[2]][2]
	
	for (nx in 1 : nrow(obj)) {
		for (ny in 1 : ncol(obj)) {
			if(obj[nx, ny]) {
				a = x - nx + mx
				b = y - ny + my
				
				if (a < 1 || b < 1 || a > 10 || 
					b > 20 || board[a, b] != "-") {
						board <<- bb
						return(FALSE)
				}
				
				setTile(a, b, tet)
			}
		}
	}
	
	return(TRUE)
}

removeTetromino <- function() {
	x = getTetX()
	y = getTetY()
	tet = getTetT()
	rot = getTetRot()
	
	bb = board
	obj = rotate(tetObj[[tet]][[1]], rot)
	
	mx = tetObj[[tet]][[2]][1]
	my = tetObj[[tet]][[2]][2]
	
	for (nx in 1 : nrow(obj)) {
		for (ny in 1 : ncol(obj)) {
			if(obj[nx, ny]) {
				a = x - nx + mx
				b = y - ny + my
				
				setTile(a, b, "-")
			}
		}
	}
}

moveTet <- function(dx, dy) {
	x = getTetX()
	y = getTetY()
	tet = getTetT()
	rot = getTetRot()
	
	bb = board
	obj = rotate(tetObj[[tet]][[1]], rot)
	
	mx = tetObj[[tet]][[2]][1]
	my = tetObj[[tet]][[2]][2]
	
	for (nx in 1 : nrow(obj)) {
		for (ny in 1 : ncol(obj)) {
			if(obj[nx, ny]) {
				a = x - nx + mx
				b = y - ny + my
				
				setTile(a, b, "-")
			}
		}
	}
	
	x <- x + dx
	y <- y + dy
	
	for (nx in 1 : nrow(obj)) {
		for (ny in 1 : ncol(obj)) {
			if(obj[nx, ny]) {
				a = x - nx + mx
				b = y - ny + my
				
				if (a < 1 || b < 1 || a > 10 || 
					b > 20 || board[a, b] != "-") {
						setBoard(bb)
						if (dy != 0) {
							setTet()
							drawTetromino()
						}
						return()
				}
				
				setTile(a, b, tet)
			}
		}
	}
	
	setTet(x, y, tet, rot)
}

rotateTet <- function(drot) {
	x = getTetX()
	y = getTetY()
	tet = getTetT()
	rot = getTetRot()
	
	bb = board
	obj = rotate(tetObj[[tet]][[1]], rot)
	
	mx = tetObj[[tet]][[2]][1]
	my = tetObj[[tet]][[2]][2]
	
	for (nx in 1 : nrow(obj)) {
		for (ny in 1 : ncol(obj)) {
			if(obj[nx, ny]) {
				a = x - nx + mx
				b = y - ny + my
				
				setTile(a, b, "-")
			}
		}
	}
	
	rot = rot + drot
	obj = rotate(tetObj[[tet]][[1]], rot)
	
	for (nx in 1 : nrow(obj)) {
		for (ny in 1 : ncol(obj)) {
			if(obj[nx, ny]) {
				a = x - nx + mx
				b = y - ny + my
				
				if (a < 1 || b < 1 || a > 10 || 
					b > 20 || board[a, b] != "-") {
						setBoard(bb)
						return()
				}
				
				setTile(a, b, tet)
			}
		}
	}
	
	setTet(x, y, tet, rot %% 4)
}

drawTiles <- function() {
	for (x in 1:width) {
		for (y in 1:height) {
			drawTile(x, y, board[x, y])
		}
	}
}

ob <<- board
redrawTiles <- function() {
	rd = list()
	
	for (x in 1 : nrow(difTiles)) {
		for (y in 1 : ncol(difTiles)) {
			dt = difTiles[x, y]
			
			if(dt != 0) {
				if (dt == "-") {
					drawTile(x, y, "-")
					
					for (i in list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))) {
						nx = x + i[1]
						ny = y + i[2]
						if (!(nx > 0 && ny > 0 && nx <= 10 && ny <= 20)) next						
						
						if (board[nx, ny] != "-") {
							if (difTiles[nx, ny] == 0) {
								rd[[length(rd) + 1]] = c(nx, ny, board[nx, ny])
							}
						}
					}
				} else {
					rd[[length(rd) + 1]] = c(x, y, dt)
				}
			}
		}
	}
	
	for (i in rd) {
		drawTile(as.numeric(i[1]), as.numeric(i[2]), i[3])
	}
	
	difTiles <<- matrix(rep(0, width * height), ncol = height, nrow = width)
	ob <<- board
}

drawScore <- function() {
	
}

shiftBoard <- function() {
	ys = c()
	for (y in ncol(board) : 1) {
		full = TRUE
		
		for (x in 1 : nrow(board)) {
			if (board[x, y] == "-") {
				full = FALSE
				break
			}
		}
		
		if (full) {
			ys = c(ys, y)
		}
	}
	
	if (length(ys) == 0) {
		combo <<- 0
		return()
	}
	
	for (i in ys) {
		for (x in 1 : nrow(board)) {
			setTile(x, i, "-")
		}
	}
	
	for (i in ys) {
		shiftDown(i)
	}
	
	updateScore(length(ys))
}

lnScores <<- list(
	c(1, 100),
	c(2, 300),
	c(3, 500),
	c(4, 1200)
)
lnLevels <<- list(
	c(9, 178),
	c(8, 146),
	c(7, 112),
	c(6,  84),
	c(5,  60),
	c(4,  40),
	c(3,  24),
	c(2,  10),
	c(1,   0)
)
lvlSpeed <<- list(
	c(1, 0.9),
	c(2, 0.8),
	c(3, 0.7),
	c(4, 0.6),
	c(5, 0.5),
	c(6, 0.4),
	c(7, 0.3),
	c(8, 0.2),
	c(9, 0.1)
)
updateScore <- function(l) {
	if (l < 1) return()
	
	lns <<- lns + l
	for (i in lnLevels) {
		if (lns >= as.numeric(i[2])) {
			level <<- as.numeric(i[1])
			break
		}
	}
	
	score <<- score + level * lnScores[[l]][2] * (combo + 1)
	
	combo <<- combo + 1
	
	drawMessage(paste("Combo +", combo), "red")
	
	print(paste("Level |", level))
	print(paste("Lines |", lns))
	print(paste("Score |", score))
	print("-------------------")
}

drawMessage <- function(message, col) {
	removeMessage()
	print(message)
	grid.text(
		message,
		x = 0.87,
		y = 0.5,
		gp = gpar(
			lwd = 5,
			col = col,
			cex = 1.5,
			font = 4
		),
	)
	msg <<- message
}

removeMessage <- function() {
	grid.text(
		msg,
		x = 0.87,
		y = 0.5,
		gp = gpar(
			lwd = 5,
			col = "white",
			cex = 1.502,
			font = 4
		)
	)
}

userInput <- function(btn) {
	removeMessage()
	if (btn == "Up" || btn == "w") {
		rotateTet(1)
	} else if (btn == "Down" || btn == "s") {
		moveTet(0, -1)
	} else if (btn == "Left" || btn == "a") {
		moveTet(-1, 0)
	} else if (btn == "Right" || btn == "d") {
		moveTet(1, 0)
	} else if (btn == " ") { #Space-bar button
		if (!swapped) {
			holdTet()
		}
	}
	redraw()
}

gameLoop <- function() {
	draw()
	curTime <<- as.numeric(Sys.time())
	for (i in 1:2^24) {
		tryCatch( 
			{
				expr = {
					evalWithTimeout( {
						getGraphicsEvent("",
							onMouseDown = NULL,
							onMouseMove = NULL,
							onKeybd = userInput)
					}, 
					timeout = 1)
				} 
			},
			error = function(e) {
				if (as.numeric(Sys.time()) - curTime >= lvlSpeed[[level]][2]) {
					curTime <<- as.numeric(Sys.time())
					moveTet(0, -1)
					redraw()
				}
			}
		)
	}
}

gameLoop()



