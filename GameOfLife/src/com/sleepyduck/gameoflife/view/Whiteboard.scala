package com.sleepyduck.gameoflife.view

import java.awt.Color
import java.awt.Graphics2D

import scala.swing.Panel

import com.sleepyduck.gameoflife.model.Board
import com.sleepyduck.gameoflife.model.PlayerEnum._

class Whiteboard(board: Board, menuHeight: Int = 50) extends Panel {
	val mousePos = new MousePos

	override def paintComponent(g: Graphics2D) {
		val dx = g.getClipBounds.width.toFloat / board.size.toFloat
		val dy = g.getClipBounds.width.toFloat / board.size.toFloat

		// Draw player moves
		for {
			x <- 0 until board.size
			y <- 0 until board.size
			x1 = (x * dx).toInt
			y1 = (y * dy).toInt
			x2 = ((x + 1) * dx).toInt
			y2 = ((y + 1) * dy).toInt
		} {
			g setColor getColor(board.data(x)(y))
			g fillRect (x1, y1 + menuHeight, x2 - x1, y2 - y1)
		}

		// Draw active player symbol
		g setColor getColor(board.activePlayer)
		val size = (menuHeight - 2 * dy).toInt
		g.fillRect((g.getClipBounds.width - size) / 2, dy toInt, size, size)

		// Draw player points
		for { i <- 0 until board.pointsP1 } {
			val iy = (i % 3 + 1) * dx.toInt
			val ix = (i / 3 + 1) * dy.toInt
			g setColor Color.RED
			g.fillRect(ix, iy, dx.toInt, dy.toInt)
			g setColor Color.WHITE
			g.drawRect(ix, iy, dx.toInt, dy.toInt)
		}

		for { i <- 0 until board.pointsP2 } {
			val iy = (i % 3 + 1) * dx.toInt
			val ix = g.getClipBounds.width - dx.toInt - (i / 3 + 1) * dy.toInt
			g setColor Color.BLUE
			g.fillRect(ix, iy, dx.toInt, dy.toInt)
			g setColor Color.WHITE
			g.drawRect(ix, iy, dx.toInt, dy.toInt)
		}

		// Draw mouse highlight
		if (mousePos.x >= 0 && mousePos.y >= 0 && mousePos.x < board.size && mousePos.y < board.size) {
			val x1 = (mousePos.x * dx).toInt
			val y1 = (mousePos.y * dy).toInt
			val x2 = ((mousePos.x + 1) * dx).toInt
			val y2 = ((mousePos.y + 1) * dy).toInt
			g setColor getColor(board.activePlayer, 127, new Color(0, 0, 0, 127))
			g.fillRect(x1, y1 + menuHeight, x2 - x1, y2 - y1)
		}

		// Draw counter
		if (board.counterMax > 0) {
			g setColor Color.BLACK
			for {
				i <- 0 until board.counterMax
				x1 = (g.getClipBounds.width.toFloat / 2.0 - dx * board.counterMax / 2.0 + dx * i).toInt
				y1 = (menuHeight - dy).toInt
			} {
				if (board.counter < i) g.drawRect(x1, y1, dx.toInt, dy.toInt)
				else g.fillRect(x1, y1, dx.toInt, dy.toInt)
			}
		}
	}

	def getColor(player: Player, alpha: Int = 255, default: Color = Color.WHITE) = player match {
		case Player1 => new Color(255, 0, 0, alpha)
		case Player2 => new Color(0, 0, 255, alpha)
		case _ => default
	}
}