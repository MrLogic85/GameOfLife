package com.sleepyduck.gameoflife.control

import com.sleepyduck.gameoflife.model.Board
import com.sleepyduck.gameoflife.model.PlayerEnum._
import scala.actors.Actor

class GameRules {
	var repaint: () => Unit = null
	var board: Board = null
	var running = false
	var boardUnchanged = false

	def isAlive(p: Player) = (board count p) + (board countPoints p) != 0
	def isNotAlive(p: Player) = !isAlive(p)

	def set(board: Board) = {
		this.board = board
		board setActivePlayer mostPoints
	}

	def mouseClicked(x: Int, y: Int) = {
		if (x >= 0 && y >= 0 && x < board.size && y < board.size) {
			if (board hasPoint board.activePlayer) {
				if (board isEmpty (x, y)) {
					board.makeMove(x, y, board.activePlayer)
					board takePointFrom board.activePlayer
					if (board hasPoint nextPlayer)
						board setActivePlayer nextPlayer
					else if (!board.hasPoint(board.activePlayer))
						board setActivePlayer null
				}
			}
		}
	}

	def keyPressed(c: Char) = {
		c match {
			case 'r' =>
				running = !running
				if (running) runBoard
			case 'c' =>
				board.clear
				board setActivePlayer mostPoints
				running = false
			case _ =>
		}
	}

	def nextPlayer = board.activePlayer match {
		case Player1 => Player2
		case Player2 => Player1
	}

	def runBoard = {
		Actor.actor {
			do {
				for { c <- 0 until 10 } {
					synchronized(wait(200))
					board countdown (c, 10)
					step
				}
				if (someoneHasLost)
					running = false
			} while (running && mostPoints == null && !boardUnchanged)
			running = false
			boardUnchanged = false
		}
	}

	def someoneHasLost = {
		def p1Lost = this isNotAlive Player1
		def p2Lost = this isNotAlive Player2
		if (p1Lost && !p2Lost)
			board setWinner Player2
		else if (p2Lost && !p1Lost)
			board setWinner Player1
		p1Lost || p2Lost
	}

	def mostPoints: Player = {
		if (board.pointsP1 > board.pointsP2)
			Player1
		else if (board.pointsP2 > board.pointsP1)
			Player2
		else if (board.pointsP1 > 0)
			(Math.random() * 2).toInt match {
				case 0 => Player1
				case 1 => Player2
				case _ => null
			}
		else
			null
	}

	def step = {
		val newData = Array.ofDim[Player](board.size, board.size)
		for {
			x <- 0 until board.size
			y <- 0 until board.size
		} {
			val wasAliveP1 = board.data(x)(y) == Player1
			val countP1 = count(x, y, Player1)
			val aliveP1 = alive(wasAliveP1, count(x, y, Player1))

			val wasAliveP2 = board.data(x)(y) == Player2
			val countP2 = count(x, y, Player2)
			val aliveP2 = alive(wasAliveP2, count(x, y, Player2))

			if (aliveP1 && aliveP2) {
				if (countP1 > countP2) {
					newData(x)(y) = Player1
					this givePointTo Player1
				} else if (countP2 > countP1) {
					newData(x)(y) = Player2
					this givePointTo Player2
				}
			} else if (aliveP1) {
				newData(x)(y) = Player1
				if (wasAliveP2)
					this givePointTo Player1
			} else if (aliveP2) {
				newData(x)(y) = Player2
				if (wasAliveP1)
					this givePointTo Player2
			}
		}
		boardUnchanged = board.data.deep == newData.deep
		board.data = newData
	}

	def givePointTo(player: Player) = {
		board givePointTo player
		board setActivePlayer mostPoints
	}

	def count(x: Int, y: Int, player: Player) = {
		val x1 = Math.max(x - 1, 0)
		val y1 = Math.max(y - 1, 0)
		val x2 = Math.min(x + 1, board.size - 1)
		val y2 = Math.min(y + 1, board.size - 1)
		(for {
			X <- x1 to x2
			Y <- y1 to y2
			if X != x || Y != y
			if board.data(X)(Y) == player
		} yield 1).foldLeft(0)(_ + _)
	}

	def alive(wasAlive: Boolean, count: Int) = {
		if (wasAlive && count < 4 && count > 1) true
		else if (count == 3) true
		else false
	}
}