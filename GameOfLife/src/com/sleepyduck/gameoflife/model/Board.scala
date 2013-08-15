package com.sleepyduck.gameoflife.model

import java.awt.Color

object PlayerEnum extends Enumeration {
	type Player = Value
	val Player1, Player2 = Value
}
import PlayerEnum._

case class Board(size: Int = 50) {
	var data = Array.ofDim[Player](size, size)
	var activePlayer = Player1
	var counter = 0
	var counterMax = 0
	var pointsP1 = 10
	var pointsP2 = 10

	def makeMove(x: Int, y: Int, player: Player) = player match {
		case Player1 => data(x)(y) = Player1
		case Player2 => data(x)(y) = Player2
		case _ =>
	}

	def setActivePlayer(player: Player) = {
		activePlayer = player
	}

	def countdown(count: Int, max: Int) = {
		counter = count
		counterMax = max
	}

	def isEmpty(x: Int, y: Int) = {
		data(x)(y) == null
	}

	def givePointTo(player: Player) = player match {
		case Player1 => if (pointsP1 < 54) pointsP1 += 1
		case Player2 => if (pointsP2 < 54) pointsP2 += 1
		case _ =>
	}

	def takePointFrom(player: Player) = player match {
		case Player1 => pointsP1 -= 1
		case Player2 => pointsP2 -= 1
		case _ =>
	}

	def hasPoint(player: Player) = player match {
		case Player1 => pointsP1 > 0
		case Player2 => pointsP2 > 0
		case _ => false
	}

	def countPoints(player: Player) = player match {
		case Player1 => pointsP1
		case Player2 => pointsP2
		case _ => 0
	}

	def clear = {
		data = Array.ofDim[Player](size, size)
		pointsP1 = 10
		pointsP2 = 10
		counter = 0
		counterMax = 0
	}

	def count(player: Player) = {
		(for {
			X <- 0 until size
			Y <- 0 until size
			if data(X)(Y) == player
		} yield 1).foldLeft(0)(_ + _)
	}

	def setWinner(player: Player) = {
		def setWinner(dataRow: Array[Player]) = for (x <- 0 to 1) (dataRow(x * (size - 1)) = player)
		data map setWinner
	}
}