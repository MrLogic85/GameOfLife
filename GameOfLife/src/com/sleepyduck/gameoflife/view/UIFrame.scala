package com.sleepyduck.gameoflife.view

import java.awt.Dimension
import scala.collection.mutable.ArrayBuffer
import scala.swing.MainFrame
import scala.swing.event.KeyPressed
import scala.swing.event.MouseClicked
import scala.swing.event.MouseMoved
import com.sleepyduck.gameoflife.model.Board
import scala.swing.event.MouseDragged
import scala.actors.Logger
import java.util.logging.Logger
import java.util.logging.Level
import scala.swing.event.MouseClicked
import scala.swing.event.MouseMoved
import scala.swing.event.MouseDragged
import scala.actors.Actor

class UIFrame(pixels: Int = 500, menuHeight: Int = 50) extends MainFrame {

	var board: Board = null
	var whiteboard: Whiteboard = null

	def findPosOfX(x: Int) = x * board.size / pixels
	def findPosOfY(y: Int) = (y - menuHeight) * board.size / pixels

	var mouseListeners = new ArrayBuffer[(Int, Int) => Unit]
	def addMouseListener(listener: (Int, Int) => Unit) = mouseListeners += listener
	def callMouseListener(x: Int, y: Int, listener: (Int, Int) => Unit) = listener(findPosOfX(x), findPosOfY(y))

	var keyListeners = new ArrayBuffer[(Char) => Unit]
	def addKeyListener(listener: (Char) => Unit) = keyListeners += listener
	def callKeyListener(c: Char, listener: (Char) => Unit) = listener(c)

	def set(board: Board) = {
		this.board = board
		whiteboard = new Whiteboard(board, menuHeight) {
			preferredSize = new Dimension(pixels, pixels + menuHeight)

			this listenTo mouse.clicks
			this listenTo mouse.moves
			this listenTo keys

			reactions += {
				case e: MouseClicked =>
					UIFrame.this mouseClicked (e.point.x, e.point.y)
					e consume
				case e: MouseDragged =>
					UIFrame.this mouseDragged (e.point.x, e.point.y)
					e consume
				case e: MouseMoved =>
					UIFrame.this mouseMoved (e.point.x, e.point.y)
					e consume
				case e: KeyPressed =>
					UIFrame.this keyPressed e.peer.getKeyChar()
					e consume
			}

		}
		contents = whiteboard
		whiteboard requestFocus
	}

	def mouseClicked(x: Int, y: Int) = {
		val tmp = callMouseListener(x, y, _: (Int, Int) => Unit)
		mouseListeners map tmp
	}

	def mouseMoved(x: Int, y: Int) = {
		whiteboard.mousePos.x = findPosOfX(x)
		whiteboard.mousePos.y = findPosOfY(y)
	}

	def mouseDragged(x: Int, y: Int) = mouseMoved(x, y);

	def keyPressed(c: Char) = {
		val tmp = callKeyListener(c, _: (Char) => Unit)
		keyListeners map tmp

	}

	Actor.actor {
		while (true) {
			synchronized(wait(10))
			repaint
		}
	}
}