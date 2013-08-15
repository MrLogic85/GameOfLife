package com.sleepyduck.gameoflife

import com.sleepyduck.gameoflife.model.Board
import com.sleepyduck.gameoflife.control.GameRules
import com.sleepyduck.gameoflife.view.UIFrame

object Main extends App {
	val board = new Board
	val UI = new UIFrame
	val rules = new GameRules
	rules set board
	UI set board
	UI addMouseListener rules.mouseClicked
	UI addKeyListener rules.keyPressed
	UI open
}