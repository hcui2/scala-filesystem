package com.hongzhucui.commands

import com.hongzhucui.filesystem.State

trait Command {
  def apply (state: State) : State
}


object Command {

  val MKDIR = "mkdir"
  val LS = "ls"

  def emptyCommand: Command = new Command {
    override def apply(state: State): State = state
  }

  def inCompleteCommand(name: String): Command = new Command {
    override def apply(state: State): State =
      state.setMessage(name + ": incomplete command!")
  }

  def from (input: String): Command = {
    val tokens: Array[String] = input.split(" ")

    if (input.isEmpty || tokens.isEmpty) emptyCommand
    else if ("mkdir".equals(tokens(0))) {
      if (tokens.length < 2) inCompleteCommand(MKDIR)
      else new Mkdir(tokens(1))
    }
    else if (LS.equals(tokens(0))){
      new Ls
    }
    else new UnknownCommand
  }

}