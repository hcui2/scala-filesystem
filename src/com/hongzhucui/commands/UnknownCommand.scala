package com.hongzhucui.commands

import com.hongzhucui.filesystem.State

class UnknownCommand extends Command {

  override def apply(state: State): State =
    state.setMessage("Command not Found!")
}
