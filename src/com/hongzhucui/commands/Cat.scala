package com.hongzhucui.commands
import com.hongzhucui.filesystem.State

class Cat (filename: String) extends Command {
  /*
  does not surpport absolute path
   */
  override def apply(state: State): State = {
    val wd = state.wd
    val dirEntry = wd.findEntry(filename)
    if (dirEntry == null || !dirEntry.isFile)
      state.setMessage(filename + ": no such file")
    else
      state.setMessage(dirEntry.asFile.contents)
  }
}
