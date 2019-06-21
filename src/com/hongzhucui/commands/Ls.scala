package com.hongzhucui.commands
import com.hongzhucui.files.DirEntry
import com.hongzhucui.filesystem.State

class Ls extends Command {

  def createNiceOuput(contents: List[DirEntry]) : String = {
    if (contents.isEmpty) ""
    else {
      val entry = contents.head
      entry.name + "{" + entry.getType + "]\n" + createNiceOuput(contents.tail)
    }
  }

  override def apply(state: State): State = {
    val contents = state.wd.contents
    val niceOutput  = createNiceOuput(contents)
    state.setMessage(niceOutput)
  }
}
