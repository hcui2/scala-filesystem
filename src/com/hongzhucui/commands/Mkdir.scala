package com.hongzhucui.commands
import com.hongzhucui.files.Directory
import com.hongzhucui.filesystem.State

class Mkdir(name : String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd
    if (wd.hasEntry(name)){
      state.setMessage("Entry " + name + " already exists")
    }
    else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(name + " must not contain separator")
    }
    else if (checkIllegal(name)) {
      state.setMessage(name + ": illegal entry name")

    }
    else {
      doMkdir(state, name)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doMkdir(state: State, name: String): State = {
    val wd = state.wd
    val fullPath = wd.path

    // get all the directory in the full path

    // create new directory entry in the wd

    // update the directory structure staring from the root

    // find new working directory instance given wd's full path, in the new directory structure



  }




}
