package com.hongzhucui.commands

import com.hongzhucui.files.{DirEntry, Directory}
import com.hongzhucui.filesystem.State

abstract class CreateEntry(name: String) extends Command{

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
      doCreateEntry(state, name)
    }
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry) : Directory = {
    if (path.isEmpty) currentDirectory.addEntry(newEntry )
    else {
      val oldEntry = currentDirectory.findEntry(path.head).asDirectory
      currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
    }
  }

  def doCreateEntry(state: State, name: String): State = {
    val wd = state.wd

    // get all the directory in the full path
    val allDirsInPath = wd.getAllFoldersInPath

    // create new directory entry in the wd
    // val newDir = Directory.empty(wd.path, name)
    val newEntry: DirEntry = crreateSpecificEntry(state)

    // update the directory structure staring from the root
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)

    // find new working directory instance given wd's full path, in the new directory structure
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def crreateSpecificEntry(state: State): DirEntry
}
