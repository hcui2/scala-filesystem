package com.hongzhucui.commands
import com.hongzhucui.files.{DirEntry, Directory}
import com.hongzhucui.filesystem.State

class Mkdir(name : String) extends CreateEntry(name) {

  override def crreateSpecificEntry(state: State): DirEntry =
    Directory.empty(state.wd.path, name)

}
