package com.hongzhucui.commands
import com.hongzhucui.files.{DirEntry, File}
import com.hongzhucui.filesystem.State

class Touch(name: String) extends CreateEntry (name) {
  override def crreateSpecificEntry(state: State): DirEntry =
    File.empty(state.wd.path, name)
}
