package com.hongzhucui.files

import com.hongzhucui.filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, contents: String)
  extends DirEntry(parentPath, name) {

  override def asDirectory: Directory = throw new FileSystemException("A file cannot be converted to a directory")

  override def asFile: File = this

  override def getType: String = "File"
}

object File {
  def empty(parentPath: String, name: String) : File =
    new File(parentPath, name, "")

}