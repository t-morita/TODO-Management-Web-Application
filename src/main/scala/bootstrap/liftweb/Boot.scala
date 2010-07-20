package bootstrap.liftweb

import net.liftweb.util._

import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("todo_webapp")     

    // Build SiteMap
    val entries = Menu(Loc("index", List("index"), "Home")) ::
       Menu(Loc("list", List("list"), "List")) ::
       Menu(Loc("login", List("login"), "Login")) ::
       Menu(Loc("add", List("add"), "Add")) ::
       Menu(Loc("edit", List("edit"), "Edit")) ::
            Menu(Loc("delete", List("delete"), "Delete")) ::
            Menu(Loc("search", List("search"), "Search")) ::
       Menu(Loc("upload", List("upload"), "Upload")) ::
          Menu(Loc("error", List("error"), "Error")) ::
             Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))
  }
}
