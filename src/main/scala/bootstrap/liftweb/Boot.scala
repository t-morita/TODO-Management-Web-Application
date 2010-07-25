package bootstrap.liftweb

import net.liftweb.util._

import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.common.{Empty, Box, Full}
import java.sql.DriverManager
import net.liftweb.mapper._
import com.mysql.jdbc.Connection
import todo_webapp.model.{TodoItem, TodoUser}

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
    def boot {
        // where to search snippet
        LiftRules.addToPackages("todo_webapp")
        DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)

        // Build SiteMap
        val entries = Menu(Loc("index", List("index"), "Home")) ::
                Menu(Loc("list", List("list"), "List")) ::
                Menu(Loc("login", List("login"), "Login")) ::
                Menu(Loc("add", List("add"), "Add")) ::
                Menu(Loc("edit", List("edit"), "Edit")) ::
                Menu(Loc("delete", List("delete"), "Delete")) ::
                Menu(Loc("upload", List("upload"), "Upload")) ::
                Menu(Loc("error", List("error"), "Error")) ::
                Nil
        LiftRules.setSiteMap(SiteMap(entries:_*))
        Schemifier.schemify(true, Log.infoF _, TodoUser, TodoItem)
    }
}

object DBVendor extends StandardDBVendor("org.h2.Driver", //"jdbc:h2:mem:lift;DB_CLOSE_DELAY=-1",
    "jdbc:h2:tcp://localhost/~/test", Box("sa"), Box(""))