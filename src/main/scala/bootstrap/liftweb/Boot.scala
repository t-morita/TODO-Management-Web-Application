package bootstrap.liftweb

import net.liftweb.util._

import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.common.{Empty, Box, Full}
import java.sql.DriverManager
import net.liftweb.mapper._
import com.mysql.jdbc.Connection
import todo_webapp.model.{TodoItem, TodoUser}
import net.liftweb.widgets.tablesorter.TableSorter

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
        val entries = Menu(Loc("list", List("list"), "List", List(Loc.Hidden))) ::
                Menu(Loc("login", List("login"), "Login")) ::
                Menu(Loc("upload", List("upload"), "Upload", List(Loc.Hidden))) ::
                Menu(Loc("error", List("error"), "Error", List(Loc.Hidden))) ::
                Nil
        TableSorter.init
        LiftRules.setSiteMap(SiteMap(entries:_*))
        Schemifier.schemify(true, Log.infoF _, TodoUser, TodoItem)
    }
}

object DBVendor extends StandardDBVendor("org.h2.Driver", //"jdbc:h2:mem:lift;DB_CLOSE_DELAY=-1",
    "jdbc:h2:tcp://localhost/~/test", Box("sa"), Box(""))