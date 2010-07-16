package todo_webapp.snippet

import net.liftweb.http._
import net.liftweb.util.Helpers._
import scala.xml._
/**
 * Created by IntelliJ IDEA.
 * User: Takeshi Morita
 * Date: 2010/07/05
 * Time: 1:24:15
 * 
 */

class TodoWebapp {

    def loginAction(xhtml: NodeSeq):  NodeSeq = {
        var userId = ""
        var password = ""
        def auth(): Any = {
            S.notice("Entry is " + userId + ", " + password)
            if (userId == "t_morita") {
                S.redirectTo("index")
            }
        }
        bind("e", xhtml,
             "userId" --> SHtml.text(userId, userId = _),
        "password" --> SHtml.password(password, password = _),
        "submit" --> SHtml.submit("ログイン", auth))
    }
}
