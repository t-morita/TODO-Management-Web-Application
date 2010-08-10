package todo_webapp.snippet

import net.liftweb.http._
import js.JE.{JsRaw, JsEq, ValById, Str}
import js.jquery.JqJE
import js.{JE, JsCmds, JsCmd}
import js.JsCmds._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml._
import collection.immutable.Queue
import todo_webapp.model.TodoUser

/**
 * Created by IntelliJ IDEA.
 * User: Takeshi Morita
 * Date: 2010/08/01
 * Time: 18:14:56
 * 
 */

class AuthSnippet {

    object currentUser extends SessionVar[TodoUser](null)

    def isInvalidUserID(name: String): Boolean = {
      name.exists(isInvalidChar)
    }

    def isInvalidPassword(password: String): Boolean = {
      password.exists(isInvalidChar)
    }

    def isInvalidChar(x: Char): Boolean = {
      !(x.isLetterOrDigit || x == '_')
    }

    def showMessageCmd(id: String, msg: String) = {
      JsCmds.SetHtml({id}, <span>{msg}</span>)
    }

    def checkUserId(userId: String, id: String) = {
      if (userId.isEmpty) {
        showMessageCmd(id, "ユーザIDを入力してください．")
      } else if (isInvalidUserID(userId)) {
        showMessageCmd(id, "ユーザIDには数字またはアルファベットを入力してください．")
      } else {
        JsCmds.Noop
      }
    }

    def checkPassword(password: String, id: String) = {
      if (password.isEmpty) {
        showMessageCmd(id, "パスワードを入力してください．")
      } else if (isInvalidPassword(password)) {
        showMessageCmd(id, "パスワードには数字またはアルファベットを入力してください．")
      } else {
        JsCmds.Noop
      }
    }

    def addUserAction(xhtml: NodeSeq):NodeSeq  = {
        var userName = ""
        var userId = ""
        var password = ""

        def setUserName(un: String): JsCmd = {
            userName = un
            if (userName.isEmpty) {
                JsCmds.SetHtml("add_user_error_message", <p>ユーザ名を入力してください．</p>)
            } else {
                JsCmds.Noop
            }
        }

        def setUserId(id: String): JsCmd = {
            userId = id
            checkUserId(userId, "add_user_error_message")
        }

        def setPassword(p: String): JsCmd = {
            password = p
            checkPassword(password, "add_user_error_message")
        }

        val textBoxClass = "class" -> "text ui-widget-content ui-corner-all"

       def addUser(): JsCmd = {
           if (!(userName.isEmpty || userId.isEmpty || password.isEmpty)) {
               TodoUser.getUser(userId) match {
                   case Some(user) =>
                       JsCmds.SetHtml("add_user_error_message", <p>別のユーザIDにしてください．</p>)
                   case _ =>
                       TodoUser.create.name(userName).userId(userId).password(password).save
                       JsCmds.Noop
               }
           } else {
               JsCmds.SetHtml("add_user_error_message", <p>ユーザ名またはユーザIDまたはパスワードが適切に入力されていません．</p>)
           }
       }

        bind("e", xhtml,
            "userName" --> SHtml.ajaxText(userName, setUserName _, textBoxClass, "size" -> "24"),
            "userId" --> SHtml.ajaxText(userId, setUserId _, textBoxClass, "size" -> "24"),
            "password" --> SHtml.ajaxText(password, setPassword _, textBoxClass, "type" -> "password", "size" -> "24"),
            "addUserButton" --> SHtml.ajaxButton("登録", addUser _,   "id" -> "add_user_button", "style" -> "visibility: hidden;"))
    }

    def logoutAction(xhtml: NodeSeq):  NodeSeq = {

        def logout(): Any = {
            currentUser(null)
            S.redirectTo("login")
        }

        def confirmLogout(): JsCmd = {
            JsCmds.SetHtml("confirm_logout_dialog", <p>ログアウトしますか？</p>) &
                    JsRaw(JE.JsFunc("openConfirmLogoutDialog", JE.Str("ログアウト")).toJsCmd)
        }

        bind("e", xhtml,
            "logoutButton" --> SHtml.submit("logout", logout _, "id" -> "logoutButton", "style" -> "visibility: hidden;"),
            "confirmLogoutButton" --> SHtml.ajaxButton("ログアウト", confirmLogout _))
    }

    def loginAction(xhtml: NodeSeq):  NodeSeq = {
        var userId = ""
        var password = ""

        def auth(): Any = {
            TodoUser.getUser(userId, password) match {
                case Some(user)  =>
                    //  S.containerSession.get.setMaxInactiveInterval(30)
                    currentUser(user)
                    S.redirectTo("list")
                case None =>
                    S.set("error_message", "ユーザ名またはパスワードが違います．")
                    S.redirectTo("error")
            }
        }

        def disableLoginButton(t: String) = {
            SetElemById("login", JsRaw(t), "disabled")
        }


        def setPassword(pass: String): JsCmd = {
          password = pass
          val checkPasswordMsg = "check_password_message"
          val cmd = checkPassword(password, checkPasswordMsg) 
          cmd match {
            case JsCmds.Noop => checkUserIdandPassword(checkPasswordMsg, "パスワードは正常．")
            case _ => cmd & disableLoginButton("true")
          }
        }

        def setUserId(id: String): JsCmd = {
          userId = id
          val checkUserIdMsg = "check_user_id_message"
          val cmd = checkUserId(userId, checkUserIdMsg)
           cmd match {
            case JsCmds.Noop => checkUserIdandPassword(checkUserIdMsg, "ユーザIDは正常．")
            case _ => cmd & disableLoginButton("true")
          }
        }

        def checkUserIdandPassword(id: String, msg: String) : JsCmd = {
          var cmd = showMessageCmd(id, msg)
            TodoUser.getUser(userId, password) match {
            case Some(user)  => cmd & disableLoginButton("false")
            case None => cmd
          }
        }

        def showAddUserDialog() : JsCmd = {
            val addUserForm = LiftRules.loadResourceAsXml("/snippet/addUser.html").openOr(<p>load error</p>)
            val dialogId = "#add_user_dialog"
            JsCmds.SetHtml(dialogId, addUserForm) &
                    JsRaw(JE.JsFunc("openFormDialog", JE.Str(dialogId), JE.Str("ユーザ登録"), JE.Str("#add_user_button")).toJsCmd)
        }

        bind("e", xhtml,
            "userId" --> SHtml.ajaxText("", setUserId _),
            "password" --> SHtml.ajaxText("", setPassword _, "type" -> "password"),
            "login" --> SHtml.submit("ログイン", auth, "id" -> "login", "disabled" -> "true"),
            "addUser" --> SHtml.ajaxButton("ユーザ登録", showAddUserDialog _ ))
    }
}

object AuthSnippet extends AuthSnippet
