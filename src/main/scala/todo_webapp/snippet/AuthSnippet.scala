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

    /**
     * 有効なユーザIDかどうかを判定します。
     *
     * @param name
     * @return
     */
    def isValidUserID(name: String): Boolean = {
        isAlphaOrDigit(name);
    }

    /**
     * 有効なパスワードかどうかを判定します。
     *
     * @param password
     * @return
     */
    def isValidPassword(password: String): Boolean = {
        return isAlphaOrDigit(password);
    }

    /**
     * 文字列が半角英数字から構成されているかどうかを判定します。
     *
     * @param str
     * @return
     */
    def isAlphaOrDigit(str: String): Boolean = {
        for (ch <- str) {
            if (!(ch.isLetterOrDigit || ch == '_')) return false
        }
        return true
    }


    def addUserAction(xhtml: NodeSeq):NodeSeq  = {
        var userName = ""
        var userId = ""
        var password = ""

        def setUserName(un: String): JsCmd = {
            userName = un
            if (userName == "") {
                JsCmds.SetHtml("add_user_error_message", <p>ユーザ名を入力してください．</p>)
            } else {
                return null
            }
        }

        def setUserId(id: String): JsCmd = {
            userId = id
            if (userId == "") {
                JsCmds.SetHtml("add_user_error_message", <p>ユーザIDを入力してください．</p>)
            } else {
                return null
            }
        }

        def setPassword(p: String): JsCmd = {
            password = p
            if (password == "") {
                JsCmds.SetHtml("add_user_error_message", <p>パスワードを入力してください．</p>)
            } else {
                return null
            }
        }

        val textBoxClass = "class" -> "text ui-widget-content ui-corner-all"

       def addUser(): JsCmd = {
           if (userName != "" && userId != "" && password != "") {
               TodoUser.getUser(userId) match {
                   case Some(user) =>
                       JsCmds.SetHtml("add_user_error_message", <p>別のユーザIDにしてください．</p>)
                   case _ =>
                       TodoUser.create.name(userName).userId(userId).password(password).save
                       return null
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
        var validPassword = false
        var validUserId = false

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

        def showMessage(id: String, msg: String) = {
            JsCmds.SetHtml({id}, <span>{msg}</span>)
        }

        def checkPassword(pass: String): JsCmd = {
            password = pass
            val check_password_message = "check_password_msg"
            if (password.length == 0) {
                validPassword = false
                showMessage(check_password_message, "パスワードが空です．") &
                        disableLoginButton("true")
            } else if (!isValidPassword(password)) {
                validPassword = false
                showMessage(check_password_message, "パスワードには数字またはアルファベットを入力してください．") &
                        disableLoginButton("true")
            } else {
                validPassword = true
                var msg = showMessage(check_password_message, "パスワード入力は正常．")
                checkUserIdandPassword(msg)
            }
        }

        def checkUserId(id: String): JsCmd = {
            userId = id
            val check_user_id_message = "check_user_id_msg"
            if (userId.length == 0) {
                validUserId = false
                showMessage(check_user_id_message,"ユーザIDが空です．") &
                        disableLoginButton("true")
            } else if (!isValidUserID(userId)) {
                validUserId = false
                showMessage(check_user_id_message,"ユーザIDには数字またはアルファベットを入力してください．") &
                        disableLoginButton("true")
            } else {
                validUserId = true
                var msg = showMessage(check_user_id_message, "ユーザIDは正常．")
                checkUserIdandPassword(msg)
            }
        }

        def checkUserIdandPassword(msg: JsCmd) : JsCmd = {
            if (validUserId && validPassword) {
                msg & disableLoginButton("false")
            } else {
                msg
            }
        }

        def showAddUserDialog() : JsCmd = {
            val addUserForm = <lift:AuthSnippet.addUserAction form="POST">
                    <div id="add_user_error_message"/>
                <fieldset>
                    <label>ユーザ名</label><br/>
                        <e:userName/><br/>
                    <label>ユーザID</label><br/>
                        <e:userId/><br/>
                    <label>パスワード</label><br/>
                        <e:password/><br/>
                        <e:addUserButton/>
                </fieldset>
            </lift:AuthSnippet.addUserAction>

            val dialogId = "#add_user_dialog"
            JsCmds.SetHtml(dialogId, addUserForm) &
                    JsRaw(JE.JsFunc("openFormDialog", JE.Str(dialogId), JE.Str("ユーザ登録"), JE.Str("#add_user_button")).toJsCmd)
        }

        bind("e", xhtml,
            "userId" --> SHtml.ajaxText("", checkUserId _),
            "password" --> SHtml.ajaxText("", checkPassword _, "type" -> "password"),
            "login" --> SHtml.submit("ログイン", auth, "id" -> "login", "disabled" -> "true"),
            "addUser" --> SHtml.ajaxButton("ユーザ登録", showAddUserDialog _ ))
    }
}

object AuthSnippet extends AuthSnippet