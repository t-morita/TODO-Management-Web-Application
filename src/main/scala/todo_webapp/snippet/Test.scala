package todo_webapp.snippet

import net.liftweb.util.Helpers._
import xml.NodeSeq
import net.liftweb.http.{SHtml, S}
import net.liftweb.http.js.JE.{JsEq, ValById, Str, JsRaw}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds._
/**
 * Created by IntelliJ IDEA.
 * User: Takeshi Morita
 * Date: 2010/07/25
 * Time: 0:39:54
 * 
 */

class Test {

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

    def ajaxTest(xhtml: NodeSeq): NodeSeq = {

        var validPassword = false
        var validUserId = false

        def test2() : Any = {
        }


        def showMessage(id: String, msg: String) = {
            JsCmds.SetHtml({id}, <span>{msg}</span>)
        }

        def disableLoginButton() = {
            SetElemById("login", JsRaw("true"), "disabled")
        }

        def enableLoginButton() = {
            SetElemById("login", JsRaw("false"), "disabled")
        }

        def checkPassword(password: String): JsCmd = {
            val check_password_message = "check_password_msg"
            if (password.length == 0) {
                validPassword = false
                showMessage(check_password_message, "パスワードが空です．") &
                        disableLoginButton()
            } else if (!isValidPassword(password)) {
                validPassword = false
                showMessage(check_password_message, "パスワードには数字またはアルファベットを入力してください．") &
                        disableLoginButton()
            } else {
                validPassword = true
                var msg = showMessage(check_password_message, "パスワード入力は正常．")
                if (validUserId && validPassword) {
                    msg & enableLoginButton()
                } else {
                    msg
                }
            }
        }

        def checkUserId(userId: String): JsCmd = {
            val check_user_id_message = "check_user_id_msg"
            if (userId.length == 0) {
                validUserId = false
                showMessage(check_user_id_message,"ユーザIDが空です．") &
                        disableLoginButton()
            } else if (!isValidUserID(userId)) {
                validUserId = false
                showMessage(check_user_id_message,"ユーザIDには数字またはアルファベットを入力してください．") &
                        disableLoginButton()
            } else {
                validUserId = true
                var msg = showMessage(check_user_id_message, "ユーザIDは正常．")
                if (validUserId && validPassword) {
                    msg & enableLoginButton()
                } else {
                    msg
                }
            }
        }

        def test(): JsCmd = {
            println(S.param("input").openOr("none"))
            JsIf(JsEq(ValById("input"), Str("hello")), {
                JsCmds.SetHtml("message", <p>Hello, Ajax</p>)
            })
        }

        bind("e", xhtml,
            "input" --> SHtml.ajaxText("", checkUserId _),
            "password" --> SHtml.ajaxText("", checkPassword _, "type" -> "password"),
            "button" --> SHtml.ajaxButton("test", test _, "id" -> "login"))
        //                 "button" --> SHtml.submit("test2", test2 _))
    }
}