package todo_webapp.snippet

import todo_webapp.model._
import net.liftweb.http._

import js.JE.{JsRaw, JsEq, ValById, Str}
import js.jquery.JqJE
import js.{JE, JsCmds, JsCmd}
import js.JsCmds._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml._
import collection.immutable.Queue
import net.liftweb.mapper.{Like, By}
import java.util.{Date, Calendar}
import java.text.SimpleDateFormat
import net.liftweb.widgets.tablesorter.TableSorter

/**
 * Created by IntelliJ IDEA.
 * User: Takeshi Morita
 * Date: 2010/07/05
 * Time: 1:24:15
 *
 */
class TodoWebapp {

    def getUserName(): NodeSeq = {
        if (currentUser.is == null)  {
            S.set("error_message", "ログインしていません．")
            S.redirectTo("error")
        }
        <span>{currentUser.is.name}</span>
    }

    def getItemTable(itemList: List[TodoItem]): NodeSeq = {
        val sdf1 = new SimpleDateFormat("yyyy年MM月dd日");
        var itemTable = new Queue[Node]
        if (itemList.size == 0) {
            return <p>作業項目はありません。</p>
        }
        for (item <- itemList) {
            var css = ""
            val itemId = item.id.toString
            var finished = "未"
            var isFinished = "false"
            val deadline: Date = item.deadline
            val user = getUserById(item.userId).get
            if (item.finishedDate != null)  {
                css = "tbl_sty1"
                val finishedDate: Date = item.finishedDate
                finished = sdf1.format(finishedDate)
                isFinished = "true"
            } else  if (currentUser.is == user && item.deadline.getTime >= Calendar.getInstance().getTimeInMillis) {
                css = "tbl_sty2"
            } else if (currentUser.is == user && item.deadline.getTime < Calendar.getInstance().getTimeInMillis) {
                css = "tbl_sty3"
            } else if (item.deadline.getTime < Calendar.getInstance().getTimeInMillis) {
                css = "tbl_sty4"
            }
            itemTable += <tr id={"itemId"+itemId}>
                <td class={css}>{item.name}</td>
                <td class={css}>{user.name}</td>
                <td class={css}>{sdf1.format(deadline)}</td>
                <td class={css}>{finished}</td>
                <td class={css}>
                    <lift:TodoWebapp.toggleFinishedAction form="POST" item_id={itemId} isFinished={isFinished}>
                            <e:isFinished/>
                    </lift:TodoWebapp.toggleFinishedAction>
                </td>
                <td class={css}>
                    <lift:TodoWebapp.editPageAction form="POST" item_id={itemId}>
                            <e:itemId/>
                            <e:editTodo/>
                    </lift:TodoWebapp.editPageAction>
                </td>
                <td class={css}>
                    <lift:TodoWebapp.deleteAction form="POST" item_id={itemId} >
                            <e:deleteItem/>
                            <e:confirmDeleteItem/>
                    </lift:TodoWebapp.deleteAction>
                </td>
            </tr>
        }
        itemTable
    }

    def sortTable() :NodeSeq = {
        TableSorter("#todolist_table")
    }

    def list(): NodeSeq = {
        getItemTable(getItemList)
    }

    def logoutAction(xhtml: NodeSeq):  NodeSeq = {
        
        def logout(): Any = {
            currentUser(null)
            S.redirectTo("login")
        }

        def confirmLogout(): JsCmd = {
            JsRaw(JE.JsFunc("openConfirmLogoutDialog", JE.Str("ログアウトしますか？"), JE.Str("ログアウト")).toJsCmd)
        }

        bind("e", xhtml,
            "logoutButton" --> SHtml.submit("logout", logout _, "id" -> "logoutButton", "style" -> "visibility: hidden;"),
            "confirmLogoutButton" --> SHtml.ajaxButton("ログアウト", confirmLogout _))
    }

    def errorAction(xhtml: NodeSeq):  NodeSeq = {

        def cancel(): Any = {
            S.redirectTo("login")
        }

        bind("e", xhtml,
            "error" --> <span>{S.get("error_message").openOr("Error")}</span>,
            "return" --> SHtml.submit("戻る", cancel))
    }

    // 要素数が１かそれ以外かで分けてもいいかもしれない
    def getUserById(id: Long): Option[TodoUser] = {
        val userList = TodoUser.findAll(By(TodoUser.id, id))
        if (userList.size == 0) {
            None
        } else {
            Some(userList(0))
        }
    }

    def getUser(userId: String): Option[TodoUser] = {
        val userList = TodoUser.findAll(By(TodoUser.userId, userId))
        if (userList.size == 0) {
            None
        } else {
            Some(userList(0))
        }
    }

    def getUser(userId: String, password: String): Option[TodoUser] = {
        val userList = TodoUser.findAll(By(TodoUser.userId, userId),By(TodoUser.password, password))
        if (userList.size == 0) {
            None
        } else {
            Some(userList(0))
        }
    }

    def insertItem(name: String, selectedUserId: Long, date: Date) = {
        val newItem  = TodoItem.create.name(name).userId(selectedUserId).deadline(date)
        newItem.save
    }

    def updateItem(id: Long,  finishedDate: Date) = {
        val updateItem = getItem(id).get.finishedDate(finishedDate)
        updateItem.save
    }

    def updateItem(id: Long, name: String, selectedUserId: Long, deadline: Date, finishedDate: Date) = {
        val updateItem = getItem(id).get.name(name).userId(selectedUserId).deadline(deadline).finishedDate(finishedDate)
        updateItem.save
    }

    def getDate(year: String, month: String, day: String): Option[Date] = {
        try {
            val calendar = Calendar.getInstance()
            calendar.clear()
            calendar.set(year.toInt, month.toInt - 1, day.toInt)
            Some(new Date(calendar.getTimeInMillis()))
        } catch {
            case e: NumberFormatException => None
        }
    }

    def editAction(xhtml: NodeSeq):  NodeSeq = {

        val itemId = S.get("item_id").openOr("none")
        val currentItem: TodoItem = getItem(itemId.toLong).get
        S.notice(currentItem.toString)

        val id = currentItem.id
        var name: String = currentItem.name
        S.notice(currentItem.deadline.toString)
        val cal = Calendar.getInstance()
        cal.setTime(currentItem.deadline)
        var year = cal.get(Calendar.YEAR).toString
        var month = (cal.get(Calendar.MONTH) + 1).toString
        var day = cal.get(Calendar.DAY_OF_MONTH).toString
        var selectedUserId: Long = currentItem.userId
        var isFinished = false
        if (currentItem.finishedDate != null) {
            isFinished = true
        }

        val userMap = getUserList.map(u => u.id.toString -> u.name.toString)

        def setSelectedUser(userId: String): Any = {
            selectedUserId = userId.toLong
        }

        def getFinishedDate(): Date = {
            if (isFinished) {
                new Date
            } else {
                null
            }
        }
        def editTodo(): Any = {
            getDate(year, month, day) match {
                case Some(date) =>
                    updateItem(id, name, selectedUserId, date, getFinishedDate)
                    S.redirectTo("list")
                case None => S.error("カレンダーの書式が間違っています")
            }
        }

        def setIsFinished(t: Boolean):Any = {
            isFinished = t
        }

        bind("e", xhtml,
            "name" --> SHtml.text(name, name = _, "size" -> "24"),
            "personInCharge" --> SHtml.select(userMap, Box(selectedUserId.toString), setSelectedUser(_)),
            "year" --> SHtml.text(year, year = _, "size" -> "8"),
            "month" --> SHtml.text(month, month = _, "size" -> "4"),
            "day" --> SHtml.text(day, day = _, "size" -> "4"),
            "finishedCheckBox" --> SHtml.checkbox(isFinished, setIsFinished(_), "size" -> "8") ,
            "submit" --> SHtml.submit("更新", editTodo))
    }


    def addPageAction(xhtml: NodeSeq):  NodeSeq = {
        def redirectAddPage() = {
            S.redirectTo("add")
        }
        bind("e", xhtml,
            "submitTodo" --> SHtml.submit("作業登録", redirectAddPage))
    }

    def editPageAction(xhtml: NodeSeq):  NodeSeq = {

        def redirectEditPage() = {
            S.set("item_id", S.param("item_id").openOr("none"))
            S.redirectTo("edit")
        }

        bind("e", xhtml,
            "itemId" --> SHtml.hidden(null, S.attr("item_id").openOr("none"), "name" -> "item_id"),
            "editTodo" --> SHtml.submit("更新", redirectEditPage))
    }

    def toggleFinishedAction(xhtml: NodeSeq):  NodeSeq = {

        val itemId =  S.attr("item_id").openOr("none")
        val isFinished = S.attr("isFinished").openOr("none").toBoolean

        def toggleFinished(): JsCmd = {
            updateItem(itemId.toLong, getFinishedDate())
            val keyword = S.get("last_search_keyword").openOr("")
            val itemList: List[TodoItem] = getSearchItemList(keyword)
            JsCmds.SetHtml("todolist", getItemTable(itemList)) &
                    JsRaw(JE.JsFunc("updateTodoListTable").toJsCmd)
        }

        def getFinishedDate(): Date = {
            isFinished match {
                case false => new Date
                case _ => null
            }
        }
        
        val buttonLabel = isFinished match {
            case true => "未完了"
            case false => "完了"
            case _ => "エラー"
        }
        
        bind("e", xhtml, "isFinished" --> SHtml.ajaxButton(buttonLabel, toggleFinished _))
    }

    def deleteAction(xhtml: NodeSeq):  NodeSeq = {

        val delItemId = S.attr("item_id").openOr("none")
        val delItem: TodoItem = getItem(delItemId.toLong).get

       def deleteTodoItem() : JsCmd = {
           delItem.delete_!
           return null;
       }

        def confirmDeleteTodoItem(): JsCmd = {
            val title = "削除確認"
            val message = "項目"+ delItem.name + "を削除します。\n よろしいですか？"
            JsRaw(JE.JsFunc("openConfirmDialog", JE.Str(message), JE.Str(title), JE.Str(delItemId)).toJsCmd)
        }

        bind("e", xhtml,
            "deleteItem" --> SHtml.ajaxButton("del", deleteTodoItem _, "id" -> ("delItemId"+delItemId), "style" -> "visibility:hidden;"),
            "confirmDeleteItem" --> SHtml.ajaxButton("削除", confirmDeleteTodoItem _))
    }

    def searchAction(xhtml: NodeSeq):  NodeSeq = {
        var keyword = ""

        def setKeyword(kw: String): JsCmd  = {
            keyword = kw
            return null
        }

        def searchTodoItem():JsCmd  = {
            S.set("last_search_keyword", keyword)
            val itemList = getSearchItemList(keyword)
            JsCmds.SetHtml("todolist", getItemTable(itemList)) &
                    JsRaw(JE.JsFunc("updateTodoListTable").toJsCmd)
        }

        bind("e", xhtml,
            "keyword" --> SHtml.ajaxText("", setKeyword _, "size" -> "24"),
            "search" --> SHtml.ajaxButton("検索", searchTodoItem _) )
    }

    def addAction(xhtml: NodeSeq):  NodeSeq = {
        var name = ""
        var year = ""
        var month = ""
        var day = ""
        var selectedUserId = ""

        def entryTodo(): Any = {
            getDate(year, month, day) match {
                case Some(date) =>
                    insertItem(name, selectedUserId.toLong, date)
                    S.redirectTo("list")
                case None => S.error("カレンダーの書式が間違っています")
            }
        }

        def setSelectedUser(userId: String): Any = {
            selectedUserId = userId
        }

        val userMap = getUserList.map(u => u.id.toString -> u.name.toString)
        S.notice(userMap.toString)
        bind("e", xhtml,
            "name" --> SHtml.text(name, name = _, "size" -> "24"),
            "personInCharge" --> SHtml.select(userMap, Box(currentUser.is.id.toString), setSelectedUser(_)),
            "year" --> SHtml.text(year, year = _, "size" -> "8"),
            "month" --> SHtml.text(month, month = _, "size" -> "4"),
            "day" --> SHtml.text(day, day = _, "size" -> "4"),
            "submit" --> SHtml.submit("登録", entryTodo))
    }

    def listAction(xhtml: NodeSeq):  NodeSeq = {
        def cancel(): Any = {
            S.redirectTo("list")
        }
        S.notice(currentUser.is.toString)
        S.notice("listAction")
        bind("e", xhtml, "cancel" --> SHtml.submit("戻る", cancel))
    }

    object currentUser extends SessionVar[TodoUser](null)

    def getSearchItemList(keyword: String) : List[TodoItem] = {
        println("key: "+ keyword)
        TodoItem.findAll(Like(TodoItem.name, "%"+keyword+"%"))
    }

    def getItemList() : List[TodoItem] = {
        TodoItem.findAll
    }

    def getItem(id: Long) : Option[TodoItem] = {
        val itemList = TodoItem.findAll(By(TodoItem.id, id))
        if (itemList.size == 0) {
            None
        } else {
            Some(itemList(0))
        }
    }

    def getUserList() : List[TodoUser] = {
        TodoUser.findAll
    }

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

    def loginAction(xhtml: NodeSeq):  NodeSeq = {
        var userId = ""
        var password = ""
        var validPassword = false
        var validUserId = false

        def auth(): Any = {
            getUser(userId, password) match {
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

        bind("e", xhtml,
            "userId" --> SHtml.ajaxText("", checkUserId _),
            "password" --> SHtml.ajaxText("", checkPassword _, "type" -> "password"),
            "submit" --> SHtml.submit("ログイン", auth, "id" -> "login", "disabled" -> "true"))
    }

}
