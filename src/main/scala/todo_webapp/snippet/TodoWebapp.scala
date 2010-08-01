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
import parsing.XhtmlParser
import io.Source
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: Takeshi Morita
 * Date: 2010/07/05
 * Time: 1:24:15
 *
 */
class TodoWebapp {

    def getUserName(): NodeSeq = {
        if (AuthSnippet.currentUser.is == null)  {
            S.set("error_message", "ログインしていません．")
            S.redirectTo("error")
        }
        <span>{AuthSnippet.currentUser.is.name}</span>
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
            val user = TodoUser.getUserById(item.userId).get
            if (item.finishedDate != null)  {
                css = "tbl_sty1"
                val finishedDate: Date = item.finishedDate
                finished = sdf1.format(finishedDate)
                isFinished = "true"
            } else  if (AuthSnippet.currentUser.is == user && item.deadline.getTime >= Calendar.getInstance().getTimeInMillis) {
                css = "tbl_sty2"
            } else if (AuthSnippet.currentUser.is == user && item.deadline.getTime < Calendar.getInstance().getTimeInMillis) {
                css = "tbl_sty3"
            } else if (item.deadline.getTime < Calendar.getInstance().getTimeInMillis) {
                css = "tbl_sty4"
            } else {
                css = "tbl_sty5"
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
                    <lift:TodoWebapp.showUpdateTodoItemDialogAction form="POST" item_id={itemId}>
                            <e:editTodo/>
                    </lift:TodoWebapp.showUpdateTodoItemDialogAction>
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
        getItemTable(TodoItem.getItemList)
    }


    def errorAction(xhtml: NodeSeq):  NodeSeq = {

        def cancel(): Any = {
            S.redirectTo("login")
        }

        bind("e", xhtml,
            "error" --> <span>{S.get("error_message").openOr("Error")}</span>,
            "return" --> SHtml.submit("戻る", cancel))
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

    def updateAction(xhtml: NodeSeq):  NodeSeq = {

        val itemId = S.get("item_id").openOr("none")
        val currentItem: TodoItem = TodoItem.getItem(itemId.toLong).get
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

        val userMap = TodoUser.getUserList.map(u => u.id.toString -> u.name.toString)

        def setSelectedUser(userId: String): JsCmd = {
            selectedUserId = userId.toLong
            return null
        }

        def getFinishedDate(): Date = {
            if (isFinished) {
                new Date
            } else {
                null
            }
        }
        
        def updateTodoItem(): JsCmd = {
            getDate(year, month, day) match {
                case Some(date) =>
                    TodoItem.updateItem(id, name, selectedUserId, date, getFinishedDate)
                    JsCmds.SetHtml("todolist", getItemTable(TodoItem.getItemList)) &
                            JsRaw(JE.JsFunc("updateTodoListTable").toJsCmd)
                case None =>
                    val dialogId = "#todoitem_dialog"
                    // もう少し良いやり方があるはず
                    JsCmds.SetHtml("todoitem_error_message", <p>カレンダーの書式が間違っています</p>)&
                    JsRaw(JE.JsFunc("openFormDialog", JE.Str(dialogId), JE.Str("作業更新"), JE.Str("#update_todoitem_button")).toJsCmd)
            }
        }

        def setIsFinished(t: Boolean): JsCmd = {
            isFinished = t
            return null
        }

       def setName(n: String): JsCmd = {
          name = n
           return null
       }

        def setYear(y: String): JsCmd = {
            year = y
            return checkDateFormat(year, month, day)
        }

        def setMonth(m: String): JsCmd = {
            month = m
            return checkDateFormat(year, month, day)
        }

        def setDay(d: String): JsCmd = {
            day = d
            return checkDateFormat(year, month, day)
        }

        val textBoxClass = "class" -> "text ui-widget-content ui-corner-all"
        
        bind("e", xhtml,
            "name" --> SHtml.ajaxText(name, setName _, "size" -> "24", textBoxClass),
            "personInCharge" --> SHtml.ajaxSelect(userMap, Box(selectedUserId.toString), setSelectedUser _),
            "year" --> SHtml.ajaxText(year, setYear _, "size" -> "8", textBoxClass),
            "month" --> SHtml.ajaxText(month, setMonth _, "size" -> "4", textBoxClass),
            "day" --> SHtml.ajaxText(day, setDay _, "size" -> "4", textBoxClass),
            "finishedCheckBox" --> SHtml.ajaxCheckbox(isFinished, setIsFinished _, "size" -> "8") ,
            "submit" --> SHtml.ajaxButton("更新", updateTodoItem _, "style" -> "visibility:hidden;", "id" -> "update_todoitem_button" ))
    }

    def checkDateFormat(year: String, month: String, day: String):JsCmd = {
        getDate(year, month, day) match {
            case None =>
                JsCmds.SetHtml("todoitem_error_message", <p>カレンダーの書式が間違っています</p>)
            case _ =>
                JsCmds.SetHtml("todoitem_error_message", <span></span>)
        }
    }

    def showAddTodoItemDialogAction(xhtml: NodeSeq):  NodeSeq = {
        val newItemId = S.attr("item_id").openOr("none")
        def openAddTodoItemDialog(): JsCmd = {
            S.set("item_id", newItemId)
            val addTodoItemForm = LiftRules.loadResourceAsXml("/snippet/addItem.html").openOr(<p>load error</p>)
            val dialogId = "#todoitem_dialog"
            JsCmds.SetHtml(dialogId, addTodoItemForm) &
            JsRaw(JE.JsFunc("openFormDialog",JE.Str(dialogId), JE.Str("作業登録"), JE.Str("#add_todoitem_button")).toJsCmd)
        }

        bind("e", xhtml, "addTodoItem" --> SHtml.ajaxButton("作業登録", openAddTodoItemDialog _))
    }

   def addTodoItemAction(xhtml: NodeSeq) : NodeSeq = {

       var name = ""
       var year = ""
       var month = ""
       var day = ""
       var selectedUserId = AuthSnippet.currentUser.is.id.toString

       def addTodoItem(): JsCmd = {
           getDate(year, month, day) match {
               case Some(date) =>
                   println("insert b")
                   TodoItem.insertItem(name, selectedUserId.toLong, date)
                   println("insert a")
                   JsCmds.SetHtml("todolist", getItemTable(TodoItem.getItemList)) &
                           JsRaw(JE.JsFunc("updateTodoListTable").toJsCmd)
               case None =>
                   val dialogId = "#todoitem_dialog"
                   // もう少し良いやり方があるはず
                   JsCmds.SetHtml("todoitem_error_message", <p>カレンダーの書式が間違っています</p>)&
                           JsRaw(JE.JsFunc("openFormDialog", JE.Str(dialogId), JE.Str("作業登録"), JE.Str("#add_todoitem_button")).toJsCmd)
           }
       }

        def setSelectedUser(userId: String): JsCmd = {
            selectedUserId = userId
            return null
        }

       def setName(n: String): JsCmd = {
           name = n
           return null
       }

       def setYear(y: String): JsCmd = {
           year = y
           return checkDateFormat(year, month, day)
       }

       def setMonth(m: String): JsCmd = {
           month = m
           return checkDateFormat(year, month, day)
       }

       def setDay(d: String): JsCmd = {
           day = d
           return checkDateFormat(year, month, day)
       }

       val userMap = TodoUser.getUserList.map(u => u.id.toString -> u.name.toString)
       S.notice(userMap.toString)
       S.notice(selectedUserId)

       val textBoxClass = "class" -> "text ui-widget-content ui-corner-all"

        bind("e", xhtml,
            "name" --> SHtml.ajaxText(name, setName _, textBoxClass, "size" -> "24"),
            "personInCharge" --> SHtml.ajaxSelect(userMap, Box(AuthSnippet.currentUser.is.id.toString), setSelectedUser(_)),
            "year" --> SHtml.ajaxText(year, setYear _, textBoxClass, "size" -> "8"),
            "month" --> SHtml.ajaxText(month, setMonth _, textBoxClass, "size" -> "4"),
            "day" --> SHtml.ajaxText(day, setDay _, textBoxClass, "size" -> "4"),
            "addTodoItemButton" --> SHtml.ajaxButton("登録", addTodoItem _,   "style" -> "visibility:hidden;", "id" -> "add_todoitem_button"))
   }

    def showUpdateTodoItemDialogAction(xhtml: NodeSeq):  NodeSeq = {
        val updateItemId = S.attr("item_id").openOr("none")
        def openUpdateTodoItemDialog(): JsCmd = {
            S.set("item_id", updateItemId)
            val updateTodoItemForm = LiftRules.loadResourceAsXml("/snippet/updateItem.html").openOr(<p>load error</p>)
            val dialogId = "#todoitem_dialog"
            JsCmds.SetHtml(dialogId, updateTodoItemForm) &
            JsRaw(JE.JsFunc("openFormDialog",JE.Str(dialogId), JE.Str("作業更新"), JE.Str("#update_todoitem_button")).toJsCmd)
        }

        bind("e", xhtml,
            "editTodo" --> SHtml.ajaxButton("更新", openUpdateTodoItemDialog _))
    }

    def toggleFinishedAction(xhtml: NodeSeq):  NodeSeq = {

        val itemId =  S.attr("item_id").openOr("none")
        val isFinished = S.attr("isFinished").openOr("none").toBoolean

        def toggleFinished(): JsCmd = {
            TodoItem.updateItem(itemId.toLong, getFinishedDate())
            val keyword = S.get("last_search_keyword").openOr("")
            val itemList: List[TodoItem] = TodoItem.getSearchItemList(keyword)
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
        val delItem: TodoItem = TodoItem.getItem(delItemId.toLong).get

       def deleteTodoItem() : JsCmd = {
           delItem.delete_!
           return null;
       }

        def confirmDeleteTodoItem(): JsCmd = {
            val title = "削除確認"
            val message = <p>項目 {delItem.name}を削除します。<br/> よろしいですか？</p>
            S.notice(delItemId)
            S.notice(delItem.name.toString)

            JsCmds.SetHtml("confirm_dialog", message) &
            JsRaw(JE.JsFunc("openConfirmDialog", JE.Str(title), JE.Str(delItemId)).toJsCmd)
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
            val itemList = TodoItem.getSearchItemList(keyword)
            JsCmds.SetHtml("todolist", getItemTable(itemList)) &
                    JsRaw(JE.JsFunc("updateTodoListTable").toJsCmd)
        }

        bind("e", xhtml,
            "keyword" --> SHtml.ajaxText("", setKeyword _, "size" -> "24"),
            "search" --> SHtml.ajaxButton("検索", searchTodoItem _) )
    }

    def listAction(xhtml: NodeSeq):  NodeSeq = {
        def cancel(): Any = {
            S.redirectTo("list")
        }
        S.notice(AuthSnippet.currentUser.is.toString)
        S.notice("listAction")
        bind("e", xhtml, "cancel" --> SHtml.submit("戻る", cancel))
    }

}
