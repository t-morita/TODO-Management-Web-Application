package todo_webapp.snippet

import todo_webapp.model._
import java.sql.Date
import java.util.Calendar
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml._
import collection.immutable.Queue

/**
 * Created by IntelliJ IDEA.
 * User: Takeshi Morita
 * Date: 2010/07/05
 * Time: 1:24:15
 * 
 */
class TodoWebapp {

    def getUserName(): NodeSeq = {
        <span>{currentUser.is.name}</span>
    }

    def errorMessage(xhtml: NodeSeq): NodeSeq = {
            <p>Error</p>
    }
    
    def list(): NodeSeq = {
        val itemList = getItemList()
        var itemTable = new Queue[Node]
        for (item <- itemList) {
          var css = ""
            var finished = "未"
            var isFinished = "false"
           if (item.finishedDate != null)  {
               css = "background-color: #cccccc;"
               finished = item.finishedDate.toString
               isFinished = "true"
           } else  if (currentUser.is == item.user && item.expireDate.getTime >= Calendar.getInstance().getTimeInMillis) {
                css="background-color: #ffbbbb;"
            } else if (currentUser.is == item.user && item.expireDate.getTime < Calendar.getInstance().getTimeInMillis) {
                css="background-color: #ffbbbb; color: #ff0000;"
            } else if (item.expireDate.getTime < Calendar.getInstance().getTimeInMillis) {
               css="color: #ff0000;"
           }
           itemTable += <tr><td style={css}>{item.name}</td><td style={css}>{item.user.name}</td>
               <td style={css}>{item.expireDate}</td>
               <td style={css}>{finished}</td>
               <td style={css}>
                   <lift:TodoWebapp.toggleFinishedAction form="POST" item_id={item.id} isFinished={isFinished}>
                           <e:isFinished/>
                   </lift:TodoWebapp.toggleFinishedAction>
               </td>
               <td style={css}>
                   <lift:TodoWebapp.editPageAction form="POST" item_id={item.id}>
                      <e:itemId/>
                      <e:editTodo/>
                   </lift:TodoWebapp.editPageAction>
               </td>
               <td style={css}>
                   <lift:TodoWebapp.deletePageAction form="POST" item_id={item.id}>
                       <e:itemId/>
                       <e:deleteTodo/>
                   </lift:TodoWebapp.deletePageAction>
               </td>
           </tr>
        }
        itemTable
    }


    def errorAction(xhtml: NodeSeq):  NodeSeq = {

        def cancel(): Any = {
            S.redirectTo("login")
        }

        bind("e", xhtml,
            "error" --> <p>Error</p>,
            "return" --> SHtml.submit("戻る", cancel))
    }

    def getUser(id: String): Option[User] = {
        val userDB = new UserDB()
        userDB.create
        val user = userDB.getUser(id)
        userDB.close
        user
    }

    def insertItem(item: Item) = {
        val itemDB = new ItemDB()
        itemDB.create
        itemDB.insertItem(item)
        itemDB.close
    }

    def updateItem(item: Item) = {
        val itemDB = new ItemDB()
        itemDB.create
        itemDB.updateItem(item)
        itemDB.close
    }

    def deleteItem(item: Item) = {
        val itemDB = new ItemDB()
        itemDB.create
        itemDB.deleteItem(item)
        itemDB.close
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

    def deleteAction(xhtml: NodeSeq):  NodeSeq = {

        val itemId = S.get("item_id").openOr("none")
        val item = getItem(itemId).get
        S.notice(item.toString)

        def deleteTodo(): Any = {
            deleteItem(item)
            S.redirectTo("list")
        }

        bind("e", xhtml,
            "itemName" --> item.name,
            "deleteTodo" --> SHtml.submit("削除", deleteTodo))
    }

    def editAction(xhtml: NodeSeq):  NodeSeq = {

        val itemId = S.get("item_id").openOr("none")
        val currentItem = getItem(itemId).get
        S.notice(currentItem.toString)

        val id = currentItem.id
        var name = currentItem.name
        S.notice(currentItem.expireDate.toString)
        val cal = Calendar.getInstance()
        cal.setTime(currentItem.expireDate)
        var year = cal.get(Calendar.YEAR).toString
        var month = (cal.get(Calendar.MONTH) + 1).toString
        var day = cal.get(Calendar.DAY_OF_MONTH).toString
        var selectedUserId = currentItem.user.id
        var isFinished = false
        if (currentItem.finishedDate != null) {
            isFinished = true
        }

        val userMap = getUserList().map(u => u.id -> u.name)

        def setSelectedUser(userId: String): Any = {
             selectedUserId = userId            
         }
        
        def getFinishedDate(): Date = {
                if (isFinished) {
                    new Date(Calendar.getInstance().getTimeInMillis())
                } else {
                    null
                }
        }
        def editTodo(): Any = {
                getDate(year, month, day) match {
                    case Some(date) =>                    
                    val item = Item(id, name, getUser(selectedUserId).get, date, getFinishedDate)
                    updateItem(item)
                    S.redirectTo("list")
                    case None => S.error("カレンダーの書式が間違っています")
                }                                
        }
        
        def setIsFinished(t: Boolean):Any = {
               isFinished = t
        }
        
            bind("e", xhtml,
                    "name" --> SHtml.text(name, name = _, "size" -> "24"),
                    "personInCharge" --> SHtml.select(userMap, Box(selectedUserId), setSelectedUser(_)),
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
        
        def toggleFinished() = {
            val item = getItem(itemId).get
            item.finishedDate = getFinishedDate()
            S.notice(item.toString)
            updateItem(item)
            S.redirectTo("list")
        }

        def getFinishedDate(): Date = {
            if (!isFinished) {
                new Date(Calendar.getInstance().getTimeInMillis())
            } else {
                null
            }
        }
       val buttonLabel = isFinished match {
           case true => "未完了"
           case false => "完了"
           case _ => "エラー"
       }
        bind("e", xhtml, "isFinished" --> SHtml.submit(buttonLabel, toggleFinished))
    }

    def deletePageAction(xhtml: NodeSeq):  NodeSeq = {

        def redirectDeletePage() = {
            S.set("item_id", S.param("item_id").openOr("none"))
            S.redirectTo("delete")
        }
        
        bind("e", xhtml,
            "itemId" --> SHtml.hidden(null, S.attr("item_id").openOr("none"), "name" -> "item_id"),
            "deleteTodo" --> SHtml.submit("削除", redirectDeletePage))
    }

    def searchPageAction(xhtml: NodeSeq):  NodeSeq = {
        var keyword = ""
        def redirectSearchPage() {
            println(keyword)
            // keywordを保存してリダイレクト
            S.redirectTo("search")
        }
        bind("e", xhtml,
            "keyword" --> SHtml.text(keyword, keyword = _, "size" -> "24"),
            "search" --> SHtml.submit("検索", redirectSearchPage))
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
                    val item = Item(null, name, getUser(selectedUserId).get, date, null)
                    insertItem(item)
                    S.redirectTo("list")
                    case None => S.error("カレンダーの書式が間違っています")
                }                                
        }
            
         def setSelectedUser(userId: String): Any = {
             selectedUserId = userId            
         }           
         
          val userMap = getUserList().map(u => u.id -> u.name)
        S.notice(userMap.toString)
          bind("e", xhtml,
                    "name" --> SHtml.text(name, name = _, "size" -> "24"),
                    "personInCharge" --> SHtml.select(userMap, Box(currentUser.is.id), setSelectedUser(_)),
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
      bind("e", xhtml, "cancel" --> SHtml.submit("キャンセル", cancel))
    }

    object currentUser extends SessionVar[User](null)
    
   def getItemList() : List[Item] = {
       val itemDB = new ItemDB()
       itemDB.create
       val itemList = itemDB.getItems()
       itemDB.close
       itemList
   }

    def getItem(id: String) : Option[Item] = {
        val itemDB = new ItemDB()
        itemDB.create
        val item = itemDB.getItem(id)
        itemDB.close
        item
    }

   def getUserList() : List[User] = {
      val userDB = new UserDB()
       userDB.create
       val userList = userDB.getUsers()
       userDB.close
       userList
    }

    def loginAction(xhtml: NodeSeq):  NodeSeq = {
        var userId = ""
        var password = ""
        def auth(): Any = {
            val userDB = new UserDB()
            userDB.create
            userDB.getUser(userId, password) match {
               case Some(user)  =>
                   S.containerSession.get.setMaxInactiveInterval(30)
                    currentUser(user)
                    S.redirectTo("list")
                case None =>
                    S.redirectTo("error")
            }
            userDB.close
        }
        bind("e", xhtml,
             "userId" --> SHtml.text(userId, userId = _),
        "password" --> SHtml.password(password, password = _),
        "submit" --> SHtml.submit("ログイン", auth))
    }
}
