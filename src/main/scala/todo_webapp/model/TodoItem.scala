package todo_webapp.model

import net.liftweb.mapper._
import java.util.Date
/**
 * Created by IntelliJ IDEA.
 * User: Takeshi Morita
 * Date: 2010/07/24
 * Time: 19:43:32
 * 
 */

class TodoItem extends LongKeyedMapper[TodoItem] with IdPK {
    def getSingleton = TodoItem

    object userId extends MappedLongForeignKey(this, TodoUser)
    object name extends MappedString(this, 50)
    object deadline extends MappedDateTime(this)
    object finishedDate extends MappedDateTime(this)
}

object TodoItem extends TodoItem with LongKeyedMetaMapper[TodoItem] {
    override def dbTableName = "TODO_ITEM_TBL"

    def getSearchItemList(keyword: String) : List[TodoItem] = {
        findAll(Like(name, "%"+keyword+"%"))
    }

    def getItemList() : List[TodoItem] = {
        findAll
    }

    def getItem(id: Long) : Option[TodoItem] = {
        val itemList = findAll(By(TodoItem.id, id))
        if (itemList.size == 0) {
            None
        } else {
            Some(itemList(0))
        }
    }

    def insertItem(name: String, selectedUserId: Long, date: Date) = {
        val newItem  = create.name(name).userId(selectedUserId).deadline(date)
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

}
