package todo_webapp.model

import net.liftweb.mapper._

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
}
