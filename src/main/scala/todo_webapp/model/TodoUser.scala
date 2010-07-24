package todo_webapp.model

import net.liftweb.mapper._

/**
 * Created by IntelliJ IDEA.
 * User: Takeshi Morita
 * Date: 2010/07/24
 * Time: 16:34:55
 * 
 */

class TodoUser extends LongKeyedMapper[TodoUser] with IdPK {
    def getSingleton = TodoUser

    object userId extends MappedString(this, 50)
    object name extends MappedString(this, 50)
    object password extends MappedString(this, 50)
}

object TodoUser extends TodoUser with LongKeyedMetaMapper[TodoUser] {
    override def dbTableName = "TODO_USER_TBL"
}

