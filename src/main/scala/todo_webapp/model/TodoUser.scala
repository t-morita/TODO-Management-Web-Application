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

    def getUserList() : List[TodoUser] = {
        findAll
    }
        // 要素数が１かそれ以外かで分けてもいいかもしれない
    def getUserById(id: Long): Option[TodoUser] = {
        val userList = findAll(By(TodoUser.id, id))
        if (userList.size == 0) {
            None
        } else {
            Some(userList(0))
        }
    }

    def getUser(userId: String): Option[TodoUser] = {
        val userList = findAll(By(TodoUser.userId, userId))
        if (userList.size == 0) {
            None
        } else {
            Some(userList(0))
        }
    }

    def getUser(userId: String, password: String): Option[TodoUser] = {
        val userList = findAll(By(TodoUser.userId, userId),By(TodoUser.password, password))
        if (userList.size == 0) {
            None
        } else {
            Some(userList(0))
        }
    }

}

