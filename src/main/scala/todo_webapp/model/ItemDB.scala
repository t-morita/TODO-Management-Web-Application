package todo_webapp.model

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * 作業項目データベースへアクセスするためのクラス。
 */
class ItemDB {

    var connection: Connection = _
    var  statement: Statement = _

    def create() = {    
        Class.forName("org.h2.Driver");
        connection = DriverManager.getConnection("jdbc:h2:tcp://localhost/~/test", "sa", "")
        statement = connection.createStatement()
    }

    def close()  = {
        if (statement != null) {
            try {
                statement.close();
            } catch  {
                case e: SQLException => println(e)
            }
            statement = null;
        }
        if (connection != null) {
            try {
                connection.close()
            } catch {
                case e: SQLException  => println(e)
            }
            connection = null;
        }
    }

    def updateItem(item: Item): Int = {
        executeUpdate(createUpdateSQL(item))
    }

    def insertItem(item: Item): Int = {
        executeUpdate(createInsertSQL(item))
    }

    def deleteItem(item: Item): Int = {
        executeUpdate(createDeleteSQL(item))
    }

    /**
     * INSERT/UPDATE/DELETE文を実行します。
     * 
     * @param sql
     * @return
     * @throws ServletException
     */
    def executeUpdate(sql: String) : Int  = {
        try {
            statement.executeUpdate(sql)
        } catch  {
            case e:SQLException =>  throw e
        }
    }

    /**
     * 追加用のSQL文を生成します。
     * 
     * @param targetItem
     * @return
     */
    def createInsertSQL(targetItem: Item): String  = {
        val buf = new StringBuffer()
        buf.append("INSERT INTO ")
        buf.append("TODO_ITEM")
        buf.append(" (NAME,USER,EXPIRE_DATE,FINISHED_DATE)")
        buf.append(" VALUES('")
        buf.append(targetItem.name)
        buf.append("', '")
        buf.append(targetItem.user.id)
        buf.append("', '")
        buf.append(targetItem.expireDate.toString)
        buf.append("', null)")
        buf.toString()
    }

    /**
     * 更新用のSQL文を生成します。
     * 
     * @param targetItem
     * @return
     */
     def createUpdateSQL(targetItem: Item): String  = {
        val buf = new StringBuffer();
        buf.append("UPDATE ")
        buf.append("TODO_ITEM")
        buf.append(" SET ")
        buf.append("NAME='")
        buf.append(targetItem.name)
        buf.append("', ")
        buf.append("EXPIRE_DATE='")
        buf.append(targetItem.expireDate.toString)
        buf.append("', ")
        buf.append("USER='")
        buf.append(targetItem.user.id)
        buf.append("', ")
        buf.append("FINISHED_DATE=")
        if (targetItem.finishedDate != null) {
            buf.append("'")
            buf.append(targetItem.finishedDate.toString)
            buf.append("'")
        } else {
            buf.append("null")
        }
        buf.append(" WHERE ID='")
        buf.append(targetItem.id)
        buf.append("'")

        buf.toString()
    }

    /**
     * 削除用のSQL文を生成します。
     * 
     * @param targetItem
     * @return
     */
    def createDeleteSQL(targetItem: Item): String  = {
        val buf = new StringBuffer()
        buf.append("DELETE FROM ")
        buf.append("TODO_ITEM")
        buf.append(" WHERE ID='")
        buf.append(targetItem.id)
        buf.append("'")

        buf.toString()
    }

    /**
     * アイテムを取得します。
     * 
     * @return
     * @throws ServletException
     */
    def getItems() : List[Item] = {
        var items = List[Item]()
        try {
            val resultSet = statement.executeQuery(createSQL(null))
            val br = resultSet.first();
            if (br == false) {
                return items
            }            
            do {
                val user = User(resultSet.getString("TODO_USER.ID"), resultSet.getString("TODO_USER.NAME"))
                val item = Item(resultSet.getString("TODO_ITEM.ID"), resultSet.getString("TODO_ITEM.NAME"),
                        user, resultSet.getDate("EXPIRE_DATE"), resultSet.getDate("FINISHED_DATE"))
                        items = item :: items                
            } while (resultSet.next())

            return items
        } catch {
            case e: SQLException => throw e
        }
    }

    /**
     * アイテムを取得します。
     * 
     * @param id
     * @return
     * @throws ServletException
     */
    def getItem(id: String): Option[Item] = {
        try {
            val resultSet = statement.executeQuery(createSQL("TODO_ITEM.ID='" + id + "'"))
            val br = resultSet.first();
            if (br == false) {
                return None
            }
            val user = User(resultSet.getString("TODO_USER.ID"), resultSet.getString("TODO_USER.NAME"))            
            return Some(Item(resultSet.getString("TODO_ITEM.ID"), resultSet.getString("TODO_ITEM.NAME"), 
                    user, resultSet.getDate("EXPIRE_DATE"), resultSet.getDate("FINISHED_DATE")))            
        } catch {
            case e: SQLException => throw e
        }
    }

    /**
     * アイテムを検索します。
     * 
     * @return
     * @throws ServletException
     */
    def searchItems(keyword: String) : List[Item] = {
        var items =List[Item]()
        val regExp = createRegExp(keyword);
        if (regExp == null) {
            return items
        }
        val where = new StringBuffer();
        val fields = List("TODO_ITEM.NAME",
                "TODO_ITEM.EXPIRE_DATE", "TODO_USER.NAME", "TODO_USER.ID",
                "TODO_ITEM.FINISHED_DATE")
        for (field <- fields) {
            if (where.length() > 0) {
                where.append(" OR ");
            }
            where.append(field);
            where.append(" REGEXP '");
            where.append(regExp);
            where.append("'");
        }
        println(where.toString());
        try {
            val resultSet = statement.executeQuery(createSQL(" ("
                    + where.toString() + ")"));
            val br = resultSet.first();
            if (br == false) {
                return items
            }
            do {
                val user = User(resultSet.getString("TODO_USER.ID"), resultSet.getString("TODO_USER.NAME"))
                val item = Item(resultSet.getString("TODO_ITEM.ID"), resultSet.getString("TODO_ITEM.NAME"), 
                        user, resultSet.getDate("EXPIRE_DATE"), resultSet.getDate("FINISHED_DATE"))
                items = item :: items
            } while (resultSet.next());

            return items
        } catch {
            case  e: SQLException => throw e
        }
    }

    /**
     * アイテム取得用のSQL文を生成します。
     * 
     * @param where
     * @return
     */
    def createSQL(where: String): String  = {
        val  buf = new StringBuffer()
        buf.append("SELECT TODO_ITEM.ID,TODO_ITEM.NAME,TODO_USER.ID,TODO_USER.NAME,EXPIRE_DATE,FINISHED_DATE FROM TODO_USER,TODO_ITEM WHERE TODO_USER.ID=TODO_ITEM.USER");
        if (where != null) {
            buf.append(" AND ")
            buf.append(where)
        }
        println(buf.toString())
        buf.toString()
    }

    /**
     * 正規表現を生成します。
     * 
     * @param keyword
     * @return
     */
    def createRegExp(keyword: String): String = {
        val tokenizer = new StringTokenizer(keyword, " \t");
        if (tokenizer.countTokens() == 0) {
            return null;
        }
        val buf = new StringBuffer()
        while (tokenizer.hasMoreTokens()) {
            val token = tokenizer.nextToken()
            if (buf.length() > 0) {
                buf.append("|")
            }
            buf.append(token)
        }
        return "(" + buf.toString() + ")"
    }

}
