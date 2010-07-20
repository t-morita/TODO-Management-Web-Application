package todo_webapp.model

import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Statement
import java.util.ArrayList


/**
 * ユーザデータベースへアクセスするためのクラス。
 */
class UserDB {

    /**
     * Connectionオブジェクトを保持します。
     */
    var connection: Connection = _

    /**
     * Statementオブジェクトを保持します。
     */
    var statement: Statement = _
    
    /**
     * オブジェクトを生成します。
     */
    def create() = {        
        Class.forName("org.h2.Driver");
        connection = DriverManager.getConnection("jdbc:h2:tcp://localhost/~/test", "sa", "");
        statement = connection.createStatement();
    }

    /**
     * 各種オブジェクトを閉じます。
     */
    def close()  = {
        if (statement != null) {
            try {
                statement.close();
            } catch  {
            case e: SQLException => println(e.toString)
            }
            statement = null;
        }
        if (connection != null) {
            try {
                connection.close();
            } catch {
                case e: SQLException => println(e.toString)
            }
            connection = null;
        }
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
            if (ch != '_') {
            if (!isAlphaOrDigit(ch)) {
                return false
            }
            }
        }
        return true
    }

    /**
     * 文字が半角英数字かどうかを判定します。
     * 
     * @param ch
     * @return
     */
    def isAlphaOrDigit(ch: Char): Boolean = {
        if ('A' <= ch && ch <= 'Z') {
            return true
        }
        if ('a' <= ch && ch <= 'z') {
            return true
        }
        if ('0' <= ch && ch <= '9') {
            return true
        }
        return false
    }

    def getUser(userID: String, password: String): Option[User] = {
        var resultSet: ResultSet = null
        try {
            // パラメータのチェック
            if (!isValidUserID(userID)) {               
                return None
            }
            if (!isValidPassword(password)) {               
                return None
            }
            resultSet = statement.executeQuery("SELECT ID,NAME FROM TODO_USER WHERE ID='"+ userID + "' AND PASSWORD='" + password + "'")                    
            val br: Boolean = resultSet.first();
            if (br == false) {
                return None
            }
            return Some(User(resultSet.getString("ID"), resultSet.getString("NAME")))     
        } catch{
        case  e: SQLException =>  e.printStackTrace()
        } finally {
            try {
                if (resultSet != null) {
                    resultSet.close();
                }
            } catch  {
                case e2: SQLException => e2.printStackTrace()
            }
        }
        return None
    }

    /**
     * ユーザを取得します。
     * 
     * @param userID
     * @return
     * @throws ServletException
     */
    def getUser(userID: String) : Option[User]  = {
        try {
            val resultSet = statement.executeQuery("SELECT ID,NAME FROM TODO_USER WHERE ID='"+ userID + "'")
            val br = resultSet.first()
            if (br == false) {
                return None
            }
            return Some(User(resultSet.getString("ID"), resultSet.getString("NAME")))
         } catch  {
         case e: SQLException =>  throw e
        }
         None
    }

    /**
     * ユーザ一覧を取得します。
     * 
     * @return
     * @throws ServletException
     */
    def getUsers(): List[User]  = {
        var users = List[User]()
        try {
            val resultSet: ResultSet = statement.executeQuery("SELECT ID,NAME FROM TODO_USER");
            val br: Boolean = resultSet.first();
            if (br == false) {
                return users
            }            
            do {
                val user = User(resultSet.getString("ID"), resultSet.getString("NAME"))
                users = user :: users
            } while (resultSet.next());
            return users
        } catch  {
            case e: SQLException =>  throw e
        }
    }

}
