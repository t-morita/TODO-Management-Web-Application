package todo_webapp.model

import junit.framework._
import Assert._

object UserDBTest {
    def suite: Test = {
        val suite = new TestSuite(classOf[UserDBTest]);
        suite
    }

    def main(args : Array[String]) {
        junit.textui.TestRunner.run(suite);
    }
}

class UserDBTest extends TestCase("UserDB"){
    def testGetUser = {
        val db = new UserDB
        db.create
        val dbUser = db.getUser("t_morita").get
        db.close
        assertEquals(dbUser, User("t_morita", "森田武史"))
    }
}