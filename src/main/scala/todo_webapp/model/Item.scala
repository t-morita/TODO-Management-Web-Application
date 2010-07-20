package todo_webapp.model

import java.sql._

case class Item(var id: String, var name: String, var user: User, var expireDate: Date, var finishedDate: Date)
