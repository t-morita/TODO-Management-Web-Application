package todo_webapp.snippet

import xml.NodeSeq
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import java.io.{File, FileOutputStream}

/**
 * Created by IntelliJ IDEA.
 * User: Takeshi Morita
 * Date: 2010/07/14
 * Time: 11:07:45
 * 
 */

class UploadFile {
    var fileHolder: Box[FileParamHolder] = Empty
    
    def uploadFile() = {
        fileHolder match {
        case Full(FileParamHolder(_,_,fn,data)) => {
                var fos: FileOutputStream = null
                try {                    
                    println(fn)
                    fos = new FileOutputStream(new File(fn))
                    fos.write(data)
                } catch {
                    case e => S.error("保存エラー")
                } finally {
                    if (fos != null) {
                        fos.close
                    }
                }
                true
            }
            case _ =>  {
                S.error("アップロードエラー")
            }
        }
    }
    
    def uploadForm(xhtml: NodeSeq) = {
        bind("e", xhtml,
            "receipt" --> SHtml.fileUpload(fh => fileHolder = Full(fh)),
            "upload" --> SHtml.submit("upload", uploadFile))
    }
}