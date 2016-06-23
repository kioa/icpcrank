import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.Scene
import javafx.application.Platform
import javafx.scene.web.{WebView,WebEngine}
import org.w3c.dom.Document;
import scala.xml.{XML,Node}
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
package object LibFx {
  object env {
    var engine: WebEngine = null
    var exitPlatform: Boolean = true
    var show: Boolean = true
    val storage: ListBuffer[String] = new ListBuffer[String]
    val bb: ListBuffer[Browser] = new ListBuffer[Browser]
    def nextSubmit: Boolean = { // 1回だけgetかpostする。getもpostもできなければfalse
      while (!bb.isEmpty) {
        val b = bb.head; bb -= b // ヘッドからブラウザを取り出し
        val res = b.browse
        if (res._1.isDefined)
          res._1.get.submit
        if (res._2.isDefined)
          res._2.get +=: bb   // ヘッドに次のブラウザを追加
        if (res._1.isDefined) // getかpostしたなら終了
          return true
      }
      return false
    }
  }

  // ロック用
  class Lock {
    var available = true
    this.acquire
    def acquire = synchronized {
      while (!available) wait()
      available = false
    }
    def release = synchronized {
      available = true
      notify()
    }
  }

  // 実行用
  class Fx(data: String*) {
    val lock = new Lock() // 最初にロックを取得
    for (s <- data)
      env.storage += s
    env.exitPlatform = true
    def add(b: Browser) { env.bb += b }
    def add(b: Seq[Browser]) { env.bb ++= b }
    def launch(args: String*) { Application.launch(classOf[Main], args: _*) }
    def result(): List[String] = {
      lock.release // ロックを解放して外部にstorageを返す
      return env.storage.toList
    } 
  }

  class Main extends Application {
    import env._
    override def start(stage: Stage) {
      val view = new WebView
      engine = view.getEngine
      nextSubmit

      import javafx.concurrent.Worker._
      import javafx.beans.value.{ChangeListener, ObservableValue}
      val listner = new ChangeListener[State]() {
        override def changed(ov: ObservableValue[_ <: State], oldState: State, newState: State) {
          if (newState == State.SUCCEEDED) {
            val url = engine.getLocation
            System.err.println("url: " + url)
            if (!nextSubmit && exitPlatform)
              Platform.exit
          }
        }
      }
      engine.getLoadWorker.stateProperty.addListener(listner)

      // view
      stage.setScene(new Scene(view, 800, 800))
      stage.setX(0); stage.setY(0)
      if (show)
        stage.show
    }
  }

  // domをxml処理するのに使う
  implicit def document2scalaxmlnode(doc: Document): Node = {
    val src                = new javax.xml.transform.dom.DOMSource(doc)
    val saxer              = new scala.xml.parsing.NoBindingFactoryAdapter
    val saxResult          = new javax.xml.transform.sax.SAXResult(saxer)
    val transformerFactory = javax.xml.transform.TransformerFactory.newInstance
    val transformer        = transformerFactory.newTransformer
    transformer.transform(src, saxResult)
    saxer.rootElem
  }

  // 1つのHttpメソッドを表す
  case class Http(url: String, method: String, params: (String, String)*) {
    require(url.startsWith("http://") || url.startsWith("https://"))
    require(method == "get" || method == "post")
    def submit {
      if (method == "get" && params.isEmpty)
        env.engine.load(url)
      else
        jsSubmit
    }
    private def jsSubmit {
      val js =
        """ var form = document.createElement("form");
            form.action = "%s";
            form.method = "%s";
            %s
            form.submit();"""
      val pp = params.map { t =>
        val name  = t._1
        val value = t._2
        val v_ = ("v_" + name).replaceAll("\\-", "_")
        """ var %s = document.createElement("input");
            %s.name  = "%s";
            %s.value = "%s";
            form.appendChild(%s);""".format(v_, v_, name, v_, value, v_)
      }
      env.engine.executeScript(js.format(url, method, pp.fold("")(_ + _)))
    }
  }
  object Http {
    def apply(url: String): Http = new Http(url, "get")
  }

  // ブラウザは、開くページを表すHttpと次に使うブラウザを返すメソッドを持つ
  trait Browser {
    // _1.isDefinedならば、submitしなければならない
    // その後、_2.isDefinedならば、browseし、新しいBrowserを得なければならない
    // Borwser.isEmptyなら一連のブラウジングが終了
    def browse: (Option[Http], Option[Browser]) 
    protected def some(http: Http, brow: Browser) =
      (Some(http), Some(brow))
    protected def some(http: Option[Http], brow: Browser) =
      (http, Some(brow))
    protected def some(http: Http, brow: Option[Browser]) =
      (Some(http), brow)
    protected def none = (None, None)
  }
  // これを継承してselfとbrowseを実装する
  // selfの実装はクラス名そのままでよい
  abstract class OneBrowser(val state: Int = 0) extends Browser {
    // stateで状態管理
    def self(state: Int): OneBrowser // 自己の複製
  }
  // ブラウザの集まり
  case class Browsers(val bb: Browser*) extends Browser {
    def browse: (Option[Http], Option[Browser]) = {
      if (bb.isEmpty)
        return none
      val res = bb.head.browse
      if (res._2.isEmpty)
        some(res._1, Browsers(bb.tail: _*))
      else
        some(res._1, Browsers((res._2.get :: bb.tail.toList): _*))
    }
  }
  // ブラウジングしない、単一のタスク
  abstract class NoBrowser extends Browser {
    def browse: (Option[Http], Option[Browser]) = {
      task
      none
    }
    def task
  }
}
