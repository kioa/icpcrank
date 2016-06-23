import LibFx._
object icpcrank {
  def isInt(s: String): Boolean = s.map(_.isDigit).reduce(_ && _)
  case class Team(id: String, rank: String, solved: Int, time: Int, _name: String, univ: String, member: String) extends Ordered[Team] {
    var urank: String = "-"
    def compare(that: Team): Int = {
      if (isInt(rank) && isInt(that.rank))
        rank.toInt - that.rank.toInt
      else if (isInt(rank) && !isInt(that.rank))
        -1
      else if (!isInt(rank) && isInt(that.rank))
        +1
      else
        id.compareTo(that.id)
    }
    def name: String = _name
      .replaceAll("""&""", "&amp;")
      .replaceAll(""">""", "&gt;")
      .replaceAll("""<""", "&lt;")
  }

  def makeTeam(s: String): Team = {
    val t = s.split(",")
    val id     = t(0)
    val rank   = t(1)
    val solved = t(2).toInt
    val time   = t(3).tail.init.toInt
    val name   = t(4)
    val r = """(.*)\s\[.*\]""".r
    val univ   = t(5) match {
      case r(u) => u
      case _    => ""
    }
    val member = if (t.size > 6) t(6) else ""
    Team(id, rank, solved, time, name, univ, member)
  }

  def makeHtml(tt: Seq[Team]): Seq[String] = {
    // choose
    val chosen  = (Nil: List[Team]).toBuffer
    val checked = (Nil: List[Team]).toBuffer
    for (t <- tt.sorted) {
      if (t.univ.startsWith("Guest") || !isInt(t.rank)) {
      }
      else if (chosen.size < 5) {
        if (chosen.count(_.univ == t.univ) < 4)
          chosen += t
      }
      else if (chosen.size < 10) {
        if (chosen.count(_.univ == t.univ) < 3)
          chosen += t
      }
      else if (chosen.size < 20) {
        if (chosen.count(_.univ == t.univ) < 2)
          chosen += t
      }
      else if (chosen.size < 33) {
        if (chosen.count(_.univ == t.univ) == 0)
          chosen += t
      }
      checked += t
      t.urank = checked.count(_.univ == t.univ) + ""
    }

    // make html
    val res = (Nil: List[String]).toBuffer

    res += "<html>"
    res += "<body>"
    res += """本家のページをコピーして色を付けてます。観戦にご活用ください。(<a href="#caution">※注</a>)"""

    def td(s: String): String = "<td>" + s + "</td>"
    def tdc(s: String, c: String): String = """<td color="%s">""".format(c) + s + "</td>"

    res += "<table>"
    res += "<tr>" + td("rank") + td("team id") + td("nickname") + td("university") + td("solved") + td("time") + "</tr>"
    for (t <- tt) {
      if (chosen.contains(t)) {
        val color = "#99ffff"
        res += """<tr bgcolor="%s">""".format(color)
      }
      else
        res += "<tr>"
      if (chosen.contains(t))
        res += td(t.rank + "(" + (chosen.indexOf(t)+1) + "/33)")
      else
        res += td(t.rank)
      res += td(t.id)
      res += td(t.name)
      res += td(t.univ + "(" + t.urank + "/" + tt.count(_.univ == t.univ) + ")")
      res += td(t.solved+"")
      res += td(t.time+"")
      res += "</tr>"
    }
    res += "</table>"

    res += "</ul>"
    res += """<a name="caution">※注</a>"""
    res += "<li>公式の結果ではありません。</li>"
    res += """<li><a href="http://icpc.iisf.or.jp/2016-tsukuba/domestic/selectionrule/?lang=ja">選抜ルール</a>の手順1を適用したときに、選抜される33チームに色を付けてます。</li>"""
    res += "<li>数分毎に本家のミラーサイトを確認して更新してます。</li>"
    res += "<li>バグってたらごめんなさい。(今年はプラクティスセッション中に動作確認する暇がないので、動かないかもです。)</li>"
    res += """</body>"""
    res += "</html>"

    res
  }

  def saveFile(rr: Seq[String]) {
    import java.util.{Calendar,Locale,TimeZone}
    import java.util.Calendar._
    val T_NOW = Calendar.getInstance(TimeZone.getTimeZone("Asia/Tokyo"), Locale.JAPAN)
    val pw = new java.io.PrintWriter("%02d%02d_%02d%02d%02d.html".format(
      T_NOW.get(MONTH), T_NOW.get(DAY_OF_MONTH), T_NOW.get(HOUR_OF_DAY), T_NOW.get(MINUTE), T_NOW.get(SECOND)))
    for (r <- rr)
      pw.println(r)
    pw.close
  }

  class Domestic2016(state: Int = 1) extends OneBrowser(state) {
    def self(state: Int) = new Domestic2016(state)
    def browse: (Option[Http], Option[Browser]) = state match {
      case 1 =>
        some(Http("http://icpc.logic.cs.tsukuba.ac.jp/standings/"), self(2))
      case 2 =>
        val doc = env.engine.getDocument
        val lili = (doc \\ "LI").filter(e => (e \\ "@class").text startsWith "team-row").tail
        for (li <- lili) {
          val s  = (li \\ "TD").map(_.text).mkString(",").tail
          val tmp = (li \ "@data-reactid").text
          val id = tmp.substring(tmp.lastIndexOf("$")+1)
          env.storage += id + "," + s
        }
        none
      case _ =>
        none
    }
  }

  def main(args: Array[String]) {
    val fx = new Fx(); env.show = false
    fx add new Domestic2016
    fx.launch()
    val ss: Seq[String] = fx.result
    val tt: Seq[Team]   = ss.map(s => makeTeam(s))
    val rr: Seq[String] = makeHtml(tt)
    for (r <- rr)
      println(r)
    saveFile(rr)
  }
}
