import io.Source

object Triangle {

  class Tnode(val value: Int) {
    var left: Tnode = null
    var right: Tnode = null
    var maxList: List[Int] = null
  }

  var triangle: Tnode = null
  var lastLine: Array[Tnode] = null

  Source.fromFile("bigtriangle.txt").getLines.foreach(line => {
    val thisLine = line.trim.split(" ").map(i => new Tnode(Integer.parseInt(i)))
    if (triangle == null) triangle = thisLine(0)
    else thisLine.indices.foreach(i => {
      if (i > 0) lastLine(i-1).right = thisLine(i)
      if (i < lastLine.size) lastLine(i).left = thisLine(i)
    })
    lastLine = thisLine
  })

  def sum(list: List[Int]) = list.foldLeft(0)((b, a) => b+a)

  def traverse(node: Tnode): List[Int] = {
    if (node.maxList != null)
      return node.maxList
    if (node.left == null && node.right == null) {
      List[Int](node.value)
    } else {
      val listLeft = traverse(node.left)
      val listRight = traverse(node.right)
      node.maxList = node.value :: (if (sum(listLeft) > sum(listRight)) listLeft else listRight)
      node.maxList
    }
  }

  def main(args: Array[String]) {
    val maxList = traverse(triangle)
    println("max list sum: "+sum(maxList))
    maxList.foreach(println)
  }

}
