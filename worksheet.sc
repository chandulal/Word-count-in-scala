
val text = "This is simple test word count problem. this is " +
  "not production code. test word count is easy. this problem " +
  "is not handle null condition"

type Text = String
type Word = String

def words: Text => List[Word] = {
  text => text.split(" ").toList
}
def toLower: Word => Word = {
  word => word.toLowerCase
}
def sortWords: List[Word] => List[Word] = {
  words => words.sorted
}
def count: List[Word] => List[(Word, Int)] = {
  words =>
    words.map(word => (word, 1))
      .groupBy(_._1)
      .map { case (_, traversable) =>
        traversable.reduce { (a, b) => (a._1, a._2 + b._2) }
      }.toList
}
def sortCount: List[(Word, Int)] => List[(Word, Int)] = {
  wordCountList =>
    wordCountList.sortBy(_._2).reverse
}
def take: Int => List[(Word, Int)] => List[(Word, Int)] = {
  n => wordCountList =>
    wordCountList.take(n)
}
def printWord: List[(Word, Int)] => Unit = {
  wordCountList =>
    wordCountList.foreach { tuple =>
      println(s"Word = ${tuple._1} Count = ${tuple._2}")
    }
}

printWord(take(5)(sortCount(count(sortWords(words(text) map toLower)))))
