package meta.example.supermarket

import meta.example.supermarket.utils.{divCeil, randElement, toInt}
import meta.example.supermarket.categories.{articleName, gram}
import meta.example.supermarket.goods.Item
import scala.collection.mutable.Map

class Fridge {
  val amountMap: Map[articleName, gram] = Map().withDefaultValue(0)
  val storage: Map[articleName, ItemDeque] = Map()
  val opened: Map[articleName, gram] = Map().withDefaultValue(0)

  def isEmpty: Boolean = {
    amountMap.size + storage.size == 0
  }

  def add(item: Item): Unit = {
    getAmount(item.name) match {
      case 0 => storage += (item.name -> new ItemDeque(item))
      case _ => storage.get(item.name).get += item
    }
    val newAmount: Int = amountMap(item.name) + item.priceUnit
    amountMap += (item.name -> newAmount)
    if (notOpened(item.name)){ opened += (item.name -> item.priceUnit) }
  }

  def getAmount(article: articleName): Int = {
    amountMap(article)
  }

  def noStock(article: articleName): Boolean = {
    amountMap(article)==0
  }

  def notOpened(article: String): Boolean = {
    opened(article)==0
  }

  def getAvailFood: Vector[articleName] = {
    opened.filterKeys(opened(_)!=0).keys.toVector
  }

  // Return the amount of unexpired food remains in the fridge
  def rmExpired(article: String): Int = {
    var expiredItem: Item = null
    while (!storage(article).isEmpty && storage(article).peek.state.isExpired) {
      expiredItem = storage(article).popLeft
      amountMap += (article -> (amountMap(article)-opened(article)))
      expiredItem.cleanExpired(opened(article))
      opened += (article -> 0)
      if (!storage(article).isEmpty){
        opened += (article -> expiredItem.priceUnit)
      }
    }
    amountMap(article)
  }

  def consume(article: articleName, amount: gram): Int = {
    val currentAmount = rmExpired(article)
    if (currentAmount <= amount){
      consumeAll(article)
      currentAmount
    } else {
      val targetUnit: Int = storage(article).peek.priceUnit
      if (amount > opened(article)) {
        set2Consume(article, divCeil(amount - opened(article), targetUnit))
      }
      opened += (article -> (opened(article)-amount+toInt(amount>opened(article))*targetUnit))
      if (notOpened(article)) {
        set2Consume(article, 1)
        opened += (article -> targetUnit)
      }
      amountMap += (article -> (amountMap(article) - amount))
      amount
    }
  }

  def consumeAll(article: articleName): Unit = {
    set2Consume(article, storage(article).size)
    amountMap += (article -> 0)
    opened += (article -> 0)
  }

  // remove count number of instances of given article
  private def set2Consume(article: articleName, count: Int): Unit = {
    (1 to count).foreach(
      _ => storage.get(article).get.popLeft.consume
    )
  }

  override def toString: String = {
    amountMap.map(pair => pair._1 + ": " + pair._2).mkString(" ") +
    "\nOpened amount map \n" +
    opened.map(pair => pair._1 + ": " + pair._2).mkString(" ") +
    "\nStorage size \n" +
    storage.map(pair => pair._1 + ": " + pair._2.size).mkString(" ")
//    getAvailFood
//      .map(food_name => food_name + " " + amountMap(food_name))
//      .mkString("\n")
  }
}

