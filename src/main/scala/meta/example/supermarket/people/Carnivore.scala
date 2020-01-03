package meta.example.supermarket

import meta.example.supermarket.categories.{articleName, gram}

object Carnivore {
  // 1: 1 unit of food (1 agent)
  val randShoppingList: Vector[categoryAmount] = Vector(
    categoryAmount(1, 0, 0, 0, 1),
    categoryAmount(0, 1, 0, 1, 1),
    categoryAmount(1, 1, 2, 2, 0),
    categoryAmount(1, 1, 1, 3, 0),
    categoryAmount(1, 1, 1, 1, 0)
  )

  val mealPlan: Vector[Vector[(articleName, gram)]] = Vector(
    Vector(
      ("Broccoli", 200),
      ("Beef", 200),
      ("Rice", 200),
      ("WhiteChocolate", 50)
    ),
    Vector(
      ("Carrots", 200),
      ("Chicken", 200),
      ("Noodles", 200)
    )
  )
}