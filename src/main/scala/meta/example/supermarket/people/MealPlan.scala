package meta.example.supermarket.people

import meta.example.supermarket.categories.{articleName, gram}
import meta.example.supermarket.{Carnivore, ShoppingList, Vegetarian}
import meta.example.supermarket.utils.toShoppingList

trait MealPlanV1_1 {
//  val shoppingList: ShoppingList = new ShoppingList(Vegetarian.randShoppingList(0))
//  val mealPlan: Vector[(articleName, gram)] = Vegetarian.mealPlan(0)
  val mealPlan: Vector[(articleName, gram)] = Carnivore.mealPlan(0)
  val shoppingList: ShoppingList = new ShoppingList(toShoppingList(mealPlan))
}

trait MealPlanV1_4 {
  val mealPlan: Vector[(articleName, gram)] = Vegetarian.mealPlan(0)
  val shoppingList: ShoppingList = new ShoppingList(toShoppingList(mealPlan))
}


