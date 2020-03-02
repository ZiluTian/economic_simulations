package generated

trait Customer1Trait extends meta.deep.runtime.Actor with meta.example.supermarket.people.People with meta.example.supermarket.people.Weekly with meta.example.supermarket.people.MealPlan1 with meta.example.supermarket.people.ImpulseShopper with meta.example.supermarket.goods.newItem {
  
    private var resetData_0: scala.Any = null;
  private val resetData_1 = scala.collection.mutable.ListBuffer.apply[scala.collection.immutable.List[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]]]();
  private var resetData_2: meta.deep.runtime.ResponseMessage = null;
  private var bindingMut_3: scala.collection.immutable.Vector[scala.Tuple2[java.lang.String, scala.Int]] = null;
  private var listValMut_4: scala.Int = 0;
  private var iterMut_5: scala.collection.Iterator[scala.Int] = null;
  private var bindingMut_6: scala.collection.immutable.List[scala.Int] = null;
  private var bindingMut_7: scala.Int = 0;
  private var bindingMut_8: meta.example.supermarket.categoryAmount = null;
  private var bindingMut_9: meta.example.supermarket.ShoppingList = null;
  private var bindingMut_10: scala.Boolean = false;
  private var bindingMut_11: scala.Double = 0.0;
  private var bindingMut_12: scala.Float = 0.0F;
  private var bindingMut_13: scala.collection.immutable.Vector[scala.Tuple2[java.lang.String, scala.Int]] = null;
  private var bindingMut_14: meta.example.supermarket.ShoppingList = null;
  private var bindingMut_15: scala.Any = null;
  private var listValMut_16: meta.deep.runtime.RequestMessage = null;
  private var iterMut_17: scala.collection.Iterator[meta.deep.runtime.RequestMessage] = null;
  var timeVar: scala.Int = 0;
  private var positionVar_19: scala.Int = 0;
  
  val commands_139 = (() => {
  val data_20 = new scala.Array[scala.Function0[scala.Unit]](18);
  data_20.update(0, (() => {
    positionVar_19 = 1;
    val x_21 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
    val x_22 = scala.Tuple2.apply[scala.Tuple2[scala.Int, scala.Int], scala.Int](x_21, 12);
    val x_23 = scala.collection.immutable.Nil.::[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]](x_22);
    resetData_1.prepend(x_23)
  }));
  data_20.update(1, (() => if (true)
    positionVar_19 = 2
  else
    positionVar_19 = 17));
  data_20.update(2, (() => {
    val x_24 = this.popRequestMessages;
    val x_25 = x_24.iterator;
    iterMut_17 = x_25;
    positionVar_19 = 3
  }));
  data_20.update(3, (() => {
    val x_26 = iterMut_17;
    val x_27 = x_26.asInstanceOf[scala.collection.Iterator[meta.deep.runtime.RequestMessage]];
    val x_28 = x_27.hasNext;
    if (x_28)
      {
        val x_29 = iterMut_17;
        val x_30 = x_29.asInstanceOf[scala.collection.Iterator[meta.deep.runtime.RequestMessage]];
        val x_31 = x_30.next();
        listValMut_16 = x_31;
        positionVar_19 = 4
      }
    else
      positionVar_19 = 7
  }));
  data_20.update(4, (() => {
    val x_32 = listValMut_16;
    val x_33 = x_32.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_34 = x_33.methodId;
    val x_35 = x_34.==(31);
    val x_36 = x_35.`unary_!`;
    if (x_36)
      {
        val x_37 = listValMut_16;
        val x_38 = x_37.asInstanceOf[meta.deep.runtime.RequestMessage];
        val x_39 = scala.collection.immutable.List.apply[meta.deep.runtime.RequestMessage](x_38);
        val x_40 = this.addReceiveMessages(x_39);
        resetData_0 = x_40;
        positionVar_19 = 5
      }
    else
      positionVar_19 = 6
  }));
  data_20.update(5, (() => positionVar_19 = 3));
  data_20.update(6, (() => {
    val x_41 = listValMut_16;
    val x_42 = x_41.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_43 = x_42.methodId;
    val x_44 = x_43.==(31);
    if (x_44)
      positionVar_19 = 1
    else
      ();
    val x_45 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
    val x_46 = scala.Tuple2.apply[scala.Tuple2[scala.Int, scala.Int], scala.Int](x_45, 11);
    val x_47 = scala.collection.immutable.Nil.::[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]](x_46);
    resetData_1.prepend(x_47)
  }));
  data_20.update(7, (() => {
    val x_48 = iterMut_17;
    val x_49 = x_48.asInstanceOf[scala.collection.Iterator[meta.deep.runtime.RequestMessage]];
    val x_50 = x_49.hasNext;
    val x_51 = x_50.`unary_!`;
    if (x_51)
      {
        this.customerInfo;
        resetData_0 = ();
        val x_52 = this.shoppingList;
        resetData_0 = x_52;
        val x_53 = resetData_0;
        val x_54 = x_53.asInstanceOf[meta.example.supermarket.ShoppingList];
        bindingMut_14 = x_54;
        val x_55 = bindingMut_14;
        val x_56 = x_55.asInstanceOf[meta.example.supermarket.ShoppingList];
        val x_57 = x_56.targetItems;
        resetData_0 = x_57;
        val x_58 = resetData_0;
        val x_59 = x_58.asInstanceOf[scala.collection.immutable.Vector[scala.Tuple2[java.lang.String, scala.Int]]];
        bindingMut_13 = x_59;
        val x_60 = scala.util.Random.nextFloat();
        resetData_0 = x_60;
        val x_61 = resetData_0;
        val x_62 = x_61.asInstanceOf[scala.Float];
        bindingMut_12 = x_62;
        val x_63 = this.priceConscious;
        resetData_0 = x_63;
        val x_64 = resetData_0;
        val x_65 = x_64.asInstanceOf[scala.Double];
        bindingMut_11 = x_65;
        val x_66 = bindingMut_11;
        val x_67 = x_66.asInstanceOf[scala.Double];
        val x_68 = bindingMut_12;
        val x_69 = x_68.asInstanceOf[scala.Float];
        val x_70 = x_69.<(x_67);
        resetData_0 = x_70;
        val x_71 = resetData_0;
        val x_72 = x_71.asInstanceOf[scala.Boolean];
        bindingMut_10 = x_72;
        val x_73 = bindingMut_10;
        val x_74 = x_73.asInstanceOf[scala.Boolean];
        val x_75 = bindingMut_13;
        val x_76 = x_75.asInstanceOf[scala.collection.immutable.Vector[scala.Tuple2[java.lang.String, scala.Int]]];
        this.buyListedItems(x_76, x_74);
        resetData_0 = ();
        val x_77 = this.shoppingList;
        resetData_0 = x_77;
        val x_78 = resetData_0;
        val x_79 = x_78.asInstanceOf[meta.example.supermarket.ShoppingList];
        bindingMut_9 = x_79;
        val x_80 = bindingMut_9;
        val x_81 = x_80.asInstanceOf[meta.example.supermarket.ShoppingList];
        val x_82 = x_81.randItems;
        resetData_0 = x_82;
        val x_83 = resetData_0;
        val x_84 = x_83.asInstanceOf[meta.example.supermarket.categoryAmount];
        bindingMut_8 = x_84;
        val x_85 = bindingMut_8;
        val x_86 = x_85.asInstanceOf[meta.example.supermarket.categoryAmount];
        this.buyRandItems(x_86);
        resetData_0 = ();
        val x_87 = this.frequency;
        resetData_0 = x_87;
        val x_88 = resetData_0;
        val x_89 = x_88.asInstanceOf[scala.Int];
        bindingMut_7 = x_89;
        val x_90 = bindingMut_7;
        val x_91 = x_90.asInstanceOf[scala.Int];
        val x_92 = scala.collection.immutable.List.range[scala.Int](0, x_91)(scala.math.Numeric.IntIsIntegral);
        resetData_0 = x_92;
        val x_93 = resetData_0;
        val x_94 = x_93.asInstanceOf[scala.collection.immutable.List[scala.Int]];
        bindingMut_6 = x_94;
        val x_95 = bindingMut_6;
        val x_96 = x_95.asInstanceOf[scala.collection.immutable.List[scala.Int]];
        val x_97 = x_96.iterator;
        iterMut_5 = x_97;
        positionVar_19 = 8
      }
    else
      ()
  }));
  data_20.update(8, (() => {
    val x_98 = iterMut_5;
    val x_99 = x_98.asInstanceOf[scala.collection.Iterator[scala.Int]];
    val x_100 = x_99.hasNext;
    val x_101 = x_100.`unary_!`;
    if (x_101)
      positionVar_19 = 9
    else
      positionVar_19 = 15
  }));
  data_20.update(9, (() => if (true)
    positionVar_19 = 2
  else
    positionVar_19 = 10));
  data_20.update(10, (() => {
    val x_102 = true.`unary_!`;
    if (x_102)
      {
        val x_103 = resetData_1.remove(0);
        val x_107 = x_103.find(((x_104: scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]) => {
          val x_105 = x_104._1;
          val x_106 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
          x_105.==(x_106)
        }));
        val x_108 = x_107.get;
        val x_109 = x_108._2;
        positionVar_19 = x_109
      }
    else
      ()
  }));
  data_20.update(11, (() => {
    val x_110 = resetData_0;
    val x_111 = x_110.asInstanceOf[scala.Any];
    bindingMut_15 = x_111;
    val x_112 = bindingMut_15;
    val x_113 = x_112.asInstanceOf[scala.Any];
    val x_114 = listValMut_16;
    val x_115 = x_114.asInstanceOf[meta.deep.runtime.RequestMessage];
    x_115.reply(this, x_113);
    resetData_0 = ();
    positionVar_19 = 5
  }));
  data_20.update(12, (() => positionVar_19 = 13));
  data_20.update(13, (() => {
    positionVar_19 = 14;
    val x_116 = timeVar;
    val x_117 = x_116.+(1);
    timeVar = x_117
  }));
  data_20.update(14, (() => positionVar_19 = 13));
  data_20.update(15, (() => {
    val x_118 = iterMut_5;
    val x_119 = x_118.asInstanceOf[scala.collection.Iterator[scala.Int]];
    val x_120 = x_119.hasNext;
    if (x_120)
      {
        val x_121 = iterMut_5;
        val x_122 = x_121.asInstanceOf[scala.collection.Iterator[scala.Int]];
        val x_123 = x_122.next();
        listValMut_4 = x_123;
        val x_124 = this.mealPlan;
        resetData_0 = x_124;
        val x_125 = resetData_0;
        val x_126 = x_125.asInstanceOf[scala.collection.immutable.Vector[scala.Tuple2[java.lang.String, scala.Int]]];
        bindingMut_3 = x_126;
        val x_127 = bindingMut_3;
        val x_128 = x_127.asInstanceOf[scala.collection.immutable.Vector[scala.Tuple2[java.lang.String, scala.Int]]];
        this.consumeFood(x_128);
        resetData_0 = ();
        this.consumeFood;
        resetData_0 = ();
        this.customerInfo;
        resetData_0 = ();
        positionVar_19 = 16
      }
    else
      ();
    val x_129 = timeVar;
    val x_130 = x_129.+(1);
    timeVar = x_130
  }));
  data_20.update(16, (() => positionVar_19 = 8));
  data_20.update(17, (() => {
    val x_131 = true.`unary_!`;
    if (x_131)
      {
        val x_132 = resetData_1.remove(0);
        val x_136 = x_132.find(((x_133: scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]) => {
          val x_134 = x_133._1;
          val x_135 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
          x_134.==(x_135)
        }));
        val x_137 = x_136.get;
        val x_138 = x_137._2;
        positionVar_19 = x_138
      }
    else
      ()
  }));
  data_20
}).apply();
  
  override def run_until(until_140: scala.Int) : meta.deep.runtime.Actor =  {
    while ({
      val x_141 = timeVar;
      val x_142 = x_141.<=(until_140);
      x_142.&&({
        val x_143 = positionVar_19;
        val x_144 = commands_139.length;
        x_143.<(x_144)
      })
    }) 
      {
        val x_145 = positionVar_19;
        val x_146 = commands_139.apply(x_145);
        x_146.apply()
      }
    ;
    this
  }
}

class Customer1 extends Customer1Trait