package generated

trait Item17Trait extends meta.deep.runtime.Actor with meta.example.supermarket.goods.Item with meta.example.supermarket.goods.Kitkat with meta.example.supermarket.goods.newItem {
    var age: Int = 0

    private var resetData_0: scala.Any = null;
  private val resetData_1 = scala.collection.mutable.ListBuffer.apply[scala.collection.immutable.List[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]]]();
  private var resetData_2: meta.deep.runtime.ResponseMessage = null;
  private var bindingMut_3: scala.Int = 0;
  private var bindingMut_4: scala.Int = 0;
  var timeVar: scala.Int = 0;
  private var positionVar_6: scala.Int = 0;
  
  val commands_48 = (() => {
  val data_7 = new scala.Array[scala.Function0[scala.Unit]](10);
  data_7.update(0, (() => positionVar_6 = 1));
  data_7.update(1, (() => {
    val x_8 = this.age;
    val x_9 = this.freshUntil;
    val x_10 = x_8.<(x_9);
    val x_13 = x_10.&&({
      val x_11 = this.state;
      val x_12 = x_11.isConsumed;
      x_12.`unary_!`
    });
    if (x_13)
      positionVar_6 = 2
    else
      positionVar_6 = 9
  }));
  data_7.update(2, (() => {
    this.itemInfo;
    resetData_0 = ();
    positionVar_6 = 3;
    val x_14 = timeVar;
    val x_15 = x_14.+(1);
    timeVar = x_15
  }));
  data_7.update(3, (() => {
    val x_16 = this.age;
    resetData_0 = x_16;
    val x_17 = resetData_0;
    val x_18 = x_17.asInstanceOf[scala.Int];
    bindingMut_4 = x_18;
    val x_19 = bindingMut_4;
    val x_20 = x_19.asInstanceOf[scala.Int];
    val x_21 = x_20.+(1);
    resetData_0 = x_21;
    val x_22 = resetData_0;
    val x_23 = x_22.asInstanceOf[scala.Int];
    bindingMut_3 = x_23;
    val x_24 = bindingMut_3;
    val x_25 = x_24.asInstanceOf[scala.Int];
    this.`age_=`(x_25);
    resetData_0 = ();
    positionVar_6 = 4
  }));
  data_7.update(4, (() => {
    val x_26 = this.age;
    val x_27 = this.freshUntil;
    val x_28 = x_26.<(x_27);
    val x_31 = x_28.&&({
      val x_29 = this.state;
      val x_30 = x_29.isConsumed;
      x_30.`unary_!`
    });
    if (x_31)
      positionVar_6 = 2
    else
      positionVar_6 = 5
  }));
  data_7.update(5, (() => {
    val x_32 = this.age;
    val x_33 = this.freshUntil;
    val x_34 = x_32.<(x_33);
    val x_37 = x_34.&&({
      val x_35 = this.state;
      val x_36 = x_35.isConsumed;
      x_36.`unary_!`
    });
    val x_38 = x_37.`unary_!`;
    if (x_38)
      positionVar_6 = 6
    else
      ()
  }));
  data_7.update(6, (() => {
    this.cleanExpired();
    resetData_0 = ();
    positionVar_6 = 7
  }));
  data_7.update(7, (() => {
    positionVar_6 = 8;
    val x_39 = timeVar;
    val x_40 = x_39.+(1);
    timeVar = x_40
  }));
  data_7.update(8, (() => positionVar_6 = 7));
  data_7.update(9, (() => {
    val x_41 = this.age;
    val x_42 = this.freshUntil;
    val x_43 = x_41.<(x_42);
    val x_46 = x_43.&&({
      val x_44 = this.state;
      val x_45 = x_44.isConsumed;
      x_45.`unary_!`
    });
    val x_47 = x_46.`unary_!`;
    if (x_47)
      positionVar_6 = 6
    else
      ()
  }));
  data_7
}).apply();
  
  override def run_until(until_49: scala.Int) : meta.deep.runtime.Actor =  {
    while ({
      val x_50 = timeVar;
      val x_51 = x_50.<=(until_49);
      x_51.&&({
        val x_52 = positionVar_6;
        val x_53 = commands_48.length;
        x_52.<(x_53)
      })
    }) 
      {
        val x_54 = positionVar_6;
        val x_55 = commands_48.apply(x_54);
        x_55.apply()
      }
    ;
    this
  }
}

class Item17 extends Item17Trait