package roll.gameplay.modules

import org.scalajs.dom.extensions._
import roll.gameplay.Form
import roll.{Touch, cp, Util}
import roll.cp.Implicits._
import org.scalajs.dom
import concurrent._
import async.Async._
import scalajs.concurrent.JSExecutionContext.Implicits.queue


class Player(val form: Form, widest: cp.Vect,PlayerIndex:Int,name :String=null,color:String=null) {
  var OutOfBound = 0.0
  if(color!="random"){
    val colorMap=Map[String,String]("black"->"#000000","blue"->"#0000ff","red"->"#ff0000","red"->"#00ff00")
    form.SetColor(Color(colorMap(color)))
  }


  form.shapes.map(_.setCollisionType(1))
  //form.body.setMoment(form.body.i / 2)
  val startPos = (form.body.getPos.x, form.body.getPos.y)
  var mouseDown = false
  var mousePos = (0.0,0.0)
  def dead={
    if(form.shapes.exists(_.getCollisionType()==0) ){
      form.shapes.map(sh=>{
        sh.setCollisionType(0)
      })
      true
    }
    else false
  }
  def isTiger={
    if(form.shapes.exists(_.getCollisionType()==3) ){
      form.shapes.map(_.setCollisionType(3))
      true
    }
    else false
  }
  def draw(ctx: dom.CanvasRenderingContext2D,active:Boolean=false) = {
    if (OutOfBound > 0) ctx.globalAlpha = OutOfBound
    if(dead){
      ctx.fillStyle ="gold"
      ctx.fillText(
        "\uD83D\uDC7B",
        form.body.getPos().x ,
        form.body.getPos().y,
        50
      )
      ctx.fillStyle ="yellow"
      ctx.fillText(
        s"p$PlayerIndex",
        form.body.getPos().x ,
        (form.body.getPos().y- 30)
      )

    }
    else
    Util.draw(ctx, form)
    ctx.globalAlpha = 1.0

    if(isTiger){
      ctx.fillStyle ="black"
      ctx.fillText(
        "\uD83D\uDC2F",
        form.body.getPos().x ,
        form.body.getPos().y,
        50
      )
    }

    if(mouseDown){
      ctx.beginPath();
      ctx.translate(form.body.getPos().x, form.body.getPos().y)
      ctx.moveTo(0,0 );
      ctx.lineTo(mousePos._1-form.body.getPos().x, mousePos._2-form.body.getPos().y);
      ctx.lineWidth = 1;
      ctx.strokeStyle = "green";
      ctx.stroke();
      ctx.fillStyle ="orange";
      ctx.fillCircle(mousePos._1-form.body.getPos().x, mousePos._2-form.body.getPos().y, 5)

    }
    else if(active){
      //print player index
      ctx.fillStyle ="green"
      ctx.fillText(
        if(name==null)
          s"p$PlayerIndex"
        else name,
        form.body.getPos().x ,
        (form.body.getPos().y.- (if(isTiger)50.0 else 0.0))
      )
    }

  }

  def update(touches: Seq[Touch]) = {
    if(dead){

    }
    else if (OutOfBound > 0.0) {
      OutOfBound -= 0.05
      if (OutOfBound < 0){
        OutOfBound = 0.0
        form.body.setPos(startPos)
        form.body.setVel((0, 0))
        form.body.setAngVel(0)
      }
    }else{
      val pos = form.body.getPos()
      if (pos.x < 0 || pos.y < 0 || pos.x > widest.x || pos.y > widest.y){
        OutOfBound = 1.0
      }
      val baseT = 0.45
      val maxW = 30
      val decay = (maxW - baseT) / maxW

      if(touches.size>0){
        touches.last match{
          case Touch.Up(p)=>
            form.body.applyImpulse((5000*(form.body.getPos().x-p.x),5000*(form.body.getPos().y-p.y)), (0, 0))
            mousePos= (-1.0,-1.0)
            mouseDown=false

          case Touch.Down(p)=>
            mousePos = (p.x,p.y)
            mouseDown = true

          case Touch.Move(p)=>
            mousePos = (p.x,p.y)

        }
      }


//      if (keys(KeyCode.left)) {
//        form.body.applyImpulse(
//          (-90000,0), (0, 0)
//        )
//      }
//      if (keys(KeyCode.right)){
//        form.body.applyImpulse(
//          (90000,0), (0, 0)
//        )
//      }
//      if (keys(KeyCode.up)){
//        form.body.applyImpulse(
//          (0,-90000), (0, 0)
//        )
//      }
//      if (keys(KeyCode.down)){
//        form.body.applyImpulse(
//          (0,90000), (0, 0)
//        )
//      }

    }
  }
}
