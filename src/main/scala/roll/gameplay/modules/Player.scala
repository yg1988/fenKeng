package roll.gameplay.modules

import org.scalajs.dom.extensions._
import roll.gameplay.Form
import roll.{Touch, cp, Util}
import roll.cp.Implicits._
import org.scalajs.dom
import concurrent._
import async.Async._
import scalajs.concurrent.JSExecutionContext.Implicits.queue


class Player(val form: Form, widest: cp.Vect) {
  var dead = 0.0

  form.shapes(0).setCollisionType(1)
  //form.body.setMoment(form.body.i / 2)
  val startPos = (form.body.getPos.x, form.body.getPos.y)
  var mouseDown = false
  var mousePos = (0.0,0.0)
  def draw(ctx: dom.CanvasRenderingContext2D) = {
    if (dead > 0) ctx.globalAlpha = dead
    Util.draw(ctx, form)
    ctx.globalAlpha = 1.0
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

  }

  def update(touches: Seq[Touch]) = {
    if (dead > 0.0) {
      dead -= 0.05
      if (dead < 0){
        dead = 0.0
        form.body.setPos(startPos)
        form.body.setVel((0, 0))
        form.body.setAngVel(0)
      }
    }else{
      val pos = form.body.getPos()
      if (pos.x < 0 || pos.y < 0 || pos.x > widest.x || pos.y > widest.y){
        dead = 1.0
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
